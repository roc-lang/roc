//! Unbundle compressed tar archives using Zig's standard library
//!
//! This module provides unbundling functionality that works on all platforms
//! including WebAssembly, by using Zig's std.compress.zstd instead of
//! the C zstd library.

const builtin = @import("builtin");
const std = @import("std");
const base58 = @import("base58");

// Constants
const TAR_EXTENSION = ".tar.zst";
const STREAM_BUFFER_SIZE: usize = 64 * 1024; // 64KB buffer for streaming operations

/// Size of the decompression window buffer for zstd.
/// 8MB (2^23 bytes) is the default and recommended size for zstd decompression.
/// This matches zstd's default maximum window size, allowing us to decompress
/// any standard zstd stream. Smaller buffers would fail on streams compressed
/// with larger window sizes.
const ZSTD_WINDOW_BUFFER_SIZE: usize = 1 << 23; // 8MB

fn toAnyReader(reader: anytype) std.io.AnyReader {
    const T = @TypeOf(reader);
    if (T == std.io.AnyReader) {
        return reader;
    }

    switch (@typeInfo(T)) {
        .pointer => |ptr_info| {
            if (ptr_info.child == std.io.AnyReader) {
                return reader.*;
            }
            if (ptr_info.child == std.io.Reader) {
                return reader.adaptToOldInterface();
            }
            if (ptr_info.child != void and ptr_info.size == .One) {
                if (@hasDecl(ptr_info.child, "any")) {
                    return reader.*.any();
                }
                return toAnyReader(reader.*);
            }
        },
        else => {
            if (@hasDecl(T, "any")) {
                return reader.any();
            }
        },
    }

    @compileError("cannot convert type '" ++ @typeName(T) ++ "' to std.io.AnyReader");
}

/// Errors that can occur during the unbundle operation.
pub const UnbundleError = error{
    DecompressionFailed,
    InvalidTarHeader,
    UnexpectedEndOfStream,
    FileCreateFailed,
    DirectoryCreateFailed,
    FileWriteFailed,
    HashMismatch,
    InvalidFilename,
    FileTooLarge,
    InvalidPath,
    NoDataExtracted,
    ChecksumFailure,
    DictionaryIdFlagUnsupported,
    MalformedBlock,
    MalformedFrame,
    WriteFailed,
    ReadFailed,
    EndOfStream,
} || std.mem.Allocator.Error;

/// Context for error reporting during unbundle operations
pub const ErrorContext = struct {
    path: []const u8,
    reason: PathValidationReason,
};

/// Specific reason why a path validation failed
pub const PathValidationReason = union(enum) {
    empty_path,
    path_too_long,
    windows_reserved_char: u8,
    absolute_path,
    path_traversal,
    current_directory_reference,
    windows_reserved_name,
    contained_backslash_on_unix,
    component_ends_with_space,
    component_ends_with_period,
};

/// Virtual table for extract operations
pub const ExtractWriter = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        createFile: *const fn (ptr: *anyopaque, path: []const u8) CreateFileError!*std.Io.Writer,
        finishFile: *const fn (ptr: *anyopaque, writer: *std.Io.Writer) void,
        makeDir: *const fn (ptr: *anyopaque, path: []const u8) MakeDirError!void,
    };

    pub const CreateFileError = error{
        FileCreateFailed,
        OutOfMemory,
    };

    pub const MakeDirError = error{
        DirectoryCreateFailed,
    };

    pub fn createFile(self: ExtractWriter, path: []const u8) CreateFileError!*std.Io.Writer {
        return self.vtable.createFile(self.ptr, path);
    }

    pub fn finishFile(self: ExtractWriter, writer: *std.Io.Writer) void {
        return self.vtable.finishFile(self.ptr, writer);
    }

    pub fn makeDir(self: ExtractWriter, path: []const u8) MakeDirError!void {
        return self.vtable.makeDir(self.ptr, path);
    }
};

/// Directory-based extract writer for filesystem extraction
pub const DirExtractWriter = struct {
    dir: std.fs.Dir,
    allocator: std.mem.Allocator,
    open_files: std.array_list.Managed(FileWriterEntry),

    const FileWriterEntry = struct {
        file: std.fs.File,
        buffer: [4096]u8,
        writer: std.fs.File.Writer,
    };

    pub fn init(dir: std.fs.Dir, allocator: std.mem.Allocator) DirExtractWriter {
        return .{
            .dir = dir,
            .allocator = allocator,
            .open_files = std.array_list.Managed(FileWriterEntry).init(allocator),
        };
    }

    pub fn deinit(self: *DirExtractWriter) void {
        // Close any remaining open files
        for (self.open_files.items) |*entry| {
            entry.file.close();
        }
        self.open_files.deinit();
    }

    pub fn extractWriter(self: *DirExtractWriter) ExtractWriter {
        return ExtractWriter{
            .ptr = self,
            .vtable = &vtable,
        };
    }

    const vtable = ExtractWriter.VTable{
        .createFile = createFile,
        .finishFile = finishFile,
        .makeDir = makeDir,
    };

    fn createFile(ptr: *anyopaque, path: []const u8) ExtractWriter.CreateFileError!*std.Io.Writer {
        const self: *DirExtractWriter = @ptrCast(@alignCast(ptr));

        // Ensure parent directories exist
        if (std.fs.path.dirname(path)) |parent| {
            self.dir.makePath(parent) catch return error.FileCreateFailed;
        }

        const file = self.dir.createFile(path, .{}) catch return error.FileCreateFailed;

        var entry = FileWriterEntry{
            .file = file,
            .buffer = undefined,
            .writer = undefined,
        };
        entry.writer = file.writer(&entry.buffer);

        self.open_files.append(entry) catch {
            file.close();
            return error.OutOfMemory;
        };

        return &self.open_files.items[self.open_files.items.len - 1].writer.interface;
    }

    fn finishFile(ptr: *anyopaque, writer: *std.Io.Writer) void {
        _ = writer;
        const self: *DirExtractWriter = @ptrCast(@alignCast(ptr));
        // Close and remove the last file
        if (self.open_files.items.len > 0) {
            const last_idx = self.open_files.items.len - 1;
            // Flush before closing
            self.open_files.items[last_idx].writer.interface.flush() catch {};
            self.open_files.items[last_idx].file.close();
            _ = self.open_files.orderedRemove(last_idx);
        }
    }

    fn makeDir(ptr: *anyopaque, path: []const u8) ExtractWriter.MakeDirError!void {
        const self: *DirExtractWriter = @ptrCast(@alignCast(ptr));
        self.dir.makePath(path) catch return error.DirectoryCreateFailed;
    }
};

/// Buffer-based extract writer for in-memory extraction
pub const BufferExtractWriter = struct {
    allocator: std.mem.Allocator,
    files: std.StringHashMap(std.array_list.Managed(u8)),
    directories: std.array_list.Managed([]u8),
    current_file_writer: ?std.Io.Writer.Allocating = null,
    current_file_path: ?[]const u8 = null,

    pub fn init(allocator: std.mem.Allocator) BufferExtractWriter {
        return .{
            .allocator = allocator,
            .files = std.StringHashMap(std.array_list.Managed(u8)).init(allocator),
            .directories = std.array_list.Managed([]u8).init(allocator),
        };
    }

    pub fn deinit(self: *BufferExtractWriter) void {
        var iter = self.files.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit();
        }
        self.files.deinit();

        for (self.directories.items) |dir| {
            self.allocator.free(dir);
        }
        self.directories.deinit();
    }

    pub fn extractWriter(self: *BufferExtractWriter) ExtractWriter {
        return ExtractWriter{
            .ptr = self,
            .vtable = &vtable,
        };
    }

    const vtable = ExtractWriter.VTable{
        .createFile = createFile,
        .finishFile = finishFile,
        .makeDir = makeDir,
    };

    fn createFile(ptr: *anyopaque, path: []const u8) ExtractWriter.CreateFileError!*std.Io.Writer {
        const self: *BufferExtractWriter = @ptrCast(@alignCast(ptr));

        const key = self.allocator.dupe(u8, path) catch return error.OutOfMemory;
        self.current_file_path = key;

        // Create allocating writer
        self.current_file_writer = std.Io.Writer.Allocating.init(self.allocator);

        return &self.current_file_writer.?.writer;
    }

    fn finishFile(ptr: *anyopaque, _: *std.Io.Writer) void {
        const self: *BufferExtractWriter = @ptrCast(@alignCast(ptr));
        if (self.current_file_writer) |*writer| {
            if (self.current_file_path) |path| {
                // Convert writer contents to Managed ArrayList
                const unmanaged_list = writer.toArrayList();
                var managed_list = std.array_list.Managed(u8).fromOwnedSlice(self.allocator, unmanaged_list.items);
                self.files.put(path, managed_list) catch {
                    // If put fails, clean up
                    managed_list.deinit();
                    self.allocator.free(path);
                };
                self.current_file_path = null;
            } else {
                writer.deinit();
            }
            self.current_file_writer = null;
        }
    }

    fn makeDir(ptr: *anyopaque, path: []const u8) ExtractWriter.MakeDirError!void {
        const self: *BufferExtractWriter = @ptrCast(@alignCast(ptr));
        const dir_copy = self.allocator.dupe(u8, path) catch return error.DirectoryCreateFailed;
        self.directories.append(dir_copy) catch {
            self.allocator.free(dir_copy);
            return error.DirectoryCreateFailed;
        };
    }
};

/// Result of path validation when an error is found
pub const PathValidationError = struct {
    path: []const u8,
    reason: PathValidationReason,
};

const WINDOWS_RESERVED_NAMES = [_][]const u8{
    "CON",  "PRN",  "AUX",  "NUL",
    "COM1", "COM2", "COM3", "COM4",
    "COM5", "COM6", "COM7", "COM8",
    "COM9", "LPT1", "LPT2", "LPT3",
    "LPT4", "LPT5", "LPT6", "LPT7",
    "LPT8", "LPT9",
};

/// Check if a path has security or compatibility issues for unbundling
pub fn pathHasUnbundleErr(path: []const u8) ?PathValidationError {
    if (path.len == 0) {
        return PathValidationError{
            .path = path,
            .reason = .empty_path,
        };
    }

    if (path.len > 255) {
        return PathValidationError{
            .path = path,
            .reason = .path_too_long,
        };
    }

    if (path[0] == '/' or path[0] == '\\') {
        return PathValidationError{
            .path = path,
            .reason = .absolute_path,
        };
    }

    if (path.len >= 2 and path[1] == ':') {
        return PathValidationError{
            .path = path,
            .reason = .absolute_path,
        };
    }

    var iter = std.mem.tokenizeScalar(u8, path, '/');
    while (iter.next()) |component| {
        if (std.mem.eql(u8, component, "..")) {
            return PathValidationError{
                .path = path,
                .reason = .path_traversal,
            };
        }

        if (std.mem.eql(u8, component, ".")) {
            return PathValidationError{
                .path = path,
                .reason = .current_directory_reference,
            };
        }

        // Use stack buffer for small components to avoid allocation
        var upper_buf: [256]u8 = undefined;
        const upper_component = if (component.len <= upper_buf.len) blk: {
            for (component, 0..) |c, i| {
                upper_buf[i] = std.ascii.toUpper(c);
            }
            break :blk upper_buf[0..component.len];
        } else blk: {
            break :blk std.ascii.allocUpperString(std.heap.page_allocator, component) catch component;
        };
        defer if (component.len > upper_buf.len and upper_component.ptr != component.ptr)
            std.heap.page_allocator.free(upper_component);

        const base_name = if (std.mem.indexOfScalar(u8, upper_component, '.')) |dot_pos|
            upper_component[0..dot_pos]
        else
            upper_component;

        for (WINDOWS_RESERVED_NAMES) |reserved| {
            if (std.mem.eql(u8, base_name, reserved)) {
                return PathValidationError{
                    .path = path,
                    .reason = .windows_reserved_name,
                };
            }
        }

        if (component.len > 0) {
            if (component[component.len - 1] == ' ') {
                return PathValidationError{
                    .path = path,
                    .reason = .component_ends_with_space,
                };
            }
            if (component[component.len - 1] == '.') {
                return PathValidationError{
                    .path = path,
                    .reason = .component_ends_with_period,
                };
            }
        }
    }

    for (path) |char| {
        switch (char) {
            0 => return PathValidationError{
                .path = path,
                .reason = .{ .windows_reserved_char = char },
            },
            '<', '>', ':', '"', '|', '?', '*' => return PathValidationError{
                .path = path,
                .reason = .{ .windows_reserved_char = char },
            },
            '\\' => {
                if (builtin.os.tag != .windows) {
                    return PathValidationError{
                        .path = path,
                        .reason = .contained_backslash_on_unix,
                    };
                }
            },
            else => {},
        }
    }

    return null;
}

/// Generic hashing reader that works with any reader type
fn HashingReader(comptime ReaderType: type) type {
    return struct {
        child_reader: ReaderType,
        hasher: *std.crypto.hash.Blake3,
        interface: std.Io.Reader,

        const Self = @This();
        pub const Error = ReaderType.Error;

        pub fn init(child_reader: ReaderType, hasher: *std.crypto.hash.Blake3) Self {
            var result = Self{
                .child_reader = child_reader,
                .hasher = hasher,
                .interface = undefined,
            };
            result.interface = .{
                .vtable = &.{
                    .stream = stream,
                },
                .buffer = &.{},
                .seek = 0,
                .end = 0,
            };
            return result;
        }

        fn stream(r: *std.Io.Reader, w: *std.Io.Writer, limit: std.Io.Limit) std.Io.Reader.StreamError!usize {
            const self: *Self = @alignCast(@fieldParentPtr("interface", r));
            const dest = limit.slice(try w.writableSliceGreedy(1));
            const n = self.read(dest) catch return std.Io.Reader.StreamError.ReadFailed;
            if (n == 0) {
                return std.Io.Reader.StreamError.EndOfStream;
            }
            w.advance(n);
            return n;
        }

        pub fn read(self: *Self, buffer: []u8) Error!usize {
            const n = try self.child_reader.read(buffer);
            if (n > 0) {
                self.hasher.update(buffer[0..n]);
            }
            return n;
        }
    };
}

/// Unbundle a compressed tar archive, streaming from input_reader to extract_writer.
///
/// This is the core streaming unbundle logic that can be used by both file-based
/// unbundling and network-based downloading.
/// If an InvalidPath error is returned, error_context will contain details about the invalid path.
pub fn unbundleStream(
    input_reader: anytype,
    extract_writer: ExtractWriter,
    expected_hash: *const [32]u8,
    error_context: ?*ErrorContext,
) UnbundleError!void {
    var hasher = std.crypto.hash.Blake3.init(.{});
    const any_reader = toAnyReader(input_reader);
    const ReaderType = @TypeOf(any_reader);
    const HashingReaderType = HashingReader(ReaderType);

    var hashing_reader = HashingReaderType.init(any_reader, &hasher);

    var window_buffer: [ZSTD_WINDOW_BUFFER_SIZE]u8 = undefined;

    const zstd_stream = std.compress.zstd.Decompress.init(&hashing_reader.interface, &window_buffer, .{});
    var decompressed_reader = zstd_stream.reader;

    var file_name_buffer: [std.fs.max_path_bytes]u8 = undefined;
    var link_name_buffer: [std.fs.max_path_bytes]u8 = undefined;
    var tar_iterator = std.tar.Iterator.init(&decompressed_reader, .{
        .file_name_buffer = &file_name_buffer,
        .link_name_buffer = &link_name_buffer,
    });

    var data_extracted = false;

    while (true) {
        const maybe_entry = tar_iterator.next() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return error.InvalidTarHeader,
        };

        const entry = maybe_entry orelse break;
        const file_path = entry.name;

        if (pathHasUnbundleErr(file_path)) |validation_error| {
            if (error_context) |ctx| {
                ctx.path = validation_error.path;
                ctx.reason = validation_error.reason;
            }
            return error.InvalidPath;
        }

        switch (entry.kind) {
            .directory => {
                try extract_writer.makeDir(file_path);
                data_extracted = true;
            },
            .file => {
                const file_writer = try extract_writer.createFile(file_path);
                defer extract_writer.finishFile(file_writer);

                try tar_iterator.streamRemaining(entry, file_writer);
                try file_writer.flush();

                data_extracted = true;
            },
            .sym_link => {
                const link_target = entry.link_name;

                if (link_target.len > 0 and link_target[0] == '/') {
                    if (error_context) |ctx| {
                        ctx.path = file_path;
                        ctx.reason = .absolute_path;
                    }
                    return error.InvalidPath;
                }

                var iter = std.mem.tokenizeScalar(u8, link_target, '/');
                while (iter.next()) |component| {
                    if (std.mem.eql(u8, component, "..")) {
                        if (error_context) |ctx| {
                            ctx.path = file_path;
                            ctx.reason = .path_traversal;
                        }
                        return error.InvalidPath;
                    }
                    if (std.mem.eql(u8, component, ".")) {
                        if (error_context) |ctx| {
                            ctx.path = file_path;
                            ctx.reason = .current_directory_reference;
                        }
                        return error.InvalidPath;
                    }
                }

                // TODO: Add symlink support to ExtractWriter interface
                data_extracted = true;
            },
        }
    }

    var actual_hash: [32]u8 = undefined;
    hasher.final(&actual_hash);

    if (!std.mem.eql(u8, &actual_hash, expected_hash)) {
        return error.HashMismatch;
    }

    if (!data_extracted) {
        return error.NoDataExtracted;
    }
}

/// Validate a base58-encoded hash string and decode it.
///
/// Returns the decoded hash if valid, or null if invalid.
pub fn validateBase58Hash(base58_str: []const u8) !?[32]u8 {
    // Valid base58 hash should be 32-44 characters
    if (base58_str.len < 32 or base58_str.len > 44) {
        return null;
    }

    var hash: [32]u8 = undefined;
    base58.decode(base58_str, &hash) catch {
        return null;
    };

    return hash;
}

/// Unbundle files from a compressed tar archive to a directory.
///
/// The filename parameter should be the base58-encoded blake3 hash + .tar.zst extension.
/// If an InvalidPath error is returned, error_context will contain details about the invalid path.
pub fn unbundle(
    allocator: std.mem.Allocator,
    input_reader: anytype,
    extract_dir: std.fs.Dir,
    filename: []const u8,
    error_context: ?*ErrorContext,
) UnbundleError!void {
    if (!std.mem.endsWith(u8, filename, TAR_EXTENSION)) {
        return error.InvalidFilename;
    }
    const base58_hash = filename[0 .. filename.len - TAR_EXTENSION.len];
    const expected_hash = (try validateBase58Hash(base58_hash)) orelse {
        return error.InvalidFilename;
    };

    var dir_writer = DirExtractWriter.init(extract_dir, allocator);
    defer dir_writer.deinit();
    return unbundleStream(input_reader, dir_writer.extractWriter(), &expected_hash, error_context);
}
