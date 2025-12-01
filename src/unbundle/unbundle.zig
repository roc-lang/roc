//! Unbundle compressed tar archives
//!
//! This module provides functionality to extract .tar.zst archives created
//! by `roc bundle`, with hash verification for integrity checking.

const builtin = @import("builtin");
const std = @import("std");
const base58 = @import("base58");
const zstd = std.compress.zstd;

// Constants
const TAR_EXTENSION = ".tar.zst";
const STREAM_BUFFER_SIZE: usize = 64 * 1024; // 64KB buffer for streaming operations
// Buffer size for stdlib zstd decompressor: window_len + block_size_max for tar extraction
const DECOMPRESS_BUFFER_SIZE: usize = zstd.default_window_len + zstd.block_size_max;
// Max path bytes - use 4096 on WASM/freestanding, std.fs.max_path_bytes elsewhere
const MAX_PATH_BYTES: usize = if (builtin.os.tag == .freestanding) 4096 else std.fs.max_path_bytes;

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

        // Append entry first to get stable memory in the array list.
        // We must initialize the writer AFTER appending, because the writer
        // stores a pointer to the buffer, and if we initialized it on a stack
        // variable before copying into the array, the pointer would be stale.
        self.open_files.append(.{
            .file = file,
            .buffer = undefined,
            .writer = undefined,
        }) catch {
            file.close();
            return error.OutOfMemory;
        };

        // Now initialize the writer with the buffer in the array (stable memory)
        const entry = &self.open_files.items[self.open_files.items.len - 1];
        entry.writer = file.writer(&entry.buffer);

        return &entry.writer.interface;
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

/// A reader wrapper that hashes all data as it passes through
const HashingReader = struct {
    inner: *std.Io.Reader,
    hasher: *std.crypto.hash.Blake3,
    interface: std.Io.Reader,

    const Self = @This();

    pub fn init(inner: *std.Io.Reader, hasher: *std.crypto.hash.Blake3, buffer: []u8) Self {
        var result = Self{
            .inner = inner,
            .hasher = hasher,
            .interface = undefined,
        };
        result.interface = .{
            .vtable = &vtable,
            .buffer = buffer,
            .seek = 0,
            .end = 0,
        };
        return result;
    }

    const vtable: std.Io.Reader.VTable = .{
        .stream = stream,
    };

    fn stream(r: *std.Io.Reader, w: *std.Io.Writer, limit: std.Io.Limit) std.Io.Reader.StreamError!usize {
        const self: *Self = @alignCast(@fieldParentPtr("interface", r));

        // Read from inner reader into the writer's buffer
        const out_buf = limit.slice(try w.writableSliceGreedy(1));
        var vec: [1][]u8 = .{out_buf};
        const bytes_read = self.inner.readVec(&vec) catch |err| switch (err) {
            error.EndOfStream => return error.EndOfStream,
            error.ReadFailed => return error.ReadFailed,
        };

        if (bytes_read > 0) {
            // Hash the compressed data as it passes through
            self.hasher.update(out_buf[0..bytes_read]);
            w.advance(bytes_read);
        }
        return bytes_read;
    }
};

/// A reader that decompresses zstd data and verifies hash incrementally
/// Uses Zig's stdlib zstd for WASM compatibility
/// Note: Must be heap-allocated to avoid self-referential pointer invalidation
const DecompressingHashReader = struct {
    allocator: std.mem.Allocator,
    hasher: std.crypto.hash.Blake3,
    expected_hash: [32]u8,
    hash_verified: bool,
    hashing_reader: HashingReader,
    decompressor: zstd.Decompress,
    hashing_buffer: []u8,
    decompressor_buffer: []u8,

    const Self = @This();

    /// Create a heap-allocated DecompressingHashReader.
    /// The caller must call deinit() to free resources.
    pub fn create(
        allocator: std.mem.Allocator,
        input_reader: *std.Io.Reader,
        expected_hash: [32]u8,
    ) !*Self {
        // Allocate the struct itself on the heap so pointers remain stable
        const self = try allocator.create(Self);
        errdefer allocator.destroy(self);

        // Allocate buffer for hashing reader
        const hashing_buffer = try allocator.alloc(u8, STREAM_BUFFER_SIZE);
        errdefer allocator.free(hashing_buffer);

        // Allocate buffer for decompressor (needs window_len + block_size_max for tar)
        const decompressor_buffer = try allocator.alloc(u8, DECOMPRESS_BUFFER_SIZE);
        errdefer allocator.free(decompressor_buffer);

        self.* = Self{
            .allocator = allocator,
            .hasher = std.crypto.hash.Blake3.init(.{}),
            .expected_hash = expected_hash,
            .hash_verified = false,
            .hashing_reader = undefined,
            .decompressor = undefined,
            .hashing_buffer = hashing_buffer,
            .decompressor_buffer = decompressor_buffer,
        };

        // Create hashing wrapper around input reader
        // Now safe because self is heap-allocated and won't move
        self.hashing_reader = HashingReader.init(input_reader, &self.hasher, hashing_buffer);

        // Create decompressor reading from hashing reader
        self.decompressor = zstd.Decompress.init(
            &self.hashing_reader.interface,
            decompressor_buffer,
            .{},
        );

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.hashing_buffer);
        self.allocator.free(self.decompressor_buffer);
        self.allocator.destroy(self);
    }

    /// Get the reader interface for tar extraction
    pub fn reader(self: *Self) *std.Io.Reader {
        return &self.decompressor.reader;
    }

    /// Verify that the hash matches. This should be called after reading is complete.
    pub fn verifyComplete(self: *Self) !void {
        // Drain remaining compressed data through the hashing reader
        // This ensures all compressed bytes are hashed even if tar didn't need them
        while (true) {
            // Try to read more compressed data through the decompressor
            var discard_buf: [4096]u8 = undefined;
            const bytes_read = self.decompressor.reader.readSliceShort(&discard_buf) catch {
                // ReadFailed indicates stream is done or error occurred
                break;
            };
            if (bytes_read == 0) break;
        }

        if (!self.hash_verified) {
            var actual_hash: [32]u8 = undefined;
            self.hasher.final(&actual_hash);
            if (!std.mem.eql(u8, &actual_hash, &self.expected_hash)) {
                return error.HashMismatch;
            }
            self.hash_verified = true;
        }
    }
};

/// Unbundle a compressed tar archive, streaming from input_reader to extract_writer.
///
/// This is the core streaming unbundle logic that can be used by both file-based
/// unbundling and network-based downloading.
/// If an InvalidPath error is returned, error_context will contain details about the invalid path.
pub fn unbundleStream(
    allocator: std.mem.Allocator,
    input_reader: *std.Io.Reader,
    extract_writer: ExtractWriter,
    expected_hash: *const [32]u8,
    error_context: ?*ErrorContext,
) UnbundleError!void {
    const decompress_reader = DecompressingHashReader.create(
        allocator,
        input_reader,
        expected_hash.*,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
    };
    defer decompress_reader.deinit();

    var file_name_buffer: [MAX_PATH_BYTES]u8 = undefined;
    var link_name_buffer: [MAX_PATH_BYTES]u8 = undefined;
    var tar_iterator = std.tar.Iterator.init(decompress_reader.reader(), .{
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

    // Verify hash after all data is read
    decompress_reader.verifyComplete() catch |err| switch (err) {
        error.HashMismatch => return error.HashMismatch,
    };

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
    input_reader: *std.Io.Reader,
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
    return unbundleStream(allocator, input_reader, dir_writer.extractWriter(), &expected_hash, error_context);
}
