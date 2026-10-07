//! Unbundle compressed tar archives
//!
//! This module provides functionality to extract .tar.zst archives created
//! by `roc bundle`, with hash verification for integrity checking.

const builtin = @import("builtin");
const std = @import("std");
const Allocator = std.mem.Allocator;
const private_dir_permissions: std.Io.Dir.Permissions = if (@hasDecl(std.Io.Dir.Permissions, "fromMode")) .fromMode(0o700) else .default_dir;
const private_file_permissions: std.Io.Dir.Permissions = if (@hasDecl(std.Io.Dir.Permissions, "fromMode")) .fromMode(0o600) else .default_file;
const base58 = @import("base58");
const zstd = std.compress.zstd;
const format = @import("format.zig");

// Constants
const TAR_EXTENSION = format.TAR_EXTENSION;
const STREAM_BUFFER_SIZE = format.STREAM_BUFFER_SIZE;
// Buffer size for stdlib zstd decompressor: window_len + block_size_max for tar extraction
const DECOMPRESS_BUFFER_SIZE: usize = zstd.default_window_len + zstd.block_size_max;
// Max path bytes - use 4096 on WASM/freestanding, std.Io.Dir.max_path_bytes elsewhere
const MAX_PATH_BYTES: usize = if (builtin.os.tag == .freestanding) 4096 else std.Io.Dir.max_path_bytes;

/// Errors that can occur during the unbundle operation.
pub const UnbundleError = error{
    DecompressionFailed,
    ExpandedSizeLimitExceeded,
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
        finishFile: *const fn (ptr: *anyopaque) std.mem.Allocator.Error!void,
        makeDir: *const fn (ptr: *anyopaque, path: []const u8) MakeDirError!void,
    };

    pub const CreateFileError = error{
        FileCreateFailed,
        OutOfMemory,
    };

    pub const MakeDirError = error{
        DirectoryCreateFailed,
        OutOfMemory,
    };

    pub fn createFile(self: ExtractWriter, path: []const u8) CreateFileError!*std.Io.Writer {
        return self.vtable.createFile(self.ptr, path);
    }

    pub fn finishFile(self: ExtractWriter) std.mem.Allocator.Error!void {
        return self.vtable.finishFile(self.ptr);
    }

    pub fn makeDir(self: ExtractWriter, path: []const u8) MakeDirError!void {
        return self.vtable.makeDir(self.ptr, path);
    }
};

/// Directory-based extract writer for filesystem extraction
pub const DirExtractWriter = struct {
    dir: std.Io.Dir,
    io: std.Io,
    allocator: std.mem.Allocator,
    open_files: std.array_list.Managed(FileWriterEntry),

    const FileWriterEntry = struct {
        file: std.Io.File,
        buffer: [4096]u8,
        writer: std.Io.File.Writer,
    };

    pub fn init(dir: std.Io.Dir, io: std.Io, allocator: std.mem.Allocator) DirExtractWriter {
        return .{
            .dir = dir,
            .io = io,
            .allocator = allocator,
            .open_files = std.array_list.Managed(FileWriterEntry).init(allocator),
        };
    }

    pub fn deinit(self: *DirExtractWriter) void {
        // Close any remaining open files
        for (self.open_files.items) |*entry| {
            entry.file.close(self.io);
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

    fn isWithinRoot(root: []const u8, resolved: []const u8) bool {
        if (pathEqual(root, resolved)) return true;
        if (resolved.len <= root.len) return false;
        if (!pathEqual(root, resolved[0..root.len])) return false;
        return std.fs.path.isSep(root[root.len - 1]) or std.fs.path.isSep(resolved[root.len]);
    }

    fn pathEqual(a: []const u8, b: []const u8) bool {
        return if (builtin.os.tag == .windows)
            std.ascii.eqlIgnoreCase(a, b)
        else
            std.mem.eql(u8, a, b);
    }

    /// Walk one component at a time from the extraction handle. Opening each
    /// component without following links prevents an existing directory link
    /// from redirecting a later create outside the extraction root.
    fn openContainedDir(self: *DirExtractWriter, path: []const u8) (std.Io.Dir.RealPathError || std.Io.Dir.OpenError || std.Io.Dir.CreateDirPathOpenError || std.Io.Dir.StatError || error{ AccessDenied, NotDir })!std.Io.Dir {
        var root_buf: [MAX_PATH_BYTES]u8 = undefined;
        const root_len = try self.dir.realPath(self.io, &root_buf);
        const root = root_buf[0..root_len];

        var current = try self.dir.openDir(self.io, ".", .{ .follow_symlinks = false });
        errdefer current.close(self.io);

        var iter = std.mem.tokenizeAny(u8, path, if (builtin.os.tag == .windows) "/\\" else "/");
        while (iter.next()) |component| {
            const next = try current.createDirPathOpen(self.io, component, .{
                .open_options = .{ .follow_symlinks = false },
                .permissions = private_dir_permissions,
            });
            current.close(self.io);
            current = next;

            var resolved_buf: [MAX_PATH_BYTES]u8 = undefined;
            const resolved_len = try current.realPath(self.io, &resolved_buf);
            if (!isWithinRoot(root, resolved_buf[0..resolved_len])) return error.AccessDenied;
            const stat = try current.stat(self.io);
            if (stat.kind != .directory) return error.NotDir;
        }
        return current;
    }

    fn createFile(ptr: *anyopaque, path: []const u8) ExtractWriter.CreateFileError!*std.Io.Writer {
        const self: *DirExtractWriter = @ptrCast(@alignCast(ptr));

        // Keep the filesystem writer safe even if a caller bypasses the tar
        // entry validator.
        if (pathHasUnbundleErr(path) != null) return error.FileCreateFailed;

        const parent_path = std.fs.path.dirname(path) orelse "";
        const parent = self.openContainedDir(parent_path) catch return error.FileCreateFailed;
        defer parent.close(self.io);

        // The handle also reads, because the containment checks below stat
        // it: Windows grants a write-only handle no right to read the file's
        // attributes.
        const name = std.fs.path.basename(path);
        const file = parent.openFile(self.io, name, .{
            .mode = .read_write,
            .follow_symlinks = false,
            .resolve_beneath = true,
        }) catch |err| switch (err) {
            error.FileNotFound => parent.createFile(self.io, name, .{
                .read = true,
                .exclusive = true,
                .truncate = false,
                .resolve_beneath = true,
                .permissions = private_file_permissions,
            }) catch return error.FileCreateFailed,
            else => return error.FileCreateFailed,
        };
        errdefer file.close(self.io);

        var root_buf: [MAX_PATH_BYTES]u8 = undefined;
        const root_len = self.dir.realPath(self.io, &root_buf) catch return error.FileCreateFailed;
        var file_buf: [MAX_PATH_BYTES]u8 = undefined;
        const file_len = file.realPath(self.io, &file_buf) catch return error.FileCreateFailed;
        if (!isWithinRoot(root_buf[0..root_len], file_buf[0..file_len])) return error.FileCreateFailed;
        const stat = file.stat(self.io) catch return error.FileCreateFailed;
        if (stat.kind != .file) return error.FileCreateFailed;
        file.setLength(self.io, 0) catch return error.FileCreateFailed;

        // Append entry first to get stable memory in the array list.
        // We must initialize the writer AFTER appending, because the writer
        // stores a pointer to the buffer, and if we initialized it on a stack
        // variable before copying into the array, the pointer would be stale.
        self.open_files.append(.{
            .file = file,
            .buffer = undefined,
            .writer = undefined,
        }) catch return error.OutOfMemory;

        // Now initialize the writer with the buffer in the array (stable memory)
        const entry = &self.open_files.items[self.open_files.items.len - 1];
        entry.writer = file.writer(self.io, &entry.buffer);

        return &entry.writer.interface;
    }

    fn finishFile(ptr: *anyopaque) std.mem.Allocator.Error!void {
        const self: *DirExtractWriter = @ptrCast(@alignCast(ptr));
        // Close and remove the last file
        if (self.open_files.items.len > 0) {
            const last_idx = self.open_files.items.len - 1;
            // Flush before closing
            self.open_files.items[last_idx].writer.interface.flush() catch {};
            self.open_files.items[last_idx].file.close(self.io);
            _ = self.open_files.orderedRemove(last_idx);
        }
    }

    fn makeDir(ptr: *anyopaque, path: []const u8) ExtractWriter.MakeDirError!void {
        const self: *DirExtractWriter = @ptrCast(@alignCast(ptr));
        if (pathHasUnbundleErr(path) != null) return error.DirectoryCreateFailed;
        const dir = self.openContainedDir(path) catch return error.DirectoryCreateFailed;
        dir.close(self.io);
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

    fn finishFile(ptr: *anyopaque) std.mem.Allocator.Error!void {
        const self: *BufferExtractWriter = @ptrCast(@alignCast(ptr));
        if (self.current_file_writer) |*writer| {
            if (self.current_file_path) |path| {
                var unmanaged = writer.toArrayList();
                var contents = unmanaged.toManaged(self.allocator);
                self.current_file_path = null;
                self.current_file_writer = null;
                const slot = self.files.getOrPut(path) catch |err| {
                    contents.deinit();
                    self.allocator.free(path);
                    return err;
                };
                if (slot.found_existing) {
                    // A later entry for a path replaces the earlier contents;
                    // the map keeps the key it already owns.
                    self.allocator.free(path);
                    slot.value_ptr.deinit();
                }
                slot.value_ptr.* = contents;
                return;
            } else {
                writer.deinit();
            }
            self.current_file_writer = null;
        }
    }

    fn makeDir(ptr: *anyopaque, path: []const u8) ExtractWriter.MakeDirError!void {
        const self: *BufferExtractWriter = @ptrCast(@alignCast(ptr));
        const dir_copy = self.allocator.dupe(u8, path) catch return error.OutOfMemory;
        self.directories.append(dir_copy) catch |err| switch (err) {
            error.OutOfMemory => {
                self.allocator.free(dir_copy);
                return error.OutOfMemory;
            },
        };
    }
};

/// An archive entry path the path rules refuse, with the reason.
pub const PathValidationError = struct {
    path: []const u8,
    reason: PathValidationReason,
};

/// Where `bundle` and `unbundle` report the path that failed validation when
/// they return `error.InvalidPath`.
pub const ErrorContext = PathValidationError;

/// File and directory base names Windows reserves for devices; creating them
/// misbehaves on Windows, so portable name validation rejects them everywhere.
pub const WINDOWS_RESERVED_NAMES = [_][]const u8{
    "CON",  "PRN",  "AUX",  "NUL",
    "COM1", "COM2", "COM3", "COM4",
    "COM5", "COM6", "COM7", "COM8",
    "COM9", "LPT1", "LPT2", "LPT3",
    "LPT4", "LPT5", "LPT6", "LPT7",
    "LPT8", "LPT9",
};

/// The archive path rules: returns why `path` is unsafe or unportable to
/// extract, or null when it is acceptable. `bundle` applies this same function
/// to every path it writes, so it never produces an archive that extraction
/// refuses.
pub fn pathHasUnbundleErr(path: []const u8) ?PathValidationError {
    const reason = pathInvalidReason(path) orelse return null;
    return .{ .path = path, .reason = reason };
}

fn pathInvalidReason(path: []const u8) ?PathValidationReason {
    if (path.len == 0) return .empty_path;
    if (path.len > format.TAR_PATH_MAX_LENGTH) return .path_too_long;
    if (path[0] == '/' or path[0] == '\\') return .absolute_path;
    if (path.len >= 2 and path[1] == ':') return .absolute_path;

    // A backslash is accepted only where it is a path separator (see the
    // character rules below), so components end at either separator: `..` and
    // reserved names cannot hide behind a backslash.
    var iter = std.mem.tokenizeAny(u8, path, "/\\");
    while (iter.next()) |component| {
        if (std.mem.eql(u8, component, "..")) return .path_traversal;
        if (std.mem.eql(u8, component, ".")) return .current_directory_reference;

        // Windows reserves a device name whatever its case and extension, so
        // the comparison is on the part before the first '.'.
        const base_name = component[0 .. std.mem.findScalar(u8, component, '.') orelse component.len];
        for (WINDOWS_RESERVED_NAMES) |reserved| {
            if (std.ascii.eqlIgnoreCase(base_name, reserved)) return .windows_reserved_name;
        }

        switch (component[component.len - 1]) {
            ' ' => return .component_ends_with_space,
            '.' => return .component_ends_with_period,
            else => {},
        }
    }

    for (path) |char| {
        switch (char) {
            0, '<', '>', ':', '"', '|', '?', '*' => return .{ .windows_reserved_char = char },
            '\\' => if (builtin.os.tag != .windows) return .contained_backslash_on_unix,
            else => {},
        }
    }

    return null;
}

fn linkTargetUnbundleErr(target: []const u8) ?PathValidationReason {
    if (target.len > 0 and (target[0] == '/' or (builtin.os.tag == .windows and target[0] == '\\'))) {
        return .absolute_path;
    }
    if (builtin.os.tag == .windows and target.len >= 2 and target[1] == ':') {
        return .absolute_path;
    }

    var iter = std.mem.tokenizeAny(u8, target, if (builtin.os.tag == .windows) "/\\" else "/");
    while (iter.next()) |component| {
        if (std.mem.eql(u8, component, "..")) return .path_traversal;
        if (std.mem.eql(u8, component, ".")) return .current_directory_reference;
    }
    return null;
}

test "symlink targets use native path separators" {
    const testing = std.testing;
    if (builtin.os.tag == .windows) {
        try testing.expect(linkTargetUnbundleErr("a\\..\\x").? == .path_traversal);
        try testing.expect(linkTargetUnbundleErr("a\\.\\x").? == .current_directory_reference);
        try testing.expect(linkTargetUnbundleErr("\\outside").? == .absolute_path);
        try testing.expect(linkTargetUnbundleErr("C:outside").? == .absolute_path);
    } else {
        try testing.expect(linkTargetUnbundleErr("a\\..\\x") == null);
        try testing.expect(linkTargetUnbundleErr("a\\.\\x") == null);
    }
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

/// A reader that counts decompressed bytes as they pass through and fails the
/// stream once an optional limit is exceeded, so a malicious archive cannot
/// expand without bound (a "zip bomb") regardless of what its zstd frame
/// header claims.
const CountingLimitReader = struct {
    inner: *std.Io.Reader,
    total_bytes: u64,
    max_bytes: ?u64,
    limit_exceeded: bool,
    interface: std.Io.Reader,

    const Self = @This();

    pub fn init(inner: *std.Io.Reader, max_bytes: ?u64, buffer: []u8) Self {
        var result = Self{
            .inner = inner,
            .total_bytes = 0,
            .max_bytes = max_bytes,
            .limit_exceeded = false,
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

        if (self.limit_exceeded) return error.ReadFailed;

        const out_buf = limit.slice(try w.writableSliceGreedy(1));
        var vec: [1][]u8 = .{out_buf};
        const bytes_read = self.inner.readVec(&vec) catch |err| switch (err) {
            error.EndOfStream => return error.EndOfStream,
            error.ReadFailed => return error.ReadFailed,
        };

        if (bytes_read > 0) {
            self.total_bytes += bytes_read;
            if (self.max_bytes) |max| {
                if (self.total_bytes > max) {
                    self.limit_exceeded = true;
                    return error.ReadFailed;
                }
            }
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
    counting_reader: CountingLimitReader,
    hashing_buffer: []u8,
    decompressor_buffer: []u8,
    counting_buffer: []u8,

    const Self = @This();

    /// Create a heap-allocated DecompressingHashReader.
    /// The caller must call deinit() to free resources.
    pub fn create(
        allocator: std.mem.Allocator,
        input_reader: *std.Io.Reader,
        expected_hash: [32]u8,
        max_expanded_bytes: ?u64,
    ) Allocator.Error!*Self {
        // Allocate the struct itself on the heap so pointers remain stable
        const self = try allocator.create(Self);
        errdefer allocator.destroy(self);

        // Allocate buffer for hashing reader
        const hashing_buffer = try allocator.alloc(u8, STREAM_BUFFER_SIZE);
        errdefer allocator.free(hashing_buffer);

        // Allocate buffer for decompressor (needs window_len + block_size_max for tar)
        const decompressor_buffer = try allocator.alloc(u8, DECOMPRESS_BUFFER_SIZE);
        errdefer allocator.free(decompressor_buffer);

        // Allocate buffer for the counting reader
        const counting_buffer = try allocator.alloc(u8, STREAM_BUFFER_SIZE);
        errdefer allocator.free(counting_buffer);

        self.* = Self{
            .allocator = allocator,
            .hasher = std.crypto.hash.Blake3.init(.{}),
            .expected_hash = expected_hash,
            .hash_verified = false,
            .hashing_reader = undefined,
            .decompressor = undefined,
            .counting_reader = undefined,
            .hashing_buffer = hashing_buffer,
            .decompressor_buffer = decompressor_buffer,
            .counting_buffer = counting_buffer,
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

        // Count every decompressed byte the consumer reads
        self.counting_reader = CountingLimitReader.init(
            &self.decompressor.reader,
            max_expanded_bytes,
            counting_buffer,
        );

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.hashing_buffer);
        self.allocator.free(self.decompressor_buffer);
        self.allocator.free(self.counting_buffer);
        self.allocator.destroy(self);
    }

    /// Get the reader interface for tar extraction
    pub fn reader(self: *Self) *std.Io.Reader {
        return &self.counting_reader.interface;
    }

    /// Total decompressed bytes consumed so far.
    pub fn expandedBytes(self: *const Self) u64 {
        return self.counting_reader.total_bytes;
    }

    /// Whether the expanded-size limit was exceeded during reading.
    pub fn limitExceeded(self: *const Self) bool {
        return self.counting_reader.limit_exceeded;
    }

    /// Verify that the hash matches. This should be called after reading is complete.
    pub fn verifyComplete(self: *Self) error{ HashMismatch, ExpandedSizeLimitExceeded }!void {
        // Drain remaining compressed data through the hashing reader
        // This ensures all compressed bytes are hashed even if tar didn't need them
        while (true) {
            // Try to read more compressed data through the decompressor
            var discard_buf: [4096]u8 = undefined;
            const bytes_read = self.counting_reader.interface.readSliceShort(&discard_buf) catch {
                // ReadFailed indicates stream is done or error occurred
                break;
            };
            if (bytes_read == 0) break;
        }

        if (self.limitExceeded()) {
            return error.ExpandedSizeLimitExceeded;
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

/// Options controlling streaming extraction.
pub const StreamOptions = struct {
    /// Maximum allowed decompressed size of the archive in bytes, or null for
    /// no limit. Exceeding it aborts extraction with ExpandedSizeLimitExceeded.
    max_expanded_bytes: ?u64 = null,
};

/// Unbundle a compressed tar archive, streaming from input_reader to extract_writer.
/// Returns the total decompressed size of the archive in bytes.
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
    options: StreamOptions,
) UnbundleError!u64 {
    const decompress_reader = DecompressingHashReader.create(
        allocator,
        input_reader,
        expected_hash.*,
        options.max_expanded_bytes,
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
            error.InvalidCharacter,
            error.OutOfMemory,
            error.Overflow,
            error.PaxInvalidAttributeEnd,
            error.PaxNullInKeyword,
            error.PaxNullInValue,
            error.PaxSizeAttrOverflow,
            error.ReadFailed,
            error.StreamTooLong,
            error.TarHeader,
            error.TarHeaderChksum,
            error.TarHeadersTooBig,
            error.TarInsufficientBuffer,
            error.TarNumericValueNegative,
            error.TarNumericValueTooBig,
            error.TarUnsupportedHeader,
            error.UnexpectedEndOfStream,
            => {
                if (decompress_reader.limitExceeded()) return error.ExpandedSizeLimitExceeded;
                return error.InvalidTarHeader;
            },
        };

        const entry = maybe_entry orelse break;
        const file_path = entry.name;

        if (pathHasUnbundleErr(file_path)) |validation_error| {
            if (error_context) |ctx| ctx.* = validation_error;
            return error.InvalidPath;
        }

        switch (entry.kind) {
            .directory => {
                try extract_writer.makeDir(file_path);
                data_extracted = true;
            },
            .file => {
                const file_writer = try extract_writer.createFile(file_path);
                // On the error path, finish the file to release the open-file
                // resource; a secondary OOM here cannot be propagated from the
                // unwind, so it is dropped. The success path commits explicitly
                // below and propagates OOM.
                errdefer extract_writer.finishFile() catch {};

                tar_iterator.streamRemaining(entry, file_writer) catch |err| {
                    if (decompress_reader.limitExceeded()) return error.ExpandedSizeLimitExceeded;
                    return err;
                };
                try file_writer.flush();
                try extract_writer.finishFile();

                data_extracted = true;
            },
            .sym_link => {
                const link_target = entry.link_name;
                if (linkTargetUnbundleErr(link_target)) |reason| {
                    if (error_context) |ctx| {
                        ctx.path = file_path;
                        ctx.reason = reason;
                    }
                    return error.InvalidPath;
                }

                // TODO: Add symlink support to ExtractWriter interface
                data_extracted = true;
            },
        }
    }

    // Verify hash after all data is read
    decompress_reader.verifyComplete() catch |err| switch (err) {
        error.HashMismatch => return error.HashMismatch,
        error.ExpandedSizeLimitExceeded => return error.ExpandedSizeLimitExceeded,
    };

    if (!data_extracted) {
        return error.NoDataExtracted;
    }

    return decompress_reader.expandedBytes();
}

/// Validate a base58-encoded hash string and decode it.
///
/// Returns the decoded hash if valid, or null if invalid.
pub fn validateBase58Hash(base58_str: []const u8) Allocator.Error!?[32]u8 {
    // Valid base58 hash should be 32-44 characters
    if (base58_str.len < 32 or base58_str.len > 44) {
        return null;
    }

    return base58.decode(base58_str) catch return null;
}

/// Unbundle files from a compressed tar archive to a directory.
///
/// The filename parameter should be the base58-encoded blake3 hash + .tar.zst extension.
/// If an InvalidPath error is returned, error_context will contain details about the invalid path.
pub fn unbundle(
    allocator: std.mem.Allocator,
    input_reader: *std.Io.Reader,
    extract_dir: std.Io.Dir,
    io: std.Io,
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

    var dir_writer = DirExtractWriter.init(extract_dir, io, allocator);
    defer dir_writer.deinit();
    _ = try unbundleStream(allocator, input_reader, dir_writer.extractWriter(), &expected_hash, error_context, .{});
}
