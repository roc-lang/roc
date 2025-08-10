const std = @import("std");
const builtin = @import("builtin");
const c = @cImport({
    @cInclude("zstd.h");
});

pub const BundleError = union(enum) {
    out_of_memory: void,
    file_path_too_long: struct {
        path: []const u8,
        max_len: usize,
    },
    file_open_failed: struct {
        path: []const u8,
        err: anyerror,
    },
    file_stat_failed: struct {
        path: []const u8,
        err: anyerror,
    },
    file_read_failed: struct {
        path: []const u8,
        err: anyerror,
    },
    compression_failed: struct {
        stage: []const u8, // "header", "data", "padding", "trailer", "finalize"
        err: anyerror,
    },
    buffer_too_small: struct {
        field: []const u8, // which tar header field
    },
    write_failed: struct {
        err: anyerror,
    },
    flush_failed: struct {
        err: anyerror,
    },

    pub fn format(
        self: BundleError,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .out_of_memory => try writer.writeAll("out of memory"),
            .file_path_too_long => |e| try writer.print("file path '{s}' exceeds maximum length of {} bytes", .{ e.path, e.max_len }),
            .file_open_failed => |e| try writer.print("failed to open file '{s}': {}", .{ e.path, e.err }),
            .file_stat_failed => |e| try writer.print("failed to stat file '{s}': {}", .{ e.path, e.err }),
            .file_read_failed => |e| try writer.print("failed to read file '{s}': {}", .{ e.path, e.err }),
            .compression_failed => |e| try writer.print("compression failed during {s}: {}", .{ e.stage, e.err }),
            .buffer_too_small => |e| try writer.print("buffer too small for tar header field: {s}", .{e.field}),
            .write_failed => |e| try writer.print("failed to write output: {}", .{e.err}),
            .flush_failed => |e| try writer.print("failed to flush output: {}", .{e.err}),
        }
    }
};

pub const UnbundleError = union(enum) {
    out_of_memory: void,
    decompression_failed: struct {
        err: anyerror,
    },
    invalid_tar_header: struct {
        reason: []const u8,
    },
    unexpected_end_of_stream: struct {
        stage: []const u8, // "input", "header", "data"
    },
    file_create_failed: struct {
        path: []const u8,
        err: anyerror,
    },
    directory_create_failed: struct {
        path: []const u8,
        err: anyerror,
    },
    file_write_failed: struct {
        path: []const u8,
        err: anyerror,
    },
    skip_bytes_failed: struct {
        reason: []const u8, // "padding", "unsupported_type"
        err: anyerror,
    },

    pub fn format(
        self: UnbundleError,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .out_of_memory => try writer.writeAll("out of memory"),
            .decompression_failed => |e| try writer.print("decompression failed: {}", .{e.err}),
            .invalid_tar_header => |e| try writer.print("invalid tar header: {s}", .{e.reason}),
            .unexpected_end_of_stream => |e| try writer.print("unexpected end of stream while reading {s}", .{e.stage}),
            .file_create_failed => |e| try writer.print("failed to create file '{s}': {}", .{ e.path, e.err }),
            .directory_create_failed => |e| try writer.print("failed to create directory '{s}': {}", .{ e.path, e.err }),
            .file_write_failed => |e| try writer.print("failed to write file '{s}': {}", .{ e.path, e.err }),
            .skip_bytes_failed => |e| try writer.print("failed to skip {s}: {}", .{ e.reason, e.err }),
        }
    }
};

pub const UnbundleResult = union(enum) {
    success: void,
    err: UnbundleError,
};

pub const BundleResult = union(enum) {
    success: void,
    err: BundleError,
};

/// Bundle files into a compressed tar archive.
///
/// The file_path_iter must yield file paths that are valid for use with `Dir.openFile`.
/// This means paths must be relative (not absolute), must not contain ".." components,
/// and on Windows must use forward slashes. File paths are limited to 100 bytes for
/// tar compatibility. Paths must be encoded as WTF-8 on Windows, UTF-8 elsewhere.
///
/// Returns structured BundleError with appropriate context on failure.
pub fn bundle(
    file_path_iter: anytype,
    compression_level: c_int,
    allocator: std.mem.Allocator,
    output_writer: anytype,
    base_dir: std.fs.Dir,
) BundleResult {
    // State tracking for error context
    var current_file_path: ?[]const u8 = null;
    var current_stage: []const u8 = "init";

    // Implementation that tracks context
    const result = bundleImpl(&current_file_path, &current_stage, file_path_iter, compression_level, allocator, output_writer, base_dir);

    if (result) |_| {
        return BundleResult{ .success = {} };
    } else |err| {
        // Map errors to structured BundleError based on context
        if (err == error.OutOfMemory) {
            return BundleResult{ .err = .{ .out_of_memory = {} } };
        } else if (err == error.FilePathTooLong) {
            return BundleResult{ .err = .{ .file_path_too_long = .{
                .path = current_file_path orelse "<unknown>",
                .max_len = 100,
            } } };
        } else if (err == error.BufferTooSmall) {
            return BundleResult{ .err = .{ .buffer_too_small = .{
                .field = current_stage,
            } } };
        } else if (err == error.ZstdError) {
            return BundleResult{ .err = .{ .compression_failed = .{
                .stage = current_stage,
                .err = err,
            } } };
        } else if (current_file_path) |path| {
            // Context-aware error handling
            if (std.mem.eql(u8, current_stage, "open")) {
                return BundleResult{ .err = .{ .file_open_failed = .{
                    .path = path,
                    .err = err,
                } } };
            } else if (std.mem.eql(u8, current_stage, "stat")) {
                return BundleResult{ .err = .{ .file_stat_failed = .{
                    .path = path,
                    .err = err,
                } } };
            } else if (std.mem.eql(u8, current_stage, "read")) {
                return BundleResult{ .err = .{ .file_read_failed = .{
                    .path = path,
                    .err = err,
                } } };
            }
        }

        // Default error mappings
        if (std.mem.eql(u8, current_stage, "write")) {
            return BundleResult{ .err = .{ .write_failed = .{ .err = err } } };
        } else if (std.mem.eql(u8, current_stage, "flush")) {
            return BundleResult{ .err = .{ .flush_failed = .{ .err = err } } };
        } else {
            // Fallback
            return BundleResult{ .err = .{ .compression_failed = .{
                .stage = current_stage,
                .err = err,
            } } };
        }
    }
}

fn bundleImpl(
    current_file_path: *?[]const u8,
    current_stage: *[]const u8,
    file_path_iter: anytype,
    compression_level: c_int,
    allocator: std.mem.Allocator,
    output_writer: anytype,
    base_dir: std.fs.Dir,
) !void {
    var buffered_writer = std.io.bufferedWriter(output_writer);
    const buffered = buffered_writer.writer();

    current_stage.* = "create_context";
    const ctx = c.ZSTD_createCCtx() orelse return error.OutOfMemory;
    defer _ = c.ZSTD_freeCCtx(ctx);

    _ = c.ZSTD_CCtx_setParameter(ctx, c.ZSTD_c_compressionLevel, compression_level);

    var compressed_buffer = std.ArrayList(u8).init(allocator);
    defer compressed_buffer.deinit();

    const out_buffer_size = c.ZSTD_CStreamOutSize();
    var out_buffer = allocator.alloc(u8, out_buffer_size) catch {
        return error.OutOfMemory;
    };
    defer allocator.free(out_buffer);

    while (try file_path_iter.next()) |file_path| {
        current_file_path.* = file_path;

        if (file_path.len > 100) {
            return error.FilePathTooLong;
        }

        current_stage.* = "open";
        const file = base_dir.openFile(file_path, .{}) catch |err| {
            return err;
        };
        defer file.close();

        current_stage.* = "stat";
        const stat = file.stat() catch |err| {
            return err;
        };
        const file_size = stat.size;

        // Create tar header manually
        var header: [512]u8 = [_]u8{0} ** 512;

        // Name (up to 100 bytes)
        const name_len = @min(file_path.len, 100);
        @memcpy(header[0..name_len], file_path[0..name_len]);

        // Mode (octal, 8 bytes)
        current_stage.* = "mode";
        _ = std.fmt.bufPrint(header[100..107], "{o:0>7}", .{0o644}) catch {
            return error.BufferTooSmall;
        };
        header[107] = 0;

        // UID and GID (octal, 8 bytes each)
        current_stage.* = "uid";
        _ = std.fmt.bufPrint(header[108..115], "{o:0>7}", .{0}) catch {
            return error.BufferTooSmall;
        };
        header[115] = 0;
        current_stage.* = "gid";
        _ = std.fmt.bufPrint(header[116..123], "{o:0>7}", .{0}) catch {
            return error.BufferTooSmall;
        };
        header[123] = 0;

        // Size (octal, 12 bytes)
        current_stage.* = "size";
        _ = std.fmt.bufPrint(header[124..135], "{o:0>11}", .{file_size}) catch {
            return error.BufferTooSmall;
        };
        header[135] = 0;

        // Mtime (octal, 12 bytes)
        const mtime: u64 = 0; // Use zero for reproducible builds
        current_stage.* = "mtime";
        _ = std.fmt.bufPrint(header[136..147], "{o:0>11}", .{mtime}) catch {
            return error.BufferTooSmall;
        };
        header[147] = 0;

        // Type flag
        header[156] = '0'; // regular file

        // Magic
        @memcpy(header[257..263], "ustar\x00");

        // Version
        @memcpy(header[263..265], "00");

        // Calculate checksum
        @memset(header[148..156], ' ');
        var checksum: u32 = 0;
        for (header) |byte| {
            checksum += byte;
        }
        current_stage.* = "checksum";
        _ = std.fmt.bufPrint(header[148..155], "{o:0>7}", .{checksum}) catch {
            return error.BufferTooSmall;
        };
        header[155] = 0;

        current_stage.* = "header";
        compressData(ctx, &header, &compressed_buffer, out_buffer) catch |err| {
            return err;
        };

        var buf: [8192]u8 = undefined;
        var bytes_written: u64 = 0;
        while (bytes_written < file_size) {
            current_stage.* = "read";
            const bytes_read = file.read(&buf) catch |err| {
                return err;
            };
            if (bytes_read == 0) break;
            current_stage.* = "data";
            compressData(ctx, buf[0..bytes_read], &compressed_buffer, out_buffer) catch |err| {
                return err;
            };
            bytes_written += bytes_read;
        }

        const padding = blockPadding(file_size);
        if (padding > 0) {
            const zeros = [_]u8{0} ** 512;
            current_stage.* = "padding";
            compressData(ctx, zeros[0..padding], &compressed_buffer, out_buffer) catch |err| {
                return err;
            };
        }
    }

    current_file_path.* = null;
    current_stage.* = "trailer";
    const trailer = [_]u8{0} ** 1024;
    compressData(ctx, &trailer, &compressed_buffer, out_buffer) catch |err| {
        return err;
    };

    // Finalize compression
    current_stage.* = "finalize";
    var in_buf = c.ZSTD_inBuffer{ .src = "", .size = 0, .pos = 0 };
    var out_buf = c.ZSTD_outBuffer{ .dst = out_buffer.ptr, .size = out_buffer.len, .pos = 0 };

    while (true) {
        const remaining = c.ZSTD_compressStream2(ctx, &out_buf, &in_buf, c.ZSTD_e_end);
        if (c.ZSTD_isError(remaining) != 0) {
            return error.ZstdError;
        }

        if (out_buf.pos > 0) {
            compressed_buffer.appendSlice(out_buffer[0..out_buf.pos]) catch {
                return error.OutOfMemory;
            };
            out_buf.pos = 0;
        }

        if (remaining == 0) break;
    }

    // Write all compressed data
    current_stage.* = "write";
    buffered.writeAll(compressed_buffer.items) catch |err| {
        return err;
    };
    current_stage.* = "flush";
    buffered_writer.flush() catch |err| {
        return err;
    };
}

fn blockPadding(size: u64) u64 {
    const remainder = size % 512;
    if (remainder == 0) return 0;
    return 512 - remainder;
}

fn compressData(ctx: *c.ZSTD_CCtx, data: []const u8, buffer: *std.ArrayList(u8), out_buffer: []u8) !void {
    var in_buf = c.ZSTD_inBuffer{ .src = data.ptr, .size = data.len, .pos = 0 };
    var out_buf = c.ZSTD_outBuffer{ .dst = out_buffer.ptr, .size = out_buffer.len, .pos = 0 };

    while (in_buf.pos < in_buf.size) {
        const result = c.ZSTD_compressStream2(ctx, &out_buf, &in_buf, c.ZSTD_e_continue);
        if (c.ZSTD_isError(result) != 0) {
            return error.ZstdError;
        }

        if (out_buf.pos > 0) {
            buffer.appendSlice(out_buffer[0..out_buf.pos]) catch {
                return error.OutOfMemory;
            };
            out_buf.pos = 0;
        }
    }
}

/// Unbundle files from a compressed tar archive.
///
/// Extracts files to the provided directory, creating subdirectories as needed.
/// Returns structured UnbundleError with appropriate context on failure.
pub fn unbundle(
    input_reader: anytype,
    extract_dir: std.fs.Dir,
    allocator: std.mem.Allocator,
) UnbundleResult {
    // State tracking for error context
    var current_file_path: ?[]const u8 = null;
    var current_stage: []const u8 = "init";

    // Implementation that tracks context
    const result = unbundleImpl(&current_file_path, &current_stage, input_reader, extract_dir, allocator);

    if (result) |_| {
        return UnbundleResult{ .success = {} };
    } else |err| {
        // Map errors to structured UnbundleError based on context
        if (err == error.OutOfMemory) {
            return UnbundleResult{ .err = .{ .out_of_memory = {} } };
        } else if (err == error.ZstdError) {
            return UnbundleResult{ .err = .{ .decompression_failed = .{ .err = err } } };
        } else if (err == error.InvalidTarHeader) {
            return UnbundleResult{ .err = .{ .invalid_tar_header = .{ .reason = current_stage } } };
        } else if (err == error.UnexpectedEndOfStream) {
            return UnbundleResult{ .err = .{ .unexpected_end_of_stream = .{ .stage = current_stage } } };
        } else if (current_file_path) |path| {
            // Context-aware error handling
            if (std.mem.eql(u8, current_stage, "create_file")) {
                return UnbundleResult{ .err = .{ .file_create_failed = .{
                    .path = path,
                    .err = err,
                } } };
            } else if (std.mem.eql(u8, current_stage, "create_dir")) {
                return UnbundleResult{ .err = .{ .directory_create_failed = .{
                    .path = path,
                    .err = err,
                } } };
            } else if (std.mem.eql(u8, current_stage, "write_file")) {
                return UnbundleResult{ .err = .{ .file_write_failed = .{
                    .path = path,
                    .err = err,
                } } };
            }
        }

        // Default error mappings
        if (std.mem.startsWith(u8, current_stage, "skip_")) {
            return UnbundleResult{
                .err = .{
                    .skip_bytes_failed = .{
                        .reason = current_stage[5..], // Remove "skip_" prefix
                        .err = err,
                    },
                },
            };
        } else {
            // Fallback
            return UnbundleResult{ .err = .{ .decompression_failed = .{ .err = err } } };
        }
    }
}

fn unbundleImpl(
    current_file_path: *?[]const u8,
    current_stage: *[]const u8,
    input_reader: anytype,
    extract_dir: std.fs.Dir,
    allocator: std.mem.Allocator,
) !void {
    var buffered_reader = std.io.bufferedReader(input_reader);
    const buffered = buffered_reader.reader();

    current_stage.* = "create_context";
    const dctx = c.ZSTD_createDCtx() orelse return error.OutOfMemory;
    defer _ = c.ZSTD_freeDCtx(dctx);

    // Read and decompress data in chunks
    var decompressed_data = std.ArrayList(u8).init(allocator);
    defer decompressed_data.deinit();

    const in_buffer_size = c.ZSTD_DStreamInSize();
    const out_buffer_size = c.ZSTD_DStreamOutSize();
    const in_buffer = allocator.alloc(u8, in_buffer_size) catch {
        return error.OutOfMemory;
    };
    defer allocator.free(in_buffer);
    var out_buffer = allocator.alloc(u8, out_buffer_size) catch {
        return error.OutOfMemory;
    };
    defer allocator.free(out_buffer);

    while (true) {
        current_stage.* = "input";
        const bytes_read = buffered.read(in_buffer) catch {
            return error.UnexpectedEndOfStream;
        };
        if (bytes_read == 0) break;

        var in_buf = c.ZSTD_inBuffer{ .src = in_buffer.ptr, .size = bytes_read, .pos = 0 };

        while (in_buf.pos < in_buf.size) {
            var out_buf = c.ZSTD_outBuffer{ .dst = out_buffer.ptr, .size = out_buffer.len, .pos = 0 };

            const result = c.ZSTD_decompressStream(dctx, &out_buf, &in_buf);
            if (c.ZSTD_isError(result) != 0) {
                return error.ZstdError;
            }

            if (out_buf.pos > 0) {
                decompressed_data.appendSlice(out_buffer[0..out_buf.pos]) catch {
                    return error.OutOfMemory;
                };
            }
        }
    }

    // Create a reader from the decompressed data
    var decompressed_stream = std.io.fixedBufferStream(decompressed_data.items);
    const reader = decompressed_stream.reader();

    var header_bytes: [512]u8 = undefined;

    while (true) {
        current_stage.* = "header";
        _ = reader.readAll(&header_bytes) catch {
            return error.UnexpectedEndOfStream;
        };

        // Check for end of archive (two consecutive zero blocks)
        if (std.mem.allEqual(u8, &header_bytes, 0)) {
            _ = reader.readAll(&header_bytes) catch {
                return error.UnexpectedEndOfStream;
            };
            if (std.mem.allEqual(u8, &header_bytes, 0)) {
                break;
            }
        }

        // Parse header manually
        const name_end = std.mem.indexOfScalar(u8, header_bytes[0..100], 0) orelse 100;
        const name = header_bytes[0..name_end];

        const size_str = header_bytes[124..136];
        current_stage.* = "invalid file size";
        const size = std.fmt.parseInt(u64, std.mem.trimRight(u8, size_str, &[_]u8{ 0, ' ' }), 8) catch {
            return error.InvalidTarHeader;
        };

        const typeflag = header_bytes[156];

        switch (typeflag) {
            '0', 0 => { // regular file
                current_file_path.* = name;

                // Create parent directories if needed
                if (std.fs.path.dirname(name)) |dir_name| {
                    current_stage.* = "create_dir";
                    extract_dir.makePath(dir_name) catch |err| {
                        return err;
                    };
                }

                current_stage.* = "create_file";
                const file = extract_dir.createFile(name, .{}) catch |err| {
                    return err;
                };
                defer file.close();

                var bytes_written: u64 = 0;
                var buf: [8192]u8 = undefined;
                while (bytes_written < size) {
                    const to_read = @min(buf.len, size - bytes_written);
                    current_stage.* = "data";
                    const bytes_read = reader.read(buf[0..to_read]) catch {
                        return error.UnexpectedEndOfStream;
                    };
                    if (bytes_read == 0) return error.UnexpectedEndOfStream;
                    current_stage.* = "write_file";
                    file.writeAll(buf[0..bytes_read]) catch |err| {
                        return err;
                    };
                    bytes_written += bytes_read;
                }

                // Skip padding
                const padding = blockPadding(size);
                if (padding > 0) {
                    current_stage.* = "skip_padding";
                    _ = reader.skipBytes(padding, .{}) catch |err| {
                        return err;
                    };
                }
            },
            '5' => { // directory
                current_file_path.* = name;
                current_stage.* = "create_dir";
                extract_dir.makePath(name) catch |err| {
                    return err;
                };
            },
            else => {
                // Skip unsupported file types
                const total_bytes = size + blockPadding(size);
                current_stage.* = "skip_unsupported_type";
                _ = reader.skipBytes(total_bytes, .{}) catch |err| {
                    return err;
                };
            },
        }
    }
}

test "bundle and unbundle roundtrip" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Create source temp directory
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    // Create test files and directories
    {
        const file = try src_dir.createFile("file1.txt", .{});
        defer file.close();
        try file.writeAll("Hello from file1!");
    }
    {
        const file = try src_dir.createFile("file2.txt", .{});
        defer file.close();
        try file.writeAll("This is file2 content.");
    }

    try src_dir.makePath("subdir1");
    {
        const file = try src_dir.createFile("subdir1/nested1.txt", .{});
        defer file.close();
        try file.writeAll("Nested file 1");
    }
    {
        const file = try src_dir.createFile("subdir1/nested2.txt", .{});
        defer file.close();
        try file.writeAll("Another nested file");
    }

    try src_dir.makePath("subdir2/deeply/nested");
    {
        const file = try src_dir.createFile("subdir2/deeply/nested/deep.txt", .{});
        defer file.close();
        try file.writeAll("Deep file content");
    }

    // Collect file paths
    const file_paths = [_][]const u8{
        "file1.txt",
        "file2.txt",
        "subdir1/nested1.txt",
        "subdir1/nested2.txt",
        "subdir2/deeply/nested/deep.txt",
    };

    // Create an iterator for the file paths
    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) !?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };

    // Bundle to memory
    var bundle_data = std.ArrayList(u8).init(allocator);
    defer bundle_data.deinit();

    const bundle_result = bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir); // compression level 3 (default)
    switch (bundle_result) {
        .success => {},
        .err => {
            return error.BundleFailed;
        },
    }

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Unbundle from memory
    var stream = std.io.fixedBufferStream(bundle_data.items);
    const unbundle_result = unbundle(stream.reader(), dst_dir, allocator);
    switch (unbundle_result) {
        .success => {},
        .err => {
            return error.UnbundleFailed;
        },
    }

    // Verify all files exist with correct content
    const file1_content = try dst_dir.readFileAlloc(allocator, "file1.txt", 1024);
    defer allocator.free(file1_content);
    try testing.expectEqualStrings("Hello from file1!", file1_content);

    const file2_content = try dst_dir.readFileAlloc(allocator, "file2.txt", 1024);
    defer allocator.free(file2_content);
    try testing.expectEqualStrings("This is file2 content.", file2_content);

    const nested1_content = try dst_dir.readFileAlloc(allocator, "subdir1/nested1.txt", 1024);
    defer allocator.free(nested1_content);
    try testing.expectEqualStrings("Nested file 1", nested1_content);

    const nested2_content = try dst_dir.readFileAlloc(allocator, "subdir1/nested2.txt", 1024);
    defer allocator.free(nested2_content);
    try testing.expectEqualStrings("Another nested file", nested2_content);

    const deep_content = try dst_dir.readFileAlloc(allocator, "subdir2/deeply/nested/deep.txt", 1024);
    defer allocator.free(deep_content);
    try testing.expectEqualStrings("Deep file content", deep_content);
}

test "bundle and unbundle over socket stream" {
    const testing = std.testing;
    const allocator = testing.allocator;

    // Skip on Windows as Unix sockets aren't supported
    if (builtin.os.tag == .windows) return error.SkipZigTest;

    // Create source temp directory with test files
    var src_tmp = testing.tmpDir(.{});
    defer src_tmp.cleanup();
    const src_dir = src_tmp.dir;

    // Create test files
    {
        const file = try src_dir.createFile("test1.txt", .{});
        defer file.close();
        try file.writeAll("Socket test file 1");
    }
    {
        const file = try src_dir.createFile("test2.txt", .{});
        defer file.close();
        try file.writeAll("This is socket test file 2!");
    }

    try src_dir.makePath("nested");
    {
        const file = try src_dir.createFile("nested/deep.txt", .{});
        defer file.close();
        try file.writeAll("Deep socket test content");
    }

    // Bundle to a file first
    var bundle_tmp = testing.tmpDir(.{});
    defer bundle_tmp.cleanup();

    const bundle_path = "test.bundle";
    const bundle_file = try bundle_tmp.dir.createFile(bundle_path, .{});
    defer bundle_file.close();

    const file_paths = [_][]const u8{
        "test1.txt",
        "test2.txt",
        "nested/deep.txt",
    };

    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) !?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var file_iter = FilePathIterator{ .paths = &file_paths };
    const bundle_result = bundle(&file_iter, 3, allocator, bundle_file.writer(), src_dir);
    switch (bundle_result) {
        .success => {},
        .err => {
            return error.BundleFailed;
        },
    }

    // Create socket in temp directory
    var socket_tmp = testing.tmpDir(.{});
    defer socket_tmp.cleanup();

    // Get the real path of the temp directory
    var real_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const real_path = try socket_tmp.dir.realpath(".", &real_path_buf);

    var socket_path_buf: [std.fs.max_path_bytes]u8 = undefined;
    const socket_path = try std.fmt.bufPrint(&socket_path_buf, "{s}/test.sock", .{real_path});

    // Create server thread
    const ServerContext = struct {
        socket_path: []const u8,
        bundle_path: []const u8,
        bundle_dir: std.fs.Dir,
        ready: std.Thread.ResetEvent = .{},
        done: std.Thread.ResetEvent = .{},

        fn run(ctx: *@This()) !void {
            const server = try std.net.Address.initUnix(ctx.socket_path);
            var listener = try server.listen(.{});
            defer listener.deinit();

            // Signal that server is ready
            ctx.ready.set();

            // Accept one connection
            const connection = try listener.accept();
            defer connection.stream.close();

            // Open and stream the bundle file
            const file = try ctx.bundle_dir.openFile(ctx.bundle_path, .{});
            defer file.close();

            // Stream file contents to socket
            var buf: [4096]u8 = undefined;
            while (true) {
                const bytes_read = try file.read(&buf);
                if (bytes_read == 0) break;
                _ = try connection.stream.writeAll(buf[0..bytes_read]);
            }

            ctx.done.set();
        }
    };

    var server_ctx = ServerContext{
        .socket_path = socket_path,
        .bundle_path = bundle_path,
        .bundle_dir = bundle_tmp.dir,
    };

    const server_thread = try std.Thread.spawn(.{}, ServerContext.run, .{&server_ctx});
    defer server_thread.join();

    // Wait for server to be ready
    server_ctx.ready.wait();

    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;

    // Connect to socket and unbundle
    const stream = try std.net.connectUnixSocket(socket_path);
    defer stream.close();

    // Unbundle from socket stream
    const unbundle_result = unbundle(stream.reader(), dst_dir, allocator);
    switch (unbundle_result) {
        .success => {},
        .err => {
            return error.UnbundleFailed;
        },
    }

    // Wait for server to finish
    server_ctx.done.wait();

    // Verify all files exist with correct content
    const file1_content = try dst_dir.readFileAlloc(allocator, "test1.txt", 1024);
    defer allocator.free(file1_content);
    try testing.expectEqualStrings("Socket test file 1", file1_content);

    const file2_content = try dst_dir.readFileAlloc(allocator, "test2.txt", 1024);
    defer allocator.free(file2_content);
    try testing.expectEqualStrings("This is socket test file 2!", file2_content);

    const deep_content = try dst_dir.readFileAlloc(allocator, "nested/deep.txt", 1024);
    defer allocator.free(deep_content);
    try testing.expectEqualStrings("Deep socket test content", deep_content);
}
