//! Bundle
//!
//! Future work:
//! - Create a zstd dictionary for roc code (using ~1-10MB of representative roc source code, with the zstd cli;
//!   adds about 110KB to our final binary) and use that. It's a backwards-compatible change (we can keep decoding
//!   dictionary-free .zst files even after we introduce the dictionary)
//! - Changing dictionaries after you've started using one is a breaking change (there's an auto-generated
//!   dictionary ID in the binary, so you know when you're trying to decode with a different dictionary than
//!   the one that the binary was compresed with, and zstd will error), and each time we add new dictionaries
//!   in a nonbreaking way, we have to add +110KB to the `roc` binary, so we should avoid this and instead
//!   only introduce a dictionary when we're confident we'll be happy with that being THE dictionary for a long time.
//! - Compress/Decompress large binary blobs (e.g. for host data, or static List(U8) imports) separately
//!   using different compression params and dictionaires (e.g. make a .tar.zst inside the main .tar.zst)

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
    // First create the tar in memory
    var tar_buffer = std.ArrayList(u8).init(allocator);
    defer tar_buffer.deinit();

    // Create tar writer
    var tar_writer = std.tar.writer(tar_buffer.writer());

    // Write files to tar
    while (try file_path_iter.next()) |file_path| {
        current_file_path.* = file_path;

        if (file_path.len > 255) {
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

        current_stage.* = "read";
        const file_content = allocator.alloc(u8, stat.size) catch {
            return error.OutOfMemory;
        };
        defer allocator.free(file_content);

        // Seek to beginning of file before reading
        file.seekTo(0) catch |err| {
            return err;
        };
        const bytes_read = file.readAll(file_content) catch |err| {
            return err;
        };
        if (bytes_read != stat.size) {
            return error.UnexpectedEndOfStream;
        }

        current_stage.* = "tar_write";
        // Use mtime of 0 for reproducible builds
        const Options = @TypeOf(tar_writer).Options;
        const options = Options{
            .mode = 0o644,
            .mtime = 0,
        };
        tar_writer.writeFileBytes(file_path, file_content, options) catch |err| {
            return err;
        };
    }

    // Finish the tar archive
    current_stage.* = "tar_finish";
    tar_writer.finish() catch |err| {
        return err;
    };

    // Now compress the tar data
    var buffered_writer = std.io.bufferedWriter(output_writer);
    const buffered = buffered_writer.writer();

    current_stage.* = "create_context";
    const ctx = c.ZSTD_createCCtx() orelse return error.OutOfMemory;
    defer _ = c.ZSTD_freeCCtx(ctx);

    _ = c.ZSTD_CCtx_setParameter(ctx, c.ZSTD_c_compressionLevel, compression_level);

    const out_buffer_size = c.ZSTD_CStreamOutSize();
    var out_buffer = allocator.alloc(u8, out_buffer_size) catch {
        return error.OutOfMemory;
    };
    defer allocator.free(out_buffer);

    // Compress the tar data
    current_stage.* = "compress";
    var in_buf = c.ZSTD_inBuffer{ .src = tar_buffer.items.ptr, .size = tar_buffer.items.len, .pos = 0 };
    var out_buf = c.ZSTD_outBuffer{ .dst = out_buffer.ptr, .size = out_buffer.len, .pos = 0 };

    // Compress all data
    while (in_buf.pos < in_buf.size) {
        const result = c.ZSTD_compressStream2(ctx, &out_buf, &in_buf, c.ZSTD_e_continue);
        if (c.ZSTD_isError(result) != 0) {
            return error.ZstdError;
        }

        if (out_buf.pos > 0) {
            buffered.writeAll(out_buffer[0..out_buf.pos]) catch |err| {
                return err;
            };
            out_buf.pos = 0;
        }
    }

    // Finalize compression
    current_stage.* = "finalize";
    in_buf = c.ZSTD_inBuffer{ .src = "", .size = 0, .pos = 0 };
    while (true) {
        const remaining = c.ZSTD_compressStream2(ctx, &out_buf, &in_buf, c.ZSTD_e_end);
        if (c.ZSTD_isError(remaining) != 0) {
            return error.ZstdError;
        }

        if (out_buf.pos > 0) {
            buffered.writeAll(out_buffer[0..out_buf.pos]) catch |err| {
                return err;
            };
            out_buf.pos = 0;
        }

        if (remaining == 0) break;
    }

    current_stage.* = "flush";
    buffered_writer.flush() catch |err| {
        return err;
    };
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

    // Buffered reader for input
    var buffered_reader = std.io.bufferedReader(input_reader);
    const buffered = buffered_reader.reader();

    current_stage = "create_context";
    const dctx = c.ZSTD_createDCtx() orelse {
        return UnbundleResult{ .err = .{ .out_of_memory = {} } };
    };
    defer _ = c.ZSTD_freeDCtx(dctx);

    // Read and decompress data in chunks
    var decompressed_data = std.ArrayList(u8).init(allocator);
    defer decompressed_data.deinit();

    const in_buffer_size = c.ZSTD_DStreamInSize();
    const out_buffer_size = c.ZSTD_DStreamOutSize();
    const in_buffer = allocator.alloc(u8, in_buffer_size) catch {
        return UnbundleResult{ .err = .{ .out_of_memory = {} } };
    };
    defer allocator.free(in_buffer);
    var out_buffer = allocator.alloc(u8, out_buffer_size) catch {
        return UnbundleResult{ .err = .{ .out_of_memory = {} } };
    };
    defer allocator.free(out_buffer);

    // Decompress the entire stream
    while (true) {
        current_stage = "input";
        const bytes_read = buffered.read(in_buffer) catch {
            return UnbundleResult{ .err = .{ .unexpected_end_of_stream = .{ .stage = current_stage } } };
        };
        if (bytes_read == 0) break;

        var in_buf = c.ZSTD_inBuffer{ .src = in_buffer.ptr, .size = bytes_read, .pos = 0 };

        while (in_buf.pos < in_buf.size) {
            var out_buf = c.ZSTD_outBuffer{ .dst = out_buffer.ptr, .size = out_buffer.len, .pos = 0 };

            const result = c.ZSTD_decompressStream(dctx, &out_buf, &in_buf);
            if (c.ZSTD_isError(result) != 0) {
                return UnbundleResult{ .err = .{ .decompression_failed = .{ .err = error.ZstdError } } };
            }

            if (out_buf.pos > 0) {
                decompressed_data.appendSlice(out_buffer[0..out_buf.pos]) catch {
                    return UnbundleResult{ .err = .{ .out_of_memory = {} } };
                };
            }
        }
    }

    // Create a reader from the decompressed data
    var decompressed_stream = std.io.fixedBufferStream(decompressed_data.items);
    const tar_reader = decompressed_stream.reader();

    // Use std.tar to parse the archive
    var file_name_buffer: [256]u8 = undefined;
    var link_name_buffer: [256]u8 = undefined;
    var tar_iter = std.tar.iterator(tar_reader, .{
        .file_name_buffer = &file_name_buffer,
        .link_name_buffer = &link_name_buffer,
    });

    // Process each file in the archive
    while (true) {
        current_stage = "header";
        const file = tar_iter.next() catch |err| {
            if (err == error.EndOfStream) break;
            return UnbundleResult{ .err = .{ .invalid_tar_header = .{ .reason = @errorName(err) } } };
        };

        if (file == null) break;
        const tar_file = file.?;

        current_file_path = tar_file.name;

        switch (tar_file.kind) {
            .file => {
                // Create parent directories if needed
                if (std.fs.path.dirname(tar_file.name)) |dir_name| {
                    current_stage = "create_dir";
                    extract_dir.makePath(dir_name) catch |err| {
                        return UnbundleResult{ .err = .{ .directory_create_failed = .{
                            .path = dir_name,
                            .err = err,
                        } } };
                    };
                }

                current_stage = "create_file";
                const out_file = extract_dir.createFile(tar_file.name, .{}) catch |err| {
                    return UnbundleResult{ .err = .{ .file_create_failed = .{
                        .path = tar_file.name,
                        .err = err,
                    } } };
                };
                defer out_file.close();

                // Copy file contents
                current_stage = "write_file";
                const reader = tar_iter.reader;
                var buf: [8192]u8 = undefined;
                var bytes_remaining = tar_file.size;
                while (bytes_remaining > 0) {
                    const to_read = @min(buf.len, bytes_remaining);
                    const bytes_read = reader.read(buf[0..to_read]) catch |err| {
                        return UnbundleResult{ .err = .{ .file_write_failed = .{
                            .path = tar_file.name,
                            .err = err,
                        } } };
                    };
                    if (bytes_read == 0) break;
                    out_file.writeAll(buf[0..bytes_read]) catch |err| {
                        return UnbundleResult{ .err = .{ .file_write_failed = .{
                            .path = tar_file.name,
                            .err = err,
                        } } };
                    };
                    bytes_remaining -= bytes_read;
                }
            },
            .directory => {
                current_stage = "create_dir";
                extract_dir.makePath(tar_file.name) catch |err| {
                    return UnbundleResult{ .err = .{ .directory_create_failed = .{
                        .path = tar_file.name,
                        .err = err,
                    } } };
                };
            },
            else => {
                // Skip other file types (symlinks, etc.)
                current_stage = "skip_unsupported_type";
                // std.tar automatically handles skipping the content for us
            },
        }
    }

    return UnbundleResult{ .success = {} };
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
