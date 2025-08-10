const std = @import("std");
const c = @cImport({
    @cInclude("zstd.h");
});

pub fn bundle(
    file_path_iter: anytype,
    compression_level: c_int,
    allocator: std.mem.Allocator,
    output_writer: anytype,
    base_dir: std.fs.Dir,
) !void {
    var buffered_writer = std.io.bufferedWriter(output_writer);
    const buffered = buffered_writer.writer();

    const cctx = c.ZSTD_createCCtx() orelse return error.OutOfMemory;
    defer _ = c.ZSTD_freeCCtx(cctx);
    
    _ = c.ZSTD_CCtx_setParameter(cctx, c.ZSTD_c_compressionLevel, compression_level);
    
    var compressed_buffer = std.ArrayList(u8).init(allocator);
    defer compressed_buffer.deinit();
    
    const out_buffer_size = c.ZSTD_CStreamOutSize();
    var out_buffer = try allocator.alloc(u8, out_buffer_size);
    defer allocator.free(out_buffer);

    while (try file_path_iter.next()) |file_path| {
        const file = try base_dir.openFile(file_path, .{});
        defer file.close();

        const stat = try file.stat();
        const file_size = stat.size;

        // Create tar header manually
        var header: [512]u8 = [_]u8{0} ** 512;
        
        // Name (up to 100 bytes)
        const name_len = @min(file_path.len, 100);
        @memcpy(header[0..name_len], file_path[0..name_len]);
        
        // Mode (octal, 8 bytes)
        _ = try std.fmt.bufPrint(header[100..107], "{o:0>7}", .{0o644});
        header[107] = 0;
        
        // UID and GID (octal, 8 bytes each)
        _ = try std.fmt.bufPrint(header[108..115], "{o:0>7}", .{0});
        header[115] = 0;
        _ = try std.fmt.bufPrint(header[116..123], "{o:0>7}", .{0});
        header[123] = 0;
        
        // Size (octal, 12 bytes)
        _ = try std.fmt.bufPrint(header[124..135], "{o:0>11}", .{file_size});
        header[135] = 0;
        
        // Mtime (octal, 12 bytes)
        const mtime: u64 = 0; // Use zero for reproducible builds
        _ = try std.fmt.bufPrint(header[136..147], "{o:0>11}", .{mtime});
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
        _ = try std.fmt.bufPrint(header[148..155], "{o:0>7}", .{checksum});
        header[155] = 0;
        
        try compressData(cctx, &header, &compressed_buffer, out_buffer);

        var buf: [8192]u8 = undefined;
        var bytes_written: u64 = 0;
        while (bytes_written < file_size) {
            const bytes_read = try file.read(&buf);
            if (bytes_read == 0) break;
            try compressData(cctx, buf[0..bytes_read], &compressed_buffer, out_buffer);
            bytes_written += bytes_read;
        }

        const padding = blockPadding(file_size);
        if (padding > 0) {
            const zeros = [_]u8{0} ** 512;
            try compressData(cctx, zeros[0..padding], &compressed_buffer, out_buffer);
        }
    }

    const trailer = [_]u8{0} ** 1024;
    try compressData(cctx, &trailer, &compressed_buffer, out_buffer);

    // Finalize compression
    var in_buf = c.ZSTD_inBuffer{ .src = "", .size = 0, .pos = 0 };
    var out_buf = c.ZSTD_outBuffer{ .dst = out_buffer.ptr, .size = out_buffer.len, .pos = 0 };
    
    while (true) {
        const remaining = c.ZSTD_compressStream2(cctx, &out_buf, &in_buf, c.ZSTD_e_end);
        if (c.ZSTD_isError(remaining) != 0) {
            return error.CompressionError;
        }
        
        if (out_buf.pos > 0) {
            try compressed_buffer.appendSlice(out_buffer[0..out_buf.pos]);
            out_buf.pos = 0;
        }
        
        if (remaining == 0) break;
    }
    
    // Write all compressed data
    try buffered.writeAll(compressed_buffer.items);
    try buffered_writer.flush();
}

fn blockPadding(size: u64) u64 {
    const remainder = size % 512;
    if (remainder == 0) return 0;
    return 512 - remainder;
}

fn compressData(cctx: *c.ZSTD_CCtx, data: []const u8, buffer: *std.ArrayList(u8), out_buffer: []u8) !void {
    var in_buf = c.ZSTD_inBuffer{ .src = data.ptr, .size = data.len, .pos = 0 };
    var out_buf = c.ZSTD_outBuffer{ .dst = out_buffer.ptr, .size = out_buffer.len, .pos = 0 };
    
    while (in_buf.pos < in_buf.size) {
        const result = c.ZSTD_compressStream2(cctx, &out_buf, &in_buf, c.ZSTD_e_continue);
        if (c.ZSTD_isError(result) != 0) {
            return error.CompressionError;
        }
        
        if (out_buf.pos > 0) {
            try buffer.appendSlice(out_buffer[0..out_buf.pos]);
            out_buf.pos = 0;
        }
    }
}

pub fn unbundle(
    input_reader: anytype,
    extract_dir: std.fs.Dir,
    allocator: std.mem.Allocator,
) !void {
    var buffered_reader = std.io.bufferedReader(input_reader);
    const buffered = buffered_reader.reader();
    
    const dctx = c.ZSTD_createDCtx() orelse return error.OutOfMemory;
    defer _ = c.ZSTD_freeDCtx(dctx);
    
    // Read and decompress data in chunks
    var decompressed_data = std.ArrayList(u8).init(allocator);
    defer decompressed_data.deinit();
    
    const in_buffer_size = c.ZSTD_DStreamInSize();
    const out_buffer_size = c.ZSTD_DStreamOutSize();
    const in_buffer = try allocator.alloc(u8, in_buffer_size);
    defer allocator.free(in_buffer);
    var out_buffer = try allocator.alloc(u8, out_buffer_size);
    defer allocator.free(out_buffer);
    
    while (true) {
        const bytes_read = try buffered.read(in_buffer);
        if (bytes_read == 0) break;
        
        var in_buf = c.ZSTD_inBuffer{ .src = in_buffer.ptr, .size = bytes_read, .pos = 0 };
        
        while (in_buf.pos < in_buf.size) {
            var out_buf = c.ZSTD_outBuffer{ .dst = out_buffer.ptr, .size = out_buffer.len, .pos = 0 };
            
            const result = c.ZSTD_decompressStream(dctx, &out_buf, &in_buf);
            if (c.ZSTD_isError(result) != 0) {
                return error.DecompressionError;
            }
            
            if (out_buf.pos > 0) {
                try decompressed_data.appendSlice(out_buffer[0..out_buf.pos]);
            }
        }
    }
    
    // Create a reader from the decompressed data
    var decompressed_stream = std.io.fixedBufferStream(decompressed_data.items);
    const reader = decompressed_stream.reader();
    
    var header_bytes: [512]u8 = undefined;
    
    while (true) {
        _ = try reader.readAll(&header_bytes);
        
        // Check for end of archive (two consecutive zero blocks)
        if (std.mem.allEqual(u8, &header_bytes, 0)) {
            _ = try reader.readAll(&header_bytes);
            if (std.mem.allEqual(u8, &header_bytes, 0)) {
                break;
            }
        }
        
        // Parse header manually
        const name_end = std.mem.indexOfScalar(u8, header_bytes[0..100], 0) orelse 100;
        const name = header_bytes[0..name_end];
        
        const size_str = header_bytes[124..136];
        const size = try std.fmt.parseInt(u64, std.mem.trimRight(u8, size_str, &[_]u8{ 0, ' ' }), 8);
        
        const typeflag = header_bytes[156];
        
        switch (typeflag) {
            '0', 0 => { // regular file
                // Create parent directories if needed
                if (std.fs.path.dirname(name)) |dir_name| {
                    try extract_dir.makePath(dir_name);
                }
                
                const file = try extract_dir.createFile(name, .{});
                defer file.close();
                
                var bytes_written: u64 = 0;
                var buf: [8192]u8 = undefined;
                while (bytes_written < size) {
                    const to_read = @min(buf.len, size - bytes_written);
                    const bytes_read = try reader.read(buf[0..to_read]);
                    if (bytes_read == 0) return error.UnexpectedEndOfStream;
                    try file.writeAll(buf[0..bytes_read]);
                    bytes_written += bytes_read;
                }
                
                // Skip padding
                const padding = blockPadding(size);
                if (padding > 0) {
                    _ = try reader.skipBytes(padding, .{});
                }
            },
            '5' => { // directory
                try extract_dir.makePath(name);
            },
            else => {
                // Skip unsupported file types
                const total_bytes = size + blockPadding(size);
                _ = try reader.skipBytes(total_bytes, .{});
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
    
    try bundle(&file_iter, 3, allocator, bundle_data.writer(), src_dir); // compression level 3 (default)
    
    // Create destination temp directory
    var dst_tmp = testing.tmpDir(.{});
    defer dst_tmp.cleanup();
    const dst_dir = dst_tmp.dir;
    
    // Unbundle from memory
    var stream = std.io.fixedBufferStream(bundle_data.items);
    try unbundle(stream.reader(), dst_dir, allocator);
    
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
