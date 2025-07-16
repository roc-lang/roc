//! Shared memory reading functionality for the Roc runtime.
//! This module provides the exported symbol that host applications can call
//! to read string data from shared memory.

const std = @import("std");

/// Exported symbol that reads a string from temporary file ROC_FILE_TO_INTERPRET
/// Returns the string data to the caller
/// Expected format in temp file: [usize length][u8... data]
export fn rocReadFromSharedMemory(output_ptr: *?[*]u8, output_len: *usize) void {
    const temp_file_path = "/tmp/ROC_FILE_TO_INTERPRET";

    // Try to open the temporary file
    const file = std.fs.openFileAbsolute(temp_file_path, .{}) catch |err| {
        // If we can't open the file, return empty string
        std.debug.print("Failed to open temp file: {}\n", .{err});
        output_ptr.* = null;
        output_len.* = 0;
        return;
    };
    defer file.close();

    // Get the size of the file
    const stat = file.stat() catch |err| {
        std.debug.print("Failed to get file stats: {}\n", .{err});
        output_ptr.* = null;
        output_len.* = 0;
        return;
    };

    if (stat.size < @sizeOf(usize)) {
        std.debug.print("File too small for length header\n", .{});
        output_ptr.* = null;
        output_len.* = 0;
        return;
    }

    // Allocate memory for the file content
    const allocator = std.heap.c_allocator;
    const file_content = allocator.alloc(u8, @intCast(stat.size)) catch |err| {
        std.debug.print("Failed to allocate memory: {}\n", .{err});
        output_ptr.* = null;
        output_len.* = 0;
        return;
    };

    // Read the file content
    _ = file.readAll(file_content) catch |err| {
        std.debug.print("Failed to read file: {}\n", .{err});
        allocator.free(file_content);
        output_ptr.* = null;
        output_len.* = 0;
        return;
    };

    // Read the length from the beginning of the file
    const length_ptr: *align(1) usize = @ptrCast(file_content.ptr);
    const string_length = length_ptr.*;

    // Check if we have enough data
    if (stat.size < @sizeOf(usize) + string_length) {
        std.debug.print("File too small for string data\n", .{});
        allocator.free(file_content);
        output_ptr.* = null;
        output_len.* = 0;
        return;
    }

    // Return pointer to the string data and its length
    output_ptr.* = file_content.ptr + @sizeOf(usize);
    output_len.* = string_length;
}
