const std = @import("std");
const str = @import("str");
const RocStr = str.RocStr;
const testing = std.testing;
const expectEqual = testing.expectEqual;
const expect = testing.expect;

comptime {
    // This is a workaround for https://github.com/ziglang/zig/issues/8218
    // which is only necessary on macOS.
    //
    // Once that issue is fixed, we can undo the changes in
    // 177cf12e0555147faa4d436e52fc15175c2c4ff0 and go back to passing
    // -fcompiler-rt in link.rs instead of doing this. Note that this
    // workaround is present in many host.zig files, so make sure to undo
    // it everywhere!
    if (std.builtin.os.tag == .macos) {
        _ = @import("compiler_rt");
    }
}

const mem = std.mem;
const Allocator = mem.Allocator;

extern fn roc__mainForHost_1_exposed([*]u8) void;
extern fn roc__mainForHost_1_size() i64;
extern fn roc__mainForHost_1_Fx_caller(*const u8, *const u8, [*]u8, [*]u8) void;
extern fn roc__mainForHost_1_Fx_size() i64;
extern fn roc__mainForHost_1_Fx_result_size() i64;

extern fn malloc(size: usize) callconv(.C) ?*c_void;
extern fn realloc(c_ptr: [*]align(@alignOf(u128)) u8, size: usize) callconv(.C) ?*c_void;
extern fn free(c_ptr: [*]align(@alignOf(u128)) u8) callconv(.C) void;

const Unit = extern struct {};

pub export fn main() u8 {
    const stdout = std.io.getStdOut().writer();

    const size = @intCast(usize, roc__mainForHost_1_size());
    const raw_output = std.heap.c_allocator.alloc(u8, size) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    defer {
        std.heap.c_allocator.free(raw_output);
    }

    roc__mainForHost_1_exposed(output);

    const elements = @ptrCast([*]u64, @alignCast(8, output));

    var flag = elements[0];

    if (flag == 0) {
        // all is well
        const function_pointer = @intToPtr(*const u8, elements[1]);
        const closure_data_pointer = @ptrCast([*]u8, output[16..size]);

        call_the_closure(function_pointer, closure_data_pointer);
    } else {
        unreachable;
    }

    return 0;
}

export fn roc_alloc(alignment: usize, size: usize) callconv(.C) *c_void {
    const stdout = std.io.getStdOut().writer();
    const allocator = testing.allocator;

    // Perform the actual malloc
    const startNs = std.time.nanoTimestamp();
    const ptr = malloc(size) orelse unreachable;
    const endNs = std.time.nanoTimestamp();

    const totalMs = @divTrunc(endNs - startNs, 1000);

    stdout.print("\x1B[36m{} | \x1B[39m Custom malloc allocated {} bytes in {} ms!\n", .{startNs, size, totalMs}) catch unreachable;

    return ptr;
}

export fn roc_realloc(alignment: usize, c_ptr: *c_void, old_size: usize, new_size: usize) callconv(.C) *c_void {
    return realloc(@alignCast(16, @ptrCast([*]u8, c_ptr)), new_size) orelse unreachable;
}

export fn roc_dealloc(alignment: usize, c_ptr: *c_void) callconv(.C) void {
    const stdout = std.io.getStdOut().writer();
    const allocator = testing.allocator;

    // Perform the actual free
    const startNs = std.time.nanoTimestamp();
    free(@alignCast(16, @ptrCast([*]u8, c_ptr)));
    const endNs = std.time.nanoTimestamp();

    const totalMs = @divTrunc(endNs - startNs, 1000);

    stdout.print("\x1B[36m{} | \x1B[39m Custom dealloc ran in {} ms!\n", .{startNs, totalMs}) catch unreachable;
}

fn call_the_closure(function_pointer: *const u8, closure_data_pointer: [*]u8) void {
    const size = roc__mainForHost_1_Fx_result_size();
    const raw_output = std.heap.c_allocator.alloc(u8, @intCast(usize, size)) catch unreachable;
    var output = @ptrCast([*]u8, raw_output);

    defer {
        std.heap.c_allocator.free(raw_output);
    }

    const flags: u8 = 0;

    roc__mainForHost_1_Fx_caller(&flags, function_pointer, closure_data_pointer, output);

    const elements = @ptrCast([*]u64, @alignCast(8, output));

    var flag = elements[0];

    if (flag == 0) {
        return;
    } else {
        unreachable;
    }
}

pub export fn roc_fx_putLine(rocPath: str.RocStr) i64 {
    const stdout = std.io.getStdOut().writer();

    const u8_ptr = rocPath.asU8ptr();

    var i: usize = 0;
    while (i < rocPath.len()) {
        stdout.print("{c}", .{u8_ptr[i]}) catch unreachable;

        i += 1;
    }

    stdout.print("\n", .{}) catch unreachable;

    return 0;
}

pub const ReadResult = extern struct {
    bytes: RocStr, // TODO RocList<u8> once Roc supports U8
    errno: i64, // TODO i32 when Roc supports I32
};

pub const WriteResult = extern struct {
    errno: i64,
};

pub export fn roc_fx_readAllUtf8(rocPath: RocStr) callconv(.C) ReadResult {
    var dir = std.fs.cwd();

    var content = dir.readFileAlloc(testing.allocator, rocPath.asSlice(), 1024) catch |e| switch (e) {
        error.FileNotFound => return .{ .bytes = RocStr.empty(), .errno = 2 },
        error.IsDir => return .{ .bytes = RocStr.empty(), .errno = 19 },
        else => return .{ .bytes = RocStr.empty(), .errno = 9999 },
    };

    var str_ptr = @ptrCast([*]u8, content);
    var roc_str3 = RocStr.init(str_ptr, content.len);

    return .{ .bytes = roc_str3, .errno = 0 };
}

pub export fn roc_fx_writeAllUtf8(filePath: RocStr, content: RocStr) callconv(.C) WriteResult {
    var dir = std.fs.cwd();

    dir.writeFile(filePath.asSlice(), content.asSlice()) catch |e| switch (e) {
        else => return .{ .errno = 1 },
    };

    return .{ .errno = 0 };
}

pub fn roc_fx_readAllUtf8_that_does_not_work(rocPath: *RocStr) ReadResult {
    const allocator = std.heap.c_allocator;

    // fopen wants a C string, so stack-allocate one using rocPath's contents
    const len = rocPath.len() + 1;

    var raw = allocator.alloc(u8, len) catch unreachable;
    var path: [*:0]u8 = @ptrCast([*:0]u8, raw);
    rocPath.memcpy(path, len);
    path[len] = 0; // nul-terminate the path, since it's a C string

    // Open the file
    const file = fopen(path, "r") orelse {
        return ReadResult{ .bytes = RocStr.empty(), .errno = errno };
    };

    // Now that the file has been opened, make sure we always (try to) close it
    // before returning, even if something went wrong while reading it.
    defer {
        if (fclose(file) != 0) {
            return ReadResult{ .bytes = RocStr.empty(), .errno = errno };
        }
    }

    // Next we'll count the total number of bytes in the file, which we need
    // to know so we can allocate a correctly-sized buffer to read into.

    // First, seek to the end of the file
    if (fseek(file, 0, SEEK_END) != 0) {
        return ReadResult{ .bytes = RocStr.empty(), .errno = errno };
    }

    // Now the current file position (which ftell returns) will be the end of
    // the file - which will be equal to the total number of bytes in the file.
    const totalBytes: c_long = ftell(file);

    // In the highly unusal case that there are no bytes to read, return early.
    if (totalBytes <= 0) {
        // If the file was empty, return an empty list.
        if (totalBytes == 0) {
            return ReadResult{ .bytes = RocStr.empty(), .errno = 0 };
        }

        // ftell returns -1 on error, so return an error here
        return ReadResult{ .bytes = RocStr.empty(), .errno = errno };
    }

    // Rewind to the beginning of the file, so we can start actually reading.
    if (fseek(file, 0, SEEK_SET) != 0) {
        return ReadResult{ .bytes = RocStr.empty(), .errno = errno };
    }

    // Allocate enough bytes for the contents of the file, plus the refcount.
    const refcountBytes = @sizeOf(usize);
    var buffer: [*]u8 = malloc(totalBytes + refcountBytes) orelse {
        // If allocation failed, throw a runtime exception for Roc to catch.

        // fclose the file explicitly before throwing, because libunwind
        // will disregard our defer block. (TODO verify this!)
        //
        // Silently ignore fclose errors here, because we're about to throw an
        // allocation failure exception; fclose failures won't affect that.
        fclose(file);

        // TODO use libunwind to throw an exception here
        // TODO set an "allocation failed" exception object for `catch` to receive
        // TODO write a test for this which simulates allocation failure
    };

    // Initialize the refcount to a positive number - meaning it's actually
    // a capacity value, which is appropriate since we return a Unique value.
    @ptrCast(buffer, [*]usize)[0] = totalBytes;

    // The buffer pointer should point to the first byte *after* the refcount
    buffer += refcountBytes;

    // Read the bytes into the buffer.
    const bytesRead = fread(buffer, 1, totalBytes, file);

    // fread indicates an error by returning a number that's different from
    // the number of elements we requested to read
    if (bytesRead != totalBytes) {
        return ReadResult{ .bytes = RocStr.empty(), .errno = errno };
    }

    // Explicitly return errno = 0 to indicate there was no error.
    //
    // (We don't want to read from the errno global here because it might have
    // a nonzero value leftover from previous unrelated operations.)
    return ReadResult{ .bytes = RocStr.init(buffer, totalBytes), .errno = 0 };
}

// const c = @cImport({
//     @cInclude("stdio.h");
//     @cInclude("stdlib.h");
// });
//
// extern var errno: c_int;
//
// const FILE = extern struct {
//     unused: u8,
// };

// extern "c" fn fopen(filename: [*:0]const u8, modes: [*:0]const u8) ?*FILE;
//extern "c" fn fopen(filename: [*:0]const u8, modes: [*:0]const u8) ?*FILE;
//extern "c" fn fclose(stream: *FILE) c_int;
//extern "c" fn fseek(stream: *FILE, offset: c_long, origin: c_int) c_int;

// extern fn fopen([*:0]const u8, [*:0]const u8) ?*FILE;
// extern fn fseek(*FILE, c_long, c_int) c_int;

//extern fn fopen([*c]const u8, [*c]const u8) [*c]FILE;
// extern fn ftell([*c]FILE) c_long;
// extern fn fread([*c]u8, size_t, size_t, [*c]FILE) size_t;
// extern fn fclose([*c]FILE) c_int;
