//! Executable memory allocation and execution.
//!
//! This module handles allocating memory, making it executable, and calling it.
//! It does NOT handle code generation or relocation patching - that's the
//! caller's responsibility.

const std = @import("std");
const builtin = @import("builtin");

/// A region of memory that can be executed as code.
pub const ExecutableMemory = struct {
    /// The executable memory region
    memory: []align(std.heap.page_size_min) u8,
    /// Size of the actual code (may be less than memory.len due to page alignment)
    code_size: usize,

    const Self = @This();

    /// Allocate executable memory and copy the given code into it.
    /// The code bytes should already have any relocations applied.
    pub fn init(code: []const u8) !Self {
        if (code.len == 0) {
            return error.EmptyCode;
        }

        // Round up to page size
        const page_size = std.heap.page_size_min;
        const alloc_size = std.mem.alignForward(usize, code.len, page_size);

        // Allocate memory with read-write permissions initially
        const memory = try allocateMemory(alloc_size);
        errdefer freeMemory(memory);

        // Copy the code
        @memcpy(memory[0..code.len], code);

        // Make the memory executable (and read-only)
        try makeExecutable(memory);

        return Self{
            .memory = memory,
            .code_size = code.len,
        };
    }

    /// Free the executable memory
    pub fn deinit(self: *Self) void {
        freeMemory(self.memory);
        self.memory = &.{};
        self.code_size = 0;
    }

    /// Get a pointer to the start of the code
    pub fn codePtr(self: *const Self) [*]const u8 {
        return self.memory.ptr;
    }

    /// Call the code as a function that takes no arguments and returns i64
    pub fn callReturnI64(self: *const Self) i64 {
        const func: *const fn () callconv(.c) i64 = @ptrCast(self.memory.ptr);
        return func();
    }

    /// Call the code as a function that takes no arguments and returns u64
    pub fn callReturnU64(self: *const Self) u64 {
        const func: *const fn () callconv(.c) u64 = @ptrCast(self.memory.ptr);
        return func();
    }

    /// Call the code as a function that takes no arguments and returns f64
    pub fn callReturnF64(self: *const Self) f64 {
        const func: *const fn () callconv(.c) f64 = @ptrCast(self.memory.ptr);
        return func();
    }

    /// Call the code as a function that takes a result pointer and returns void.
    /// This is the Roc calling convention for functions that return values.
    pub fn callWithResultPtr(self: *const Self, result_ptr: *anyopaque) void {
        const func: *const fn (*anyopaque) callconv(.c) void = @ptrCast(self.memory.ptr);
        func(result_ptr);
    }

    /// Call the code as a function that takes a result pointer and RocOps pointer.
    pub fn callWithResultPtrAndRocOps(self: *const Self, result_ptr: *anyopaque, roc_ops: *anyopaque) void {
        const func: *const fn (*anyopaque, *anyopaque) callconv(.c) void = @ptrCast(self.memory.ptr);
        func(result_ptr, roc_ops);
    }
};

/// Allocate memory that can be made executable
fn allocateMemory(size: usize) ![]align(std.heap.page_size_min) u8 {
    switch (builtin.os.tag) {
        .macos, .ios, .tvos, .watchos, .linux, .freebsd, .openbsd, .netbsd => {
            const prot = std.posix.PROT.READ | std.posix.PROT.WRITE;
            const flags = std.posix.MAP{ .TYPE = .PRIVATE, .ANONYMOUS = true };
            const result = std.posix.mmap(null, size, prot, flags, -1, 0) catch {
                return error.MmapFailed;
            };
            return @alignCast(result[0..size]);
        },
        .windows => {
            const mem = std.os.windows.VirtualAlloc(
                null,
                size,
                std.os.windows.MEM_COMMIT | std.os.windows.MEM_RESERVE,
                std.os.windows.PAGE_READWRITE,
            ) catch return error.VirtualAllocFailed;
            const ptr: [*]align(std.heap.page_size_min) u8 = @ptrCast(@alignCast(mem));
            return ptr[0..size];
        },
        else => return error.UnsupportedPlatform,
    }
}

/// Make the memory executable
fn makeExecutable(memory: []align(std.heap.page_size_min) u8) !void {
    switch (builtin.os.tag) {
        .macos, .ios, .tvos, .watchos, .linux, .freebsd, .openbsd, .netbsd => {
            const prot = std.posix.PROT.READ | std.posix.PROT.EXEC;
            std.posix.mprotect(memory, prot) catch return error.MprotectFailed;
        },
        .windows => {
            var old_protect: std.os.windows.DWORD = undefined;
            std.os.windows.VirtualProtect(
                memory.ptr,
                memory.len,
                std.os.windows.PAGE_EXECUTE_READ,
                &old_protect,
            ) catch return error.VirtualProtectFailed;
        },
        else => return error.UnsupportedPlatform,
    }
}

/// Free executable memory
fn freeMemory(memory: []align(std.heap.page_size_min) u8) void {
    switch (builtin.os.tag) {
        .macos, .ios, .tvos, .watchos, .linux, .freebsd, .openbsd, .netbsd => {
            std.posix.munmap(memory);
        },
        .windows => {
            const result = std.os.windows.VirtualFree(memory.ptr, 0, std.os.windows.MEM_RELEASE);
            if (@typeInfo(@TypeOf(result)) == .error_union) {
                _ = result catch {};
            }
        },
        else => {},
    }
}

// Tests

test "execute simple x86_64 code" {
    if (builtin.cpu.arch != .x86_64) return error.SkipZigTest;

    // mov eax, 42; ret
    const code = [_]u8{ 0xB8, 0x2A, 0x00, 0x00, 0x00, 0xC3 };

    var mem = try ExecutableMemory.init(&code);
    defer mem.deinit();

    const result = mem.callReturnI64();
    try std.testing.expectEqual(@as(i64, 42), result);
}

test "execute x86_64 return 64-bit value" {
    if (builtin.cpu.arch != .x86_64) return error.SkipZigTest;

    // movabs rax, 0x123456789ABCDEF0; ret
    const code = [_]u8{ 0x48, 0xB8, 0xF0, 0xDE, 0xBC, 0x9A, 0x78, 0x56, 0x34, 0x12, 0xC3 };

    var mem = try ExecutableMemory.init(&code);
    defer mem.deinit();

    const result = mem.callReturnU64();
    try std.testing.expectEqual(@as(u64, 0x123456789ABCDEF0), result);
}

test "execute aarch64 code" {
    if (builtin.cpu.arch != .aarch64) return error.SkipZigTest;

    // mov w0, #42; ret
    const code = [_]u8{ 0x40, 0x05, 0x80, 0x52, 0xC0, 0x03, 0x5F, 0xD6 };

    var mem = try ExecutableMemory.init(&code);
    defer mem.deinit();

    const result = mem.callReturnI64();
    try std.testing.expectEqual(@as(i64, 42), result);
}
