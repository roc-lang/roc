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
    /// Offset from start where execution should begin (for compiled procedures)
    entry_offset: usize,

    const Self = @This();

    /// Allocate executable memory and copy the given code into it.
    /// The code bytes should already have any relocations applied.
    pub fn init(code: []const u8) !Self {
        return initWithEntryOffset(code, 0);
    }

    /// Allocate executable memory with a specific entry offset.
    /// Use this when procedures are compiled before the main expression.
    pub fn initWithEntryOffset(code: []const u8, entry_offset: usize) !Self {
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
            .entry_offset = entry_offset,
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

    /// Get a pointer to the entry point (may differ from codePtr if entry_offset is non-zero)
    pub fn entryPtr(self: *const Self) [*]const u8 {
        return self.memory.ptr + self.entry_offset;
    }

    /// Call the code as a function that takes no arguments and returns i64
    pub fn callReturnI64(self: *const Self) i64 {
        const func: *const fn () callconv(.c) i64 = @ptrCast(@alignCast(self.entryPtr()));
        return func();
    }

    /// Call the code as a function that takes no arguments and returns u64
    pub fn callReturnU64(self: *const Self) u64 {
        const func: *const fn () callconv(.c) u64 = @ptrCast(@alignCast(self.entryPtr()));
        return func();
    }

    /// Call the code as a function that takes no arguments and returns f64
    pub fn callReturnF64(self: *const Self) f64 {
        const func: *const fn () callconv(.c) f64 = @ptrCast(@alignCast(self.entryPtr()));
        return func();
    }

    /// Call the code as a function that takes a result pointer and returns void.
    /// This is the Roc calling convention for functions that return values.
    pub fn callWithResultPtr(self: *const Self, result_ptr: *anyopaque) void {
        const func: *const fn (*anyopaque) callconv(.c) void = @ptrCast(@alignCast(self.entryPtr()));
        func(result_ptr);
    }

    /// Call the code as a function that takes a result pointer and RocOps pointer.
    pub fn callWithResultPtrAndRocOps(self: *const Self, result_ptr: *anyopaque, roc_ops: *anyopaque) void {
        const func: *const fn (*anyopaque, *anyopaque) callconv(.c) void = @ptrCast(@alignCast(self.entryPtr()));
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
        // allocateMemory returns error.UnsupportedPlatform for other OSes,
        // so freeMemory should never be called on them
        else => unreachable,
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

test "execute x86_64 with result ptr" {
    if (builtin.cpu.arch != .x86_64) return error.SkipZigTest;

    // Windows x64: RCX = result_ptr, RDX = roc_ops (ignored)
    // System V: RDI = result_ptr, RSI = roc_ops (ignored)
    // mov qword [arg0], 42; ret
    const code = if (builtin.os.tag == .windows)
        // Windows: mov qword ptr [rcx], 42; ret
        [_]u8{ 0x48, 0xC7, 0x01, 0x2A, 0x00, 0x00, 0x00, 0xC3 }
    else
        // System V: mov qword ptr [rdi], 42; ret
        [_]u8{ 0x48, 0xC7, 0x07, 0x2A, 0x00, 0x00, 0x00, 0xC3 };

    var mem = try ExecutableMemory.init(&code);
    defer mem.deinit();

    var result: i64 = 0;
    var dummy_roc_ops: u64 = 0xDEADBEEF;
    mem.callWithResultPtrAndRocOps(@ptrCast(&result), @ptrCast(&dummy_roc_ops));
    try std.testing.expectEqual(@as(i64, 42), result);
}

test "execute x86_64 with full prologue/epilogue" {
    if (builtin.cpu.arch != .x86_64) return error.SkipZigTest;

    // This mimics the generated code structure:
    // - Prologue: push rbp; mov rbp,rsp; push rbx; push r12; sub rsp,1024
    // - Save args: mov rbx, rcx/rdi (result ptr); mov r12, rdx/rsi (roc_ops - ignored)
    // - Compute: mov rax, 42
    // - Store result: mov [rbx], rax
    // - Epilogue: add rsp,1024; pop r12; pop rbx; pop rbp; ret

    const code = if (builtin.os.tag == .windows) blk: {
        // Windows x64: RCX = result_ptr, RDX = roc_ops
        break :blk [_]u8{
            // Prologue
            0x55, // push rbp
            0x48, 0x89, 0xE5, // mov rbp, rsp
            0x53, // push rbx
            0x41, 0x54, // push r12
            0x48, 0x81, 0xEC, 0x00, 0x04, 0x00, 0x00, // sub rsp, 1024
            // Save args
            0x48, 0x89, 0xCB, // mov rbx, rcx (result ptr)
            0x49, 0x89, 0xD4, // mov r12, rdx (roc_ops)
            // Compute result
            0x48, 0xC7, 0xC0, 0x2A, 0x00, 0x00, 0x00, // mov rax, 42
            // Store to result ptr
            0x48, 0x89, 0x03, // mov [rbx], rax
            // Epilogue
            0x48, 0x81, 0xC4, 0x00, 0x04, 0x00, 0x00, // add rsp, 1024
            0x41, 0x5C, // pop r12
            0x5B, // pop rbx
            0x5D, // pop rbp
            0xC3, // ret
        };
    } else blk: {
        // System V: RDI = result_ptr, RSI = roc_ops
        break :blk [_]u8{
            // Prologue
            0x55, // push rbp
            0x48, 0x89, 0xE5, // mov rbp, rsp
            0x53, // push rbx
            0x41, 0x54, // push r12
            0x48, 0x81, 0xEC, 0x00, 0x04, 0x00, 0x00, // sub rsp, 1024
            // Save args
            0x48, 0x89, 0xFB, // mov rbx, rdi (result ptr)
            0x49, 0x89, 0xF4, // mov r12, rsi (roc_ops)
            // Compute result
            0x48, 0xC7, 0xC0, 0x2A, 0x00, 0x00, 0x00, // mov rax, 42
            // Store to result ptr
            0x48, 0x89, 0x03, // mov [rbx], rax
            // Epilogue
            0x48, 0x81, 0xC4, 0x00, 0x04, 0x00, 0x00, // add rsp, 1024
            0x41, 0x5C, // pop r12
            0x5B, // pop rbx
            0x5D, // pop rbp
            0xC3, // ret
        };
    };

    var mem = try ExecutableMemory.init(&code);
    defer mem.deinit();

    var result: i64 = 0;
    var dummy_roc_ops: u64 = 0xDEADBEEF;
    mem.callWithResultPtrAndRocOps(@ptrCast(&result), @ptrCast(&dummy_roc_ops));
    try std.testing.expectEqual(@as(i64, 42), result);
}
