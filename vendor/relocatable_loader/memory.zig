//! Anonymous memory for a loaded image: mapped writable, filled and patched,
//! then its leading code region made executable, following the dev backend's
//! executable-memory handling on each operating system.

const std = @import("std");
const builtin = @import("builtin");

const page_size = std.heap.page_size_min;

const win32 = struct {
    const DWORD = u32;
    const BOOL = c_int;
    const MEM_COMMIT: DWORD = 0x1000;
    const MEM_RESERVE: DWORD = 0x2000;
    const MEM_RELEASE: DWORD = 0x8000;
    const PAGE_READWRITE: DWORD = 0x04;
    const PAGE_EXECUTE_READ: DWORD = 0x20;

    extern "kernel32" fn VirtualAlloc(lpAddress: ?*anyopaque, dwSize: usize, flAllocationType: DWORD, flProtect: DWORD) callconv(.winapi) ?*anyopaque;
    extern "kernel32" fn VirtualFree(lpAddress: *anyopaque, dwSize: usize, dwFreeType: DWORD) callconv(.winapi) BOOL;
    extern "kernel32" fn VirtualProtect(lpAddress: *anyopaque, dwSize: usize, flNewProtect: DWORD, lpflOldProtect: *DWORD) callconv(.winapi) BOOL;
    extern "kernel32" fn GetCurrentProcess() callconv(.winapi) *anyopaque;
    extern "kernel32" fn FlushInstructionCache(hProcess: *anyopaque, lpBaseAddress: ?*const anyopaque, dwSize: usize) callconv(.winapi) BOOL;
};

pub const Mapping = struct {
    base: [*]align(page_size) u8,
    len: usize,

    /// Map `len` bytes of zeroed, writable memory, preferably near `hint`
    /// (an address inside this binary's code) so direct branches between the
    /// image and the host stay in range. The hint is only a preference.
    pub fn map(len: usize, hint: usize) error{MappingFailed}!Mapping {
        const aligned_len = std.mem.alignForward(usize, len, page_size);
        switch (builtin.os.tag) {
            .windows => {
                const preferred: ?*anyopaque = if (hint > (1 << 30)) @ptrFromInt(std.mem.alignBackward(usize, hint - (1 << 28), 1 << 16)) else null;
                const ptr = win32.VirtualAlloc(preferred, aligned_len, win32.MEM_COMMIT | win32.MEM_RESERVE, win32.PAGE_READWRITE) orelse
                    win32.VirtualAlloc(null, aligned_len, win32.MEM_COMMIT | win32.MEM_RESERVE, win32.PAGE_READWRITE) orelse
                    return error.MappingFailed;
                return .{ .base = @ptrCast(@alignCast(ptr)), .len = aligned_len };
            },
            else => {
                const prot: std.posix.PROT = .{ .READ = true, .WRITE = true };
                const flags: std.posix.MAP = .{ .TYPE = .PRIVATE, .ANONYMOUS = true };
                const preferred: ?[*]align(page_size) u8 = if (hint > (1 << 30))
                    @ptrFromInt(std.mem.alignBackward(usize, hint - (1 << 28), page_size))
                else
                    null;
                const region = std.posix.mmap(preferred, aligned_len, prot, flags, -1, 0) catch
                    std.posix.mmap(null, aligned_len, prot, flags, -1, 0) catch return error.MappingFailed;
                return .{ .base = region.ptr, .len = aligned_len };
            },
        }
    }

    /// Make the first `exec_len` bytes (a whole number of pages) executable
    /// and read-only; the rest stays writable.
    pub fn protectCode(self: *const Mapping, exec_len: usize) error{MappingFailed}!void {
        if (exec_len == 0) return;
        std.debug.assert(exec_len % page_size == 0 and exec_len <= self.len);
        switch (builtin.os.tag) {
            .windows => {
                var old: win32.DWORD = undefined;
                if (win32.VirtualProtect(self.base, exec_len, win32.PAGE_EXECUTE_READ, &old) == 0) return error.MappingFailed;
                _ = win32.FlushInstructionCache(win32.GetCurrentProcess(), self.base, exec_len);
            },
            else => {
                const prot: std.posix.PROT = .{ .READ = true, .EXEC = true };
                if (std.c.mprotect(@ptrCast(self.base), exec_len, prot) != 0) return error.MappingFailed;
            },
        }
    }

    pub fn unmap(self: *const Mapping) void {
        switch (builtin.os.tag) {
            .windows => _ = win32.VirtualFree(self.base, 0, win32.MEM_RELEASE),
            else => std.posix.munmap(self.base[0..self.len]),
        }
    }
};
