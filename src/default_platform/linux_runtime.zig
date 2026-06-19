//! Freestanding Linux runtime for the build-only default app platform.
//!
//! This provides raw syscall-based stdout/stderr, allocation, crash reporting,
//! and stack-overflow signal handling without linking libc.

const std = @import("std");
const builtin = @import("builtin");
const linux = std.os.linux;

pub const panic = std.debug.no_panic;

const stdout_fd: i32 = 1;
const stderr_fd: i32 = 2;
const ansi_function_name = "\x1b[94m";
const ansi_reset = "\x1b[0m";
const page_size: usize = 4096;
const allocation_header_words = 3;
const allocation_header_size = allocation_header_words * @sizeOf(usize);
const alt_signal_stack_size: usize = 64 * 1024;
const max_backtrace_frames = 64;
const seamless_slice_tag: usize = 1;

const BacktraceEntry = extern struct {
    start: usize,
    end: usize,
    name_ptr: [*]const u8,
    name_len: usize,
    file_ptr: [*]const u8,
    file_len: usize,
    line: u32,
    column: u32,
};

const roc_default_backtrace_table = @extern(*const [*]const BacktraceEntry, .{
    .name = "roc_default_backtrace_table",
    .linkage = .weak,
});
const roc_default_backtrace_count = @extern(*const usize, .{
    .name = "roc_default_backtrace_count",
    .linkage = .weak,
});

comptime {
    if (builtin.os.tag != .linux) {
        @compileError("default platform Linux runtime must be built for Linux");
    }

    @export(&defaultMemcpy, .{ .name = "memcpy", .linkage = .weak });
    @export(&defaultMemmove, .{ .name = "memmove", .linkage = .weak });
    @export(&defaultMemset, .{ .name = "memset", .linkage = .weak });
    @export(&defaultTrunc, .{ .name = "trunc", .linkage = .weak });
}

export fn roc_default_runtime_init() callconv(.c) void {
    installSignalHandlers();
}

export fn roc_dbg(bytes: [*]const u8, len: usize) callconv(.c) void {
    writeAll(stderr_fd, bytes[0..len]);
    writeLiteral(stderr_fd, "\n");
}

export fn roc_expect_failed(bytes: [*]const u8, len: usize) callconv(.c) noreturn {
    writeLiteral(stderr_fd, "Roc expect failed: ");
    writeAll(stderr_fd, bytes[0..len]);
    writeLiteral(stderr_fd, "\n");
    printBacktrace(@returnAddress(), @frameAddress());
    exitFailure();
}

export fn roc_crashed(bytes: [*]const u8, len: usize) callconv(.c) noreturn {
    writeLiteral(stderr_fd, "Roc application crashed with this message:\n\n\t");
    writeAll(stderr_fd, bytes[0..len]);
    writeLiteral(stderr_fd, "\n\n");
    printBacktrace(@returnAddress(), @frameAddress());
    exitFailure();
}

export fn roc_default_echo_line(str: RocStr) callconv(.c) void {
    var owned = str;
    const message = owned.asSlice();
    writeAll(stdout_fd, message);
    writeLiteral(stdout_fd, "\n");
    owned.decref();
}

export fn roc_default_exit(code: u8) callconv(.c) noreturn {
    linux.exit_group(code);
}

export fn roc_alloc(length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const byte_alignment = normalizedAlignment(alignment);
    const prefix = alignForward(allocation_header_size, byte_alignment);
    const total = pageAlign(prefix + length);
    const raw_addr = linux.mmap(
        null,
        total,
        .{ .READ = true, .WRITE = true },
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    );
    if (linux.errno(raw_addr) != .SUCCESS) return null;

    const raw: [*]u8 = @ptrFromInt(raw_addr);
    const user = raw + prefix;
    storeAllocationHeader(user, prefix, total, length);
    return @ptrCast(user);
}

export fn roc_realloc(ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) ?*anyopaque {
    const old_user: [*]u8 = @ptrCast(ptr);
    const old_len = allocationHeaderValue(old_user, 2);

    const new_ptr = roc_alloc(new_length, alignment) orelse return null;
    const new_user: [*]u8 = @ptrCast(new_ptr);
    const copy_len = @min(old_len, new_length);
    var i: usize = 0;
    while (i < copy_len) : (i += 1) {
        new_user[i] = old_user[i];
    }
    roc_dealloc(ptr, alignment);
    return new_ptr;
}

export fn roc_dealloc(ptr: *anyopaque, _: usize) callconv(.c) void {
    const user: [*]u8 = @ptrCast(ptr);
    const prefix = allocationHeaderValue(user, 0);
    const total = allocationHeaderValue(user, 1);
    const raw = user - prefix;
    _ = linux.munmap(raw, total);
}

fn installSignalHandlers() void {
    const stack_addr = linux.mmap(
        null,
        alt_signal_stack_size,
        .{ .READ = true, .WRITE = true },
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true, .STACK = true },
        -1,
        0,
    );
    if (linux.errno(stack_addr) != .SUCCESS) {
        writeLiteral(stderr_fd, "Roc runtime failed to allocate signal stack\n");
        exitFailure();
    }

    const stack: linux.stack_t = .{
        .sp = @ptrFromInt(stack_addr),
        .flags = 0,
        .size = alt_signal_stack_size,
    };
    if (linux.errno(linux.sigaltstack(&stack, null)) != .SUCCESS) {
        writeLiteral(stderr_fd, "Roc runtime failed to install signal stack\n");
        exitFailure();
    }

    installSignal(.SEGV);
    installSignal(.BUS);
    installSignal(.ILL);
    installSignal(.FPE);
    installSignal(.ABRT);
    installSignal(.TRAP);
}

fn installSignal(sig: linux.SIG) void {
    var action: linux.Sigaction = .{
        .handler = .{ .sigaction = signalHandler },
        .mask = linux.sigemptyset(),
        .flags = linux.SA.SIGINFO | linux.SA.ONSTACK,
    };
    if (linux.errno(linux.sigaction(sig, &action, null)) != .SUCCESS) {
        writeLiteral(stderr_fd, "Roc runtime failed to install signal handler\n");
        exitFailure();
    }
}

fn signalHandler(sig: linux.SIG, _: *const linux.siginfo_t, ctx: ?*anyopaque) callconv(.c) void {
    if (sig == .SEGV) {
        writeLiteral(stderr_fd, "Roc application overflowed its stack memory\n\n");
    } else {
        writeLiteral(stderr_fd, "Roc process terminated by signal ");
        writeUnsigned(stderr_fd, @intFromEnum(sig));
        writeLiteral(stderr_fd, "\n\n");
    }

    if (ctx) |context_ptr| {
        switch (builtin.cpu.arch) {
            .x86_64 => {
                const context: *const X86_64UContext = @ptrCast(@alignCast(context_ptr));
                const rip: usize = @bitCast(context.mcontext.gregs[REG_RIP]);
                const rbp: usize = @bitCast(context.mcontext.gregs[REG_RBP]);
                printBacktrace(rip, rbp);
            },
            .aarch64 => {
                const context: *const Aarch64UContext = @ptrCast(@alignCast(context_ptr));
                const pc: usize = @intCast(context.mcontext.pc);
                const fp: usize = @intCast(context.mcontext.regs[29]);
                printBacktrace(pc, fp);
            },
            else => {},
        }
    }

    exitFailure();
}

const REG_RBP = 10;
const REG_RIP = 16;

const X86_64MContext = extern struct {
    gregs: [23]i64,
    fpregs: usize,
    reserved1: [8]u64,
};

const X86_64UContext = extern struct {
    flags: u64,
    link: ?*anyopaque,
    stack: linux.stack_t,
    mcontext: X86_64MContext,
    sigmask: linux.sigset_t,
};

const Aarch64MContext = extern struct {
    fault_address: u64 align(16),
    regs: [30]u64,
    lr: u64,
    sp: u64,
    pc: u64,
};

const Aarch64UContext = extern struct {
    flags: usize,
    link: ?*anyopaque,
    stack: linux.stack_t,
    sigmask: linux.sigset_t,
    unused: [120]u8,
    mcontext: Aarch64MContext,
};

const Frame = extern struct {
    previous: ?*const Frame,
    return_address: usize,
};

fn printBacktrace(first_ip: usize, first_frame_addr: usize) void {
    if (!hasMappedBacktraceFrame(first_ip, first_frame_addr)) return;

    writeLiteral(stderr_fd, "Backtrace:\n");
    printMappedInstructionPointer(first_ip);
    var frame_addr = firstFrameFromAddress(first_frame_addr);
    var frames: usize = 0;
    while (frame_addr) |frame| : (frames += 1) {
        if (frames >= max_backtrace_frames) break;
        const return_address = frame.return_address;
        if (return_address == 0) break;
        printMappedInstructionPointer(return_address);

        const next = frame.previous orelse break;
        const current_addr = @intFromPtr(frame);
        const next_addr = @intFromPtr(next);
        if (next_addr <= current_addr) break;
        if (next_addr - current_addr > 1024 * 1024) break;
        if ((next_addr & (@alignOf(Frame) - 1)) != 0) break;
        frame_addr = next;
    }
}

fn hasMappedBacktraceFrame(first_ip: usize, first_frame_addr: usize) bool {
    if (lookupBacktraceEntry(first_ip) != null) return true;
    var frame_addr = firstFrameFromAddress(first_frame_addr);
    var frames: usize = 0;
    while (frame_addr) |frame| : (frames += 1) {
        if (frames >= max_backtrace_frames) break;
        const return_address = frame.return_address;
        if (return_address == 0) break;
        if (lookupBacktraceEntry(return_address) != null) return true;

        const next = frame.previous orelse break;
        const current_addr = @intFromPtr(frame);
        const next_addr = @intFromPtr(next);
        if (next_addr <= current_addr) break;
        if (next_addr - current_addr > 1024 * 1024) break;
        if ((next_addr & (@alignOf(Frame) - 1)) != 0) break;
        frame_addr = next;
    }
    return false;
}

fn firstFrameFromAddress(frame_addr: usize) ?*const Frame {
    if (frame_addr == 0) return null;
    if ((frame_addr & (@alignOf(Frame) - 1)) != 0) return null;
    return @ptrFromInt(frame_addr);
}

fn printMappedInstructionPointer(ip: usize) void {
    if (lookupBacktraceEntry(ip)) |entry| {
        writeLiteral(stderr_fd, "  ");
        writeLiteral(stderr_fd, ansi_function_name);
        writeAll(stderr_fd, entry.name_ptr[0..entry.name_len]);
        writeLiteral(stderr_fd, ansi_reset);
        if (entry.file_len != 0) {
            writeLiteral(stderr_fd, " ");
            writeAll(stderr_fd, entry.file_ptr[0..entry.file_len]);
            if (entry.line != 0) {
                writeLiteral(stderr_fd, ":");
                writeUnsigned(stderr_fd, entry.line);
                if (entry.column != 0) {
                    writeLiteral(stderr_fd, ":");
                    writeUnsigned(stderr_fd, entry.column);
                }
            }
        }
        writeLiteral(stderr_fd, "\n");
    }
}

fn lookupBacktraceEntry(ip: usize) ?BacktraceEntry {
    const count_ptr = roc_default_backtrace_count orelse return null;
    const table_ptr = roc_default_backtrace_table orelse return null;
    const count = count_ptr.*;
    const table = table_ptr.*;

    var best: ?BacktraceEntry = null;
    var i: usize = 0;
    while (i < count) : (i += 1) {
        const entry = table[i];
        if (ip < entry.start) continue;
        if (entry.end != 0 and ip >= entry.end) continue;
        if (best == null or entry.start > best.?.start) {
            best = entry;
        }
    }
    return best;
}

fn storeAllocationHeader(user: [*]u8, prefix: usize, total: usize, length: usize) void {
    allocationHeaderPtr(user, 0).* = prefix;
    allocationHeaderPtr(user, 1).* = total;
    allocationHeaderPtr(user, 2).* = length;
}

fn allocationHeaderValue(user: [*]u8, index: usize) usize {
    return allocationHeaderPtr(user, index).*;
}

fn allocationHeaderPtr(user: [*]u8, index: usize) *usize {
    const byte_offset = (allocation_header_words - index) * @sizeOf(usize);
    return @ptrCast(@alignCast(user - byte_offset));
}

fn normalizedAlignment(alignment: usize) usize {
    return @max(alignment, @alignOf(usize));
}

fn alignForward(value: usize, alignment: usize) usize {
    return (value + alignment - 1) & ~(alignment - 1);
}

fn pageAlign(value: usize) usize {
    return alignForward(value, page_size);
}

fn writeLiteral(fd: i32, comptime text: []const u8) void {
    writeAll(fd, text);
}

fn writeAll(fd: i32, bytes: []const u8) void {
    var remaining = bytes;
    while (remaining.len != 0) {
        const written = linux.write(fd, remaining.ptr, remaining.len);
        if (linux.errno(written) != .SUCCESS) return;
        remaining = remaining[written..];
    }
}

fn writeUnsigned(fd: i32, value: anytype) void {
    var buf: [20]u8 = undefined;
    var index = buf.len;
    var n: u64 = @intCast(value);
    if (n == 0) {
        writeLiteral(fd, "0");
        return;
    }
    while (n != 0) {
        index -= 1;
        buf[index] = '0' + @as(u8, @intCast(n % 10));
        n /= 10;
    }
    writeAll(fd, buf[index..]);
}

fn exitFailure() noreturn {
    linux.exit_group(1);
}

fn defaultMemcpy(dest: [*]u8, src: [*]const u8, len: usize) callconv(.c) [*]u8 {
    var i: usize = 0;
    while (i < len) : (i += 1) {
        dest[i] = src[i];
    }
    return dest;
}

fn defaultMemmove(dest: [*]u8, src: [*]const u8, len: usize) callconv(.c) [*]u8 {
    if (@intFromPtr(dest) <= @intFromPtr(src)) {
        var i: usize = 0;
        while (i < len) : (i += 1) {
            dest[i] = src[i];
        }
    } else {
        var i = len;
        while (i != 0) {
            i -= 1;
            dest[i] = src[i];
        }
    }
    return dest;
}

fn defaultMemset(dest: [*]u8, value: c_int, len: usize) callconv(.c) [*]u8 {
    const byte: u8 = @bitCast(@as(i8, @truncate(value)));
    var i: usize = 0;
    while (i < len) : (i += 1) {
        dest[i] = byte;
    }
    return dest;
}

fn defaultTrunc(value: f64) callconv(.c) f64 {
    const bits: u64 = @bitCast(value);
    const exponent_bits = (bits >> 52) & 0x7ff;
    const exponent: i32 = @as(i32, @intCast(exponent_bits)) - 1023;

    if (exponent >= 52) return value;
    if (exponent < 0) return @bitCast(bits & (@as(u64, 1) << 63));

    const fraction_bits: u6 = @intCast(52 - exponent);
    const fraction_mask = (@as(u64, 1) << fraction_bits) - 1;
    return @bitCast(bits & ~fraction_mask);
}

const RocStr = extern struct {
    bytes: ?[*]u8,
    capacity_or_alloc_ptr: usize,
    length: usize,

    fn isSmallStr(self: RocStr) bool {
        return @as(isize, @bitCast(self.length)) < 0;
    }

    fn isSeamlessSlice(self: RocStr) bool {
        return !self.isSmallStr() and (self.capacity_or_alloc_ptr & seamless_slice_tag) == seamless_slice_tag;
    }

    fn len(self: RocStr) usize {
        if (self.isSmallStr()) {
            const raw: *const [@sizeOf(RocStr)]u8 = @ptrCast(&self);
            return raw.*[@sizeOf(RocStr) - 1] ^ 0b1000_0000;
        }
        return self.length;
    }

    fn allocationPtr(self: RocStr) ?[*]u8 {
        if (self.isSmallStr()) return null;
        if (self.isSeamlessSlice()) {
            return @ptrFromInt(self.capacity_or_alloc_ptr & ~seamless_slice_tag);
        }
        return self.bytes;
    }

    fn asSlice(self: *const RocStr) []const u8 {
        const ptr: [*]const u8 = if (self.isSmallStr())
            @ptrCast(self)
        else
            @ptrCast(self.bytes.?);
        return ptr[0..self.len()];
    }

    fn decref(self: *RocStr) void {
        const data = self.allocationPtr() orelse return;
        const refcount_ptr: *isize = @ptrCast(@alignCast(data - @sizeOf(usize)));
        const refcount = refcount_ptr.*;
        if (refcount == 0) return;

        const last = @atomicRmw(isize, refcount_ptr, .Sub, 1, .monotonic);
        if (last == 1) {
            roc_dealloc(data - @sizeOf(usize), @alignOf(usize));
        }
    }
};
