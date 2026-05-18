// Lowest layer of the codebase, that contains types and code used in higher layers

const std = @import("std");

pub const StableArray = @import("stable-array").StableArray;

pub const LogLevel = enum(c_int) {
    Info,
    Error,
};

pub const Logger = struct {
    const LogFn = *const fn (level: LogLevel, text: [:0]const u8) void;

    log_fn: ?LogFn,

    pub fn default() Logger {
        return .{
            .log_fn = &defaultLog,
        };
    }

    pub fn empty() Logger {
        return .{
            .log_fn = null,
        };
    }

    fn defaultLog(level: LogLevel, text: [:0]const u8) void {
        var fd = switch (level) {
            .Info => std.fs.File.stdout(),
            .Error => std.fs.File.stderr(),
        };

        var buffer: [1024]u8 = undefined;
        var writer = fd.writer(&buffer);
        const w: *std.io.Writer = &writer.interface;

        nosuspend w.writeAll(text) catch |e| {
            std.debug.print("Failed logging due to error: {}\n", .{e});
        };

        nosuspend w.flush() catch |e| {
            std.debug.print("Failed flushing log due to error: {}\n", .{e});
        };
    }

    pub fn info(self: Logger, comptime format: []const u8, args: anytype) void {
        self.log(.Info, format, args);
    }

    pub fn err(self: Logger, comptime format: []const u8, args: anytype) void {
        self.log(.Error, format, args);
    }

    pub fn log(self: Logger, level: LogLevel, comptime format: []const u8, args: anytype) void {
        if (self.log_fn) |logger| {
            var buf: [2048]u8 = undefined;
            const formatted = std.fmt.bufPrintZ(&buf, format ++ "\n", args) catch |e| {
                std.debug.print("Failed logging due to error: {}\n", .{e});
                return;
            };
            logger(level, formatted);
        }
    }
};

pub const ScratchAllocator = struct {
    buffer: StableArray(u8),

    const InitOpts = struct {
        max_size: usize,
    };

    fn init(opts: InitOpts) ScratchAllocator {
        return ScratchAllocator{
            .buffer = StableArray(u8).init(opts.max_size),
        };
    }

    pub fn allocator(self: *ScratchAllocator) std.mem.Allocator {
        return std.mem.Allocator.init(self, alloc, resize, free);
    }

    pub fn reset(self: *ScratchAllocator) void {
        self.buffer.resize(0) catch unreachable;
    }

    fn alloc(
        self: *ScratchAllocator,
        len: usize,
        ptr_align: u29,
        len_align: u29,
        ret_addr: usize,
    ) std.mem.Allocator.Error![]u8 {
        _ = ret_addr;
        _ = len_align;

        const alloc_size = len;
        const offset_begin = std.mem.alignForward(self.buffer.items.len, ptr_align);
        const offset_end = offset_begin + alloc_size;
        self.buffer.resize(offset_end) catch {
            return std.mem.Allocator.Error.OutOfMemory;
        };
        return self.buffer.items[offset_begin..offset_end];
    }

    fn resize(
        self: *ScratchAllocator,
        old_mem: []u8,
        old_align: u29,
        new_size: usize,
        len_align: u29,
        ret_addr: usize,
    ) ?usize {
        _ = self;
        _ = old_align;
        _ = ret_addr;

        if (new_size > old_mem.len) {
            return null;
        }
        const aligned_size: usize = if (len_align == 0) new_size else std.mem.alignForward(new_size, len_align);
        return aligned_size;
    }

    fn free(
        self: *ScratchAllocator,
        old_mem: []u8,
        old_align: u29,
        ret_addr: usize,
    ) void {
        _ = self;
        _ = old_mem;
        _ = old_align;
        _ = ret_addr;
    }
};
