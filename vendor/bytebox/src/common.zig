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

    fn defaultLog(_: LogLevel, text: [:0]const u8) void {
        std.debug.print("{s}", .{text});
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
            const formatted = std.fmt.bufPrintSentinel(&buf, format ++ "\n", args, 0) catch |e| {
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
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = @ptrCast(&alloc),
                .resize = @ptrCast(&resize),
                .remap = std.mem.Allocator.noRemap,
                .free = @ptrCast(&free),
            },
        };
    }

    pub fn reset(self: *ScratchAllocator) void {
        self.buffer.resize(0) catch unreachable;
    }

    fn alloc(
        self: *ScratchAllocator,
        len: usize,
        alignment: std.mem.Alignment,
        _: usize,
    ) ?[*]u8 {
        const alloc_size = len;
        const offset_begin = std.mem.alignForward(usize, self.buffer.items.len, alignment.toByteUnits());
        const offset_end = offset_begin + alloc_size;
        self.buffer.resize(offset_end) catch {
            return null;
        };
        return self.buffer.items[offset_begin..offset_end].ptr;
    }

    fn resize(
        _: *ScratchAllocator,
        old_mem: []u8,
        _: std.mem.Alignment,
        new_size: usize,
        _: usize,
    ) bool {
        return new_size <= old_mem.len;
    }

    fn free(
        self: *ScratchAllocator,
        _: []u8,
        _: std.mem.Alignment,
        _: usize,
    ) void {
        _ = self;
    }
};
