//! RocOps environment used by compiler-owned evaluation.

const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const interpreter = @import("interpreter.zig");

const RocOps = builtins.host_abi.RocOps;

const CompilerHost = @This();

/// Bookkeeping stored immediately before every allocation handed to Roc
/// code, so a free or reallocation reads the block's size back from the
/// block itself and the live blocks form a list that `deinit` can release,
/// without a lookup table touched on every allocation.
const Header = extern struct {
    prev: ?*Header,
    next: ?*Header,
    size: usize,
    alignment: usize,

    /// The payload follows the header at this offset, which keeps every
    /// supported payload alignment because the block itself is aligned to
    /// `block_alignment`.
    const payload_offset = @sizeOf(Header);
    const block_alignment: std.mem.Alignment = .@"16";

    comptime {
        std.debug.assert(payload_offset % 16 == 0);
    }

    fn fromPayload(ptr: *anyopaque) *Header {
        const bytes: [*]u8 = @ptrCast(ptr);
        return @ptrCast(@alignCast(bytes - payload_offset));
    }

    fn payload(self: *Header) [*]u8 {
        const bytes: [*]u8 = @ptrCast(self);
        return bytes + payload_offset;
    }

    fn block(self: *Header) []align(16) u8 {
        const bytes: [*]align(16) u8 = @ptrCast(@alignCast(self));
        return bytes[0 .. payload_offset + self.size];
    }
};

allocator: std.mem.Allocator,
/// The most recently allocated live block; blocks link through their headers.
live_blocks: ?*Header = null,
debug_messages: std.ArrayList([]u8) = .empty,
roc_ops: ?RocOps = null,
/// How an allocation failure leaves the evaluation this host serves. The
/// host's allocation callbacks never return null, so the interpreter that
/// calls them must be bound before it evaluates anything.
out_of_memory: ?interpreter.OutOfMemoryUnwind = null,
expect_message: ?[]u8 = null,

pub fn init(allocator: std.mem.Allocator) CompilerHost {
    return .{
        .allocator = allocator,
    };
}

pub fn deinit(self: *CompilerHost) void {
    self.freeRemainingAllocations();
    self.clearDebugMessages();
    self.debug_messages.deinit(self.allocator);
    if (self.expect_message) |msg| self.allocator.free(msg);
    self.* = CompilerHost.init(self.allocator);
}

/// Release everything one evaluation left in this host, keeping its RocOps
/// table and interpreter binding for the next evaluation.
pub fn reset(self: *CompilerHost) void {
    self.freeRemainingAllocations();
    self.clearDebugMessages();
    if (self.expect_message) |msg| self.allocator.free(msg);
    self.expect_message = null;
}

/// Return the RocOps table used by compiler-owned evaluation.
pub fn ops(self: *CompilerHost) *RocOps {
    if (self.roc_ops == null) {
        self.roc_ops = .{
            .env = @ptrCast(self),
            .roc_alloc = rocAlloc,
            .roc_dealloc = rocDealloc,
            .roc_realloc = rocRealloc,
            .roc_dbg = rocDbg,
            .roc_expect_failed = rocExpectFailed,
            .roc_crashed = rocCrashed,
            .hosted_fns = builtins.host_abi.emptyHostedFunctions(),
        };
    }
    return &self.roc_ops.?;
}

/// Bind the interpreter whose evaluation an allocation failure unwinds.
pub fn bindInterpreter(self: *CompilerHost, interp: *const interpreter.Interpreter) void {
    self.out_of_memory = interp.outOfMemoryUnwind();
}

/// Return `dbg` messages captured during the current interpreter evaluation.
pub fn debugMessages(self: *const CompilerHost) []const []const u8 {
    return self.debug_messages.items;
}

/// Free and clear all captured `dbg` messages.
pub fn clearDebugMessages(self: *CompilerHost) void {
    for (self.debug_messages.items) |msg| {
        self.allocator.free(msg);
    }
    self.debug_messages.clearRetainingCapacity();
}

fn rocAlloc(roc_ops: *RocOps, length: usize, alignment: usize) callconv(.c) *anyopaque {
    const self: *CompilerHost = @ptrCast(@alignCast(roc_ops.env));
    const header = self.allocateBlock(length, alignment) orelse self.outOfMemory();
    return @ptrCast(header.payload());
}

fn rocDealloc(roc_ops: *RocOps, ptr: *anyopaque, _: usize) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(roc_ops.env));
    self.freeBlock(Header.fromPayload(ptr));
}

fn rocRealloc(roc_ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) *anyopaque {
    const self: *CompilerHost = @ptrCast(@alignCast(roc_ops.env));
    const old_header = Header.fromPayload(ptr);

    // Allocate the new block before releasing the old one, so a failure
    // leaves the old allocation intact and live.
    const new_header = self.allocateBlock(new_length, alignment) orelse self.outOfMemory();
    const copied = @min(old_header.size, new_length);
    @memcpy(new_header.payload()[0..copied], old_header.payload()[0..copied]);
    self.freeBlock(old_header);
    return @ptrCast(new_header.payload());
}

/// Allocate a live block for a payload of `size` bytes at `alignment`.
fn allocateBlock(self: *CompilerHost, size: usize, alignment: usize) ?*Header {
    switch (alignment) {
        1, 2, 4, 8, 16 => {},
        else => base.invariant("{s}", .{"unsupported compiler RocOps allocation alignment"}),
    }
    const block = self.allocator.alignedAlloc(u8, Header.block_alignment, Header.payload_offset + size) catch return null;
    const header: *Header = @ptrCast(@alignCast(block.ptr));
    header.* = .{ .prev = null, .next = self.live_blocks, .size = size, .alignment = alignment };
    if (self.live_blocks) |next| next.prev = header;
    self.live_blocks = header;
    return header;
}

/// Unlink and free one live block.
fn freeBlock(self: *CompilerHost, header: *Header) void {
    if (std.debug.runtime_safety) {
        var cursor = self.live_blocks;
        while (cursor) |live| : (cursor = live.next) {
            if (live == header) break;
        } else base.invariant("{s}", .{"compiler RocOps released a pointer it did not allocate"});
    }
    if (header.prev) |prev| prev.next = header.next else self.live_blocks = header.next;
    if (header.next) |next| next.prev = header.prev;
    self.allocator.free(header.block());
}

/// Turn an allocation failure into a Roc crash that unwinds the bound
/// interpreter's evaluation instead of aborting the whole compiler.
fn outOfMemory(self: *CompilerHost) noreturn {
    const unwind = self.out_of_memory orelse
        base.invariant("{s}", .{"compiler RocOps allocation failed with no interpreter bound"});
    unwind.unwind();
}

fn rocDbg(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(roc_ops.env));
    const owned = self.allocator.dupe(u8, bytes[0..len]) catch return;
    self.debug_messages.append(self.allocator, owned) catch {
        self.allocator.free(owned);
        return;
    };
}

fn rocExpectFailed(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompilerHost = @ptrCast(@alignCast(roc_ops.env));
    if (self.expect_message) |msg| self.allocator.free(msg);
    self.expect_message = self.allocator.dupe(u8, bytes[0..len]) catch null;
}

/// The interpreter reports a Roc crash as `error.Crash` and never calls the
/// host's `roc_crashed`, so reaching this is a compiler bug.
fn rocCrashed(_: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    base.invariant("compiler host invariant violated: roc_crashed called during compiler-owned evaluation: {s}", .{bytes[0..len]});
}

/// Returns null on allocation failure (OOM).
fn freeRemainingAllocations(self: *CompilerHost) void {
    while (self.live_blocks) |header| self.freeBlock(header);
}

test "compiler host declarations are referenced" {
    std.testing.refAllDecls(@This());
}
