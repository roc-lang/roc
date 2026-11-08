//! RocOps environment for native dev-backend compile-time evaluation.
//!
//! Each compile-time root gets its own host env. Runtime allocations are freed
//! when Roc frees them, and whatever a root still holds when it finishes is
//! bulk-freed after the result has been copied into the checked ConstStore. Host effects and crash state are recorded on the env so
//! the finalizer can replay them deterministically in root request order.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const builtins = @import("builtins");

const sljmp = @import("sljmp");
const crash_boundary_mod = @import("crash_boundary.zig");
const signal_handler = base.signal_handler;

const Allocator = std.mem.Allocator;
const RocOps = builtins.host_abi.RocOps;
const JmpBuf = sljmp.JmpBuf;
const longjmp = sljmp.longjmp;

const CompileTimeHost = @This();

/// Every compile-time allocation starts with a header recording its byte
/// length and alignment; reallocation and deallocation read the block's length
/// from it.
///
/// In safety-checked builds the Roc bytes sit `alignment` bytes past the
/// header region, so their address is a multiple of the requested alignment
/// and never of twice it. Code that leans on more alignment than it asked for
/// then misbehaves here instead of only on a host whose allocator differs,
/// and a free or reallocation at a different alignment than the allocation
/// fails loudly: production hosts locate a block from the alignment they are
/// given. Release builds place the bytes right after the header region and
/// check nothing.
const allocation_header_bytes = 32;
const max_allocation_alignment = 16;
/// The most bytes a block sets aside between its header region and Roc's
/// bytes. Every block reserves this much, so a block's size follows from its
/// length alone and a reused block fits a request of any alignment.
const max_payload_skew = if (std.debug.runtime_safety) max_allocation_alignment else 0;
/// Debug-only stamp a live header carries, so a pointer Roc hands back that
/// no compile-time allocation produced, or hands back twice, is caught.
const allocation_header_stamp: u64 = 0x5f6d_6f63_7074_696d;

/// The largest allocation request compile-time evaluation for a program of
/// `address_space` grants Roc code. A 32-bit program's `roc_alloc` cannot be
/// asked for more bytes than its `usize` holds, so evaluating for one runs out
/// of address space past them; a 64-bit program's evaluation is refused only
/// what the compiler's memory cannot satisfy (design.md "Compile-Time
/// Constants").
pub fn maxAllocationBytes(address_space: base.target.TargetUsize) usize {
    return switch (address_space) {
        .u32 => std.math.maxInt(u32),
        .u64 => std.math.maxInt(usize),
    };
}

/// The crash message of a compile-time allocation that was refused.
const out_of_memory_message = "ran out of memory";

const AllocationHeader = extern struct {
    size: usize,
    alignment: usize,
    stamp: u64,
};

/// A small block is carved from the root's arena at a power-of-two size,
/// header included, and a freed one waits on its size class's free list for
/// the next request of that class. The arena takes no lock, so the many
/// small allocations Roc makes never contend across roots.
const min_small_block_log2 = 6;
const max_small_block_log2 = 15;
const small_class_count = max_small_block_log2 - min_small_block_log2 + 1;
const max_small_payload = (1 << max_small_block_log2) - allocation_header_bytes - max_payload_skew;

/// A freed small block's header, linking it into its class's free list. Its
/// stamp overlays the live header's, so freeing a block clears the stamp.
const FreeBlock = extern struct {
    next: ?*FreeBlock,
    unused: usize,
    stamp: u64,
};

/// A large block comes from the backing allocator and goes back to it when
/// Roc frees it. Its links precede its allocation header, so the live large
/// blocks form a list a reset can release without a table.
const LargeLinks = extern struct {
    prev: ?*LargeLinks,
    next: ?*LargeLinks,
};
const large_links_bytes = allocation_header_bytes;
const large_block_overhead = large_links_bytes + allocation_header_bytes + max_payload_skew;

comptime {
    std.debug.assert(@sizeOf(AllocationHeader) <= allocation_header_bytes);
    std.debug.assert(@sizeOf(FreeBlock) <= allocation_header_bytes);
    std.debug.assert(@offsetOf(FreeBlock, "stamp") == @offsetOf(AllocationHeader, "stamp"));
    std.debug.assert(@sizeOf(LargeLinks) <= large_links_bytes);
    std.debug.assert(std.math.isPowerOfTwo(allocation_header_bytes));
    std.debug.assert(max_allocation_alignment < allocation_header_bytes);
}

/// How a native compile-time root finished.
pub const Termination = enum {
    returned,
    crashed,
    host_oom,
};

/// Where in generated code a failure happened, as the machine left it. The
/// evaluating program resolves it to the failing statement and its meaning
/// through its line table and failure sites (design.md "Compile-Time Failure
/// Sites").
pub const FailurePoint = struct {
    /// The program counter a fault stopped at, when it stopped in generated
    /// code.
    fault: ?usize = null,
    /// The return addresses into generated code on the frame-pointer chain,
    /// innermost first: the generated call the failure came out of, then each
    /// generated call enclosing it.
    calls: []const usize = &.{},
};

/// A failed inline `expect` observed during native compile-time evaluation,
/// with the point in generated code it was reported from.
pub const ExpectFailedEvent = struct {
    message: []u8,
    point: FailurePoint,
};

/// Root-local host effects captured during native compile-time evaluation.
pub const HostEvent = union(enum) {
    dbg: []u8,
    expect_failed: ExpectFailedEvent,
    crashed: []u8,

    pub fn bytes(self: HostEvent) []const u8 {
        return switch (self) {
            inline .dbg, .crashed => |msg| msg,
            .expect_failed => |event| event.message,
        };
    }
};

arena: base.SingleThreadArena,
host_arena: base.SingleThreadArena,
/// Freed small blocks by size class, ready for reuse.
free_lists: [small_class_count]?*FreeBlock = @splat(null),
/// Live large blocks, most recently allocated first.
large_blocks: ?*LargeLinks = null,
roc_ops: ?RocOps = null,
events: std.ArrayListUnmanaged(HostEvent) = .empty,
/// The address range of the generated code this host runs.
code_start: usize = 0,
code_end: usize = 0,
/// The loaded objects a linked program's code calls into, which a failure's
/// frames run through too. Borrowed from the evaluation.
linked_code: []const []const u8 = &.{},
/// The frame that called into generated code; every generated frame lies
/// below it on the stack.
entry_frame: usize = 0,
/// Where the run failed, once it has.
failure: ?FailurePoint = null,
timing_io: ?std.Io = null,
jmp_buf: JmpBuf = undefined,
active_jmp_buf: ?*JmpBuf = null,
termination: Termination = .returned,
/// The largest allocation request Roc code is granted (`maxAllocationBytes`).
max_allocation_bytes: usize,

/// A host for evaluating the roots of a program whose pointers are
/// `address_space` wide.
pub fn init(allocator: Allocator, address_space: base.target.TargetUsize) CompileTimeHost {
    const backing_allocator = hostBytesAllocator(allocator);
    return .{
        .arena = base.SingleThreadArena.init(backing_allocator),
        .host_arena = base.SingleThreadArena.init(backing_allocator),
        .max_allocation_bytes = maxAllocationBytes(address_space),
    };
}

pub fn deinit(self: *CompileTimeHost) void {
    self.clearHostState();
    self.freeLargeBlocks();
    self.host_arena.deinit();
    self.arena.deinit();
    self.* = undefined;
}

/// Return the RocOps table passed to the generated root wrapper.
pub fn ops(self: *CompileTimeHost) *RocOps {
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

/// Clear per-run state and release root-local compile-time host memory.
pub fn resetForRun(self: *CompileTimeHost) void {
    self.clearHostState();
    _ = self.host_arena.reset(.free_all);
    self.failure = null;
    self.freeLargeBlocks();
    self.free_lists = @splat(null);
    _ = self.arena.reset(.free_all);
    self.termination = .returned;
    self.active_jmp_buf = null;
}

/// setjmp/longjmp boundary for catching root-local crashes.
pub const CrashBoundary = crash_boundary_mod.CrashBoundary(CompileTimeHost, true);

/// Record a caught stack overflow exactly the way a Roc `crash` is recorded,
/// so the finalizer reports it as this root's compile-time crash: at the
/// faulting instruction when it is generated code, otherwise at the generated
/// call the fault happened inside of.
pub fn noteStackOverflow(self: *CompileTimeHost) void {
    if (signal_handler.takeOverflowFrame()) |frame| {
        self.failure = .{
            .fault = if (self.inGeneratedCode(frame.pc)) frame.pc else null,
            .calls = self.generatedCalls(frame.fp, frame.sp),
        };
    }
    self.appendEvent(.crashed, crash_boundary_mod.STACK_OVERFLOW_MESSAGE);
    self.termination = .crashed;
}

/// Set the generated code this host runs, for locating its failures, and
/// the loaded objects it calls into.
pub fn setGeneratedCode(self: *CompileTimeHost, code: []const u8, linked_code: []const []const u8) void {
    self.code_start = @intFromPtr(code.ptr);
    self.code_end = self.code_start + code.len;
    self.linked_code = linked_code;
}

fn inGeneratedCode(self: *const CompileTimeHost, address: usize) bool {
    if (address >= self.code_start and address < self.code_end) return true;
    for (self.linked_code) |code| {
        if (address >= @intFromPtr(code.ptr) and address < @intFromPtr(code.ptr) + code.len) return true;
    }
    return false;
}

/// Every return address into generated code on the frame-pointer chain that
/// starts at `frame`, innermost first. Generated code and the builtins and
/// host functions between it and a failure keep frame pointers, so the chain
/// is intact; every record read lies between `low` and the frame that called
/// into generated code, which is live stack.
fn generatedCalls(self: *CompileTimeHost, frame: usize, low: usize) []const usize {
    var calls = std.ArrayListUnmanaged(usize).empty;
    self.appendGeneratedCalls(&calls, frame, low);
    return calls.items;
}

fn appendGeneratedCalls(self: *CompileTimeHost, calls: *std.ArrayListUnmanaged(usize), frame: usize, low: usize) void {
    var fp = frame;
    while (fp % @alignOf(usize) == 0 and fp >= low and fp + 2 * @sizeOf(usize) <= self.entry_frame) {
        const record: *const [2]usize = @ptrFromInt(fp);
        if (self.inGeneratedCode(record[1])) calls.append(self.host_arena.allocator(), record[1]) catch self.jump(.host_oom);
        if (record[0] <= fp) break;
        fp = record[0];
    }
}

/// Where in generated code the failure being reported from this host
/// callback happened.
fn currentFailurePoint(self: *CompileTimeHost, frame: usize) FailurePoint {
    if (comptime builtin.os.tag == .windows and builtin.cpu.arch == .x86_64) return self.windowsX64FailurePoint();
    return .{ .calls = self.generatedCalls(frame, frame) };
}

/// On Windows x64, compiled host and builtin code points its frame pointer
/// into the middle of its frame rather than at a saved-frame-pointer record,
/// so the frames between this callback and generated code are unwound with
/// the system unwind tables. Generated frames are frame-pointer records, so
/// the chain is walked from the innermost one.
fn windowsX64FailurePoint(self: *CompileTimeHost) FailurePoint {
    const windows = std.os.windows;
    var context: windows.CONTEXT = undefined;
    windows.ntdll.RtlCaptureContext(&context);
    var history: windows.UNWIND_HISTORY_TABLE = undefined;
    @memset(@as(*[@sizeOf(windows.UNWIND_HISTORY_TABLE)]u8, @ptrCast(&history)), 0);
    while (!self.inGeneratedCode(context.Rip)) {
        if (context.Rsp >= self.entry_frame) return .{};
        var image_base: usize = undefined;
        if (windows.ntdll.RtlLookupFunctionEntry(context.Rip, &image_base, &history)) |function| {
            var handler_data: ?*anyopaque = null;
            var establisher_frame: usize = undefined;
            _ = windows.ntdll.RtlVirtualUnwind(windows.UNW_FLAG_NHANDLER, image_base, context.Rip, function, &context, &handler_data, &establisher_frame, null);
        } else {
            context.Rip = @as(*const usize, @ptrFromInt(context.Rsp)).*;
            context.Rsp += @sizeOf(usize);
        }
    }
    var calls = std.ArrayListUnmanaged(usize).empty;
    calls.append(self.host_arena.allocator(), context.Rip) catch self.jump(.host_oom);
    self.appendGeneratedCalls(&calls, context.Rbp, context.Rsp);
    return .{ .calls = calls.items };
}

/// Install a crash boundary before calling generated root code.
pub fn enterCrashBoundary(self: *CompileTimeHost) CrashBoundary {
    return CrashBoundary.init(self);
}

/// Return the latest captured Roc crash message, if any.
pub fn crashMessage(self: *const CompileTimeHost) ?[]const u8 {
    for (0..self.events.items.len) |i| {
        const idx = self.events.items.len - 1 - i;
        switch (self.events.items[idx]) {
            .crashed => |msg| return msg,
            .dbg, .expect_failed => {},
        }
    }
    return null;
}

fn clearHostState(self: *CompileTimeHost) void {
    self.events = .empty;
}

fn appendEvent(self: *CompileTimeHost, comptime tag: std.meta.Tag(HostEvent), bytes: []const u8) void {
    const owned = self.dupeEventBytes(bytes);
    const host_allocator = self.host_arena.allocator();
    self.events.append(host_allocator, @unionInit(HostEvent, @tagName(tag), owned)) catch self.jump(.host_oom);
}

fn appendExpectFailedEvent(self: *CompileTimeHost, bytes: []const u8, point: FailurePoint) void {
    const owned = self.dupeEventBytes(bytes);
    const host_allocator = self.host_arena.allocator();
    self.events.append(host_allocator, .{ .expect_failed = .{
        .message = owned,
        .point = point,
    } }) catch self.jump(.host_oom);
}

fn dupeEventBytes(self: *CompileTimeHost, bytes: []const u8) []u8 {
    const host_allocator = self.host_arena.allocator();
    return host_allocator.dupe(u8, bytes) catch self.jump(.host_oom);
}

fn jump(self: *CompileTimeHost, termination: Termination) noreturn {
    self.termination = termination;
    if (sljmp.supported) {
        if (self.active_jmp_buf) |active_jmp_buf| {
            self.active_jmp_buf = null;
            longjmp(active_jmp_buf, 1);
        }
    }
    base.invariant("{s}", .{"compile-time host failure escaped without an active crash boundary"});
}

fn rocAlloc(roc_ops: *RocOps, length: usize, alignment: usize) callconv(.c) *anyopaque {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    return @ptrCast(self.allocateRocBytes(length, alignment));
}

fn rocDealloc(roc_ops: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.freeRocBytes(ptr, alignment, "dealloc");
}

fn rocRealloc(roc_ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) *anyopaque {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    const header = allocationHeader(ptr);
    assertLive(ptr, alignment, "realloc");
    const old_size = header.size;
    const new_size = @max(new_length, 1);
    // A small block already holds every length of its size class.
    if (old_size <= max_small_payload and new_size <= max_small_payload and smallClass(old_size) == smallClass(new_size)) {
        if (alignment > max_allocation_alignment or !std.math.isPowerOfTwo(alignment)) {
            base.invariant("{s}", .{"unsupported compile-time RocOps allocation alignment"});
        }
        header.size = new_size;
        return ptr;
    }
    const new_bytes = self.allocateRocBytes(new_length, alignment);
    const old_bytes: [*]u8 = @ptrCast(ptr);
    const copied = @min(old_size, new_size);
    @memcpy(new_bytes[0..copied], old_bytes[0..copied]);
    self.freeRocBytes(ptr, alignment, "realloc");
    return @ptrCast(new_bytes);
}

fn assertLive(ptr: *anyopaque, alignment: usize, comptime operation: []const u8) void {
    if (!std.debug.runtime_safety) return;
    const header = allocationHeader(ptr);
    if (header.alignment != alignment) {
        std.debug.panic("CompileTimeHost: " ++ operation ++ " at alignment {d} of a block allocated at alignment {d} (ptr=0x{x})", .{ alignment, header.alignment, @intFromPtr(ptr) });
    }
    std.debug.assert(header.stamp == allocation_header_stamp);
}

/// The size class of a small block whose payload is `size` bytes.
fn smallClass(size: usize) usize {
    const block_log2 = std.math.log2_int_ceil(usize, allocation_header_bytes + max_payload_skew + size);
    return @max(block_log2, min_small_block_log2) - min_small_block_log2;
}

/// Bytes between the header region and Roc bytes at `alignment`.
fn payloadSkew(alignment: usize) usize {
    return if (std.debug.runtime_safety) alignment else 0;
}

/// Hand out `length` bytes (at least one) at `alignment`, after their header;
/// exactly `alignment` in safety-checked builds.
fn allocateRocBytes(self: *CompileTimeHost, length: usize, alignment: usize) [*]u8 {
    if (alignment > max_allocation_alignment or !std.math.isPowerOfTwo(alignment)) {
        base.invariant("{s}", .{"unsupported compile-time RocOps allocation alignment"});
    }
    if (length > self.max_allocation_bytes) self.rocOutOfMemory();
    const size = @max(length, 1);
    const header: *AllocationHeader = if (size <= max_small_payload)
        self.allocateSmallBlock(smallClass(size))
    else
        self.allocateLargeBlock(size);
    header.size = size;
    header.alignment = alignment;
    if (std.debug.runtime_safety) header.stamp = allocation_header_stamp;
    const bytes: [*]u8 = @ptrCast(header);
    return bytes + allocation_header_bytes + payloadSkew(alignment);
}

fn allocateSmallBlock(self: *CompileTimeHost, class: usize) *AllocationHeader {
    if (self.free_lists[class]) |free| {
        self.free_lists[class] = free.next;
        return @ptrCast(free);
    }
    const block_bytes = @as(usize, 1) << @intCast(class + min_small_block_log2);
    const block = self.arena.allocator().alignedAlloc(u8, .fromByteUnits(allocation_header_bytes), block_bytes) catch {
        self.rocOutOfMemory();
    };
    return @ptrCast(@alignCast(block.ptr));
}

fn allocateLargeBlock(self: *CompileTimeHost, size: usize) *AllocationHeader {
    const total = std.math.add(usize, large_block_overhead, size) catch self.rocOutOfMemory();
    const block = self.arena.child_allocator.alignedAlloc(u8, .fromByteUnits(allocation_header_bytes), total) catch {
        self.rocOutOfMemory();
    };
    const links: *LargeLinks = @ptrCast(@alignCast(block.ptr));
    links.* = .{ .prev = null, .next = self.large_blocks };
    if (self.large_blocks) |next| next.prev = links;
    self.large_blocks = links;
    return @ptrCast(@alignCast(block.ptr + large_links_bytes));
}

fn freeRocBytes(self: *CompileTimeHost, ptr: *anyopaque, alignment: usize, comptime operation: []const u8) void {
    assertLive(ptr, alignment, operation);
    const header = allocationHeader(ptr);
    const size = header.size;
    if (size <= max_small_payload) {
        const class = smallClass(size);
        const free: *FreeBlock = @ptrCast(header);
        free.* = .{ .next = self.free_lists[class], .unused = 0, .stamp = 0 };
        self.free_lists[class] = free;
        return;
    }
    const header_bytes: [*]u8 = @ptrCast(header);
    const links: *LargeLinks = @ptrCast(@alignCast(header_bytes - large_links_bytes));
    if (std.debug.runtime_safety) header.stamp = 0;
    self.freeLargeBlock(links, size);
}

fn freeLargeBlock(self: *CompileTimeHost, links: *LargeLinks, size: usize) void {
    if (links.prev) |prev| prev.next = links.next else self.large_blocks = links.next;
    if (links.next) |next| next.prev = links.prev;
    const block: [*]align(allocation_header_bytes) u8 = @ptrCast(@alignCast(links));
    // Returned without overwriting its bytes, so freeing a block Roc code
    // reserved but never wrote touches none of its pages.
    self.arena.child_allocator.rawFree(block[0 .. large_block_overhead + size], .fromByteUnits(allocation_header_bytes), @returnAddress());
}

fn freeLargeBlocks(self: *CompileTimeHost) void {
    while (self.large_blocks) |links| {
        const block: [*]u8 = @ptrCast(links);
        const header: *AllocationHeader = @ptrCast(@alignCast(block + large_links_bytes));
        self.freeLargeBlock(links, header.size);
    }
}

fn allocationHeader(ptr: *anyopaque) *AllocationHeader {
    // ptr = block start + allocation_header_bytes + skew, and the skew is less
    // than allocation_header_bytes, so rounding down recovers the start.
    const start = (@intFromPtr(ptr) - allocation_header_bytes) & ~@as(usize, allocation_header_bytes - 1);
    return @ptrFromInt(start);
}

fn rocDbg(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.appendEvent(.dbg, bytes[0..len]);
}

fn rocExpectFailed(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.appendExpectFailedEvent(bytes[0..len], self.currentFailurePoint(@frameAddress()));
}

fn rocCrashed(roc_ops: *RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.crash(bytes[0..len]);
}

/// Refuse an allocation Roc code asked for: the root crashes where it asked,
/// as a host that cannot satisfy `roc_alloc` stops the program.
fn rocOutOfMemory(self: *CompileTimeHost) noreturn {
    self.crash(out_of_memory_message);
}

fn crash(self: *CompileTimeHost, message: []const u8) noreturn {
    self.failure = self.currentFailurePoint(@frameAddress());
    self.appendEvent(.crashed, message);
    self.jump(.crashed);
}

fn hostBytesAllocator(allocator: Allocator) Allocator {
    return switch (@import("builtin").target.os.tag) {
        .freestanding => std.heap.wasm_allocator,
        .wiiu,
        .@"switch",
        .gba,
        .psx,
        .tios,
        .ashetos,
        .other,
        .contiki,
        .fuchsia,
        .hermit,
        .managarm,
        .haiku,
        .hurd,
        .illumos,
        .linux,
        .plan9,
        .rtems,
        .serenity,
        .dragonfly,
        .freebsd,
        .netbsd,
        .openbsd,
        .driverkit,
        .ios,
        .maccatalyst,
        .macos,
        .tvos,
        .visionos,
        .watchos,
        .windows,
        .uefi,
        .@"3ds",
        .ps3,
        .ps4,
        .ps5,
        .psp,
        .vita,
        .emscripten,
        .wasi,
        .amdhsa,
        .amdpal,
        .cuda,
        .mesa3d,
        .nvcl,
        .opencl,
        .opengl,
        .vulkan,
        => allocator,
    };
}

test "compile-time allocations honor alignment, keep their bytes across reallocation, and churn in constant time" {
    var host = CompileTimeHost.init(std.testing.allocator, .u64);
    defer host.deinit();
    const roc_ops = host.ops();

    inline for (.{ 1, 2, 4, 8, 16 }) |alignment| {
        const ptr = roc_ops.roc_alloc(roc_ops, 3, alignment);
        try std.testing.expect(@intFromPtr(ptr) % alignment == 0);
        if (std.debug.runtime_safety) try std.testing.expect(@intFromPtr(ptr) % (2 * alignment) != 0);
        roc_ops.roc_dealloc(roc_ops, ptr, alignment);
    }

    // Growing keeps every old byte; shrinking keeps the bytes that fit. A
    // realloc keeps the block's alignment: production's host_alloc finds a
    // block's start from the alignment it is handed.
    const first = roc_ops.roc_alloc(roc_ops, 5, 8);
    const first_bytes: [*]u8 = @ptrCast(first);
    @memcpy(first_bytes[0..5], "hello");
    const grown = roc_ops.roc_realloc(roc_ops, first, 40, 8);
    const grown_bytes: [*]u8 = @ptrCast(grown);
    try std.testing.expectEqualStrings("hello", grown_bytes[0..5]);
    try std.testing.expect(@intFromPtr(grown) % 8 == 0);
    const shrunk = roc_ops.roc_realloc(roc_ops, grown, 2, 8);
    const shrunk_bytes: [*]u8 = @ptrCast(shrunk);
    try std.testing.expectEqualStrings("he", shrunk_bytes[0..2]);
    roc_ops.roc_dealloc(roc_ops, shrunk, 8);

    // A root that once held many blocks and then frees and allocates in a
    // loop must not pay for the blocks it once held on every operation.
    var live: [65_536]*anyopaque = undefined;
    for (&live) |*slot| slot.* = roc_ops.roc_alloc(roc_ops, 16, 8);
    for (live) |ptr| roc_ops.roc_dealloc(roc_ops, ptr, 8);
    var cycle: usize = 0;
    while (cycle < 1_000_000) : (cycle += 1) {
        const ptr = roc_ops.roc_alloc(roc_ops, 16, 8);
        roc_ops.roc_dealloc(roc_ops, ptr, 8);
    }
}

test "memory a compile-time root frees is reused or returned to the backing allocator" {
    var host = CompileTimeHost.init(std.testing.allocator, .u64);
    defer host.deinit();
    const roc_ops = host.ops();

    // A root that frees each list before building the next holds one list's
    // memory at a time, however many lists it builds.
    const churn = struct {
        fn run(ops_: *RocOps, cycles: usize) void {
            for (0..cycles) |_| {
                const ptr = ops_.roc_alloc(ops_, 1024, 8);
                ops_.roc_dealloc(ops_, ptr, 8);
            }
        }
    };
    churn.run(roc_ops, 1);
    const churned_once = host.arena.queryCapacity();
    churn.run(roc_ops, 100_000);
    try std.testing.expectEqual(churned_once, host.arena.queryCapacity());

    // A list growing by reallocation releases each block it outgrows, so
    // growing a second list reuses the first one's memory.
    const grow = struct {
        fn run(ops_: *RocOps) void {
            var list = ops_.roc_alloc(ops_, 8, 8);
            var length: usize = 8;
            while (length < 4 * 1024 * 1024) {
                length *= 2;
                list = ops_.roc_realloc(ops_, list, length, 8);
                const bytes: [*]u8 = @ptrCast(list);
                bytes[length - 1] = 1;
            }
            ops_.roc_dealloc(ops_, list, 8);
        }
    };
    grow.run(roc_ops);
    const grown_once = host.arena.queryCapacity();
    try std.testing.expect(host.large_blocks == null);
    for (0..100) |_| grow.run(roc_ops);
    try std.testing.expectEqual(grown_once, host.arena.queryCapacity());
    try std.testing.expect(host.large_blocks == null);

    // Large blocks still live when the root finishes are released with it.
    _ = roc_ops.roc_alloc(roc_ops, 1024 * 1024, 16);
    _ = roc_ops.roc_alloc(roc_ops, 3 * 1024 * 1024, 1);
    host.resetForRun();
    try std.testing.expect(host.large_blocks == null);
}

/// A backing allocator that refuses every request past `limit` bytes and
/// records whether one reached it, so a test can tell an allocation the host
/// asked for from one it refused itself without holding the memory.
pub const RefusingAllocator = struct {
    child: Allocator,
    limit: usize,
    refused: bool = false,

    pub fn allocator(self: *RefusingAllocator) Allocator {
        return .{ .ptr = self, .vtable = &.{ .alloc = alloc, .resize = resize, .remap = remap, .free = free } };
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *RefusingAllocator = @ptrCast(@alignCast(ctx));
        if (len > self.limit) {
            self.refused = true;
            return null;
        }
        return self.child.rawAlloc(len, alignment, ret_addr);
    }

    fn resize(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *RefusingAllocator = @ptrCast(@alignCast(ctx));
        return new_len <= self.limit and self.child.rawResize(memory, alignment, new_len, ret_addr);
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *RefusingAllocator = @ptrCast(@alignCast(ctx));
        if (new_len > self.limit) return null;
        return self.child.rawRemap(memory, alignment, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        const self: *RefusingAllocator = @ptrCast(@alignCast(ctx));
        self.child.rawFree(memory, alignment, ret_addr);
    }
};

test "a 64-bit program's evaluation asks the compiler for any allocation, and a 32-bit one's runs out of address space past a 32-bit roc_alloc" {
    if (@sizeOf(usize) < 8) return error.SkipZigTest;
    const past_32_bits = @as(usize, std.math.maxInt(u32)) + 1;
    inline for (.{ base.target.TargetUsize.u64, base.target.TargetUsize.u32 }) |address_space| {
        var backing = RefusingAllocator{ .child = std.testing.allocator, .limit = 1 << 20 };
        var host = CompileTimeHost.init(backing.allocator(), address_space);
        defer host.deinit();
        const Request = struct {
            roc_ops: *RocOps,
            length: usize,

            fn call(self: *@This()) void {
                _ = self.roc_ops.roc_alloc(self.roc_ops, self.length, 8);
            }
        };
        var request = Request{ .roc_ops = host.ops(), .length = past_32_bits };
        host.resetForRun();
        var boundary = host.enterCrashBoundary();
        defer boundary.deinit();
        _ = try boundary.run(Request, &request, Request.call);

        // Either way the root crashes where it asked, as it would on a
        // target out of memory; only a 64-bit program's evaluation asks.
        try std.testing.expectEqual(Termination.crashed, host.termination);
        try std.testing.expectEqualStrings(out_of_memory_message, host.crashMessage().?);
        try std.testing.expectEqual(address_space == .u64, backing.refused);
    }
}

test "compile-time host declarations are referenced" {
    std.testing.refAllDecls(@This());
}
