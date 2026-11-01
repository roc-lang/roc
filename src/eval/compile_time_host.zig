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

/// Every compile-time allocation carries its byte length in a header this
/// many bytes before the bytes handed to Roc. The header keeps the Roc bytes
/// at the strictest alignment a request may ask for, so one block layout
/// serves every request, and reallocation and deallocation read the block's
/// length from the header.
const allocation_header_bytes = 16;
const max_allocation_alignment = allocation_header_bytes;
/// Debug-only stamp a live header carries, so a pointer Roc hands back that
/// no compile-time allocation produced, or hands back twice, is caught.
const allocation_header_stamp: u64 = 0x5f6d_6f63_7074_696d;

const AllocationHeader = extern struct {
    size: usize,
    stamp: u64,
};

/// A small block is carved from the root's arena at a power-of-two size,
/// header included, and a freed one waits on its size class's free list for
/// the next request of that class. The arena takes no lock, so the many
/// small allocations Roc makes never contend across roots.
const min_small_block_log2 = 5;
const max_small_block_log2 = 15;
const small_class_count = max_small_block_log2 - min_small_block_log2 + 1;
const max_small_payload = (1 << max_small_block_log2) - allocation_header_bytes;

/// A freed small block's header, linking it into its class's free list.
const FreeBlock = extern struct {
    next: ?*FreeBlock,
    stamp: u64,
};

/// A large block comes from the backing allocator and goes back to it when
/// Roc frees it. Its links precede its allocation header, so the live large
/// blocks form a list a reset can release without a table.
const LargeLinks = extern struct {
    prev: ?*LargeLinks,
    next: ?*LargeLinks,
};
const large_links_bytes = 16;

comptime {
    std.debug.assert(@sizeOf(AllocationHeader) <= allocation_header_bytes);
    std.debug.assert(@sizeOf(FreeBlock) <= allocation_header_bytes);
    std.debug.assert(@sizeOf(LargeLinks) == large_links_bytes);
    std.debug.assert(large_links_bytes % max_allocation_alignment == 0);
    std.debug.assert(std.math.isPowerOfTwo(allocation_header_bytes));
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
/// The frame that called into generated code; every generated frame lies
/// below it on the stack.
entry_frame: usize = 0,
/// Where the run failed, once it has.
failure: ?FailurePoint = null,
timing_io: ?std.Io = null,
jmp_buf: JmpBuf = undefined,
active_jmp_buf: ?*JmpBuf = null,
termination: Termination = .returned,

pub fn init(allocator: Allocator) CompileTimeHost {
    const backing_allocator = hostBytesAllocator(allocator);
    return .{
        .arena = base.SingleThreadArena.init(backing_allocator),
        .host_arena = base.SingleThreadArena.init(backing_allocator),
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

/// Set the generated code this host runs, for locating its failures.
pub fn setGeneratedCode(self: *CompileTimeHost, code: []const u8) void {
    self.code_start = @intFromPtr(code.ptr);
    self.code_end = self.code_start + code.len;
}

fn inGeneratedCode(self: *const CompileTimeHost, address: usize) bool {
    return address >= self.code_start and address < self.code_end;
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

fn rocDealloc(roc_ops: *RocOps, ptr: *anyopaque, _: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    self.freeRocBytes(ptr);
}

fn rocRealloc(roc_ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) *anyopaque {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    const header = allocationHeader(ptr);
    assertLive(header);
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
    self.freeRocBytes(ptr);
    return @ptrCast(new_bytes);
}

fn assertLive(header: *AllocationHeader) void {
    if (!std.debug.runtime_safety) return;
    std.debug.assert(header.stamp == allocation_header_stamp);
}

/// The size class of a small block whose payload is `size` bytes.
fn smallClass(size: usize) usize {
    const block_log2 = std.math.log2_int_ceil(usize, allocation_header_bytes + size);
    return @max(block_log2, min_small_block_log2) - min_small_block_log2;
}

/// Hand out `length` bytes (at least one) at `alignment`, behind their header.
fn allocateRocBytes(self: *CompileTimeHost, length: usize, alignment: usize) [*]u8 {
    if (alignment > max_allocation_alignment or !std.math.isPowerOfTwo(alignment)) {
        base.invariant("{s}", .{"unsupported compile-time RocOps allocation alignment"});
    }
    const size = @max(length, 1);
    const header: *AllocationHeader = if (size <= max_small_payload)
        self.allocateSmallBlock(smallClass(size))
    else
        self.allocateLargeBlock(size);
    header.size = size;
    if (std.debug.runtime_safety) header.stamp = allocation_header_stamp;
    const bytes: [*]u8 = @ptrCast(header);
    return bytes + allocation_header_bytes;
}

fn allocateSmallBlock(self: *CompileTimeHost, class: usize) *AllocationHeader {
    if (self.free_lists[class]) |free| {
        self.free_lists[class] = free.next;
        return @ptrCast(free);
    }
    const block_bytes = @as(usize, 1) << @intCast(class + min_small_block_log2);
    const block = self.arena.allocator().alignedAlloc(u8, .fromByteUnits(allocation_header_bytes), block_bytes) catch {
        self.jump(.host_oom);
    };
    return @ptrCast(@alignCast(block.ptr));
}

fn allocateLargeBlock(self: *CompileTimeHost, size: usize) *AllocationHeader {
    const total = std.math.add(usize, large_links_bytes + allocation_header_bytes, size) catch self.jump(.host_oom);
    const block = self.arena.child_allocator.alignedAlloc(u8, .fromByteUnits(allocation_header_bytes), total) catch {
        self.jump(.host_oom);
    };
    const links: *LargeLinks = @ptrCast(@alignCast(block.ptr));
    links.* = .{ .prev = null, .next = self.large_blocks };
    if (self.large_blocks) |next| next.prev = links;
    self.large_blocks = links;
    return @ptrCast(@alignCast(block.ptr + large_links_bytes));
}

fn freeRocBytes(self: *CompileTimeHost, ptr: *anyopaque) void {
    const header = allocationHeader(ptr);
    assertLive(header);
    const size = header.size;
    if (size <= max_small_payload) {
        const class = smallClass(size);
        const free: *FreeBlock = @ptrCast(header);
        free.* = .{ .next = self.free_lists[class], .stamp = 0 };
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
    self.arena.child_allocator.free(block[0 .. large_links_bytes + allocation_header_bytes + size]);
}

fn freeLargeBlocks(self: *CompileTimeHost) void {
    while (self.large_blocks) |links| {
        const block: [*]u8 = @ptrCast(links);
        const header: *AllocationHeader = @ptrCast(@alignCast(block + large_links_bytes));
        self.freeLargeBlock(links, header.size);
    }
}

fn allocationHeader(ptr: *anyopaque) *AllocationHeader {
    const bytes: [*]u8 = @ptrCast(ptr);
    return @ptrCast(@alignCast(bytes - allocation_header_bytes));
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
    self.failure = self.currentFailurePoint(@frameAddress());
    self.appendEvent(.crashed, bytes[0..len]);
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
    var host = CompileTimeHost.init(std.testing.allocator);
    defer host.deinit();
    const roc_ops = host.ops();

    inline for (.{ 1, 2, 4, 8, 16 }) |alignment| {
        const ptr = roc_ops.roc_alloc(roc_ops, 3, alignment);
        try std.testing.expect(@intFromPtr(ptr) % alignment == 0);
        roc_ops.roc_dealloc(roc_ops, ptr, alignment);
    }

    // Growing keeps every old byte; shrinking keeps the bytes that fit.
    const first = roc_ops.roc_alloc(roc_ops, 5, 1);
    const first_bytes: [*]u8 = @ptrCast(first);
    @memcpy(first_bytes[0..5], "hello");
    const grown = roc_ops.roc_realloc(roc_ops, first, 40, 8);
    const grown_bytes: [*]u8 = @ptrCast(grown);
    try std.testing.expectEqualStrings("hello", grown_bytes[0..5]);
    try std.testing.expect(@intFromPtr(grown) % 8 == 0);
    const shrunk = roc_ops.roc_realloc(roc_ops, grown, 2, 1);
    const shrunk_bytes: [*]u8 = @ptrCast(shrunk);
    try std.testing.expectEqualStrings("he", shrunk_bytes[0..2]);
    roc_ops.roc_dealloc(roc_ops, shrunk, 1);

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
    var host = CompileTimeHost.init(std.testing.allocator);
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

test "compile-time host declarations are referenced" {
    std.testing.refAllDecls(@This());
}
