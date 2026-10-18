//! RocOps environment for native dev-backend compile-time evaluation.
//!
//! Each compile-time root gets its own host env. Runtime allocations go into a
//! root-local arena and are bulk-freed after the result has been copied into the
//! checked ConstStore. Host effects and crash state are recorded on the env so
//! the finalizer can replay them deterministically in root request order.

const std = @import("std");
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

const Allocation = struct {
    size: usize,
    alignment: usize,
};

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
allocations: std.AutoHashMapUnmanaged(usize, Allocation) = .empty,
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
    var fp = frame;
    while (fp % @alignOf(usize) == 0 and fp >= low and fp + 2 * @sizeOf(usize) <= self.entry_frame) {
        const record: *const [2]usize = @ptrFromInt(fp);
        if (self.inGeneratedCode(record[1])) calls.append(self.host_arena.allocator(), record[1]) catch self.jump(.host_oom);
        if (record[0] <= fp) break;
        fp = record[0];
    }
    return calls.items;
}

/// Where in generated code the failure being reported from this host
/// callback happened.
fn currentFailurePoint(self: *CompileTimeHost, frame: usize) FailurePoint {
    return .{ .calls = self.generatedCalls(frame, frame) };
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
    self.allocations = .empty;
}

fn appendEvent(self: *CompileTimeHost, comptime tag: std.meta.Tag(HostEvent), bytes: []const u8) void {
    const owned = self.dupeEventBytes(bytes);
    const host_allocator = self.host_arena.allocator();
    self.events.append(host_allocator, @unionInit(HostEvent, @tagName(tag), owned)) catch {
        self.jump(.host_oom);
        unreachable;
    };
}

fn appendExpectFailedEvent(self: *CompileTimeHost, bytes: []const u8, point: FailurePoint) void {
    const owned = self.dupeEventBytes(bytes);
    const host_allocator = self.host_arena.allocator();
    self.events.append(host_allocator, .{ .expect_failed = .{
        .message = owned,
        .point = point,
    } }) catch {
        self.jump(.host_oom);
        unreachable;
    };
}

fn dupeEventBytes(self: *CompileTimeHost, bytes: []const u8) []u8 {
    const host_allocator = self.host_arena.allocator();
    return host_allocator.dupe(u8, bytes) catch {
        self.jump(.host_oom);
        unreachable;
    };
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
    const alloc_len = @max(length, 1);
    const arena_allocator = self.arena.allocator();
    const ptr = allocateBytes(arena_allocator, alloc_len, alignment) orelse {
        self.jump(.host_oom);
    };
    self.allocations.put(self.host_arena.allocator(), @intFromPtr(ptr), .{ .size = alloc_len, .alignment = alignment }) catch {
        self.jump(.host_oom);
    };
    return @ptrCast(ptr);
}

fn rocDealloc(roc_ops: *RocOps, ptr: *anyopaque, _: usize) callconv(.c) void {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    _ = self.allocations.fetchRemove(@intFromPtr(ptr)) orelse {
        base.invariant("{s}", .{"compile-time RocOps deallocated unknown pointer"});
    };
}

fn rocRealloc(roc_ops: *RocOps, ptr: *anyopaque, new_length: usize, alignment: usize) callconv(.c) *anyopaque {
    const self: *CompileTimeHost = @ptrCast(@alignCast(roc_ops.env));
    const old_info = self.allocations.get(@intFromPtr(ptr)) orelse {
        base.invariant("{s}", .{"compile-time RocOps reallocated unknown pointer"});
    };
    const alloc_len = @max(new_length, 1);
    const arena_allocator = self.arena.allocator();
    const new_ptr = allocateBytes(arena_allocator, alloc_len, alignment) orelse {
        self.jump(.host_oom);
    };
    const old_bytes: [*]u8 = @ptrCast(@alignCast(ptr));
    @memcpy(new_ptr[0..@min(old_info.size, alloc_len)], old_bytes[0..@min(old_info.size, alloc_len)]);
    self.allocations.put(self.host_arena.allocator(), @intFromPtr(new_ptr), .{ .size = alloc_len, .alignment = alignment }) catch {
        self.jump(.host_oom);
    };
    _ = self.allocations.remove(@intFromPtr(ptr));
    return @ptrCast(new_ptr);
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

fn allocateBytes(allocator: Allocator, len: usize, alignment: usize) ?[*]u8 {
    return switch (alignment) {
        1 => (allocator.alignedAlloc(u8, .@"1", len) catch return null).ptr,
        2 => (allocator.alignedAlloc(u8, .@"2", len) catch return null).ptr,
        4 => (allocator.alignedAlloc(u8, .@"4", len) catch return null).ptr,
        8 => (allocator.alignedAlloc(u8, .@"8", len) catch return null).ptr,
        16 => (allocator.alignedAlloc(u8, .@"16", len) catch return null).ptr,
        else => base.invariant("{s}", .{"unsupported compile-time RocOps allocation alignment"}),
    };
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

test "compile-time host declarations are referenced" {
    std.testing.refAllDecls(@This());
}
