//! Developer-only native root allocation experiments. The root selects its
//! callbacks once; ordinary compiler builds never read the selectors.
const std = @import("std");
const base = @import("base");
const builtins = @import("builtins");
const root = @import("root_allocator.zig");
const RocOps = builtins.host_abi.RocOps;
const Allocator = std.mem.Allocator;
const Policy = root.Policy;

pub const Kind = enum { tests, ctfe };

pub fn rcMode(comptime kind: Kind) builtins.in_process_host.RcMode {
    return if (base.dev_diagnostics.isSet(if (kind == .tests) .test_single_thread_rc else .ctfe_single_thread_rc)) .single_thread else .requested;
}

fn selected(comptime kind: Kind) ?Policy {
    const value = base.dev_diagnostics.get(if (kind == .tests) .test_allocator else .ctfe_allocator) orelse return null;
    if (std.mem.eql(u8, value, "baseline")) return null;
    inline for (@typeInfo(Policy).@"enum".field_names) |name| {
        if (std.mem.eql(u8, value, name)) return @field(Policy, name);
    }
    std.debug.panic("unknown root allocator experiment: {s}", .{value});
}

pub const Experiment = struct {
    state: ?*root.RootAllocator = null,
    backing: Allocator = undefined,
    policy: ?Policy = null,
    record_stats: bool = false,
    kind: Kind = .tests,
    original: RocOps = undefined,
    baseline_stats: root.Stats = .{},

    pub fn enable(self: *Experiment, comptime kind: Kind, backing: Allocator, comptime Host: type, ops: *RocOps) Allocator.Error!void {
        const policy = selected(kind);
        const stats = base.dev_diagnostics.isSet(.root_allocator_stats);
        if (policy == null and !stats) return;
        const state = try backing.create(root.RootAllocator);
        state.* = root.RootAllocator.init(backing);
        self.* = .{ .state = state, .backing = backing, .policy = policy, .record_stats = stats, .kind = kind, .original = ops.* };
        if (policy) |chosen| switch (chosen) {
            inline else => |p| if (stats) install(Host, p, true, ops) else install(Host, p, false, ops),
        } else install(Host, null, true, ops);
    }

    pub fn reset(self: *Experiment) void {
        if (self.state) |state| state.reset();
        self.baseline_stats = .{};
    }

    pub fn deinit(self: *Experiment) void {
        if (self.state) |state| {
            if (self.record_stats) {
                const s = if (self.policy == null) self.baseline_stats else state.stats();
                std.debug.print("root-allocator kind={s} policy={s} allocs={d} reallocs={d} frees={d} bytes={d} live={d} peak_live={d} reserved_known={d} reserved={d} peak_reserved={d}\n", .{
                    @tagName(self.kind), if (self.policy) |p| @tagName(p) else "baseline", s.alloc_count, s.realloc_count, s.free_count, s.alloc_bytes, s.live_bytes, s.peak_live_bytes, @intFromBool(self.policy != null), s.reserved_bytes, s.peak_reserved_bytes,
                });
            }
            state.deinit();
            self.backing.destroy(state);
            self.state = null;
        }
    }
};

fn install(comptime Host: type, comptime policy: ?Policy, comptime stats: bool, ops: *RocOps) void {
    const Hooks = struct {
        fn host(roc_ops: *RocOps) *Host {
            return @ptrCast(@alignCast(roc_ops.env));
        }
        fn alloc(roc_ops: *RocOps, length: usize, alignment: usize) callconv(.c) *anyopaque {
            const owner = host(roc_ops);
            const exp = &owner.allocation_experiment;
            if (policy) |p| {
                if (@hasField(Host, "allocation_call_count")) owner.allocation_call_count += 1;
                return exp.state.?.alloc(p, stats, length, alignment) catch owner.experimentalAllocationFailed();
            } else {
                const ptr = exp.original.roc_alloc(roc_ops, length, alignment);
                if (@hasDecl(Host, "experimentalSetAllocationSize")) Host.experimentalSetAllocationSize(ptr, length);
                exp.baseline_stats.alloc_count +|= 1;
                exp.baseline_stats.alloc_bytes +|= length;
                exp.baseline_stats.live_bytes += Host.experimentalAllocationSize(ptr);
                exp.baseline_stats.peak_live_bytes = @max(exp.baseline_stats.live_bytes, exp.baseline_stats.peak_live_bytes);
                return ptr;
            }
        }
        fn free(roc_ops: *RocOps, ptr: *anyopaque, alignment: usize) callconv(.c) void {
            const owner = host(roc_ops);
            const exp = &owner.allocation_experiment;
            if (policy) |p| {
                exp.state.?.free(p, stats, ptr, alignment);
            } else {
                const size = Host.experimentalAllocationSize(ptr);
                exp.baseline_stats.free_count +|= 1;
                exp.baseline_stats.live_bytes -= size;
                exp.original.roc_dealloc(roc_ops, ptr, alignment);
            }
        }
        fn realloc(roc_ops: *RocOps, ptr: *anyopaque, length: usize, alignment: usize) callconv(.c) *anyopaque {
            const owner = host(roc_ops);
            const exp = &owner.allocation_experiment;
            if (policy) |p| {
                if (@hasField(Host, "allocation_call_count")) owner.allocation_call_count += 1;
                return exp.state.?.realloc(p, stats, ptr, length, alignment) catch owner.experimentalAllocationFailed();
            } else {
                const old_size = Host.experimentalAllocationSize(ptr);
                const result = exp.original.roc_realloc(roc_ops, ptr, length, alignment);
                if (@hasDecl(Host, "experimentalSetAllocationSize")) Host.experimentalSetAllocationSize(result, length);
                exp.baseline_stats.realloc_count +|= 1;
                exp.baseline_stats.alloc_bytes +|= length;
                exp.baseline_stats.live_bytes = exp.baseline_stats.live_bytes - old_size + Host.experimentalAllocationSize(result);
                exp.baseline_stats.peak_live_bytes = @max(exp.baseline_stats.live_bytes, exp.baseline_stats.peak_live_bytes);
                return result;
            }
        }
    };
    ops.roc_alloc = Hooks.alloc;
    ops.roc_dealloc = Hooks.free;
    ops.roc_realloc = Hooks.realloc;
}
