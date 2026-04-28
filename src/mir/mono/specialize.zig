//! Specialization-driven mono MIR construction state.
//!
//! This is the only API that may turn checked procedure templates into mono MIR
//! procedures. It reserves procedure identities before body lowering so recursive
//! and mutually-recursive mono specializations cannot allocate duplicate
//! procedure values.

const std = @import("std");
const check = @import("check");

const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;
const checked_artifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;

pub const MonoProcHandle = enum(u32) { _ };

pub const MonoSpecializationReason = union(enum) {
    root: checked_artifact.RootRequest,
    call_proc: Ast.ExprId,
    proc_value: Ast.ExprId,
    static_dispatch_target: checked_artifact.RootSource,
    comptime_dependency_summary: u32,
};

pub const MonoSpecializationRequest = struct {
    template: canonical.ProcedureTemplateRef,
    requested_mono_fn_ty: canonical.CanonicalTypeKey,
    reason: MonoSpecializationReason,
};

pub const ReservedState = enum {
    reserved,
    lowering,
    lowered,
};

pub const ReservedMonoProc = struct {
    proc: canonical.ProcedureValueRef,
    local_handle: MonoProcHandle,
    state: ReservedState,
};

pub const Proc = struct {
    key: canonical.MonoSpecializationKey,
    proc: canonical.ProcedureValueRef,
    local_handle: MonoProcHandle,
    body: ?Ast.DefId = null,
};

pub const Program = struct {
    allocator: Allocator,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.* = Program.init(self.allocator);
    }

    pub fn addProc(self: *Program, key: canonical.MonoSpecializationKey, reserved: ReservedMonoProc) Allocator.Error!void {
        try self.procs.append(self.allocator, .{
            .key = key,
            .proc = reserved.proc,
            .local_handle = reserved.local_handle,
        });
    }
};

pub const Queue = struct {
    allocator: Allocator,
    requested: std.AutoHashMap(canonical.MonoSpecializationKey, ReservedMonoProc),
    pending: std.ArrayList(canonical.MonoSpecializationKey),

    pub fn init(allocator: Allocator) Queue {
        return .{
            .allocator = allocator,
            .requested = std.AutoHashMap(canonical.MonoSpecializationKey, ReservedMonoProc).init(allocator),
            .pending = .empty,
        };
    }

    pub fn deinit(self: *Queue) void {
        self.pending.deinit(self.allocator);
        self.requested.deinit();
        self.* = Queue.init(self.allocator);
    }

    pub fn reserve(self: *Queue, request: MonoSpecializationRequest) Allocator.Error!ReservedMonoProc {
        const key = canonical.MonoSpecializationKey{
            .template = request.template,
            .requested_mono_fn_ty = request.requested_mono_fn_ty,
        };
        if (self.requested.get(key)) |existing| return existing;

        const reserved = ReservedMonoProc{
            .proc = .{ .proc_base = request.template.proc_base },
            .local_handle = @enumFromInt(@as(u32, @intCast(self.requested.count()))),
            .state = .reserved,
        };
        try self.requested.put(key, reserved);
        try self.pending.append(self.allocator, key);
        return reserved;
    }

    pub fn markLowering(self: *Queue, key: canonical.MonoSpecializationKey) void {
        const entry = self.requested.getPtr(key) orelse unreachable;
        switch (entry.state) {
            .reserved => entry.state = .lowering,
            .lowering, .lowered => unreachable,
        }
    }

    pub fn markLowered(self: *Queue, key: canonical.MonoSpecializationKey) void {
        const entry = self.requested.getPtr(key) orelse unreachable;
        switch (entry.state) {
            .lowering => entry.state = .lowered,
            .reserved, .lowered => unreachable,
        }
    }
};

test "mono specialization queue reserves once" {
    var queue = Queue.init(std.testing.allocator);
    defer queue.deinit();

    const template = canonical.ProcedureTemplateRef{
        .proc_base = @enumFromInt(0),
        .template = @enumFromInt(0),
    };
    const request = MonoSpecializationRequest{
        .template = template,
        .requested_mono_fn_ty = .{},
        .reason = .{ .comptime_dependency_summary = 0 },
    };

    const first = try queue.reserve(request);
    const second = try queue.reserve(request);
    try std.testing.expectEqual(first.local_handle, second.local_handle);
    try std.testing.expectEqual(@as(usize, 1), queue.pending.items.len);
}
