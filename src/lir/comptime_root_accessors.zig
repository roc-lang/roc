//! Rebuilds completed compile-time roots inside their accessors when the
//! program that evaluated them is reused as the runtime program.
//!
//! A program lowered before its roots are evaluated reads each root through
//! an accessor procedure whose body is the slot read. Once the roots have
//! completed, a root that decodes to a construction (see
//! `ComptimeScalarValues`) is rebuilt in that body instead: an empty list
//! becomes the `with_capacity` it was evaluated with, a uniform table its
//! repeat loop, a record or tag of those their constructor. The call sites
//! are untouched, so the reference counting already inserted for them stays
//! correct: a call result is an owned value either way, and the emitted
//! bodies are complete without a further pass. The slots those accessors no
//! longer read become unreachable and leave the image with the next
//! reachability pass.
const std = @import("std");
const core = @import("lir_core");
const postcheck = @import("postcheck");
const LIR = core.LIR;
const Program = core.Program;
const Allocator = std.mem.Allocator;
const scalar_values = postcheck.ComptimeScalarValues;

const EmitContext = struct {
    store: *core.LirStore,
    new_locals: *std.ArrayList(LIR.LocalId),
    next_join_point: *u32,

    pub fn addLocal(self: EmitContext, layout_idx: @import("layout").Idx) Allocator.Error!LIR.LocalId {
        const local = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try self.new_locals.append(self.store.allocator, local);
        return local;
    }

    pub fn freshJoinPointId(self: EmitContext) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point.*);
        self.next_join_point.* += 1;
        return id;
    }
};

/// Replaces the body of every accessor whose root completed as a
/// construction with that construction.
pub fn rebuild(allocator: Allocator, result: *Program.Result, frozen: *const Program.FrozenStaticData) Allocator.Error!void {
    var values = try scalar_values.CompletedScalarValues.init(allocator, result, frozen);
    defer values.deinit(allocator);
    if (values.entries.count() == 0) return;
    const store = &result.store;
    var next_join_point: u32 = 0;
    for (store.getCFStmts()) |stmt| {
        if (stmt == .join) next_join_point = @max(next_join_point, @intFromEnum(stmt.join.id) + 1);
    }
    var new_locals: std.ArrayList(LIR.LocalId) = .empty;
    defer new_locals.deinit(allocator);
    for (result.static_data_values.items) |entry| {
        const accessor = entry.accessor orelse continue;
        const root = entry.compile_time_root orelse continue;
        if (root.role != .value) continue;
        const construction = values.constructionFor(root.module, root.root, entry.layout_idx) orelse continue;
        new_locals.clearRetainingCapacity();
        const context = EmitContext{ .store = store, .new_locals = &new_locals, .next_join_point = &next_join_point };
        const value = try context.addLocal(entry.layout_idx);
        const ret = try store.addCFStmt(.{ .ret = .{ .value = value } });
        const body = try scalar_values.emit(context, store, &result.layouts, value, construction, ret) orelse continue;
        const proc = store.getProcSpecPtr(accessor);
        proc.body = body;
        proc.frame_locals = try store.addLocalSpan(new_locals.items);
        if (store.procNeedsStackProbe(&result.layouts, proc.*)) proc.stack_probe = .required;
    }
}

test "comptime root accessors declarations are referenced" {
    std.testing.refAllDecls(@This());
}
