//! Statement-only LIR reference-count insertion.
//!
//! This pass operates purely over explicit locals and explicit control-flow.
//! Ownership must already be represented precisely in `CFStmt` result semantics
//! before codegen sees the IR.

const std = @import("std");
const layout_mod = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const LayoutIdx = layout_mod.Idx;

const CFStmt = LIR.CFStmt;
const CFStmtId = LIR.CFStmtId;
const LocalRef = LIR.LocalRef;
const LirProcSpecId = LIR.LirProcSpecId;
const AssignRefStmt = std.meta.TagPayload(CFStmt, .assign_ref);
const AssignLiteralStmt = std.meta.TagPayload(CFStmt, .assign_literal);
const AssignCallStmt = std.meta.TagPayload(CFStmt, .assign_call);
const AssignLowLevelStmt = std.meta.TagPayload(CFStmt, .assign_low_level);
const AssignListStmt = std.meta.TagPayload(CFStmt, .assign_list);
const AssignStructStmt = std.meta.TagPayload(CFStmt, .assign_struct);
const AssignTagStmt = std.meta.TagPayload(CFStmt, .assign_tag);

/// Inserts explicit reference-count statements into statement-only LIR.
pub const RcInsertPass = struct {
    allocator: Allocator,
    store: *LirStore,
    layout_store: *const layout_mod.Store,

    symbol_use_counts: std.AutoHashMap(u64, u32),
    symbol_alias_sources: std.AutoHashMap(u64, u64),

    /// Initializes an RC insertion pass over the provided LIR store.
    pub fn init(allocator: Allocator, store: *LirStore, layout_store: *const layout_mod.Store) Allocator.Error!RcInsertPass {
        return .{
            .allocator = allocator,
            .store = store,
            .layout_store = layout_store,
            .symbol_use_counts = std.AutoHashMap(u64, u32).init(allocator),
            .symbol_alias_sources = std.AutoHashMap(u64, u64).init(allocator),
        };
    }

    /// Releases all temporary analysis state owned by this pass.
    pub fn deinit(self: *RcInsertPass) void {
        self.symbol_use_counts.deinit();
        self.symbol_alias_sources.deinit();
    }

    /// Inserts RC statements for one proc body.
    pub fn insertRcOpsForProc(self: *RcInsertPass, proc_id: LirProcSpecId) Allocator.Error!void {
        self.symbol_use_counts.clearRetainingCapacity();
        self.symbol_alias_sources.clearRetainingCapacity();

        const proc = self.store.getProcSpec(proc_id);
        try self.countUsesInStmt(proc.body);
        self.store.getProcSpecPtr(proc_id).body = try self.processStmt(proc.body);
    }

    /// Inserts RC statements for every proc currently stored in the LIR store.
    pub fn insertRcOpsForAllProcs(self: *RcInsertPass) Allocator.Error!void {
        for (0..self.store.getProcSpecs().len) |i| {
            try self.insertRcOpsForProc(@enumFromInt(@as(u32, @intCast(i))));
        }
    }

    fn localKey(local: LocalRef) u64 {
        return @bitCast(local.symbol);
    }

    fn layoutNeedsRc(self: *const RcInsertPass, layout_idx: LayoutIdx) bool {
        return self.layout_store.layoutContainsRefcounted(self.layout_store.getLayout(layout_idx));
    }

    fn bumpUse(self: *RcInsertPass, local: LocalRef) Allocator.Error!void {
        if (!self.layoutNeedsRc(local.layout_idx)) return;
        const gop = try self.symbol_use_counts.getOrPut(localKey(local));
        if (!gop.found_existing) gop.value_ptr.* = 0;
        gop.value_ptr.* += 1;
    }

    fn countUsesInLocals(self: *RcInsertPass, locals: []const LocalRef) Allocator.Error!void {
        for (locals) |local| try self.bumpUse(local);
    }

    fn refOpSource(op: LIR.RefOp) LocalRef {
        return switch (op) {
            .local => |local| local,
            .discriminant => |disc| disc.source,
            .field => |field| field.source,
            .tag_payload => |payload| payload.source,
            .nominal => |nominal| nominal.backing_ref,
        };
    }

    fn countUsesInStmt(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!void {
        switch (self.store.getCFStmt(stmt_id)) {
            .assign_ref => |assign| {
                try self.bumpUse(refOpSource(assign.op));
                try self.countUsesInStmt(assign.next);
            },
            .assign_literal => |assign| try self.countUsesInStmt(assign.next),
            .assign_call => |assign| {
                try self.countUsesInLocals(self.store.getLocalRefs(assign.args));
                try self.countUsesInStmt(assign.next);
            },
            .assign_low_level => |assign| {
                try self.countUsesInLocals(self.store.getLocalRefs(assign.args));
                try self.countUsesInStmt(assign.next);
            },
            .assign_list => |assign| {
                try self.countUsesInLocals(self.store.getLocalRefs(assign.elems));
                try self.countUsesInStmt(assign.next);
            },
            .assign_struct => |assign| {
                try self.countUsesInLocals(self.store.getLocalRefs(assign.fields));
                try self.countUsesInStmt(assign.next);
            },
            .assign_tag => |assign| {
                try self.countUsesInLocals(self.store.getLocalRefs(assign.args));
                try self.countUsesInStmt(assign.next);
            },
            .runtime_error => {},
            .incref => |inc| {
                try self.bumpUse(inc.value);
                try self.countUsesInStmt(inc.next);
            },
            .decref => |dec| {
                try self.bumpUse(dec.value);
                try self.countUsesInStmt(dec.next);
            },
            .free => |free_stmt| {
                try self.bumpUse(free_stmt.value);
                try self.countUsesInStmt(free_stmt.next);
            },
            .switch_stmt => |switch_stmt| {
                try self.bumpUse(switch_stmt.cond);
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.countUsesInStmt(branch.body);
                }
                try self.countUsesInStmt(switch_stmt.default_branch);
            },
            .join => |join| {
                try self.countUsesInStmt(join.body);
                try self.countUsesInStmt(join.remainder);
            },
            .borrow_scope => |scope| {
                try self.countUsesInStmt(scope.body);
                try self.countUsesInStmt(scope.remainder);
            },
            .scope_exit => {},
            .jump => |jump| try self.countUsesInLocals(self.store.getLocalRefs(jump.args)),
            .ret => |ret| try self.bumpUse(ret.value),
            .crash => {},
        }
    }

    fn resultSemantics(stmt: CFStmt) LIR.ResultSemantics {
        return switch (stmt) {
            .assign_ref => |assign| assign.result,
            .assign_literal => |assign| assign.result,
            .assign_call => |assign| assign.result,
            .assign_low_level => |assign| assign.result,
            .assign_list => |assign| assign.result,
            .assign_struct => |assign| assign.result,
            .assign_tag => |assign| assign.result,
            else => unreachable,
        };
    }

    fn recordAliasSemantics(self: *RcInsertPass, target: LocalRef, semantics: LIR.ResultSemantics) Allocator.Error!void {
        const target_key = localKey(target);
        switch (semantics) {
            .fresh => {},
            .alias_of => |source| try self.symbol_alias_sources.put(target_key, localKey(source.owner)),
            .borrow_of => |borrowed| try self.symbol_alias_sources.put(target_key, localKey(borrowed.owner)),
        }
    }

    fn resultIsFresh(stmt: CFStmt) bool {
        return switch (resultSemantics(stmt)) {
            .fresh => true,
            else => false,
        };
    }

    fn processStmt(self: *RcInsertPass, stmt_id: CFStmtId) Allocator.Error!CFStmtId {
        const stmt = self.store.getCFStmt(stmt_id);
        return switch (stmt) {
            .assign_ref => |assign| try self.processAssignRef(assign),
            .assign_literal => |assign| try self.processAssignLiteral(assign),
            .assign_call => |assign| try self.processAssignCall(assign),
            .assign_low_level => |assign| try self.processAssignLowLevel(assign),
            .assign_list => |assign| try self.processAssignList(assign),
            .assign_struct => |assign| try self.processAssignStruct(assign),
            .assign_tag => |assign| try self.processAssignTag(assign),
            .incref, .decref, .free, .runtime_error => stmt_id,
            .switch_stmt => |switch_stmt| blk: {
                const start = self.store.cf_switch_branches.items.len;
                for (self.store.getCFSwitchBranches(switch_stmt.branches)) |branch| {
                    try self.store.cf_switch_branches.append(self.allocator, .{
                        .value = branch.value,
                        .body = try self.processStmt(branch.body),
                    });
                }
                break :blk try self.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = switch_stmt.cond,
                    .branches = .{
                        .start = @intCast(start),
                        .len = @intCast(self.store.cf_switch_branches.items.len - start),
                    },
                    .default_branch = try self.processStmt(switch_stmt.default_branch),
                } });
            },
            .join => |join| try self.store.addCFStmt(.{ .join = .{
                .id = join.id,
                .params = join.params,
                .body = try self.processStmt(join.body),
                .remainder = try self.processStmt(join.remainder),
            } }),
            .borrow_scope => |scope| try self.store.addCFStmt(.{ .borrow_scope = .{
                .id = scope.id,
                .body = try self.processStmt(scope.body),
                .remainder = try self.processStmt(scope.remainder),
            } }),
            .scope_exit, .jump, .ret, .crash => stmt_id,
        };
    }

    fn wrapAssignLike(self: *RcInsertPass, stmt: CFStmt, target: LocalRef, next_raw: CFStmtId) Allocator.Error!CFStmtId {
        const next = try self.processStmt(next_raw);
        const use_count = self.symbol_use_counts.get(localKey(target)) orelse 0;
        const semantics = resultSemantics(stmt);

        if (self.layoutNeedsRc(target.layout_idx)) try self.recordAliasSemantics(target, semantics);

        if (self.layoutNeedsRc(target.layout_idx) and use_count > 1 and resultIsFresh(stmt)) {
            const decref_stmt = try self.store.addCFStmt(.{ .decref = .{
                .value = target,
                .next = next,
            } });
            const assign_stmt = try self.rebuildAssignStmt(stmt, decref_stmt);
            return try self.store.addCFStmt(.{ .incref = .{
                .value = target,
                .count = @intCast(use_count - 1),
                .next = assign_stmt,
            } });
        }

        if (self.layoutNeedsRc(target.layout_idx) and use_count == 0 and resultIsFresh(stmt)) {
            const free_stmt = try self.store.addCFStmt(.{ .free = .{
                .value = target,
                .next = next,
            } });
            return try self.rebuildAssignStmt(stmt, free_stmt);
        }

        return try self.rebuildAssignStmt(stmt, next);
    }

    fn rebuildAssignStmt(self: *RcInsertPass, stmt: CFStmt, next: CFStmtId) Allocator.Error!CFStmtId {
        return switch (stmt) {
            .assign_ref => |assign| self.store.addCFStmt(.{ .assign_ref = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .next = next,
            } }),
            .assign_literal => |assign| self.store.addCFStmt(.{ .assign_literal = .{
                .target = assign.target,
                .result = assign.result,
                .value = assign.value,
                .next = next,
            } }),
            .assign_call => |assign| self.store.addCFStmt(.{ .assign_call = .{
                .target = assign.target,
                .result = assign.result,
                .proc = assign.proc,
                .args = assign.args,
                .next = next,
            } }),
            .assign_low_level => |assign| self.store.addCFStmt(.{ .assign_low_level = .{
                .target = assign.target,
                .result = assign.result,
                .op = assign.op,
                .args = assign.args,
                .next = next,
            } }),
            .assign_list => |assign| self.store.addCFStmt(.{ .assign_list = .{
                .target = assign.target,
                .result = assign.result,
                .elems = assign.elems,
                .next = next,
            } }),
            .assign_struct => |assign| self.store.addCFStmt(.{ .assign_struct = .{
                .target = assign.target,
                .result = assign.result,
                .fields = assign.fields,
                .next = next,
            } }),
            .assign_tag => |assign| self.store.addCFStmt(.{ .assign_tag = .{
                .target = assign.target,
                .result = assign.result,
                .discriminant = assign.discriminant,
                .args = assign.args,
                .next = next,
            } }),
            else => unreachable,
        };
    }

    fn processAssignRef(self: *RcInsertPass, assign: AssignRefStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_ref = assign }, assign.target, assign.next);
    }

    fn processAssignLiteral(self: *RcInsertPass, assign: AssignLiteralStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_literal = assign }, assign.target, assign.next);
    }

    fn processAssignCall(self: *RcInsertPass, assign: AssignCallStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_call = assign }, assign.target, assign.next);
    }

    fn processAssignLowLevel(self: *RcInsertPass, assign: AssignLowLevelStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_low_level = assign }, assign.target, assign.next);
    }

    fn processAssignList(self: *RcInsertPass, assign: AssignListStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_list = assign }, assign.target, assign.next);
    }

    fn processAssignStruct(self: *RcInsertPass, assign: AssignStructStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_struct = assign }, assign.target, assign.next);
    }

    fn processAssignTag(self: *RcInsertPass, assign: AssignTagStmt) Allocator.Error!CFStmtId {
        return self.wrapAssignLike(.{ .assign_tag = assign }, assign.target, assign.next);
    }
};
