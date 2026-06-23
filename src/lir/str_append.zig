//! Creates internal append-into-string variants for direct string producers.
//!
//! This runs before ARC. It consumes this explicit caller shape:
//!
//! ```text
//! result = call(args...)
//! out    = str_concat(acc, result)
//! ```
//!
//! and rewrites it to:
//!
//! ```text
//! out = call_append(acc, args...)
//! ```
//!
//! The generated variant has the ordinary explicit signature:
//!
//! ```text
//! call_append(acc: Str, args...) -> Str
//! ```
//!
//! If the source proc directly returns `Str.concat(left, right)`, the append
//! variant builds `Str.concat(Str.concat(acc, left), right)` so the source
//! proc's intermediate returned string is not materialized.

const std = @import("std");
const Allocator = std.mem.Allocator;
const core = @import("lir_core");
const layout_mod = @import("layout");

const LIR = core.LIR;
const LirStore = core.LirStore;
const CFStmtId = LIR.CFStmtId;
const LocalId = LIR.LocalId;
const LowLevelOp = LIR.LowLevel;

pub const ResourceError = Allocator.Error;

pub fn run(store: *LirStore, layouts: *layout_mod.Store) ResourceError!void {
    _ = layouts;
    var pass = StrAppendPass{
        .store = store,
        .variants = std.AutoHashMap(VariantKey, LIR.LirProcSpecId).init(store.allocator),
    };
    defer pass.variants.deinit();

    const proc_count = store.proc_specs.items.len;
    var proc_index: usize = 0;
    while (proc_index < proc_count) : (proc_index += 1) {
        try pass.transformProc(@enumFromInt(proc_index));
    }
}

const VariantKey = struct {
    source: LIR.LirProcSpecId,
};

const StrAppendPass = struct {
    store: *LirStore,
    variants: std.AutoHashMap(VariantKey, LIR.LirProcSpecId),

    fn transformProc(self: *StrAppendPass, proc_id: LIR.LirProcSpecId) ResourceError!void {
        const proc = self.store.getProcSpec(proc_id);
        if (proc.body == null or proc.hosted != null or proc.abi != .roc) return;

        var work = std.ArrayList(CFStmtId).empty;
        defer work.deinit(self.store.allocator);
        var visited = std.AutoHashMap(CFStmtId, void).init(self.store.allocator);
        defer visited.deinit();

        try work.append(self.store.allocator, proc.body.?);
        while (work.pop()) |stmt_id| {
            const entry = try visited.getOrPut(stmt_id);
            if (entry.found_existing) continue;

            _ = try self.rewriteAt(stmt_id);
            try self.appendSuccessors(&work, stmt_id);
        }
    }

    fn rewriteAt(self: *StrAppendPass, call_stmt_id: CFStmtId) ResourceError!bool {
        const call_stmt = switch (self.store.getCFStmt(call_stmt_id)) {
            .assign_call => |s| s,
            else => return false,
        };

        if (!self.isStrLayout(self.store.getLocal(call_stmt.target).layout_idx)) return false;

        const callee = self.store.getProcSpec(call_stmt.proc);
        if (callee.body == null or callee.hosted != null or callee.abi != .roc) return false;
        if (callee.ret_layout != .str) return false;

        const concat = try self.findConcatAfterCall(call_stmt.target, call_stmt.next) orelse return false;
        const concat_stmt_id = concat.stmt;
        const concat_stmt = switch (self.store.getCFStmt(concat_stmt_id)) {
            .assign_low_level => |s| s,
            else => return false,
        };
        if (!self.isStrLayout(self.store.getLocal(concat_stmt.target).layout_idx)) return false;

        if (!self.isStrLayout(self.store.getLocal(concat.accumulator).layout_idx)) return false;

        const variant = try self.appendVariant(call_stmt.proc);

        var args = std.ArrayList(LocalId).empty;
        defer args.deinit(self.store.allocator);
        try args.append(self.store.allocator, concat.accumulator);
        try args.appendSlice(self.store.allocator, self.store.getLocalSpan(call_stmt.args));

        self.store.getCFStmtPtr(call_stmt_id).* = .{ .assign_call = .{
            .target = concat_stmt.target,
            .proc = variant,
            .args = try self.store.addLocalSpan(args.items),
            .is_cold = call_stmt.is_cold,
            .next = concat_stmt.next,
        } };

        return true;
    }

    const ConcatAfterCall = struct {
        stmt: CFStmtId,
        accumulator: LocalId,
    };

    fn findConcatAfterCall(self: *StrAppendPass, source: LocalId, first_stmt: CFStmtId) ResourceError!?ConcatAfterCall {
        var aliases = std.AutoHashMap(LocalId, LocalId).init(self.store.allocator);
        defer aliases.deinit();

        var current = first_stmt;
        while (true) {
            switch (self.store.getCFStmt(current)) {
                .assign_ref => |stmt| switch (stmt.op) {
                    .local => |local| {
                        if (self.store.getLocal(stmt.target).layout_idx != self.store.getLocal(local).layout_idx) return null;
                        try aliases.put(stmt.target, resolveAlias(&aliases, local));
                        current = stmt.next;
                        continue;
                    },
                    else => return null,
                },
                .assign_low_level => |stmt| {
                    if (stmt.op != .str_concat) return null;
                    const args = self.store.getLocalSpan(stmt.args);
                    if (args.len != 2) return null;
                    if (resolveAlias(&aliases, args[1]) != source) return null;
                    return .{
                        .stmt = current,
                        .accumulator = resolveAlias(&aliases, args[0]),
                    };
                },
                else => return null,
            }
        }
    }

    fn resolveAlias(aliases: *const std.AutoHashMap(LocalId, LocalId), local: LocalId) LocalId {
        var current = local;
        while (aliases.get(current)) |next| {
            current = next;
        }
        return current;
    }

    fn appendVariant(self: *StrAppendPass, source: LIR.LirProcSpecId) ResourceError!LIR.LirProcSpecId {
        const key = VariantKey{ .source = source };
        if (self.variants.get(key)) |variant| return variant;

        const variant = try self.createAppendVariant(source);
        try self.variants.put(key, variant);
        return variant;
    }

    fn createAppendVariant(self: *StrAppendPass, source: LIR.LirProcSpecId) ResourceError!LIR.LirProcSpecId {
        const source_spec = self.store.getProcSpec(source);
        const source_body = source_spec.body orelse unreachable;
        const source_args = self.store.getLocalSpan(source_spec.args);

        const accumulator = try self.store.addLocal(.{ .layout_idx = .str });

        var variant_args = try std.ArrayList(LocalId).initCapacity(self.store.allocator, source_args.len + 1);
        defer variant_args.deinit(self.store.allocator);
        variant_args.appendAssumeCapacity(accumulator);

        for (source_args) |source_arg| {
            const arg = try self.store.addLocal(.{ .layout_idx = self.store.getLocal(source_arg).layout_idx });
            variant_args.appendAssumeCapacity(arg);
        }

        var cloner = try BodyCloner.init(self.store, accumulator);
        defer cloner.deinit();

        for (source_args, variant_args.items[1..]) |source_arg, variant_arg| {
            cloner.local_map[@intFromEnum(source_arg)] = variant_arg;
        }

        const source_frame = self.store.getLocalSpan(source_spec.frame_locals);
        try cloner.new_locals.appendSlice(self.store.allocator, variant_args.items);
        for (source_frame) |local| {
            _ = try cloner.mapLocal(local);
        }

        const body = try cloner.cloneStmt(source_body);

        var frame_locals = try std.ArrayList(LocalId).initCapacity(self.store.allocator, cloner.new_locals.items.len);
        defer frame_locals.deinit(self.store.allocator);
        frame_locals.appendSliceAssumeCapacity(cloner.new_locals.items);
        std.mem.sort(LocalId, frame_locals.items, {}, localIdLessThan);
        const unique_len = uniqueSortedLocals(frame_locals.items);

        const variant = try self.store.addProcSpec(.{
            .name = self.store.freshSyntheticSymbol(),
            .args = try self.store.addLocalSpan(variant_args.items),
            .frame_locals = try self.store.addLocalSpan(frame_locals.items[0..unique_len]),
            .body = body,
            .ret_layout = .str,
            .abi = .roc,
        });
        try self.store.copyProcDebugInfo(variant, source);

        return variant;
    }

    fn appendSuccessors(
        self: *StrAppendPass,
        work: *std.ArrayList(CFStmtId),
        stmt_id: CFStmtId,
    ) ResourceError!void {
        switch (self.store.getCFStmt(stmt_id)) {
            inline .assign_ref,
            .assign_literal,
            .init_uninitialized,
            .assign_call,
            .assign_call_erased,
            .assign_packed_erased_fn,
            .assign_low_level,
            .assign_list,
            .assign_struct,
            .assign_tag,
            .store_struct,
            .store_tag,
            .set_local,
            .debug,
            .expect,
            .comptime_branch_taken,
            .incref,
            .decref,
            .decref_if_initialized,
            .free,
            => |s| try work.append(self.store.allocator, s.next),

            .switch_stmt => |s| {
                if (s.continuation) |continuation| try work.append(self.store.allocator, continuation);
                try work.append(self.store.allocator, s.default_branch);
                for (self.store.getCFSwitchBranches(s.branches)) |branch| {
                    try work.append(self.store.allocator, branch.body);
                }
            },
            .switch_initialized_payload => |s| {
                try work.append(self.store.allocator, s.initialized_branch);
                try work.append(self.store.allocator, s.uninitialized_branch);
            },
            .str_match => |s| {
                try work.append(self.store.allocator, s.on_match);
                try work.append(self.store.allocator, s.on_miss);
            },
            .str_match_set => |s| {
                for (self.store.getStrMatchArms(s.arms)) |arm| {
                    try work.append(self.store.allocator, arm.on_match);
                }
                try work.append(self.store.allocator, s.on_miss);
            },
            .join => |s| {
                try work.append(self.store.allocator, s.body);
                try work.append(self.store.allocator, s.remainder);
            },
            .runtime_error,
            .comptime_exhaustiveness_failed,
            .expect_err,
            .loop_continue,
            .loop_break,
            .jump,
            .ret,
            .crash,
            => {},
        }
    }

    fn isStrLayout(self: *const StrAppendPass, layout_idx: layout_mod.Idx) bool {
        _ = self;
        return layout_idx == .str;
    }

    fn localIdLessThan(_: void, a: LocalId, b: LocalId) bool {
        return @intFromEnum(a) < @intFromEnum(b);
    }
};

const BodyCloner = struct {
    store: *LirStore,
    accumulator: LocalId,
    local_map: []?LocalId,
    stmt_map: std.AutoHashMap(CFStmtId, CFStmtId),
    new_locals: std.ArrayList(LocalId),

    fn init(store: *LirStore, accumulator: LocalId) ResourceError!BodyCloner {
        const local_map = try store.allocator.alloc(?LocalId, store.locals.items.len);
        @memset(local_map, null);
        return .{
            .store = store,
            .accumulator = accumulator,
            .local_map = local_map,
            .stmt_map = std.AutoHashMap(CFStmtId, CFStmtId).init(store.allocator),
            .new_locals = .empty,
        };
    }

    fn deinit(self: *BodyCloner) void {
        self.new_locals.deinit(self.store.allocator);
        self.stmt_map.deinit();
        self.store.allocator.free(self.local_map);
    }

    fn cloneStmt(self: *BodyCloner, old_id: CFStmtId) ResourceError!CFStmtId {
        if (self.stmt_map.get(old_id)) |existing| return existing;

        const cloned = switch (self.store.getCFStmt(old_id)) {
            .init_uninitialized => |s| try self.store.addCFStmt(.{ .init_uninitialized = .{
                .target = try self.mapLocal(s.target),
                .next = try self.cloneStmt(s.next),
            } }),
            .assign_ref => |s| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = try self.mapLocal(s.target),
                .op = try self.mapRefOp(s.op),
                .next = try self.cloneStmt(s.next),
            } }),
            .assign_literal => |s| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = try self.mapLocal(s.target),
                .value = s.value,
                .next = try self.cloneStmt(s.next),
            } }),
            .assign_call => |s| try self.store.addCFStmt(.{ .assign_call = .{
                .target = try self.mapLocal(s.target),
                .proc = s.proc,
                .args = try self.mapLocalSpan(s.args),
                .is_cold = s.is_cold,
                .next = try self.cloneStmt(s.next),
            } }),
            .assign_call_erased => |s| try self.store.addCFStmt(.{ .assign_call_erased = .{
                .target = try self.mapLocal(s.target),
                .closure = try self.mapLocal(s.closure),
                .args = try self.mapLocalSpan(s.args),
                .next = try self.cloneStmt(s.next),
            } }),
            .assign_packed_erased_fn => |s| try self.store.addCFStmt(.{ .assign_packed_erased_fn = .{
                .target = try self.mapLocal(s.target),
                .proc = s.proc,
                .capture = try self.mapMaybeLocal(s.capture),
                .capture_layout = s.capture_layout,
                .on_drop = s.on_drop,
                .reuse = try self.mapMaybeLocal(s.reuse),
                .reuse_unique = s.reuse_unique,
                .next = try self.cloneStmt(s.next),
            } }),
            .assign_low_level => |s| if (s.op == .str_concat and self.directReturnOf(s.next, s.target))
                try self.cloneConcatReturn(s)
            else
                try self.store.addCFStmt(.{ .assign_low_level = .{
                    .target = try self.mapLocal(s.target),
                    .op = s.op,
                    .rc_effect = s.rc_effect,
                    .unique_args = s.unique_args,
                    .args = try self.mapLocalSpan(s.args),
                    .next = try self.cloneStmt(s.next),
                } }),
            .assign_list => |s| try self.store.addCFStmt(.{ .assign_list = .{
                .target = try self.mapLocal(s.target),
                .elems = try self.mapLocalSpan(s.elems),
                .next = try self.cloneStmt(s.next),
            } }),
            .assign_struct => |s| try self.store.addCFStmt(.{ .assign_struct = .{
                .target = try self.mapLocal(s.target),
                .fields = try self.mapLocalSpan(s.fields),
                .next = try self.cloneStmt(s.next),
            } }),
            .assign_tag => |s| try self.store.addCFStmt(.{ .assign_tag = .{
                .target = try self.mapLocal(s.target),
                .variant_index = s.variant_index,
                .discriminant = s.discriminant,
                .payload = try self.mapMaybeLocal(s.payload),
                .next = try self.cloneStmt(s.next),
            } }),
            .store_struct => |s| try self.store.addCFStmt(.{ .store_struct = .{
                .dest = try self.mapLocal(s.dest),
                .struct_layout = s.struct_layout,
                .fields = try self.mapLocalSpan(s.fields),
                .next = try self.cloneStmt(s.next),
            } }),
            .store_tag => |s| try self.store.addCFStmt(.{ .store_tag = .{
                .dest = try self.mapLocal(s.dest),
                .tag_layout = s.tag_layout,
                .variant_index = s.variant_index,
                .discriminant = s.discriminant,
                .payload = try self.mapMaybeLocal(s.payload),
                .next = try self.cloneStmt(s.next),
            } }),
            .set_local => |s| try self.store.addCFStmt(.{ .set_local = .{
                .target = try self.mapLocal(s.target),
                .value = try self.mapLocal(s.value),
                .mode = s.mode,
                .next = try self.cloneStmt(s.next),
            } }),
            .debug => |s| try self.store.addCFStmt(.{ .debug = .{
                .message = try self.mapLocal(s.message),
                .next = try self.cloneStmt(s.next),
            } }),
            .expect => |s| try self.store.addCFStmt(.{ .expect = .{
                .condition = try self.mapLocal(s.condition),
                .next = try self.cloneStmt(s.next),
            } }),
            .expect_err => |s| try self.store.addCFStmt(.{ .expect_err = .{
                .message = try self.mapLocal(s.message),
                .region = s.region,
            } }),
            .runtime_error => try self.store.addCFStmt(.runtime_error),
            .comptime_exhaustiveness_failed => |s| try self.store.addCFStmt(.{ .comptime_exhaustiveness_failed = .{
                .site = s.site,
            } }),
            .comptime_branch_taken => |s| try self.store.addCFStmt(.{ .comptime_branch_taken = .{
                .site = s.site,
                .branch_index = s.branch_index,
                .next = try self.cloneStmt(s.next),
            } }),
            .incref => |s| try self.store.addCFStmt(.{ .incref = .{
                .value = try self.mapLocal(s.value),
                .rc = s.rc,
                .count = s.count,
                .atomicity = s.atomicity,
                .next = try self.cloneStmt(s.next),
            } }),
            .decref => |s| try self.store.addCFStmt(.{ .decref = .{
                .value = try self.mapLocal(s.value),
                .rc = s.rc,
                .atomicity = s.atomicity,
                .next = try self.cloneStmt(s.next),
            } }),
            .decref_if_initialized => |s| try self.store.addCFStmt(.{ .decref_if_initialized = .{
                .cond = try self.mapLocal(s.cond),
                .cond_mask = s.cond_mask,
                .value = try self.mapLocal(s.value),
                .rc = s.rc,
                .atomicity = s.atomicity,
                .next = try self.cloneStmt(s.next),
            } }),
            .free => |s| try self.store.addCFStmt(.{ .free = .{
                .value = try self.mapLocal(s.value),
                .rc = s.rc,
                .atomicity = s.atomicity,
                .next = try self.cloneStmt(s.next),
            } }),
            .switch_stmt => |s| try self.cloneSwitch(s),
            .switch_initialized_payload => |s| try self.store.addCFStmt(.{ .switch_initialized_payload = .{
                .cond = try self.mapLocal(s.cond),
                .cond_mask = s.cond_mask,
                .payload = try self.mapLocal(s.payload),
                .uninitialized_is_cold = s.uninitialized_is_cold,
                .initialized_branch = try self.cloneStmt(s.initialized_branch),
                .uninitialized_branch = try self.cloneStmt(s.uninitialized_branch),
            } }),
            .str_match => |s| try self.store.addCFStmt(.{ .str_match = .{
                .source = try self.mapLocal(s.source),
                .prefix = s.prefix,
                .steps = try self.mapStrMatchSteps(s.steps),
                .end = s.end,
                .on_match = try self.cloneStmt(s.on_match),
                .on_miss = try self.cloneStmt(s.on_miss),
            } }),
            .str_match_set => |s| try self.cloneStrMatchSet(s),
            .loop_continue => try self.store.addCFStmt(.loop_continue),
            .loop_break => try self.store.addCFStmt(.loop_break),
            .join => |s| try self.store.addCFStmt(.{ .join = .{
                .id = s.id,
                .params = try self.mapLocalSpan(s.params),
                .maybe_uninitialized_params = try self.mapLocalSpan(s.maybe_uninitialized_params),
                .maybe_uninitialized_conditions = try self.mapLocalSpan(s.maybe_uninitialized_conditions),
                .maybe_uninitialized_condition_masks = s.maybe_uninitialized_condition_masks,
                .body = try self.cloneStmt(s.body),
                .remainder = try self.cloneStmt(s.remainder),
            } }),
            .jump => |s| try self.store.addCFStmt(.{ .jump = .{ .target = s.target } }),
            .ret => |s| try self.cloneRet(s.value),
            .crash => |s| try self.store.addCFStmt(.{ .crash = .{ .msg = s.msg } }),
        };

        try self.stmt_map.put(old_id, cloned);
        return cloned;
    }

    fn cloneRet(self: *BodyCloner, value: LocalId) ResourceError!CFStmtId {
        const target = try self.addTemp(.str);
        const ret_stmt = try self.store.addCFStmt(.{ .ret = .{ .value = target } });
        return try self.concatInto(target, self.accumulator, try self.mapLocal(value), ret_stmt);
    }

    fn cloneConcatReturn(self: *BodyCloner, s: anytype) ResourceError!CFStmtId {
        const args = self.store.getLocalSpan(s.args);
        if (args.len != 2) return try self.cloneRet(s.target);

        const first_append = try self.addTemp(.str);
        const final = try self.mapLocal(s.target);
        const ret_stmt = try self.store.addCFStmt(.{ .ret = .{ .value = final } });
        const second = try self.concatInto(final, first_append, try self.mapLocal(args[1]), ret_stmt);
        return try self.concatInto(first_append, self.accumulator, try self.mapLocal(args[0]), second);
    }

    fn concatInto(self: *BodyCloner, target: LocalId, left: LocalId, right: LocalId, next: CFStmtId) ResourceError!CFStmtId {
        return try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .str_concat,
            .rc_effect = LowLevelOp.str_concat.rcEffect(),
            .args = try self.store.addLocalSpan(&.{ left, right }),
            .next = next,
        } });
    }

    fn directReturnOf(self: *const BodyCloner, next: CFStmtId, value: LocalId) bool {
        return switch (self.store.getCFStmt(next)) {
            .ret => |ret_stmt| ret_stmt.value == value,
            else => false,
        };
    }

    fn cloneSwitch(self: *BodyCloner, s: anytype) ResourceError!CFStmtId {
        const old_branches = self.store.getCFSwitchBranches(s.branches);
        const branches = try self.store.allocator.alloc(LIR.CFSwitchBranch, old_branches.len);
        defer self.store.allocator.free(branches);
        for (old_branches, branches) |old, *new| {
            new.* = .{
                .value = old.value,
                .body = try self.cloneStmt(old.body),
            };
        }
        return try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = try self.mapLocal(s.cond),
            .branches = try self.store.addCFSwitchBranches(branches),
            .default_branch = try self.cloneStmt(s.default_branch),
            .default_is_cold = s.default_is_cold,
            .continuation = if (s.continuation) |continuation| try self.cloneStmt(continuation) else null,
        } });
    }

    fn cloneStrMatchSet(self: *BodyCloner, s: anytype) ResourceError!CFStmtId {
        const old_arms = self.store.getStrMatchArms(s.arms);
        const arms = try self.store.allocator.alloc(LIR.StrMatchArm, old_arms.len);
        defer self.store.allocator.free(arms);
        for (old_arms, arms) |old, *new| {
            new.* = .{
                .prefix = old.prefix,
                .steps = try self.mapStrMatchSteps(old.steps),
                .end = old.end,
                .on_match = try self.cloneStmt(old.on_match),
            };
        }
        return try self.store.addCFStmt(.{ .str_match_set = .{
            .source = try self.mapLocal(s.source),
            .arms = try self.store.addStrMatchArms(arms),
            .on_miss = try self.cloneStmt(s.on_miss),
        } });
    }

    fn mapStrMatchSteps(self: *BodyCloner, span: LIR.StrMatchStepSpan) ResourceError!LIR.StrMatchStepSpan {
        const old_steps = self.store.getStrMatchSteps(span);
        const steps = try self.store.allocator.alloc(LIR.StrMatchStep, old_steps.len);
        defer self.store.allocator.free(steps);
        for (old_steps, steps) |old, *new| {
            new.* = old;
            new.capture = switch (old.capture) {
                .discard => .discard,
                .view => |local| .{ .view = try self.mapLocal(local) },
            };
        }
        return try self.store.addStrMatchSteps(steps);
    }

    fn mapRefOp(self: *BodyCloner, op: LIR.RefOp) ResourceError!LIR.RefOp {
        return switch (op) {
            .local => |local| .{ .local = try self.mapLocal(local) },
            .discriminant => |d| .{ .discriminant = .{ .source = try self.mapLocal(d.source) } },
            .field => |f| .{ .field = .{
                .source = try self.mapLocal(f.source),
                .field_idx = f.field_idx,
            } },
            .tag_payload => |t| .{ .tag_payload = .{
                .source = try self.mapLocal(t.source),
                .payload_idx = t.payload_idx,
                .variant_index = t.variant_index,
                .tag_discriminant = t.tag_discriminant,
            } },
            .tag_payload_struct => |t| .{ .tag_payload_struct = .{
                .source = try self.mapLocal(t.source),
                .variant_index = t.variant_index,
                .tag_discriminant = t.tag_discriminant,
            } },
            .list_reinterpret => |l| .{ .list_reinterpret = .{ .backing_ref = try self.mapLocal(l.backing_ref) } },
            .nominal => |n| .{ .nominal = .{ .backing_ref = try self.mapLocal(n.backing_ref) } },
        };
    }

    fn mapLocalSpan(self: *BodyCloner, span: LIR.LocalSpan) ResourceError!LIR.LocalSpan {
        const old_locals = self.store.getLocalSpan(span);
        const locals = try self.store.allocator.alloc(LocalId, old_locals.len);
        defer self.store.allocator.free(locals);
        for (old_locals, locals) |old, *new| {
            new.* = try self.mapLocal(old);
        }
        return try self.store.addLocalSpan(locals);
    }

    fn mapMaybeLocal(self: *BodyCloner, maybe: ?LocalId) ResourceError!?LocalId {
        return if (maybe) |local| try self.mapLocal(local) else null;
    }

    fn mapLocal(self: *BodyCloner, old: LocalId) ResourceError!LocalId {
        const index = @intFromEnum(old);
        if (index >= self.local_map.len) unreachable;
        if (self.local_map[index]) |existing| return existing;

        const fresh = try self.store.addLocal(.{ .layout_idx = self.store.getLocal(old).layout_idx });
        self.local_map[index] = fresh;
        try self.new_locals.append(self.store.allocator, fresh);
        return fresh;
    }

    fn addTemp(self: *BodyCloner, layout_idx: layout_mod.Idx) ResourceError!LocalId {
        const local = try self.store.addLocal(.{ .layout_idx = layout_idx });
        try self.new_locals.append(self.store.allocator, local);
        return local;
    }
};

fn uniqueSortedLocals(items: []LocalId) usize {
    var unique_len: usize = 0;
    for (items, 0..) |local, idx| {
        if (idx > 0 and items[unique_len - 1] == local) continue;
        items[unique_len] = local;
        unique_len += 1;
    }
    return unique_len;
}
