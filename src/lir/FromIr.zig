//! Lower cor-style executable IR into statement-only LIR, committing final
//! memory layout exactly once at the shared `IR -> LIR/layout` boundary.

const builtin = @import("builtin");
const std = @import("std");
const base = @import("base");
const can = @import("can");
const ir = @import("ir");
const layout_mod = @import("layout");
const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const ModuleEnv = can.ModuleEnv;

/// Public struct `Result`.
pub const Result = struct {
    store: LirStore,
    layouts: layout_mod.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    proc_ids_by_symbol: std.AutoHashMap(u32, LIR.LirProcSpecId),

    pub fn deinit(self: *Result) void {
        self.proc_ids_by_symbol.deinit();
        self.root_procs.deinit(self.store.allocator);
        self.layouts.deinit();
        self.store.deinit();
    }
};

/// Run this compilation stage.
pub fn run(
    allocator: std.mem.Allocator,
    all_module_envs: []const *const ModuleEnv,
    builtin_str_ident: ?base.Ident.Idx,
    target_usize: base.target.TargetUsize,
    input: *ir.Lower.Result,
) std.mem.Allocator.Error!Result {
    var lowerer = try Lowerer.init(allocator, all_module_envs, builtin_str_ident, target_usize, input.take(allocator));
    errdefer lowerer.deinit();
    try lowerer.registerProcPlaceholders();
    try lowerer.lowerAllDefs();
    return lowerer.finish();
}

const Lowerer = struct {
    allocator: std.mem.Allocator,
    input: ir.Lower.Result,
    store: LirStore,
    layouts: layout_mod.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    proc_ids_by_symbol: std.AutoHashMap(u32, LIR.LirProcSpecId),
    proc_ret_layout_refs: std.AutoHashMap(u32, ir.Layout.Ref),
    local_layout_refs: std.AutoHashMap(u32, ir.Layout.Ref),
    ir_to_layout: std.AutoHashMap(u32, layout_mod.Idx),
    next_join_id: u32,

    const BlockExit = union(enum) {
        ret,
        jump: LIR.JoinPointId,
        jump_void: LIR.JoinPointId,
        loop_continue,
    };

    fn init(
        allocator: std.mem.Allocator,
        all_module_envs: []const *const ModuleEnv,
        builtin_str_ident: ?base.Ident.Idx,
        target_usize: base.target.TargetUsize,
        input: ir.Lower.Result,
    ) std.mem.Allocator.Error!Lowerer {
        return .{
            .allocator = allocator,
            .input = input,
            .store = LirStore.init(allocator),
            .layouts = try layout_mod.Store.init(all_module_envs, builtin_str_ident, allocator, target_usize),
            .root_procs = .empty,
            .proc_ids_by_symbol = std.AutoHashMap(u32, LIR.LirProcSpecId).init(allocator),
            .proc_ret_layout_refs = std.AutoHashMap(u32, ir.Layout.Ref).init(allocator),
            .local_layout_refs = std.AutoHashMap(u32, ir.Layout.Ref).init(allocator),
            .ir_to_layout = std.AutoHashMap(u32, layout_mod.Idx).init(allocator),
            .next_join_id = 0,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.input.deinit();
        self.ir_to_layout.deinit();
        self.local_layout_refs.deinit();
        self.proc_ret_layout_refs.deinit();
        self.proc_ids_by_symbol.deinit();
        self.root_procs.deinit(self.allocator);
        self.layouts.deinit();
        self.store.deinit();
    }

    fn finish(self: *Lowerer) Result {
        const result = Result{
            .store = self.store,
            .layouts = self.layouts,
            .root_procs = self.root_procs,
            .proc_ids_by_symbol = self.proc_ids_by_symbol,
        };
        self.ir_to_layout.deinit();
        self.local_layout_refs.deinit();
        self.proc_ret_layout_refs.deinit();
        self.input.deinit();
        return result;
    }

    fn registerProcPlaceholders(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.store.defsSlice()) |def| {
            const ir_args = self.input.store.sliceVarSpan(def.args);
            const arg_locals = try self.allocator.alloc(LIR.LocalId, ir_args.len);
            defer self.allocator.free(arg_locals);
            for (ir_args, 0..) |arg, i| {
                const local_id = try self.store.addLocal(.{
                    .layout_idx = try self.lowerLayoutId(arg.layout),
                });
                arg_locals[i] = local_id;
                try self.local_layout_refs.put(@intFromEnum(local_id), arg.layout);
            }
            const proc_id = try self.store.addProcSpec(.{
                .name = lirSymbol(def.name),
                .args = try self.store.addLocalSpan(arg_locals),
                .ret_layout = try self.lowerLayoutId(def.ret_layout),
                .owned_params = if (def.hosted != null)
                    try self.hostedOwnedParams(arg_locals)
                else
                    .empty(),
                .hosted = if (def.hosted) |hosted| .{
                    .symbol_name = hosted.symbol_name,
                    .index = hosted.index,
                } else null,
                .result_contract = .fresh,
            });
            try self.proc_ids_by_symbol.put(def.name.raw(), proc_id);
            try self.proc_ret_layout_refs.put(@intFromEnum(proc_id), def.ret_layout);
            try self.root_procs.append(self.allocator, proc_id);
        }
    }

    fn lowerAllDefs(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.store.defsSlice()) |def| {
            try self.lowerDef(def);
        }
    }

    fn lowerDef(self: *Lowerer, def: ir.Ast.Def) std.mem.Allocator.Error!void {
        const proc_id = self.proc_ids_by_symbol.get(def.name.raw()) orelse debugPanic("lir.from_ir.lowerDef missing proc placeholder");
        var proc = ProcLowerer.init(self, proc_id);
        defer proc.deinit();

        const args = self.input.store.sliceVarSpan(def.args);
        const proc_ptr = self.store.getProcSpecPtr(proc_id);
        const arg_span = proc_ptr.args;
        const arg_locals = self.store.getLocalSpan(arg_span);
        if (builtin.mode == .Debug and args.len != arg_locals.len) {
            std.debug.panic(
                "lir.from_ir invariant violated: proc {d} placeholder stored {d} args but source def has {d}",
                .{ @intFromEnum(proc_id), arg_locals.len, args.len },
            );
        }
        for (args, arg_locals) |arg, local_id| {
            const arg_layout = try self.lowerLayoutId(arg.layout);
            try proc.locals_by_var.put(.{
                .symbol = arg.symbol.raw(),
                .layout = @intFromEnum(arg_layout),
            }, local_id);
        }
        const body = if (def.body) |body_id|
            try proc.lowerBlock(body_id, .ret)
        else if (def.hosted != null)
            null
        else
            debugPanic("lir.from_ir invariant violated: non-hosted def missing body");
        const ret_layout = try self.lowerLayoutId(def.ret_layout);
        proc_ptr.* = .{
            .name = lirSymbol(def.name),
            .args = arg_span,
            .body = body,
            .ret_layout = ret_layout,
            .owned_params = if (def.hosted != null)
                try self.hostedOwnedParams(arg_locals)
            else
                .empty(),
            .hosted = if (def.hosted) |hosted| .{
                .symbol_name = hosted.symbol_name,
                .index = hosted.index,
            } else null,
            .result_contract = .fresh,
        };
    }

    fn hostedOwnedParams(self: *Lowerer, args: []const LIR.LocalId) std.mem.Allocator.Error!LIR.LocalSpan {
        var owned = std.ArrayList(LIR.LocalId).empty;
        defer owned.deinit(self.allocator);

        for (args) |arg_local| {
            const layout_idx = self.store.getLocal(arg_local).layout_idx;
            if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(layout_idx))) continue;
            try owned.append(self.allocator, arg_local);
        }

        return try self.store.addLocalSpan(owned.items);
    }

    fn lowerLayoutId(self: *Lowerer, ref: ir.Layout.Ref) std.mem.Allocator.Error!layout_mod.Idx {
        return switch (ref) {
            .canonical => |layout_idx| layout_idx,
            .local => |node_id| blk: {
                const key = @intFromEnum(node_id);
                if (self.ir_to_layout.get(key)) |existing| break :blk existing;

                var commit = try self.layouts.commitGraph(&self.input.layouts, ref);
                defer commit.deinit(self.allocator);

                try self.ir_to_layout.put(key, commit.root_idx);
                for (commit.value_layouts, 0..) |layout_idx, i| {
                    try self.ir_to_layout.put(@intCast(i), layout_idx);
                }
                break :blk commit.root_idx;
            },
        };
    }

    fn lowerStringId(self: *Lowerer, idx: base.StringLiteral.Idx) std.mem.Allocator.Error!base.StringLiteral.Idx {
        return self.store.insertString(self.input.strings.get(idx));
    }

    fn lowerLiteralWithLayout(
        self: *Lowerer,
        lit: ir.Ast.Lit,
        layout_ref: ir.Layout.Ref,
    ) std.mem.Allocator.Error!LIR.LiteralValue {
        const layout_idx = try self.lowerLayoutId(layout_ref);
        return switch (lit) {
            .int => |value| switch (layout_idx) {
                .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64 => blk: {
                    const is_unsigned = layout_idx == .u8 or layout_idx == .u16 or layout_idx == .u32 or layout_idx == .u64;
                    if (builtin.mode == .Debug and is_unsigned and value < 0) {
                        std.debug.panic(
                            "LIR lowering invariant violated: negative int literal {d} for unsigned layout {}",
                            .{ value, layout_idx },
                        );
                    }
                    const lit_value: i64 = if (is_unsigned)
                        @bitCast(@as(u64, @intCast(value)))
                    else
                        @intCast(value);
                    break :blk .{ .i64_literal = .{
                        .value = lit_value,
                        .layout_idx = layout_idx,
                    } };
                },
                .u128, .i128 => .{ .i128_literal = .{ .value = value, .layout_idx = layout_idx } },
                else => std.debug.panic(
                    "LIR lowering invariant violated: int literal with non-int layout {}",
                    .{layout_idx},
                ),
            },
            .f32 => |value| blk: {
                if (builtin.mode == .Debug and layout_idx != .f32) {
                    std.debug.panic(
                        "LIR lowering invariant violated: f32 literal with non-f32 layout {}",
                        .{layout_idx},
                    );
                }
                break :blk .{ .f32_literal = value };
            },
            .f64 => |value| blk: {
                if (builtin.mode == .Debug and layout_idx != .f64) {
                    std.debug.panic(
                        "LIR lowering invariant violated: f64 literal with non-f64 layout {}",
                        .{layout_idx},
                    );
                }
                break :blk .{ .f64_literal = value };
            },
            .dec => |value| blk: {
                if (builtin.mode == .Debug and layout_idx != .dec) {
                    std.debug.panic(
                        "LIR lowering invariant violated: dec literal with non-dec layout {}",
                        .{layout_idx},
                    );
                }
                break :blk .{ .dec_literal = value };
            },
            .str => |value| .{ .str_literal = try self.lowerStringId(value) },
            .bool => |value| .{ .bool_literal = value },
        };
    }

    fn nextJoin(self: *Lowerer) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_id);
        self.next_join_id += 1;
        return id;
    }
};

const ProcLowerer = struct {
    parent: *Lowerer,
    proc_id: LIR.LirProcSpecId,
    locals_by_var: std.AutoHashMap(VarKey, LIR.LocalId),
    join_locals_by_id: std.AutoHashMap(u32, LIR.LocalId),
    loop_break_targets: std.ArrayList(LIR.CFStmtId),

    const VarKey = struct {
        symbol: u64,
        layout: u32,
    };

    fn init(parent: *Lowerer, proc_id: LIR.LirProcSpecId) ProcLowerer {
        return .{
            .parent = parent,
            .proc_id = proc_id,
            .locals_by_var = std.AutoHashMap(VarKey, LIR.LocalId).init(parent.allocator),
            .join_locals_by_id = std.AutoHashMap(u32, LIR.LocalId).init(parent.allocator),
            .loop_break_targets = .empty,
        };
    }

    fn deinit(self: *ProcLowerer) void {
        self.locals_by_var.deinit();
        self.join_locals_by_id.deinit();
        self.loop_break_targets.deinit(self.parent.allocator);
    }

    fn lowerVar(self: *ProcLowerer, value: ir.Ast.Var) std.mem.Allocator.Error!LIR.LocalId {
        const layout_idx = try self.parent.lowerLayoutId(value.layout);
        const key: VarKey = .{
            .symbol = value.symbol.raw(),
            .layout = @intFromEnum(layout_idx),
        };
        if (self.locals_by_var.get(key)) |existing| return existing;
        const local_id = try self.parent.store.addLocal(.{
            .layout_idx = layout_idx,
        });
        try self.locals_by_var.put(key, local_id);
        try self.parent.local_layout_refs.put(@intFromEnum(local_id), value.layout);
        return local_id;
    }

    fn lowerVarSpan(self: *ProcLowerer, vars: []const ir.Ast.Var) std.mem.Allocator.Error![]LIR.LocalId {
        const out = try self.parent.allocator.alloc(LIR.LocalId, vars.len);
        for (vars, 0..) |var_, i| {
            out[i] = try self.lowerVar(var_);
        }
        return out;
    }

    fn localLayout(self: *const ProcLowerer, local_id: LIR.LocalId) layout_mod.Idx {
        return self.parent.store.getLocal(local_id).layout_idx;
    }

    fn localLayoutRef(self: *const ProcLowerer, local_id: LIR.LocalId) ir.Layout.Ref {
        return self.parent.local_layout_refs.get(@intFromEnum(local_id)) orelse
            debugPanic("lir.from_ir missing explicit logical layout ref for local");
    }

    fn procRetLayout(self: *const ProcLowerer) layout_mod.Idx {
        return self.parent.store.getProcSpec(self.proc_id).ret_layout;
    }

    fn procRetLayoutRef(self: *const ProcLowerer) ir.Layout.Ref {
        return self.parent.proc_ret_layout_refs.get(@intFromEnum(self.proc_id)) orelse
            debugPanic("lir.from_ir missing explicit proc return layout ref");
    }

    fn freshAggregateOwnership(self: *ProcLowerer, inputs: []const LIR.LocalId) std.mem.Allocator.Error!LIR.OwnershipSemantics {
        var consumed_count: usize = 0;
        var retained_count: usize = 0;

        for (inputs) |input| {
            switch (self.localOwnershipRole(input)) {
                .consume_owned => consumed_count += 1,
                .retain_borrow => retained_count += 1,
            }
        }

        const consumed = try self.parent.allocator.alloc(LIR.LocalId, consumed_count);
        defer self.parent.allocator.free(consumed);
        const retained = try self.parent.allocator.alloc(LIR.LocalId, retained_count);
        defer self.parent.allocator.free(retained);

        var consumed_index: usize = 0;
        var retained_index: usize = 0;
        for (inputs) |input| {
            switch (self.localOwnershipRole(input)) {
                .consume_owned => {
                    consumed[consumed_index] = self.consumeOwnerForLocal(input);
                    consumed_index += 1;
                },
                .retain_borrow => {
                    retained[retained_index] = input;
                    retained_index += 1;
                },
            }
        }

        return .{
            .materialization = .fresh_aggregate,
            .consumed_owned_inputs = try self.parent.store.addLocalSpan(consumed),
            .retained_borrows = try self.parent.store.addLocalSpan(retained),
        };
    }

    const AggregateInputOwnershipRole = enum {
        consume_owned,
        retain_borrow,
    };

    fn localOwnershipRole(self: *const ProcLowerer, local: LIR.LocalId) AggregateInputOwnershipRole {
        const semantics = self.localResultSemantics(local) orelse return .retain_borrow;
        return switch (semantics) {
            .fresh => .consume_owned,
            .borrow_of => .retain_borrow,
            .alias_of => |alias| if (alias.projections.isEmpty()) .consume_owned else .retain_borrow,
        };
    }

    fn consumeOwnerForLocal(self: *const ProcLowerer, local: LIR.LocalId) LIR.LocalId {
        const semantics = self.localResultSemantics(local) orelse return local;
        return switch (semantics) {
            .fresh, .borrow_of => local,
            .alias_of => |alias| if (alias.projections.isEmpty()) alias.owner else local,
        };
    }

    fn localResultSemantics(self: *const ProcLowerer, local: LIR.LocalId) ?LIR.ResultSemantics {
        for (self.parent.store.cf_stmts.items) |stmt| {
            switch (stmt) {
                .assign_ref => |assign| if (assign.target == local) return assign.result,
                .assign_literal => |assign| if (assign.target == local) return assign.result,
                .assign_call => |assign| if (assign.target == local) return assign.result,
                .assign_call_erased => |assign| if (assign.target == local) return assign.result,
                .assign_low_level => |assign| if (assign.target == local) return assign.result,
                .assign_list => |assign| if (assign.target == local) return assign.result,
                .assign_struct => |assign| if (assign.target == local) return assign.result,
                .assign_tag => |assign| if (assign.target == local) return assign.result,
                .for_list => |for_stmt| if (for_stmt.elem == local) return for_stmt.elem_result,
                else => {},
            }
        }

        return null;
    }

    fn lowLevelOwnership(self: *ProcLowerer, op: base.LowLevel, args: []const LIR.LocalId) std.mem.Allocator.Error!LIR.OwnershipSemantics {
        const arg_ownership = op.getArgOwnership();
        var retained_count: usize = 0;
        var consumed_count: usize = 0;
        for (args, 0..) |_, i| {
            if (op.borrowedArgRetainedByResult(i)) retained_count += 1;
            if (i < arg_ownership.len and arg_ownership[i] == .consume) consumed_count += 1;
        }

        const retained = try self.parent.allocator.alloc(LIR.LocalId, retained_count);
        defer self.parent.allocator.free(retained);
        const consumed = try self.parent.allocator.alloc(LIR.LocalId, consumed_count);
        defer self.parent.allocator.free(consumed);

        var write_index: usize = 0;
        var consume_index: usize = 0;
        for (args, 0..) |arg, i| {
            if (!op.borrowedArgRetainedByResult(i)) continue;
            retained[write_index] = arg;
            write_index += 1;
        }
        for (args, 0..) |arg, i| {
            if (i >= arg_ownership.len) continue;
            if (arg_ownership[i] != .consume) continue;
            consumed[consume_index] = arg;
            consume_index += 1;
        }

        return .{
            .materialization = switch (op.resultMaterialization()) {
                .direct => .direct,
                .copy_from_borrowed_input => .copy_from_borrowed_input,
                .fresh_aggregate => .fresh_aggregate,
            },
            .consumed_owned_inputs = try self.parent.store.addLocalSpan(consumed),
            .retained_borrows = try self.parent.store.addLocalSpan(retained),
        };
    }

    fn lowLevelResultSemantics(op: base.LowLevel, args: []const LIR.LocalId) LIR.ResultSemantics {
        return switch (op.procResultSemantics()) {
            .fresh, .no_return => .fresh,
            .alias_arg => |arg_index| blk: {
                if (builtin.mode == .Debug and arg_index >= args.len) {
                    debugPanic("lir.from_ir low-level alias_arg summary out of bounds");
                }
                break :blk .{ .alias_of = .{ .owner = args[arg_index] } };
            },
            .borrow_arg => |arg_index| blk: {
                if (builtin.mode == .Debug and arg_index >= args.len) {
                    debugPanic("lir.from_ir low-level borrow_arg summary out of bounds");
                }
                break :blk .{ .borrow_of = .{ .owner = args[arg_index], .region = .proc } };
            },
        };
    }

    fn refOwnership(self: *ProcLowerer, result: LIR.ResultSemantics, op: LIR.RefOp) std.mem.Allocator.Error!LIR.OwnershipSemantics {
        if (result != .fresh) return .{};

        return switch (op) {
            .local => |source| .{
                .consumed_owned_inputs = try self.parent.store.addLocalSpan(&.{source}),
            },
            .field,
            .tag_payload,
            .tag_payload_struct,
            .list_reinterpret,
            .nominal,
            => .{
                .materialization = .copy_from_borrowed_input,
            },
            .discriminant => .{},
        };
    }

    fn addAssignRef(
        self: *ProcLowerer,
        target: LIR.LocalId,
        result: LIR.ResultSemantics,
        op: LIR.RefOp,
        next: LIR.CFStmtId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        return self.parent.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .result = result,
            .ownership = try self.refOwnership(result, op),
            .op = op,
            .next = next,
        } });
    }

    fn freshLocalWithRef(self: *ProcLowerer, ref: ir.Layout.Ref) std.mem.Allocator.Error!LIR.LocalId {
        return self.freshLocalWithLayoutAndRef(try self.parent.lowerLayoutId(ref), ref);
    }

    fn freshLocalWithLayoutAndRef(
        self: *ProcLowerer,
        layout_idx: layout_mod.Idx,
        ref: ir.Layout.Ref,
    ) std.mem.Allocator.Error!LIR.LocalId {
        const local_id = try self.parent.store.addLocal(.{ .layout_idx = layout_idx });
        try self.parent.local_layout_refs.put(@intFromEnum(local_id), ref);
        return local_id;
    }

    fn lowerPhysicalStructFieldLayout(
        self: *ProcLowerer,
        struct_layout_idx: layout_mod.Idx,
        field_index: u16,
    ) layout_mod.Idx {
        const ls = &self.parent.layouts;
        const struct_layout = ls.getLayout(struct_layout_idx);
        if (struct_layout.tag == .zst) {
            return struct_layout_idx;
        }
        if (builtin.mode == .Debug and struct_layout.tag != .struct_) {
            std.debug.panic(
                "lir.from_ir invariant violated: expected struct layout for field lookup, got {s}",
                .{@tagName(struct_layout.tag)},
            );
        }
        return ls.getStructFieldLayoutByOriginalIndex(struct_layout.data.struct_.idx, field_index);
    }

    fn lowerUnionPayloadLayout(
        self: *ProcLowerer,
        union_layout_idx: layout_mod.Idx,
        tag_discriminant: u16,
    ) layout_mod.Idx {
        const ls = &self.parent.layouts;
        const union_layout = ls.getLayout(union_layout_idx);
        return switch (union_layout.tag) {
            .tag_union => blk: {
                const variants = ls.getTagUnionVariants(ls.getTagUnionData(union_layout.data.tag_union.idx));
                if (builtin.mode == .Debug and tag_discriminant >= variants.len) {
                    std.debug.panic(
                        "lir.from_ir invariant violated: tag discriminant {d} out of bounds for union layout {d}",
                        .{ tag_discriminant, @intFromEnum(union_layout_idx) },
                    );
                }
                break :blk variants.get(tag_discriminant).payload_layout;
            },
            .box => self.lowerUnionPayloadLayout(union_layout.data.box, tag_discriminant),
            .zst => .zst,
            .scalar, .box_of_zst, .list, .list_of_zst, .struct_, .closure => {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "lir.from_ir invariant violated: expected tag union layout for payload lookup, got {s}",
                        .{@tagName(union_layout.tag)},
                    );
                }
                unreachable;
            },
        };
    }

    fn unionDiscriminantLayout(self: *ProcLowerer, union_layout_idx: layout_mod.Idx) layout_mod.Idx {
        const ls = &self.parent.layouts;
        const union_layout = ls.getLayout(union_layout_idx);
        return switch (union_layout.tag) {
            .tag_union => blk: {
                const variant_count = ls.getTagUnionVariants(ls.getTagUnionData(union_layout.data.tag_union.idx)).len;
                break :blk switch (variant_count) {
                    0...0xff => .u8,
                    0x100...0xffff => .u16,
                    0x1_0000...0xffff_ffff => .u32,
                    else => .u64,
                };
            },
            .box => self.unionDiscriminantLayout(union_layout.data.box),
            .zst => .u8,
            .scalar, .box_of_zst, .list, .list_of_zst, .struct_, .closure => {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "lir.from_ir invariant violated: expected tag union layout for discriminant lookup, got {s}",
                        .{@tagName(union_layout.tag)},
                    );
                }
                unreachable;
            },
        };
    }

    fn lowerListElemLayout(self: *ProcLowerer, list_layout_idx: layout_mod.Idx) layout_mod.Idx {
        const ls = &self.parent.layouts;
        const list_layout = ls.getLayout(list_layout_idx);
        return switch (list_layout.tag) {
            .list => list_layout.data.list,
            .list_of_zst => .zst,
            else => {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "lir.from_ir invariant violated: expected list layout for element lookup, got {s}",
                        .{@tagName(list_layout.tag)},
                    );
                }
                unreachable;
            },
        };
    }

    fn layoutRefsEqual(left: ir.Layout.Ref, right: ir.Layout.Ref) bool {
        return switch (left) {
            .canonical => |left_idx| switch (right) {
                .canonical => |right_idx| left_idx == right_idx,
                .local => false,
            },
            .local => |left_node| switch (right) {
                .canonical => false,
                .local => |right_node| left_node == right_node,
            },
        };
    }

    fn localMatchesShape(
        self: *const ProcLowerer,
        local_id: LIR.LocalId,
        layout_idx: layout_mod.Idx,
        ref: ir.Layout.Ref,
    ) bool {
        if (self.localLayout(local_id) == .zst and layout_idx == .zst) {
            return true;
        }
        return self.localLayout(local_id) == layout_idx and
            layoutRefsEqual(self.localLayoutRef(local_id), ref);
    }

    fn requireLocalMatchesShape(
        self: *const ProcLowerer,
        source_local: LIR.LocalId,
        target_layout: layout_mod.Idx,
        target_ref: ir.Layout.Ref,
        comptime context: []const u8,
    ) void {
        if (self.localMatchesShape(source_local, target_layout, target_ref)) return;

        if (builtin.mode == .Debug) {
            std.debug.panic(
                "lir.from_ir invariant violated: {s} missing explicit bridge source_local={d} source_layout={d} source_ref={any} target_layout={d} target_ref={any}",
                .{
                    context,
                    @intFromEnum(source_local),
                    @intFromEnum(self.localLayout(source_local)),
                    self.localLayoutRef(source_local),
                    @intFromEnum(target_layout),
                    target_ref,
                },
            );
        }
        unreachable;
    }

    fn unwrapNominalRef(self: *ProcLowerer, ref: ir.Layout.Ref) ir.Layout.Ref {
        var current = ref;
        while (true) {
            switch (current) {
                .canonical => return current,
                .local => |node_id| switch (self.parent.input.layouts.getNode(node_id)) {
                    .nominal => |nominal| current = nominal,
                    else => return current,
                },
            }
        }
    }

    fn explicitListElemLayoutRef(self: *ProcLowerer, ref: ir.Layout.Ref) ir.Layout.Ref {
        var current = ref;
        while (true) {
            switch (current) {
                .canonical => |layout_idx| {
                    const layout_val = self.parent.layouts.getLayout(layout_idx);
                    switch (layout_val.tag) {
                        .list => return .{ .canonical = layout_val.data.list },
                        .list_of_zst => return .{ .canonical = .zst },
                        else => debugPanic("lir.from_ir expected explicit list element layout ref"),
                    }
                },
                .local => |node_id| switch (self.parent.input.layouts.getNode(node_id)) {
                    .nominal => |nominal| current = nominal,
                    .list => |elem| return elem,
                    else => debugPanic("lir.from_ir expected explicit list element layout ref"),
                },
            }
        }
    }

    fn physicalBoxChildLayoutRef(self: *ProcLowerer, ref: ir.Layout.Ref) ir.Layout.Ref {
        var current = ref;
        while (true) {
            switch (current) {
                .canonical => |layout_idx| {
                    const layout_val = self.parent.layouts.getLayout(layout_idx);
                    return switch (layout_val.tag) {
                        .box => .{ .canonical = layout_val.data.box },
                        .box_of_zst => .{ .canonical = .zst },
                        else => ref,
                    };
                },
                .local => |node_id| switch (self.parent.input.layouts.getNode(node_id)) {
                    .nominal => |nominal| current = nominal,
                    .box => |child| return child,
                    else => return ref,
                },
            }
        }
    }

    fn isListLayout(layout_val: layout_mod.Layout) bool {
        return layout_val.tag == .list or layout_val.tag == .list_of_zst;
    }

    fn structFieldLayoutRef(
        self: *ProcLowerer,
        ref: ir.Layout.Ref,
        field_index: u16,
    ) ir.Layout.Ref {
        var current = ref;
        while (true) {
            switch (current) {
                .canonical => |layout_idx| {
                    const layout_val = self.parent.layouts.getLayout(layout_idx);
                    switch (layout_val.tag) {
                        .zst => return .{ .canonical = layout_idx },
                        .struct_ => {
                            const struct_idx = layout_val.data.struct_.idx;
                            const field_layout = self.parent.layouts.getStructFieldLayoutByOriginalIndex(
                                struct_idx,
                                field_index,
                            );
                            return .{ .canonical = field_layout };
                        },
                        .box => {
                            current = .{ .canonical = layout_val.data.box };
                            continue;
                        },
                        else => debugPanic("lir.from_ir expected struct logical layout ref"),
                    }
                },
                .local => |node_id| switch (self.parent.input.layouts.getNode(node_id)) {
                    .nominal => |nominal| current = nominal,
                    .box => |child| current = child,
                    .struct_ => |fields| {
                        return self.unwrapNominalRef(self.parent.input.layouts.getFields(fields)[field_index].child);
                    },
                    else => debugPanic("lir.from_ir expected struct logical layout ref"),
                },
            }
        }
    }

    fn unionPayloadLayoutRef(
        self: *ProcLowerer,
        ref: ir.Layout.Ref,
        tag_discriminant: u16,
    ) ir.Layout.Ref {
        var current = ref;
        while (true) {
            switch (current) {
                .canonical => |layout_idx| {
                    const layout_val = self.parent.layouts.getLayout(layout_idx);
                    switch (layout_val.tag) {
                        .tag_union => {
                            const tu_data = self.parent.layouts.getTagUnionData(layout_val.data.tag_union.idx);
                            const variants = self.parent.layouts.getTagUnionVariants(tu_data);
                            const payload_layout = variants.get(tag_discriminant).payload_layout;
                            return .{ .canonical = payload_layout };
                        },
                        .box => {
                            current = .{ .canonical = layout_val.data.box };
                            continue;
                        },
                        .zst => return .{ .canonical = .zst },
                        else => debugPanic("lir.from_ir expected tag-union logical layout ref"),
                    }
                },
                .local => |node_id| switch (self.parent.input.layouts.getNode(node_id)) {
                    .nominal => |nominal| current = nominal,
                    .box => |child| current = child,
                    .tag_union => |variants| return self.parent.input.layouts.getRefs(variants)[tag_discriminant],
                    else => debugPanic("lir.from_ir expected tag-union logical layout ref"),
                },
            }
        }
    }

    fn unionDiscriminantLayoutRef(
        self: *ProcLowerer,
        ref: ir.Layout.Ref,
    ) ir.Layout.Ref {
        var current = ref;
        while (true) {
            switch (current) {
                .canonical => |layout_idx| {
                    const layout_val = self.parent.layouts.getLayout(layout_idx);
                    switch (layout_val.tag) {
                        .tag_union => {
                            const tu_data = self.parent.layouts.getTagUnionData(layout_val.data.tag_union.idx);
                            const prim: layout_mod.Idx = switch (tu_data.discriminant_size) {
                                0, 1 => .u8,
                                2 => .u16,
                                4 => .u32,
                                8 => .u64,
                                else => unreachable,
                            };
                            return .{ .canonical = prim };
                        },
                        .box => {
                            current = .{ .canonical = layout_val.data.box };
                            continue;
                        },
                        .zst => return .{ .canonical = .u8 },
                        else => debugPanic("lir.from_ir expected tag-union logical layout ref"),
                    }
                },
                .local => |node_id| switch (self.parent.input.layouts.getNode(node_id)) {
                    .nominal => |nominal| current = nominal,
                    .box => |child| current = child,
                    .tag_union => |variants| {
                        const prim: layout_mod.Idx = if (@bitSizeOf(usize) <= 32) switch (self.parent.input.layouts.getRefs(variants).len) {
                            0...0xff => .u8,
                            0x100...0xffff => .u16,
                            0x1_0000...std.math.maxInt(u32) => .u32,
                        } else switch (self.parent.input.layouts.getRefs(variants).len) {
                            0...0xff => .u8,
                            0x100...0xffff => .u16,
                            0x1_0000...0xffff_ffff => .u32,
                            else => .u64,
                        };
                        return .{ .canonical = prim };
                    },
                    else => debugPanic("lir.from_ir expected tag-union logical layout ref"),
                },
            }
        }
    }

    fn lowerSingletonTagUnionIntoTagUnion(
        self: *ProcLowerer,
        source_local: LIR.LocalId,
        target_local: LIR.LocalId,
        next: LIR.CFStmtId,
        source_payload_ref: ir.Layout.Ref,
        target_discriminant: u16,
        payload_plan: ?ir.Ast.BridgePlanId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const target_layout = self.localLayout(target_local);
        const target_ref = self.localLayoutRef(target_local);
        const target_payload_layout = self.lowerUnionPayloadLayout(target_layout, target_discriminant);
        const target_payload_is_zst = self.parent.layouts.isZeroSized(self.parent.layouts.getLayout(target_payload_layout));

        if (target_payload_is_zst) {
            return try self.parent.store.addCFStmt(.{ .assign_tag = .{
                .target = target_local,
                .result = .fresh,
                .ownership = try self.freshAggregateOwnership(&.{}),
                .discriminant = target_discriminant,
                .payload = null,
                .next = next,
            } });
        }

        const source_payload_layout = try self.parent.lowerLayoutId(source_payload_ref);
        const source_payload_local = try self.freshLocalWithLayoutAndRef(source_payload_layout, source_payload_ref);
        const target_payload_ref = self.unionPayloadLayoutRef(target_ref, target_discriminant);
        const target_payload_local = try self.freshLocalWithLayoutAndRef(
            target_payload_layout,
            target_payload_ref,
        );

        const assign_tag = try self.parent.store.addCFStmt(.{ .assign_tag = .{
            .target = target_local,
            .result = .fresh,
            .ownership = try self.freshAggregateOwnership(&.{target_payload_local}),
            .discriminant = target_discriminant,
            .payload = target_payload_local,
            .next = next,
        } });
        const bridged_payload = try self.lowerPlannedBridgeIntoLocal(
            source_payload_local,
            target_payload_local,
            assign_tag,
            payload_plan orelse debugPanic("lir.from_ir singleton tag bridge missing payload plan"),
        );
        return try self.addAssignRef(source_payload_local, .fresh, .{ .tag_payload_struct = .{
            .source = source_local,
            .tag_discriminant = 0,
        } }, bridged_payload);
    }

    fn lowerTagUnionIntoSingletonTagUnion(
        self: *ProcLowerer,
        source_local: LIR.LocalId,
        target_local: LIR.LocalId,
        next: LIR.CFStmtId,
        target_payload_ref: ir.Layout.Ref,
        source_discriminant: u16,
        payload_plan: ?ir.Ast.BridgePlanId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const actual_layout = self.localLayout(source_local);
        const actual_ref = self.localLayoutRef(source_local);
        const target_layout = self.localLayout(target_local);
        const actual_payload_layout = self.lowerUnionPayloadLayout(actual_layout, source_discriminant);
        const actual_payload_ref = self.unionPayloadLayoutRef(actual_ref, source_discriminant);
        const target_payload_layout = try self.parent.lowerLayoutId(target_payload_ref);
        const target_payload_is_zst = self.parent.layouts.isZeroSized(self.parent.layouts.getLayout(target_payload_layout));

        const accepted_branch = if (target_payload_is_zst)
            next
        else blk: {
            const actual_payload_local = try self.freshLocalWithLayoutAndRef(actual_payload_layout, actual_payload_ref);
            const target_payload_local = try self.freshLocalWithLayoutAndRef(
                target_payload_layout,
                target_payload_ref,
            );

            const assign_tag = if (self.parent.layouts.getLayout(target_layout).tag == .zst)
                next
            else
                try self.parent.store.addCFStmt(.{ .assign_tag = .{
                    .target = target_local,
                    .result = .fresh,
                    .ownership = try self.freshAggregateOwnership(&.{target_payload_local}),
                    .discriminant = 0,
                    .payload = target_payload_local,
                    .next = next,
                } });
            const bridged_payload = try self.lowerPlannedBridgeIntoLocal(
                actual_payload_local,
                target_payload_local,
                assign_tag,
                payload_plan orelse debugPanic("lir.from_ir singleton tag bridge missing payload plan"),
            );
            break :blk try self.addAssignRef(actual_payload_local, .fresh, .{ .tag_payload_struct = .{
                .source = source_local,
                .tag_discriminant = source_discriminant,
            } }, bridged_payload);
        };

        const cond_local = try self.freshLocalWithRef(self.unionDiscriminantLayoutRef(actual_ref));
        const default_branch = try self.parent.store.addCFStmt(.runtime_error);
        const branches = try self.parent.store.addCFSwitchBranches(&.{
            .{ .value = source_discriminant, .body = accepted_branch },
        });
        const switch_stmt = try self.parent.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond_local,
            .branches = branches,
            .default_branch = default_branch,
        } });
        return try self.addAssignRef(cond_local, .fresh, .{ .discriminant = .{ .source = source_local } }, switch_stmt);
    }

    fn lowerExplicitBridgeIntoLocal(
        self: *ProcLowerer,
        source_local: LIR.LocalId,
        target_local: LIR.LocalId,
        next: LIR.CFStmtId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        _ = self;
        _ = source_local;
        _ = target_local;
        _ = next;
        debugPanic("lir.from_ir missing explicit bridge plan");
    }

    fn lowerPlannedBridgeIntoLocal(
        self: *ProcLowerer,
        source_local: LIR.LocalId,
        target_local: LIR.LocalId,
        next: LIR.CFStmtId,
        plan_id: ir.Ast.BridgePlanId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const actual_layout = self.localLayout(source_local);
        const target_layout = self.localLayout(target_local);
        const ls = &self.parent.layouts;
        const actual = ls.getLayout(actual_layout);
        const target = ls.getLayout(target_layout);

        return switch (self.parent.input.store.getBridgePlan(plan_id)) {
            .zst => next,
            .direct => blk: {
                if (target.tag == .zst) break :blk next;
                if (actual_layout != target_layout and !(ls.isZeroSized(actual) and ls.isZeroSized(target))) {
                    debugPanic("lir.from_ir direct bridge plan shape mismatch");
                }
                break :blk try self.addAssignRef(target_local, .fresh, .{ .local = source_local }, next);
            },
            .list_reinterpret => blk: {
                if (!isListLayout(actual) or !isListLayout(target)) {
                    debugPanic("lir.from_ir list bridge plan shape mismatch");
                }
                break :blk try self.addAssignRef(target_local, .fresh, .{ .list_reinterpret = .{ .backing_ref = source_local } }, next);
            },
            .nominal_reinterpret => try self.addAssignRef(target_local, .fresh, .{ .nominal = .{ .backing_ref = source_local } }, next),
            .box_unbox => |child_plan| blk: {
                const unboxed_ref = self.physicalBoxChildLayoutRef(self.localLayoutRef(source_local));
                const unboxed_layout = try self.parent.lowerLayoutId(unboxed_ref);
                const unboxed_local = try self.freshLocalWithLayoutAndRef(unboxed_layout, unboxed_ref);
                const bridged = try self.lowerPlannedBridgeIntoLocal(unboxed_local, target_local, next, child_plan);
                const unbox_args = try self.parent.store.addLocalSpan(&.{source_local});
                break :blk try self.parent.store.addCFStmt(.{ .assign_low_level = .{
                    .target = unboxed_local,
                    .result = lowLevelResultSemantics(.box_unbox, &.{source_local}),
                    .ownership = try self.lowLevelOwnership(.box_unbox, &.{source_local}),
                    .op = .box_unbox,
                    .args = unbox_args,
                    .next = bridged,
                } });
            },
            .box_box => |child_plan| blk: {
                const boxed_child_ref = self.physicalBoxChildLayoutRef(self.localLayoutRef(target_local));
                const boxed_child_layout = try self.parent.lowerLayoutId(boxed_child_ref);
                const boxed_child_local = try self.freshLocalWithLayoutAndRef(boxed_child_layout, boxed_child_ref);
                const box_args = try self.parent.store.addLocalSpan(&.{boxed_child_local});
                const box_stmt = try self.parent.store.addCFStmt(.{ .assign_low_level = .{
                    .target = target_local,
                    .result = lowLevelResultSemantics(.box_box, &.{boxed_child_local}),
                    .ownership = try self.lowLevelOwnership(.box_box, &.{boxed_child_local}),
                    .op = .box_box,
                    .args = box_args,
                    .next = next,
                } });
                break :blk try self.lowerPlannedBridgeIntoLocal(source_local, boxed_child_local, box_stmt, child_plan);
            },
            .struct_ => |field_plans| try self.bridgeStructIntoLocal(source_local, target_local, next, field_plans),
            .tag_union => |variant_plans| try self.bridgeTagUnionIntoLocal(source_local, target_local, next, variant_plans),
            .singleton_to_tag_union => |plan| try self.lowerSingletonTagUnionIntoTagUnion(
                source_local,
                target_local,
                next,
                plan.source_payload,
                plan.target_discriminant,
                plan.payload_plan,
            ),
            .tag_union_to_singleton => |plan| try self.lowerTagUnionIntoSingletonTagUnion(
                source_local,
                target_local,
                next,
                plan.target_payload,
                plan.source_discriminant,
                plan.payload_plan,
            ),
        };
    }

    fn bridgeStructIntoLocal(
        self: *ProcLowerer,
        source_local: LIR.LocalId,
        target_local: LIR.LocalId,
        next: LIR.CFStmtId,
        field_plan_span: ir.Ast.Span(ir.Ast.BridgePlanId),
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const ls = &self.parent.layouts;
        const actual_layout = self.localLayout(source_local);
        const target_layout = self.localLayout(target_local);
        if (ls.getLayout(actual_layout).tag == .zst and ls.getLayout(target_layout).tag == .zst) {
            return next;
        }
        const actual_ref = self.localLayoutRef(source_local);
        const target_ref = self.localLayoutRef(target_local);
        const target_info = ls.getStructInfo(ls.getLayout(target_layout));
        const field_plans = self.parent.input.store.sliceBridgePlanSpan(field_plan_span);

        const field_count = target_info.fields.len;
        if (field_plans.len != field_count) {
            debugPanic("lir.from_ir struct bridge plan arity mismatch");
        }
        const source_fields = try self.parent.allocator.alloc(LIR.LocalId, field_count);
        defer self.parent.allocator.free(source_fields);
        const target_fields = try self.parent.allocator.alloc(LIR.LocalId, field_count);
        defer self.parent.allocator.free(target_fields);

        for (0..field_count) |i| {
            const field_index: u16 = @intCast(i);
            const actual_field_ref = self.structFieldLayoutRef(actual_ref, field_index);
            const target_field_ref = self.structFieldLayoutRef(target_ref, field_index);
            const actual_field_layout = self.lowerPhysicalStructFieldLayout(actual_layout, field_index);
            const target_field_layout = self.lowerPhysicalStructFieldLayout(target_layout, field_index);
            source_fields[i] = try self.freshLocalWithLayoutAndRef(actual_field_layout, actual_field_ref);
            target_fields[i] = if (actual_field_layout == target_field_layout and
                layoutRefsEqual(actual_field_ref, target_field_ref))
                source_fields[i]
            else
                try self.freshLocalWithLayoutAndRef(target_field_layout, target_field_ref);
        }
        const assign_struct = try self.parent.store.addCFStmt(.{ .assign_struct = .{
            .target = target_local,
            .result = .fresh,
            .ownership = try self.freshAggregateOwnership(target_fields),
            .fields = try self.parent.store.addLocalSpan(target_fields),
            .next = next,
        } });
        var cursor = assign_struct;
        var bridge_i = source_fields.len;
        while (bridge_i > 0) {
            bridge_i -= 1;
            const source_field = source_fields[bridge_i];
            const target_field = target_fields[bridge_i];
            if (source_field == target_field) continue;
            cursor = try self.lowerPlannedBridgeIntoLocal(source_field, target_field, cursor, field_plans[bridge_i]);
        }

        var i = field_count;
        while (i > 0) {
            i -= 1;
            cursor = try self.addAssignRef(source_fields[i], .fresh, .{ .field = .{
                .source = source_local,
                .field_idx = @intCast(i),
            } }, cursor);
        }

        return cursor;
    }

    fn bridgeTagUnionIntoLocal(
        self: *ProcLowerer,
        source_local: LIR.LocalId,
        target_local: LIR.LocalId,
        next: LIR.CFStmtId,
        variant_plan_span: ir.Ast.Span(ir.Ast.BridgePlanId),
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const ls = &self.parent.layouts;
        const actual_layout = self.localLayout(source_local);
        const target_layout = self.localLayout(target_local);
        const actual_ref = self.localLayoutRef(source_local);
        const target_ref = self.localLayoutRef(target_local);
        const actual_info = ls.getTagUnionInfo(ls.getLayout(actual_layout));
        const target_info = ls.getTagUnionInfo(ls.getLayout(target_layout));
        const variant_plans = self.parent.input.store.sliceBridgePlanSpan(variant_plan_span);

        if (builtin.mode == .Debug and actual_info.variants.len != target_info.variants.len) {
            std.debug.panic(
                "lir.from_ir invariant violated: tag union bridge variant-count mismatch from layout {d} to {d}",
                .{ @intFromEnum(actual_layout), @intFromEnum(target_layout) },
            );
        }

        const default_branch = try self.parent.store.addCFStmt(.runtime_error);
        const branches = try self.parent.allocator.alloc(LIR.CFSwitchBranch, actual_info.variants.len);
        defer self.parent.allocator.free(branches);
        if (variant_plans.len != actual_info.variants.len) {
            debugPanic("lir.from_ir tag-union bridge plan arity mismatch");
        }

        for (0..actual_info.variants.len) |i| {
            const discriminant: u16 = @intCast(i);
            const actual_payload_layout = self.lowerUnionPayloadLayout(actual_layout, discriminant);
            const target_payload_layout = self.lowerUnionPayloadLayout(target_layout, discriminant);
            const target_payload_is_zst = ls.isZeroSized(ls.getLayout(target_payload_layout));

            const branch_body = if (target_payload_is_zst)
                try self.parent.store.addCFStmt(.{ .assign_tag = .{
                    .target = target_local,
                    .result = .fresh,
                    .ownership = try self.freshAggregateOwnership(&.{}),
                    .discriminant = discriminant,
                    .payload = null,
                    .next = next,
                } })
            else blk: {
                const actual_payload_ref = self.unionPayloadLayoutRef(actual_ref, discriminant);
                const target_payload_ref = self.unionPayloadLayoutRef(target_ref, discriminant);
                const actual_payload_local = try self.freshLocalWithLayoutAndRef(actual_payload_layout, actual_payload_ref);
                const target_payload_local = try self.freshLocalWithLayoutAndRef(
                    target_payload_layout,
                    target_payload_ref,
                );

                const assign_tag = try self.parent.store.addCFStmt(.{ .assign_tag = .{
                    .target = target_local,
                    .result = .fresh,
                    .ownership = try self.freshAggregateOwnership(&.{target_payload_local}),
                    .discriminant = discriminant,
                    .payload = target_payload_local,
                    .next = next,
                } });
                const bridged_payload = try self.lowerPlannedBridgeIntoLocal(
                    actual_payload_local,
                    target_payload_local,
                    assign_tag,
                    variant_plans[i],
                );
                break :blk try self.addAssignRef(actual_payload_local, .fresh, .{ .tag_payload_struct = .{
                    .source = source_local,
                    .tag_discriminant = discriminant,
                } }, bridged_payload);
            };

            branches[i] = .{
                .value = i,
                .body = branch_body,
            };
        }

        const cond_local = try self.freshLocalWithRef(self.unionDiscriminantLayoutRef(actual_ref));
        const switch_stmt = try self.parent.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond_local,
            .branches = try self.parent.store.addCFSwitchBranches(branches),
            .default_branch = default_branch,
        } });

        return try self.addAssignRef(cond_local, .fresh, .{ .discriminant = .{ .source = source_local } }, switch_stmt);
    }

    fn lowerBlock(self: *ProcLowerer, block_id: ir.Ast.BlockId, exit: Lowerer.BlockExit) std.mem.Allocator.Error!LIR.CFStmtId {
        const block = self.parent.input.store.getBlock(block_id);
        var next = try self.lowerTerm(block.term, exit);
        const stmt_ids = self.parent.input.store.sliceStmtSpan(block.stmts);
        var i = stmt_ids.len;
        while (i > 0) {
            i -= 1;
            next = try self.lowerStmt(self.parent.input.store.getStmt(stmt_ids[i]), next);
        }
        return next;
    }

    fn lowerRetValue(self: *ProcLowerer, value: ir.Ast.Var) std.mem.Allocator.Error!LIR.CFStmtId {
        const source_local = try self.lowerVar(value);
        const ret_layout = self.procRetLayout();
        self.requireLocalMatchesShape(source_local, ret_layout, self.procRetLayoutRef(), "return");
        return try self.parent.store.addCFStmt(.{ .ret = .{ .value = source_local } });
    }

    fn lowerTerm(self: *ProcLowerer, term: ir.Ast.Term, exit: Lowerer.BlockExit) std.mem.Allocator.Error!LIR.CFStmtId {
        return switch (term) {
            .value => |value| switch (exit) {
                .ret => try self.lowerRetValue(value),
                .jump => |join_id| blk: {
                    const arg = try self.lowerVar(value);
                    const join_local = self.join_locals_by_id.get(@intFromEnum(join_id)) orelse
                        debugPanic("lir.from_ir.lowerTerm missing join local for jump");
                    if (arg == join_local or
                        (self.localLayout(arg) == self.localLayout(join_local) and
                            layoutRefsEqual(self.localLayoutRef(arg), self.localLayoutRef(join_local))))
                    {
                        break :blk if (arg == join_local)
                            try self.parent.store.addCFStmt(.{ .jump = .{
                                .target = join_id,
                                .args = try self.parent.store.addLocalSpan(&.{join_local}),
                            } })
                        else
                            try self.parent.store.addCFStmt(.{ .jump = .{
                                .target = join_id,
                                .args = try self.parent.store.addLocalSpan(&.{arg}),
                            } });
                    }
                    const bridged_local = try self.freshLocalWithLayoutAndRef(
                        self.localLayout(join_local),
                        self.localLayoutRef(join_local),
                    );
                    const jump_stmt = try self.parent.store.addCFStmt(.{ .jump = .{
                        .target = join_id,
                        .args = try self.parent.store.addLocalSpan(&.{bridged_local}),
                    } });
                    break :blk try self.lowerExplicitBridgeIntoLocal(arg, bridged_local, jump_stmt);
                },
                .jump_void => |join_id| try self.parent.store.addCFStmt(.{ .jump = .{
                    .target = join_id,
                    .args = LIR.LocalSpan.empty(),
                } }),
                .loop_continue => try self.parent.store.addCFStmt(.loop_continue),
            },
            .return_ => |value| try self.lowerRetValue(value),
            .crash => |msg| try self.parent.store.addCFStmt(.{ .crash = .{ .msg = try self.parent.lowerStringId(msg) } }),
            .runtime_error => try self.parent.store.addCFStmt(.runtime_error),
            .@"unreachable" => try self.parent.store.addCFStmt(.runtime_error),
        };
    }

    fn lowerStmt(self: *ProcLowerer, stmt: ir.Ast.Stmt, next: LIR.CFStmtId) std.mem.Allocator.Error!LIR.CFStmtId {
        return switch (stmt) {
            .let_ => |let_stmt| try self.lowerExprInto(let_stmt.bind, let_stmt.expr, next),
            .set => |set_stmt| try self.parent.store.addCFStmt(.{ .set_local = .{
                .target = try self.lowerVar(set_stmt.target),
                .value = try self.lowerVar(set_stmt.value),
                .next = next,
            } }),
            .switch_ => |switch_stmt| try self.lowerSwitchStmt(switch_stmt, next),
            .expect => |cond| try self.parent.store.addCFStmt(.{ .expect = .{
                .condition = try self.lowerVar(cond),
                .next = next,
            } }),
            .debug => |message| try self.parent.store.addCFStmt(.{ .debug = .{
                .message = try self.lowerVar(message),
                .next = next,
            } }),
            .break_ => self.currentLoopBreakTarget(),
            .for_list => |for_stmt| blk: {
                try self.loop_break_targets.append(self.parent.allocator, next);
                defer _ = self.loop_break_targets.pop();
                const iterable_local = try self.lowerVar(for_stmt.iterable);
                const body_elem_local = try self.lowerVar(for_stmt.elem);
                const iterable_elem_ref = self.explicitListElemLayoutRef(self.localLayoutRef(iterable_local));
                const iterable_elem_layout = self.lowerListElemLayout(self.localLayout(iterable_local));
                const raw_elem_local = if (self.localMatchesShape(body_elem_local, iterable_elem_layout, iterable_elem_ref))
                    body_elem_local
                else
                    try self.freshLocalWithLayoutAndRef(iterable_elem_layout, iterable_elem_ref);
                const body_core = try self.lowerBlock(for_stmt.body, .loop_continue);
                const body = if (raw_elem_local == body_elem_local)
                    body_core
                else
                    try self.lowerPlannedBridgeIntoLocal(
                        raw_elem_local,
                        body_elem_local,
                        body_core,
                        for_stmt.elem_bridge_plan,
                    );
                break :blk try self.parent.store.addCFStmt(.{ .for_list = .{
                    .elem = raw_elem_local,
                    .elem_result = .fresh,
                    .elem_ownership = .{ .materialization = .copy_from_borrowed_input },
                    .iterable = iterable_local,
                    .iterable_elem_layout = iterable_elem_layout,
                    .body = body,
                    .next = next,
                } });
            },
            .while_ => |while_stmt| try self.lowerWhileStmt(while_stmt, next),
        };
    }

    fn lowerSwitchStmt(
        self: *ProcLowerer,
        switch_stmt: @FieldType(ir.Ast.Stmt, "switch_"),
        next: LIR.CFStmtId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const join_var = switch_stmt.join orelse debugPanic("lir.from_ir.lowerSwitchStmt missing join var");
        const join_local = try self.lowerVar(join_var);
        const join_id = self.parent.nextJoin();
        try self.join_locals_by_id.put(@intFromEnum(join_id), join_local);

        const default_body = try self.lowerBlock(switch_stmt.default_block, .{ .jump = join_id });
        const ir_branches = self.parent.input.store.sliceBranchSpan(switch_stmt.branches);
        var lowered_branches = try self.parent.allocator.alloc(LIR.CFSwitchBranch, ir_branches.len);
        defer self.parent.allocator.free(lowered_branches);
        for (ir_branches, 0..) |branch_id, i| {
            const branch = self.parent.input.store.getBranch(branch_id);
            lowered_branches[i] = .{
                .value = branch.value,
                .body = try self.lowerBlock(branch.block, .{ .jump = join_id }),
            };
        }

        const switch_body = try self.parent.store.addCFStmt(.{ .switch_stmt = .{
            .cond = try self.lowerVar(switch_stmt.cond),
            .branches = try self.parent.store.addCFSwitchBranches(lowered_branches),
            .default_branch = default_body,
        } });

        return try self.parent.store.addCFStmt(.{ .join = .{
            .id = join_id,
            .params = try self.parent.store.addLocalSpan(&.{join_local}),
            .body = next,
            .remainder = switch_body,
        } });
    }

    fn lowerWhileStmt(
        self: *ProcLowerer,
        while_stmt: @FieldType(ir.Ast.Stmt, "while_"),
        next: LIR.CFStmtId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const loop_id = self.parent.nextJoin();
        try self.loop_break_targets.append(self.parent.allocator, next);
        defer _ = self.loop_break_targets.pop();
        const body = try self.lowerBlock(while_stmt.body, .{ .jump_void = loop_id });
        const cond = try self.lowerConditionBlock(while_stmt.cond, body, next);
        const enter_loop = try self.parent.store.addCFStmt(.{ .jump = .{
            .target = loop_id,
            .args = LIR.LocalSpan.empty(),
        } });
        return try self.parent.store.addCFStmt(.{ .join = .{
            .id = loop_id,
            .params = LIR.LocalSpan.empty(),
            .body = cond,
            .remainder = enter_loop,
        } });
    }

    fn currentLoopBreakTarget(self: *const ProcLowerer) LIR.CFStmtId {
        if (self.loop_break_targets.items.len == 0) {
            return debugPanic("lir.from_ir.break_ invariant violated: break outside loop");
        }
        return self.loop_break_targets.items[self.loop_break_targets.items.len - 1];
    }

    fn lowerConditionBlock(
        self: *ProcLowerer,
        block_id: ir.Ast.BlockId,
        true_body: LIR.CFStmtId,
        false_body: LIR.CFStmtId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const block = self.parent.input.store.getBlock(block_id);
        var next = switch (block.term) {
            .value => |value| blk: {
                const branches = try self.parent.store.addCFSwitchBranches(&.{
                    .{ .value = 1, .body = true_body },
                });
                break :blk try self.parent.store.addCFStmt(.{ .switch_stmt = .{
                    .cond = try self.lowerVar(value),
                    .branches = branches,
                    .default_branch = false_body,
                } });
            },
            else => try self.lowerTerm(block.term, .ret),
        };

        const stmt_ids = self.parent.input.store.sliceStmtSpan(block.stmts);
        var i = stmt_ids.len;
        while (i > 0) {
            i -= 1;
            next = try self.lowerStmt(self.parent.input.store.getStmt(stmt_ids[i]), next);
        }
        return next;
    }

    fn lowerExprInto(
        self: *ProcLowerer,
        bind: ir.Ast.Var,
        expr_id: ir.Ast.ExprId,
        next: LIR.CFStmtId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const target = try self.lowerVar(bind);
        const expr = self.parent.input.store.getExpr(expr_id);
        return switch (expr) {
            .bridge => |value| blk: {
                const source = try self.lowerVar(value.value);
                break :blk try self.lowerPlannedBridgeIntoLocal(
                    source,
                    target,
                    next,
                    value.plan,
                );
            },
            .var_ => |value| blk: {
                const source = try self.lowerVar(value);
                if (source == target) break :blk next;
                if (!layoutRefsEqual(self.localLayoutRef(source), self.localLayoutRef(target))) {
                    break :blk try self.lowerExplicitBridgeIntoLocal(source, target, next);
                }
                self.requireLocalMatchesShape(source, self.localLayout(target), self.localLayoutRef(target), "var");
                break :blk try self.addAssignRef(target, .fresh, .{ .local = source }, next);
            },
            .lit => |lit| try self.parent.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .result = .fresh,
                .value = try self.parent.lowerLiteralWithLayout(lit, bind.layout),
                .next = next,
            } }),
            .fn_ptr => |name| try self.parent.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .result = .fresh,
                .value = .{ .proc_ref = self.lookupProcId(name) },
                .next = next,
            } }),
            .null_ptr => try self.parent.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .result = .fresh,
                .value = .null_ptr,
                .next = next,
            } }),
            .make_union => |union_expr| blk: {
                if (self.parent.layouts.getLayout(self.localLayout(target)).tag == .zst) {
                    break :blk next;
                }
                var payload_source: ?LIR.LocalId = null;
                var payload_local: ?LIR.LocalId = null;
                if (union_expr.payload) |payload| {
                    const source_local = try self.lowerVar(payload);
                    const payload_ref = self.unionPayloadLayoutRef(self.localLayoutRef(target), union_expr.discriminant);
                    const payload_layout = self.lowerUnionPayloadLayout(
                        self.localLayout(target),
                        union_expr.discriminant,
                    );
                    payload_source = source_local;
                    self.requireLocalMatchesShape(source_local, payload_layout, payload_ref, "make_union payload");
                    payload_local = source_local;
                }

                const assign_tag = try self.parent.store.addCFStmt(.{ .assign_tag = .{
                    .target = target,
                    .result = .fresh,
                    .ownership = try self.freshAggregateOwnership(if (payload_local) |local| &.{local} else &.{}),
                    .discriminant = union_expr.discriminant,
                    .payload = payload_local,
                    .next = next,
                } });

                if (payload_source) |source_local| {
                    if (payload_local.? != source_local) {
                        debugPanic("lir.from_ir make_union payload invariant violated");
                    }
                    break :blk assign_tag;
                }
                break :blk assign_tag;
            },
            .get_union_id => |value| blk: {
                const source_local = try self.lowerVar(value);
                const source_layout = self.parent.layouts.getLayout(self.localLayout(source_local));
                if (source_layout.tag == .zst) {
                    break :blk try self.parent.store.addCFStmt(.{ .assign_literal = .{
                        .target = target,
                        .result = .fresh,
                        .value = .{ .i64_literal = .{ .value = 0, .layout_idx = self.localLayout(target) } },
                        .next = next,
                    } });
                }
                if (source_layout.tag == .tag_union) {
                    const tu_data = self.parent.layouts.getTagUnionData(source_layout.data.tag_union.idx);
                    if (tu_data.discriminant_size == 0) {
                        break :blk try self.parent.store.addCFStmt(.{ .assign_literal = .{
                            .target = target,
                            .result = .fresh,
                            .value = .{ .i64_literal = .{ .value = 0, .layout_idx = self.localLayout(target) } },
                            .next = next,
                        } });
                    }
                }
                break :blk try self.addAssignRef(target, .fresh, .{ .discriminant = .{ .source = source_local } }, next);
            },
            .get_union_struct => |payload| blk: {
                const source = try self.lowerVar(payload.value);
                if (self.parent.layouts.getLayout(self.localLayout(source)).tag == .zst) {
                    break :blk next;
                }
                const logical_payload_ref = self.unionPayloadLayoutRef(self.localLayoutRef(source), payload.tag_discriminant);
                const logical_payload_layout = try self.parent.lowerLayoutId(logical_payload_ref);
                const physical_payload_layout = self.lowerUnionPayloadLayout(
                    self.localLayout(source),
                    payload.tag_discriminant,
                );
                const access = if (physical_payload_layout == logical_payload_layout) blk_access: {
                    self.requireLocalMatchesShape(target, logical_payload_layout, logical_payload_ref, "get_union_struct");
                    break :blk_access try self.addAssignRef(target, .fresh, .{ .tag_payload_struct = .{
                        .source = source,
                        .tag_discriminant = payload.tag_discriminant,
                    } }, next);
                } else blk_access: {
                    self.requireLocalMatchesShape(target, logical_payload_layout, logical_payload_ref, "get_union_struct");
                    const raw_payload = try self.freshLocalWithLayoutAndRef(
                        physical_payload_layout,
                        logical_payload_ref,
                    );
                    const bridged = try self.lowerExplicitBridgeIntoLocal(raw_payload, target, next);
                    break :blk_access try self.addAssignRef(raw_payload, .fresh, .{ .tag_payload_struct = .{
                        .source = source,
                        .tag_discriminant = payload.tag_discriminant,
                    } }, bridged);
                };
                break :blk access;
            },
            .make_struct => |fields| blk: {
                if (self.parent.layouts.getLayout(self.localLayout(target)).tag == .zst) {
                    break :blk next;
                }
                const source_locals = try self.lowerVarSpan(self.parent.input.store.sliceVarSpan(fields));
                defer self.parent.allocator.free(source_locals);
                const field_locals = try self.parent.allocator.alloc(LIR.LocalId, source_locals.len);
                defer self.parent.allocator.free(field_locals);

                for (source_locals, 0..) |source_local, i| {
                    const field_index: u16 = @intCast(i);
                    const slot_layout = self.lowerPhysicalStructFieldLayout(self.localLayout(target), field_index);
                    const slot_ref = self.structFieldLayoutRef(self.localLayoutRef(target), field_index);
                    field_locals[i] = if (self.localMatchesShape(source_local, slot_layout, slot_ref))
                        source_local
                    else
                        try self.freshLocalWithLayoutAndRef(slot_layout, slot_ref);
                }

                const span = try self.parent.store.addLocalSpan(field_locals);
                const assign_struct = try self.parent.store.addCFStmt(.{ .assign_struct = .{
                    .target = target,
                    .result = .fresh,
                    .ownership = try self.freshAggregateOwnership(field_locals),
                    .fields = span,
                    .next = next,
                } });
                var cursor = assign_struct;
                var bridge_i = field_locals.len;
                while (bridge_i > 0) {
                    bridge_i -= 1;
                    if (field_locals[bridge_i] != source_locals[bridge_i]) {
                        cursor = try self.lowerExplicitBridgeIntoLocal(
                            source_locals[bridge_i],
                            field_locals[bridge_i],
                            cursor,
                        );
                    }
                }
                break :blk cursor;
            },
            .layout_size => |layout_ref| blk: {
                const layout_idx = try self.parent.lowerLayoutId(layout_ref);
                const layout_val = self.parent.layouts.getLayout(layout_idx);
                const size = self.parent.layouts.layoutSize(layout_val);
                break :blk try self.parent.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .result = .fresh,
                    .value = .{ .i64_literal = .{ .value = @intCast(size), .layout_idx = .u32 } },
                    .next = next,
                } });
            },
            .make_list => |list| blk: {
                const source_vars = self.parent.input.store.sliceVarSpan(list.elems);
                const elem_plans = self.parent.input.store.sliceBridgePlanSpan(list.elem_bridge_plans);
                if (source_vars.len != elem_plans.len) {
                    debugPanic("lir.from_ir make_list bridge plan arity mismatch");
                }
                const source_locals = try self.lowerVarSpan(source_vars);
                defer self.parent.allocator.free(source_locals);
                const elem_ref = self.explicitListElemLayoutRef(self.localLayoutRef(target));
                const elem_layout = self.lowerListElemLayout(self.localLayout(target));
                const elem_locals = try self.parent.allocator.alloc(LIR.LocalId, source_locals.len);
                defer self.parent.allocator.free(elem_locals);
                for (source_locals, 0..) |source_local, i| {
                    elem_locals[i] = if (self.localMatchesShape(source_local, elem_layout, elem_ref))
                        source_local
                    else
                        try self.freshLocalWithLayoutAndRef(elem_layout, elem_ref);
                }
                const assign_list = try self.parent.store.addCFStmt(.{ .assign_list = .{
                    .target = target,
                    .result = .fresh,
                    .ownership = try self.freshAggregateOwnership(elem_locals),
                    .elems = try self.parent.store.addLocalSpan(elem_locals),
                    .next = next,
                } });
                var cursor = assign_list;
                var bridge_i = elem_locals.len;
                while (bridge_i > 0) {
                    bridge_i -= 1;
                    if (elem_locals[bridge_i] != source_locals[bridge_i]) {
                        cursor = try self.lowerPlannedBridgeIntoLocal(
                            source_locals[bridge_i],
                            elem_locals[bridge_i],
                            cursor,
                            elem_plans[bridge_i],
                        );
                    }
                }
                break :blk cursor;
            },
            .get_struct_field => |field| blk: {
                const source = try self.lowerVar(field.record);
                if (self.parent.layouts.getLayout(self.localLayout(source)).tag == .zst) {
                    break :blk next;
                }
                const logical_field_ref = self.structFieldLayoutRef(self.localLayoutRef(source), field.field_index);
                const logical_field_layout = try self.parent.lowerLayoutId(logical_field_ref);
                const physical_field_layout = self.lowerPhysicalStructFieldLayout(self.localLayout(source), field.field_index);
                const access = if (physical_field_layout == logical_field_layout) blk_access: {
                    self.requireLocalMatchesShape(target, logical_field_layout, logical_field_ref, "get_struct_field");
                    break :blk_access try self.addAssignRef(target, .fresh, .{ .field = .{
                        .source = source,
                        .field_idx = field.field_index,
                    } }, next);
                } else blk_access: {
                    self.requireLocalMatchesShape(target, logical_field_layout, logical_field_ref, "get_struct_field");
                    const raw_field = try self.freshLocalWithLayoutAndRef(
                        physical_field_layout,
                        logical_field_ref,
                    );
                    const bridged = try self.lowerExplicitBridgeIntoLocal(raw_field, target, next);
                    break :blk_access try self.addAssignRef(raw_field, .fresh, .{ .field = .{
                        .source = source,
                        .field_idx = field.field_index,
                    } }, bridged);
                };
                break :blk access;
            },
            .call_direct => |call| blk: {
                const source_locals = try self.lowerVarSpan(self.parent.input.store.sliceVarSpan(call.args));
                defer self.parent.allocator.free(source_locals);
                const proc_id = self.lookupProcId(call.proc);
                const proc_spec = self.parent.store.getProcSpec(proc_id);
                const param_locals = self.parent.store.getLocalSpan(proc_spec.args);
                if (builtin.mode == .Debug and source_locals.len != param_locals.len) {
                    std.debug.panic(
                        "lir.from_ir invariant violated: direct call to proc {d} lowered {d} args but callee expects {d}",
                        .{ @intFromEnum(proc_id), source_locals.len, param_locals.len },
                    );
                }

                const call_args = try self.parent.allocator.alloc(LIR.LocalId, source_locals.len);
                defer self.parent.allocator.free(call_args);

                for (source_locals, param_locals, 0..) |source_local, param_local, i| {
                    const expected_layout = self.localLayout(param_local);
                    const expected_ref = self.localLayoutRef(param_local);
                    call_args[i] = if (self.localMatchesShape(source_local, expected_layout, expected_ref))
                        source_local
                    else
                        try self.freshLocalWithLayoutAndRef(expected_layout, expected_ref);
                }

                const proc_ret_ref = self.parent.proc_ret_layout_refs.get(@intFromEnum(proc_id)) orelse
                    debugPanic("lir.from_ir.call_direct missing explicit callee return layout ref");
                if (!self.localMatchesShape(target, proc_spec.ret_layout, proc_ret_ref)) {
                    std.debug.panic(
                        "lir.from_ir invariant violated: call_direct result for proc {d} missing explicit bridge from layout {d} to layout {d}",
                        .{
                            call.proc.raw(),
                            @intFromEnum(self.localLayout(target)),
                            @intFromEnum(proc_spec.ret_layout),
                        },
                    );
                }

                const assign_call = try self.parent.store.addCFStmt(.{ .assign_call = .{
                    .target = target,
                    .result = .fresh,
                    .proc = proc_id,
                    .args = try self.parent.store.addLocalSpan(call_args),
                    .next = next,
                } });
                var cursor = assign_call;
                var bridge_i = call_args.len;
                while (bridge_i > 0) {
                    bridge_i -= 1;
                    if (call_args[bridge_i] != source_locals[bridge_i]) {
                        cursor = try self.lowerExplicitBridgeIntoLocal(
                            source_locals[bridge_i],
                            call_args[bridge_i],
                            cursor,
                        );
                    }
                }
                break :blk cursor;
            },
            .call_erased => |call| blk: {
                const locals = try self.lowerVarSpan(self.parent.input.store.sliceVarSpan(call.args));
                defer self.parent.allocator.free(locals);
                break :blk try self.parent.store.addCFStmt(.{ .assign_call_erased = .{
                    .target = target,
                    .result = .fresh,
                    .ownership = .{},
                    .closure = try self.lowerVar(call.func),
                    .args = try self.parent.store.addLocalSpan(locals),
                    .capture_layout = if (call.capture_layout) |capture_ref|
                        try self.parent.lowerLayoutId(capture_ref)
                    else
                        null,
                    .next = next,
                } });
            },
            .call_low_level => |call| blk: {
                const locals = try self.lowerVarSpan(self.parent.input.store.sliceVarSpan(call.args));
                defer self.parent.allocator.free(locals);

                if (call.op == .list_get_unsafe or call.op == .list_first or call.op == .list_last) {
                    if (locals.len == 0) {
                        debugPanic("lir.from_ir list element low-level missing list argument");
                    }
                    const elem_ref = self.explicitListElemLayoutRef(self.localLayoutRef(locals[0]));
                    const elem_layout = self.lowerListElemLayout(self.localLayout(locals[0]));
                    self.requireLocalMatchesShape(target, elem_layout, elem_ref, "call_low_level list result");
                }

                break :blk try self.parent.store.addCFStmt(.{ .assign_low_level = .{
                    .target = target,
                    .result = lowLevelResultSemantics(call.op, locals),
                    .ownership = try self.lowLevelOwnership(call.op, locals),
                    .op = call.op,
                    .args = try self.parent.store.addLocalSpan(locals),
                    .next = next,
                } });
            },
        };
    }

    fn lookupProcId(self: *ProcLowerer, symbol: ir.Ast.Symbol) LIR.LirProcSpecId {
        return self.parent.proc_ids_by_symbol.get(symbol.raw()) orelse debugPanic("lir.from_ir.lookupProcId missing proc");
    }
};

fn lirSymbol(symbol: ir.Ast.Symbol) LIR.Symbol {
    return LIR.Symbol.fromRaw(symbol.raw());
}

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}

test "lir from ir tests" {
    std.testing.refAllDecls(@This());
}
