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

pub fn run(
    allocator: std.mem.Allocator,
    all_module_envs: []const *const ModuleEnv,
    builtin_str_ident: ?base.Ident.Idx,
    target_usize: base.target.TargetUsize,
    input: ir.Lower.Result,
) std.mem.Allocator.Error!Result {
    var lowerer = try Lowerer.init(allocator, all_module_envs, builtin_str_ident, target_usize, input);
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
            .ir_to_layout = std.AutoHashMap(u32, layout_mod.Idx).init(allocator),
            .next_join_id = 0,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.ir_to_layout.deinit();
        self.proc_ids_by_symbol.deinit();
        self.root_procs.deinit(self.allocator);
        self.layouts.deinit();
        self.store.deinit();
        self.input.deinit();
    }

    fn finish(self: *Lowerer) Result {
        const result = Result{
            .store = self.store,
            .layouts = self.layouts,
            .root_procs = self.root_procs,
            .proc_ids_by_symbol = self.proc_ids_by_symbol,
        };
        self.input.deinit();
        return result;
    }

    fn registerProcPlaceholders(self: *Lowerer) std.mem.Allocator.Error!void {
        for (self.input.store.defsSlice()) |def| {
            const ir_args = self.input.store.sliceVarSpan(def.args);
            const arg_locals = try self.allocator.alloc(LIR.LocalId, ir_args.len);
            defer self.allocator.free(arg_locals);
            for (ir_args, 0..) |arg, i| {
                arg_locals[i] = try self.store.addLocal(.{
                    .layout_idx = try self.lowerLayoutId(arg.layout),
                });
            }
            const proc_id = try self.store.addProcSpec(.{
                .name = lirSymbol(def.name),
                .args = try self.store.addLocalSpan(arg_locals),
                .body = try self.store.addCFStmt(.runtime_error),
                .ret_layout = try self.lowerLayoutId(def.ret_layout),
                .result_contract = .fresh,
            });
            try self.proc_ids_by_symbol.put(def.name.raw(), proc_id);
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
        const body = try proc.lowerBlock(def.body, .ret);
        const ret_layout = try self.lowerLayoutId(def.ret_layout);

        proc_ptr.* = .{
            .name = lirSymbol(def.name),
            .args = arg_span,
            .body = body,
            .ret_layout = ret_layout,
            .result_contract = .fresh,
        };
    }

    fn lowerLayoutId(self: *Lowerer, ref: ir.Layout.Ref) std.mem.Allocator.Error!layout_mod.Idx {
        return switch (ref) {
            .canonical => |layout_idx| layout_idx,
            .local => |node_id| blk: {
                const key = @intFromEnum(node_id);
                if (self.ir_to_layout.get(key)) |existing| break :blk existing;

                var commit = try self.layouts.commitGraph(&self.input.layouts, ref);
                defer commit.deinit(self.allocator);

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

    fn lowerLiteral(self: *Lowerer, lit: ir.Ast.Lit) std.mem.Allocator.Error!LIR.LiteralValue {
        return switch (lit) {
            .int => |value| .{ .i128_literal = .{ .value = value, .layout_idx = .i128 } },
            .f32 => |value| .{ .f32_literal = value },
            .f64 => |value| .{ .f64_literal = value },
            .dec => |value| .{ .dec_literal = value },
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
            .loop_break_targets = .empty,
        };
    }

    fn deinit(self: *ProcLowerer) void {
        self.locals_by_var.deinit();
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

    fn procRetLayout(self: *const ProcLowerer) layout_mod.Idx {
        return self.parent.store.getProcSpec(self.proc_id).ret_layout;
    }

    fn freshLocalWithLayout(self: *ProcLowerer, layout_idx: layout_mod.Idx) std.mem.Allocator.Error!LIR.LocalId {
        return self.parent.store.addLocal(.{ .layout_idx = layout_idx });
    }

    fn lowerStructFieldLayout(
        self: *ProcLowerer,
        struct_layout_idx: layout_mod.Idx,
        field_index: u16,
    ) layout_mod.Idx {
        const ls = &self.parent.layouts;
        const struct_layout = ls.getLayout(struct_layout_idx);
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
            .scalar, .box_of_zst, .list, .list_of_zst, .struct_, .closure, .zst => {
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
            .scalar, .box_of_zst, .list, .list_of_zst, .struct_, .closure, .zst => {
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

    fn bridgeValueToLayout(
        self: *ProcLowerer,
        source_local: LIR.LocalId,
        target_layout: layout_mod.Idx,
        next: LIR.CFStmtId,
    ) std.mem.Allocator.Error!struct { local: LIR.LocalId, next: LIR.CFStmtId } {
        const actual_layout = self.localLayout(source_local);
        if (actual_layout == target_layout) {
            return .{ .local = source_local, .next = next };
        }

        const target_local = try self.freshLocalWithLayout(target_layout);
        return .{
            .local = target_local,
            .next = try self.bridgeValueIntoLocal(source_local, target_local, next),
        };
    }

    fn bridgeValueIntoLocal(
        self: *ProcLowerer,
        source_local: LIR.LocalId,
        target_local: LIR.LocalId,
        next: LIR.CFStmtId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const actual_layout = self.localLayout(source_local);
        const target_layout = self.localLayout(target_local);
        if (actual_layout == target_layout) {
            return try self.parent.store.addCFStmt(.{ .assign_ref = .{
                .target = target_local,
                .result = .{ .alias_of = .{ .owner = source_local } },
                .op = .{ .local = source_local },
                .next = next,
            } });
        }

        const ls = &self.parent.layouts;
        const actual = ls.getLayout(actual_layout);
        const target = ls.getLayout(target_layout);

        if (ls.layoutsHaveSameRuntimeRepresentation(actual_layout, target_layout)) {
            return try self.parent.store.addCFStmt(.{ .assign_ref = .{
                .target = target_local,
                .result = .fresh,
                .op = .{ .nominal = .{ .backing_ref = source_local } },
                .next = next,
            } });
        }

        if (actual.tag == .box and actual.data.box == target_layout) {
            const args = try self.parent.store.addLocalSpan(&.{source_local});
            return try self.parent.store.addCFStmt(.{ .assign_low_level = .{
                .target = target_local,
                .result = .fresh,
                .op = .box_unbox,
                .args = args,
                .next = next,
            } });
        }

        if (target.tag == .box and target.data.box == actual_layout) {
            const args = try self.parent.store.addLocalSpan(&.{source_local});
            return try self.parent.store.addCFStmt(.{ .assign_low_level = .{
                .target = target_local,
                .result = .fresh,
                .op = .box_box,
                .args = args,
                .next = next,
            } });
        }

        if (target.tag == .box and ls.layoutsHaveSameRuntimeRepresentation(actual_layout, target.data.box)) {
            const recast_local = try self.freshLocalWithLayout(target.data.box);
            const box_args = try self.parent.store.addLocalSpan(&.{recast_local});
            const box_stmt = try self.parent.store.addCFStmt(.{ .assign_low_level = .{
                .target = target_local,
                .result = .fresh,
                .op = .box_box,
                .args = box_args,
                .next = next,
            } });
            const recast_stmt = try self.parent.store.addCFStmt(.{ .assign_ref = .{
                .target = recast_local,
                .result = .fresh,
                .op = .{ .nominal = .{ .backing_ref = source_local } },
                .next = box_stmt,
            } });
            return recast_stmt;
        }

        if (actual.tag == .box and ls.layoutsHaveSameRuntimeRepresentation(actual.data.box, target_layout)) {
            const unboxed_local = try self.freshLocalWithLayout(actual.data.box);
            const recast_stmt = try self.parent.store.addCFStmt(.{ .assign_ref = .{
                .target = target_local,
                .result = .fresh,
                .op = .{ .nominal = .{ .backing_ref = unboxed_local } },
                .next = next,
            } });
            const unbox_args = try self.parent.store.addLocalSpan(&.{source_local});
            const unbox_stmt = try self.parent.store.addCFStmt(.{ .assign_low_level = .{
                .target = unboxed_local,
                .result = .fresh,
                .op = .box_unbox,
                .args = unbox_args,
                .next = recast_stmt,
            } });
            return unbox_stmt;
        }

        if (actual.tag == .struct_ and target.tag == .struct_) {
            return try self.bridgeStructIntoLocal(source_local, target_local, next);
        }

        if (actual.tag == .tag_union and target.tag == .tag_union) {
            return try self.bridgeTagUnionIntoLocal(source_local, target_local, next);
        }

        if (target.tag == .box) {
            const boxed_child_local = try self.freshLocalWithLayout(target.data.box);
            const box_args = try self.parent.store.addLocalSpan(&.{boxed_child_local});
            const box_stmt = try self.parent.store.addCFStmt(.{ .assign_low_level = .{
                .target = target_local,
                .result = .fresh,
                .op = .box_box,
                .args = box_args,
                .next = next,
            } });
            return try self.bridgeValueIntoLocal(source_local, boxed_child_local, box_stmt);
        }

        if (actual.tag == .box) {
            const unboxed_local = try self.freshLocalWithLayout(actual.data.box);
            const bridged = try self.bridgeValueIntoLocal(unboxed_local, target_local, next);
            const unbox_args = try self.parent.store.addLocalSpan(&.{source_local});
            return try self.parent.store.addCFStmt(.{ .assign_low_level = .{
                .target = unboxed_local,
                .result = .fresh,
                .op = .box_unbox,
                .args = unbox_args,
                .next = bridged,
            } });
        }

        if (builtin.mode == .Debug) {
            const actual_layout_val = ls.getLayout(actual_layout);
            const target_layout_val = ls.getLayout(target_layout);
            const target_box_child: ?layout_mod.Idx = if (target_layout_val.tag == .box) target_layout_val.data.box else null;
            const actual_box_child: ?layout_mod.Idx = if (actual_layout_val.tag == .box) actual_layout_val.data.box else null;
            const target_box_child_tag: ?[]const u8 = if (target_box_child) |child| @tagName(ls.getLayout(child).tag) else null;
            const actual_box_child_tag: ?[]const u8 = if (actual_box_child) |child| @tagName(ls.getLayout(child).tag) else null;
            const box_child_matches = if (target_box_child) |child| ls.layoutsHaveSameRuntimeRepresentation(actual_layout, child) else false;
            std.debug.panic(
                "lir.from_ir invariant violated: no explicit bridge from layout {d} ({s}, child={any}/{any}) to layout {d} ({s}, child={any}/{any}, child_match={})",
                .{
                    @intFromEnum(actual_layout),
                    @tagName(actual_layout_val.tag),
                    actual_box_child,
                    actual_box_child_tag,
                    @intFromEnum(target_layout),
                    @tagName(target_layout_val.tag),
                    target_box_child,
                    target_box_child_tag,
                    box_child_matches,
                },
            );
        }
        unreachable;
    }

    fn bridgeStructIntoLocal(
        self: *ProcLowerer,
        source_local: LIR.LocalId,
        target_local: LIR.LocalId,
        next: LIR.CFStmtId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const ls = &self.parent.layouts;
        const actual_layout = self.localLayout(source_local);
        const target_layout = self.localLayout(target_local);
        const actual_info = ls.getStructInfo(ls.getLayout(actual_layout));
        const target_info = ls.getStructInfo(ls.getLayout(target_layout));

        if (builtin.mode == .Debug) {
            if (actual_info.fields.len != target_info.fields.len) {
                std.debug.panic(
                    "lir.from_ir invariant violated: struct bridge field-count mismatch from layout {d} to {d}",
                    .{ @intFromEnum(actual_layout), @intFromEnum(target_layout) },
                );
            }
            for (0..target_info.fields.len) |i| {
                const actual_field = actual_info.fields.get(@intCast(i));
                const target_field = target_info.fields.get(@intCast(i));
                if (actual_field.index != target_field.index) {
                    std.debug.panic(
                        "lir.from_ir invariant violated: struct bridge semantic field-index mismatch at position {d}: {d} vs {d}",
                        .{ i, actual_field.index, target_field.index },
                    );
                }
            }
        }

        const field_count = target_info.fields.len;
        const source_fields = try self.parent.allocator.alloc(LIR.LocalId, field_count);
        defer self.parent.allocator.free(source_fields);
        const target_fields = try self.parent.allocator.alloc(LIR.LocalId, field_count);
        defer self.parent.allocator.free(target_fields);

        for (0..field_count) |i| {
            const field_index: u16 = @intCast(i);
            const actual_field_layout = self.lowerStructFieldLayout(actual_layout, field_index);
            const target_field_layout = self.lowerStructFieldLayout(target_layout, field_index);
            source_fields[i] = try self.freshLocalWithLayout(actual_field_layout);
            target_fields[i] = if (actual_field_layout == target_field_layout)
                source_fields[i]
            else
                try self.freshLocalWithLayout(target_field_layout);
        }

        const assign_struct = try self.parent.store.addCFStmt(.{ .assign_struct = .{
            .target = target_local,
            .result = .fresh,
            .fields = try self.parent.store.addLocalSpan(target_fields),
            .next = next,
        } });
        var cursor = try self.emitBridgesIntoLocals(source_fields, target_fields, assign_struct);

        var i = field_count;
        while (i > 0) {
            i -= 1;
            cursor = try self.parent.store.addCFStmt(.{ .assign_ref = .{
                .target = source_fields[i],
                .result = .fresh,
                .op = .{ .field = .{
                    .source = source_local,
                    .field_idx = @intCast(i),
                } },
                .next = cursor,
            } });
        }

        return cursor;
    }

    fn bridgeTagUnionIntoLocal(
        self: *ProcLowerer,
        source_local: LIR.LocalId,
        target_local: LIR.LocalId,
        next: LIR.CFStmtId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        const ls = &self.parent.layouts;
        const actual_layout = self.localLayout(source_local);
        const target_layout = self.localLayout(target_local);
        const actual_info = ls.getTagUnionInfo(ls.getLayout(actual_layout));
        const target_info = ls.getTagUnionInfo(ls.getLayout(target_layout));

        if (builtin.mode == .Debug and actual_info.variants.len != target_info.variants.len) {
            std.debug.panic(
                "lir.from_ir invariant violated: tag union bridge variant-count mismatch from layout {d} to {d}",
                .{ @intFromEnum(actual_layout), @intFromEnum(target_layout) },
            );
        }

        const default_branch = try self.parent.store.addCFStmt(.runtime_error);
        const branches = try self.parent.allocator.alloc(LIR.CFSwitchBranch, actual_info.variants.len);
        defer self.parent.allocator.free(branches);

        for (0..actual_info.variants.len) |i| {
            const discriminant: u16 = @intCast(i);
            const actual_payload_layout = self.lowerUnionPayloadLayout(actual_layout, discriminant);
            const target_payload_layout = self.lowerUnionPayloadLayout(target_layout, discriminant);
            const target_payload_is_zst = ls.isZeroSized(ls.getLayout(target_payload_layout));

            const branch_body = if (target_payload_is_zst)
                try self.parent.store.addCFStmt(.{ .assign_tag = .{
                    .target = target_local,
                    .result = .fresh,
                    .discriminant = discriminant,
                    .payload = null,
                    .next = next,
                } })
            else blk: {
                const actual_payload_local = try self.freshLocalWithLayout(actual_payload_layout);
                const target_payload_local = if (actual_payload_layout == target_payload_layout)
                    actual_payload_local
                else
                    try self.freshLocalWithLayout(target_payload_layout);

                const assign_tag = try self.parent.store.addCFStmt(.{ .assign_tag = .{
                    .target = target_local,
                    .result = .fresh,
                    .discriminant = discriminant,
                    .payload = target_payload_local,
                    .next = next,
                } });
                const bridged_payload = try self.bridgeValueIntoLocal(actual_payload_local, target_payload_local, assign_tag);
                break :blk try self.parent.store.addCFStmt(.{ .assign_ref = .{
                    .target = actual_payload_local,
                    .result = .fresh,
                    .op = .{ .tag_payload_struct = .{
                        .source = source_local,
                        .tag_discriminant = discriminant,
                    } },
                    .next = bridged_payload,
                } });
            };

            branches[i] = .{
                .value = i,
                .body = branch_body,
            };
        }

        const cond_local = try self.freshLocalWithLayout(self.unionDiscriminantLayout(actual_layout));
        const switch_stmt = try self.parent.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond_local,
            .branches = try self.parent.store.addCFSwitchBranches(branches),
            .default_branch = default_branch,
        } });

        return try self.parent.store.addCFStmt(.{ .assign_ref = .{
            .target = cond_local,
            .result = .fresh,
            .op = .{ .discriminant = .{ .source = source_local } },
            .next = switch_stmt,
        } });
    }

    fn emitBridgesIntoLocals(
        self: *ProcLowerer,
        source_locals: []const LIR.LocalId,
        target_locals: []const LIR.LocalId,
        next: LIR.CFStmtId,
    ) std.mem.Allocator.Error!LIR.CFStmtId {
        if (source_locals.len != target_locals.len) {
            debugPanic("lir.from_ir invariant violated: bridge source/target local arity mismatch");
        }
        var cursor = next;
        var i = source_locals.len;
        while (i > 0) {
            i -= 1;
            const source_local = source_locals[i];
            const target_local = target_locals[i];
            if (source_local == target_local) continue;
            cursor = try self.bridgeValueIntoLocal(source_local, target_local, cursor);
        }
        return cursor;
    }

    fn planStructFieldsIntoTarget(
        self: *ProcLowerer,
        target_local: LIR.LocalId,
        fields: []const ir.Ast.Var,
    ) std.mem.Allocator.Error!struct { sources: []LIR.LocalId, fields: []LIR.LocalId } {
        const sources = try self.parent.allocator.alloc(LIR.LocalId, fields.len);
        errdefer self.parent.allocator.free(sources);
        const lowered = try self.parent.allocator.alloc(LIR.LocalId, fields.len);
        errdefer self.parent.allocator.free(lowered);

        for (fields, 0..) |field, i| {
            const source_local = try self.lowerVar(field);
            const slot_layout = self.lowerStructFieldLayout(self.localLayout(target_local), @intCast(i));
            sources[i] = source_local;
            lowered[i] = if (self.localLayout(source_local) == slot_layout)
                source_local
            else
                try self.freshLocalWithLayout(slot_layout);
        }
        return .{ .sources = sources, .fields = lowered };
    }

    fn planListElemsIntoTarget(
        self: *ProcLowerer,
        target_local: LIR.LocalId,
        elems: []const ir.Ast.Var,
    ) std.mem.Allocator.Error!struct { sources: []LIR.LocalId, elems: []LIR.LocalId } {
        const sources = try self.parent.allocator.alloc(LIR.LocalId, elems.len);
        errdefer self.parent.allocator.free(sources);
        const lowered = try self.parent.allocator.alloc(LIR.LocalId, elems.len);
        errdefer self.parent.allocator.free(lowered);

        const elem_layout = self.lowerListElemLayout(self.localLayout(target_local));
        for (elems, 0..) |elem, i| {
            const source_local = try self.lowerVar(elem);
            sources[i] = source_local;
            lowered[i] = if (self.localLayout(source_local) == elem_layout)
                source_local
            else
                try self.freshLocalWithLayout(elem_layout);
        }
        return .{ .sources = sources, .elems = lowered };
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
        if (self.localLayout(source_local) == ret_layout) {
            return try self.parent.store.addCFStmt(.{ .ret = .{ .value = source_local } });
        }

        const target_local = try self.freshLocalWithLayout(ret_layout);
        const ret_stmt = try self.parent.store.addCFStmt(.{ .ret = .{ .value = target_local } });
        return try self.bridgeValueIntoLocal(source_local, target_local, ret_stmt);
    }

    fn lowerTerm(self: *ProcLowerer, term: ir.Ast.Term, exit: Lowerer.BlockExit) std.mem.Allocator.Error!LIR.CFStmtId {
        return switch (term) {
            .value => |value| switch (exit) {
                .ret => try self.lowerRetValue(value),
                .jump => |join_id| blk: {
                    const arg = try self.lowerVar(value);
                    break :blk try self.parent.store.addCFStmt(.{ .jump = .{
                        .target = join_id,
                        .args = try self.parent.store.addLocalSpan(&.{arg}),
                    } });
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
                const iterable_elem_layout = self.lowerListElemLayout(self.localLayout(iterable_local));
                const loop_elem_local = if (self.localLayout(body_elem_local) == iterable_elem_layout)
                    body_elem_local
                else
                    try self.freshLocalWithLayout(iterable_elem_layout);
                const body = try self.lowerBlock(for_stmt.body, .loop_continue);
                const bridged_body = if (loop_elem_local == body_elem_local)
                    body
                else
                    try self.bridgeValueIntoLocal(loop_elem_local, body_elem_local, body);
                break :blk try self.parent.store.addCFStmt(.{ .for_list = .{
                    .elem = loop_elem_local,
                    .iterable = iterable_local,
                    .body = bridged_body,
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
            .var_ => |value| try self.parent.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .result = .{ .alias_of = .{ .owner = try self.lowerVar(value) } },
                .op = .{ .local = try self.lowerVar(value) },
                .next = next,
            } }),
            .lit => |lit| try self.parent.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .result = .fresh,
                .value = try self.parent.lowerLiteral(lit),
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
                var payload_source: ?LIR.LocalId = null;
                var payload_local: ?LIR.LocalId = null;
                if (union_expr.payload) |payload| {
                    const source_local = try self.lowerVar(payload);
                    const payload_layout = self.lowerUnionPayloadLayout(
                        self.localLayout(target),
                        union_expr.discriminant,
                    );
                    payload_source = source_local;
                    payload_local = if (self.localLayout(source_local) == payload_layout)
                        source_local
                    else
                        try self.freshLocalWithLayout(payload_layout);
                }

                const assign_tag = try self.parent.store.addCFStmt(.{ .assign_tag = .{
                    .target = target,
                    .result = .fresh,
                    .discriminant = union_expr.discriminant,
                    .payload = payload_local,
                    .next = next,
                } });

                if (payload_source) |source_local| {
                    if (payload_local.? == source_local) break :blk assign_tag;
                    break :blk try self.bridgeValueIntoLocal(source_local, payload_local.?, assign_tag);
                }
                break :blk assign_tag;
            },
            .get_union_id => |value| try self.parent.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .result = .fresh,
                .op = .{ .discriminant = .{ .source = try self.lowerVar(value) } },
                .next = next,
            } }),
            .get_union_struct => |payload| blk: {
                const source = try self.lowerVar(payload.value);
                const actual_payload_layout = self.lowerUnionPayloadLayout(
                    self.localLayout(source),
                    payload.tag_discriminant,
                );
                if (actual_payload_layout == self.localLayout(target)) {
                    break :blk try self.parent.store.addCFStmt(.{ .assign_ref = .{
                        .target = target,
                        .result = .fresh,
                        .op = .{ .tag_payload_struct = .{
                            .source = source,
                            .tag_discriminant = payload.tag_discriminant,
                        } },
                        .next = next,
                    } });
                }

                const raw_target = try self.freshLocalWithLayout(actual_payload_layout);
                const bridged = try self.bridgeValueIntoLocal(raw_target, target, next);
                const access = try self.parent.store.addCFStmt(.{ .assign_ref = .{
                    .target = raw_target,
                    .result = .fresh,
                    .op = .{ .tag_payload_struct = .{
                        .source = source,
                        .tag_discriminant = payload.tag_discriminant,
                    } },
                    .next = bridged,
                } });
                break :blk access;
            },
            .make_struct => |fields| blk: {
                const planned = try self.planStructFieldsIntoTarget(target, self.parent.input.store.sliceVarSpan(fields));
                defer self.parent.allocator.free(planned.sources);
                defer self.parent.allocator.free(planned.fields);
                const span = try self.parent.store.addLocalSpan(planned.fields);
                const assign_struct = try self.parent.store.addCFStmt(.{ .assign_struct = .{
                    .target = target,
                    .result = .fresh,
                    .fields = span,
                    .next = next,
                } });
                break :blk try self.emitBridgesIntoLocals(planned.sources, planned.fields, assign_struct);
            },
            .make_list => |elems| blk: {
                const planned = try self.planListElemsIntoTarget(target, self.parent.input.store.sliceVarSpan(elems));
                defer self.parent.allocator.free(planned.sources);
                defer self.parent.allocator.free(planned.elems);
                const assign_list = try self.parent.store.addCFStmt(.{ .assign_list = .{
                    .target = target,
                    .result = .fresh,
                    .elems = try self.parent.store.addLocalSpan(planned.elems),
                    .next = next,
                } });
                break :blk try self.emitBridgesIntoLocals(planned.sources, planned.elems, assign_list);
            },
            .get_struct_field => |field| blk: {
                const source = try self.lowerVar(field.record);
                const actual_field_layout = self.lowerStructFieldLayout(self.localLayout(source), field.field_index);
                if (actual_field_layout == self.localLayout(target)) {
                    break :blk try self.parent.store.addCFStmt(.{ .assign_ref = .{
                        .target = target,
                        .result = .fresh,
                        .op = .{ .field = .{
                            .source = source,
                            .field_idx = field.field_index,
                        } },
                        .next = next,
                    } });
                }

                const raw_target = try self.freshLocalWithLayout(actual_field_layout);
                const bridged = try self.bridgeValueIntoLocal(raw_target, target, next);
                const access = try self.parent.store.addCFStmt(.{ .assign_ref = .{
                    .target = raw_target,
                    .result = .fresh,
                    .op = .{ .field = .{
                        .source = source,
                        .field_idx = field.field_index,
                    } },
                    .next = bridged,
                } });
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

                const bridged_locals = try self.parent.allocator.alloc(LIR.LocalId, source_locals.len);
                defer self.parent.allocator.free(bridged_locals);
                for (source_locals, param_locals, 0..) |source_local, param_local, i| {
                    const expected_layout = self.localLayout(param_local);
                    bridged_locals[i] = if (self.localLayout(source_local) == expected_layout)
                        source_local
                    else
                        try self.freshLocalWithLayout(expected_layout);
                }

                const raw_target = if (self.localLayout(target) == proc_spec.ret_layout)
                    target
                else
                    try self.freshLocalWithLayout(proc_spec.ret_layout);
                const call_next = if (raw_target == target)
                    next
                else
                    try self.bridgeValueIntoLocal(raw_target, target, next);

                const assign_call = try self.parent.store.addCFStmt(.{ .assign_call = .{
                    .target = raw_target,
                    .result = .fresh,
                    .proc = proc_id,
                    .args = try self.parent.store.addLocalSpan(bridged_locals),
                    .next = call_next,
                } });
                break :blk try self.emitBridgesIntoLocals(source_locals, bridged_locals, assign_call);
            },
            .call_indirect => |call| blk: {
                const locals = try self.lowerVarSpan(self.parent.input.store.sliceVarSpan(call.args));
                defer self.parent.allocator.free(locals);
                break :blk try self.parent.store.addCFStmt(.{ .assign_call_indirect = .{
                    .target = target,
                    .result = .fresh,
                    .closure = try self.lowerVar(call.func),
                    .args = try self.parent.store.addLocalSpan(locals),
                    .next = next,
                } });
            },
            .call_low_level => |call| blk: {
                const locals = try self.lowerVarSpan(self.parent.input.store.sliceVarSpan(call.args));
                defer self.parent.allocator.free(locals);
                break :blk try self.parent.store.addCFStmt(.{ .assign_low_level = .{
                    .target = target,
                    .result = .fresh,
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
