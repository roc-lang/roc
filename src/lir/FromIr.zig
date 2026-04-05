//! Lower cor-style executable IR into statement-only LIR.

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
    active_layouts: std.AutoHashMap(u32, layout_mod.Idx),
    next_join_id: u32,

    const BlockExit = union(enum) {
        ret,
        jump: LIR.JoinPointId,
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
            .active_layouts = std.AutoHashMap(u32, layout_mod.Idx).init(allocator),
            .next_join_id = 0,
        };
    }

    fn deinit(self: *Lowerer) void {
        self.active_layouts.deinit();
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
            const proc_id = try self.store.addProcSpec(.{
                .name = lirSymbol(def.name),
                .args = LIR.LocalSpan.empty(),
                .body = try self.store.addCFStmt(.runtime_error),
                .ret_layout = .zst,
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
        try proc.indexStructFieldDefs(def.body);

        const args = self.input.store.sliceVarSpan(def.args);
        const arg_locals = try proc.lowerVarSpan(args);
        defer self.allocator.free(arg_locals);
        const body = try proc.lowerBlock(def.body, .ret);
        const ret_layout = switch (self.input.store.getBlock(def.body).term) {
            .value => |value| try self.lowerLayoutId(value.layout),
            .return_ => |value| try self.lowerLayoutId(value.layout),
            .crash, .runtime_error => .zst,
        };

        const proc_ptr = self.store.getProcSpecPtr(proc_id);
        proc_ptr.* = .{
            .name = lirSymbol(def.name),
            .args = try self.store.addLocalSpan(arg_locals),
            .body = body,
            .ret_layout = ret_layout,
            .result_contract = .fresh,
        };
    }

    fn lowerLayoutId(self: *Lowerer, id: ir.Layout.LayoutId) std.mem.Allocator.Error!layout_mod.Idx {
        if (self.ir_to_layout.get(@intFromEnum(id))) |existing| return existing;
        if (self.active_layouts.get(@intFromEnum(id))) |active| return active;

        const content = self.input.layouts.getContent(id);
        if (content == .primitive) {
            const lowered = try self.lowerLayoutContent(content);
            try self.ir_to_layout.put(@intFromEnum(id), lowered);
            return lowered;
        }

        const placeholder = try self.layouts.reserveLayout(layout_mod.Layout.box(.zst));
        try self.active_layouts.put(@intFromEnum(id), placeholder);

        const lowered = try self.lowerLayoutContent(content);
        const lowered_layout = self.layouts.getLayout(lowered);
        self.layouts.updateLayout(placeholder, lowered_layout);

        _ = self.active_layouts.remove(@intFromEnum(id));
        try self.ir_to_layout.put(@intFromEnum(id), placeholder);
        return placeholder;
    }

    fn lowerLayoutContent(self: *Lowerer, content: ir.Layout.Content) std.mem.Allocator.Error!layout_mod.Idx {
        return switch (content) {
            .primitive => |prim| switch (prim) {
                .bool => .bool,
                .str => .str,
                .u8 => .u8,
                .i8 => .i8,
                .u16 => .u16,
                .i16 => .i16,
                .u32 => .u32,
                .i32 => .i32,
                .u64 => .u64,
                .i64 => .i64,
                .u128 => .u128,
                .i128 => .i128,
                .f32 => .f32,
                .f64 => .f64,
                .dec => .dec,
                .opaque_ptr => .opaque_ptr,
            },
            .list => |elem| try self.layouts.insertList(try self.lowerLayoutId(elem)),
            .box => |elem| try self.layouts.insertBox(try self.lowerLayoutId(elem)),
            .struct_ => |fields| blk: {
                const ir_fields = self.input.layouts.sliceLayoutSpan(fields);
                var lowered_fields = try self.allocator.alloc(layout_mod.StructField, ir_fields.len);
                defer self.allocator.free(lowered_fields);
                for (ir_fields, 0..) |field, i| {
                    lowered_fields[i] = .{
                        .index = @intCast(i),
                        .layout = try self.lowerLayoutId(field),
                    };
                }
                break :blk try self.layouts.putStructFields(lowered_fields);
            },
            .union_ => |variants| blk: {
                const ir_variants = self.input.layouts.sliceLayoutSpan(variants);
                var lowered_variants = try self.allocator.alloc(layout_mod.Idx, ir_variants.len);
                defer self.allocator.free(lowered_variants);
                for (ir_variants, 0..) |variant, i| {
                    lowered_variants[i] = try self.lowerLayoutId(variant);
                }
                break :blk try self.layouts.putTagUnion(lowered_variants);
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
    locals_by_symbol: std.AutoHashMap(u32, LIR.LocalId),
    struct_fields_by_symbol: std.AutoHashMap(u32, ir.Ast.Span(ir.Ast.Var)),

    fn init(parent: *Lowerer, proc_id: LIR.LirProcSpecId) ProcLowerer {
        return .{
            .parent = parent,
            .proc_id = proc_id,
            .locals_by_symbol = std.AutoHashMap(u32, LIR.LocalId).init(parent.allocator),
            .struct_fields_by_symbol = std.AutoHashMap(u32, ir.Ast.Span(ir.Ast.Var)).init(parent.allocator),
        };
    }

    fn deinit(self: *ProcLowerer) void {
        self.struct_fields_by_symbol.deinit();
        self.locals_by_symbol.deinit();
    }

    fn indexStructFieldDefs(self: *ProcLowerer, block_id: ir.Ast.BlockId) std.mem.Allocator.Error!void {
        const block = self.parent.input.store.getBlock(block_id);
        for (self.parent.input.store.sliceStmtSpan(block.stmts)) |stmt_id| {
            const stmt = self.parent.input.store.getStmt(stmt_id);
            switch (stmt) {
                .let_ => |let_stmt| {
                    switch (self.parent.input.store.getExpr(let_stmt.expr)) {
                        .make_struct => |fields| try self.struct_fields_by_symbol.put(let_stmt.bind.symbol.raw(), fields),
                        else => {},
                    }
                },
                .switch_ => |switch_stmt| {
                    for (self.parent.input.store.sliceBranchSpan(switch_stmt.branches)) |branch_id| {
                        const branch = self.parent.input.store.getBranch(branch_id);
                        try self.indexStructFieldDefs(branch.block);
                    }
                    try self.indexStructFieldDefs(switch_stmt.default_block);
                },
                .for_list => |for_stmt| try self.indexStructFieldDefs(for_stmt.body),
                .set, .expect => {},
            }
        }
    }

    fn lowerVar(self: *ProcLowerer, value: ir.Ast.Var) std.mem.Allocator.Error!LIR.LocalId {
        if (self.locals_by_symbol.get(value.symbol.raw())) |existing| return existing;
        const local_id = try self.parent.store.addLocal(.{
            .layout_idx = try self.parent.lowerLayoutId(value.layout),
        });
        try self.locals_by_symbol.put(value.symbol.raw(), local_id);
        return local_id;
    }

    fn lowerVarSpan(self: *ProcLowerer, vars: []const ir.Ast.Var) std.mem.Allocator.Error![]LIR.LocalId {
        const out = try self.parent.allocator.alloc(LIR.LocalId, vars.len);
        for (vars, 0..) |var_, i| {
            out[i] = try self.lowerVar(var_);
        }
        return out;
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

    fn lowerTerm(self: *ProcLowerer, term: ir.Ast.Term, exit: Lowerer.BlockExit) std.mem.Allocator.Error!LIR.CFStmtId {
        return switch (term) {
            .value => |value| switch (exit) {
                .ret => try self.parent.store.addCFStmt(.{ .ret = .{ .value = try self.lowerVar(value) } }),
                .jump => |join_id| blk: {
                    const arg = try self.lowerVar(value);
                    break :blk try self.parent.store.addCFStmt(.{ .jump = .{
                        .target = join_id,
                        .args = try self.parent.store.addLocalSpan(&.{arg}),
                    } });
                },
                .loop_continue => try self.parent.store.addCFStmt(.loop_continue),
            },
            .return_ => |value| try self.parent.store.addCFStmt(.{ .ret = .{ .value = try self.lowerVar(value) } }),
            .crash => |msg| try self.parent.store.addCFStmt(.{ .crash = .{ .msg = try self.parent.lowerStringId(msg) } }),
            .runtime_error => try self.parent.store.addCFStmt(.runtime_error),
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
            .for_list => |for_stmt| try self.parent.store.addCFStmt(.{ .for_list = .{
                .elem = try self.lowerVar(for_stmt.elem),
                .iterable = try self.lowerVar(for_stmt.iterable),
                .body = try self.lowerBlock(for_stmt.body, .loop_continue),
                .next = next,
            } }),
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
            .make_union => |union_expr| try self.parent.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .result = .fresh,
                .discriminant = union_expr.discriminant,
                .args = try self.lowerUnionArgs(union_expr.payload),
                .next = next,
            } }),
            .get_union_id => |value| try self.parent.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .result = .fresh,
                .op = .{ .discriminant = .{ .source = try self.lowerVar(value) } },
                .next = next,
            } }),
            .get_union_struct => |payload| try self.parent.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .result = .fresh,
                .op = .{ .tag_payload_struct = .{
                    .source = try self.lowerVar(payload.value),
                    .tag_discriminant = payload.tag_discriminant,
                } },
                .next = next,
            } }),
            .make_struct => |fields| blk: {
                const locals = try self.lowerVarSpan(self.parent.input.store.sliceVarSpan(fields));
                defer self.parent.allocator.free(locals);
                const span = try self.parent.store.addLocalSpan(locals);
                break :blk try self.parent.store.addCFStmt(.{ .assign_struct = .{
                    .target = target,
                    .result = .fresh,
                    .fields = span,
                    .next = next,
                } });
            },
            .make_list => |elems| blk: {
                const locals = try self.lowerVarSpan(self.parent.input.store.sliceVarSpan(elems));
                defer self.parent.allocator.free(locals);
                break :blk try self.parent.store.addCFStmt(.{ .assign_list = .{
                    .target = target,
                    .result = .fresh,
                    .elems = try self.parent.store.addLocalSpan(locals),
                    .next = next,
                } });
            },
            .get_struct_field => |field| try self.parent.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .result = .fresh,
                .op = .{ .field = .{
                    .source = try self.lowerVar(field.record),
                    .field_idx = field.field_index,
                } },
                .next = next,
            } }),
            .call_direct => |call| blk: {
                const locals = try self.lowerVarSpan(self.parent.input.store.sliceVarSpan(call.args));
                defer self.parent.allocator.free(locals);
                break :blk try self.parent.store.addCFStmt(.{ .assign_call = .{
                    .target = target,
                    .result = .fresh,
                    .proc = self.lookupProcId(call.proc),
                    .args = try self.parent.store.addLocalSpan(locals),
                    .next = next,
                } });
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

    fn lowerUnionArgs(self: *ProcLowerer, payload: ?ir.Ast.Var) std.mem.Allocator.Error!LIR.LocalSpan {
        if (payload == null) return LIR.LocalSpan.empty();
        const fields = self.struct_fields_by_symbol.get(payload.?.symbol.raw()) orelse
            debugPanic("lir.from_ir.lowerUnionArgs missing payload struct fields");
        const locals = try self.lowerVarSpan(self.parent.input.store.sliceVarSpan(fields));
        defer self.parent.allocator.free(locals);
        return try self.parent.store.addLocalSpan(locals);
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
