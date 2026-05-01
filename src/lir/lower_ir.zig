//! Source-blind IR to statement-only LIR lowering boundary.
//!
//! This stage consumes executable IR and committed logical layouts. It is not
//! allowed to inspect checked CIR, MIR builders, method names, source syntax, or
//! reference-counting policy. Reference counting is inserted later by `arc.zig`.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const ir = @import("ir");
const mir = @import("mir");
const layout_mod = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;

pub const LowerResourceError = Allocator.Error;

pub const ProcMapEntry = struct {
    executable_proc: ir.Ast.ProcRef,
    lir_proc: LIR.LirProcSpecId,
};

pub const Result = struct {
    canonical_names: mir.Hosted.CanonicalNameStore,
    store: LirStore,
    layouts: layout_mod.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    proc_map: std.ArrayList(ProcMapEntry),

    pub fn deinit(self: *Result) void {
        self.proc_map.deinit(self.store.allocator);
        self.root_procs.deinit(self.store.allocator);
        self.layouts.deinit();
        self.store.deinit();
        self.canonical_names.deinit();
    }

    pub fn lirProcForExecutable(self: *const Result, proc: ir.Ast.ProcRef) ?LIR.LirProcSpecId {
        for (self.proc_map.items) |entry| {
            if (entry.executable_proc == proc) return entry.lir_proc;
        }
        return null;
    }
};

pub fn run(
    allocator: Allocator,
    all_module_envs: []const *const ModuleEnv,
    builtin_str_ident: ?base.Ident.Idx,
    target_usize: base.target.TargetUsize,
    input: ir.Lower.Program,
    explicit_roots: []const ir.Ast.ProcRef,
) LowerResourceError!Result {
    var owned_input = input;
    errdefer owned_input.deinit();

    var lowerer = try Lowerer.init(allocator, all_module_envs, builtin_str_ident, target_usize, &owned_input);
    errdefer lowerer.deinit();
    lowerer.canonical_names = owned_input.canonical_names;
    owned_input.canonical_names = mir.Hosted.CanonicalNameStore.init(allocator);

    try lowerer.registerProcPlaceholders();
    try lowerer.lowerAllDefs();
    try lowerer.bindRoots(explicit_roots);

    owned_input.deinit();
    return lowerer.finish();
}

const Lowerer = struct {
    allocator: Allocator,
    canonical_names: mir.Hosted.CanonicalNameStore,
    input: *const ir.Lower.Program,
    store: LirStore,
    layouts: layout_mod.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    proc_map: std.ArrayList(ProcMapEntry),

    fn init(
        allocator: Allocator,
        all_module_envs: []const *const ModuleEnv,
        builtin_str_ident: ?base.Ident.Idx,
        target_usize: base.target.TargetUsize,
        input: *const ir.Lower.Program,
    ) LowerResourceError!Lowerer {
        return .{
            .allocator = allocator,
            .canonical_names = mir.Hosted.CanonicalNameStore.init(allocator),
            .input = input,
            .store = LirStore.init(allocator),
            .layouts = try layout_mod.Store.init(all_module_envs, builtin_str_ident, allocator, target_usize),
            .root_procs = .empty,
            .proc_map = .empty,
            .local_env = std.AutoHashMap(ir.Ast.Symbol, LIR.LocalId).init(allocator),
        };
    }

    fn deinit(self: *Lowerer) void {
        self.local_env.deinit();
        self.proc_map.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.layouts.deinit();
        self.store.deinit();
        self.canonical_names.deinit();
    }

    fn finish(self: *Lowerer) Result {
        const result = Result{
            .canonical_names = self.canonical_names,
            .store = self.store,
            .layouts = self.layouts,
            .root_procs = self.root_procs,
            .proc_map = self.proc_map,
        };
        self.local_env.deinit();
        self.store = LirStore.init(self.allocator);
        self.layouts = undefined;
        self.canonical_names = mir.Hosted.CanonicalNameStore.init(self.allocator);
        self.root_procs = .empty;
        self.proc_map = .empty;
        self.local_env = std.AutoHashMap(ir.Ast.Symbol, LIR.LocalId).init(self.allocator);
        return result;
    }

    fn registerProcPlaceholders(self: *Lowerer) LowerResourceError!void {
        for (self.input.store.defsSlice()) |def| {
            const args = self.input.store.sliceVarSpan(def.args);
            const arg_locals = try self.allocator.alloc(LIR.LocalId, args.len);
            defer self.allocator.free(arg_locals);

            for (args, 0..) |arg, i| {
                arg_locals[i] = try self.store.addLocal(.{
                    .layout_idx = try self.lowerLayoutRef(arg.layout),
                });
            }

            const proc_id = try self.store.addProcSpec(.{
                .name = self.store.freshSyntheticSymbol(),
                .args = try self.store.addLocalSpan(arg_locals),
                .body = null,
                .ret_layout = try self.lowerLayoutRef(def.ret_layout),
                .hosted = if (def.hosted) |hosted| .{
                    .external_symbol_name = hosted.external_symbol_name,
                    .dispatch_index = hosted.dispatch_index,
                } else null,
            });

            try self.proc_map.append(self.allocator, .{
                .executable_proc = def.proc,
                .lir_proc = proc_id,
            });
        }
    }

    fn lowerAllDefs(self: *Lowerer) LowerResourceError!void {
        for (self.input.store.defsSlice()) |def| {
            if (def.hosted != null) continue;
            if (def.body == null) {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic("lir.lower_ir invariant violated: non-hosted IR def has no body", .{});
                }
                unreachable;
            }
            try self.lowerDef(def);
        }
    }

    fn lowerDef(self: *Lowerer, def: ir.Ast.Def) LowerResourceError!void {
        const lir_proc = self.lirProcForExecutable(def.proc) orelse {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("lir.lower_ir invariant violated: missing proc placeholder", .{});
            }
            unreachable;
        };

        const proc = self.store.getProcSpecPtr(lir_proc);
        self.local_env.clearRetainingCapacity();
        const ir_args = self.input.store.sliceVarSpan(def.args);
        const lir_args = self.store.getLocalSpan(proc.args);
        for (ir_args, lir_args) |ir_arg, lir_arg| {
            try self.local_env.put(ir_arg.symbol, lir_arg);
        }
        proc.body = try self.lowerBlock(def.body.?);
    }

    fn bindRoots(self: *Lowerer, explicit_roots: []const ir.Ast.ProcRef) LowerResourceError!void {
        try self.root_procs.ensureTotalCapacity(self.allocator, explicit_roots.len);
        for (explicit_roots) |root| {
            const proc = self.lirProcForExecutable(root) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic("lir.lower_ir invariant violated: explicit root has no LIR proc", .{});
                }
                unreachable;
            };
            self.root_procs.appendAssumeCapacity(proc);
        }
    }

    fn lowerBlock(self: *Lowerer, block_id: ir.Ast.BlockId) LowerResourceError!LIR.CFStmtId {
        const block = self.input.store.getBlock(block_id);
        var next = switch (block.term) {
            .return_ => |ret| try self.store.addCFStmt(.{ .ret = .{ .value = try self.lowerVar(ret) } }),
            .value => |value| try self.store.addCFStmt(.{ .ret = .{ .value = try self.lowerVar(value) } }),
            .crash => |msg| try self.store.addCFStmt(.{ .crash = .{ .msg = try self.lowerProgramLiteral(msg) } }),
            .runtime_error, .@"unreachable" => try self.store.addCFStmt(.{ .runtime_error = {} }),
        };

        const stmts = self.input.store.sliceStmtSpan(block.stmts);
        var i = stmts.len;
        while (i > 0) {
            i -= 1;
            next = try self.lowerStmt(stmts[i], next);
        }
        return next;
    }

    fn lowerStmt(self: *Lowerer, stmt_id: ir.Ast.StmtId, next: LIR.CFStmtId) LowerResourceError!LIR.CFStmtId {
        const stmt = self.input.store.getStmt(stmt_id);
        return switch (stmt) {
            .let_ => |let_| try self.lowerExprInto(try self.localForVar(let_.bind), self.input.store.getExpr(let_.expr), next),
            .set => |set| try self.store.addCFStmt(.{ .set_local = .{
                .target = try self.lowerVar(set.target),
                .value = try self.lowerVar(set.value),
                .next = next,
            } }),
            .debug => |value| try self.store.addCFStmt(.{ .debug = .{
                .message = try self.lowerVar(value),
                .next = next,
            } }),
            .expect => |value| try self.store.addCFStmt(.{ .expect = .{
                .condition = try self.lowerVar(value),
                .next = next,
            } }),
            .switch_ => |switch_| try self.lowerSwitch(switch_, next),
            .break_,
            .for_list,
            .while_,
            => lirInvariant("lir.lower_ir reached IR statement form whose LIR lowering is still missing"),
        };
    }

    fn lowerSwitch(self: *Lowerer, switch_: anytype, next: LIR.CFStmtId) LowerResourceError!LIR.CFStmtId {
        const input_branches = self.input.store.sliceBranchSpan(switch_.branches);
        const branches = try self.allocator.alloc(LIR.CFSwitchBranch, input_branches.len);
        defer self.allocator.free(branches);

        const join_local = if (switch_.join) |join| try self.localForVar(join) else null;
        for (input_branches, 0..) |branch_id, i| {
            const branch = self.input.store.getBranch(branch_id);
            branches[i] = .{
                .value = branch.value,
                .body = try self.lowerBlockWithContinuation(branch.block, join_local, next),
            };
        }

        return try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = try self.lowerVar(switch_.cond),
            .branches = try self.store.addCFSwitchBranches(branches),
            .default_branch = try self.lowerBlockWithContinuation(switch_.default_block, join_local, next),
        } });
    }

    fn lowerBlockWithContinuation(
        self: *Lowerer,
        block_id: ir.Ast.BlockId,
        join_local: ?LIR.LocalId,
        continuation: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const block = self.input.store.getBlock(block_id);
        var next = switch (block.term) {
            .return_ => |ret| try self.store.addCFStmt(.{ .ret = .{ .value = try self.lowerVar(ret) } }),
            .value => |value| blk: {
                const join = join_local orelse break :blk continuation;
                break :blk try self.store.addCFStmt(.{ .set_local = .{
                    .target = join,
                    .value = try self.lowerVar(value),
                    .next = continuation,
                } });
            },
            .crash => |msg| try self.store.addCFStmt(.{ .crash = .{ .msg = try self.lowerProgramLiteral(msg) } }),
            .runtime_error, .@"unreachable" => try self.store.addCFStmt(.{ .runtime_error = {} }),
        };

        const stmts = self.input.store.sliceStmtSpan(block.stmts);
        var i = stmts.len;
        while (i > 0) {
            i -= 1;
            next = try self.lowerStmt(stmts[i], next);
        }
        return next;
    }

    fn lowerExprInto(self: *Lowerer, target: LIR.LocalId, expr: ir.Ast.Expr, next: LIR.CFStmtId) LowerResourceError!LIR.CFStmtId {
        return switch (expr) {
            .var_ => |var_| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .local = try self.lowerVar(var_) },
                .next = next,
            } }),
            .lit => |lit| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = try self.lowerLiteral(lit, self.store.getLocal(target).layout_idx),
                .next = next,
            } }),
            .fn_ptr => |proc| try self.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .{ .proc_ref = self.lirProcForExecutable(proc) orelse lirInvariant("lir.lower_ir reached fn_ptr before proc placeholder") },
                .next = next,
            } }),
            .null_ptr => try self.store.addCFStmt(.{ .assign_literal = .{
                .target = target,
                .value = .null_ptr,
                .next = next,
            } }),
            .make_struct => |fields| try self.store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = try self.lowerVarSpan(fields),
                .next = next,
            } }),
            .make_list => |list| try self.store.addCFStmt(.{ .assign_list = .{
                .target = target,
                .elems = try self.lowerVarSpan(list.elems),
                .next = next,
            } }),
            .make_union => |tag| try self.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .discriminant = tag.discriminant,
                .payload = if (tag.payload) |payload| try self.lowerVar(payload) else null,
                .next = next,
            } }),
            .get_union_id => |source| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .discriminant = .{
                    .source = try self.lowerVar(source),
                } },
                .next = next,
            } }),
            .get_union_struct => |payload| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .tag_payload_struct = .{
                    .source = try self.lowerVar(payload.value),
                    .tag_discriminant = payload.tag_discriminant,
                } },
                .next = next,
            } }),
            .get_struct_field => |field| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .field = .{
                    .source = try self.lowerVar(field.record),
                    .field_idx = field.field_index,
                } },
                .next = next,
            } }),
            .nominal_reinterpret => |backing| try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .nominal = .{
                    .backing_ref = try self.lowerVar(backing),
                } },
                .next = next,
            } }),
            .call_direct => |call| try self.store.addCFStmt(.{ .assign_call = .{
                .target = target,
                .proc = self.lirProcForExecutable(call.proc) orelse lirInvariant("lir.lower_ir reached call_direct before proc placeholder"),
                .args = try self.lowerVarSpan(call.args),
                .next = next,
            } }),
            .call_low_level => |call| try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = call.op,
                .args = try self.lowerVarSpan(call.args),
                .next = next,
            } }),
            .bridge,
            .layout_size,
            .call_erased,
            => lirInvariant("lir.lower_ir reached IR expression form whose LIR lowering is still missing"),
        };
    }

    fn lowerLiteral(self: *Lowerer, lit: ir.Ast.Lit, layout_idx: layout_mod.Idx) LowerResourceError!LIR.LiteralValue {
        return switch (lit) {
            .int => |value| .{ .i128_literal = .{
                .value = value,
                .layout_idx = layout_idx,
            } },
            .f32 => |value| .{ .f32_literal = value },
            .f64 => |value| .{ .f64_literal = value },
            .dec => |value| .{ .dec_literal = value },
            .str => |literal| .{ .str_literal = try self.lowerProgramLiteral(literal) },
            .bool => |value| .{ .bool_literal = value },
        };
    }

    fn lowerVarSpan(self: *Lowerer, span: ir.Ast.Span(ir.Ast.Var)) LowerResourceError!LIR.LocalSpan {
        const vars = self.input.store.sliceVarSpan(span);
        if (vars.len == 0) return LIR.LocalSpan.empty();
        const locals = try self.allocator.alloc(LIR.LocalId, vars.len);
        defer self.allocator.free(locals);
        for (vars, 0..) |var_, i| {
            locals[i] = try self.lowerVar(var_);
        }
        return try self.store.addLocalSpan(locals);
    }

    fn lowerProgramLiteral(self: *Lowerer, literal: ir.Ast.ProgramLiteralId) LowerResourceError!base.StringLiteral.Idx {
        return try self.store.insertString(self.input.literal_pool.get(literal));
    }

    fn lowerVar(self: *Lowerer, var_: ir.Ast.Var) LowerResourceError!LIR.LocalId {
        if (var_.symbol.isNone()) {
            return try self.store.addLocal(.{
                .layout_idx = try self.lowerLayoutRef(var_.layout),
            });
        }
        if (self.local_env.get(var_.symbol)) |local| return local;
        return try self.localForVar(var_);
    }

    fn localForVar(self: *Lowerer, var_: ir.Ast.Var) LowerResourceError!LIR.LocalId {
        if (var_.symbol.isNone()) {
            return try self.store.addLocal(.{
                .layout_idx = try self.lowerLayoutRef(var_.layout),
            });
        }
        if (self.local_env.get(var_.symbol)) |local| return local;
        const local = try self.store.addLocal(.{
            .layout_idx = try self.lowerLayoutRef(var_.layout),
        });
        try self.local_env.put(var_.symbol, local);
        return local;
    }

    fn lowerLayoutRef(self: *Lowerer, ref: ir.Layout.Ref) LowerResourceError!layout_mod.Idx {
        return switch (ref) {
            .canonical => |idx| idx,
            .local => blk: {
                var commit = try self.layouts.commitGraph(&self.input.layouts, ref);
                defer commit.deinit(self.allocator);
                break :blk commit.root_idx;
            },
        };
    }

    fn lirProcForExecutable(self: *const Lowerer, proc: ir.Ast.ProcRef) ?LIR.LirProcSpecId {
        for (self.proc_map.items) |entry| {
            if (entry.executable_proc == proc) return entry.lir_proc;
        }
        return null;
    }
};

fn lirInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "IR to LIR lowering exposes only resource errors" {
    std.testing.refAllDecls(@This());
}
