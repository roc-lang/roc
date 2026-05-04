//! Source-blind IR to statement-only LIR lowering boundary.
//!
//! This stage consumes executable IR and committed logical layouts. It is not
//! allowed to inspect checked CIR, MIR builders, method names, source syntax, or
//! reference-counting policy. Reference counting is inserted later by `arc.zig`.

const std = @import("std");
const base = @import("base");
const ir = @import("ir");
const mir = @import("mir");
const layout_mod = @import("layout");

const LIR = @import("LIR.zig");
const LirStore = @import("LirStore.zig");

const Allocator = std.mem.Allocator;

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
    root_metadata: std.ArrayList(mir.Ids.RootMetadata),
    proc_map: std.ArrayList(ProcMapEntry),

    pub fn deinit(self: *Result) void {
        self.proc_map.deinit(self.store.allocator);
        self.root_metadata.deinit(self.store.allocator);
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
    target_usize: base.target.TargetUsize,
    input: ir.Lower.Program,
    explicit_roots: []const ir.Ast.ProcRef,
    explicit_root_metadata: []const mir.Ids.RootMetadata,
) LowerResourceError!Result {
    var owned_input = input;
    errdefer owned_input.deinit();

    var lowerer = try Lowerer.init(allocator, target_usize, &owned_input);
    errdefer lowerer.deinit();
    lowerer.canonical_names = owned_input.canonical_names;
    owned_input.canonical_names = mir.Hosted.CanonicalNameStore.init(allocator);

    try lowerer.registerProcPlaceholders();
    try lowerer.lowerAllDefs();
    try lowerer.bindRoots(explicit_roots, explicit_root_metadata);

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
    root_metadata: std.ArrayList(mir.Ids.RootMetadata),
    proc_map: std.ArrayList(ProcMapEntry),
    local_env: std.AutoHashMap(ir.Ast.Symbol, LIR.LocalId),
    break_targets: std.ArrayList(LIR.CFStmtId),
    next_join_point: u32,

    fn init(
        allocator: Allocator,
        target_usize: base.target.TargetUsize,
        input: *const ir.Lower.Program,
    ) LowerResourceError!Lowerer {
        return .{
            .allocator = allocator,
            .canonical_names = mir.Hosted.CanonicalNameStore.init(allocator),
            .input = input,
            .store = LirStore.init(allocator),
            .layouts = try layout_mod.Store.init(allocator, target_usize),
            .root_procs = .empty,
            .root_metadata = .empty,
            .proc_map = .empty,
            .break_targets = .empty,
            .next_join_point = 0,
            .local_env = std.AutoHashMap(ir.Ast.Symbol, LIR.LocalId).init(allocator),
        };
    }

    fn deinit(self: *Lowerer) void {
        self.break_targets.deinit(self.allocator);
        self.local_env.deinit();
        self.proc_map.deinit(self.allocator);
        self.root_metadata.deinit(self.allocator);
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
            .root_metadata = self.root_metadata,
            .proc_map = self.proc_map,
        };
        self.local_env.deinit();
        self.break_targets.deinit(self.allocator);
        self.store = LirStore.init(self.allocator);
        self.layouts = undefined;
        self.canonical_names = mir.Hosted.CanonicalNameStore.init(self.allocator);
        self.root_procs = .empty;
        self.root_metadata = .empty;
        self.proc_map = .empty;
        self.break_targets = .empty;
        self.next_join_point = 0;
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

    fn bindRoots(
        self: *Lowerer,
        explicit_roots: []const ir.Ast.ProcRef,
        explicit_root_metadata: []const mir.Ids.RootMetadata,
    ) LowerResourceError!void {
        if (explicit_roots.len != explicit_root_metadata.len) {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic(
                    "lir.lower_ir invariant violated: explicit root metadata mismatch roots={d} metadata={d}",
                    .{ explicit_roots.len, explicit_root_metadata.len },
                );
            }
            unreachable;
        }
        try self.root_procs.ensureTotalCapacity(self.allocator, explicit_roots.len);
        try self.root_metadata.ensureTotalCapacity(self.allocator, explicit_root_metadata.len);
        for (explicit_roots, explicit_root_metadata) |root, metadata| {
            const proc = self.lirProcForExecutable(root) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic("lir.lower_ir invariant violated: explicit root has no LIR proc", .{});
                }
                unreachable;
            };
            self.root_procs.appendAssumeCapacity(proc);
            self.root_metadata.appendAssumeCapacity(metadata);
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
                .mode = .overwrite_owned,
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
            .return_ => |value| try self.store.addCFStmt(.{ .ret = .{ .value = try self.lowerVar(value) } }),
            .crash => |msg| try self.store.addCFStmt(.{ .crash = .{ .msg = try self.lowerProgramLiteral(msg) } }),
            .runtime_error => try self.store.addCFStmt(.{ .runtime_error = {} }),
            .switch_ => |switch_| try self.lowerSwitch(switch_, next),
            .break_ => self.currentBreakTarget(),
            .for_list => |for_list| try self.lowerForList(for_list, next),
            .while_ => |while_| try self.lowerWhile(while_, next),
        };
    }

    fn lowerForList(self: *Lowerer, for_list: anytype, next: LIR.CFStmtId) LowerResourceError!LIR.CFStmtId {
        const loop_continue = try self.store.addCFStmt(.loop_continue);
        const break_start = try self.pushBreakTarget(try self.store.addCFStmt(.loop_break));
        defer self.restoreBreakTargets(break_start);
        return try self.store.addCFStmt(.{ .for_list = .{
            .elem = try self.localForVar(for_list.elem),
            .elem_mode = .borrowed_from_iterable,
            .iterable = try self.lowerVar(for_list.iterable),
            .iterable_elem_layout = try self.lowerLayoutRef(for_list.elem.layout),
            .body = try self.lowerBlockWithContinuation(for_list.body, null, loop_continue),
            .next = next,
        } });
    }

    fn lowerWhile(self: *Lowerer, while_: anytype, next: LIR.CFStmtId) LowerResourceError!LIR.CFStmtId {
        const join_id = self.freshJoinPointId();
        const initial_jump = try self.store.addCFStmt(.{ .jump = .{
            .target = join_id,
            .args = LIR.LocalSpan.empty(),
        } });
        const loop_jump = try self.store.addCFStmt(.{ .jump = .{
            .target = join_id,
            .args = LIR.LocalSpan.empty(),
        } });

        const break_start = try self.pushBreakTarget(next);
        defer self.restoreBreakTargets(break_start);
        const body = try self.lowerBlockWithContinuation(while_.body, null, loop_jump);

        const cond_local = try self.store.addLocal(.{
            .layout_idx = try self.lowerLayoutRef(self.blockReturnLayout(while_.cond)),
        });
        const branches = [_]LIR.CFSwitchBranch{.{
            .value = 1,
            .body = body,
        }};
        const cond_switch = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = cond_local,
            .branches = try self.store.addCFSwitchBranches(&branches),
            .default_branch = next,
            .continuation = next,
        } });
        const cond_body = try self.lowerBlockWithContinuation(while_.cond, cond_local, cond_switch);

        return try self.store.addCFStmt(.{ .join = .{
            .id = join_id,
            .params = LIR.LocalSpan.empty(),
            .body = cond_body,
            .remainder = initial_jump,
        } });
    }

    fn pushBreakTarget(self: *Lowerer, target: LIR.CFStmtId) LowerResourceError!usize {
        const start = self.break_targets.items.len;
        try self.break_targets.append(self.allocator, target);
        return start;
    }

    fn restoreBreakTargets(self: *Lowerer, start: usize) void {
        self.break_targets.shrinkRetainingCapacity(start);
    }

    fn currentBreakTarget(self: *Lowerer) LIR.CFStmtId {
        if (self.break_targets.items.len == 0) {
            lirInvariant("lir.lower_ir reached break outside a lowered loop");
        }
        return self.break_targets.items[self.break_targets.items.len - 1];
    }

    fn freshJoinPointId(self: *Lowerer) LIR.JoinPointId {
        const id: LIR.JoinPointId = @enumFromInt(self.next_join_point);
        self.next_join_point += 1;
        return id;
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
            .continuation = next,
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
                    .mode = .initialize_join_result,
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

    fn blockReturnLayout(self: *const Lowerer, block_id: ir.Ast.BlockId) ir.Ast.LayoutRef {
        const block = self.input.store.getBlock(block_id);
        return switch (block.term) {
            .value => |value| value.layout,
            .return_ => |value| value.layout,
            .crash, .runtime_error, .@"unreachable" => .{ .canonical = .zst },
        };
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
            .structural_eq => |eq| blk: {
                const args = [_]LIR.LocalId{
                    try self.lowerVar(eq.lhs),
                    try self.lowerVar(eq.rhs),
                };
                break :blk try self.store.addCFStmt(.{ .assign_low_level = .{
                    .target = target,
                    .op = .num_is_eq,
                    .rc_effect = LIR.LowLevel.num_is_eq.rcEffect(),
                    .args = try self.store.addLocalSpan(&args),
                    .next = next,
                } });
            },
            .call_low_level => |call| try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = call.op,
                .rc_effect = call.rc_effect,
                .args = try self.lowerVarSpan(call.args),
                .next = next,
            } }),
            .call_erased => |call| try self.store.addCFStmt(.{ .assign_call_erased = .{
                .target = target,
                .closure = try self.lowerVar(call.func),
                .args = try self.lowerVarSpan(call.args),
                .capture_layout = if (call.capture_layout) |capture_layout| try self.lowerLayoutRef(capture_layout) else null,
                .next = next,
            } }),
            .packed_erased_fn => |packed| try self.lowerPackedErasedFnInto(target, packed, next),
            .layout_size => |layout_ref| blk: {
                const layout_idx = try self.lowerLayoutRef(layout_ref);
                const size = self.layouts.layoutSize(self.layouts.getLayout(layout_idx));
                break :blk try self.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .i128_literal = .{
                        .value = size,
                        .layout_idx = self.store.getLocal(target).layout_idx,
                    } },
                    .next = next,
                } });
            },
            .bridge => |bridge| try self.lowerBridgeExpr(target, bridge, next),
        };
    }

    fn lowerPackedErasedFnInto(
        self: *Lowerer,
        target: LIR.LocalId,
        packed: anytype,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const target_layout = self.store.getLocal(target).layout_idx;
        const has_capture = packed.capture != null;
        if (has_capture != (packed.capture_layout != null)) {
            lirInvariant("lir.lower_ir packed erased fn capture value disagrees with capture layout");
        }
        const field_count: usize = if (has_capture) 2 else 1;
        const fields = try self.allocator.alloc(LIR.LocalId, field_count);
        defer self.allocator.free(fields);
        for (fields, 0..) |*field, i| {
            field.* = try self.store.addLocal(.{
                .layout_idx = self.structFieldLayout(target_layout, i),
            });
        }

        var current = try self.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.store.addLocalSpan(fields),
            .next = next,
        } });

        if (packed.capture) |capture| {
            const args = [_]LIR.LocalId{try self.lowerVar(capture)};
            current = try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = fields[1],
                .op = .box_box,
                .rc_effect = LIR.LowLevel.box_box.rcEffect(),
                .args = try self.store.addLocalSpan(&args),
                .next = current,
            } });
        }

        return try self.store.addCFStmt(.{ .assign_literal = .{
            .target = fields[0],
            .value = .{ .proc_ref = self.lirProcForExecutable(packed.proc) orelse lirInvariant("lir.lower_ir reached packed_erased_fn before proc placeholder") },
            .next = current,
        } });
    }

    fn lowerBridgeExpr(
        self: *Lowerer,
        target: LIR.LocalId,
        bridge: anytype,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const source = try self.lowerVar(bridge.value);
        return try self.lowerBridgePlanInto(target, source, bridge.plan, next);
    }

    fn lowerBridgePlanInto(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        plan_id: ir.Ast.BridgePlanId,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        return switch (self.input.store.getBridgePlan(plan_id)) {
            .direct => try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .local = source },
                .next = next,
            } }),
            .zst => try self.store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = LIR.LocalSpan.empty(),
                .next = next,
            } }),
            .list_reinterpret => try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .list_reinterpret = .{ .backing_ref = source } },
                .next = next,
            } }),
            .nominal_reinterpret => try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .nominal = .{ .backing_ref = source } },
                .next = next,
            } }),
            .box_box => |child| try self.lowerBoxBoxBridge(target, source, child, next),
            .box_unbox => |child| try self.lowerBoxUnboxBridge(target, source, child, next),
            .struct_ => |children| try self.lowerStructBridge(target, source, children, next),
            .tag_union => |children| try self.lowerTagUnionBridge(target, source, children, next),
            .singleton_to_tag_union => |singleton| try self.lowerSingletonToTagUnionBridge(target, source, singleton, next),
            .tag_union_to_singleton => |singleton| try self.lowerTagUnionToSingletonBridge(target, source, singleton, next),
        };
    }

    fn lowerBoxBoxBridge(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        child: ir.Ast.BridgePlanId,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const payload_layout = self.boxPayloadLayout(self.store.getLocal(target).layout_idx);
        const payload = try self.store.addLocal(.{ .layout_idx = payload_layout });
        const args = [_]LIR.LocalId{payload};
        const box_stmt = try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = target,
            .op = .box_box,
            .rc_effect = LIR.LowLevel.box_box.rcEffect(),
            .args = try self.store.addLocalSpan(&args),
            .next = next,
        } });
        return try self.lowerBridgePlanInto(payload, source, child, box_stmt);
    }

    fn lowerBoxUnboxBridge(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        child: ir.Ast.BridgePlanId,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const payload_layout = self.boxPayloadLayout(self.store.getLocal(source).layout_idx);
        const payload = try self.store.addLocal(.{ .layout_idx = payload_layout });
        const child_start = try self.lowerBridgePlanInto(target, payload, child, next);
        const args = [_]LIR.LocalId{source};
        return try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = payload,
            .op = .box_unbox,
            .rc_effect = LIR.LowLevel.box_unbox.rcEffect(),
            .args = try self.store.addLocalSpan(&args),
            .next = child_start,
        } });
    }

    fn lowerStructBridge(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        children: ir.Ast.Span(ir.Ast.BridgePlanId),
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const child_plans = self.input.store.sliceBridgePlanSpan(children);
        const source_layout = self.store.getLocal(source).layout_idx;
        const target_layout = self.store.getLocal(target).layout_idx;
        if (child_plans.len != self.structFieldCount(source_layout) or child_plans.len != self.structFieldCount(target_layout)) {
            lirInvariant("lir.lower_ir struct bridge field count does not match source and target layouts");
        }

        const field_values = try self.allocator.alloc(LIR.LocalId, child_plans.len);
        defer self.allocator.free(field_values);
        for (child_plans, 0..) |_, i| {
            field_values[i] = try self.store.addLocal(.{
                .layout_idx = self.structFieldLayout(target_layout, i),
            });
        }

        var current = try self.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.store.addLocalSpan(field_values),
            .next = next,
        } });

        var i = child_plans.len;
        while (i > 0) {
            i -= 1;
            const source_field = try self.store.addLocal(.{
                .layout_idx = self.structFieldLayout(source_layout, i),
            });
            current = try self.lowerBridgePlanInto(field_values[i], source_field, child_plans[i], current);
            current = try self.store.addCFStmt(.{ .assign_ref = .{
                .target = source_field,
                .op = .{ .field = .{
                    .source = source,
                    .field_idx = @intCast(i),
                } },
                .next = current,
            } });
        }
        return current;
    }

    fn lowerTagUnionBridge(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        children: ir.Ast.Span(ir.Ast.BridgePlanId),
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const child_plans = self.input.store.sliceBridgePlanSpan(children);
        const source_layout = self.store.getLocal(source).layout_idx;
        const target_layout = self.store.getLocal(target).layout_idx;
        if (child_plans.len != self.tagUnionVariantCount(source_layout) or child_plans.len != self.tagUnionVariantCount(target_layout)) {
            lirInvariant("lir.lower_ir tag-union bridge variant count does not match source and target layouts");
        }

        const branches = try self.allocator.alloc(LIR.CFSwitchBranch, child_plans.len);
        defer self.allocator.free(branches);
        for (child_plans, 0..) |child_plan, i| {
            branches[i] = .{
                .value = @intCast(i),
                .body = try self.lowerTagUnionBridgeBranch(target, source, source_layout, target_layout, i, child_plan, next),
            };
        }

        const discriminant = try self.store.addLocal(.{ .layout_idx = .u16 });
        const switch_stmt = try self.store.addCFStmt(.{ .switch_stmt = .{
            .cond = discriminant,
            .branches = try self.store.addCFSwitchBranches(branches),
            .default_branch = try self.store.addCFStmt(.{ .runtime_error = {} }),
            .continuation = next,
        } });
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = discriminant,
            .op = .{ .discriminant = .{ .source = source } },
            .next = switch_stmt,
        } });
    }

    fn lowerTagUnionBridgeBranch(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        source_layout: layout_mod.Idx,
        target_layout: layout_mod.Idx,
        variant_index: usize,
        child_plan: ir.Ast.BridgePlanId,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const target_payload_layout = self.tagUnionPayloadLayout(target_layout, variant_index);
        const payload = if (self.isZstLayout(target_payload_layout))
            null
        else
            try self.store.addLocal(.{ .layout_idx = target_payload_layout });
        const assign_tag = try self.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .discriminant = @intCast(variant_index),
            .payload = payload,
            .next = next,
        } });

        const source_payload_layout = self.tagUnionPayloadLayout(source_layout, variant_index);
        const source_payload = try self.store.addLocal(.{ .layout_idx = source_payload_layout });
        const after_extract = if (payload) |payload_local|
            try self.lowerBridgePlanInto(payload_local, source_payload, child_plan, assign_tag)
        else
            assign_tag;
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = source_payload,
            .op = .{ .tag_payload_struct = .{
                .source = source,
                .tag_discriminant = @intCast(variant_index),
            } },
            .next = after_extract,
        } });
    }

    fn lowerSingletonToTagUnionBridge(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        singleton: anytype,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const target_payload_layout = self.tagUnionPayloadLayout(self.store.getLocal(target).layout_idx, @intCast(singleton.target_discriminant));
        if (self.isZstLayout(target_payload_layout)) {
            return try self.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .discriminant = singleton.target_discriminant,
                .payload = null,
                .next = next,
            } });
        }

        if (singleton.payload_plan) |payload_plan| {
            const bridged = try self.store.addLocal(.{ .layout_idx = target_payload_layout });
            const assign_tag = try self.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .discriminant = singleton.target_discriminant,
                .payload = bridged,
                .next = next,
            } });
            return try self.lowerBridgePlanInto(bridged, source, payload_plan, assign_tag);
        }

        return try self.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .discriminant = singleton.target_discriminant,
            .payload = source,
            .next = next,
        } });
    }

    fn lowerTagUnionToSingletonBridge(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        singleton: anytype,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const source_payload_layout = self.tagUnionPayloadLayout(self.store.getLocal(source).layout_idx, @intCast(singleton.source_discriminant));
        const source_payload = try self.store.addLocal(.{ .layout_idx = source_payload_layout });
        const after_extract = if (singleton.payload_plan) |payload_plan|
            try self.lowerBridgePlanInto(target, source_payload, payload_plan, next)
        else if (self.isZstLayout(self.store.getLocal(target).layout_idx))
            try self.store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = LIR.LocalSpan.empty(),
                .next = next,
            } })
        else
            try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .local = source_payload },
                .next = next,
            } });
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = source_payload,
            .op = .{ .tag_payload_struct = .{
                .source = source,
                .tag_discriminant = singleton.source_discriminant,
            } },
            .next = after_extract,
        } });
    }

    fn boxPayloadLayout(self: *const Lowerer, box_layout_idx: layout_mod.Idx) layout_mod.Idx {
        const box_layout = self.layouts.getLayout(box_layout_idx);
        return switch (box_layout.tag) {
            .box => box_layout.data.box,
            .box_of_zst => .zst,
            else => lirInvariant("lir.lower_ir box bridge expected box layout"),
        };
    }

    fn structFieldCount(self: *const Lowerer, struct_layout_idx: layout_mod.Idx) usize {
        const struct_layout = self.layouts.getLayout(struct_layout_idx);
        if (struct_layout.tag != .struct_) lirInvariant("lir.lower_ir struct bridge expected struct layout");
        const data = self.layouts.getStructData(struct_layout.data.struct_.idx);
        return data.getFields().count;
    }

    fn structFieldLayout(self: *const Lowerer, struct_layout_idx: layout_mod.Idx, field_index: usize) layout_mod.Idx {
        const struct_layout = self.layouts.getLayout(struct_layout_idx);
        if (struct_layout.tag != .struct_) lirInvariant("lir.lower_ir struct bridge expected struct layout");
        return self.layouts.getStructFieldLayoutByOriginalIndex(struct_layout.data.struct_.idx, @intCast(field_index));
    }

    fn tagUnionVariantCount(self: *const Lowerer, tag_union_layout_idx: layout_mod.Idx) usize {
        const tag_union_layout = self.layouts.getLayout(tag_union_layout_idx);
        if (tag_union_layout.tag != .tag_union) lirInvariant("lir.lower_ir tag-union bridge expected tag-union layout");
        const data = self.layouts.getTagUnionData(tag_union_layout.data.tag_union.idx);
        return self.layouts.getTagUnionVariants(data).len;
    }

    fn tagUnionPayloadLayout(self: *const Lowerer, tag_union_layout_idx: layout_mod.Idx, variant_index: usize) layout_mod.Idx {
        const tag_union_layout = self.layouts.getLayout(tag_union_layout_idx);
        if (tag_union_layout.tag != .tag_union) lirInvariant("lir.lower_ir tag-union bridge expected tag-union layout");
        const data = self.layouts.getTagUnionData(tag_union_layout.data.tag_union.idx);
        const variants = self.layouts.getTagUnionVariants(data);
        if (variant_index >= variants.len) lirInvariant("lir.lower_ir tag-union bridge payload index exceeded variant count");
        return variants.get(@intCast(variant_index)).payload_layout;
    }

    fn isZstLayout(self: *const Lowerer, layout_idx: layout_mod.Idx) bool {
        return self.layouts.isZeroSized(self.layouts.getLayout(layout_idx));
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
