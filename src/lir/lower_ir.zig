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
const repr = mir.LambdaSolved.Representation;

/// Public `LowerResourceError` declaration.
pub const LowerResourceError = Allocator.Error;

/// Public `ProcMapEntry` declaration.
pub const ProcMapEntry = struct {
    executable_proc: ir.Ast.ProcRef,
    lir_proc: LIR.LirProcSpecId,
};

/// Public `RequestedLayout` declaration.
pub const RequestedLayout = struct {
    key: repr.CanonicalExecValueTypeKey,
    layout_idx: layout_mod.Idx,
};

/// Runtime encoding for a callable-set member after layout commit.
pub const CallableSetRuntimeEncodingMember = struct {
    member: repr.CallableSetMemberId,
    variant_index: u16,
    discriminant: u16,
    payload_layout: layout_mod.Idx,
    payload_exec_key: ?repr.CanonicalExecValueTypeKey = null,
};

/// Runtime encoding for one callable-set value layout after layout commit.
pub const CallableSetRuntimeEncoding = struct {
    callable_set_key: repr.CanonicalCallableSetKey,
    value_layout: layout_mod.Idx,
    members: []const CallableSetRuntimeEncodingMember,
};

/// Public `Result` declaration.
pub const Result = struct {
    canonical_names: mir.Hosted.CanonicalNameStore,
    store: LirStore,
    layouts: layout_mod.Store,
    root_procs: std.ArrayList(LIR.LirProcSpecId),
    root_metadata: std.ArrayList(mir.Ids.RootMetadata),
    proc_map: std.ArrayList(ProcMapEntry),
    requested_layouts: std.ArrayList(RequestedLayout),
    callable_set_runtime_encodings: std.ArrayList(CallableSetRuntimeEncoding),

    pub fn deinit(self: *Result) void {
        deinitCallableSetRuntimeEncodingContents(self.store.allocator, self.callable_set_runtime_encodings.items);
        self.callable_set_runtime_encodings.deinit(self.store.allocator);
        self.requested_layouts.deinit(self.store.allocator);
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

    pub fn requestedLayoutForKey(
        self: *const Result,
        key: repr.CanonicalExecValueTypeKey,
    ) ?layout_mod.Idx {
        for (self.requested_layouts.items) |entry| {
            if (repr.canonicalExecValueTypeKeyEql(entry.key, key)) return entry.layout_idx;
        }
        return null;
    }
};

pub fn deinitCallableSetRuntimeEncodings(
    allocator: Allocator,
    encodings: []const CallableSetRuntimeEncoding,
) void {
    deinitCallableSetRuntimeEncodingContents(allocator, encodings);
    if (encodings.len > 0) allocator.free(encodings);
}

fn deinitCallableSetRuntimeEncodingContents(
    allocator: Allocator,
    encodings: []const CallableSetRuntimeEncoding,
) void {
    for (encodings) |encoding| {
        if (encoding.members.len > 0) allocator.free(encoding.members);
    }
}

/// Public `run` function.
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
    try lowerer.lowerCallableSetRuntimeEncodings();
    try lowerer.lowerAllDefs();
    try lowerer.bindRoots(explicit_roots, explicit_root_metadata);
    try lowerer.lowerRequestedLayouts();

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
    requested_layouts: std.ArrayList(RequestedLayout),
    callable_set_runtime_encodings: std.ArrayList(CallableSetRuntimeEncoding),
    local_env: std.AutoHashMap(ir.Ast.Symbol, LIR.LocalId),
    layout_ref_cache: std.AutoHashMap(u64, layout_mod.Idx),
    raw_layout_value_cache: std.AutoHashMap(layout_mod.Idx, layout_mod.Idx),
    break_targets: std.ArrayList(LIR.CFStmtId),
    next_join_point: u32,

    const TagUnionSource = struct {
        source: LIR.LocalId,
        layout: layout_mod.Idx,
    };

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
            .requested_layouts = .empty,
            .callable_set_runtime_encodings = .empty,
            .break_targets = .empty,
            .next_join_point = 0,
            .local_env = std.AutoHashMap(ir.Ast.Symbol, LIR.LocalId).init(allocator),
            .layout_ref_cache = std.AutoHashMap(u64, layout_mod.Idx).init(allocator),
            .raw_layout_value_cache = std.AutoHashMap(layout_mod.Idx, layout_mod.Idx).init(allocator),
        };
    }

    fn deinit(self: *Lowerer) void {
        self.break_targets.deinit(self.allocator);
        self.raw_layout_value_cache.deinit();
        self.layout_ref_cache.deinit();
        self.local_env.deinit();
        self.proc_map.deinit(self.allocator);
        for (self.callable_set_runtime_encodings.items) |encoding| {
            if (encoding.members.len > 0) self.allocator.free(encoding.members);
        }
        self.callable_set_runtime_encodings.deinit(self.allocator);
        self.requested_layouts.deinit(self.allocator);
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
            .requested_layouts = self.requested_layouts,
            .callable_set_runtime_encodings = self.callable_set_runtime_encodings,
        };
        self.local_env.deinit();
        self.layout_ref_cache.deinit();
        self.raw_layout_value_cache.deinit();
        self.break_targets.deinit(self.allocator);
        self.store = LirStore.init(self.allocator);
        self.layouts = undefined;
        self.canonical_names = mir.Hosted.CanonicalNameStore.init(self.allocator);
        self.root_procs = .empty;
        self.root_metadata = .empty;
        self.proc_map = .empty;
        self.requested_layouts = .empty;
        self.callable_set_runtime_encodings = .empty;
        self.break_targets = .empty;
        self.next_join_point = 0;
        self.local_env = std.AutoHashMap(ir.Ast.Symbol, LIR.LocalId).init(self.allocator);
        self.layout_ref_cache = std.AutoHashMap(u64, layout_mod.Idx).init(self.allocator);
        self.raw_layout_value_cache = std.AutoHashMap(layout_mod.Idx, layout_mod.Idx).init(self.allocator);
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
                .abi = switch (def.origin) {
                    .source => .roc,
                    .erased_direct_proc_adapter, .erased_adapter => .erased_callable,
                },
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

    fn lowerRequestedLayouts(self: *Lowerer) LowerResourceError!void {
        const requests = self.input.requested_layouts.items;
        if (requests.len == 0) return;
        try self.requested_layouts.ensureUnusedCapacity(self.allocator, requests.len);
        for (requests) |request| {
            self.requested_layouts.appendAssumeCapacity(.{
                .key = request.key,
                .layout_idx = try self.lowerLayoutRef(request.layout),
            });
        }
    }

    fn lowerCallableSetRuntimeEncodings(self: *Lowerer) LowerResourceError!void {
        const encodings = self.input.callable_set_runtime_encodings.items;
        if (encodings.len == 0) return;
        try self.callable_set_runtime_encodings.ensureUnusedCapacity(self.allocator, encodings.len);
        for (encodings) |encoding| {
            const source_members = self.input.callable_set_runtime_encoding_members.items[encoding.members.start..][0..encoding.members.len];
            const members = try self.allocator.alloc(CallableSetRuntimeEncodingMember, source_members.len);
            errdefer self.allocator.free(members);
            for (source_members, 0..) |member, i| {
                members[i] = .{
                    .member = member.member,
                    .variant_index = member.variant_index,
                    .discriminant = member.discriminant,
                    .payload_layout = try self.lowerLayoutRef(member.payload_layout),
                    .payload_exec_key = member.payload_exec_key,
                };
            }
            const lowered = CallableSetRuntimeEncoding{
                .callable_set_key = encoding.callable_set_key,
                .value_layout = try self.lowerLayoutRef(encoding.value_layout),
                .members = members,
            };
            if (@import("builtin").mode == .Debug) {
                self.verifyCallableSetRuntimeEncoding(lowered);
            }
            self.callable_set_runtime_encodings.appendAssumeCapacity(lowered);
        }
    }

    fn verifyCallableSetRuntimeEncoding(self: *const Lowerer, encoding: CallableSetRuntimeEncoding) void {
        if (@import("builtin").mode != .Debug) return;

        for (encoding.members, 0..) |member, i| {
            for (encoding.members[0..i]) |previous| {
                if (previous.member == member.member) {
                    lirInvariant("lir.lower_ir callable-set runtime encoding contained duplicate member");
                }
                if (previous.variant_index == member.variant_index) {
                    lirInvariant("lir.lower_ir callable-set runtime encoding contained duplicate variant index");
                }
                if (previous.discriminant == member.discriminant) {
                    lirInvariant("lir.lower_ir callable-set runtime encoding contained duplicate discriminant");
                }
            }
        }

        const value_layout = self.layouts.getLayout(encoding.value_layout);
        if (value_layout.tag != .tag_union) {
            if (encoding.members.len == 1 and self.isZstLayout(encoding.value_layout)) return;
            lirInvariant("lir.lower_ir callable-set runtime encoding value layout was not a tag union");
        }
        const data = self.layouts.getTagUnionData(value_layout.data.tag_union.idx);
        const variants = self.layouts.getTagUnionVariants(data);
        for (encoding.members) |member| {
            if (@as(usize, member.variant_index) >= variants.len) {
                lirInvariant("lir.lower_ir callable-set runtime encoding variant index exceeded layout variants");
            }
            if (variants.get(@intCast(member.variant_index)).payload_layout != member.payload_layout) {
                lirInvariant("lir.lower_ir callable-set runtime encoding payload layout differed from committed layout");
            }
        }
    }

    fn callableSetRuntimeEncoding(
        self: *const Lowerer,
        id: ir.Ast.CallableSetRuntimeEncodingId,
    ) CallableSetRuntimeEncoding {
        const index = @intFromEnum(id);
        if (index >= self.callable_set_runtime_encodings.items.len) {
            lirInvariant("lir.lower_ir callable-set runtime encoding id out of range");
        }
        return self.callable_set_runtime_encodings.items[index];
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
                .mode = .replace_existing,
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
            .elem_source = .aliases_iterable_element,
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

    fn assignZst(self: *Lowerer, target: LIR.LocalId, next: LIR.CFStmtId) LowerResourceError!LIR.CFStmtId {
        return try self.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = LIR.LocalSpan.empty(),
            .next = next,
        } });
    }

    fn lowerFieldRefInto(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        field_index: u16,
        field_bridge_plan: ?ir.Ast.BridgePlanId,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const source_layout = self.store.getLocal(source).layout_idx;
        const target_layout = self.store.getLocal(target).layout_idx;
        if (self.isZstLayout(source_layout)) {
            if (self.isZstLayout(target_layout)) return try self.assignZst(target, next);
            lirInvariant("lir.lower_ir field access from zst source expected zst target");
        }
        const field_layout = self.structFieldLayout(source_layout, field_index);
        const field_bridge_is_direct = if (field_bridge_plan) |plan| self.input.store.getBridgePlan(plan) == .direct else true;
        if (target_layout == field_layout and field_bridge_is_direct) {
            if (self.isZstLayout(target_layout)) return try self.assignZst(target, next);
            return try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .field = .{
                    .source = source,
                    .field_idx = field_index,
                } },
                .next = next,
            } });
        }

        const raw_field = try self.store.addLocal(.{ .layout_idx = field_layout });
        const after_extract = if (field_bridge_plan) |plan|
            try self.lowerBridgePlanInto(target, raw_field, plan, next)
        else
            try self.lowerPhysicalSlotInto(target, raw_field, next);
        if (self.isZstLayout(field_layout)) return after_extract;
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = raw_field,
            .op = .{ .field = .{
                .source = source,
                .field_idx = field_index,
            } },
            .next = after_extract,
        } });
    }

    fn materializeTagUnionSource(
        self: *Lowerer,
        source: LIR.LocalId,
        source_layout: layout_mod.Idx,
    ) LowerResourceError!TagUnionSource {
        const layout = self.layouts.getLayout(source_layout);
        return switch (layout.tag) {
            .tag_union => .{
                .source = source,
                .layout = source_layout,
            },
            .box => blk: {
                const child = self.layouts.getLayout(layout.data.box);
                if (child.tag != .tag_union) {
                    lirInvariant("lir.lower_ir recursive tag source box did not contain a tag union");
                }
                const unboxed = try self.store.addLocal(.{ .layout_idx = layout.data.box });
                break :blk .{
                    .source = unboxed,
                    .layout = layout.data.box,
                };
            },
            else => lirInvariant("lir.lower_ir tag operation expected tag-union source layout"),
        };
    }

    fn prependTagUnionSourceUnbox(
        self: *Lowerer,
        original_source: LIR.LocalId,
        source: TagUnionSource,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        if (source.source == original_source) return next;
        return try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = source.source,
            .op = .box_unbox,
            .rc_effect = LIR.LowLevel.box_unbox.rcEffect(),
            .args = try self.store.addLocalSpan(&[_]LIR.LocalId{original_source}),
            .next = next,
        } });
    }

    fn lowerUnionDiscriminantInto(
        self: *Lowerer,
        target: LIR.LocalId,
        union_id: anytype,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const expected_callable_set_encoding: ?CallableSetRuntimeEncoding = switch (union_id.source) {
            .known_singleton => |discriminant| {
                return try self.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .i128_literal = .{
                        .value = @intCast(discriminant),
                        .layout_idx = self.store.getLocal(target).layout_idx,
                    } },
                    .next = next,
                } });
            },
            .known_singleton_callable_set => |singleton| {
                _ = self.callableSetRuntimeEncoding(singleton.encoding);
                return try self.store.addCFStmt(.{ .assign_literal = .{
                    .target = target,
                    .value = .{ .i128_literal = .{
                        .value = @intCast(singleton.discriminant),
                        .layout_idx = self.store.getLocal(target).layout_idx,
                    } },
                    .next = next,
                } });
            },
            .runtime_tag_union => null,
            .runtime_callable_set => |encoding| self.callableSetRuntimeEncoding(encoding),
        };
        const source = try self.lowerVar(union_id.value);
        const source_layout = self.store.getLocal(source).layout_idx;
        const source_union = try self.materializeTagUnionSource(source, source_layout);
        if (expected_callable_set_encoding) |encoding| {
            if (source_union.layout != encoding.value_layout) {
                lirInvariant("lir.lower_ir callable-set discriminant source layout differed from runtime encoding layout");
            }
        }
        const read_discriminant = try self.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .discriminant = .{
                .source = source_union.source,
            } },
            .next = next,
        } });
        return try self.prependTagUnionSourceUnbox(source, source_union, read_discriminant);
    }

    fn lowerTagPayloadStructRefInto(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        variant_index: u16,
        tag_discriminant: u16,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const source_layout = self.store.getLocal(source).layout_idx;
        const target_layout = self.store.getLocal(target).layout_idx;
        if (self.isZstLayout(source_layout)) {
            if (self.isZstLayout(target_layout)) return try self.assignZst(target, next);
            lirInvariant("lir.lower_ir tag payload access from zst source expected zst target");
        }
        const source_union = try self.materializeTagUnionSource(source, source_layout);
        const payload_layout = self.tagUnionPayloadLayout(source_union.layout, variant_index);
        if (target_layout == payload_layout) {
            if (self.isZstLayout(target_layout)) {
                const assign_zst = try self.assignZst(target, next);
                return try self.prependTagUnionSourceUnbox(source, source_union, assign_zst);
            }
            const extract = try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .tag_payload_struct = .{
                    .source = source_union.source,
                    .variant_index = variant_index,
                    .tag_discriminant = tag_discriminant,
                } },
                .next = next,
            } });
            return try self.prependTagUnionSourceUnbox(source, source_union, extract);
        }

        const raw_payload = try self.store.addLocal(.{ .layout_idx = payload_layout });
        const after_extract = try self.lowerPhysicalSlotInto(target, raw_payload, next);
        const extract = try self.store.addCFStmt(.{ .assign_ref = .{
            .target = raw_payload,
            .op = .{ .tag_payload_struct = .{
                .source = source_union.source,
                .variant_index = variant_index,
                .tag_discriminant = tag_discriminant,
            } },
            .next = after_extract,
        } });
        return try self.prependTagUnionSourceUnbox(source, source_union, extract);
    }

    fn lowerMakeStructInto(
        self: *Lowerer,
        target: LIR.LocalId,
        make_struct: anytype,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const target_layout = self.store.getLocal(target).layout_idx;
        if (self.isZstLayout(target_layout)) return try self.assignZst(target, next);

        const vars = self.input.store.sliceVarSpan(make_struct.fields);
        const bridge_plans = self.input.store.sliceBridgePlanSpan(make_struct.field_bridge_plans);
        const logical_field_count = self.structLogicalFieldCount(target_layout);
        if (vars.len < logical_field_count) {
            lirInvariant("lir.lower_ir struct construction field count does not match target layout");
        }
        if (bridge_plans.len != vars.len) {
            lirInvariant("lir.lower_ir struct construction bridge count does not match field count");
        }

        const source_fields = try self.allocator.alloc(LIR.LocalId, vars.len);
        defer self.allocator.free(source_fields);
        const physical_fields = try self.allocator.alloc(LIR.LocalId, vars.len);
        defer self.allocator.free(physical_fields);

        for (vars, 0..) |var_, i| {
            const source = try self.lowerVar(var_);
            source_fields[i] = source;
            const field_layout = self.structFieldLayout(target_layout, i);
            physical_fields[i] = if (self.store.getLocal(source).layout_idx == field_layout)
                source
            else
                try self.store.addLocal(.{ .layout_idx = field_layout });
        }

        var current = try self.store.addCFStmt(.{ .assign_struct = .{
            .target = target,
            .fields = try self.store.addLocalSpan(physical_fields),
            .next = next,
        } });

        var i = physical_fields.len;
        while (i > 0) {
            i -= 1;
            if (physical_fields[i] != source_fields[i]) {
                current = try self.lowerBridgePlanInto(physical_fields[i], source_fields[i], bridge_plans[i], current);
            }
        }
        return current;
    }

    fn lowerMakeUnionInto(
        self: *Lowerer,
        target: LIR.LocalId,
        tag: anytype,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        var payload: ?LIR.LocalId = null;
        var payload_bridge: ?struct {
            target: LIR.LocalId,
            source: LIR.LocalId,
            plan: ir.Ast.BridgePlanId,
        } = null;
        const target_layout = self.store.getLocal(target).layout_idx;
        if (tag.payload) |payload_var| {
            const source = try self.lowerVar(payload_var);
            if (self.isZstLayout(target_layout)) {
                if (!self.isZstLayout(self.store.getLocal(source).layout_idx)) {
                    lirInvariant("lir.lower_ir zst tag construction received non-zst payload");
                }
                return try self.assignZst(target, next);
            }
            const expected_payload_layout = self.tagPayloadLayoutForConstruction(target_layout, tag.variant_index) orelse
                lirInvariant("lir.lower_ir tag construction had payload for layout without payload storage");
            if (self.store.getLocal(source).layout_idx == expected_payload_layout) {
                payload = source;
            } else {
                const plan = tag.payload_bridge_plan orelse
                    lirInvariant("lir.lower_ir tag construction missing payload bridge plan");
                const physical_payload = try self.store.addLocal(.{ .layout_idx = expected_payload_layout });
                payload = physical_payload;
                payload_bridge = .{
                    .target = physical_payload,
                    .source = source,
                    .plan = plan,
                };
            }
        } else if (tag.payload_bridge_plan != null) {
            lirInvariant("lir.lower_ir tag construction had payload bridge without payload");
        }
        if (self.isZstLayout(target_layout)) return try self.assignZst(target, next);

        const assign_tag = try self.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .variant_index = tag.variant_index,
            .discriminant = tag.discriminant,
            .payload = payload,
            .next = next,
        } });
        return if (payload_bridge) |bridge|
            try self.lowerBridgePlanInto(bridge.target, bridge.source, bridge.plan, assign_tag)
        else
            assign_tag;
    }

    fn lowerMakeListInto(
        self: *Lowerer,
        target: LIR.LocalId,
        list: anytype,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const vars = self.input.store.sliceVarSpan(list.elems);
        const bridge_plans = self.input.store.sliceBridgePlanSpan(list.elem_bridge_plans);
        if (bridge_plans.len != vars.len) {
            lirInvariant("lir.lower_ir list construction bridge count does not match element count");
        }
        if (vars.len == 0) {
            return try self.store.addCFStmt(.{ .assign_list = .{
                .target = target,
                .elems = LIR.LocalSpan.empty(),
                .next = next,
            } });
        }

        const elem_layout = self.listElemLayout(self.store.getLocal(target).layout_idx);
        const source_elems = try self.allocator.alloc(LIR.LocalId, vars.len);
        defer self.allocator.free(source_elems);
        const physical_elems = try self.allocator.alloc(LIR.LocalId, vars.len);
        defer self.allocator.free(physical_elems);

        for (vars, 0..) |var_, i| {
            const source = try self.lowerVar(var_);
            source_elems[i] = source;
            physical_elems[i] = if (self.store.getLocal(source).layout_idx == elem_layout)
                source
            else
                try self.store.addLocal(.{ .layout_idx = elem_layout });
        }

        var current = try self.store.addCFStmt(.{ .assign_list = .{
            .target = target,
            .elems = try self.store.addLocalSpan(physical_elems),
            .next = next,
        } });

        var i = physical_elems.len;
        while (i > 0) {
            i -= 1;
            if (physical_elems[i] != source_elems[i]) {
                current = try self.lowerBridgePlanInto(physical_elems[i], source_elems[i], bridge_plans[i], current);
            }
        }
        return current;
    }

    fn lowerNominalReinterpretInto(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        if (self.canLowerPhysicalSlotInto(target, source)) {
            return try self.lowerPhysicalSlotInto(target, source, next);
        }
        return try self.store.addCFStmt(.{ .assign_ref = .{
            .target = target,
            .op = .{ .nominal = .{ .backing_ref = source } },
            .next = next,
        } });
    }

    fn lowerListGetUnsafeInto(
        self: *Lowerer,
        target: LIR.LocalId,
        call: anytype,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const vars = self.input.store.sliceVarSpan(call.args);
        if (vars.len != 2) {
            lirInvariant("lir.lower_ir list_get_unsafe expected exactly list and index arguments");
        }

        const list = try self.lowerVar(vars[0]);
        const index = try self.lowerVar(vars[1]);
        const elem_layout = self.listElemLayout(self.store.getLocal(list).layout_idx);
        const target_layout = self.store.getLocal(target).layout_idx;
        const read_target = if (target_layout == elem_layout)
            target
        else
            try self.store.addLocal(.{ .layout_idx = elem_layout });

        const read_args = [_]LIR.LocalId{ list, index };
        const after_read = if (read_target == target)
            next
        else
            try self.lowerPhysicalSlotInto(target, read_target, next);

        return try self.store.addCFStmt(.{ .assign_low_level = .{
            .target = read_target,
            .op = call.op,
            .rc_effect = call.rc_effect,
            .args = try self.store.addLocalSpan(&read_args),
            .next = after_read,
        } });
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
            .make_struct => |make_struct| try self.lowerMakeStructInto(target, make_struct, next),
            .make_list => |list| try self.lowerMakeListInto(target, list, next),
            .make_union => |tag| try self.lowerMakeUnionInto(target, tag, next),
            .get_union_id => |source| try self.lowerUnionDiscriminantInto(target, source, next),
            .get_union_struct => |payload| try self.lowerTagPayloadStructRefInto(
                target,
                try self.lowerVar(payload.value),
                payload.variant_index,
                payload.tag_discriminant,
                next,
            ),
            .get_struct_field => |field| try self.lowerFieldRefInto(
                target,
                try self.lowerVar(field.record),
                field.field_index,
                field.field_bridge_plan,
                next,
            ),
            .nominal_reinterpret => |backing| try self.lowerNominalReinterpretInto(target, try self.lowerVar(backing), next),
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
            .call_low_level => |call| blk: {
                if (call.op == .list_get_unsafe) {
                    break :blk try self.lowerListGetUnsafeInto(target, call, next);
                }

                if (call.op == .box_box or call.op == .box_unbox) {
                    const target_layout = self.store.getLocal(target).layout_idx;
                    if (self.isErasedCallableLayout(target_layout)) {
                        const vars = self.input.store.sliceVarSpan(call.args);
                        if (vars.len != 1) {
                            lirInvariant("lir.lower_ir erased-callable Box operation expected exactly one argument");
                        }
                        const source = try self.lowerVar(vars[0]);
                        const source_layout = self.store.getLocal(source).layout_idx;
                        if (!self.isErasedCallableLayout(source_layout)) {
                            lirInvariant("lir.lower_ir erased-callable Box operation source was not an erased callable");
                        }
                        break :blk try self.store.addCFStmt(.{ .assign_ref = .{
                            .target = target,
                            .op = .{ .local = source },
                            .next = next,
                        } });
                    }
                }

                break :blk try self.store.addCFStmt(.{ .assign_low_level = .{
                    .target = target,
                    .op = call.op,
                    .rc_effect = call.rc_effect,
                    .args = try self.lowerVarSpan(call.args),
                    .next = next,
                } });
            },
            .call_erased => |call| try self.store.addCFStmt(.{ .assign_call_erased = .{
                .target = target,
                .closure = try self.lowerVar(call.func),
                .args = try self.lowerVarSpan(call.args),
                .next = next,
            } }),
            .packed_erased_fn => |packed_fn| try self.lowerPackedErasedFnInto(target, packed_fn, next),
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
        packed_fn: anytype,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const has_capture = packed_fn.capture != null;
        if (has_capture != (packed_fn.capture_layout != null)) {
            lirInvariant("lir.lower_ir packed erased fn capture value disagrees with capture layout");
        }
        const capture_layout = if (packed_fn.capture_layout) |capture_layout_ref|
            try self.lowerLayoutRef(capture_layout_ref)
        else
            null;
        const on_drop: LIR.ErasedCallableOnDrop = if (capture_layout) |layout_idx| blk: {
            const helper_key = layout_mod.RcHelperKey{ .op = .decref, .layout_idx = layout_idx };
            if (self.layouts.rcHelperPlan(helper_key) == .noop) {
                break :blk .none;
            }
            break :blk .{ .rc_helper = helper_key };
        } else .none;
        return try self.store.addCFStmt(.{ .assign_packed_erased_fn = .{
            .target = target,
            .proc = self.lirProcForExecutable(packed_fn.proc) orelse lirInvariant("lir.lower_ir reached packed_erased_fn before proc placeholder"),
            .capture = if (packed_fn.capture) |capture| try self.lowerVar(capture) else null,
            .capture_layout = capture_layout,
            .on_drop = on_drop,
            .next = next,
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
            .direct => try self.lowerPhysicalSlotInto(target, source, next),
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
            .nominal_reinterpret => try self.lowerNominalReinterpretInto(target, source, next),
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
        if (self.isZstLayout(source_layout) or self.isZstLayout(target_layout)) {
            if (!self.isZstLayout(source_layout) or !self.isZstLayout(target_layout)) {
                lirInvariant("lir.lower_ir struct bridge with ZST endpoint expected both endpoints to be ZST");
            }
            return try self.assignZst(target, next);
        }
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
            current = try self.lowerFieldRefInto(source_field, source, @intCast(i), null, current);
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
        if (self.isZstLayout(source_layout) or self.isZstLayout(target_layout)) {
            if (!self.isZstLayout(source_layout) or !self.isZstLayout(target_layout) or child_plans.len != 1) {
                lirInvariant("lir.lower_ir tag-union bridge with ZST endpoint expected both endpoints to be the same singleton/ZST shape");
            }
            return try self.store.addCFStmt(.{ .assign_struct = .{
                .target = target,
                .fields = LIR.LocalSpan.empty(),
                .next = next,
            } });
        }
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
            .variant_index = @intCast(variant_index),
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
                .variant_index = @intCast(variant_index),
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
                .variant_index = singleton.target_discriminant,
                .discriminant = singleton.target_discriminant,
                .payload = null,
                .next = next,
            } });
        }

        if (singleton.payload_plan) |payload_plan| {
            const bridged = try self.store.addLocal(.{ .layout_idx = target_payload_layout });
            const assign_tag = try self.store.addCFStmt(.{ .assign_tag = .{
                .target = target,
                .variant_index = singleton.target_discriminant,
                .discriminant = singleton.target_discriminant,
                .payload = bridged,
                .next = next,
            } });
            return try self.lowerBridgePlanInto(bridged, source, payload_plan, assign_tag);
        }

        return try self.store.addCFStmt(.{ .assign_tag = .{
            .target = target,
            .variant_index = singleton.target_discriminant,
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
                .variant_index = singleton.source_discriminant,
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

    fn isErasedCallableLayout(self: *const Lowerer, layout_idx: layout_mod.Idx) bool {
        return self.layouts.getLayout(layout_idx).tag == .erased_callable;
    }

    fn listElemLayout(self: *const Lowerer, list_layout_idx: layout_mod.Idx) layout_mod.Idx {
        const resolved = self.layouts.resolvedListLayoutIdx(list_layout_idx) orelse
            lirInvariant("lir.lower_ir list construction expected a resolved list layout");
        const list_layout = self.layouts.getLayout(resolved);
        return switch (list_layout.tag) {
            .list => list_layout.data.list,
            .list_of_zst => .zst,
            else => lirInvariant("lir.lower_ir list construction expected list layout"),
        };
    }

    fn lowerPhysicalSlotInto(
        self: *Lowerer,
        target: LIR.LocalId,
        source: LIR.LocalId,
        next: LIR.CFStmtId,
    ) LowerResourceError!LIR.CFStmtId {
        const target_layout = self.store.getLocal(target).layout_idx;
        const source_layout = self.store.getLocal(source).layout_idx;
        if (target_layout == source_layout) {
            if (self.isZstLayout(target_layout)) return try self.assignZst(target, next);
            return try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .local = source },
                .next = next,
            } });
        }

        if (self.rawLayoutValueEquivalent(target_layout, source_layout)) {
            if (self.isListLikeLayout(target_layout) and self.isListLikeLayout(source_layout)) {
                return try self.store.addCFStmt(.{ .assign_ref = .{
                    .target = target,
                    .op = .{ .list_reinterpret = .{ .backing_ref = source } },
                    .next = next,
                } });
            }

            return try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .nominal = .{ .backing_ref = source } },
                .next = next,
            } });
        }

        if (self.listStorageEquivalent(target_layout, source_layout)) {
            return try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .list_reinterpret = .{ .backing_ref = source } },
                .next = next,
            } });
        }

        if (self.erasedHandlePointerEquivalent(target_layout, source_layout)) {
            return try self.store.addCFStmt(.{ .assign_ref = .{
                .target = target,
                .op = .{ .nominal = .{ .backing_ref = source } },
                .next = next,
            } });
        }

        if (self.boxLayoutContains(source_layout, target_layout)) {
            const args = [_]LIR.LocalId{source};
            return try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = .box_unbox,
                .rc_effect = LIR.LowLevel.box_unbox.rcEffect(),
                .args = try self.store.addLocalSpan(&args),
                .next = next,
            } });
        }

        if (self.boxLayoutContains(target_layout, source_layout)) {
            const args = [_]LIR.LocalId{source};
            return try self.store.addCFStmt(.{ .assign_low_level = .{
                .target = target,
                .op = .box_box,
                .rc_effect = LIR.LowLevel.box_box.rcEffect(),
                .args = try self.store.addLocalSpan(&args),
                .next = next,
            } });
        }

        if (@import("builtin").mode == .Debug) {
            const target_tag = self.layouts.getLayout(target_layout).tag;
            const source_tag = self.layouts.getLayout(source_layout).tag;
            const target_elem = self.nonZstListElementLayout(target_layout);
            const source_elem = self.nonZstListElementLayout(source_layout);
            const target_elem_index: i64 = if (target_elem) |elem| @intFromEnum(elem) else -1;
            const source_elem_index: i64 = if (source_elem) |elem| @intFromEnum(elem) else -1;
            const target_elem_tag = if (target_elem) |elem| @tagName(self.layouts.getLayout(elem).tag) else "zst";
            const source_elem_tag = if (source_elem) |elem| @tagName(self.layouts.getLayout(elem).tag) else "zst";
            std.debug.panic(
                "lir.lower_ir physical recursive slot bridge expected direct, box, unbox, or list storage-equivalent layout relation: target_layout={} target_tag={s} target_elem={d} target_elem_tag={s} source_layout={} source_tag={s} source_elem={d} source_elem_tag={s}",
                .{ target_layout, @tagName(target_tag), target_elem_index, target_elem_tag, source_layout, @tagName(source_tag), source_elem_index, source_elem_tag },
            );
        }
        unreachable;
    }

    fn canLowerPhysicalSlotInto(self: *const Lowerer, target: LIR.LocalId, source: LIR.LocalId) bool {
        const target_layout = self.store.getLocal(target).layout_idx;
        const source_layout = self.store.getLocal(source).layout_idx;
        return target_layout == source_layout or
            self.rawLayoutValueEquivalent(target_layout, source_layout) or
            self.listStorageEquivalent(target_layout, source_layout) or
            self.erasedHandlePointerEquivalent(target_layout, source_layout) or
            self.boxLayoutContains(source_layout, target_layout) or
            self.boxLayoutContains(target_layout, source_layout);
    }

    fn rawLayoutValueEquivalent(self: *const Lowerer, a: layout_mod.Idx, b: layout_mod.Idx) bool {
        if (self.raw_layout_value_cache.get(a)) |value| {
            if (value == b) return true;
        }
        if (self.raw_layout_value_cache.get(b)) |value| {
            if (value == a) return true;
        }
        return false;
    }

    fn listStorageEquivalent(self: *const Lowerer, a: layout_mod.Idx, b: layout_mod.Idx) bool {
        if (!self.isListLikeLayout(a) or !self.isListLikeLayout(b)) return false;
        return self.physicalStorageEquivalent(a, b, self.layouts.resolved_list_layouts.items.len + 1);
    }

    fn physicalStorageEquivalent(self: *const Lowerer, a: layout_mod.Idx, b: layout_mod.Idx, remaining: usize) bool {
        if (a == b) return true;
        if (self.rawLayoutValueEquivalent(a, b)) return true;
        if (self.erasedHandlePointerEquivalent(a, b)) return true;
        if (remaining == 0) return false;

        if (!self.isListLikeLayout(a) or !self.isListLikeLayout(b)) return false;
        const a_elem = self.nonZstListElementLayout(a);
        const b_elem = self.nonZstListElementLayout(b);
        if (a_elem == null or b_elem == null) return a_elem == null and b_elem == null;
        return self.physicalStorageEquivalent(a_elem.?, b_elem.?, remaining - 1);
    }

    fn nonZstListElementLayout(self: *const Lowerer, layout_idx: layout_mod.Idx) ?layout_mod.Idx {
        const list_layout = self.layouts.getLayout(layout_idx);
        return switch (list_layout.tag) {
            .list => if (self.isZstLayout(list_layout.data.list)) null else list_layout.data.list,
            .list_of_zst => null,
            else => null,
        };
    }

    fn erasedHandlePointerEquivalent(self: *const Lowerer, a: layout_mod.Idx, b: layout_mod.Idx) bool {
        return (a == .opaque_ptr and self.isBoxPointerLayout(b)) or
            (b == .opaque_ptr and self.isBoxPointerLayout(a));
    }

    fn isBoxPointerLayout(self: *const Lowerer, layout_idx: layout_mod.Idx) bool {
        return switch (self.layouts.getLayout(layout_idx).tag) {
            .box, .box_of_zst => true,
            else => false,
        };
    }

    fn isListLikeLayout(self: *const Lowerer, layout_idx: layout_mod.Idx) bool {
        return switch (self.layouts.getLayout(layout_idx).tag) {
            .list, .list_of_zst => true,
            else => false,
        };
    }

    fn boxLayoutContains(self: *const Lowerer, box_layout_idx: layout_mod.Idx, payload_layout_idx: layout_mod.Idx) bool {
        const box_layout = self.layouts.getLayout(box_layout_idx);
        return switch (box_layout.tag) {
            .box => box_layout.data.box == payload_layout_idx or
                (self.raw_layout_value_cache.get(box_layout.data.box) orelse box_layout.data.box) == payload_layout_idx,
            .box_of_zst => self.isZstLayout(payload_layout_idx),
            else => false,
        };
    }

    fn tagPayloadLayoutForConstruction(
        self: *const Lowerer,
        target_layout_idx: layout_mod.Idx,
        variant_index: u16,
    ) ?layout_mod.Idx {
        const target_layout = self.layouts.getLayout(target_layout_idx);
        return switch (target_layout.tag) {
            .tag_union => self.tagUnionPayloadLayout(target_layout_idx, variant_index),
            .box => blk: {
                const inner = self.layouts.getLayout(target_layout.data.box);
                if (inner.tag != .tag_union) break :blk null;
                break :blk self.tagUnionPayloadLayout(target_layout.data.box, variant_index);
            },
            else => null,
        };
    }

    fn structFieldCount(self: *const Lowerer, struct_layout_idx: layout_mod.Idx) usize {
        const struct_layout = self.layouts.getLayout(struct_layout_idx);
        if (struct_layout.tag != .struct_) lirInvariant("lir.lower_ir struct bridge expected struct layout");
        const data = self.layouts.getStructData(struct_layout.data.struct_.idx);
        return data.getFields().count;
    }

    fn structLogicalFieldCount(self: *const Lowerer, struct_layout_idx: layout_mod.Idx) usize {
        const struct_layout = self.layouts.getLayout(struct_layout_idx);
        if (struct_layout.tag != .struct_) lirInvariant("lir.lower_ir struct bridge expected struct layout");
        const data = self.layouts.getStructData(struct_layout.data.struct_.idx);
        const fields = self.layouts.struct_fields.sliceRange(data.getFields());
        var max_field_index: usize = 0;
        for (0..fields.len) |i| {
            const field = fields.get(@intCast(i));
            max_field_index = @max(max_field_index, @as(usize, @intCast(field.index)) + 1);
        }
        return max_field_index;
    }

    fn structFieldLayout(self: *const Lowerer, struct_layout_idx: layout_mod.Idx, field_index: usize) layout_mod.Idx {
        const struct_layout = self.layouts.getLayout(struct_layout_idx);
        if (struct_layout.tag != .struct_) lirInvariant("lir.lower_ir struct bridge expected struct layout");
        const layout = self.layouts.getStructFieldLayoutByOriginalIndex(struct_layout.data.struct_.idx, @intCast(field_index));
        return if (layout == layout_mod.Idx.none) .zst else layout;
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
                const key = layout_mod.graphRefKey(ref);
                if (self.layout_ref_cache.get(key)) |existing| break :blk existing;
                var commit = try self.layouts.commitGraph(&self.input.layouts, ref);
                defer commit.deinit(self.allocator);
                try self.cacheGraphCommit(&commit);
                break :blk commit.root_idx;
            },
        };
    }

    fn cacheGraphCommit(self: *Lowerer, commit: *const layout_mod.Store.GraphCommit) LowerResourceError!void {
        for (commit.value_layouts, 0..) |layout_idx, i| {
            const local_ref: ir.Layout.Ref = .{ .local = @enumFromInt(@as(u32, @intCast(i))) };
            const key = layout_mod.graphRefKey(local_ref);
            if (self.layout_ref_cache.get(key)) |existing| {
                if (existing != layout_idx) {
                    lirInvariant("lir.lower_ir layout graph node committed to two physical layout ids");
                }
                continue;
            }
            try self.layout_ref_cache.put(key, layout_idx);
        }
        for (commit.raw_layouts, commit.value_layouts) |raw_layout, value_layout| {
            if (self.raw_layout_value_cache.get(raw_layout)) |existing| {
                if (existing != value_layout) {
                    lirInvariant("lir.lower_ir raw recursive layout committed to two logical layout ids");
                }
                continue;
            }
            try self.raw_layout_value_cache.put(raw_layout, value_layout);
        }
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
