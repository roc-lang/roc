//! Lambda-solved MIR construction state.

const std = @import("std");
const check = @import("check");
const symbol_mod = @import("symbol");
const Lifted = @import("../lifted/mod.zig");
const MonoRow = @import("../mono_row/mod.zig");
const ids = @import("../ids.zig");

const Ast = @import("ast.zig");
const Type = @import("type.zig");
const repr = @import("representation.zig");

const Allocator = std.mem.Allocator;
const canonical = check.CanonicalNames;
const checked_artifact = check.CheckedArtifact;

pub const Proc = struct {
    proc: canonical.MirProcedureRef,
    body: Ast.DefId,
    representation_instance: repr.ProcRepresentationInstanceId,
};

const ProcBuildRecord = struct {
    proc: canonical.MirProcedureRef,
    body: Ast.DefId,
    representation_instance: repr.ProcRepresentationInstanceId,
    solve_session: repr.RepresentationSolveSessionId,
    value_store: repr.ValueInfoStoreId,
    public_roots: repr.ProcPublicValueRoots,
};

pub const Program = struct {
    allocator: Allocator,
    canonical_names: canonical.CanonicalNameStore,
    literal_pool: ids.ProgramLiteralPool,
    symbols: symbol_mod.Store,
    row_shapes: MonoRow.Store,
    types: Type.Store,
    ast: Ast.Store,
    procs: std.ArrayList(Proc),
    executable_synthetic_procs: std.ArrayList(ids.ExecutableSyntheticProc),
    root_procs: std.ArrayList(canonical.MirProcedureRef),
    root_metadata: std.ArrayList(ids.RootMetadata),
    solve_sessions: std.ArrayList(repr.RepresentationSolveSession),
    proc_instances: std.ArrayList(repr.ProcRepresentationInstance),
    value_stores: std.ArrayList(repr.ValueInfoStore),

    pub fn init(allocator: Allocator) Program {
        return .{
            .allocator = allocator,
            .canonical_names = canonical.CanonicalNameStore.init(allocator),
            .literal_pool = ids.ProgramLiteralPool.init(allocator),
            .symbols = symbol_mod.Store.init(allocator),
            .row_shapes = MonoRow.Store.init(allocator),
            .types = Type.Store.init(allocator),
            .ast = Ast.Store.init(allocator),
            .procs = .empty,
            .executable_synthetic_procs = .empty,
            .root_procs = .empty,
            .root_metadata = .empty,
            .solve_sessions = .empty,
            .proc_instances = .empty,
            .value_stores = .empty,
        };
    }

    pub fn deinit(self: *Program) void {
        for (self.value_stores.items) |*store| {
            store.deinit();
        }
        self.value_stores.deinit(self.allocator);
        for (self.proc_instances.items) |*instance| {
            repr.deinitProcRepresentationInstance(self.allocator, instance);
        }
        self.proc_instances.deinit(self.allocator);
        for (self.solve_sessions.items) |*session| {
            session.deinit(self.allocator);
        }
        self.solve_sessions.deinit(self.allocator);
        self.root_metadata.deinit(self.allocator);
        self.root_procs.deinit(self.allocator);
        self.executable_synthetic_procs.deinit(self.allocator);
        self.procs.deinit(self.allocator);
        self.ast.deinit();
        self.types.deinit();
        self.row_shapes.deinit();
        self.symbols.deinit();
        self.literal_pool.deinit();
        self.canonical_names.deinit();
        self.* = Program.init(self.allocator);
    }
};

const ProcedureDependencyGraph = struct {
    allocator: Allocator,
    input: *const Lifted.Lift.Program,
    proc_instance_map: *const std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId),
    edges: []std.ArrayList(u32),

    fn init(
        allocator: Allocator,
        input: *const Lifted.Lift.Program,
        proc_instance_map: *const std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId),
    ) Allocator.Error!ProcedureDependencyGraph {
        const edges = try allocator.alloc(std.ArrayList(u32), input.procs.items.len);
        errdefer allocator.free(edges);
        for (edges) |*edge_list| {
            edge_list.* = .empty;
        }
        errdefer {
            for (edges) |*edge_list| {
                edge_list.deinit(allocator);
            }
        }

        var graph = ProcedureDependencyGraph{
            .allocator = allocator,
            .input = input,
            .proc_instance_map = proc_instance_map,
            .edges = edges,
        };
        for (input.procs.items, 0..) |proc, i| {
            _ = proc;
            try graph.collectDefDependencies(@intCast(i), input.procs.items[i].body);
        }
        return graph;
    }

    fn deinit(self: *ProcedureDependencyGraph) void {
        for (self.edges) |*edge_list| {
            edge_list.deinit(self.allocator);
        }
        self.allocator.free(self.edges);
    }

    fn collectDefDependencies(self: *ProcedureDependencyGraph, source_index: u32, def_id: Lifted.Ast.DefId) Allocator.Error!void {
        const def = self.input.ast.getDef(def_id);
        switch (def.value) {
            .fn_ => |fn_| try self.collectExprDependencies(source_index, fn_.body),
            .hosted_fn => {},
            .val => |expr| try self.collectExprDependencies(source_index, expr),
            .run => |run| try self.collectExprDependencies(source_index, run.body),
        }
    }

    fn collectExprDependencies(self: *ProcedureDependencyGraph, source_index: u32, expr_id: Lifted.Ast.ExprId) Allocator.Error!void {
        const expr = self.input.ast.getExpr(expr_id);
        switch (expr.data) {
            .var_,
            .capture_ref,
            .int_lit,
            .frac_f32_lit,
            .frac_f64_lit,
            .dec_lit,
            .bool_lit,
            .str_lit,
            .const_instance,
            .unit,
            .crash,
            .runtime_error,
            => {},
            .tag => |tag| {
                try self.collectTagPayloadEvalDependencies(source_index, tag.eval_order);
                try self.collectTagPayloadAssemblyDependencies(source_index, tag.assembly_order);
            },
            .record => |record| {
                try self.collectRecordFieldEvalDependencies(source_index, record.eval_order);
                try self.collectRecordFieldAssemblyDependencies(source_index, record.assembly_order);
            },
            .nominal_reinterpret => |backing| try self.collectExprDependencies(source_index, backing),
            .access => |access| try self.collectExprDependencies(source_index, access.record),
            .structural_eq => |eq| {
                try self.collectExprDependencies(source_index, eq.lhs);
                try self.collectExprDependencies(source_index, eq.rhs);
            },
            .bool_not => |child| try self.collectExprDependencies(source_index, child),
            .let_ => |let_| {
                try self.collectExprDependencies(source_index, let_.body);
                try self.collectExprDependencies(source_index, let_.rest);
            },
            .call_value => |call| {
                try self.collectExprDependencies(source_index, call.func);
                try self.collectExprSpanDependencies(source_index, call.args);
            },
            .call_proc => |call| {
                try self.appendProcedureDependency(source_index, call.proc);
                try self.collectExprSpanDependencies(source_index, call.args);
            },
            .proc_value => |proc_value| {
                try self.appendProcedureDependency(source_index, proc_value.proc);
                try self.collectCaptureArgDependencies(source_index, proc_value.captures);
            },
            .inspect => |child| try self.collectExprDependencies(source_index, child),
            .low_level => |low_level| try self.collectExprSpanDependencies(source_index, low_level.args),
            .match_ => |match_| {
                try self.collectExprDependencies(source_index, match_.cond);
                for (self.input.ast.sliceBranchSpan(match_.branches)) |branch_id| {
                    const branch = self.input.ast.getBranch(branch_id);
                    if (branch.guard) |guard| try self.collectExprDependencies(source_index, guard);
                    try self.collectExprDependencies(source_index, branch.body);
                }
            },
            .if_ => |if_| {
                try self.collectExprDependencies(source_index, if_.cond);
                try self.collectExprDependencies(source_index, if_.then_body);
                try self.collectExprDependencies(source_index, if_.else_body);
            },
            .block => |block| {
                for (self.input.ast.sliceStmtSpan(block.stmts)) |stmt_id| {
                    try self.collectStmtDependencies(source_index, self.input.ast.getStmt(stmt_id));
                }
                try self.collectExprDependencies(source_index, block.final_expr);
            },
            .tuple => |items| try self.collectExprSpanDependencies(source_index, items),
            .tag_payload => |payload| try self.collectExprDependencies(source_index, payload.tag_union),
            .tuple_access => |access| try self.collectExprDependencies(source_index, access.tuple),
            .list => |items| try self.collectExprSpanDependencies(source_index, items),
            .return_ => |child| try self.collectExprDependencies(source_index, child),
            .for_ => |for_| {
                try self.collectExprDependencies(source_index, for_.iterable);
                try self.collectExprDependencies(source_index, for_.body);
            },
        }
    }

    fn collectStmtDependencies(self: *ProcedureDependencyGraph, source_index: u32, stmt: Lifted.Ast.Stmt) Allocator.Error!void {
        switch (stmt) {
            .decl => |decl| try self.collectExprDependencies(source_index, decl.body),
            .var_decl => |decl| try self.collectExprDependencies(source_index, decl.body),
            .reassign => |reassign| try self.collectExprDependencies(source_index, reassign.body),
            .expr => |expr| try self.collectExprDependencies(source_index, expr),
            .debug => |expr| try self.collectExprDependencies(source_index, expr),
            .expect => |expr| try self.collectExprDependencies(source_index, expr),
            .crash,
            .break_,
            => {},
            .return_ => |expr| try self.collectExprDependencies(source_index, expr),
            .for_ => |for_| {
                try self.collectExprDependencies(source_index, for_.iterable);
                try self.collectExprDependencies(source_index, for_.body);
            },
            .while_ => |while_| {
                try self.collectExprDependencies(source_index, while_.cond);
                try self.collectExprDependencies(source_index, while_.body);
            },
        }
    }

    fn collectExprSpanDependencies(self: *ProcedureDependencyGraph, source_index: u32, span: Lifted.Ast.Span(Lifted.Ast.ExprId)) Allocator.Error!void {
        for (self.input.ast.sliceExprSpan(span)) |expr_id| {
            try self.collectExprDependencies(source_index, expr_id);
        }
    }

    fn collectCaptureArgDependencies(self: *ProcedureDependencyGraph, source_index: u32, span: Lifted.Ast.Span(Lifted.Ast.CaptureArg)) Allocator.Error!void {
        for (self.input.ast.sliceCaptureArgSpan(span)) |capture| {
            try self.collectExprDependencies(source_index, capture.expr);
        }
    }

    fn collectRecordFieldEvalDependencies(self: *ProcedureDependencyGraph, source_index: u32, span: Lifted.Ast.Span(Lifted.Ast.RecordFieldEval)) Allocator.Error!void {
        for (self.input.ast.sliceRecordFieldEvalSpan(span)) |field| {
            try self.collectExprDependencies(source_index, field.value);
        }
    }

    fn collectRecordFieldAssemblyDependencies(self: *ProcedureDependencyGraph, source_index: u32, span: Lifted.Ast.Span(Lifted.Ast.RecordFieldAssembly)) Allocator.Error!void {
        for (self.input.ast.sliceRecordFieldAssemblySpan(span)) |field| {
            try self.collectExprDependencies(source_index, field.value);
        }
    }

    fn collectTagPayloadEvalDependencies(self: *ProcedureDependencyGraph, source_index: u32, span: Lifted.Ast.Span(Lifted.Ast.TagPayloadEval)) Allocator.Error!void {
        for (self.input.ast.sliceTagPayloadEvalSpan(span)) |payload| {
            try self.collectExprDependencies(source_index, payload.value);
        }
    }

    fn collectTagPayloadAssemblyDependencies(self: *ProcedureDependencyGraph, source_index: u32, span: Lifted.Ast.Span(Lifted.Ast.TagPayloadAssembly)) Allocator.Error!void {
        for (self.input.ast.sliceTagPayloadAssemblySpan(span)) |payload| {
            try self.collectExprDependencies(source_index, payload.value);
        }
    }

    fn appendProcedureDependency(self: *ProcedureDependencyGraph, source_index: u32, proc: canonical.MirProcedureRef) Allocator.Error!void {
        const target_instance = self.proc_instance_map.get(proc) orelse {
            lambdaInvariant("lambda-solved procedure dependency referenced an unreserved procedure");
        };
        try self.edges[source_index].append(self.allocator, @intFromEnum(target_instance));
    }
};

const ProcedureSccBuilder = struct {
    allocator: Allocator,
    graph: *const ProcedureDependencyGraph,
    next_index: i32 = 0,
    indices: []i32,
    lowlinks: []i32,
    on_stack: []bool,
    stack: std.ArrayList(u32),
    proc_session_ids: []repr.RepresentationSolveSessionId,

    fn init(allocator: Allocator, graph: *const ProcedureDependencyGraph) Allocator.Error!ProcedureSccBuilder {
        const len = graph.edges.len;
        const indices = try allocator.alloc(i32, len);
        errdefer allocator.free(indices);
        const lowlinks = try allocator.alloc(i32, len);
        errdefer allocator.free(lowlinks);
        const on_stack = try allocator.alloc(bool, len);
        errdefer allocator.free(on_stack);
        const proc_session_ids = try allocator.alloc(repr.RepresentationSolveSessionId, len);
        errdefer allocator.free(proc_session_ids);

        @memset(indices, -1);
        @memset(lowlinks, -1);
        @memset(on_stack, false);

        return .{
            .allocator = allocator,
            .graph = graph,
            .indices = indices,
            .lowlinks = lowlinks,
            .on_stack = on_stack,
            .stack = .empty,
            .proc_session_ids = proc_session_ids,
        };
    }

    fn deinit(self: *ProcedureSccBuilder) void {
        self.stack.deinit(self.allocator);
        self.allocator.free(self.proc_session_ids);
        self.allocator.free(self.on_stack);
        self.allocator.free(self.lowlinks);
        self.allocator.free(self.indices);
    }

    fn build(self: *ProcedureSccBuilder, program: *Program) Allocator.Error![]repr.RepresentationSolveSessionId {
        for (self.graph.edges, 0..) |_, i| {
            if (self.indices[i] == -1) {
                try self.strongConnect(@intCast(i), program);
            }
        }
        const result = try self.allocator.dupe(repr.RepresentationSolveSessionId, self.proc_session_ids);
        return result;
    }

    fn strongConnect(self: *ProcedureSccBuilder, v: u32, program: *Program) Allocator.Error!void {
        self.indices[v] = self.next_index;
        self.lowlinks[v] = self.next_index;
        self.next_index += 1;
        try self.stack.append(self.allocator, v);
        self.on_stack[v] = true;

        for (self.graph.edges[v].items) |w| {
            if (self.indices[w] == -1) {
                try self.strongConnect(w, program);
                self.lowlinks[v] = @min(self.lowlinks[v], self.lowlinks[w]);
            } else if (self.on_stack[w]) {
                self.lowlinks[v] = @min(self.lowlinks[v], self.indices[w]);
            }
        }

        if (self.lowlinks[v] == self.indices[v]) {
            var members = std.ArrayList(repr.ProcRepresentationInstanceId){};
            errdefer members.deinit(self.allocator);

            while (true) {
                const w = self.stack.pop() orelse lambdaInvariant("lambda-solved SCC stack unexpectedly emptied");
                self.on_stack[w] = false;
                try members.append(self.allocator, @enumFromInt(w));
                if (w == v) break;
            }

            std.mem.sort(repr.ProcRepresentationInstanceId, members.items, {}, struct {
                fn lessThan(_: void, lhs: repr.ProcRepresentationInstanceId, rhs: repr.ProcRepresentationInstanceId) bool {
                    return @intFromEnum(lhs) < @intFromEnum(rhs);
                }
            }.lessThan);

            const session_id: repr.RepresentationSolveSessionId = @enumFromInt(@as(u32, @intCast(program.solve_sessions.items.len)));
            for (members.items) |member| {
                self.proc_session_ids[@intFromEnum(member)] = session_id;
            }

            const owned_members = try members.toOwnedSlice(self.allocator);
            errdefer self.allocator.free(owned_members);
            try program.solve_sessions.append(self.allocator, .{
                .members = owned_members,
                .representation_store = repr.RepresentationStore.init(self.allocator),
                .state = .building,
            });
        }
    }
};

fn createRepresentationSolveSessions(
    allocator: Allocator,
    input: *const Lifted.Lift.Program,
    proc_instance_map: *const std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId),
    program: *Program,
) Allocator.Error![]repr.RepresentationSolveSessionId {
    var graph = try ProcedureDependencyGraph.init(allocator, input, proc_instance_map);
    defer graph.deinit();

    var builder = try ProcedureSccBuilder.init(allocator, &graph);
    defer builder.deinit();

    return try builder.build(program);
}

pub fn run(allocator: Allocator, lifted: Lifted.Lift.Program) Allocator.Error!Program {
    var input = lifted;
    errdefer input.deinit();

    var program = Program.init(allocator);
    errdefer program.deinit();
    program.canonical_names = input.canonical_names;
    input.canonical_names = canonical.CanonicalNameStore.init(allocator);
    program.literal_pool = input.literal_pool;
    input.literal_pool = ids.ProgramLiteralPool.init(allocator);
    program.symbols = input.symbols;
    input.symbols = symbol_mod.Store.init(allocator);
    program.row_shapes = input.row_shapes;
    input.row_shapes = MonoRow.Store.init(allocator);

    try program.procs.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.solve_sessions.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.proc_instances.ensureTotalCapacity(allocator, input.procs.items.len);
    try program.value_stores.ensureTotalCapacity(allocator, input.procs.items.len);
    var proc_build_records = std.ArrayList(ProcBuildRecord).empty;
    defer proc_build_records.deinit(allocator);
    try proc_build_records.ensureTotalCapacity(allocator, input.procs.items.len);

    var type_importer = TypeImporter.init(allocator, &input.types, &program.types);
    defer type_importer.deinit();

    var proc_instance_map = std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId).init(allocator);
    defer proc_instance_map.deinit();
    try proc_instance_map.ensureTotalCapacity(@intCast(input.procs.items.len));

    for (input.procs.items, 0..) |proc, i| {
        const instance: repr.ProcRepresentationInstanceId = @enumFromInt(@as(u32, @intCast(i)));
        program.value_stores.appendAssumeCapacity(repr.ValueInfoStore.init(allocator));
        proc_instance_map.putAssumeCapacity(proc.proc, instance);
    }

    const proc_session_ids = try createRepresentationSolveSessions(allocator, &input, &proc_instance_map, &program);
    defer allocator.free(proc_session_ids);

    for (input.procs.items, 0..) |proc, i| {
        const instance: repr.ProcRepresentationInstanceId = @enumFromInt(@as(u32, @intCast(i)));
        const session_id = proc_session_ids[i];
        const session_index = @intFromEnum(session_id);
        const value_store_id: repr.ValueInfoStoreId = @enumFromInt(@as(u32, @intCast(i)));

        var solver = BodySolver{
            .allocator = allocator,
            .input = &input.ast,
            .output = &program.ast,
            .canonical_names = &program.canonical_names,
            .type_importer = &type_importer,
            .representation_store = &program.solve_sessions.items[session_index].representation_store,
            .value_store = &program.value_stores.items[i],
            .env = std.AutoHashMap(Ast.Symbol, repr.BindingInfoId).init(allocator),
            .expr_map = std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId).init(allocator),
            .instance = instance,
            .proc_instance_map = &proc_instance_map,
        };
        defer solver.deinit();

        const body = try solver.lowerDef(proc.body);
        const roots = solver.public_roots orelse lambdaInvariant("lambda-solved MIR built a procedure without public roots");
        proc_build_records.appendAssumeCapacity(.{
            .proc = proc.proc,
            .solve_session = session_id,
            .value_store = value_store_id,
            .public_roots = roots,
            .body = body,
            .representation_instance = instance,
        });
    }
    try appendCrossProcedureRepresentationEdges(&program, proc_build_records.items);
    try solveRepresentationSessions(&program, proc_build_records.items);
    try sealProcRepresentationInstances(&program, proc_build_records.items);
    try finalizeValueTransformBoundaries(&program);
    verifySealedLambdaSolvedProgram(&program);
    for (program.solve_sessions.items) |*session| {
        session.representation_store.verifySealed();
        session.state = .sealed;
    }
    try program.executable_synthetic_procs.appendSlice(allocator, input.executable_synthetic_procs.items);
    try program.root_procs.appendSlice(allocator, input.root_procs.items);
    try program.root_metadata.appendSlice(allocator, input.root_metadata.items);

    input.deinit();
    return program;
}

fn verifySealedLambdaSolvedProgram(program: *const Program) void {
    if (@import("builtin").mode != .Debug) return;
    for (program.value_stores.items) |value_store| {
        for (value_store.values.items) |value| {
            if (value.solved_class == null) {
                lambdaInvariant("lambda-solved sealed program contains a value without a solved representation class");
            }
        }
        for (value_store.call_sites.items) |call_site| {
            if (call_site.dispatch == null) {
                lambdaInvariant("lambda-solved sealed program contains an unresolved call-site dispatch");
            }
        }
    }
}

fn sealProcRepresentationInstances(
    program: *Program,
    records: []const ProcBuildRecord,
) Allocator.Error!void {
    for (records) |record| {
        const session_index = @intFromEnum(record.solve_session);
        const value_store_index = @intFromEnum(record.value_store);
        const executable_key = try repr.executableSpecializationKeyForProc(
            program.allocator,
            &program.canonical_names,
            &program.types,
            &program.solve_sessions.items[session_index].representation_store,
            &program.value_stores.items[value_store_index],
            record.proc,
            record.public_roots,
        );
        try program.proc_instances.append(program.allocator, .{
            .proc = record.proc,
            .executable_specialization_key = executable_key,
            .solve_session = record.solve_session,
            .value_store = record.value_store,
            .public_roots = record.public_roots,
        });
        try program.procs.append(program.allocator, .{
            .proc = record.proc,
            .body = record.body,
            .representation_instance = record.representation_instance,
        });
    }
}

fn appendCrossProcedureRepresentationEdges(program: *Program, records: []const ProcBuildRecord) Allocator.Error!void {
    for (records) |*record| {
        var linker = CrossProcedureRepresentationLinker{
            .program = program,
            .records = records,
            .representation_store = &program.solve_sessions.items[@intFromEnum(record.solve_session)].representation_store,
            .value_store = &program.value_stores.items[@intFromEnum(record.value_store)],
        };
        try linker.appendCallSiteEdges();
        try linker.appendProcValueEdges();
    }
}

const CrossProcedureRepresentationLinker = struct {
    program: *Program,
    records: []const ProcBuildRecord,
    representation_store: *repr.RepresentationStore,
    value_store: *const repr.ValueInfoStore,

    fn appendCallSiteEdges(self: *CrossProcedureRepresentationLinker) Allocator.Error!void {
        for (self.value_store.call_sites.items) |call_site| {
            const dispatch = call_site.dispatch orelse {
                const callee = call_site.callee orelse lambdaInvariant("lambda-solved unresolved call site has no callee");
                try self.appendPendingCallValueEdges(call_site, callee);
                continue;
            };
            switch (dispatch) {
                .call_proc => |target| try self.appendDirectCallEdges(call_site, target),
                .call_value_finite => |key| try self.appendFiniteCallValueEdges(call_site, key),
                .call_value_erased => {},
            }
        }
    }

    fn appendDirectCallEdges(
        self: *CrossProcedureRepresentationLinker,
        call_site: repr.CallSiteInfo,
        target_id: repr.ProcRepresentationInstanceId,
    ) Allocator.Error!void {
        const target = self.procInstance(target_id);
        const args = self.value_store.sliceValueSpan(call_site.args);
        const params = self.valueStoreFor(target).sliceValueSpan(target.public_roots.params);
        if (args.len != params.len) {
            lambdaInvariant("lambda-solved call_proc representation edge arity differs from target params");
        }
        for (args, params, 0..) |arg, param, i| {
            _ = try self.representation_store.appendRepresentationEdge(.{
                .from = .{ .local = self.valueRoot(arg) },
                .to = .{ .procedure_public = self.publicRootRef(target_id, target, param) },
                .kind = .{ .function_arg = @intCast(i) },
            });
        }
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .procedure_public = self.publicRootRef(target_id, target, target.public_roots.function_root) },
            .to = .{ .local = call_site.requested_fn_root },
            .kind = .value_alias,
        });
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .procedure_public = self.publicRootRef(target_id, target, target.public_roots.ret) },
            .to = .{ .local = self.valueRoot(call_site.result) },
            .kind = .function_return,
        });
    }

    fn appendFiniteCallValueEdges(
        self: *CrossProcedureRepresentationLinker,
        call_site: repr.CallSiteInfo,
        key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!void {
        const descriptor = self.representation_store.callableSetDescriptor(key) orelse {
            lambdaInvariant("lambda-solved finite call_value representation edge has no callable-set descriptor");
        };
        if (descriptor.members.len == 0) {
            lambdaInvariant("lambda-solved finite call_value representation edge reached empty callable-set descriptor");
        }
        for (descriptor.members) |member| {
            const target_id = self.procInstanceForSource(member.source_proc);
            try self.appendDirectCallEdges(call_site, target_id);
        }
    }

    fn appendPendingCallValueEdges(
        self: *CrossProcedureRepresentationLinker,
        call_site: repr.CallSiteInfo,
        callee: repr.ValueInfoId,
    ) Allocator.Error!void {
        const value_info = self.value_store.values.items[@intFromEnum(callee)];
        const callable = value_info.callable orelse lambdaInvariant("lambda-solved pending call_value has non-callable callee");
        switch (self.representation_store.callableEmissionPlan(callable.emission_plan)) {
            .finite => |key| try self.appendFiniteCallValueEdges(call_site, key),
            .already_erased,
            .erase_finite_set,
            .erase_proc_value,
            => {},
        }
    }

    fn appendProcValueEdges(self: *CrossProcedureRepresentationLinker) Allocator.Error!void {
        for (self.value_store.values.items, 0..) |value_info, raw_value| {
            const callable = value_info.callable orelse continue;
            const source = switch (callable.source) {
                .proc_value => |source| source,
                else => continue,
            };
            const target_id = self.procInstanceForSource(source.proc);
            const target = self.procInstance(target_id);
            const source_captures = source.captures;
            const target_captures = self.valueStoreFor(target).sliceValueSpan(target.public_roots.captures);
            if (source_captures.len != target_captures.len) {
                lambdaInvariant("lambda-solved proc_value representation edge capture arity differs from target captures");
            }
            _ = raw_value;
            for (source_captures, target_captures) |source_capture, target_capture| {
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = self.valueRoot(source_capture) },
                    .to = .{ .procedure_public = self.publicRootRef(target_id, target, target_capture) },
                    .kind = .value_move,
                });
            }
        }
    }

    fn publicRootRef(
        self: *CrossProcedureRepresentationLinker,
        target_id: repr.ProcRepresentationInstanceId,
        target: *const ProcBuildRecord,
        value: repr.ValueInfoId,
    ) repr.ProcPublicRootRef {
        return .{
            .instance = target_id,
            .value = value,
            .rep_root = self.valueStoreFor(target).values.items[@intFromEnum(value)].root,
        };
    }

    fn procInstanceForSource(
        self: *CrossProcedureRepresentationLinker,
        proc: canonical.MirProcedureRef,
    ) repr.ProcRepresentationInstanceId {
        for (self.records, 0..) |record, raw| {
            if (canonical.mirProcedureRefEql(record.proc, proc)) {
                return @enumFromInt(@as(u32, @intCast(raw)));
            }
        }
        lambdaInvariant("lambda-solved cross-procedure representation edge referenced missing procedure instance");
    }

    fn procInstance(
        self: *CrossProcedureRepresentationLinker,
        id: repr.ProcRepresentationInstanceId,
    ) *const ProcBuildRecord {
        const index = @intFromEnum(id);
        if (index >= self.records.len) {
            lambdaInvariant("lambda-solved cross-procedure representation edge referenced out-of-range procedure instance");
        }
        return &self.records[index];
    }

    fn valueStoreFor(
        self: *CrossProcedureRepresentationLinker,
        instance: *const ProcBuildRecord,
    ) *const repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(instance.value_store)];
    }

    fn valueRoot(self: *CrossProcedureRepresentationLinker, value: repr.ValueInfoId) repr.RepRootId {
        return self.value_store.values.items[@intFromEnum(value)].root;
    }
};

fn solveRepresentationSessions(
    program: *Program,
    records: []const ProcBuildRecord,
) Allocator.Error!void {
    for (program.solve_sessions.items, 0..) |*session, raw_session| {
        session.state = .solving;
        {
            var solver = RepresentationClassSolver{
                .allocator = program.allocator,
                .program = program,
                .records = records,
                .session_id = @enumFromInt(@as(u32, @intCast(raw_session))),
                .session = session,
                .parents = &.{},
                .ranks = &.{},
                .classes = std.AutoHashMap(u32, repr.RepresentationClassId).init(program.allocator),
            };
            defer solver.deinit();
            try solver.solve();
        }
    }
}

const RepresentationClassSolver = struct {
    allocator: Allocator,
    program: *Program,
    records: []const ProcBuildRecord,
    session_id: repr.RepresentationSolveSessionId,
    session: *repr.RepresentationSolveSession,
    parents: []u32,
    ranks: []u8,
    classes: std.AutoHashMap(u32, repr.RepresentationClassId),

    fn deinit(self: *RepresentationClassSolver) void {
        self.classes.deinit();
        if (self.ranks.len > 0) self.allocator.free(self.ranks);
        if (self.parents.len > 0) self.allocator.free(self.parents);
    }

    fn solve(self: *RepresentationClassSolver) Allocator.Error!void {
        try self.initUnionFind();
        for (self.session.representation_store.representation_edges.items) |edge| {
            if (!self.edgeUnionsValueFlow(edge)) continue;
            const from = self.endpointRootInSession(edge.from) orelse continue;
            const to = self.endpointRootInSession(edge.to) orelse continue;
            self.unionRoots(from, to);
        }
        try self.assignValueClasses();
    }

    fn initUnionFind(self: *RepresentationClassSolver) Allocator.Error!void {
        const len = self.session.representation_store.roots_len;
        self.parents = if (len == 0) &.{} else try self.allocator.alloc(u32, len);
        errdefer if (self.parents.len > 0) self.allocator.free(self.parents);
        self.ranks = if (len == 0) &.{} else try self.allocator.alloc(u8, len);
        errdefer if (self.ranks.len > 0) self.allocator.free(self.ranks);
        for (self.parents, 0..) |*parent, i| {
            parent.* = @intCast(i);
        }
        @memset(self.ranks, 0);
    }

    fn edgeUnionsValueFlow(self: *RepresentationClassSolver, edge: repr.RepresentationEdge) bool {
        return switch (edge.kind) {
            .value_alias,
            .value_move,
            .branch_join,
            .loop_phi,
            .mutable_version,
            => true,
            .function_arg,
            .function_return,
            => !self.edgeTouchesRequestedFunctionRoot(edge),
            .function_callable,
            .record_field,
            .tuple_elem,
            .tag_payload,
            .list_elem,
            .box_payload,
            .nominal_backing,
            => false,
        };
    }

    fn edgeTouchesRequestedFunctionRoot(self: *RepresentationClassSolver, edge: repr.RepresentationEdge) bool {
        if (self.endpointIsRequestedFunctionRoot(edge.from)) return true;
        return self.endpointIsRequestedFunctionRoot(edge.to);
    }

    fn endpointIsRequestedFunctionRoot(self: *RepresentationClassSolver, endpoint: repr.RepresentationEndpoint) bool {
        const root = self.endpointRootInSession(endpoint) orelse return false;
        return switch (self.session.representation_store.rootKind(root)) {
            .call_value_requested_fn,
            .call_proc_requested_fn,
            => true,
            else => false,
        };
    }

    fn endpointRootInSession(self: *RepresentationClassSolver, endpoint: repr.RepresentationEndpoint) ?repr.RepRootId {
        return switch (endpoint) {
            .local => |root| root,
            .procedure_public => |public| blk: {
                const record = self.recordForInstance(public.instance);
                if (record.solve_session != self.session_id) break :blk null;
                break :blk public.rep_root;
            },
        };
    }

    fn recordForInstance(
        self: *RepresentationClassSolver,
        instance: repr.ProcRepresentationInstanceId,
    ) *const ProcBuildRecord {
        const index = @intFromEnum(instance);
        if (index >= self.records.len) {
            lambdaInvariant("lambda-solved representation solver referenced out-of-range procedure instance");
        }
        return &self.records[index];
    }

    fn unionRoots(self: *RepresentationClassSolver, a: repr.RepRootId, b: repr.RepRootId) void {
        const a_index = @intFromEnum(a);
        const b_index = @intFromEnum(b);
        if (a_index >= self.parents.len or b_index >= self.parents.len) {
            lambdaInvariant("lambda-solved representation solver reached an out-of-range root");
        }
        const a_rep = self.find(a_index);
        const b_rep = self.find(b_index);
        if (a_rep == b_rep) return;
        if (self.ranks[a_rep] < self.ranks[b_rep]) {
            self.parents[a_rep] = b_rep;
        } else if (self.ranks[a_rep] > self.ranks[b_rep]) {
            self.parents[b_rep] = a_rep;
        } else {
            self.parents[b_rep] = a_rep;
            self.ranks[a_rep] += 1;
        }
    }

    fn find(self: *RepresentationClassSolver, index: u32) u32 {
        var current = index;
        while (self.parents[current] != current) {
            current = self.parents[current];
        }
        const root = current;
        current = index;
        while (self.parents[current] != current) {
            const next = self.parents[current];
            self.parents[current] = root;
            current = next;
        }
        return root;
    }

    fn assignValueClasses(self: *RepresentationClassSolver) Allocator.Error!void {
        self.session.representation_store.classes_len = 0;
        for (self.session.members) |member| {
            const record = self.recordForInstance(member);
            const value_store = &self.program.value_stores.items[@intFromEnum(record.value_store)];
            for (value_store.values.items) |*value| {
                value.solved_class = try self.classForRoot(value.root);
            }
        }
    }

    fn classForRoot(
        self: *RepresentationClassSolver,
        root: repr.RepRootId,
    ) Allocator.Error!repr.RepresentationClassId {
        const root_index = @intFromEnum(root);
        if (root_index >= self.parents.len) {
            lambdaInvariant("lambda-solved representation solver assigned class for out-of-range root");
        }
        const representative = self.find(root_index);
        if (self.classes.get(representative)) |existing| return existing;
        const class = self.session.representation_store.reserveClass();
        try self.classes.put(representative, class);
        return class;
    }
};

fn finalizeValueTransformBoundaries(program: *Program) Allocator.Error!void {
    for (program.proc_instances.items, 0..) |*instance, raw_instance| {
        var finalizer = ValueTransformFinalizer{
            .allocator = program.allocator,
            .program = program,
            .instance_id = @enumFromInt(@as(u32, @intCast(raw_instance))),
            .instance = instance,
        };
        try finalizer.finalizeCallSites();
        try finalizer.finalizeCallableConstructions();
        try finalizer.finalizeProcValueErasePlans();
        try finalizer.finalizeJoins();
        try finalizer.finalizeReturns();
    }
}

const ValueTransformFinalizer = struct {
    allocator: Allocator,
    program: *Program,
    instance_id: repr.ProcRepresentationInstanceId,
    instance: *const repr.ProcRepresentationInstance,

    fn finalizeCallSites(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.call_sites.items, 0..) |*call_site, raw_call_site| {
            const call_site_id: repr.CallSiteInfoId = @enumFromInt(@as(u32, @intCast(raw_call_site)));
            const dispatch = call_site.dispatch orelse {
                const callee = call_site.callee orelse lambdaInvariant("lambda-solved unresolved call site has no callee");
                try self.finalizePendingCallValue(call_site_id, call_site, callee);
                continue;
            };
            switch (dispatch) {
                .call_proc => |target| try self.finalizeCallProc(call_site_id, call_site, target),
                .call_value_finite => |key| try self.finalizeCallValueFinite(call_site_id, call_site, key),
                .call_value_erased => |sig_key| try self.finalizeCallValueErased(call_site_id, call_site, sig_key),
            }
        }
    }

    fn finalizeJoins(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.joins.items, 0..) |*join, raw_join| {
            if (!join.input_transforms.isEmpty()) {
                lambdaInvariant("lambda-solved value transform finalization reached an already-finalized join");
            }
            const join_id: repr.JoinInfoId = @enumFromInt(@as(u32, @intCast(raw_join)));
            const inputs = value_store.sliceJoinInputSpan(join.inputs);
            const boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, inputs.len);
            defer self.allocator.free(boundaries);

            const result_to = try self.localEndpoint(join.result);
            for (inputs, 0..) |input, i| {
                const from = try self.localEndpoint(input.value);
                const kind = self.joinBoundaryKind(join_id, input.source);
                const transform = try self.appendExistingValueTransform(kind, from, result_to);
                boundaries[i] = try self.representationStore().appendValueTransformBoundary(.{
                    .kind = kind,
                    .from_value = input.value,
                    .to_value = join.result,
                    .from_endpoint = from,
                    .to_endpoint = result_to,
                    .transform = transform,
                });
            }

            join.input_transforms = try self.valueStore().addValueTransformBoundarySpan(boundaries);
        }
    }

    fn finalizeReturns(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.returns.items, 0..) |*ret, raw_return| {
            if (ret.transform != null) {
                lambdaInvariant("lambda-solved value transform finalization reached an already-finalized return");
            }

            const from = try self.localEndpoint(ret.value);
            const to = try self.targetReturnEndpoint(self.instance_id, self.instance);
            const kind: repr.ValueTransformBoundaryKind = .{ .return_value = @enumFromInt(@as(u32, @intCast(raw_return))) };
            const transform = try self.appendExistingValueTransform(kind, from, to);
            ret.transform = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = ret.value,
                .to_value = self.instance.public_roots.ret,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
        }
    }

    fn joinBoundaryKind(
        self: *ValueTransformFinalizer,
        join_id: repr.JoinInfoId,
        source: repr.JoinInputSource,
    ) repr.ValueTransformBoundaryKind {
        _ = self;
        _ = join_id;
        return switch (source) {
            .if_branch => |if_branch| .{ .if_branch_result = .{
                .if_expr = if_branch.if_expr,
                .branch = if_branch.branch,
            } },
            .source_match_branch => |match_branch| .{ .source_match_branch_result = .{
                .match = match_branch.match,
                .branch = match_branch.branch,
                .alternative = match_branch.alternative,
            } },
            .loop_phi => |loop_phi| .{ .loop_phi = loop_phi },
        };
    }

    fn finalizePendingCallValue(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        call_site: *repr.CallSiteInfo,
        callee: repr.ValueInfoId,
    ) Allocator.Error!void {
        const dispatch = self.resolvedCallValueDispatch(callee);
        call_site.dispatch = dispatch;
        switch (dispatch) {
            .call_value_finite => |key| try self.finalizeCallValueFinite(call_site_id, call_site, key),
            .call_value_erased => |sig_key| try self.finalizeCallValueErased(call_site_id, call_site, sig_key),
            .call_proc => lambdaInvariant("lambda-solved pending call_value resolved to a non-call_value dispatch"),
        }
    }

    fn resolvedCallValueDispatch(
        self: *ValueTransformFinalizer,
        callee: repr.ValueInfoId,
    ) repr.CallSiteDispatch {
        const value_info = self.valueStore().values.items[@intFromEnum(callee)];
        const callable = value_info.callable orelse lambdaInvariant("lambda-solved call_value callee has no callable representation");
        return switch (self.representationStore().callableEmissionPlan(callable.emission_plan)) {
            .finite => |key| .{ .call_value_finite = key },
            .already_erased => |erased| .{ .call_value_erased = erased.sig_key },
            .erase_finite_set => |erase| .{ .call_value_erased = erase.adapter.erased_fn_sig_key },
            .erase_proc_value => |erase| .{ .call_value_erased = erase.erased_fn_sig_key },
        };
    }

    fn finalizeCallProc(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        call_site: *repr.CallSiteInfo,
        target_id: repr.ProcRepresentationInstanceId,
    ) Allocator.Error!void {
        self.verifyCallSiteUnfinalized(call_site);
        const args = self.valueStore().sliceValueSpan(call_site.args);
        const target_instance = self.procInstance(target_id);
        const target_params = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.params);
        if (args.len != target_params.len or args.len != target_instance.executable_specialization_key.exec_arg_tys.len) {
            lambdaInvariant("lambda-solved call_proc boundary finalization saw target arity mismatch");
        }

        const arg_boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, args.len);
        defer self.allocator.free(arg_boundaries);

        for (args, target_params, 0..) |arg, target_param, i| {
            const from = try self.localEndpoint(arg);
            const to = try self.targetParamEndpoint(target_id, target_instance, target_param, @intCast(i));
            const kind: repr.ValueTransformBoundaryKind = .{ .call_arg = .{
                .call = call_site_id,
                .arg_index = @intCast(i),
            } };
            const transform = try self.appendExistingValueTransform(kind, from, to);
            arg_boundaries[i] = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = arg,
                .to_value = target_param,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
        }

        const result_from = try self.targetReturnEndpoint(target_id, target_instance);
        const result_to = try self.localEndpoint(call_site.result);
        const result_kind: repr.ValueTransformBoundaryKind = .{ .call_result = call_site_id };
        const result_transform = try self.appendExistingValueTransform(result_kind, result_from, result_to);
        const result_boundary = try self.representationStore().appendValueTransformBoundary(.{
            .kind = result_kind,
            .from_value = target_instance.public_roots.ret,
            .to_value = call_site.result,
            .from_endpoint = result_from,
            .to_endpoint = result_to,
            .transform = result_transform,
        });

        call_site.arg_transforms = try self.valueStore().addValueTransformBoundarySpan(arg_boundaries);
        call_site.result_transform = result_boundary;
    }

    fn finalizeCallValueFinite(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        call_site: *repr.CallSiteInfo,
        callable_set_key: repr.CanonicalCallableSetKey,
    ) Allocator.Error!void {
        self.verifyCallSiteUnfinalized(call_site);
        const descriptor = self.representationStore().callableSetDescriptor(callable_set_key) orelse {
            lambdaInvariant("lambda-solved finite call boundary finalization referenced a missing callable-set descriptor");
        };
        if (descriptor.members.len == 0) {
            lambdaInvariant("lambda-solved finite call boundary finalization saw empty callable-set descriptor");
        }

        const branch_boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, descriptor.members.len);
        defer self.allocator.free(branch_boundaries);

        const result_to = try self.localEndpoint(call_site.result);
        for (descriptor.members, 0..) |member, i| {
            const target_id = self.procInstanceForSource(member.source_proc);
            const target_instance = self.procInstance(target_id);
            const result_from = try self.targetReturnEndpoint(target_id, target_instance);
            const kind: repr.ValueTransformBoundaryKind = .{ .callable_match_branch_result = .{
                .call = call_site_id,
                .member = .{
                    .callable_set_key = callable_set_key,
                    .member_index = member.member,
                },
            } };
            const result_transform = try self.appendExistingValueTransform(kind, result_from, result_to);
            branch_boundaries[i] = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = target_instance.public_roots.ret,
                .to_value = call_site.result,
                .from_endpoint = result_from,
                .to_endpoint = result_to,
                .transform = result_transform,
            });
        }

        call_site.branch_result_transforms = try self.valueStore().addValueTransformBoundarySpan(branch_boundaries);
    }

    fn finalizeCallValueErased(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        call_site: *repr.CallSiteInfo,
        sig_key: repr.ErasedFnSigKey,
    ) Allocator.Error!void {
        self.verifyCallSiteUnfinalized(call_site);
        const abi = self.representationStore().erased_fn_abis.abiFor(sig_key.abi) orelse {
            lambdaInvariant("lambda-solved erased call boundary finalization referenced an unpublished ABI");
        };
        const args = self.valueStore().sliceValueSpan(call_site.args);
        if (args.len != abi.arg_exec_keys.len or args.len != abi.fixed_arity) {
            lambdaInvariant("lambda-solved erased call boundary finalization saw ABI arity mismatch");
        }

        const arg_boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, args.len);
        defer self.allocator.free(arg_boundaries);

        for (args, 0..) |arg, i| {
            const from = try self.localEndpoint(arg);
            const to = self.rawArgEndpoint(call_site_id, @intCast(i), from.logical_ty, abi.arg_exec_keys[i]);
            const kind: repr.ValueTransformBoundaryKind = .{ .call_arg = .{
                .call = call_site_id,
                .arg_index = @intCast(i),
            } };
            const transform = try self.appendExistingValueTransform(kind, from, to);
            arg_boundaries[i] = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = arg,
                .to_value = arg,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
        }

        const result_to = try self.localEndpoint(call_site.result);
        const result_from = self.rawResultEndpoint(call_site_id, result_to.logical_ty, abi.ret_exec_key);
        const result_kind: repr.ValueTransformBoundaryKind = .{ .call_result = call_site_id };
        const result_transform = try self.appendExistingValueTransform(result_kind, result_from, result_to);
        const result_boundary = try self.representationStore().appendValueTransformBoundary(.{
            .kind = result_kind,
            .from_value = call_site.result,
            .to_value = call_site.result,
            .from_endpoint = result_from,
            .to_endpoint = result_to,
            .transform = result_transform,
        });

        call_site.arg_transforms = try self.valueStore().addValueTransformBoundarySpan(arg_boundaries);
        call_site.result_transform = result_boundary;
    }

    fn finalizeCallableConstructions(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.values.items, 0..) |value_info, raw_value| {
            const callable = value_info.callable orelse continue;
            const construction_id = callable.construction_plan orelse continue;
            const value_id: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
            try self.finalizeCallableConstruction(value_id, construction_id);
        }
    }

    fn finalizeCallableConstruction(
        self: *ValueTransformFinalizer,
        value_id: repr.ValueInfoId,
        construction_id: repr.CallableSetConstructionPlanId,
    ) Allocator.Error!void {
        const construction_snapshot = self.representationStore().callableConstructionPlan(construction_id);
        if (construction_snapshot.result != value_id) {
            lambdaInvariant("lambda-solved callable construction finalization reached a construction attached to a different value");
        }
        if (construction_snapshot.capture_transforms.len != 0) {
            lambdaInvariant("lambda-solved callable construction finalization reached already-finalized capture transforms");
        }

        const member = self.representationStore().callableSetMember(construction_snapshot.callable_set_key, construction_snapshot.selected_member) orelse {
            lambdaInvariant("lambda-solved callable construction finalization selected a missing callable-set member");
        };
        const target_id = self.procInstanceForSource(member.source_proc);
        const target_instance = self.procInstance(target_id);
        const target_captures = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.captures);
        const source_captures = construction_snapshot.capture_values;
        if (source_captures.len != target_captures.len or source_captures.len != member.capture_slots.len) {
            lambdaInvariant("lambda-solved callable construction finalization saw capture arity mismatch");
        }

        const boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, source_captures.len);
        defer self.allocator.free(boundaries);

        for (source_captures, target_captures, 0..) |source_capture, target_capture, i| {
            const slot = member.capture_slots[i];
            if (slot.slot != @as(u32, @intCast(i))) {
                lambdaInvariant("lambda-solved callable construction finalization saw non-canonical capture slot");
            }
            const from = try self.localEndpoint(source_capture);
            const to = try self.targetCaptureEndpoint(target_id, target_instance, target_capture, slot.slot, slot.exec_value_ty);
            const capture_boundary = try self.representationStore().reserveCaptureBoundary(.{
                .owner = .{ .callable_set_construction = .{
                    .construction = construction_id,
                    .selected_member = .{
                        .callable_set_key = construction_snapshot.callable_set_key,
                        .member_index = construction_snapshot.selected_member,
                    },
                } },
                .target_instance = target_id,
                .slot = slot.slot,
                .source_capture_value = source_capture,
                .target_capture_value = target_capture,
                .boundary = @enumFromInt(std.math.maxInt(u32)),
            });
            const kind: repr.ValueTransformBoundaryKind = .{ .capture_value = capture_boundary };
            const transform = try self.appendExistingValueTransform(kind, from, to);
            const boundary = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = source_capture,
                .to_value = target_capture,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
            self.representationStore().fillCaptureBoundary(capture_boundary, boundary);
            boundaries[i] = boundary;
        }

        try self.representationStore().setCallableConstructionCaptureTransforms(construction_id, boundaries);
    }

    fn finalizeProcValueErasePlans(self: *ValueTransformFinalizer) Allocator.Error!void {
        const value_store = self.valueStore();
        for (value_store.values.items, 0..) |value_info, raw_value| {
            const callable = value_info.callable orelse continue;
            const emission = self.representationStore().callableEmissionPlan(callable.emission_plan);
            const erase = switch (emission) {
                .erase_proc_value => |erase| erase,
                else => continue,
            };
            const value_id: repr.ValueInfoId = @enumFromInt(@as(u32, @intCast(raw_value)));
            try self.finalizeProcValueErasePlan(value_id, callable, callable.emission_plan, erase);
        }
    }

    fn finalizeProcValueErasePlan(
        self: *ValueTransformFinalizer,
        value_id: repr.ValueInfoId,
        callable: repr.CallableValueInfo,
        emission_plan_id: repr.CallableValueEmissionPlanId,
        erase: repr.ProcValueErasePlan,
    ) Allocator.Error!void {
        if (erase.source_value != value_id) {
            lambdaInvariant("lambda-solved proc-value erase finalization reached a plan attached to a different value");
        }
        if (erase.capture_transforms.len != 0) {
            lambdaInvariant("lambda-solved proc-value erase finalization reached already-finalized capture transforms");
        }

        const source = switch (callable.source) {
            .proc_value => |source| source,
            else => lambdaInvariant("lambda-solved proc-value erase finalization reached a non-proc callable source"),
        };
        if (!canonical.procedureCallableRefEql(source.proc.callable, erase.proc_value)) {
            lambdaInvariant("lambda-solved proc-value erase finalization source procedure differs from erase plan");
        }
        if (!repr.canonicalTypeKeyEql(source.fn_ty, erase.proc_value.source_fn_ty)) {
            lambdaInvariant("lambda-solved proc-value erase finalization source function type differs from erase plan");
        }

        const target_instance = self.procInstance(erase.target_instance);
        const target_captures = self.valueStoreFor(target_instance).sliceValueSpan(target_instance.public_roots.captures);
        const source_captures = source.captures;
        if (source_captures.len != target_captures.len or source_captures.len != erase.capture_slots.len) {
            lambdaInvariant("lambda-solved proc-value erase finalization saw capture arity mismatch");
        }

        const boundaries = try self.allocator.alloc(repr.ValueTransformBoundaryId, source_captures.len);
        defer self.allocator.free(boundaries);

        for (source_captures, target_captures, 0..) |source_capture, target_capture, i| {
            const slot = erase.capture_slots[i];
            if (slot.slot != @as(u32, @intCast(i))) {
                lambdaInvariant("lambda-solved proc-value erase finalization saw non-canonical capture slot");
            }
            const from = try self.localEndpoint(source_capture);
            const to = try self.targetCaptureEndpoint(erase.target_instance, target_instance, target_capture, slot.slot, slot.exec_value_ty);
            const capture_boundary = try self.representationStore().reserveCaptureBoundary(.{
                .owner = .{ .proc_value_erase = .{
                    .emission_plan = emission_plan_id,
                    .source_value = value_id,
                    .proc_value = erase.proc_value,
                    .erased_fn_sig_key = erase.erased_fn_sig_key,
                },
                } },
                .target_instance = erase.target_instance,
                .slot = slot.slot,
                .source_capture_value = source_capture,
                .target_capture_value = target_capture,
                .boundary = @enumFromInt(std.math.maxInt(u32)),
            });
            const kind: repr.ValueTransformBoundaryKind = .{ .capture_value = capture_boundary };
            const transform = try self.appendExistingValueTransform(kind, from, to);
            const boundary = try self.representationStore().appendValueTransformBoundary(.{
                .kind = kind,
                .from_value = source_capture,
                .to_value = target_capture,
                .from_endpoint = from,
                .to_endpoint = to,
                .transform = transform,
            });
            self.representationStore().fillCaptureBoundary(capture_boundary, boundary);
            boundaries[i] = boundary;
        }

        try self.representationStore().setProcValueEraseCaptureTransforms(emission_plan_id, boundaries);
    }

    fn verifyCallSiteUnfinalized(
        self: *ValueTransformFinalizer,
        call_site: *const repr.CallSiteInfo,
    ) void {
        _ = self;
        if (!call_site.arg_transforms.isEmpty() or
            !call_site.branch_result_transforms.isEmpty() or
            call_site.result_transform != null)
        {
            lambdaInvariant("lambda-solved value transform finalization reached an already-finalized call site");
        }
    }

    fn appendExistingValueTransform(
        self: *ValueTransformFinalizer,
        kind: repr.ValueTransformBoundaryKind,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const scope = try self.representationStore().appendTransformEndpointScope(.{
            .root_kind = kind,
            .root_from = from,
            .root_to = to,
        });
        return try self.planValueTransform(scope, from, to, &.{});
    }

    fn planValueTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        provenance: []const repr.BoxBoundaryId,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        if (!repr.canonicalExecValueTypeKeyEql(from.exec_ty.key, to.exec_ty.key)) {
            return try self.planNonIdentityValueTransform(scope, from, to, provenance);
        }
        return try self.appendSessionValueTransform(scope, from, to, .none, .identity);
    }

    fn planNonIdentityValueTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        provenance: []const repr.BoxBoundaryId,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const from_payload = self.sessionPayload(from.exec_ty.ty);
        const to_payload = self.sessionPayload(to.exec_ty.ty);
        return switch (from_payload) {
            .record => |source| switch (to_payload) {
                .record => |target| try self.planRecordTransform(scope, from, to, source, target, provenance),
                else => self.transformInvariant("lambda-solved record value transform target is not a record"),
            },
            .tuple => |source| switch (to_payload) {
                .tuple => |target| try self.planTupleTransform(scope, from, to, source, target, provenance),
                else => self.transformInvariant("lambda-solved tuple value transform target is not a tuple"),
            },
            .tag_union => |source| switch (to_payload) {
                .tag_union => |target| try self.planTagUnionTransform(scope, from, to, source, target, provenance),
                else => self.transformInvariant("lambda-solved tag-union value transform target is not a tag union"),
            },
            .nominal => |source| switch (to_payload) {
                .nominal => |target| try self.planNominalTransform(scope, from, to, source, target, provenance),
                else => self.transformInvariant("lambda-solved nominal value transform target is not nominal"),
            },
            .list => |source| switch (to_payload) {
                .list => |target| try self.planListTransform(scope, from, to, source, target, provenance),
                else => self.transformInvariant("lambda-solved list value transform target is not a list"),
            },
            .box => |source| switch (to_payload) {
                .box => |target| try self.planBoxTransform(scope, from, to, source, target, .box_to_box, provenance),
                else => self.transformInvariant("lambda-solved box value transform target is not a box"),
            },
            .callable_set => |source| switch (to_payload) {
                .erased_fn => |target| try self.planFiniteCallableToErasedTransform(scope, from, to, source, target, provenance),
                else => self.transformInvariant("lambda-solved finite callable value transform target is not erased callable"),
            },
            .erased_fn => |source| switch (to_payload) {
                .erased_fn => |target| try self.planAlreadyErasedCallableTransform(scope, from, to, source, target),
                else => self.transformInvariant("lambda-solved erased callable value transform target is not erased callable"),
            },
            .primitive,
            .recursive_ref,
            .pending,
            => self.transformInvariant("lambda-solved value transform has incompatible executable payloads"),
        };
    }

    fn appendSessionValueTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        provenance: checked_artifact.ValueTransformProvenance,
        op: repr.SessionExecutableValueTransformOp,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const id = try self.representationStore().appendSessionExecutableValueTransform(.{
            .scope = scope,
            .from = from,
            .to = to,
            .provenance = provenance,
            .op = op,
        });
        return .{ .session = id };
    }

    fn sessionPayload(
        self: *ValueTransformFinalizer,
        ref: repr.SessionExecutableTypePayloadRef,
    ) repr.SessionExecutableTypePayload {
        return self.representationStore().session_executable_type_payloads.get(ref.payload);
    }

    fn transformInvariant(
        self: *ValueTransformFinalizer,
        comptime message: []const u8,
    ) noreturn {
        _ = self;
        lambdaInvariant(message);
    }

    fn planRecordTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableRecordPayload,
        target: repr.SessionExecutableRecordPayload,
        provenance: []const repr.BoxBoundaryId,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const fields = try self.allocator.alloc(repr.SessionValueTransformRecordField, target.fields.len);
        defer self.allocator.free(fields);

        for (target.fields, 0..) |target_field, i| {
            const label = self.program.row_shapes.recordField(target_field.field).label;
            const source_field = self.recordFieldPayloadByLabel(source, label) orelse {
                lambdaInvariant("lambda-solved record transform target field has no source field");
            };
            if (source_field.field != target_field.field) {
                lambdaInvariant("lambda-solved record transform reached distinct source/target field ids");
            }

            const from_child = try self.transformChildEndpoint(
                scope,
                from,
                .from,
                .{ .record_field = source_field.field },
                try self.recordFieldLogicalType(from.logical_ty, source_field.field),
                source_field.ty,
                source_field.key,
            );
            const to_child = try self.transformChildEndpoint(
                scope,
                to,
                .to,
                .{ .record_field = target_field.field },
                try self.recordFieldLogicalType(to.logical_ty, target_field.field),
                target_field.ty,
                target_field.key,
            );
            fields[i] = .{
                .field = target_field.field,
                .transform = try self.planValueTransform(scope, from_child, to_child, provenance),
            };
        }

        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{
            .record = fields,
        });
    }

    fn planTupleTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: []const repr.SessionExecutableTupleElemPayload,
        target: []const repr.SessionExecutableTupleElemPayload,
        provenance: []const repr.BoxBoundaryId,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        if (source.len != target.len) {
            lambdaInvariant("lambda-solved tuple transform arity mismatch");
        }
        const elems = try self.allocator.alloc(repr.SessionValueTransformTupleElem, target.len);
        defer self.allocator.free(elems);

        for (target, 0..) |target_elem, i| {
            const source_elem = source[i];
            if (source_elem.index != target_elem.index) {
                lambdaInvariant("lambda-solved tuple transform source/target element index mismatch");
            }
            const from_child = try self.transformChildEndpoint(
                scope,
                from,
                .from,
                .{ .tuple_elem = source_elem.index },
                try self.tupleElemLogicalType(from.logical_ty, source_elem.index),
                source_elem.ty,
                source_elem.key,
            );
            const to_child = try self.transformChildEndpoint(
                scope,
                to,
                .to,
                .{ .tuple_elem = target_elem.index },
                try self.tupleElemLogicalType(to.logical_ty, target_elem.index),
                target_elem.ty,
                target_elem.key,
            );
            elems[i] = .{
                .index = target_elem.index,
                .transform = try self.planValueTransform(scope, from_child, to_child, provenance),
            };
        }

        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{
            .tuple = elems,
        });
    }

    fn planTagUnionTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableTagUnionPayload,
        target: repr.SessionExecutableTagUnionPayload,
        provenance: []const repr.BoxBoundaryId,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const cases = try self.allocator.alloc(repr.SessionValueTransformTagCase, source.variants.len);
        @memset(cases, .{
            .source_tag = @enumFromInt(0),
            .target_tag = @enumFromInt(0),
            .payloads = &.{},
        });
        defer {
            for (cases) |case| {
                if (case.payloads.len > 0) self.allocator.free(case.payloads);
            }
            self.allocator.free(cases);
        }

        for (source.variants, 0..) |source_variant, i| {
            const source_label = self.program.row_shapes.tag(source_variant.tag).label;
            const target_variant = self.tagVariantPayloadByLabel(target, source_label) orelse {
                lambdaInvariant("lambda-solved tag transform source tag has no target tag");
            };
            if (source_variant.payloads.len != target_variant.payloads.len) {
                lambdaInvariant("lambda-solved tag transform payload arity mismatch");
            }

            const payloads = try self.allocator.alloc(repr.SessionValueTransformTagPayloadEdge, target_variant.payloads.len);
            errdefer self.allocator.free(payloads);
            for (target_variant.payloads, 0..) |target_payload, payload_i| {
                const source_payload = source_variant.payloads[payload_i];
                const source_index = self.program.row_shapes.tagPayload(source_payload.payload).logical_index;
                const target_index = self.program.row_shapes.tagPayload(target_payload.payload).logical_index;
                if (source_index != target_index) {
                    lambdaInvariant("lambda-solved tag transform source/target payload index mismatch");
                }

                const from_child = try self.transformChildEndpoint(
                    scope,
                    from,
                    .from,
                    .{ .tag_payload = .{ .tag = source_variant.tag, .payload_index = source_index } },
                    try self.tagPayloadLogicalType(from.logical_ty, source_variant.tag, source_index),
                    source_payload.ty,
                    source_payload.key,
                );
                const to_child = try self.transformChildEndpoint(
                    scope,
                    to,
                    .to,
                    .{ .tag_payload = .{ .tag = target_variant.tag, .payload_index = target_index } },
                    try self.tagPayloadLogicalType(to.logical_ty, target_variant.tag, target_index),
                    target_payload.ty,
                    target_payload.key,
                );
                payloads[payload_i] = .{
                    .source_payload_index = source_index,
                    .target_payload_index = target_index,
                    .transform = try self.planValueTransform(scope, from_child, to_child, provenance),
                };
            }

            cases[i] = .{
                .source_tag = source_variant.tag,
                .target_tag = target_variant.tag,
                .payloads = payloads,
            };
        }

        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{
            .tag_union = cases,
        });
    }

    fn planNominalTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableNominalPayload,
        target: repr.SessionExecutableNominalPayload,
        provenance: []const repr.BoxBoundaryId,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        if (source.nominal.module_name != target.nominal.module_name or
            source.nominal.type_name != target.nominal.type_name)
        {
            lambdaInvariant("lambda-solved nominal transform source/target nominal mismatch");
        }
        const from_child = try self.transformChildEndpoint(
            scope,
            from,
            .from,
            .{ .nominal_backing = source.nominal },
            try self.nominalBackingLogicalType(from.logical_ty, source.nominal),
            source.backing,
            source.backing_key,
        );
        const to_child = try self.transformChildEndpoint(
            scope,
            to,
            .to,
            .{ .nominal_backing = target.nominal },
            try self.nominalBackingLogicalType(to.logical_ty, target.nominal),
            target.backing,
            target.backing_key,
        );
        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{ .nominal = .{
            .nominal = target.nominal,
            .backing = try self.planValueTransform(scope, from_child, to_child, provenance),
        } });
    }

    fn planListTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableTypePayloadChild,
        target: repr.SessionExecutableTypePayloadChild,
        provenance: []const repr.BoxBoundaryId,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const from_child = try self.transformChildEndpoint(
            scope,
            from,
            .from,
            .list_elem,
            try self.listElemLogicalType(from.logical_ty),
            source.ty,
            source.key,
        );
        const to_child = try self.transformChildEndpoint(
            scope,
            to,
            .to,
            .list_elem,
            try self.listElemLogicalType(to.logical_ty),
            target.ty,
            target.key,
        );
        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{ .list = .{
            .elem = try self.planValueTransform(scope, from_child, to_child, provenance),
        } });
    }

    fn planBoxTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableTypePayloadChild,
        target: repr.SessionExecutableTypePayloadChild,
        kind: checked_artifact.BoxPayloadTransformKind,
        provenance: []const repr.BoxBoundaryId,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const boundary = self.boxBoundaryForEndpoints(from, to) orelse blk: {
            if (provenance.len == 0) {
                lambdaInvariant("lambda-solved box transform has no explicit Box boundary provenance");
            }
            break :blk provenance[provenance.len - 1];
        };
        const child_provenance = try self.extendBoxProvenance(provenance, boundary);
        const child_provenance_owned = child_provenance.len != provenance.len;
        defer if (child_provenance_owned) self.allocator.free(child_provenance);

        const from_child = try self.transformChildEndpoint(
            scope,
            from,
            .from,
            .box_payload,
            try self.boxPayloadLogicalType(from.logical_ty),
            source.ty,
            source.key,
        );
        const to_child = try self.transformChildEndpoint(
            scope,
            to,
            .to,
            .box_payload,
            try self.boxPayloadLogicalType(to.logical_ty),
            target.ty,
            target.key,
        );
        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(child_provenance), .{ .box_payload = .{
            .boundary = boundary,
            .kind = kind,
            .payload = try self.planValueTransform(scope, from_child, to_child, child_provenance),
        } });
    }

    fn planFiniteCallableToErasedTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableCallableSetPayload,
        target: repr.SessionExecutableErasedFnPayload,
        provenance: []const repr.BoxBoundaryId,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        const plan = try self.selectedFiniteCallableErasurePlan(from, to, source, target, provenance);
        return try self.appendSessionValueTransform(scope, from, to, self.provenanceFor(provenance), .{
            .callable_to_erased = .{ .finite_value = plan },
        });
    }

    fn selectedFiniteCallableErasurePlan(
        self: *ValueTransformFinalizer,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableCallableSetPayload,
        target: repr.SessionExecutableErasedFnPayload,
        provenance: []const repr.BoxBoundaryId,
    ) Allocator.Error!repr.FiniteSetErasePlan {
        _ = to;
        _ = provenance;

        const value_id = switch (from.owner) {
            .local_value => |value| value,
            else => lambdaInvariant("lambda-solved finite callable erasure reached a non-local source without an assigned emission plan"),
        };
        const value_info = self.valueStore().values.items[@intFromEnum(value_id)];
        const callable = value_info.callable orelse {
            lambdaInvariant("lambda-solved finite callable erasure source value has no callable metadata");
        };
        const emission = self.representationStore().callableEmissionPlan(callable.emission_plan);
        const erase = switch (emission) {
            .erase_finite_set => |erase| erase,
            .finite => lambdaInvariant("lambda-solved finite callable erasure reached a callable occurrence before erased emission plans were assigned"),
            .erase_proc_value => lambdaInvariant("lambda-solved finite callable erasure reached a direct proc-value erase occurrence; its source endpoint should already be erased"),
            .already_erased => lambdaInvariant("lambda-solved finite callable erasure reached an already-erased callable occurrence"),
        };
        if (!repr.callableSetKeyEql(erase.adapter.callable_set_key, source.key)) {
            lambdaInvariant("lambda-solved finite callable erasure selected adapter for a different callable-set key");
        }
        if (!repr.erasedFnSigKeyEql(erase.adapter.erased_fn_sig_key, target.sig_key)) {
            lambdaInvariant("lambda-solved finite callable erasure selected adapter with a different erased signature");
        }
        if (!repr.captureShapeKeyEql(erase.adapter.capture_shape_key, target.capture_shape_key)) {
            lambdaInvariant("lambda-solved finite callable erasure selected adapter with a different capture shape");
        }
        return erase;
    }

    fn planAlreadyErasedCallableTransform(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
        source: repr.SessionExecutableErasedFnPayload,
        target: repr.SessionExecutableErasedFnPayload,
    ) Allocator.Error!checked_artifact.ExecutableValueTransformRef {
        if (!repr.erasedFnSigKeyEql(source.sig_key, target.sig_key)) {
            lambdaInvariant("lambda-solved already-erased callable transform changed erased signature");
        }
        return try self.appendSessionValueTransform(scope, from, to, .none, .{
            .already_erased_callable = .{ .sig_key = target.sig_key },
        });
    }

    fn provenanceFor(
        self: *ValueTransformFinalizer,
        provenance: []const repr.BoxBoundaryId,
    ) checked_artifact.ValueTransformProvenance {
        _ = self;
        return if (provenance.len == 0) .none else .{ .box_erasure = provenance };
    }

    fn extendBoxProvenance(
        self: *ValueTransformFinalizer,
        provenance: []const repr.BoxBoundaryId,
        boundary: repr.BoxBoundaryId,
    ) Allocator.Error![]const repr.BoxBoundaryId {
        for (provenance) |existing| {
            if (existing == boundary) return provenance;
        }
        const out = try self.allocator.alloc(repr.BoxBoundaryId, provenance.len + 1);
        @memcpy(out[0..provenance.len], provenance);
        out[provenance.len] = boundary;
        return out;
    }

    fn transformChildEndpoint(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        parent: repr.SessionExecutableValueEndpoint,
        side: repr.TransformEndpointSide,
        step: repr.TransformEndpointPathStep,
        logical_ty: Type.TypeVarId,
        payload: repr.SessionExecutableTypePayloadRef,
        key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const path = try self.transformChildPath(scope, parent, side, step);
        return .{
            .owner = .{ .transform_child = .{
                .scope = scope,
                .side = side,
                .path = path,
            } },
            .logical_ty = logical_ty,
            .exec_ty = .{
                .ty = payload,
                .key = key,
            },
        };
    }

    fn transformChildPath(
        self: *ValueTransformFinalizer,
        scope: repr.TransformEndpointScopeId,
        parent: repr.SessionExecutableValueEndpoint,
        side: repr.TransformEndpointSide,
        step: repr.TransformEndpointPathStep,
    ) Allocator.Error!repr.TransformEndpointPathId {
        const existing = switch (parent.owner) {
            .transform_child => |child| blk: {
                if (child.scope != scope or child.side != side) {
                    lambdaInvariant("lambda-solved transform child endpoint scope mismatch");
                }
                break :blk self.representationStore().transformEndpointPath(child.path);
            },
            else => &.{},
        };
        const steps = try self.allocator.alloc(repr.TransformEndpointPathStep, existing.len + 1);
        defer self.allocator.free(steps);
        @memcpy(steps[0..existing.len], existing);
        steps[existing.len] = step;
        return try self.representationStore().appendTransformEndpointPath(steps);
    }

    fn recordFieldPayloadByLabel(
        self: *ValueTransformFinalizer,
        record: repr.SessionExecutableRecordPayload,
        label: canonical.RecordFieldLabelId,
    ) ?repr.SessionExecutableRecordFieldPayload {
        for (record.fields) |field| {
            if (self.program.row_shapes.recordField(field.field).label == label) return field;
        }
        return null;
    }

    fn tagVariantPayloadByLabel(
        self: *ValueTransformFinalizer,
        tag_union: repr.SessionExecutableTagUnionPayload,
        label: canonical.TagLabelId,
    ) ?repr.SessionExecutableTagVariantPayload {
        for (tag_union.variants) |variant| {
            if (self.program.row_shapes.tag(variant.tag).label == label) return variant;
        }
        return null;
    }

    fn recordFieldLogicalType(
        self: *ValueTransformFinalizer,
        record_ty: Type.TypeVarId,
        field: MonoRow.RecordFieldId,
    ) Allocator.Error!Type.TypeVarId {
        const label = self.program.row_shapes.recordField(field).label;
        const root = self.program.types.unlinkConst(record_ty);
        const content = switch (self.program.types.getNode(root)) {
            .content => |content| content,
            else => lambdaInvariant("lambda-solved record transform endpoint has non-record logical type"),
        };
        const record = switch (content) {
            .record => |record| record,
            else => lambdaInvariant("lambda-solved record transform endpoint has non-record content"),
        };
        const fields = self.program.types.sliceFields(record.fields);
        for (fields) |candidate| {
            if (candidate.name == label) return candidate.ty;
        }
        lambdaInvariant("lambda-solved record transform field missing from logical type");
    }

    fn tupleElemLogicalType(
        self: *ValueTransformFinalizer,
        tuple_ty: Type.TypeVarId,
        elem: u32,
    ) Allocator.Error!Type.TypeVarId {
        const root = self.program.types.unlinkConst(tuple_ty);
        const content = switch (self.program.types.getNode(root)) {
            .content => |content| content,
            else => lambdaInvariant("lambda-solved tuple transform endpoint has non-tuple logical type"),
        };
        const tuple = switch (content) {
            .tuple => |tuple| self.program.types.sliceTypeVarSpan(tuple),
            else => lambdaInvariant("lambda-solved tuple transform endpoint has non-tuple content"),
        };
        const index: usize = @intCast(elem);
        if (index >= tuple.len) lambdaInvariant("lambda-solved tuple transform element index out of range");
        return tuple[index];
    }

    fn tagPayloadLogicalType(
        self: *ValueTransformFinalizer,
        tag_union_ty: Type.TypeVarId,
        tag: MonoRow.TagId,
        payload_index: u32,
    ) Allocator.Error!Type.TypeVarId {
        const label = self.program.row_shapes.tag(tag).label;
        const root = self.program.types.unlinkConst(tag_union_ty);
        const content = switch (self.program.types.getNode(root)) {
            .content => |content| content,
            else => lambdaInvariant("lambda-solved tag transform endpoint has non-tag logical type"),
        };
        const tag_union = switch (content) {
            .tag_union => |tag_union| tag_union,
            else => lambdaInvariant("lambda-solved tag transform endpoint has non-tag content"),
        };
        const tags = self.program.types.sliceTags(tag_union.tags);
        for (tags) |candidate| {
            if (candidate.name != label) continue;
            const args = self.program.types.sliceTypeVarSpan(candidate.args);
            const index: usize = @intCast(payload_index);
            if (index >= args.len) lambdaInvariant("lambda-solved tag transform payload index out of range");
            return args[index];
        }
        lambdaInvariant("lambda-solved tag transform tag missing from logical type");
    }

    fn nominalBackingLogicalType(
        self: *ValueTransformFinalizer,
        nominal_ty: Type.TypeVarId,
        nominal_key: canonical.NominalTypeKey,
    ) Allocator.Error!Type.TypeVarId {
        const root = self.program.types.unlinkConst(nominal_ty);
        return switch (self.program.types.getNode(root)) {
            .nominal => |nominal| blk: {
                if (nominal.nominal.module_name != nominal_key.module_name or
                    nominal.nominal.type_name != nominal_key.type_name)
                {
                    lambdaInvariant("lambda-solved nominal transform logical nominal mismatch");
                }
                break :blk nominal.backing;
            },
            else => lambdaInvariant("lambda-solved nominal transform endpoint has non-nominal logical type"),
        };
    }

    fn listElemLogicalType(
        self: *ValueTransformFinalizer,
        list_ty: Type.TypeVarId,
    ) Allocator.Error!Type.TypeVarId {
        const root = self.program.types.unlinkConst(list_ty);
        const content = switch (self.program.types.getNode(root)) {
            .content => |content| content,
            else => lambdaInvariant("lambda-solved list transform endpoint has non-list logical type"),
        };
        return switch (content) {
            .list => |elem| elem,
            else => lambdaInvariant("lambda-solved list transform endpoint has non-list content"),
        };
    }

    fn boxPayloadLogicalType(
        self: *ValueTransformFinalizer,
        box_ty: Type.TypeVarId,
    ) Allocator.Error!Type.TypeVarId {
        const root = self.program.types.unlinkConst(box_ty);
        const content = switch (self.program.types.getNode(root)) {
            .content => |content| content,
            else => lambdaInvariant("lambda-solved box transform endpoint has non-box logical type"),
        };
        return switch (content) {
            .box => |payload| payload,
            else => lambdaInvariant("lambda-solved box transform endpoint has non-box content"),
        };
    }

    fn boxBoundaryForEndpoints(
        self: *ValueTransformFinalizer,
        from: repr.SessionExecutableValueEndpoint,
        to: repr.SessionExecutableValueEndpoint,
    ) ?repr.BoxBoundaryId {
        if (self.boxBoundaryForEndpoint(from)) |boundary| return boundary;
        return self.boxBoundaryForEndpoint(to);
    }

    fn boxBoundaryForEndpoint(
        self: *ValueTransformFinalizer,
        endpoint: repr.SessionExecutableValueEndpoint,
    ) ?repr.BoxBoundaryId {
        return switch (endpoint.owner) {
            .local_value => |value| blk: {
                const info = self.valueStore().values.items[@intFromEnum(value)];
                break :blk if (info.boxed) |boxed| boxed.boundary else null;
            },
            else => null,
        };
    }

    fn localEndpoint(
        self: *ValueTransformFinalizer,
        value: repr.ValueInfoId,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const info = self.valueStore().values.items[@intFromEnum(value)];
        return .{
            .owner = .{ .local_value = value },
            .logical_ty = info.logical_ty,
            .exec_ty = try repr.sessionExecutableTypeEndpointForValue(
                self.allocator,
                &self.program.canonical_names,
                &self.program.row_shapes,
                &self.program.types,
                self.representationStore(),
                self.valueStore(),
                value,
            ),
        };
    }

    fn targetParamEndpoint(
        self: *ValueTransformFinalizer,
        target_id: repr.ProcRepresentationInstanceId,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value: repr.ValueInfoId,
        index: u32,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const target_value_store = self.valueStoreFor(target_instance);
        const target_info = target_value_store.values.items[@intFromEnum(target_value)];
        const exec_ty = try repr.sessionExecutableTypeEndpointForValueIntoStore(
            self.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStoreFor(target_instance),
            &self.representationStore().session_executable_type_payloads,
            target_value_store,
            target_value,
        );
        const arg_index: usize = @intCast(index);
        if (!repr.canonicalExecValueTypeKeyEql(exec_ty.key, target_instance.executable_specialization_key.exec_arg_tys[arg_index])) {
            lambdaInvariant("lambda-solved target procedure parameter endpoint key differs from target executable specialization");
        }
        return .{
            .owner = .{ .procedure_param = .{
                .instance = target_id,
                .index = index,
            } },
            .logical_ty = target_info.logical_ty,
            .exec_ty = exec_ty,
        };
    }

    fn targetReturnEndpoint(
        self: *ValueTransformFinalizer,
        target_id: repr.ProcRepresentationInstanceId,
        target_instance: *const repr.ProcRepresentationInstance,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const target_value_store = self.valueStoreFor(target_instance);
        const target_value = target_instance.public_roots.ret;
        const target_info = target_value_store.values.items[@intFromEnum(target_value)];
        const exec_ty = try repr.sessionExecutableTypeEndpointForValueIntoStore(
            self.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStoreFor(target_instance),
            &self.representationStore().session_executable_type_payloads,
            target_value_store,
            target_value,
        );
        if (!repr.canonicalExecValueTypeKeyEql(exec_ty.key, target_instance.executable_specialization_key.exec_ret_ty)) {
            lambdaInvariant("lambda-solved target procedure return endpoint key differs from target executable specialization");
        }
        return .{
            .owner = .{ .procedure_return = target_id },
            .logical_ty = target_info.logical_ty,
            .exec_ty = exec_ty,
        };
    }

    fn targetCaptureEndpoint(
        self: *ValueTransformFinalizer,
        target_id: repr.ProcRepresentationInstanceId,
        target_instance: *const repr.ProcRepresentationInstance,
        target_value: repr.ValueInfoId,
        slot: u32,
        expected_key: repr.CanonicalExecValueTypeKey,
    ) Allocator.Error!repr.SessionExecutableValueEndpoint {
        const target_value_store = self.valueStoreFor(target_instance);
        const target_info = target_value_store.values.items[@intFromEnum(target_value)];
        const exec_ty = try repr.sessionExecutableTypeEndpointForValueIntoStore(
            self.allocator,
            &self.program.canonical_names,
            &self.program.row_shapes,
            &self.program.types,
            self.representationStoreFor(target_instance),
            &self.representationStore().session_executable_type_payloads,
            target_value_store,
            target_value,
        );
        if (!repr.canonicalExecValueTypeKeyEql(exec_ty.key, expected_key)) {
            lambdaInvariant("lambda-solved target procedure capture endpoint key differs from callable-set member schema");
        }
        return .{
            .owner = .{ .procedure_capture = .{
                .instance = target_id,
                .slot = slot,
            } },
            .logical_ty = target_info.logical_ty,
            .exec_ty = exec_ty,
        };
    }

    fn rawArgEndpoint(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        index: u32,
        logical_ty: Type.TypeVarId,
        key: repr.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableValueEndpoint {
        return .{
            .owner = .{ .call_raw_arg = .{
                .call = call_site_id,
                .index = index,
            } },
            .logical_ty = logical_ty,
            .exec_ty = self.sessionEndpointForPublishedKey(key),
        };
    }

    fn rawResultEndpoint(
        self: *ValueTransformFinalizer,
        call_site_id: repr.CallSiteInfoId,
        logical_ty: Type.TypeVarId,
        key: repr.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableValueEndpoint {
        return .{
            .owner = .{ .call_raw_result = call_site_id },
            .logical_ty = logical_ty,
            .exec_ty = self.sessionEndpointForPublishedKey(key),
        };
    }

    fn sessionEndpointForPublishedKey(
        self: *ValueTransformFinalizer,
        key: repr.CanonicalExecValueTypeKey,
    ) repr.SessionExecutableTypeEndpoint {
        const payload = self.representationStore().session_executable_type_payloads.refForKey(key) orelse {
            lambdaInvariant("lambda-solved raw ABI endpoint key has no session executable type payload");
        };
        return .{
            .ty = payload,
            .key = key,
        };
    }

    fn procInstanceForSource(
        self: *ValueTransformFinalizer,
        proc: canonical.MirProcedureRef,
    ) repr.ProcRepresentationInstanceId {
        for (self.program.proc_instances.items, 0..) |instance, raw| {
            if (canonical.mirProcedureRefEql(instance.proc, proc)) {
                return @enumFromInt(@as(u32, @intCast(raw)));
            }
        }
        lambdaInvariant("lambda-solved finite call boundary finalization referenced missing member procedure");
    }

    fn procInstance(
        self: *ValueTransformFinalizer,
        id: repr.ProcRepresentationInstanceId,
    ) *const repr.ProcRepresentationInstance {
        const index = @intFromEnum(id);
        if (index >= self.program.proc_instances.items.len) {
            lambdaInvariant("lambda-solved value transform finalization referenced missing procedure instance");
        }
        return &self.program.proc_instances.items[index];
    }

    fn valueStore(self: *ValueTransformFinalizer) *repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(self.instance.value_store)];
    }

    fn valueStoreFor(self: *ValueTransformFinalizer, instance: *const repr.ProcRepresentationInstance) *repr.ValueInfoStore {
        return &self.program.value_stores.items[@intFromEnum(instance.value_store)];
    }

    fn representationStore(self: *ValueTransformFinalizer) *repr.RepresentationStore {
        return &self.program.solve_sessions.items[@intFromEnum(self.instance.solve_session)].representation_store;
    }

    fn representationStoreFor(self: *ValueTransformFinalizer, instance: *const repr.ProcRepresentationInstance) *repr.RepresentationStore {
        return &self.program.solve_sessions.items[@intFromEnum(instance.solve_session)].representation_store;
    }
};

const TypeImporter = struct {
    allocator: Allocator,
    input: *const Lifted.Type.Store,
    output: *Type.Store,
    active: std.AutoHashMap(Lifted.Type.TypeId, Type.TypeVarId),

    fn init(allocator: Allocator, input: *const Lifted.Type.Store, output: *Type.Store) TypeImporter {
        return .{
            .allocator = allocator,
            .input = input,
            .output = output,
            .active = std.AutoHashMap(Lifted.Type.TypeId, Type.TypeVarId).init(allocator),
        };
    }

    fn deinit(self: *TypeImporter) void {
        self.active.deinit();
    }

    fn importType(self: *TypeImporter, source: Lifted.Type.TypeId) Allocator.Error!Type.TypeVarId {
        switch (self.input.getTypePreservingNominal(source)) {
            .link => |next| return try self.importType(next),
            else => {},
        }

        if (self.active.get(source)) |existing| return existing;

        const target = try self.output.freshUnbd();
        try self.active.put(source, target);
        errdefer _ = self.active.remove(source);

        const node: Type.Node = switch (self.input.getTypePreservingNominal(source)) {
            .placeholder,
            .unbd,
            => lambdaInvariant("lambda-solved type import received unresolved lifted type"),
            .link => unreachable,
            .primitive => |prim| .{ .content = .{ .primitive = prim } },
            .func => |func| blk: {
                const args = try self.allocator.alloc(Type.TypeVarId, func.args.len);
                defer self.allocator.free(args);
                for (func.args, 0..) |arg, i| {
                    args[i] = try self.importType(arg);
                }
                const ret = try self.importType(func.ret);
                break :blk .{ .content = .{ .func = .{
                    .fixed_arity = @intCast(func.args.len),
                    .args = try self.output.addTypeVarSpan(args),
                    .ret = ret,
                    .callable = self.output.freshCallableVar(),
                } } };
            },
            .nominal => |nominal| blk: {
                const args = try self.allocator.alloc(Type.TypeVarId, nominal.args.len);
                defer self.allocator.free(args);
                for (nominal.args, 0..) |arg, i| {
                    args[i] = try self.importType(arg);
                }
                break :blk .{ .nominal = .{
                    .nominal = nominal.nominal,
                    .is_opaque = nominal.is_opaque,
                    .args = try self.output.addTypeVarSpan(args),
                    .backing = try self.importType(nominal.backing),
                } };
            },
            .list => |elem| .{ .content = .{ .list = try self.importType(elem) } },
            .box => |elem| .{ .content = .{ .box = try self.importType(elem) } },
            .tuple => |elems| blk: {
                const items = try self.allocator.alloc(Type.TypeVarId, elems.len);
                defer self.allocator.free(items);
                for (elems, 0..) |elem, i| {
                    items[i] = try self.importType(elem);
                }
                break :blk .{ .content = .{ .tuple = try self.output.addTypeVarSpan(items) } };
            },
            .tag_union => |tag_union| blk: {
                const tags = try self.allocator.alloc(Type.Tag, tag_union.tags.len);
                defer self.allocator.free(tags);
                for (tag_union.tags, 0..) |tag, i| {
                    const args = try self.allocator.alloc(Type.TypeVarId, tag.args.len);
                    defer self.allocator.free(args);
                    for (tag.args, 0..) |arg, j| {
                        args[j] = try self.importType(arg);
                    }
                    tags[i] = .{
                        .name = tag.name,
                        .args = try self.output.addTypeVarSpan(args),
                    };
                }
                break :blk .{ .content = .{ .tag_union = .{ .tags = try self.output.addTags(tags) } } };
            },
            .record => |record| blk: {
                const fields = try self.allocator.alloc(Type.Field, record.fields.len);
                defer self.allocator.free(fields);
                for (record.fields, 0..) |field, i| {
                    fields[i] = .{
                        .name = field.name,
                        .ty = try self.importType(field.ty),
                    };
                }
                break :blk .{ .content = .{ .record = .{ .fields = try self.output.addFields(fields) } } };
            },
        };

        self.output.setNode(target, node);
        _ = self.active.remove(source);
        return target;
    }
};

const BodySolver = struct {
    allocator: Allocator,
    input: *const Lifted.Ast.Store,
    output: *Ast.Store,
    canonical_names: *const canonical.CanonicalNameStore,
    type_importer: *TypeImporter,
    representation_store: *repr.RepresentationStore,
    value_store: *repr.ValueInfoStore,
    env: std.AutoHashMap(Ast.Symbol, repr.BindingInfoId),
    expr_map: std.AutoHashMap(Lifted.Ast.ExprId, Ast.ExprId),
    instance: repr.ProcRepresentationInstanceId,
    proc_instance_map: *const std.AutoHashMap(canonical.MirProcedureRef, repr.ProcRepresentationInstanceId),
    public_roots: ?repr.ProcPublicValueRoots = null,
    active_captures: ?repr.Span(repr.ValueInfoId) = null,
    next_source_match_id: u32 = 0,
    next_if_expr_id: u32 = 0,

    fn deinit(self: *BodySolver) void {
        self.expr_map.deinit();
        self.env.deinit();
    }

    fn lowerDef(self: *BodySolver, def_id: Lifted.Ast.DefId) Allocator.Error!Ast.DefId {
        const def = self.input.getDef(def_id);
        return try self.output.addDef(.{
            .proc = def.proc,
            .value = switch (def.value) {
                .fn_ => |fn_| blk: {
                    const lowered_args = try self.lowerParamSpan(fn_.args);
                    const capture_values = try self.lowerCaptureSlotRoots(fn_.captures);
                    const previous_captures = self.active_captures;
                    self.active_captures = capture_values;
                    defer self.active_captures = previous_captures;
                    const body = try self.lowerExpr(fn_.body);
                    const body_value = self.exprValue(body);
                    const function_root = self.representation_store.reserveRoot();
                    self.public_roots = .{
                        .params = lowered_args.values,
                        .ret = body_value,
                        .captures = capture_values,
                        .function_root = function_root,
                    };
                    break :blk .{ .fn_ = .{
                        .args = lowered_args.symbols,
                        .body = body,
                        .representation_instance = self.instance,
                    } };
                },
                .hosted_fn => |hosted| blk: {
                    const lowered_args = try self.lowerParamSpan(hosted.args);
                    const ret_ty = try self.type_importer.importType(hosted.ret_ty);
                    const ret = try self.newValue(ret_ty, .{});
                    self.public_roots = .{
                        .params = lowered_args.values,
                        .ret = ret,
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .hosted_fn = .{
                        .proc = hosted.proc,
                        .args = lowered_args.symbols,
                        .ret_ty = ret_ty,
                        .hosted = hosted.hosted,
                    } };
                },
                .val => |expr| blk: {
                    const body = try self.lowerExpr(expr);
                    self.public_roots = .{
                        .params = repr.Span(repr.ValueInfoId).empty(),
                        .ret = self.exprValue(body),
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .val = body };
                },
                .run => |run| blk: {
                    const body = try self.lowerExpr(run.body);
                    self.public_roots = .{
                        .params = repr.Span(repr.ValueInfoId).empty(),
                        .ret = self.exprValue(body),
                        .captures = repr.Span(repr.ValueInfoId).empty(),
                        .function_root = self.representation_store.reserveRoot(),
                    };
                    break :blk .{ .run = .{ .body = body } };
                },
            },
        });
    }

    const LoweredParams = struct {
        symbols: Ast.Span(Ast.TypedSymbol),
        values: repr.Span(repr.ValueInfoId),
    };

    fn lowerParamSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.TypedSymbol)) Allocator.Error!LoweredParams {
        const input_items = self.input.sliceTypedSymbolSpan(span);
        if (input_items.len == 0) return .{
            .symbols = Ast.Span(Ast.TypedSymbol).empty(),
            .values = repr.Span(repr.ValueInfoId).empty(),
        };
        const symbols = try self.allocator.alloc(Ast.TypedSymbol, input_items.len);
        defer self.allocator.free(symbols);
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |param, i| {
            const ty = try self.type_importer.importType(param.ty);
            const value = try self.newValue(ty, param.source_ty);
            const binding = try self.value_store.addBinding(.{
                .symbol = param.symbol,
                .value = value,
                .root = self.valueRoot(value),
            });
            try self.env.put(param.symbol, binding);
            symbols[i] = .{
                .ty = ty,
                .source_ty = param.source_ty,
                .symbol = param.symbol,
                .binding_info = binding,
            };
            values[i] = value;
        }
        return .{
            .symbols = try self.output.addTypedSymbolSpan(symbols),
            .values = try self.value_store.addValueSpan(values),
        };
    }

    fn lowerCaptureSlotRoots(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.CaptureSlot)) Allocator.Error!repr.Span(repr.ValueInfoId) {
        const input_items = self.input.sliceCaptureSlotSpan(span);
        if (input_items.len == 0) return repr.Span(repr.ValueInfoId).empty();
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |slot, i| {
            const ty = try self.type_importer.importType(slot.ty);
            const value = try self.newValue(ty, slot.source_ty);
            const binding = try self.value_store.addBinding(.{
                .symbol = slot.source_symbol,
                .value = value,
                .root = self.valueRoot(value),
            });
            try self.env.put(slot.source_symbol, binding);
            values[i] = value;
        }
        return try self.value_store.addValueSpan(values);
    }

    fn lowerExpr(self: *BodySolver, expr_id: Lifted.Ast.ExprId) Allocator.Error!Ast.ExprId {
        if (self.expr_map.get(expr_id)) |existing| return existing;

        const expr = self.input.getExpr(expr_id);
        const ty = try self.type_importer.importType(expr.ty);
        switch (expr.data) {
            .var_ => |symbol| {
                const binding_info = self.env.get(symbol) orelse lambdaInvariant("lambda-solved variable occurrence has no published binding info");
                const binding = self.value_store.bindings.items[@intFromEnum(binding_info)];
                const lowered = try self.output.addExpr(ty, expr.source_ty, binding.value, .{ .var_ = .{
                    .symbol = symbol,
                    .binding_info = binding_info,
                } });
                try self.expr_map.put(expr_id, lowered);
                return lowered;
            },
            .capture_ref => |slot| {
                const captures_span = self.active_captures orelse lambdaInvariant("lambda-solved capture_ref reached a procedure without capture roots");
                const captures = self.value_store.sliceValueSpan(captures_span);
                const capture_index: usize = @intCast(slot);
                if (capture_index >= captures.len) lambdaInvariant("lambda-solved capture_ref slot does not exist in procedure capture roots");
                const lowered = try self.output.addExpr(ty, expr.source_ty, captures[capture_index], .{ .capture_ref = slot });
                try self.expr_map.put(expr_id, lowered);
                return lowered;
            },
            else => {},
        }
        switch (expr.data) {
            .let_ => |let_| {
                const body = try self.lowerExpr(let_.body);
                const bind_ty = try self.type_importer.importType(let_.bind.ty);
                const binding = try self.value_store.addBinding(.{
                    .symbol = let_.bind.symbol,
                    .value = self.exprValue(body),
                    .root = self.valueRoot(self.exprValue(body)),
                });
                const previous = try self.env.fetchPut(let_.bind.symbol, binding);
                defer {
                    if (previous) |entry| {
                        self.env.put(let_.bind.symbol, entry.value) catch unreachable;
                    } else {
                        _ = self.env.remove(let_.bind.symbol);
                    }
                }
                const rest = try self.lowerExpr(let_.rest);
                const lowered = try self.output.addExpr(ty, expr.source_ty, self.exprValue(rest), .{ .let_ = .{
                    .bind = .{
                        .ty = bind_ty,
                        .source_ty = let_.bind.source_ty,
                        .symbol = let_.bind.symbol,
                        .binding_info = binding,
                    },
                    .body = body,
                    .rest = rest,
                } });
                try self.expr_map.put(expr_id, lowered);
                return lowered;
            },
            .block => |block| {
                const stmts = try self.lowerStmtSpan(block.stmts);
                const final_expr = try self.lowerExpr(block.final_expr);
                const lowered = try self.output.addExpr(ty, expr.source_ty, self.exprValue(final_expr), .{ .block = .{
                    .stmts = stmts,
                    .final_expr = final_expr,
                } });
                try self.expr_map.put(expr_id, lowered);
                return lowered;
            },
            .inspect => |child| {
                const lowered_child = try self.lowerExpr(child);
                const lowered = try self.output.addExpr(ty, expr.source_ty, self.exprValue(lowered_child), .{ .inspect = lowered_child });
                try self.expr_map.put(expr_id, lowered);
                return lowered;
            },
            else => {},
        }
        switch (expr.data) {
            .access => |access| {
                const record = try self.lowerExpr(access.record);
                const source = self.exprValue(record);
                if (self.value_store.values.items[@intFromEnum(source)].aggregate) |aggregate| {
                    const result = switch (aggregate) {
                        .record => |record_info| self.recordAggregateFieldValue(record_info, access.field),
                        else => lambdaInvariant("lambda-solved record access source had non-record aggregate metadata"),
                    };
                    const projection = try self.value_store.addProjection(.{
                        .source = source,
                        .result = result,
                        .root = self.valueRoot(result),
                        .kind = .{ .record_field = access.field },
                    });
                    const lowered = try self.output.addExpr(ty, expr.source_ty, result, .{ .access = .{
                        .record = record,
                        .field = access.field,
                        .projection_info = projection,
                    } });
                    try self.expr_map.put(expr_id, lowered);
                    return lowered;
                }
            },
            .tuple_access => |access| {
                const tuple = try self.lowerExpr(access.tuple);
                const source = self.exprValue(tuple);
                if (self.value_store.values.items[@intFromEnum(source)].aggregate) |aggregate| {
                    const result = switch (aggregate) {
                        .tuple => |tuple_info| self.tupleAggregateElemValue(tuple_info, access.elem_index),
                        else => lambdaInvariant("lambda-solved tuple access source had non-tuple aggregate metadata"),
                    };
                    const projection = try self.value_store.addProjection(.{
                        .source = source,
                        .result = result,
                        .root = self.valueRoot(result),
                        .kind = .{ .tuple_elem = access.elem_index },
                    });
                    const lowered = try self.output.addExpr(ty, expr.source_ty, result, .{ .tuple_access = .{
                        .tuple = tuple,
                        .elem_index = access.elem_index,
                        .projection_info = projection,
                    } });
                    try self.expr_map.put(expr_id, lowered);
                    return lowered;
                }
            },
            .tag_payload => |payload| {
                const tag_union = try self.lowerExpr(payload.tag_union);
                const source = self.exprValue(tag_union);
                if (self.value_store.values.items[@intFromEnum(source)].aggregate) |aggregate| {
                    const result = switch (aggregate) {
                        .tag => |tag_info| self.tagAggregatePayloadValue(tag_info, payload.payload),
                        else => lambdaInvariant("lambda-solved tag payload source had non-tag aggregate metadata"),
                    };
                    const projection = try self.value_store.addProjection(.{
                        .source = source,
                        .result = result,
                        .root = self.valueRoot(result),
                        .kind = .{ .tag_payload = payload.payload },
                    });
                    const lowered = try self.output.addExpr(ty, expr.source_ty, result, .{ .tag_payload = .{
                        .tag_union = tag_union,
                        .payload = payload.payload,
                        .projection_info = projection,
                    } });
                    try self.expr_map.put(expr_id, lowered);
                    return lowered;
                }
            },
            else => {},
        }

        const value = try self.newValue(ty, expr.source_ty);
        const lowered = try self.output.addExpr(ty, expr.source_ty, value, switch (expr.data) {
            .var_,
            .capture_ref,
            => unreachable,
            .int_lit => |literal| .{ .int_lit = literal },
            .frac_f32_lit => |literal| .{ .frac_f32_lit = literal },
            .frac_f64_lit => |literal| .{ .frac_f64_lit = literal },
            .dec_lit => |literal| .{ .dec_lit = literal },
            .bool_lit => |literal| .{ .bool_lit = literal },
            .str_lit => |literal| .{ .str_lit = literal },
            .const_instance => |const_instance| .{ .const_instance = const_instance },
            .tag => |tag| blk: {
                const eval_order = try self.lowerTagPayloadEvalSpan(tag.eval_order);
                const assembly_order = try self.lowerTagPayloadAssemblySpan(tag.assembly_order);
                try self.publishTagAggregate(value, tag.union_shape, tag.tag, assembly_order);
                break :blk .{ .tag = .{
                    .union_shape = tag.union_shape,
                    .tag = tag.tag,
                    .eval_order = eval_order,
                    .assembly_order = assembly_order,
                    .constructor_ty = try self.type_importer.importType(tag.constructor_ty),
                } };
            },
            .record => |record| blk: {
                const eval_order = try self.lowerRecordFieldEvalSpan(record.eval_order);
                const assembly_order = try self.lowerRecordFieldAssemblySpan(record.assembly_order);
                try self.publishRecordAggregate(value, record.shape, assembly_order);
                break :blk .{ .record = .{
                    .shape = record.shape,
                    .eval_order = eval_order,
                    .assembly_order = assembly_order,
                } };
            },
            .nominal_reinterpret => |backing| .{ .nominal_reinterpret = try self.lowerExpr(backing) },
            .access => |access| blk: {
                const record = try self.lowerExpr(access.record);
                const projection = try self.value_store.addProjection(.{
                    .source = self.exprValue(record),
                    .result = value,
                    .root = self.valueRoot(value),
                    .kind = .{ .record_field = access.field },
                });
                break :blk .{ .access = .{
                    .record = record,
                    .field = access.field,
                    .projection_info = projection,
                } };
            },
            .structural_eq => |eq| .{ .structural_eq = .{
                .lhs = try self.lowerExpr(eq.lhs),
                .rhs = try self.lowerExpr(eq.rhs),
            } },
            .bool_not => |child| .{ .bool_not = try self.lowerExpr(child) },
            .let_ => unreachable,
            .call_value => |call| blk: {
                const func = try self.lowerExpr(call.func);
                const callee_value = self.exprValue(func);
                const lowered_args = try self.lowerExprSpanWithValues(call.args);
                const requested_fn_ty = try self.type_importer.importType(call.requested_fn_ty);
                const requested_fn_root = self.representation_store.reserveRoot();
                const call_site = try self.value_store.addCallSite(.{
                    .callee = callee_value,
                    .args = lowered_args.values,
                    .result = value,
                    .requested_fn_root = requested_fn_root,
                    .requested_source_fn_ty = call.requested_source_fn_ty,
                    .dispatch = null,
                });
                try self.publishCallValueRequestedFunctionEdges(
                    call_site,
                    callee_value,
                    lowered_args.values,
                    value,
                    requested_fn_root,
                );
                break :blk .{ .call_value = .{
                    .func = func,
                    .args = lowered_args.exprs,
                    .requested_fn_ty = requested_fn_ty,
                    .requested_source_fn_ty = call.requested_source_fn_ty,
                    .call_site = call_site,
                } };
            },
            .call_proc => |call| blk: {
                const lowered_args = try self.lowerExprSpanWithValues(call.args);
                const requested_fn_ty = try self.type_importer.importType(call.requested_fn_ty);
                const requested_fn_root = self.representation_store.reserveRoot();
                const call_site = try self.value_store.addCallSite(.{
                    .callee = null,
                    .args = lowered_args.values,
                    .result = value,
                    .requested_fn_root = requested_fn_root,
                    .requested_source_fn_ty = call.requested_source_fn_ty,
                    .dispatch = .{ .call_proc = self.procRepresentationInstance(call.proc) },
                });
                try self.publishCallProcRequestedFunctionEdges(
                    call_site,
                    lowered_args.values,
                    value,
                    requested_fn_root,
                );
                break :blk .{ .call_proc = .{
                    .proc = call.proc,
                    .args = lowered_args.exprs,
                    .requested_fn_ty = requested_fn_ty,
                    .requested_source_fn_ty = call.requested_source_fn_ty,
                    .call_site = call_site,
                } };
            },
            .proc_value => |proc_value| blk: {
                const captures = try self.lowerCaptureArgSpanWithValues(proc_value.captures);
                const whole_function_root = self.representation_store.reserveRoot();
                try self.representation_store.publishRootKind(whole_function_root, .{ .proc_value_fn = .{
                    .instance = self.instance,
                    .value = value,
                } });
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = self.valueRoot(value) },
                    .to = .{ .local = whole_function_root },
                    .kind = .value_alias,
                });
                const callable = try self.representation_store.addSingletonProcValueCallable(
                    self.canonical_names,
                    self.type_importer.output,
                    self.value_store,
                    value,
                    whole_function_root,
                    proc_value.proc,
                    self.value_store.sliceValueSpan(captures.values),
                );
                self.value_store.values.items[@intFromEnum(value)].callable = callable;
                break :blk .{ .proc_value = .{
                    .proc = proc_value.proc,
                    .captures = captures.args,
                    .fn_ty = try self.type_importer.importType(proc_value.fn_ty),
                } };
            },
            .inspect => unreachable,
            .low_level => |low_level| try self.lowerLowLevel(value, expr.source_ty, low_level),
            .block => unreachable,
            .tuple => |items| blk: {
                const lowered_items = try self.lowerExprSpanWithValues(items);
                try self.publishTupleAggregate(value, lowered_items.values);
                break :blk .{ .tuple = lowered_items.exprs };
            },
            .tag_payload => |payload| blk: {
                const tag_union = try self.lowerExpr(payload.tag_union);
                const projection = try self.value_store.addProjection(.{
                    .source = self.exprValue(tag_union),
                    .result = value,
                    .root = self.valueRoot(value),
                    .kind = .{ .tag_payload = payload.payload },
                });
                break :blk .{ .tag_payload = .{
                    .tag_union = tag_union,
                    .payload = payload.payload,
                    .projection_info = projection,
                } };
            },
            .tuple_access => |access| blk: {
                const tuple = try self.lowerExpr(access.tuple);
                const projection = try self.value_store.addProjection(.{
                    .source = self.exprValue(tuple),
                    .result = value,
                    .root = self.valueRoot(value),
                    .kind = .{ .tuple_elem = access.elem_index },
                });
                break :blk .{ .tuple_access = .{
                    .tuple = tuple,
                    .elem_index = access.elem_index,
                    .projection_info = projection,
                } };
            },
            .list => |items| blk: {
                const lowered_items = try self.lowerExprSpanWithValues(items);
                try self.publishListAggregate(value, lowered_items.values);
                break :blk .{ .list = lowered_items.exprs };
            },
            .unit => .unit,
            .return_ => |child| blk: {
                const lowered_child = try self.lowerExpr(child);
                const return_info = try self.value_store.addReturn(.{
                    .value = self.exprValue(lowered_child),
                });
                break :blk .{ .return_ = .{
                    .expr = lowered_child,
                    .return_info = return_info,
                } };
            },
            .crash => |literal| .{ .crash = literal },
            .runtime_error => .runtime_error,
            .match_ => |match_| blk: {
                const match_id = self.freshSourceMatchId();
                const cond = try self.lowerExpr(match_.cond);
                const lowered_branches = try self.lowerBranchSpan(match_.branches);
                const branch_inputs = try self.joinInputsForBranches(match_id, lowered_branches);
                const join_info = try self.value_store.addJoin(.{
                    .result = value,
                    .inputs = branch_inputs,
                    .root = self.valueRoot(value),
                    .kind = .match_expr,
                });
                try self.publishJoinRepresentationEdges(value, branch_inputs);
                break :blk .{ .match_ = .{
                    .cond = cond,
                    .branches = lowered_branches,
                    .is_try_suffix = match_.is_try_suffix,
                    .join_info = join_info,
                } };
            },
            .if_ => |if_| blk: {
                const if_expr_id = self.freshIfExprId();
                const cond = try self.lowerExpr(if_.cond);
                const then_body = try self.lowerExpr(if_.then_body);
                const else_body = try self.lowerExpr(if_.else_body);
                var inputs = std.ArrayList(repr.JoinInputInfo).empty;
                defer inputs.deinit(self.allocator);
                if (self.exprReturnsValue(then_body)) {
                    try inputs.append(self.allocator, .{
                        .source = .{ .if_branch = .{
                            .if_expr = if_expr_id,
                            .branch = .then_,
                        } },
                        .value = self.exprValue(then_body),
                    });
                }
                if (self.exprReturnsValue(else_body)) {
                    try inputs.append(self.allocator, .{
                        .source = .{ .if_branch = .{
                            .if_expr = if_expr_id,
                            .branch = .else_,
                        } },
                        .value = self.exprValue(else_body),
                    });
                }
                const join_info = try self.value_store.addJoin(.{
                    .result = value,
                    .inputs = try self.value_store.addJoinInputSpan(inputs.items),
                    .root = self.valueRoot(value),
                    .kind = .if_expr,
                });
                try self.publishJoinRepresentationEdges(value, self.value_store.joins.items[@intFromEnum(join_info)].inputs);
                break :blk .{ .if_ = .{
                    .cond = cond,
                    .then_body = then_body,
                    .else_body = else_body,
                    .join_info = join_info,
                } };
            },
            .for_ => |for_| try self.lowerForExpr(value, for_),
        });
        try self.expr_map.put(expr_id, lowered);
        return lowered;
    }

    const SavedBinding = struct {
        symbol: Ast.Symbol,
        previous: ?repr.BindingInfoId,
    };

    fn lowerPatScoped(
        self: *BodySolver,
        pat_id: Lifted.Ast.PatId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.getPat(pat_id);
        const ty = try self.type_importer.importType(pat.ty);
        const value = try self.newValue(ty, pat.source_ty);
        return try self.lowerPatScopedWithValue(pat_id, ty, value, saved);
    }

    fn lowerPatScopedWithValue(
        self: *BodySolver,
        pat_id: Lifted.Ast.PatId,
        ty: Type.TypeVarId,
        value: repr.ValueInfoId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.PatId {
        const pat = self.input.getPat(pat_id);
        return try self.output.addPat(.{ .ty = ty, .source_ty = pat.source_ty, .value_info = value, .data = switch (pat.data) {
            .bool_lit => |literal| .{ .bool_lit = literal },
            .int_lit => |literal| .{ .int_lit = literal },
            .frac_f32_lit => |literal| .{ .frac_f32_lit = literal },
            .frac_f64_lit => |literal| .{ .frac_f64_lit = literal },
            .dec_lit => |literal| .{ .dec_lit = literal },
            .str_lit => |literal| .{ .str_lit = literal },
            .wildcard => .wildcard,
            .nominal => |child| .{ .nominal = try self.lowerPatScoped(child, saved) },
            .tuple => |items| .{ .tuple = try self.lowerPatSpanScoped(items, saved) },
            .record => |record| .{ .record = .{
                .shape = record.shape,
                .fields = try self.lowerRecordFieldPatternSpanScoped(record.fields, saved),
                .rest = if (record.rest) |rest| try self.lowerPatScoped(rest, saved) else null,
            } },
            .list => |list| .{ .list = .{
                .items = try self.lowerPatSpanScoped(list.items, saved),
                .rest = if (list.rest) |rest| .{
                    .index = rest.index,
                    .pattern = if (rest.pattern) |pattern| try self.lowerPatScoped(pattern, saved) else null,
                } else null,
            } },
            .as => |as| blk: {
                const binding = try self.bindPatternSymbol(as.symbol, value, saved);
                break :blk .{ .as = .{
                    .pattern = try self.lowerPatScopedWithValue(as.pattern, ty, value, saved),
                    .symbol = as.symbol,
                    .binding_info = binding,
                } };
            },
            .var_ => |symbol| blk: {
                const binding = try self.bindPatternSymbol(symbol, value, saved);
                break :blk .{ .var_ = .{
                    .symbol = symbol,
                    .binding_info = binding,
                } };
            },
            .tag => |tag| .{ .tag = .{
                .union_shape = tag.union_shape,
                .tag = tag.tag,
                .payloads = try self.lowerTagPayloadPatternSpan(tag.payloads, saved),
            } },
        } });
    }

    fn bindPatternSymbol(
        self: *BodySolver,
        symbol: Ast.Symbol,
        value: repr.ValueInfoId,
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!repr.BindingInfoId {
        const binding = try self.value_store.addBinding(.{
            .symbol = symbol,
            .value = value,
            .root = self.valueRoot(value),
        });
        const previous = try self.env.fetchPut(symbol, binding);
        try saved.append(self.allocator, .{
            .symbol = symbol,
            .previous = if (previous) |entry| entry.value else null,
        });
        return binding;
    }

    fn lowerPatSpanScoped(
        self: *BodySolver,
        span: Lifted.Ast.Span(Lifted.Ast.PatId),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.PatId) {
        const input_items = self.input.slicePatSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.PatId).empty();
        const output_items = try self.allocator.alloc(Ast.PatId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |item, i| {
            output_items[i] = try self.lowerPatScoped(item, saved);
        }
        return try self.output.addPatSpan(output_items);
    }

    fn lowerRecordFieldPatternSpanScoped(
        self: *BodySolver,
        span: Lifted.Ast.Span(Lifted.Ast.RecordFieldPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.RecordFieldPattern) {
        const input_items = self.input.sliceRecordFieldPatternSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldPattern).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldPattern, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .pattern = try self.lowerPatScoped(field.pattern, saved),
            };
        }
        return try self.output.addRecordFieldPatternSpan(output_items);
    }

    fn restoreBindings(self: *BodySolver, saved: *std.ArrayList(SavedBinding), start: usize) void {
        while (saved.items.len > start) {
            const binding = saved.pop().?;
            if (binding.previous) |previous| {
                self.env.put(binding.symbol, previous) catch unreachable;
            } else {
                _ = self.env.remove(binding.symbol);
            }
        }
    }

    fn lowerBranch(self: *BodySolver, branch_id: Lifted.Ast.BranchId) Allocator.Error!Ast.BranchId {
        const branch = self.input.getBranch(branch_id);
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const pat = try self.lowerPatScoped(branch.pat, &saved);
        defer self.restoreBindings(&saved, 0);
        const guard = if (branch.guard) |guard| try self.lowerExpr(guard) else null;
        const body = try self.lowerExpr(branch.body);
        return try self.output.addBranch(.{
            .pat = pat,
            .guard = guard,
            .body = body,
            .degenerate = branch.degenerate,
        });
    }

    fn lowerForExpr(self: *BodySolver, value: repr.ValueInfoId, for_: anytype) Allocator.Error!Ast.Expr.Data {
        _ = value;
        var saved = std.ArrayList(SavedBinding).empty;
        defer saved.deinit(self.allocator);
        const patt = try self.lowerPatScoped(for_.patt, &saved);
        defer self.restoreBindings(&saved, 0);
        return .{ .for_ = .{
            .patt = patt,
            .iterable = try self.lowerExpr(for_.iterable),
            .body = try self.lowerExpr(for_.body),
        } };
    }

    fn lowerStmt(self: *BodySolver, stmt_id: Lifted.Ast.StmtId) Allocator.Error!Ast.StmtId {
        const stmt = self.input.getStmt(stmt_id);
        return try self.output.addStmt(switch (stmt) {
            .decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                const bind_ty = try self.type_importer.importType(decl.bind.ty);
                const binding = try self.value_store.addBinding(.{
                    .symbol = decl.bind.symbol,
                    .value = self.exprValue(body),
                    .root = self.valueRoot(self.exprValue(body)),
                });
                try self.env.put(decl.bind.symbol, binding);
                break :blk .{ .decl = .{
                    .bind = .{
                        .ty = bind_ty,
                        .source_ty = decl.bind.source_ty,
                        .symbol = decl.bind.symbol,
                        .binding_info = binding,
                    },
                    .body = body,
                } };
            },
            .var_decl => |decl| blk: {
                const body = try self.lowerExpr(decl.body);
                const bind_ty = try self.type_importer.importType(decl.bind.ty);
                const binding = try self.value_store.addBinding(.{
                    .symbol = decl.bind.symbol,
                    .value = self.exprValue(body),
                    .root = self.valueRoot(self.exprValue(body)),
                });
                try self.env.put(decl.bind.symbol, binding);
                break :blk .{ .var_decl = .{
                    .bind = .{
                        .ty = bind_ty,
                        .source_ty = decl.bind.source_ty,
                        .symbol = decl.bind.symbol,
                        .binding_info = binding,
                    },
                    .body = body,
                } };
            },
            .reassign => |reassign| blk: {
                const body = try self.lowerExpr(reassign.body);
                const binding = self.env.get(reassign.target) orelse lambdaInvariant("lambda-solved reassignment target has no binding info");
                break :blk .{ .reassign = .{
                    .target = reassign.target,
                    .version = binding,
                    .body = body,
                } };
            },
            .expr => |expr| .{ .expr = try self.lowerExpr(expr) },
            .debug => |expr| .{ .debug = try self.lowerExpr(expr) },
            .expect => |expr| .{ .expect = try self.lowerExpr(expr) },
            .crash => |literal| .{ .crash = literal },
            .return_ => |expr| blk: {
                const lowered_child = try self.lowerExpr(expr);
                const return_info = try self.value_store.addReturn(.{
                    .value = self.exprValue(lowered_child),
                });
                break :blk .{ .return_ = .{
                    .expr = lowered_child,
                    .return_info = return_info,
                } };
            },
            .break_ => .break_,
            .for_ => |for_| blk: {
                var saved = std.ArrayList(SavedBinding).empty;
                defer saved.deinit(self.allocator);
                const patt = try self.lowerPatScoped(for_.patt, &saved);
                defer self.restoreBindings(&saved, 0);
                break :blk .{ .for_ = .{
                    .patt = patt,
                    .iterable = try self.lowerExpr(for_.iterable),
                    .body = try self.lowerExpr(for_.body),
                } };
            },
            .while_ => |while_| .{ .while_ = .{
                .cond = try self.lowerExpr(while_.cond),
                .body = try self.lowerExpr(while_.body),
            } },
        });
    }

    const LoweredExprSpan = struct {
        exprs: Ast.Span(Ast.ExprId),
        values: repr.Span(repr.ValueInfoId),
    };

    fn publishCallValueRequestedFunctionEdges(
        self: *BodySolver,
        call_site: repr.CallSiteInfoId,
        callee_value: repr.ValueInfoId,
        args: repr.Span(repr.ValueInfoId),
        result: repr.ValueInfoId,
        requested_fn_root: repr.RepRootId,
    ) Allocator.Error!void {
        try self.representation_store.publishRootKind(requested_fn_root, .{ .call_value_requested_fn = .{
            .instance = self.instance,
            .call_site = call_site,
        } });
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = self.valueRoot(callee_value) },
            .to = .{ .local = requested_fn_root },
            .kind = .value_alias,
        });
        try self.publishRequestedFunctionArgAndReturnEdges(args, result, requested_fn_root);
    }

    fn publishCallProcRequestedFunctionEdges(
        self: *BodySolver,
        call_site: repr.CallSiteInfoId,
        args: repr.Span(repr.ValueInfoId),
        result: repr.ValueInfoId,
        requested_fn_root: repr.RepRootId,
    ) Allocator.Error!void {
        try self.representation_store.publishRootKind(requested_fn_root, .{ .call_proc_requested_fn = .{
            .instance = self.instance,
            .call_site = call_site,
        } });
        try self.publishRequestedFunctionArgAndReturnEdges(args, result, requested_fn_root);
    }

    fn publishRequestedFunctionArgAndReturnEdges(
        self: *BodySolver,
        args: repr.Span(repr.ValueInfoId),
        result: repr.ValueInfoId,
        requested_fn_root: repr.RepRootId,
    ) Allocator.Error!void {
        for (self.value_store.sliceValueSpan(args), 0..) |arg, i| {
            _ = try self.representation_store.appendRepresentationEdge(.{
                .from = .{ .local = self.valueRoot(arg) },
                .to = .{ .local = requested_fn_root },
                .kind = .{ .function_arg = @intCast(i) },
            });
        }
        _ = try self.representation_store.appendRepresentationEdge(.{
            .from = .{ .local = requested_fn_root },
            .to = .{ .local = self.valueRoot(result) },
            .kind = .function_return,
        });
    }

    fn lowerExprSpanWithValues(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.ExprId)) Allocator.Error!LoweredExprSpan {
        const input_items = self.input.sliceExprSpan(span);
        if (input_items.len == 0) return .{
            .exprs = Ast.Span(Ast.ExprId).empty(),
            .values = repr.Span(repr.ValueInfoId).empty(),
        };
        const exprs = try self.allocator.alloc(Ast.ExprId, input_items.len);
        defer self.allocator.free(exprs);
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |expr, i| {
            exprs[i] = try self.lowerExpr(expr);
            values[i] = self.exprValue(exprs[i]);
        }
        return .{
            .exprs = try self.output.addExprSpan(exprs),
            .values = try self.value_store.addValueSpan(values),
        };
    }

    fn procRepresentationInstance(
        self: *const BodySolver,
        proc: canonical.MirProcedureRef,
    ) repr.ProcRepresentationInstanceId {
        return self.proc_instance_map.get(proc) orelse lambdaInvariant("lambda-solved call_proc target was not reserved before body lowering");
    }

    fn lowerExprSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.ExprId)) Allocator.Error!Ast.Span(Ast.ExprId) {
        return (try self.lowerExprSpanWithValues(span)).exprs;
    }

    fn lowerLowLevel(
        self: *BodySolver,
        result_value: repr.ValueInfoId,
        result_source_ty: canonical.CanonicalTypeKey,
        low_level: anytype,
    ) Allocator.Error!Ast.Expr.Data {
        const lowered_args = try self.lowerExprSpanWithValues(low_level.args);
        const source_constraint_ty = try self.type_importer.importType(low_level.source_constraint_ty);
        switch (low_level.op) {
            .box_box => {
                const arg_values = self.value_store.sliceValueSpan(lowered_args.values);
                if (arg_values.len != 1) lambdaInvariant("lambda-solved Box.box reached non-unary low-level expression");
                const payload_value = arg_values[0];
                const payload_info = self.value_store.values.items[@intFromEnum(payload_value)];
                const result_root = self.valueRoot(result_value);
                const payload_root = self.valueRoot(payload_value);
                const boundary = try self.representation_store.appendBoxBoundary(self.allocator, .{
                    .box_ty = result_source_ty,
                    .payload_source_ty = payload_info.source_ty,
                    .payload_boundary_ty = payload_info.source_ty,
                    .direction = .box,
                    .source_root = payload_root,
                    .boundary_root = result_root,
                    .payload_plan = .unchanged,
                });
                _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = boundary });
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = result_root },
                    .to = .{ .local = payload_root },
                    .kind = .box_payload,
                });
                self.value_store.values.items[@intFromEnum(result_value)].boxed = .{
                    .box_root = result_root,
                    .payload_root = payload_root,
                    .payload_value = payload_value,
                    .boundary = boundary,
                };
            },
            .box_unbox => {
                const arg_values = self.value_store.sliceValueSpan(lowered_args.values);
                if (arg_values.len != 1) lambdaInvariant("lambda-solved Box.unbox reached non-unary low-level expression");
                const boxed_value = arg_values[0];
                const boxed_info = self.value_store.values.items[@intFromEnum(boxed_value)];
                const boundary = try self.representation_store.appendBoxBoundary(self.allocator, .{
                    .box_ty = boxed_info.source_ty,
                    .payload_source_ty = result_source_ty,
                    .payload_boundary_ty = result_source_ty,
                    .direction = .unbox,
                    .source_root = self.valueRoot(boxed_value),
                    .boundary_root = self.valueRoot(result_value),
                    .payload_plan = .unchanged,
                });
                _ = try self.representation_store.appendRepresentationRequirement(.{ .require_box_erased = boundary });
                _ = try self.representation_store.appendRepresentationEdge(.{
                    .from = .{ .local = self.valueRoot(boxed_value) },
                    .to = .{ .local = self.valueRoot(result_value) },
                    .kind = .box_payload,
                });
            },
            else => {},
        }
        return .{ .low_level = .{
            .op = low_level.op,
            .args = lowered_args.exprs,
            .source_constraint_ty = source_constraint_ty,
        } };
    }

    fn lowerStmtSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.StmtId)) Allocator.Error!Ast.Span(Ast.StmtId) {
        const input_items = self.input.sliceStmtSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.StmtId).empty();
        const output_items = try self.allocator.alloc(Ast.StmtId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |stmt, i| {
            output_items[i] = try self.lowerStmt(stmt);
        }
        return try self.output.addStmtSpan(output_items);
    }

    fn lowerBranchSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.BranchId)) Allocator.Error!Ast.Span(Ast.BranchId) {
        const input_items = self.input.sliceBranchSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.BranchId).empty();
        const output_items = try self.allocator.alloc(Ast.BranchId, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |branch, i| {
            output_items[i] = try self.lowerBranch(branch);
        }
        return try self.output.addBranchSpan(output_items);
    }

    fn joinInputsForBranches(
        self: *BodySolver,
        match_id: repr.SourceMatchId,
        span: Ast.Span(Ast.BranchId),
    ) Allocator.Error!repr.Span(repr.JoinInputInfo) {
        if (span.len == 0) return repr.Span(repr.JoinInputInfo).empty();
        const branch_ids = self.output.branch_ids.items[span.start..][0..span.len];
        const inputs = try self.allocator.alloc(repr.JoinInputInfo, branch_ids.len);
        defer self.allocator.free(inputs);
        var input_len: usize = 0;
        for (branch_ids, 0..) |branch_id, i| {
            const body = self.output.branches.items[@intFromEnum(branch_id)].body;
            if (!self.exprReturnsValue(body)) continue;
            inputs[input_len] = .{
                .source = .{ .source_match_branch = .{
                    .match = match_id,
                    .branch = @enumFromInt(@as(u32, @intCast(i))),
                    .alternative = @enumFromInt(@as(u32, @intCast(i))),
                } },
                .value = self.exprValue(body),
            };
            input_len += 1;
        }
        return try self.value_store.addJoinInputSpan(inputs[0..input_len]);
    }

    fn publishJoinRepresentationEdges(
        self: *BodySolver,
        result: repr.ValueInfoId,
        inputs: repr.Span(repr.JoinInputInfo),
    ) Allocator.Error!void {
        const result_root = self.valueRoot(result);
        for (self.value_store.sliceJoinInputSpan(inputs)) |input| {
            _ = try self.representation_store.appendRepresentationEdge(.{
                .from = .{ .local = self.valueRoot(input.value) },
                .to = .{ .local = result_root },
                .kind = .branch_join,
            });
        }
    }

    fn exprReturnsValue(self: *const BodySolver, expr_id: Ast.ExprId) bool {
        return switch (self.output.exprs.items[@intFromEnum(expr_id)].data) {
            .return_,
            .crash,
            .runtime_error,
            => false,
            else => true,
        };
    }

    fn freshSourceMatchId(self: *BodySolver) repr.SourceMatchId {
        const id: repr.SourceMatchId = @enumFromInt(self.next_source_match_id);
        self.next_source_match_id += 1;
        return id;
    }

    fn freshIfExprId(self: *BodySolver) repr.IfExprId {
        const id: repr.IfExprId = @enumFromInt(self.next_if_expr_id);
        self.next_if_expr_id += 1;
        return id;
    }

    const LoweredCaptureArgs = struct {
        args: Ast.Span(Ast.CaptureArg),
        values: repr.Span(repr.ValueInfoId),
    };

    fn lowerCaptureArgSpanWithValues(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.CaptureArg)) Allocator.Error!LoweredCaptureArgs {
        const input_items = self.input.sliceCaptureArgSpan(span);
        if (input_items.len == 0) return .{
            .args = Ast.Span(Ast.CaptureArg).empty(),
            .values = repr.Span(repr.ValueInfoId).empty(),
        };
        const output_items = try self.allocator.alloc(Ast.CaptureArg, input_items.len);
        defer self.allocator.free(output_items);
        const values = try self.allocator.alloc(repr.ValueInfoId, input_items.len);
        defer self.allocator.free(values);
        for (input_items, 0..) |capture, i| {
            const expr = try self.lowerExpr(capture.expr);
            const value = self.exprValue(expr);
            output_items[i] = .{
                .slot = capture.slot,
                .value_info = value,
                .expr = expr,
            };
            values[i] = value;
        }
        return .{
            .args = try self.output.addCaptureArgSpan(output_items),
            .values = try self.value_store.addValueSpan(values),
        };
    }

    fn lowerRecordFieldEvalSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.RecordFieldEval)) Allocator.Error!Ast.Span(Ast.RecordFieldEval) {
        const input_items = self.input.sliceRecordFieldEvalSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldEval).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldEval, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.output.addRecordFieldEvalSpan(output_items);
    }

    fn lowerRecordFieldAssemblySpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.RecordFieldAssembly)) Allocator.Error!Ast.Span(Ast.RecordFieldAssembly) {
        const input_items = self.input.sliceRecordFieldAssemblySpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.RecordFieldAssembly).empty();
        const output_items = try self.allocator.alloc(Ast.RecordFieldAssembly, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |field, i| {
            output_items[i] = .{
                .field = field.field,
                .value = try self.lowerExpr(field.value),
            };
        }
        return try self.output.addRecordFieldAssemblySpan(output_items);
    }

    fn lowerTagPayloadEvalSpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.TagPayloadEval)) Allocator.Error!Ast.Span(Ast.TagPayloadEval) {
        const input_items = self.input.sliceTagPayloadEvalSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadEval).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadEval, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .value = try self.lowerExpr(payload.value),
            };
        }
        return try self.output.addTagPayloadEvalSpan(output_items);
    }

    fn lowerTagPayloadAssemblySpan(self: *BodySolver, span: Lifted.Ast.Span(Lifted.Ast.TagPayloadAssembly)) Allocator.Error!Ast.Span(Ast.TagPayloadAssembly) {
        const input_items = self.input.sliceTagPayloadAssemblySpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadAssembly).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadAssembly, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .value = try self.lowerExpr(payload.value),
            };
        }
        return try self.output.addTagPayloadAssemblySpan(output_items);
    }

    fn recordAggregateFieldValue(self: *const BodySolver, record: anytype, field: MonoRow.RecordFieldId) repr.ValueInfoId {
        _ = self;
        for (record.fields) |field_info| {
            if (field_info.field == field) return field_info.value;
        }
        lambdaInvariant("lambda-solved record aggregate projection referenced a missing field");
    }

    fn tupleAggregateElemValue(self: *const BodySolver, tuple: []const repr.ElemValueInfo, elem_index: u32) repr.ValueInfoId {
        _ = self;
        for (tuple) |elem| {
            if (elem.index == elem_index) return elem.value;
        }
        lambdaInvariant("lambda-solved tuple aggregate projection referenced a missing element");
    }

    fn tagAggregatePayloadValue(self: *const BodySolver, tag: anytype, payload: MonoRow.TagPayloadId) repr.ValueInfoId {
        _ = self;
        for (tag.payloads) |payload_info| {
            if (payload_info.payload == payload) return payload_info.value;
        }
        lambdaInvariant("lambda-solved tag aggregate projection referenced a missing payload");
    }

    fn publishRecordAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        shape: MonoRow.RecordShapeId,
        assembly_order: Ast.Span(Ast.RecordFieldAssembly),
    ) Allocator.Error!void {
        const assemblies = self.output.record_field_assemblies.items[assembly_order.start..][0..assembly_order.len];
        const fields = try self.allocator.alloc(repr.FieldValueInfo, assemblies.len);
        errdefer if (fields.len > 0) self.allocator.free(fields);

        for (assemblies, 0..) |field, i| {
            fields[i] = .{
                .field = field.field,
                .value = self.exprValue(field.value),
            };
        }
        try self.publishAggregate(value, .{ .record = .{
            .shape = shape,
            .fields = fields,
        } });
    }

    fn publishTupleAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        elems: repr.Span(repr.ValueInfoId),
    ) Allocator.Error!void {
        const elem_values = self.value_store.sliceValueSpan(elems);
        const infos = try self.allocator.alloc(repr.ElemValueInfo, elem_values.len);
        errdefer if (infos.len > 0) self.allocator.free(infos);

        for (elem_values, 0..) |elem, i| {
            infos[i] = .{
                .index = @intCast(i),
                .value = elem,
            };
        }
        try self.publishAggregate(value, .{ .tuple = infos });
    }

    fn publishTagAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        union_shape: MonoRow.TagUnionShapeId,
        tag_id: MonoRow.TagId,
        assembly_order: Ast.Span(Ast.TagPayloadAssembly),
    ) Allocator.Error!void {
        const assemblies = self.output.tag_payload_assemblies.items[assembly_order.start..][0..assembly_order.len];
        const payloads = try self.allocator.alloc(repr.TagPayloadValueInfo, assemblies.len);
        errdefer if (payloads.len > 0) self.allocator.free(payloads);

        for (assemblies, 0..) |payload, i| {
            payloads[i] = .{
                .payload = payload.payload,
                .value = self.exprValue(payload.value),
            };
        }
        try self.publishAggregate(value, .{ .tag = .{
            .union_shape = union_shape,
            .tag = tag_id,
            .payloads = payloads,
        } });
    }

    fn publishListAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        elems: repr.Span(repr.ValueInfoId),
    ) Allocator.Error!void {
        const elem_values = self.value_store.sliceValueSpan(elems);
        const owned_elems = try self.allocator.dupe(repr.ValueInfoId, elem_values);
        errdefer if (owned_elems.len > 0) self.allocator.free(owned_elems);

        try self.publishAggregate(value, .{ .list = .{
            .elem_root = self.representation_store.reserveRoot(),
            .elems = owned_elems,
        } });
    }

    fn publishAggregate(
        self: *BodySolver,
        value: repr.ValueInfoId,
        aggregate: repr.AggregateValueInfo,
    ) Allocator.Error!void {
        const value_info = &self.value_store.values.items[@intFromEnum(value)];
        if (value_info.aggregate != null) lambdaInvariant("lambda-solved value published aggregate metadata twice");
        value_info.aggregate = aggregate;
        try self.publishAggregateRepresentationEdges(value, aggregate);
    }

    fn publishAggregateRepresentationEdges(
        self: *BodySolver,
        value: repr.ValueInfoId,
        aggregate: repr.AggregateValueInfo,
    ) Allocator.Error!void {
        const root = self.valueRoot(value);
        switch (aggregate) {
            .record => |record| {
                for (record.fields) |field| {
                    _ = try self.representation_store.appendRepresentationEdge(.{
                        .from = .{ .local = root },
                        .to = .{ .local = self.valueRoot(field.value) },
                        .kind = .{ .record_field = field.field },
                    });
                }
            },
            .tuple => |tuple| {
                for (tuple) |elem| {
                    _ = try self.representation_store.appendRepresentationEdge(.{
                        .from = .{ .local = root },
                        .to = .{ .local = self.valueRoot(elem.value) },
                        .kind = .{ .tuple_elem = elem.index },
                    });
                }
            },
            .tag => |tag| {
                for (tag.payloads) |payload| {
                    _ = try self.representation_store.appendRepresentationEdge(.{
                        .from = .{ .local = root },
                        .to = .{ .local = self.valueRoot(payload.value) },
                        .kind = .{ .tag_payload = payload.payload },
                    });
                }
            },
            .list => |list| {
                for (list.elems) |elem| {
                    _ = try self.representation_store.appendRepresentationEdge(.{
                        .from = .{ .local = root },
                        .to = .{ .local = self.valueRoot(elem) },
                        .kind = .list_elem,
                    });
                }
            },
        }
    }

    fn lowerTagPayloadPatternSpan(
        self: *BodySolver,
        span: Lifted.Ast.Span(Lifted.Ast.TagPayloadPattern),
        saved: *std.ArrayList(SavedBinding),
    ) Allocator.Error!Ast.Span(Ast.TagPayloadPattern) {
        const input_items = self.input.sliceTagPayloadPatternSpan(span);
        if (input_items.len == 0) return Ast.Span(Ast.TagPayloadPattern).empty();
        const output_items = try self.allocator.alloc(Ast.TagPayloadPattern, input_items.len);
        defer self.allocator.free(output_items);
        for (input_items, 0..) |payload, i| {
            output_items[i] = .{
                .payload = payload.payload,
                .pattern = try self.lowerPatScoped(payload.pattern, saved),
            };
        }
        return try self.output.addTagPayloadPatternSpan(output_items);
    }

    fn newValue(
        self: *BodySolver,
        ty: Type.TypeVarId,
        source_ty: canonical.CanonicalTypeKey,
    ) Allocator.Error!repr.ValueInfoId {
        const root = self.representation_store.reserveRoot();
        const value = try self.value_store.addValue(.{
            .logical_ty = ty,
            .source_ty = source_ty,
            .root = root,
        });
        try self.representation_store.publishRootKind(root, .{ .local_value = .{
            .instance = self.instance,
            .value = value,
        } });
        return value;
    }

    fn exprValue(self: *const BodySolver, expr: Ast.ExprId) repr.ValueInfoId {
        return self.output.exprs.items[@intFromEnum(expr)].value_info;
    }

    fn valueRoot(self: *const BodySolver, value: repr.ValueInfoId) repr.RepRootId {
        return self.value_store.values.items[@intFromEnum(value)].root;
    }
};

fn lambdaInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) std.debug.panic(message, .{});
    unreachable;
}

test "lambda-solved program owns representation tables" {
    std.testing.refAllDecls(@This());
}
