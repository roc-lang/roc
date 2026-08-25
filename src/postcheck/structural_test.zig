//! Structural assertions for post-check stage boundaries.

const std = @import("std");
const check = @import("check");

const Common = @import("common.zig");
const Mono = @import("monotype/ast.zig");
const MonoType = @import("monotype/type.zig");
const Lifted = @import("monotype_lifted/ast.zig");
const LambdaSolved = @import("lambda_solved/ast.zig");
const LambdaSolvedType = @import("lambda_solved/type.zig");
const LambdaMono = @import("lambda_mono/ast.zig");
const LambdaMonoType = @import("lambda_mono/type.zig");
const LIR = @import("lir_core").LIR;
const names = check.CheckedNames;

fn unionFieldCount(comptime T: type) comptime_int {
    return @typeInfo(T).@"union".fields.len;
}

fn structFieldType(comptime T: type, comptime name: []const u8) type {
    inline for (@typeInfo(T).@"struct".fields) |field| {
        if (std.mem.eql(u8, field.name, name)) return field.type;
    }
    @compileError("missing struct field: " ++ name);
}

fn unionPayloadType(comptime T: type, comptime name: []const u8) type {
    inline for (@typeInfo(T).@"union".fields) |field| {
        if (std.mem.eql(u8, field.name, name)) return field.type;
    }
    @compileError("missing union field: " ++ name);
}

fn sourceSliceBetween(source: []const u8, start: []const u8, end: []const u8) []const u8 {
    const start_index = std.mem.find(u8, source, start) orelse @panic("missing source slice start marker");
    const after_start = source[start_index..];
    const end_index = std.mem.find(u8, after_start, end) orelse @panic("missing source slice end marker");
    return after_start[0..end_index];
}

fn expectContains(haystack: []const u8, needle: []const u8) error{TestUnexpectedResult}!void {
    try std.testing.expect(std.mem.find(u8, haystack, needle) != null);
}

fn expectNotContains(haystack: []const u8, needle: []const u8) error{TestUnexpectedResult}!void {
    try std.testing.expect(std.mem.find(u8, haystack, needle) == null);
}

test "Monotype has direct call and structural expression forms" {
    try std.testing.expect(@hasField(Mono.ExprData, "call_proc"));
    try std.testing.expect(@hasField(Mono.ExprData, "call_value"));
    try std.testing.expect(@hasField(Mono.ExprData, "structural_eq"));
    try std.testing.expect(@hasField(Mono.ExprData, "structural_hash"));
    try std.testing.expect(@hasField(Mono.ExprData, "loop_"));
}

test "Monotype types are closed checked types without row tails" {
    try std.testing.expect(@hasField(MonoType.Content, "record"));
    try std.testing.expect(@hasField(MonoType.Content, "tag_union"));
    try std.testing.expect(@hasField(MonoType.Content, "func"));
    try std.testing.expect(@hasField(MonoType.Content, "erased"));
    try std.testing.expect(@hasField(MonoType.Content, "zst"));

    try std.testing.expect(!@hasField(MonoType.Content, "record_unbound"));
    try std.testing.expect(!@hasField(MonoType.Content, "empty_record"));
    try std.testing.expect(!@hasField(MonoType.Content, "empty_tag_union"));
    try std.testing.expect(!@hasField(MonoType.Content, "row_var"));
    try std.testing.expect(!@hasField(MonoType.Content, "lambda_set"));
}

test "post-check row entries carry checked label ids until LIR indices" {
    try std.testing.expect(structFieldType(Mono.FieldExpr, "name") == names.RecordFieldNameId);
    try std.testing.expect(structFieldType(Mono.RecordDestruct, "name") == names.RecordFieldNameId);
    try std.testing.expect(structFieldType(MonoType.Field, "name") == names.RecordFieldNameId);
    try std.testing.expect(structFieldType(MonoType.Tag, "name") == names.TagNameId);
    try std.testing.expect(structFieldType(LambdaMono.FieldExpr, "name") == names.RecordFieldNameId);
    try std.testing.expect(structFieldType(LambdaMono.RecordDestruct, "name") == names.RecordFieldNameId);
    try std.testing.expect(structFieldType(LambdaMonoType.Field, "name") == names.RecordFieldNameId);
    try std.testing.expect(structFieldType(LambdaMonoType.Tag, "name") == names.TagNameId);

    const lir_field = unionPayloadType(LIR.RefOp, "field");
    const lir_payload = unionPayloadType(LIR.RefOp, "tag_payload");
    try std.testing.expect(structFieldType(lir_field, "field_idx") == u16);
    try std.testing.expect(structFieldType(lir_payload, "payload_idx") == u16);
}

test "Monotype record expression lowering does not keep mutable field-store slices across child lowering" {
    const lower_source = @embedFile("monotype/lower.zig");
    const lower_record_expr = sourceSliceBetween(lower_source, "fn lowerRecordExpr", "fn recordUpdateFieldValue");

    try expectContains(lower_record_expr, "const target_fields");
    try expectContains(lower_record_expr, "const target_field_count");
    try expectContains(lower_record_expr, "const target_field_list = try GuardedList.dupe(self.allocator, Type.Field, self.builder.program.types.fieldSpan(target_fields));");
    try expectContains(lower_record_expr, "const field = target_field_list[i];");
    try std.testing.expect(std.mem.find(u8, lower_record_expr, "target_field_borrow") == null);
    try std.testing.expect(std.mem.find(u8, lower_record_expr, "for (target_fields") == null);
}

test "Monotype lookup lowering uses explicit resolved use nodes" {
    const lower_source = @embedFile("monotype/lower.zig");
    const lower_call = sourceSliceBetween(lower_source, "fn lowerCall", "fn directCallInstantiationSourceFnType");
    const lower_expr_type = sourceSliceBetween(lower_source, "fn lowerExprType", "fn lowerExpr(self:");
    const lower_expr_at_type = sourceSliceBetween(lower_source, "fn lowerExprAtType", "fn sameType");
    const lower_lookup_at_type = sourceSliceBetween(lower_source, "fn lowerLookupExprAtType", "fn lowerProcedureUseValue");
    const lookup_type_node = sourceSliceBetween(lower_source, "fn lookupExprTypeNode", "fn lookupExprMonoType");

    try expectContains(lower_call, "if (try self.indirectCalleeMonoType(call.func, call.args, expected_ret_ty)) |fn_ty| {");
    try expectContains(lower_call, "var fn_node = try call_ctx.instantiateCallNodeFromCallerAtNode(");
    try std.testing.expect(std.mem.find(u8, lower_call, "try self.lowerExprType(call.func)") == null);
    try std.testing.expect(std.mem.find(u8, lower_call, "try self.lowerType(call.source_fn_ty_payload)") == null);

    try expectContains(lower_expr_type, ".lookup_required => |resolved| try self.lookupExprTypeNode(expr.ty, resolved)");
    try expectContains(lower_expr_at_type, ".lookup_required => |resolved| return try self.lowerLookupExprAtType(expr.ty, resolved, ty)");
    try expectContains(lookup_type_node, "return try self.lowerTypeNode(checked_ty);");
    try std.testing.expect(std.mem.find(u8, lookup_type_node, "lookupExprMonoType") == null);
    try expectContains(lower_lookup_at_type, ".platform_required_const => |required| return try self.restoreConstUseAtType(");
    try expectContains(lower_lookup_at_type, "required.const_use,\n                ty,\n                try self.evidenceForUseSite(record.expr),");
    try expectContains(lower_lookup_at_type, ".platform_required_proc => |proc| try self.lowerProcedureUseValueAtNode(proc.procedure, try self.activeNodeFromType(ty), try self.evidenceForUseSite(record.expr), proc.root_evidence)");
    try expectContains(lower_source, "fn lowerCallableEvalBindingValueAtNode(");
    try expectContains(lower_source, "try self.restoreConstFnAtNode(view, fn_id, request_fn_node)");
    try expectContains(lower_source, "try body_ctx.graphFunctionNode(&.{}, request_fn_node)");
    try expectContains(lower_source, "body_ctx.lowerComptimeRootExprAtCell(");
}

test "Monotype specialization has no target backend or LIR imports" {
    const sources = .{
        @embedFile("monotype/ast.zig"),
        @embedFile("monotype/type.zig"),
        @embedFile("monotype/lower.zig"),
        @embedFile("monotype/solve.zig"),
        @embedFile("monotype/specialize.zig"),
        @embedFile("monotype/serialize.zig"),
        @embedFile("monotype_lifted/ast.zig"),
        @embedFile("monotype_lifted/lift.zig"),
        @embedFile("monotype_lifted/spec_constr.zig"),
    };
    const forbidden_imports = .{
        "@import(\"backend\")",
        "@import(\"layout\")",
        "@import(\"lir\")",
        "@import(\"lir_core\")",
        "@import(\"roc_target\")",
        "@import(\"llvm\")",
        "@import(\"wasm\")",
    };

    inline for (sources) |source| {
        inline for (forbidden_imports) |needle| {
            try expectNotContains(source, needle);
        }
    }
}

test "Lifted functions own captures and consume Monotype expression storage" {
    try std.testing.expect(@hasField(Lifted.Fn, "captures"));
    try std.testing.expect(Lifted.ExprId == Mono.ExprId);
    try std.testing.expect(Lifted.PatId == Mono.PatId);
    try std.testing.expect(Lifted.StmtId == Mono.StmtId);
    try std.testing.expect(Lifted.ExprData == Mono.ExprData);
    try std.testing.expect(@hasField(Lifted.ExprData, "fn_ref"));
    try std.testing.expect(@hasField(Lifted.ExprData, "call_proc"));
    try std.testing.expect(@hasField(Lifted.ExprData, "call_value"));
    try std.testing.expect(@hasField(Mono.FnSlot, "local"));
    try std.testing.expect(@hasField(Mono.FnSlot, "imported"));
    try std.testing.expect(@hasField(Mono.ProcCallee, "func"));
    try std.testing.expect(@hasField(Mono.ProcCallee, "lifted"));
}

test "Lambda Solved keeps lifted syntax and stores callable sets in types" {
    try std.testing.expect(@hasField(LambdaSolved.Program, "lifted"));
    try std.testing.expect(@hasField(LambdaSolved.Program, "types"));
    try std.testing.expect(@hasField(LambdaSolvedType.Content, "func"));
    try std.testing.expect(@hasField(LambdaSolvedType.Content, "lambda_set"));
    try std.testing.expect(@hasField(LambdaSolvedType.Content, "erased"));
    try std.testing.expect(@hasField(LambdaSolvedType.FnMember, "captures"));

    try std.testing.expect(!@hasField(LambdaSolvedType.Content, "callable"));
    try std.testing.expect(!@hasField(LambdaSolvedType.Content, "erased_fn"));
}

test "SpecConstr owns strict binding chains and retains opaque discarded work" {
    const source = @embedFile("monotype_lifted/spec_constr.zig");
    try expectContains(source, "const ClonedValue = struct");
    try expectContains(source, "bindings: BindingChain");
    try expectContains(source, "const discarded = try self.cloneExprValueInto(stmt_expr, &block_bindings)");
    try expectContains(source, "_ = try self.makeReusableForMatch(discarded, &block_bindings)");
    try expectNotContains(source, "fn_effect_free");
    try expectNotContains(source, "effect_marks");
    try expectNotContains(source, "PendingLet");
    try expectNotContains(source, "localUseBeforeEffect");
    try expectNotContains(source, "unsafeLeafCount");
}

test "Lambda Mono has concrete callable values and no function type" {
    try std.testing.expect(@hasField(LambdaMono.ExprData, "direct_call"));
    try std.testing.expect(@hasField(LambdaMono.ExprData, "indirect_erased_call"));
    try std.testing.expect(@hasField(LambdaMono.ExprData, "packed_erased_fn"));
    try std.testing.expect(@hasField(LambdaMono.ExprData, "callable"));
    try std.testing.expect(@hasField(LambdaMono.ExprData, "capture_access"));
    try std.testing.expect(@hasField(LambdaMonoType.Content, "callable"));
    try std.testing.expect(@hasField(LambdaMonoType.Content, "erased_fn"));
    try std.testing.expect(@hasField(LambdaMonoType.Content, "erased_capture_ptr"));

    try std.testing.expect(!@hasField(LambdaMono.ExprData, "call_value"));
    try std.testing.expect(!@hasField(LambdaMono.ExprData, "call_proc"));
    try std.testing.expect(!@hasField(LambdaMono.ExprData, "lambda"));
    try std.testing.expect(!@hasField(LambdaMonoType.Content, "func"));
    try std.testing.expect(!@hasField(LambdaMonoType.Content, "lambda_set"));
}

test "Bool and source for have no special LIR statement forms" {
    try std.testing.expect(!@hasField(LIR.CFStmt, "for_list"));
    try std.testing.expect(!@hasField(LIR.CFStmt, "for_"));
    try std.testing.expect(!@hasField(LIR.CFStmt, "bool_value"));
    try std.testing.expect(!@hasField(LIR.LiteralValue, "bool_literal"));

    try std.testing.expect(@hasField(LIR.CFStmt, "assign_tag"));
    try std.testing.expect(@hasField(LIR.CFStmt, "join"));
    try std.testing.expect(@hasField(LIR.CFStmt, "jump"));
}

test "post-check expression forms do not reintroduce checked-only syntax" {
    const checked_only = .{
        "dispatch_call",
        "type_dispatch_call",
        "method_call",
        "method_eq",
        "anno_only",
        "for_",
    };

    inline for (checked_only) |name| {
        try std.testing.expect(!@hasField(Mono.ExprData, name));
        try std.testing.expect(!@hasField(Lifted.ExprData, name));
        try std.testing.expect(!@hasField(LambdaMono.ExprData, name));
    }
}

test "stage expression forms only shrink checked syntax or add runtime encoding forms" {
    try std.testing.expect(unionFieldCount(Lifted.ExprData) <= unionFieldCount(Mono.ExprData));
    try std.testing.expect(unionFieldCount(LambdaMono.ExprData) >= unionFieldCount(Lifted.ExprData));

    try std.testing.expect(@hasField(LambdaMono.ExprData, "direct_call"));
    try std.testing.expect(@hasField(LambdaMono.ExprData, "callable"));
}

test "post-check stage products do not store expression cache state" {
    const ir_types = .{
        Mono.Program,
        Lifted.Program,
        LambdaSolved.Program,
        LambdaMono.Program,
    };

    inline for (ir_types) |T| {
        try std.testing.expect(!@hasField(T, "expr_map"));
        try std.testing.expect(!@hasField(T, "memoized_exprs"));
    }
}

test "checked module artifact does not store post-check lowering products" {
    @setEvalBranchQuota(1_000_000);
    comptime assertNoPostCheckType(check.CheckedModule.CheckedModuleArtifact.Serialized, "CheckedModuleArtifact.Serialized");
}

fn assertNoPostCheckType(comptime T: type, comptime path: []const u8) void {
    const type_name = @typeName(T);
    if (std.mem.find(u8, type_name, "postcheck") != null or
        std.mem.find(u8, type_name, "lir.") != null or
        std.mem.find(u8, type_name, "monotype") != null or
        std.mem.find(u8, type_name, "lambda") != null)
    {
        @compileError(path ++ " stores post-check lowering type " ++ type_name);
    }

    switch (@typeInfo(T)) {
        .array => |array| assertNoPostCheckType(array.child, path ++ "[]"),
        .optional => |optional| assertNoPostCheckType(optional.child, path ++ "?"),
        .pointer => |pointer| assertNoPostCheckType(pointer.child, path ++ ".*"),
        .@"struct" => |info| {
            inline for (info.fields) |field| {
                assertNoPostCheckType(field.type, path ++ "." ++ field.name);
            }
        },
        .@"union" => |info| {
            inline for (info.fields) |field| {
                assertNoPostCheckType(field.type, path ++ "." ++ field.name);
            }
        },
        .type,
        .void,
        .bool,
        .noreturn,
        .int,
        .float,
        .comptime_float,
        .comptime_int,
        .undefined,
        .null,
        .error_union,
        .error_set,
        .@"enum",
        .@"fn",
        .@"opaque",
        .frame,
        .@"anyframe",
        .vector,
        .enum_literal,
        => {},
    }
}

test "Monotype lifting mutates only callable expression nodes in place" {
    const lifted_source = @embedFile("monotype_lifted/lift.zig");
    try expectContains(lifted_source, "source: Mono.ProgramView");
    try expectContains(lifted_source, "const source_view = movedMonoView(&owned, &program);");
    try expectContains(lifted_source, "Lifter.init(allocator, source_view, &program)");
    try std.testing.expect(std.mem.find(u8, lifted_source, "self.source.") != null);
    try std.testing.expect(std.mem.find(u8, lifted_source, "self.source.exprs.items") == null);
    try std.testing.expect(std.mem.find(u8, lifted_source, "self.source.pats.items") == null);
    try std.testing.expect(std.mem.find(u8, lifted_source, "self.source.stmts.items") == null);
    try std.testing.expect(std.mem.find(u8, lifted_source, "self.source.locals.items") == null);

    const rewrite_expr = sourceSliceBetween(lifted_source, "fn rewriteExpr", "fn liftLambda");
    try expectContains(rewrite_expr, "self.output.setExprData(expr_id, .{ .fn_ref");
    try expectContains(rewrite_expr, "self.output.setExprData(expr_id, .{ .call_proc");

    const lift_lambda = sourceSliceBetween(lifted_source, "fn liftLambda", "fn reserveFn");
    try expectContains(lift_lambda, "self.output.setExprData(expr_id, .{ .fn_ref = .{");

    const lambda_mono_source = @embedFile("lambda_mono/lower.zig");
    const lower_fn = sourceSliceBetween(lambda_mono_source, "fn lowerFnSpec", "fn ensureOwnFnSpec");
    try expectContains(lower_fn, "self.captures.clearRetainingCapacity();");
    try expectContains(lower_fn, "@memset(self.expr_map, null);");
    try expectContains(lower_fn, "@memset(self.pat_map, null);");
    try expectContains(lower_fn, "@memset(self.stmt_map, null);");
}

test "Lambda Solved consumes lifted program through a read-only view" {
    const lifted_ast_source = @embedFile("monotype_lifted/ast.zig");
    try expectContains(lifted_ast_source, "pub const ProgramView = struct");
    try expectContains(lifted_ast_source, "pub fn view(self: *const Program) ProgramView");

    const solve_source = @embedFile("lambda_solved/solve.zig");
    try expectContains(solve_source, "lifted: Lifted.ProgramView");
    try expectContains(solve_source, "const lifted = program.lifted.view();");
    try std.testing.expect(std.mem.find(u8, solve_source, "self.program.lifted.") == null);
}

test "Lambda Solved unify does not yield explicit empty tag unions" {
    const solve_source = @embedFile("lambda_solved/solve.zig");
    const unify_source = sourceSliceBetween(solve_source, "fn unify", "fn transparentAliasBacking");
    try expectNotContains(unify_source, "isEmptyTagUnion");
    try expectNotContains(unify_source, "empty tag union");
    try expectNotContains(solve_source, "fn isEmptyTagUnion");
}

test "Monotype lowering does not use unsolved_monos side table" {
    const lower_source = @embedFile("monotype/lower.zig");
    const solve_source = @embedFile("monotype/solve.zig");
    try expectNotContains(lower_source, "unsolved_monos");
    try expectNotContains(solve_source, "unsolved_monos");
}

test "Monotype instantiation does not reopen empty tag union views" {
    const solve_source = @embedFile("monotype/solve.zig");
    try expectNotContains(solve_source, "reopenUnsolvedEmptyTagUnionView");
}

test "Monotype generated-private selection cannot become ordinary or reopen finished types" {
    const solve_source = @embedFile("monotype/solve.zig");
    const selection = sourceSliceBetween(
        solve_source,
        "pub fn selectGeneratedPrivateRepresentation(",
        "fn relateOpaqueInterfacePair(",
    );
    try expectContains(selection, "incorrect public/private direction");
    try expectContains(selection, "containsFinishedMono(public_node)");
    try expectContains(selection, "containsFinishedMono(private_node)");
    try expectContains(selection, "finished Monotype reached generated-private representation selection");
    try expectContains(selection, "selectGeneratedPrivateRepresentationAtWidth(public_node, private_node, .exact)");
    try expectContains(selection, "selectGeneratedPrivateRepresentationAtWidth(public_node, private_node, .construction)");
    try expectContains(selection, "unifyRootsTransitively(public_node, private_node, true, row_width)");

    const ordinary_unify = sourceSliceBetween(
        solve_source,
        "pub fn unify(self: *InstGraph",
        "fn relationStamp(",
    );
    try expectContains(ordinary_unify, "unifyRootsTransitively(a, b, false, .exact)");
    try expectContains(ordinary_unify, "unifyRootsTransitively(a, b, false, .construction)");
    try expectContains(ordinary_unify, "generated-private representation reached ordinary public/private graph unification");

    const lower_source = @embedFile("monotype/lower.zig");
    const request_selection = sourceSliceBetween(
        lower_source,
        "fn selectRequestRepresentation(",
        "const HostedTryAdapterCapability",
    );
    try expectContains(request_selection, "containsFinishedMono(public_node)");
    try expectContains(request_selection, "containsFinishedMono(private_node)");
    try expectContains(request_selection, "relateOpaqueInterface(public_node, private_node)");
    try expectContains(request_selection, "selectGeneratedPrivateRepresentation(public_node, private_node)");

    const dispatch_selection = sourceSliceBetween(
        lower_source,
        "fn selectExprRepresentationAtNode(",
        "fn lowerCallExprAtNode(",
    );
    try expectContains(dispatch_selection, "selectRequestRepresentation(");
    try expectContains(dispatch_selection, "try self.lowerExprTypeNode(checked_expr)");

    const dispatch_instantiation = sourceSliceBetween(
        lower_source,
        "fn instantiateCallableDispatchPlanCallNodeFromCallerAtNode(",
        "fn relateFormalToOperand(",
    );
    try expectContains(dispatch_instantiation, "callable_plan: CallableDispatchPlan");
    try expectContains(dispatch_instantiation, "try relateRequestComponent(self.graph, fn_graph.args[index], dispatcher_node)");

    const entry_wrapper = sourceSliceBetween(
        lower_source,
        "fn lowerEntryWrapperAtCell(",
        "fn instantiateTemplateDispatchRelations(",
    );
    try expectContains(entry_wrapper, "try relateRequestComponent(self.graph, declared_ret_node, body_ret_node)");
    try expectContains(entry_wrapper, "containsGeneratedPrivate(body_ret_node)");
    try expectContains(entry_wrapper, ".ret = produced_ret_cell");
}

test "Monotype active snapshots reject unresolved rows and cannot be refilled" {
    const solve_source = @embedFile("monotype/solve.zig");
    try expectContains(solve_source, "immutable Monotype snapshot requested for an unresolved instantiation graph node");
    try expectContains(solve_source, "GraphTypeFinals.initActiveSnapshot(self)");
    try expectNotContains(solve_source, "replaceGraphView");
    try expectNotContains(solve_source, "fn fillMono(");
}

test "Monotype draft local identity stays graph-native" {
    const lower_source = @embedFile("monotype/lower.zig");
    const identity = sourceSliceBetween(
        lower_source,
        "fn sameLocalIdentity(self: *BodyContext",
        "fn stmtDependsOnFreeLocal",
    );
    try expectContains(identity, "lhs_data.ty.toGraphNode(self.graph)");
    try expectContains(identity, "rhs_data.ty.toGraphNode(self.graph)");
    try expectContains(identity, "self.graph.sameClass(lhs_node, rhs_node)");
    try expectNotContains(identity, "activeTypeFromCell");
}

test "Monotype iterator result completion stays out of relation replay and retains public request lookups" {
    const lower_source = @embedFile("monotype/lower.zig");
    const dispatch_result = sourceSliceBetween(
        lower_source,
        "fn callableDispatchResultTypeNodeInPhase(",
        "fn materializeEvidence(",
    );
    try expectContains(dispatch_result, "if (phase == .expression_lowering)");
    try expectContains(dispatch_result, "lowerAndCompleteIteratorMethodResultAtNode(");

    const completion = sourceSliceBetween(
        lower_source,
        "fn completeDeferredIteratorResult(",
        "fn constUseMonoType(",
    );
    try expectContains(completion, "try updateTemplateSpecInterfaceLookups(");
    try expectContains(completion, "completed_source.evidence_digest.bytes");
    try expectNotContains(completion, "unregisterTemplateSpec");

    const template_spec = sourceSliceBetween(
        lower_source,
        "const DraftTemplateSpec = struct",
        "const DraftConstUseProvenance",
    );
    try expectContains(template_spec, "lookup_request_fn_node: ?NodeId");
}

test "Monotype direct uninhabited calls lower argument through graph cell" {
    const lower_source = @embedFile("monotype/lower.zig");
    const direct_call = sourceSliceBetween(
        lower_source,
        "fn lowerDirectCallWithUninhabitedArgument",
        "fn indirectCalleeMonoType",
    );
    try expectContains(direct_call, "lowerUninhabitedScrutineeAtTypeCell");
    try expectNotContains(direct_call, "activeTypeFromNode");
}

test "Monotype uninitialized binders retain unresolved graph cells" {
    const lower_source = @embedFile("monotype/lower.zig");
    const uninitialized = sourceSliceBetween(
        lower_source,
        "fn lowerUninitializedPatternStatement",
        "fn lowerPatternStatement",
    );
    try expectContains(uninitialized, "checked_pattern.data != .assign");
    try expectContains(uninitialized, "lowerShapeFreePatternAtCell");
    try expectNotContains(uninitialized, "activeTypeFromNode");
}

test "Monotype pattern statements retain graph provenance" {
    const lower_source = @embedFile("monotype/lower.zig");
    const statement = sourceSliceBetween(
        lower_source,
        "fn lowerPatternStatement(",
        "fn patternIsShapeFree(",
    );
    try expectContains(statement, "lowerShapeFreePatternAtCell(pattern, value_cell)");
    try expectContains(statement, "lowerPatternAtNode(pattern, try value_cell.toGraphNode(self.graph))");
    try expectNotContains(statement, "activeTypeFromCell(value_cell)");
    try expectNotContains(statement, "lowerPatternAtType(pattern");
}

test "Monotype expanded record-rest statements retain graph provenance" {
    const lower_source = @embedFile("monotype/lower.zig");
    const record_rest = sourceSliceBetween(
        lower_source,
        "fn appendExpandedPatternStatement(",
        "fn checkedStatementHasRuntimeEffect(",
    );
    try expectContains(record_rest, "const value_node = try self.lowerExprTypeNode(expr)");
    try expectContains(record_rest, "addLocalWithBinderCell(self.builder.symbols.fresh(), value_cell, null)");
    try expectContains(record_rest, "self.graph.recordFieldNode(value_node, name)");
    try expectContains(record_rest, "self.lowerPatternAtNode(child, field_node)");
    try expectContains(record_rest, "lowerRecordRestValueWithTypeCell");
    try expectContains(record_rest, "self.lowerPatternAtNode(child, rest_node)");
    try expectNotContains(record_rest, "exprType(value)");
    try expectNotContains(record_rest, "activeTypeFromNode(rest_node)");
    try expectNotContains(record_rest, "lowerPatternAtType(");
}

test "Monotype gates divergent relations and crash dispatches before type instantiation" {
    const lower_source = @embedFile("monotype/lower.zig");
    const divergent_call = sourceSliceBetween(
        lower_source,
        "fn lowerDivergentCallOperand",
        "fn instantiateCallNodeFromCallerAtNode",
    );
    try expectContains(divergent_call, "lowerDivergentExprAtTypeCell(operand, ret_cell)");
    try expectNotContains(divergent_call, "lowerTypeView");
    try expectNotContains(divergent_call, "activeTypeFromNode");

    const crash_dispatch = sourceSliceBetween(
        lower_source,
        "fn lowerDispatchExprAtType(",
        "const expected_ret_ty:",
    );
    try expectContains(crash_dispatch, "expected_ret_cell: DraftTypeCell");
    try expectContains(crash_dispatch, ".crash => |reason|");
    try expectContains(crash_dispatch, "addExprWithTypeCell(expected_ret_cell");
    try expectNotContains(crash_dispatch, "unitType()");
    try expectNotContains(crash_dispatch, "plan.callable_ty");

    const contextual_gate = sourceSliceBetween(
        lower_source,
        "fn lowerExprAtTypeCellWithDemand(",
        "fn lowerExprAtTypeCellInner(",
    );
    try expectContains(contextual_gate, "self.checkedExprDivergesInLoweredRuntime(checked_expr)");
    try expectContains(contextual_gate, "fn lowerExprAtTypeCellWithKnownDivergence(");
    try expectContains(contextual_gate, "if (expr_diverges)");
    try expectContains(contextual_gate, "lowerDivergentExprAtTypeCell(checked_expr, cell)");

    const result_lookup = sourceSliceBetween(
        lower_source,
        "fn dispatchResultTypeNodeInPhase(",
        "fn callableDispatchResultTypeNodeInPhase(",
    );
    try expectContains(result_lookup, "rejected dispatch reached result type lookup without a contextual result cell");
    try expectNotContains(result_lookup, "unitType()");

    const relation_gate = sourceSliceBetween(
        lower_source,
        "fn relateExprAtNode(",
        "fn relateTagExprAtNode(",
    );
    try expectContains(relation_gate, "if (self.checkedExprDivergesInLoweredRuntime(checked_expr)) return;");
    try expectNotContains(relation_gate, "checkedTypeContainsError");
}

test "Monotype dispatch result modes retain graph-backed result types" {
    const lower_source = @embedFile("monotype/lower.zig");
    const parametric_low_level = sourceSliceBetween(
        lower_source,
        "if (direct_parametric_low_level) |op| {",
        "const call_data = if (direct_graph_call)",
    );
    try expectContains(parametric_low_level, "applyDispatchResultMode(plan.result_mode, call_expr)");
    try expectNotContains(parametric_low_level, "activeTypeFromNode(plan_ret_node)");

    const result_mode = sourceSliceBetween(
        lower_source,
        "fn applyDispatchResultMode(",
        "fn typeCellHasBuiltinOwner(",
    );
    try expectContains(result_mode, "self.exprTypeCell(expr)");
    try expectContains(result_mode, "self.addExprWithTypeCell(result_cell");
    try expectNotContains(result_mode, "Type.TypeId");
    try expectNotContains(result_mode, "activeTypeFrom");
    try expectNotContains(result_mode, "primitiveType(.bool)");
}

test "Monotype const type lookup remains graph-native" {
    const lower_source = @embedFile("monotype/lower.zig");
    try expectContains(lower_source, "fn constUseTypeNode");
    try expectContains(lower_source, "return try self.constUseTypeNode(checked_ty, const_use)");
}

test "Monotype does not attach durable request types as active snapshots" {
    const lower_source = @embedFile("monotype/lower.zig");
    const solve_source = @embedFile("monotype/solve.zig");
    try expectNotContains(lower_source, ".addMonoView(");
    try expectNotContains(solve_source, "pub fn addMonoView");
    try expectContains(lower_source, "lowerTemplateBodyAtNode(template_ref, template, root_node)");
    try expectContains(lower_source, "lowerStrInspectIntrinsicAtNode(fn_nodes, ret_cell)");
}

test "Monotype pairs stored record children in lexicographic field order" {
    const lower_source = @embedFile("monotype/lower.zig");
    const restore_record = sourceSliceBetween(
        lower_source,
        "fn restoreConstRecordAtNode",
        "fn restoreConstTagPayloadsAtNode",
    );
    try expectContains(restore_record, "const graph_fields = (try self.graph.recordNodes(record_node)).fields");
    try expectContains(restore_record, "const fields = try self.allocator.dupe(InstField, graph_fields)");
    try expectContains(restore_record, "instRecordFieldLessThan");
}

test "Monotype prepares consts inside equality before frozen equality emission" {
    const lower_source = @embedFile("monotype/lower.zig");
    const prepare = sourceSliceBetween(
        lower_source,
        "fn prepareDraftDeferredExprs",
        "fn finalizeDraftConstUse",
    );
    const restore_index = std.mem.find(u8, prepare, "finalizeDraftConstUse") orelse
        return error.TestUnexpectedResult;
    const equality_index = std.mem.find(u8, prepare, "prepareDraftStructuralEq") orelse
        return error.TestUnexpectedResult;
    try std.testing.expect(restore_index < equality_index);

    const seal = sourceSliceBetween(
        lower_source,
        "fn sealActiveBodyDraft",
        "fn markDraftNestedReady",
    );
    const freeze_index = std.mem.find(u8, seal, "freezeRelations") orelse
        return error.TestUnexpectedResult;
    const emit_index = std.mem.find(u8, seal, "emitDraftDeferredStructuralEqs") orelse
        return error.TestUnexpectedResult;
    try std.testing.expect(freeze_index < emit_index);
    try expectNotContains(prepare, "sealNode");
    try expectNotContains(lower_source, "EvidenceComponentSnapshot");
}

test "Monotype rejects unfilled deferred equality emission plans" {
    const lower_source = @embedFile("monotype/lower.zig");
    try expectContains(lower_source, "deferred structural equality had no prepared emission plan");
    try expectContains(lower_source, "deferred structural equality emission plan did not fill its reservation");
    try expectNotContains(lower_source, "sealed_operand_ty");
    try expectNotContains(lower_source, "changed after derivation sealing");
}

test "Monotype structural equality result probes remain graph-native" {
    const lower_source = @embedFile("monotype/lower.zig");
    const equality_source = sourceSliceBetween(
        lower_source,
        "fn structuralEqualityOperandType",
        "fn prepareStructuralEqNode",
    );
    try expectContains(equality_source, "fn structuralEqualityExprResultNode");
    try expectContains(equality_source, "try self.callResultTypeNode");
    try expectContains(equality_source, "try self.dispatchResultTypeNode");
    try expectContains(equality_source, "try self.lookupExprTypeNode");
    try expectContains(equality_source, "try self.fieldAccessTypeNode");
    try expectContains(equality_source, "self.graph.typeIsResolved(operand_node)");
    try expectNotContains(equality_source, "structuralEqualityExprResultType");

    const dispatch_equality = sourceSliceBetween(
        lower_source,
        "fn lowerStructuralEqualityAtNode(",
        "const StructuralBinaryOperands = struct",
    );
    try expectContains(dispatch_equality, "self.graph.functionNodes(callable_node)");
    try expectContains(dispatch_equality, "self.graph.typeIsResolved(fn_nodes.args[0])");
    try expectContains(dispatch_equality, "deferStructuralEqOperandsAtNode");
    try expectNotContains(dispatch_equality, "resolvedTypeViewForNode(callable_node)");
}

test "Monotype loop carries remain graph-native through headers and backedges" {
    const lower_source = @embedFile("monotype/lower.zig");
    const loop_source = sourceSliceBetween(
        lower_source,
        "const LoopCarry = struct",
        "fn collectReassignedBindersInExpr",
    );
    try expectContains(loop_source, "ty: DraftTypeCell");
    try expectContains(loop_source, "const ty = self.localTypeCell(initial)");
    try expectContains(loop_source, "fn loopStateTypeCell");
    try expectContains(loop_source, "try self.addExprWithTypeCell(carry.ty");
    try expectContains(loop_source, "try self.addPatWithTypeCell(carry.ty");
    try expectContains(loop_source, "try self.draft.addTypedLocalSpan(params)");
    try expectNotContains(loop_source, "try self.localType(initial)");
    try expectNotContains(loop_source, "fn loopStateType(");
}

test "Monotype indirect calls retain graph-native function provenance" {
    const lower_source = @embedFile("monotype/lower.zig");
    const call_source = sourceSliceBetween(
        lower_source,
        "fn lowerCallAtType(",
        "fn lowerDirectCallWithUninhabitedArgument(",
    );
    try expectContains(call_source, "instantiateCallNodeFromCallerAtNode");
    try expectContains(call_source, "const fn_nodes = try self.graph.functionNodes(fn_node)");
    try expectContains(call_source, "try self.prepareExprSpanAtNodes(call.args, fn_nodes.args)");
    try expectContains(call_source, ".callee = try self.lowerExprAtTypeCell(call.func, DraftTypeCell.fromGraphNode(fn_node))");
    try expectContains(call_source, ".args = try self.lowerPreparedExprSpanAtNodes(call.args, fn_nodes.args)");
    try expectContains(call_source, ".ret_ty = DraftTypeCell.fromGraphNode(fn_nodes.ret)");
    try expectNotContains(lower_source, "instantiateCallTypeFromCallerAtType");

    const direct_prepare = std.mem.find(u8, call_source, "try self.prepareExprSpanAtNodes(call.args, fn_nodes.args)").?;
    const direct_specialize = std.mem.find(u8, call_source, "const callee = try self.fnTemplateForDirectCallAtNode").?;
    try std.testing.expect(direct_prepare < direct_specialize);
}

test "Monotype open specialization lookup covers the complete function interface" {
    const lower_source = @embedFile("monotype/lower.zig");
    const template_source = sourceSliceBetween(
        lower_source,
        "fn lowerDraftTemplateFromContext(",
        "fn lowerDraftNestedFromContext(",
    );
    const nested_source = sourceSliceBetween(
        lower_source,
        "fn lowerDraftNestedFromContext(",
        "fn lowerExprAtTypeCell(",
    );
    inline for (.{ template_source, nested_source }) |lookup_source| {
        try expectContains(lookup_source, "functionInterfaceIterator(request_fn_node)");
        try expectContains(lookup_source, "classMemberIterator(interface_node)");
        try expectContains(lookup_source, "seen_specs.getOrPut(raw_spec)");
        try expectContains(lookup_source, "draftOpenCandidateQualifies(");
        try expectContains(lookup_source, "spec.runtime_demand_guard_frames");
        try expectContains(lookup_source, "source_ctx.runtimeDemandGuardFrameAddresses()");
        try expectContains(lookup_source, "if (!selection.add(raw_spec, exact_interface))");
        try expectContains(lookup_source, "if (selection.selected()) |raw_spec|");
        try expectContains(lookup_source, "try source_ctx.graph.unifyRecursiveFunctionInterface(");
        try expectContains(lookup_source, "spec.initial_request_arg_classes");
        try expectNotContains(lookup_source, "functionInterfaceAnchor");
    }
    try expectContains(template_source, "draftTemplateSpecLookupRequestNode(spec)");
    try expectContains(nested_source, "sameFunctionInterface(spec.request_fn_node, request_fn_node)");
    const interface_registration = sourceSliceBetween(
        lower_source,
        "fn updateTemplateSpecInterfaceLookups(",
        "fn draftNestedSpecRequestNode(",
    );
    try expectContains(interface_registration, "indexed_nodes.getOrPut(interface_node)");
    try expectContains(interface_registration, "draftOpenRequestKey(interface_node)");
    try expectContains(nested_source, "std.meta.eql(spec.lexical_owner, source_ctx.draft.current_owner)");
}

test "Monotype match lowering relates patterns before specialization and projects graph cells" {
    const lower_source = @embedFile("monotype/lower.zig");
    const match_source = sourceSliceBetween(
        lower_source,
        "fn lowerMatch(",
        "fn savePatternBinders(",
    );
    try expectContains(match_source, "const scrutinee_cell = DraftTypeCell.fromGraphNode(scrutinee_node)");
    try expectContains(match_source, "try relateRequestComponent(");
    try expectContains(match_source, "try entry.ctx.preRegisterPatternBindersAtNode");
    try expectContains(match_source, "entry.ctx.runtime_demand_guard_frames = try entry.ctx.withMatchBranchRuntimeDemandGuardFrame");
    try expectContains(match_source, "try entry.ctx.lowerMatchBranchBody");
    try expectContains(match_source, "try entry.ctx.lowerMatchPatternAtNode");
    try expectNotContains(match_source, "resolvedTypeViewForNode(scrutinee_node)");
    try expectNotContains(match_source, "lowerPatternAtType(entry.pattern.pattern");
    try expectNotContains(lower_source, "rebindPreRegisteredPatternBindersAtNode");

    const relate = std.mem.find(u8, match_source, "try relateRequestComponent(").?;
    const prepare_binders = std.mem.find(u8, match_source, "try entry.ctx.preRegisterPatternBindersAtNode").?;
    const prepare_result = std.mem.find(u8, match_source, "try entry.ctx.prepareControlFlowResultSelection").?;
    const guards = std.mem.find(u8, match_source, "entry.ctx.runtime_demand_guard_frames =").?;
    const lower_body = std.mem.find(u8, match_source, "try entry.ctx.lowerMatchBranchBody").?;
    const lower_pattern = std.mem.find(u8, match_source, "try entry.ctx.lowerMatchPatternAtNode").?;
    try std.testing.expect(relate < prepare_binders);
    try std.testing.expect(prepare_binders < prepare_result);
    try std.testing.expect(prepare_result < guards);
    try std.testing.expect(guards < lower_body);
    try std.testing.expect(lower_body < lower_pattern);

    const binder_source = sourceSliceBetween(
        lower_source,
        "fn materializePatternBinderAtCell(",
        "fn lowerUninhabitedScrutinee(",
    );
    try expectContains(binder_source, "self.draft.setLocalType(local, cell)");
}

test "Monotype runtime demands snapshot pass-local compositional impossibility proofs" {
    const lower_source = @embedFile("monotype/lower.zig");
    const demand_source = sourceSliceBetween(
        lower_source,
        "fn requireLoweredExpr(",
        "fn nodeIsProvenUninhabited(",
    );
    try expectContains(demand_source, ".impossibility_proof = try self.currentRuntimeImpossibilityProof(expr_proof)");
    try expectContains(demand_source, ".frames = self.runtime_demand_guard_frames");
    try expectContains(demand_source, ".local_proof = try self.anyImpossibilityProof(&.{ entry_guard_proof, expr_proof })");
    try expectNotContains(demand_source, "producer");
    try expectNotContains(demand_source, "flattenRuntimeDemandGuardFrames");

    const proof_data = sourceSliceBetween(
        lower_source,
        "const RuntimeImpossibilityProofId",
        "const DraftStructuralEqMethodCall",
    );
    try expectContains(proof_data, "any: DraftSpan(RuntimeImpossibilityProofId)");
    try expectContains(proof_data, "all: DraftSpan(RuntimeImpossibilityProofId)");
    try expectContains(proof_data, "pending,");
    try expectContains(proof_data, "forward: RuntimeImpossibilityProofId");
    try expectContains(proof_data, "impossibility_proof: ?RuntimeImpossibilityProofId");
    try expectContains(proof_data, "statement_success");
    try expectContains(proof_data, "const RuntimeDemandGuardFrameStack = struct");
    try expectContains(proof_data, "parent: ?RuntimeDemandGuardFrameId");
    try expectContains(proof_data, "try draft.runtime_demand_guard_frames.append");
    try expectNotContains(proof_data, "alloc(RuntimeDemandGuardFrame, existing.len + 1)");
    try expectContains(proof_data, "runtime impossibility proof graph contained a cycle");

    const composition = sourceSliceBetween(
        lower_source,
        "fn exprDataImpossibilityProof(",
        "fn stmtDataImpossibilityProof(",
    );
    try expectContains(composition, ".call_value => |call| try self.anyImpossibilityProof(&.{");
    try expectContains(composition, "try self.cellImpossibilityProof(ty)");
    try expectContains(composition, ".low_level => |call| try self.anyImpossibilityProof(&.{");
    try expectContains(composition, ".field_access => |field| try self.anyImpossibilityProof(&.{");
    try expectContains(composition, ".tuple_access => |tuple| try self.anyImpossibilityProof(&.{");
    try expectContains(composition, "try self.allImpossibilityProof(alternatives.items)");
    try expectContains(composition, "try self.allImpossibilityProof(&.{");

    const cell_proof = sourceSliceBetween(
        lower_source,
        "fn cellImpossibilityProof(",
        "fn patDataImpossibilityProof(",
    );
    try expectContains(cell_proof, ".graph_node => |node| try self.maybeNodeImpossibilityProof(node)");
    try expectContains(cell_proof, ".sealed => |ty| if (try self.typeIsProvenUninhabited(ty))");
    try expectContains(cell_proof, "else\n                null");
    try expectNotContains(cell_proof, ".never");
    try expectNotContains(cell_proof, "toGraphNode");

    const cell_boundary = sourceSliceBetween(
        lower_source,
        "fn lowerExprAtTypeCell(",
        "fn lowerExprAtTypeCellInner(",
    );
    try expectContains(cell_boundary, "self.lowerExprAtTypeCellWithDemand(checked_expr, cell, .runtime_value)");
    try expectContains(cell_boundary, "const region = self.sourceRegionForExpr(expr)");
    try expectContains(cell_boundary, "self.builder.program.current_loc = try self.sourceLocFor(region)");
    try expectContains(cell_boundary, "self.builder.program.current_region = region");
    try expectContains(cell_boundary, "return switch (cell)");
    try expectContains(cell_boundary, ".sealed => |ty|");
    try expectContains(cell_boundary, "self.requireLoweredExprAtCell(expr, cell, demand, lowered)");
    try expectContains(cell_boundary, ".graph_node => |expected_node|");
    try expectContains(cell_boundary, "self.requireLoweredExpr(expr, expected_node, demand, lowered)");
    try expectNotContains(cell_boundary, "const expected_node = try cell.toGraphNode(self.graph)");

    const producers = sourceSliceBetween(
        lower_source,
        "fn addExpr(self: *BodyContext",
        "fn addFieldExprSpan(",
    );
    try expectContains(producers, "expr_impossibility_proofs.items[@intFromEnum(id)] = try self.exprDataImpossibilityProof");
    try expectContains(producers, "pat_impossibility_proofs.items[@intFromEnum(id)] = try self.patDataImpossibilityProof");
    try expectContains(producers, "stmt_impossibility_proofs.items[@intFromEnum(id)] = try self.stmtDataImpossibilityProof(stmt)");

    const statement_frames = sourceSliceBetween(
        lower_source,
        "fn withStatementSuccessRuntimeDemandGuardFrame(",
        "fn runtimeDemandGuardFrameAddresses(",
    );
    try expectContains(statement_frames, "runtimeDemandGuardFrameAddressRaw(@intFromEnum(statement_id), .statement_success)");
    try expectContains(statement_frames, "try pushRuntimeDemandGuardFrame(");
    try expectContains(lower_source, "body_ctx.runtime_demand_guard_frames = source_ctx.runtime_demand_guard_frames");
    try expectContains(lower_source, "runtimeDemandGuardFrameStackContains(self.draft, self.runtime_demand_guard_frames, address)");
    try expectContains(lower_source, "const proof_reservation = try self.addImpossibilityProof(.pending)");
    try expectContains(lower_source, ".{ .forward = proof }");
    try expectContains(lower_source, "try self.resolveDraftConstUseReservations(body_draft)");
    try expectContains(lower_source, "sources[reservation_index] = restored");
    try expectContains(lower_source, "deferred const reservation dependencies formed a cycle");
    try expectContains(lower_source, "body_draft.expr_locs.items[reservation_index] = body_draft.expr_locs.items[restored_index]");
    try expectContains(lower_source, "body_draft.expr_regions.items[reservation_index] = body_draft.expr_regions.items[restored_index]");
    try expectContains(lower_source, "const DraftConstUseProvenance = union(enum)");
    try expectContains(lower_source, "hoisted: checked.HoistedConstEntry");
    try expectContains(lower_source, "hoisted const use reached a declared deferred boundary");
    try expectContains(lower_source, "declared const use reached a hoisted deferred boundary");
    try expectContains(lower_source, "deferred hoisted const provenance referenced a different const template");
    try expectContains(lower_source, "ctx.restoredHoistedConstAtNode(entry, boundary.witness_node)");
    try expectContains(lower_source, "relateRequestComponent(graph, boundary.witness_node, restored_node)");
    try expectNotContains(lower_source, "body_draft.exprs.items[reservation_index].ty = DraftTypeCell.fromGraphNode(boundary.request_node)");
    try expectNotContains(lower_source, "runtimeResultProducerForDraftCallee");
    try expectNotContains(lower_source, "runtimeDemandHasUninhabitedProducerGuard");
}

test "Monotype closed direct low-level lowering stays sealed and allocation disciplined" {
    const lower_source = @embedFile("monotype/lower.zig");

    const low_level = sourceSliceBetween(
        lower_source,
        "fn lowerClosedDirectLowLevelDispatch(",
        "fn lowerClosedDispatchOperandsAtTypes(",
    );
    try expectContains(low_level, "lowerClosedDispatchOperandsAtTypes(");
    try expectNotContains(low_level, "activeNodeFromType(callable_ty)");
    try expectNotContains(low_level, "constrainTypeToMono");

    const sealed_operands = sourceSliceBetween(
        lower_source,
        "fn lowerClosedDispatchOperandsAtTypes(",
        "fn lowerClosedDirectProcedureDispatch(",
    );
    try expectContains(sealed_operands, "self.typeIsProvenUninhabited(arg_ty)");
    try expectContains(sealed_operands, "self.reserveExprSpan(operands.len)");
    try expectContains(sealed_operands, "self.lowerDispatchOperandAtType(operand, ty)");
    try expectNotContains(sealed_operands, "InstGraph");
    try expectNotContains(sealed_operands, "activeNodeFromType");

    const graph_operands = sourceSliceBetween(
        lower_source,
        "fn lowerClosedDispatchOperandsAtNode(",
        "fn lowerDispatchWithUninhabitedArgument(",
    );
    try expectContains(graph_operands, "prepareDispatchOperandsAtNodes(operands, function.args, &.{})");
    try expectContains(graph_operands, "lowerPreparedDispatchOperandsAtNodes(");
    try expectNotContains(graph_operands, "relateExprAtNode");
    try expectNotContains(graph_operands, "ensureNestedCallableAtNode");

    const pattern_statement = sourceSliceBetween(
        lower_source,
        "fn lowerPatternStatement(",
        "fn patternIsShapeFree(",
    );
    try expectContains(pattern_statement, "self.graphFreeResultTypeForExpr(expr)");
    try expectContains(pattern_statement, ".{ .sealed = ty }");
    try expectContains(pattern_statement, "lowerExprAtTypeCellWithKnownDivergence(");

    const binder_map = sourceSliceBetween(
        lower_source,
        "const BinderMap = struct",
        "const TypedBinder = struct",
    );
    try expectContains(binder_map, "locals: ?[]?DraftLocalId = null");
    try expectContains(binder_map, "if (self.locals == null)");
    try expectNotContains(binder_map, "AutoHashMap");

    const inst_node = sourceSliceBetween(
        lower_source,
        "fn instNode(self: *BodyContext",
        "fn freshInstNode(self: *BodyContext",
    );
    try expectNotContains(inst_node, "self.builder.lowerType(self.view, checked_ty)");
    try expectNotContains(inst_node, "self.graph.importMono(closed_ty)");

    const fresh_inst_node = sourceSliceBetween(
        lower_source,
        "fn freshInstNode(self: *BodyContext",
        "fn scopedNode(self: *BodyContext",
    );
    try expectContains(fresh_inst_node, "TypeInstantiationContext.init");
    try expectNotContains(fresh_inst_node, "BodyContext.initWithMethodScope");

    const source_mapping = sourceSliceBetween(
        lower_source,
        "fn sourceLocFor(",
        "fn sourceRegionForExpr(",
    );
    try expectContains(source_mapping, ".file = self.source_file_id");
    try expectNotContains(source_mapping, "sourceFileIdFor");

    const proof_fold = sourceSliceBetween(
        lower_source,
        "fn anyRuntimeImpossibilityProof(",
        "const BinderRestore = struct",
    );
    try expectContains(proof_fold, "addManyAsSlice(allocator, active_count)");
    try expectNotContains(proof_fold, "std.ArrayList");
}

test "Monotype inspect-only unresolved values defer until final graph sealing" {
    const lower_source = @embedFile("monotype/lower.zig");
    const inspect_source = sourceSliceBetween(
        lower_source,
        "fn lowerStrInspectIntrinsicAtNode(",
        "fn lowerLambdaTemplateAtNode(",
    );
    try expectContains(inspect_source, "try self.graph.typeIsResolved(arg_node)");
    try expectContains(inspect_source, "try self.deferInspectAtNode(local_expr, arg_node, ret_ty)");
    try expectContains(inspect_source, ".impossibility_proof = try self.currentRuntimeImpossibilityProof(null)");
    try expectNotContains(inspect_source, "else => try self.inspectCall(local_expr, try self.activeTypeFromNode(arg_node)");

    const prepare_inspect = sourceSliceBetween(
        lower_source,
        "fn prepareDraftInspectMethods(",
        "fn emitDraftDeferredStructuralEqs(",
    );
    try expectContains(prepare_inspect, "if (try ctx.deferredInspectHasProvenUninhabitedValueGuard(boundary)) return false");

    const emit_inspect = sourceSliceBetween(
        lower_source,
        "fn emitDraftDeferredInspects(",
        "fn emitDraftStructuralEq(",
    );
    try expectContains(emit_inspect, "FrozenRuntimeImpossibilityProofEvaluator.init(self.allocator, graph, body_draft)");
    try expectContains(emit_inspect, "try graph.finalizesAsUninhabited(boundary.value_node)");
    try expectContains(emit_inspect, "try impossibility_evaluator.holds(boundary.impossibility_proof)");
    try expectContains(emit_inspect, "try ctx.zeroBranchMatchAtTypeCell(boundary.value");
    try expectContains(emit_inspect, "const value_ty = try sealer.sealNode(boundary.value_node)");
    try expectContains(emit_inspect, "break :blk try ctx.inspectCall(boundary.value, value_ty, boundary.ret_ty)");

    const deferred_guard = sourceSliceBetween(
        lower_source,
        "fn deferredInspectHasProvenUninhabitedValueGuard(",
        "fn nodeIsProvenUninhabitedInner(",
    );
    try expectContains(deferred_guard, "self.nodeIsProvenUninhabited(boundary.value_node)");
    try expectContains(deferred_guard, "self.activeImpossibilityProofHolds(boundary.impossibility_proof)");
    try expectContains(deferred_guard, "active runtime impossibility proof graph contained a cycle");

    const template_body = sourceSliceBetween(
        lower_source,
        "fn lowerTemplateBodyAtNode(",
        "fn lowerEntryWrapperAtCell(",
    );
    try expectContains(template_body, "self.function_entry_demand_guards = fn_nodes.args");

    const nested_body = sourceSliceBetween(
        lower_source,
        "fn lowerNestedFunctionAtNode(",
        "fn lowerNestedLambdaTemplateAtNode(",
    );
    try expectContains(nested_body, "fn_nodes.args.len + capture_entry_guards.len");
    try expectContains(nested_body, "@memcpy(entry_guards[fn_nodes.args.len..], capture_entry_guards)");

    const current_proof = sourceSliceBetween(
        lower_source,
        "fn currentRuntimeImpossibilityProof(",
        "fn exprImpossibilityProof(",
    );
    try expectContains(current_proof, "self.function_entry_demand_guards.len");
    try expectContains(current_proof, "combined = try self.anyImpossibilityProof(&.{ combined, frame.proof })");
    try expectContains(current_proof, "return try self.anyImpossibilityProof(&.{ combined, expression_proof })");
    try expectNotContains(current_proof, "std.ArrayList");

    const frame_addresses = sourceSliceBetween(
        lower_source,
        "fn runtimeDemandGuardFrameAddresses(",
        "fn runtimeDemandGuardsForPattern(",
    );
    try expectNotContains(frame_addresses, "function_entry_demand_guards");
    try expectContains(lower_source, "child.function_entry_demand_guards = self.function_entry_demand_guards");
    try expectContains(lower_source, "child.function_entry_demand_guards = &.{}");

    const seal_source = sourceSliceBetween(
        lower_source,
        "fn sealActiveBodyDraft(",
        "fn markDraftNestedReady(",
    );
    try expectContains(seal_source, "try graph.freezeRelations()");
    try expectContains(seal_source, "try self.emitDraftDeferredInspects(body_draft, graph, &sealer)");
    try expectContains(lower_source, "try self.prepareDraftInspectMethods(body_draft, graph, boundary)");
    try expectContains(lower_source, "try self.methodTargetCalleeAtNode(lookup, request_node, .synthesize)");
    try expectContains(lower_source, "ctx.frozen_inspect_method_calls = &prepared_methods");
    const freeze = std.mem.find(u8, seal_source, "try graph.freezeRelations()").?;
    const emit = std.mem.find(u8, seal_source, "try self.emitDraftDeferredInspects").?;
    try std.testing.expect(freeze < emit);
    const durable_inhabitation = sourceSliceBetween(
        lower_source,
        "fn typeIsProvenUninhabited(self: *BodyContext",
        "fn checkedPatternIsProvenUninhabited(",
    );
    try expectContains(durable_inhabitation, "self.builder.typeIsProvenUninhabited(ty)");
    try expectNotContains(durable_inhabitation, "activeNodeFromType");
    const inspect_call = sourceSliceBetween(
        lower_source,
        "fn inspectCall(self: *BodyContext",
        "fn inspectDefForType(self: *BodyContext",
    );
    try expectContains(inspect_call, ".sealed = try self.functionType(&.{value_ty}, str_ty)");
    try expectNotContains(inspect_call, "oneArgFnTypeCell");
    const to_inspect = sourceSliceBetween(
        lower_source,
        "fn toInspectCall(self: *BodyContext",
        "fn prepareInspectMethodsAtNode(",
    );
    try expectContains(to_inspect, "self.frozen_inspect_method_calls");
    try expectContains(to_inspect, "deferred inspect method was not reserved before relation freeze");
}

test "Monotype iterator One bodies preserve explicit reachability guard frames" {
    const lower_source = @embedFile("monotype/lower.zig");
    const iterator = sourceSliceBetween(
        lower_source,
        "fn iteratorOneBranch(",
        "fn uninhabitedIteratorOneBranch(",
    );
    try expectContains(iterator, "self.constrainCheckedInterfaceToCell(self.view.bodies.pattern(for_.pattern).ty, item_cell)");
    try expectContains(iterator, "self.withIteratorOneRuntimeDemandGuardFrame(for_.pattern, step)");
    try expectContains(iterator, "defer self.runtime_demand_guard_frames = previous_runtime_demand_guard_frames");
    const relate = std.mem.find(u8, iterator, "self.constrainCheckedInterfaceToCell").?;
    const frame = std.mem.find(u8, iterator, "self.withIteratorOneRuntimeDemandGuardFrame").?;
    try std.testing.expect(relate < frame);

    const guard = sourceSliceBetween(
        lower_source,
        "fn withIteratorOneRuntimeDemandGuardFrame(",
        "fn runtimeDemandGuardFrameAddresses(",
    );
    try expectContains(guard, "guards[0] = step.one_payload_node");
    try expectContains(guard, "self.runtimeDemandGuardsForPattern(pattern_id, step.one_item.node)");
    try expectContains(guard, "runtimeDemandGuardFrameAddress(pattern_id, .iterator_one)");
    try expectNotContains(guard, "tagLabelText");
}

test "Monotype materialized success continuations use one root-pattern guard frame" {
    const lower_source = @embedFile("monotype/lower.zig");
    const root = sourceSliceBetween(
        lower_source,
        "fn lowerMaterializedPatternThen(",
        "fn lowerMaterializedPatternThenInner(",
    );
    try expectContains(root, ".root_pattern = pattern_id");
    try expectContains(root, ".root_node = try value_cell.toGraphNode(self.graph)");
    try expectNotContains(root, "publishes_runtime_result");

    const continuation = sourceSliceBetween(
        lower_source,
        "fn lowerPatternSuccessContinuation(",
        "fn lowerMaterializedPatternValueThen(",
    );
    try expectContains(continuation, "self.withPatternSuccessRuntimeDemandGuardFrame(guard)");
    try expectContains(continuation, "defer self.runtime_demand_guard_frames = previous_runtime_demand_guard_frames");
    try expectContains(continuation, "self.lowerBindingContinuation(continuation, result_cell)");
    try expectNotContains(continuation, "recordRuntimeResultSuccessGuards");
    try expectNotContains(continuation, "lowerPatternShellAtNode");
    try expectNotContains(continuation, "applyPatternLiteralGuardsAtCell");

    const pending = sourceSliceBetween(
        lower_source,
        "fn applyPendingMaterializedPatterns(",
        "fn lowerWrappedMaterializedPatternThen(",
    );
    try expectContains(pending, "self.lowerMaterializedPatternThenInner(");
    try expectContains(pending, "miss,\n                null,");
    try expectNotContains(pending, "self.lowerMaterializedPatternThen(");

    const addresses = sourceSliceBetween(
        lower_source,
        "fn runtimeDemandGuardFrameAddress(",
        "fn runtimeDemandGuardFrameAddresses(",
    );
    try expectContains(addresses, "kind: RuntimeDemandGuardFrameKind");
    try expectContains(addresses, "runtimeDemandGuardFrameAddress(guard.root_pattern, .pattern_success)");
    try expectContains(addresses, "runtimeDemandGuardFrameAddress(pattern_id, .iterator_one)");

    const lambda = sourceSliceBetween(
        lower_source,
        "fn lowerLambdaArgsAndBodyAtCell(",
        "fn lowerNestedFunctionAtNode(",
    );
    try expectContains(lambda, ".materialized_args = .{");
    try expectNotContains(lambda, "result_producer_guards");
    try expectNotContains(lower_source, "result_producer_guards");
}

test "Monotype materialized list patterns retain graph element provenance" {
    const lower_source = @embedFile("monotype/lower.zig");
    const materialized = sourceSliceBetween(
        lower_source,
        "fn lowerMaterializedPatternValueThen(",
        "fn runtimeCrashExpr(",
    );
    try expectContains(materialized, "const elem_node = try self.graph.listElementNode(value_node)");
    try expectContains(materialized, "self.addExprWithTypeCell(elem_cell");
    try expectContains(materialized, "self.lowerPatternPlanPlaceholderAtNode(pattern_id, elem_node");
    try expectContains(materialized, "self.applyPendingMaterializedPatterns(");
    try expectContains(materialized, "sequence_index == rest_index");
    try expectContains(materialized, "self.preRegisterPatternBindersAtNode(pattern_id, value_node)");
    try expectContains(materialized, "self.relateRecordRestNodeToSource(value_node, rest_node)");
    try expectContains(materialized, "fn recordRestNodeForPattern(");
    try expectNotContains(materialized, "activeTypeFromCell");
    try expectNotContains(materialized, "activeTypeFromNode");
    try expectNotContains(materialized, "lowerTypeView");
    try expectNotContains(materialized, "lowerPatternAtType(");
}

test "Monotype recursive materialization predicate stays paired with graph shell descent" {
    const lower_source = @embedFile("monotype/lower.zig");
    const predicate = sourceSliceBetween(
        lower_source,
        "fn patternNeedsExplicitBinding(",
        "const CheckedPatternRefutabilityAdapter",
    );
    try expectContains(predicate, ".as => |as| try self.patternNeedsExplicitBinding(as.pattern)");
    try expectContains(predicate, ".applied_tag => |tag|");
    try expectContains(predicate, ".nominal => |nominal| try self.patternNeedsExplicitBinding(nominal.backing_pattern)");
    try expectContains(predicate, ".tuple => |items|");
    try expectContains(predicate, "patternRequiresOwnMaterialization");
    try expectContains(predicate, "recordDestructsHaveOptionalField");

    const prepass = sourceSliceBetween(
        lower_source,
        "fn preRegisterPatternBindersAtNode(",
        "/// Lower the directly representable shell",
    );
    try expectContains(prepass, "std.AutoHashMap(PatternNodeVisit, void)");
    try expectContains(prepass, "self.graph.sameClass(existing_node, node)");
    try expectContains(prepass, "backing.node == node");
    try expectContains(prepass, "self.recordRestNodeForPattern(node, destructs, rest)");

    const shell = sourceSliceBetween(
        lower_source,
        "fn lowerPatternShellAtNode(",
        "fn lowerPatternPlanPlaceholderAtNode(",
    );
    try expectContains(shell, "fn lowerPatternShellAtNode(");
    try expectContains(shell, "std.AutoHashMap(PatternNodeVisit, void)");
    try expectContains(shell, "const key: PatternNodeVisit");
    try expectContains(shell, "active.contains(key)");
    try expectContains(shell, "defer _ = active.remove(key)");
    try expectContains(shell, "backing.node == representation_node");
    try expectContains(shell, "constructorRepresentationNode(node)");
    try expectContains(shell, "lowerPatternPlanPlaceholderAtNode");
    try expectContains(shell, ".applied_tag => |tag|");
    try expectContains(shell, ".nominal => |nominal|");
    try expectContains(shell, ".record_destructure => |destructs|");
    try expectContains(shell, ".tuple => |items|");
    try expectContains(lower_source, "recursively materialized pattern reached ordinary graph pattern lowering");
    try expectContains(lower_source, "allow_recursive_pattern_lowering_for_match");

    const guards = sourceSliceBetween(
        lower_source,
        "fn runtimeDemandGuardsForPattern(",
        "fn savePatternBinders(",
    );
    try expectContains(guards, "std.AutoHashMap(PatternNodeVisit, void)");
    try expectContains(guards, "const key: PatternNodeVisit");
    try expectContains(guards, "active.contains(key)");
    try expectContains(guards, "defer _ = active.remove(key)");
    try expectContains(guards, "backing.node == node");
    try expectContains(guards, "if (list.patterns.len != 0)");
}

test "Monotype lambda argument patterns retain graph provenance" {
    const lower_source = @embedFile("monotype/lower.zig");
    const lambda_args = sourceSliceBetween(
        lower_source,
        "fn lowerLambdaArgsAndBodyAtCell(",
        "const body_loc = self.exprLoc(body);",
    );
    try expectContains(lambda_args, "self.lowerShapeFreePatternAtCell(pattern_id, arg_cell)");
    try expectContains(lambda_args, "self.lowerPatternAtNode(pattern_id, arg_node)");
    try expectContains(lambda_args, ".ty = arg_cell");
    try expectContains(lambda_args, "} }, body_ret_cell);");
    try expectNotContains(lambda_args, "activeTypeFromNode(arg_node)");
    try expectNotContains(lambda_args, "activeTypeFromNode(ret_node)");
    try expectNotContains(lambda_args, "lowerPatternAtType(pattern_id");
}

test "Monotype returns consume the active specialization return cell" {
    const lower_source = @embedFile("monotype/lower.zig");
    const lower_return = sourceSliceBetween(
        lower_source,
        "fn lowerReturn(",
        "fn lowerComptimeRootExprAtCell(",
    );
    try expectContains(lower_source, "self.current_return_target = .{ .lambda = lambda_id, .cell = body_ret_cell }");
    try expectContains(lower_return, "ret.lambda != target.lambda");
    try expectContains(lower_return, "self.lowerExprAtTypeCell(ret.expr, target.cell)");
    try expectNotContains(lower_source, "returnTargetTypeCell");

    const lambda_node = sourceSliceBetween(
        lower_source,
        "fn lambdaFunctionNode(",
        "fn lowerLambdaExprAtNode(",
    );
    try expectContains(lambda_node, "const fn_node = try self.instNode(source_fn_ty)");
    try expectNotContains(lambda_node, "lowerExprTypeNode(lambda.body)");
}

test "Monotype encoding intrinsics consume producer-owned identity and result topology" {
    const lower_source = @embedFile("monotype/lower.zig");
    const selector = sourceSliceBetween(
        lower_source,
        "fn callsiteIntrinsicForMethodTarget(",
        "fn lowerFieldNamesRenameFieldNames(",
    );
    try expectContains(selector, "proc.intrinsic");
    try expectContains(selector, ".intrinsic => |intrinsic|");
    try expectContains(selector, "intrinsic.callsiteArity()");
    try expectNotContains(selector, "exportNameText");
    try expectNotContains(selector, "getIdentText");
    try expectNotContains(selector, "moduleForId");
    try expectNotContains(lower_source, "callsiteIntrinsicForBuiltinText");
    try expectNotContains(lower_source, "callsiteIntrinsicReturnType");

    const intrinsic_call = sourceSliceBetween(
        lower_source,
        "fn lowerCallsiteIntrinsicCallExpr(",
        "fn lowerCallsiteIntrinsicArgAtType(",
    );
    try expectContains(intrinsic_call, "intrinsic.requestResultSource()");
    try expectContains(intrinsic_call, "checkedMonoRequestNode(self.graph, callable.ret, callable.args[index], .exact)");
    try expectContains(intrinsic_call, "self.currentPhaseTypeForNode(callable.ret)");
    try expectNotContains(intrinsic_call, "generatedFieldNamesBackingValueFieldNames");

    switch (check.CheckedArtifact.IntrinsicId.field_names_rename_fields.requestResultSource()) {
        .argument => |index| try std.testing.expectEqual(@as(u8, 0), index),
        .declared_return => return error.TestUnexpectedResult,
    }
    try std.testing.expect(check.CheckedArtifact.IntrinsicId.parse_tag_union.requestResultSource() == .declared_return);
}

test "Monotype evidence chains retain checker-recorded lexical scope topology" {
    const lower_source = @embedFile("monotype/lower.zig");
    try expectNotContains(lower_source, "recursiveNestedEvidenceChainEql");
    try expectNotContains(lower_source, "evidence_frame_root_counts");
    try expectNotContains(lower_source, "parent.* = source_ctx.evidence");
    try expectNotContains(lower_source, "publicOpaqueUnificationType");
    try expectContains(lower_source, "const EvidenceScope = struct");
    try expectContains(lower_source, "const scope_record = view.templates.dispatch_scopes[raw_scope]");
    try expectContains(lower_source, "context.evidence");
    try expectContains(lower_source, "const_evidence_frames");

    try std.testing.expect(@hasField(check.ConstStore.ConstFn, "evidence_frames"));
    try std.testing.expect(@hasField(check.ConstStore.ConstFn, "evidence_frame_head"));
    try std.testing.expect(!@hasField(check.ConstStore.ConstFn, "evidence_frame_root_counts"));
}

test "Monotype generated-private call requests retain separate request nodes" {
    const lower_source = @embedFile("monotype/lower.zig");
    const full_request = sourceSliceBetween(
        lower_source,
        "fn instantiateTargetCallNodeFromMonoArgs",
        "fn exprCallResultEvidenceNode",
    );
    const iterator = sourceSliceBetween(
        lower_source,
        "fn instantiateIteratorPlanCallNodeFromCaller",
        "fn iteratorOperandNode",
    );

    try expectContains(full_request, "checkedMonoRequestNode");
    try expectContains(full_request, "functionRequestNode(self.graph, fn_node, request_args, request_ret)");
    try expectNotContains(lower_source, "instantiateTargetCallNodeFromMonoArgAtIndex");
    try expectNotContains(lower_source, "methodTargetMonoTypeFromArgAtIndexIsolated");
    try expectContains(iterator, "checkedMonoRequestNode");
    try expectContains(iterator, "self.graphFunctionNode(request_args, request_ret)");
    try expectNotContains(iterator, "self.graph.unify(formal_node, try self.graph.importMono(evidence_ty))");
}

test "hosted Try adaptation consumes checker-recorded nominal provenance" {
    const lower_source = @embedFile("monotype/lower.zig");
    const graph_relation = sourceSliceBetween(
        lower_source,
        "fn graphHostedTryInfoOrNull(",
        "const Builder = struct",
    );
    const adapter_source = sourceSliceBetween(
        lower_source,
        "fn hostedTryAdapterSourceType(",
        "fn hostedTryAdapterBody(",
    );
    try std.testing.expect(@hasField(check.CheckedModule.CheckedProcedureTemplate, "hosted_try_adapter"));
    try expectContains(lower_source, "template.hosted_try_adapter");
    try expectContains(graph_relation, "capability.def");
    try expectContains(graph_relation, "capability.ok_type_arg_index");
    try expectContains(graph_relation, "capability.err_type_arg_index");
    try expectContains(adapter_source, "capability orelse return null");
    try expectContains(lower_source, "sameTypeDef(named.def, capability.def)");
    try expectContains(lower_source, "tagByNameOrNull(backing_ty.ty, capability.ok_tag)");
    try expectContains(lower_source, "tagByNameOrNull(backing_ty.ty, capability.err_tag)");
    try expectNotContains(lower_source, "fn graphTryInfoOrNull(");
    try expectNotContains(lower_source, "fn graphTagByText(");
    try expectNotContains(lower_source, "fn tryInfoOrNull(");
    try expectNotContains(lower_source, "fn tagByTextOrNull(");
}

test "Monotype draft compaction preserves shared source files and procedure debug names" {
    const lower_source = @embedFile("monotype/lower.zig");
    const compaction = sourceSliceBetween(lower_source, "fn buildDraftCoreMaps", "fn draftSpecIdentityEql");
    try expectNotContains(compaction, "if (!retain) continue");
    try expectContains(compaction, "retain or kind == .source_files");
    try expectContains(lower_source, "core_id_mode: CoreIdMode");
    try expectContains(lower_source, ".identity => identity_start + raw");
    try expectNotContains(lower_source, "core_maps: ?*const Builder.DraftCoreMaps");
    try expectContains(lower_source, "registerDraftProcDebugNameForTemplate");
    try expectContains(lower_source, "draft.proc_debug_names.append");
}

test "Postcheck does not synthesize runtime crash for uninhabited lambda arguments" {
    const lower_source = @embedFile("monotype/lower.zig");
    try expectNotContains(lower_source, "called function with an uninhabited argument");
    try expectNotContains(lower_source, "uninhabited value reached Str.inspect");
}

test "Monotype consumes producer-recorded runtime-mode divergence and explicit unreachable markers" {
    const lower_source = @embedFile("monotype/lower.zig");
    try expectContains(lower_source, "statementDiverges(statement_id, self.checkedInlineExpectMode())");
    try expectContains(lower_source, "exprDiverges(expr_id, self.checkedInlineExpectMode())");
    try expectNotContains(lower_source, "reached code after checked control transfer");
    try expectNotContains(lower_source, "checkedAnyExprDivergesInLoweredRuntime");
    try std.testing.expect(@hasField(check.CheckedModule.StoredCheckedExpr, "diverges_without_inline_expects"));
    try std.testing.expect(@hasField(check.CheckedModule.StoredCheckedStatement, "diverges_without_inline_expects"));
    try expectContains(lower_source, "exprEvaluationMayBeElidedForInspect");
    try expectNotContains(lower_source, "exprIsSideEffectFreeCallableSyntax");
    try std.testing.expect(@hasField(check.CheckedModule.StoredCheckedExpr, "evaluation_may_be_elided_for_inspect"));
}

test "Monotype inspect-only lowering is gated by explicit demand" {
    const lower_source = @embedFile("monotype/lower.zig");
    try expectContains(lower_source, "self.inspectCallDemand(call)");
    try expectContains(lower_source, "if (demand != .inspect_only) return null;");
}

test "Lambda Mono consumes Lambda Solved through a read-only view" {
    const solved_ast_source = @embedFile("lambda_solved/ast.zig");
    try expectContains(solved_ast_source, "pub const ProgramView = struct");
    try expectContains(solved_ast_source, "pub fn view(self: *const Program) ProgramView");

    const lower_source = @embedFile("lambda_mono/lower.zig");
    try expectContains(lower_source, "solved: Solved.ProgramView");
    try expectContains(lower_source, "const solved_view = movedSolvedView(&owned, &program);");
    try std.testing.expect(std.mem.find(u8, lower_source, "self.solved.lifted.fns.items") == null);
    try std.testing.expect(std.mem.find(u8, lower_source, "self.solved.fn_tys.items") == null);
}

test "direct LIR verification consumes producer-owned specialization identities" {
    const mono_lower = @embedFile("lambda_mono/lower.zig");
    const direct_lower = @embedFile("solved_lir_lower.zig");
    try expectContains(mono_lower, "pub const SpecializationIdentity = struct");
    try expectContains(mono_lower, ".debug_specialization_identities = options.debug_specialization_identities");
    try expectContains(direct_lower, "by_identity.get(specializationIdentity(entry.spec))");
    try expectContains(direct_lower, "different types for an exact function specialization");
}

fn countDefinitions(source: []const u8, decl: []const u8) usize {
    var count: usize = 0;
    var rest = source;
    while (std.mem.find(u8, rest, decl)) |index| {
        count += 1;
        rest = rest[index + decl.len ..];
    }
    return count;
}

test "each primitive mapping has exactly one definition" {
    // `MonoType.Primitive` is an alias of `checked.CheckedPrimitive`
    // (monotype/type.zig), so nothing in the type system stops a consumer from
    // writing a second switch over the same 24 members. These tables are the
    // single source of truth; a copy would let two lowering paths decide a
    // layout, hasher op, or inspect op differently for the same primitive.
    const sources = [_][]const u8{
        @embedFile("common.zig"),
        @embedFile("monotype/type.zig"),
        @embedFile("monotype/lower.zig"),
        @embedFile("solved_lir_lower.zig"),
        @embedFile("boxy/lower.zig"),
        @embedFile("boxy/plan.zig"),
        @embedFile("boxy/layouts.zig"),
    };
    const single_definition = [_][]const u8{
        "fn primitiveLayout(",
        "fn primitiveInspectLowLevelOp(",
        "fn hasherWriteOp(",
    };
    for (single_definition) |decl| {
        var total: usize = 0;
        for (sources) |source| total += countDefinitions(source, decl);
        try std.testing.expectEqual(@as(usize, 1), total);
    }
    try expectContains(@embedFile("common.zig"), "pub fn primitiveLayout(");
    try expectContains(@embedFile("common.zig"), "pub fn primitiveInspectLowLevelOp(");
    try expectContains(@embedFile("common.zig"), "pub fn hasherWriteOp(");

    // The primitive-to-owner table lives beside `CheckedPrimitive` itself, so
    // post-check holds no definition of it at all and consumes the checked one.
    for (sources) |source| {
        try expectNotContains(source, "fn builtinOwnerForPrimitive(");
        try expectNotContains(source, "fn builtinOwnerFromPrimitive(");
    }
    const owner_fn = @typeInfo(@TypeOf(check.CheckedModule.builtinOwnerForPrimitive)).@"fn";
    try std.testing.expect(owner_fn.params.len == 1);
    try std.testing.expect(owner_fn.params[0].type.? == check.CheckedModule.CheckedPrimitive);
    try std.testing.expect(owner_fn.return_type.? == check.StaticDispatchRegistry.BuiltinOwner);
}

test "boxy representation queries have one definition on the plan" {
    // Boxy planning decides an unwrap, a descriptor argument, or a dictionary
    // argument by asking these; Boxy lowering emits that decision by asking the
    // same ones. A second copy lets the two disagree about the program they are
    // both describing, which is how the planner's
    // `repSubtreeHasDescriptorInOtherChildren` carve-out came to be absent from
    // both lowering copies. Every consumer goes through `Plan.RepQuery` /
    // `Plan.NamedRepQuery`; none re-derives.
    const consumers = [_][]const u8{
        @embedFile("boxy/lower.zig"),
        @embedFile("boxy/layouts.zig"),
    };
    const shared = [_][]const u8{
        "repSubtreeHasDescriptor",
        "repSubtreeHasDescriptorInner",
        "repSubtreeHasDescriptorInOtherChildren",
        "repSubtreeHasDictionary",
        "repSubtreeHasDictionaryInner",
        "repSubtreeHasDictionaryInOtherChildren",
        "repSubtreeContainsRep",
        "repSubtreeContainsRepInner",
        "structuralWrapperBackingRep",
        "descriptorArgumentIdentityRep",
        "dictionaryArgumentIdentityRep",
        "workerChildCanMatchUnwrappedCallRep",
        "workerChildCanMatchUnwrappedCallRepForDictionaries",
        "functionChildren",
        "functionIdentityRep",
        "requiredSingleChild",
        "sameChildRoleKind",
        "childRolesMatch",
        "findMatchingChildByRole",
        "findMatchingChildBySourceType",
        "findMatchingDictionaryChildBySourceType",
        "findMatchingTagPayloadInRep",
        "findMatchingTagPayloadInRowExtension",
        "findMatchingTagPayloadInRowExtensionInner",
        "recordFieldNameMatches",
        "tagLabelNameMatches",
    };
    const plan_source = @embedFile("boxy/plan.zig");
    inline for (shared) |name| {
        const decl = "fn " ++ name ++ "(";
        for (consumers) |source| {
            try std.testing.expectEqual(@as(usize, 0), countDefinitions(source, decl));
        }
        try std.testing.expectEqual(@as(usize, 1), countDefinitions(plan_source, decl));
    }

    // The exact-role and same-kind role comparisons are different predicates
    // and must not collapse into each other: `sameChildRoleKind` answers false
    // for every role carrying a payload.
    try expectContains(plan_source, "pub fn sameChildRoleKind(");
    try expectNotContains(plan_source, "fn sameChildRole(");
}

test "boxy stage tests share one set of checked-type fixtures" {
    const sources = [_][]const u8{
        @embedFile("boxy/lower.zig"),
        @embedFile("boxy/plan.zig"),
        @embedFile("boxy/layouts.zig"),
    };
    for (sources) |source| {
        try expectNotContains(source, "fn builtinNominal(");
        try expectNotContains(source, "fn fixtureTableIndex(");
    }
    try expectContains(@embedFile("boxy/test_fixtures.zig"), "pub fn builtinNominal(");
    try expectContains(@embedFile("boxy/test_fixtures.zig"), "pub fn tableIndex(");
}

test "one pattern walk collects bound locals" {
    // The lift pass's bound-set scan, its capture graph builder, and
    // SpecConstr's body-local scope all walk a pattern for the locals it binds.
    // They differ only in what they do at a binding site, never in which
    // positions bind, so the walk lives once on the lifted AST. Three copies
    // could come to disagree about a position (whether a list rest pattern
    // binds, say) and only one of them would be right.
    const consumers = [_][]const u8{
        @embedFile("monotype_lifted/lift.zig"),
        @embedFile("monotype_lifted/spec_constr.zig"),
    };
    for (consumers) |source| {
        try expectNotContains(source, "fn forEachBoundLocal(");
        // Each consumer keeps only a thin `bindPat` that names its binder.
        try expectContains(source, "forEachBoundLocal(");
    }
    try expectContains(@embedFile("monotype_lifted/ast.zig"), "pub fn forEachBoundLocal(");
}

test "the match compiler's stated consumers are its actual consumers" {
    // `match_tree.zig` and `mod.zig` both claimed this compiler was "shared by
    // both LIR lowerers". It is not: `.boxy` folds match branches into a
    // sequential chain of its own. A doc comment asserting a sharing invariant
    // that does not hold is worse than none, because the next reader budgets
    // for one match semantics and there are two. This pins the claim to the
    // imports, so whichever way the gap closes, the comment has to move with it.
    const boxy_uses_it = std.mem.find(u8, @embedFile("boxy/lower.zig"), "match_tree") != null;
    const header = @embedFile("match_tree.zig");
    const mod_source = @embedFile("mod.zig");
    if (boxy_uses_it) {
        try expectNotContains(header, "does not consume this yet");
        try expectNotContains(mod_source, "does not use it yet");
    } else {
        try expectContains(header, "does not consume this yet");
        try expectContains(mod_source, "does not use it yet");
        try expectContains(@embedFile("solved_lir_lower.zig"), "match_tree.Compiler(");
    }
}

test "post-check IR stores append spans through one implementation" {
    // All three post-check IRs address their flat side tables the same way, and
    // the append that produces a span was written out once per table per store
    // (33 copies of the same three lines). It lives in `Common` now, so a change
    // to how spans are allocated cannot land on one table and miss the rest.
    const stores = [_][]const u8{
        @embedFile("monotype/ast.zig"),
        @embedFile("monotype_lifted/ast.zig"),
        @embedFile("lambda_mono/ast.zig"),
    };
    for (stores) |source| {
        // The hand-written form these replaced. Its absence is the invariant.
        try expectNotContains(source, "const start: u32 = @intCast(self.expr_ids.len());");
        try expectContains(source, "Common.appendSpan(");
    }
    try expectContains(@embedFile("common.zig"), "pub fn appendSpan(");
    try expectContains(@embedFile("common.zig"), "pub fn appendNonemptySpan(");
}

test "const restoration has one implementation per shape" {
    // `Builder` and `BodyContext` emit into different stores and different
    // expression-data types, but which const shape maps to which expression
    // form, and the length invariants relating a stored aggregate to its
    // checked type, are one set of rules. Two copies could come to disagree
    // about, say, whether a stored record's length is checked against the
    // checked type at all.
    const source = @embedFile("monotype/lower.zig");
    const shapes = [_][]const u8{
        "constRestoreData",
        "constRestoreListData",
        "constRestoreList",
        "constRestoreTuple",
        "constRestoreRecord",
        "constRestoreTagPayloads",
    };
    inline for (shapes) |name| {
        try std.testing.expectEqual(@as(usize, 1), countDefinitions(source, "fn " ++ name ++ "("));
    }
    // The scope-local copies these replaced must not come back.
    try expectNotContains(source, "    fn restoreConstData(");
    try expectNotContains(source, "    fn restoreConstRecord(");
    try expectNotContains(source, "    fn restoreConstTuple(");
    try expectNotContains(source, "    fn restoreConstTagPayloads(");
}

test "post-check invariant helper is failure-only" {
    const fn_info = @typeInfo(@TypeOf(Common.invariant)).@"fn";
    try std.testing.expect(fn_info.return_type.? == noreturn);
}

test "hoist-preserving iterator producers are reachable through their method names" {
    // The checker pre-filters hoist classification on
    // `hoist_preserving_method_names` before resolving a receiver, so every
    // producer that answers "preserves hoistable source input" must be
    // reachable through one of those names; one that is not would silently
    // stop having its receiver hoisted. This lives here because the type
    // checker's own sources may not compare strings.
    const registry = check.StaticDispatchRegistry;
    for (registry.iterator_procedure_names) |entry| {
        if (!entry[1].preservesHoistableSourceInput()) continue;
        const qualified_name = entry[0];
        const start = if (std.mem.findScalarLast(u8, qualified_name, '.')) |dot| dot + 1 else 0;
        const method = qualified_name[start..];
        var reachable = false;
        for (registry.hoist_preserving_method_names) |candidate| {
            if (std.mem.eql(u8, method, candidate)) reachable = true;
        }
        if (!reachable) {
            std.debug.print("unreachable hoist-preserving producer: {s}\n", .{qualified_name});
        }
        try std.testing.expect(reachable);
    }
}
