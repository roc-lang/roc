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
    const start_index = std.mem.find(u8, source, start) orelse {
        std.debug.print("missing source slice start marker: {s}\n", .{start});
        @panic("missing source slice start marker");
    };
    const after_start = source[start_index..];
    const end_index = std.mem.find(u8, after_start, end) orelse {
        std.debug.print("missing source slice end marker after {s}: {s}\n", .{ start, end });
        @panic("missing source slice end marker");
    };
    return after_start[0..end_index];
}

fn expectContains(haystack: []const u8, needle: []const u8) error{TestUnexpectedResult}!void {
    if (std.mem.find(u8, haystack, needle) == null) {
        std.debug.print("missing structural assertion: {s}\n", .{needle});
        return error.TestUnexpectedResult;
    }
}

fn expectNotContains(haystack: []const u8, needle: []const u8) error{TestUnexpectedResult}!void {
    if (std.mem.find(u8, haystack, needle) != null) {
        std.debug.print("unexpected structural assertion: {s}\n", .{needle});
        return error.TestUnexpectedResult;
    }
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
    const lower_call = sourceSliceBetween(lower_source, "fn lowerCall(self:", "fn directCallInstantiationSourceFnType");
    const lower_expr_type_node = sourceSliceBetween(lower_source, "fn lowerExprTypeNode", "fn sourceLocFor(");
    const lower_expr_at_cell = sourceSliceBetween(lower_source, "fn lowerExprAtTypeCell", "fn lowerPrimitiveNumeralAtNode");
    const lower_lookup_at_node = sourceSliceBetween(lower_source, "fn lowerLookupExprAtNode", "fn lowerProcedureUseValueAtNode");
    const lookup_type_node = sourceSliceBetween(lower_source, "fn lookupExprTypeNode", "fn fnTemplateForDirectCallAtNode");
    const lower_expr_inner = sourceSliceBetween(lower_source, "fn lowerExprInner", "fn lowerReturn");

    try expectContains(lower_call, "const checked_callee_node = try self.persistentCheckedBaseNode(call.source_fn_ty_payload)");
    try expectContains(lower_call, "const checked_callee = try self.graph.functionNodes(checked_callee_node)");
    try expectNotContains(lower_call, "lowerExprTypeNode(call.func)");
    try expectContains(lower_call, "try self.preLowerDirectCallOperands(call.args, null, &pre_lowered);");
    try expectContains(lower_call, "selections = try self.directCallSelectionsFromPublishedPlan(");
    try expectContains(lower_call, "const request_fn_node = try self.materializeCallSelectionSpan(");
    try expectNotContains(lower_call, "functionRequestNode(");
    try expectContains(lower_call, "const callee = try self.lowerExprAtCallConsumerRequest(");
    try expectContains(lower_call, "const callee_node = try self.builder.completePendingProducedNode(");
    try expectContains(lower_call, "const callee_fn = try self.graph.functionNodes(callee_node);");
    try std.testing.expect(std.mem.find(u8, lower_call, "try self.lowerExprType(call.func)") == null);
    try std.testing.expect(std.mem.find(u8, lower_call, "try self.lowerType(call.source_fn_ty_payload)") == null);
    try std.testing.expect(std.mem.find(u8, lower_call, "indirectCalleeMonoType") == null);
    try std.testing.expect(std.mem.find(u8, lower_call, "instantiateCallNodeFromCallerAtNode") == null);
    try expectNotContains(lower_call, "functionRequestFromAvailableProducedArgumentsWithGeneratedInterner");
    try expectNotContains(lower_call, "applyProducedTypeToRequest");
    try expectNotContains(lower_source, "fn checkedSelectionsForCallExpr(");
    try expectNotContains(lower_source, "fn publishCallOperandSelections(");
    try expectNotContains(lower_source, "fn propagateCheckedSelections(");
    try expectNotContains(lower_source, "specializationCallIdentityRelationsForExpr(");
    try expectNotContains(lower_source, "callArgumentEvidenceNode");
    try expectNotContains(lower_source, "callResultTypeNode");
    try expectNotContains(lower_source, "dispatchResultTypeNode");
    try expectNotContains(lower_source, "fieldAccessTypeNode");

    try expectContains(lower_expr_type_node, ".lookup_required => |resolved| try self.lookupExprTypeNode(expr_id, resolved)");
    try expectNotContains(lower_expr_type_node, "callResultTypeNode");
    try expectNotContains(lower_expr_type_node, "dispatchResultTypeNode");
    try expectNotContains(lower_expr_type_node, "fieldAccessTypeNode");
    try expectContains(lower_expr_at_cell, ".lookup_required => |resolved| break :blk try self.lowerLookupExprAtNode(checked_expr, resolved, expected_node)");
    try expectContains(lookup_type_node, "return try self.lowerTypeNode(checked_ty);");
    try std.testing.expect(std.mem.find(u8, lookup_type_node, "lookupExprMonoType") == null);
    try expectContains(lower_expr_inner, ".lookup_local => |lookup| return try self.lowerLookupExprAtNode(");
    try expectContains(lower_expr_inner, ".lookup_external => |resolved| return try self.lowerLookupExprAtNode(");
    try expectContains(lower_expr_inner, ".lookup_required => |resolved| return try self.lowerLookupExprAtNode(");
    try expectContains(lower_lookup_at_node, ".platform_required_const => |required|");
    try expectContains(lower_lookup_at_node, "required.const_use,\n                    try self.evidenceForUseSite(record.expr),");
    try expectContains(lower_lookup_at_node, ".platform_required_proc => |proc| return try self.lowerProcedureUseValueAtNode(");
    try expectContains(lower_lookup_at_node, "proc.procedure,\n                expected_node,");
    try expectNotContains(lower_source, "fn lowerLookupExprAtType");
    try expectNotContains(lower_source, "fn lowerExprType(");
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

test "Monotype lowering carries exact produced types without containment scans" {
    const solve_source = @embedFile("monotype/solve.zig");
    try expectNotContains(solve_source, "containsGeneratedPrivate");
    try expectNotContains(solve_source, "nodeIsGeneratedPrivateRoot");
    try expectNotContains(solve_source, "unifyIteratorOwnerStampedPublic");
    try expectNotContains(solve_source, "class_member_next");
    try expectNotContains(solve_source, "OpenFunctionInterfaceShape");
    try expectNotContains(solve_source, "typeCanSealFromExplicitEvidence");
    try expectNotContains(solve_source, ".checked_variable => .empty_tag_union");

    try expectNotContains(solve_source, "finalizeGeneratedIteratorRepresentations");
    try expectNotContains(solve_source, "finalizeGeneratedIteratorIdentities");
    try expectNotContains(solve_source, "bindGeneratedIteratorAuthoritativeTypes");
    try expectNotContains(solve_source, "InstGeneratedIterator");
    try expectNotContains(solve_source, "generated_iterator:");

    try expectNotContains(solve_source, "applyProducedTypeToRequest");
    try expectNotContains(solve_source, "applyCheckedTypeMapping");
    try expectNotContains(solve_source, "applyProducedTypePair");

    const ordinary_unify = sourceSliceBetween(
        solve_source,
        "pub fn unify(self: *InstGraph",
        "fn relationStamp(",
    );
    try expectContains(ordinary_unify, "unifyRootsTransitively(a, b)");
    try expectContains(ordinary_unify, "generated-private representation reached ordinary public/private graph unification");
    try expectNotContains(ordinary_unify, "isIteratorRepresentationTierRelation");

    const lower_source = @embedFile("monotype/lower.zig");
    try expectNotContains(lower_source, "containsGeneratedPrivate");
    try expectNotContains(lower_source, "nodeIsGeneratedPrivateRoot");
    try expectNotContains(lower_source, "unifyIteratorOwnerStampedPublic");
    try expectNotContains(lower_source, "class_member_next");
    try expectNotContains(lower_source, "OpenFunctionInterfaceShape");
    try expectNotContains(lower_source, "selectRequestRepresentation");

    try expectNotContains(lower_source, "selectExprRepresentationAtNode");

    try expectNotContains(lower_source, "InterfaceReplay");
    try expectNotContains(lower_source, "applyCheckedTemplateInterfaceRelations");
    try expectNotContains(lower_source, "applyCheckedTemplateInterfaceScopeRelations");
    try expectNotContains(solve_source, "pub fn provisionalTypeViewForNode");
    try expectNotContains(lower_source, "isNestedCallableExpr");
    try expectNotContains(lower_source, "isContextualValueExpr");
    try expectNotContains(lower_source, "reassignedPatternStorageCell");
    try expectNotContains(solve_source, "joinProducedTypeRepresentations");
    try expectNotContains(solve_source, "applyCompoundStorageRepresentation");
    try expectNotContains(solve_source, "materializeReassignedStorageRequest");

    const list_iterator_producer = sourceSliceBetween(
        lower_source,
        ".list_iter => {",
        ".str_iter_utf8 => {",
    );
    try expectContains(list_iterator_producer, "generatedIteratorNode(");
    try expectContains(list_iterator_producer, "public_fn.ret,");
    try expectContains(list_iterator_producer, "self.graph.listElementNode(request_args[0])");
    try expectNotContains(lower_source, "publicIteratorNodeWithItem(");
    const generated_iterator_producer = sourceSliceBetween(
        lower_source,
        "fn generatedIteratorNode(",
        "fn generatedIteratorBackingNode(",
    );
    try expectContains(generated_iterator_producer, "generatedIteratorPublicSource(public_iterator)");
    try expectContains(generated_iterator_producer, "lookupGeneratedIterator(public_source.def, item_node)");
    try expectNotContains(generated_iterator_producer, "existing_identity");
    try expectContains(generated_iterator_producer, "def.generated = ctx.identity");
    try expectContains(generated_iterator_producer, "generatedIteratorBackingNode(");
    const generated_iterator_builder = sourceSliceBetween(
        lower_source,
        "fn generatedIteratorNodeFromPublicSource(",
        "fn existingGeneratedIteratorNode(",
    );
    try expectContains(generated_iterator_builder, "addRecursiveGeneratedIterator(lookup.digest");
    try expectNotContains(generated_iterator_builder, "addRecursiveNode(");
    try expectNotContains(generated_iterator_builder, "registerGeneratedIteratorAtDigest(");

    const generated_call_identity = sourceSliceBetween(
        lower_source,
        "fn generatedNominalFromSelectedArguments(",
        "fn exactSelectionForChecked(",
    );
    try expectContains(generated_call_identity, "slot.generated_argument_source");
    try expectContains(generated_call_identity, ".exact_selection => self.exactSelectionForChecked(");
    try expectContains(generated_call_identity, ".checked_substitution => try self.materializeCallProjectionSubtree(");
    try expectContains(generated_call_identity, "slot.generated_argument_projection");
    try expectNotContains(generated_call_identity, "for (plan.projections");
    try expectNotContains(generated_call_identity, "specializationSlotOccurrences(slot)");
    try expectContains(generated_call_identity, "materializeCallProjectionSubtree(");
    try expectContains(generated_call_identity, ".concrete_checked => try self.persistentCheckedBaseNode(");
    try expectContains(generated_call_identity, "generatedIteratorNominalNode(");
    try expectNotContains(generated_call_identity, "instNominalBackingNode(");
    try expectNotContains(generated_call_identity, "generatedIteratorNode(public_node");
    try expectNotContains(generated_call_identity, "instantiateProducedOccurrenceWithSelections(");

    const projection_application = sourceSliceBetween(
        lower_source,
        "const SparseProjectionSelection = struct",
        "fn checkedCallRootEdge(",
    );
    try expectContains(projection_application, "fn sparseProjectionSelections(");
    try expectContains(projection_application, "fn applySparseProjectionSelection(");
    try expectContains(projection_application, "reverse_path");
    try expectContains(projection_application, "rebuildSpecializationProjectionParent(");
    try expectNotContains(projection_application, "subtree_end");
    try expectNotContains(projection_application, "blocked_by_exact_parent");
    try expectNotContains(projection_application, "selection_cells");
    try expectNotContains(projection_application, "base_nodes");
    try expectNotContains(projection_application, "checked-id hash table");

    const generated_call_slots = sourceSliceBetween(
        lower_source,
        "// Checking stores generated slots after every generated dependency.",
        "// Result-context identities may themselves be supplied by an operand",
    );
    try expectContains(generated_call_slots, "generatedNominalFromSelectedArguments(\n                plan,\n                slot,\n                selections.items,");
    try expectNotContains(generated_call_slots, "instantiateProducedOccurrenceWithSelections(slot.checked");

    const generated_nominal_lookup = sourceSliceBetween(
        solve_source,
        "pub fn lookupGeneratedNominal(",
        "pub fn registerGeneratedNominalAtDigest(",
    );
    try expectContains(generated_nominal_lookup, "implementation_args: []const NodeId");
    try expectContains(generated_nominal_lookup, "writer.writeNodeSpan(implementation_args)");
    try expectNotContains(generated_nominal_lookup, "backing: NodeId");
    try expectNotContains(generated_nominal_lookup, "writeNode(backing)");

    const generated_nominal_reservation = sourceSliceBetween(
        lower_source,
        "fn reserveGeneratedNominal(",
        "fn completeGeneratedNominal(",
    );
    try expectContains(generated_nominal_reservation, "lookupGeneratedNominal(template.def, public_args)");
    try expectContains(generated_nominal_reservation, ".existing = imported");
    try expectNotContains(generated_nominal_reservation, "backing_node");

    const parse_tag_union_preparation = sourceSliceBetween(
        lower_source,
        "fn prepareParseTagUnionCodecCall(",
        "fn graphErrorIsExactUnitTag(",
    );
    try expectContains(parse_tag_union_preparation, "reserveGeneratedNominal(target.args[1], &spec_args)");
    try expectContains(parse_tag_union_preparation, ".vacant => |identity|");
    try expectContains(parse_tag_union_preparation, "generatedParseTagUnionSpecBackingNode(shape_node)");

    try expectContains(solve_source, "generated_iterators_by_item: collections.DenseMap(NodeId");
    try expectContains(solve_source, "direct_request_selections: std.ArrayList(DirectRequestSelection)");
    const open_request_key = sourceSliceBetween(
        lower_source,
        "fn draftSelectionRequestKey(",
        "const DraftTemplateSpec = struct",
    );
    try expectContains(open_request_key, "graph.directRequestSelections(request_fn_node)");
    try expectContains(open_request_key, "selection.base.checked");
    try expectContains(open_request_key, "selection.produced");
    try expectNotContains(open_request_key, "graph.functionNodes");
    try expectNotContains(open_request_key, "writeNode");

    const dispatch_instantiation = sourceSliceBetween(
        lower_source,
        "fn storedDispatchRequestFromCheckedPlan(",
        "fn lookupExprTypeNode(",
    );
    try expectContains(dispatch_instantiation, "callable_plan: CallableDispatchPlan");
    try expectContains(dispatch_instantiation, "directCallSelectionsFromPublishedPlan(");
    try expectContains(dispatch_instantiation, "materializeCallSelectionSpan(");
    try expectContains(dispatch_instantiation, "@memset(available, false)");
    try expectNotContains(dispatch_instantiation, "lowerExprTypeNode");
    try expectNotContains(dispatch_instantiation, "applyCheckedTypeMapping");
    try expectContains(lower_source, "specializationValueFlowForExpr(checked_expr)");
    try expectNotContains(lower_source, "instantiateTemplateDispatchRelations");
    try expectNotContains(lower_source, "replayStoredEvidenceRelations");

    const target_request = sourceSliceBetween(
        lower_source,
        "fn methodTargetRequestFromCallsiteEdges(",
        "fn exactMethodTargetNode(",
    );
    try expectContains(target_request, "const no_new_callsite_arguments");
    try expectContains(target_request, "callsite.args,\n            no_new_callsite_arguments,");
    try expectContains(target_request, "callsite.args,\n            available,");
    try expectContains(target_request, "const checked_target = try self.instNode(lookup.target.callable_ty)");
    try expectContains(target_request, "checked_target,\n            callsite.ret,");
    try expectNotContains(target_request, "methodTargetSignatureNode(lookup)");
}

test "checked calls share one interned shape and have no whole-value plans" {
    const CheckedArtifact = check.CheckedArtifact;
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallPlan, "shape"));
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallShape, "slots"));
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallShape, "projections"));
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallShape, "argument_roots"));
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallShape, "result_root"));
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallShape, "dispatcher_root"));
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallShape, "target_argument_roots"));
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallShape, "target_result_root"));
    try std.testing.expect(!@hasField(CheckedArtifact.SpecializationCallPlan, "slots"));
    try std.testing.expect(!@hasField(CheckedArtifact.SpecializationCallPlan, "projections"));
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallConsumerBinding, "source_kind"));
    try std.testing.expect(@hasField(CheckedArtifact.CheckedProcedureTemplateTable, "specialization_call_shapes"));
    try std.testing.expect(@hasField(CheckedArtifact.CheckedProcedureTemplateTable, "specialization_call_shapes_by_type"));
    try std.testing.expect(@hasField(CheckedArtifact.CheckedProcedureTemplateTable, "specialization_call_root_edges"));
    try std.testing.expect(!@hasField(CheckedArtifact.CheckedProcedureTemplateTable, "specialization_value_plans_by_type"));
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallSlot, "generated_argument_source"));
    try std.testing.expect(@hasField(CheckedArtifact.SpecializationCallSlot, "generated_argument_projection"));

    const lower_source = @embedFile("monotype/lower.zig");
    try expectContains(lower_source, "plan.argument_roots[index]");
    try expectContains(lower_source, "plan.result_root");
    try expectContains(lower_source, "plan.dispatcher_root");
    try expectNotContains(lower_source, "for (plan.projections");
    try expectNotContains(lower_source, "fn callRootProjection(");
}

test "unsubstituted checked bases are persistent and checked-node reservations are recursion-only" {
    const lower_source = @embedFile("monotype/lower.zig");
    try expectContains(lower_source, "persistent_checked_base_nodes: std.AutoHashMap(solve.CheckedBaseKey, InstantiationNodeState)");
    try expectContains(lower_source, "fn usesPersistentCheckedBase(");
    try expectContains(lower_source, "return self.instantiation.authority == .checked_base and self.active_checked_selections == null;");

    const inst_node = sourceSliceBetween(
        lower_source,
        "fn instNode(self: *BodyContext",
        "fn checkedExprOccurrenceNode(",
    );
    try expectContains(inst_node, ".building => |maybe_reserved| if (maybe_reserved) |reserved|");
    try expectContains(inst_node, "recursive_checked_node_reservations");
    try expectNotContains(inst_node, "newNode(.{ .unresolved = InstVariable.placeholder() });\n        try self.putScopedNodeState");
}

test "record literals request and retain only immediate exact field nodes" {
    const lower_source = @embedFile("monotype/lower.zig");
    const record_literal = sourceSliceBetween(
        lower_source,
        "fn lowerRecordLiteralDirect(",
        "fn lowerRecordUpdateDirect(",
    );
    try expectContains(record_literal, "self.graph.recordConstructionFieldNode(");
    try expectContains(record_literal, "specializationRecordPlanForExpr(checked_expr)");
    try expectContains(record_literal, "projectionSelectionArgumentNode(plan, selections, index)");
    try expectContains(record_literal, "specializationProjectionOperandConsumerBindings(plan, index)");
    try expectContains(record_literal, "self.directSelectionsForCall(");
    try expectContains(record_literal, "fn lowerRecordLiteralFromExactChildren(");
    try expectContains(record_literal, ".ty = self.preLoweredChildNodeAt(children, field.value)");
    try expectNotContains(record_literal, "checkedRecordLiteralFieldType");
    try expectNotContains(lower_source, "instantiateCheckedTypeWithSelectionsAtAuthority");
    try expectNotContains(lower_source, "instantiateProducedOccurrenceWithSelections");
}

test "Monotype producers return and compose exact graph nodes directly" {
    const lower_source = @embedFile("monotype/lower.zig");
    const solve_source = @embedFile("monotype/solve.zig");

    const lower_expr = sourceSliceBetween(
        lower_source,
        "fn lowerExpr(self: *BodyContext",
        "fn lowerExprInner(",
    );
    try expectContains(lower_expr, "enterProducedOccurrenceInstantiation()");
    try expectContains(lower_expr, "self.exprTypeCell(lowered).toGraphNode(self.graph)");
    try expectContains(lower_expr, "return try self.requireLoweredExpr(");

    const contextual_expr = sourceSliceBetween(
        lower_source,
        "fn lowerExprAtTypeCellWithKnownDivergence(",
        "fn listNodeWithElement(",
    );
    try expectContains(contextual_expr, "enterProducedOccurrenceInstantiation()");

    const call = sourceSliceBetween(
        lower_source,
        "fn lowerCallAtExpectedNode(",
        "fn lowerDirectCallWithUninhabitedArgument(",
    );
    try expectNotContains(call, "produced_instantiation = TypeInstantiationContext.init(");

    const dispatch = sourceSliceBetween(
        lower_source,
        "fn lowerDispatchExprAtType(",
        "fn lowerClosedDirectLowLevelDispatch(",
    );
    try expectNotContains(dispatch, "produced_instantiation = TypeInstantiationContext.init(");

    const tuple = sourceSliceBetween(
        lower_source,
        "fn lowerTupleConstructorAtNodeWithRelation(",
        "fn lowerListConstructorAtNode(",
    );
    try expectContains(tuple, "produced_item.* = try self.exprTypeCell(item).toGraphNode(self.graph)");
    try expectContains(tuple, "self.graph.newNode(.{ .tuple = produced_items })");

    const list = sourceSliceBetween(
        lower_source,
        "fn lowerListConstructorAtNodeWithRelation(",
        "fn lowerRecordConstructorAtNode(",
    );
    try expectContains(list, "const produced_element = try self.exprTypeCell(lowered[0]).toGraphNode(self.graph)");
    try expectContains(list, "self.graph.newNode(.{ .list = produced_element })");
    try expectContains(list, "requireSameExactProducedValue(");

    const tag = sourceSliceBetween(
        lower_source,
        "fn lowerTagConstructorAtNodeWithRelation(",
        "fn lowerNominalConstructorAtNode(",
    );
    try expectContains(tag, "produced_payload.* = try self.exprTypeCell(payload_expr).toGraphNode(self.graph)");
    try expectContains(tag, "self.graph.newNode(.{ .tag_union");

    const low_level = sourceSliceBetween(
        lower_source,
        "fn lowerProducedLowLevelExprAtNode(",
        "fn lowerExprAtTypeCellInner(",
    );
    try expectContains(low_level, ".box_from_item => |box| try self.graph.newNode(.{ .box = arg_nodes[box.item_arg] })");
    try expectContains(low_level, ".box_item => |box| try self.graph.boxElementNode(arg_nodes[box.box_arg])");

    const lookup = sourceSliceBetween(
        lower_source,
        "fn lowerLookupExprAtNode(",
        "fn deferConstUseAtNode(",
    );
    try expectContains(lookup, "const local_cell = self.localTypeCell(local_id)");
    try expectContains(lookup, "self.addExprWithTypeCell(\n                local_cell,");

    const captures = sourceSliceBetween(
        lower_source,
        "fn lowerClosureCaptureExprSpan(",
        "fn closureFunctionNode(",
    );
    try expectContains(captures, "const cell = self.localTypeCell(local)");
    try expectContains(captures, "self.addExprWithTypeCell(\n                cell,");

    const function_result = sourceSliceBetween(
        solve_source,
        "pub fn completeFunctionResult(",
        "pub fn completeProducedSelection(",
    );
    try expectContains(function_result, "const exact_produced = self.find(produced_ret)");
    try expectContains(function_result, ".{ .redirect = exact_produced }");
}

test "generated identities are atomic and reserve before backing work" {
    const solve_source = @embedFile("monotype/solve.zig");
    const writer = sourceSliceBetween(
        solve_source,
        "fn writeNode(self: *GeneratedIdentityWriter",
        "fn writeNodeSpan(",
    );
    const atomic = std.mem.find(u8, writer, "InstGraph.isGeneratedPrivateRootContent(content)") orelse
        return error.TestExpectedEqual;
    const cycle_walk = std.mem.find(u8, writer, "for (self.visiting.items") orelse
        return error.TestExpectedEqual;
    try std.testing.expect(atomic < cycle_walk);
    try expectContains(writer, "self.writeBytes(&digest.bytes)");

    const generated_lookup = sourceSliceBetween(
        solve_source,
        "pub fn lookupGeneratedIterator(",
        "pub fn addRecursiveGeneratedIterator(",
    );
    const dense_lookup = std.mem.find(u8, generated_lookup, "generated_iterators_by_item.getPtr(item_root)") orelse
        return error.TestExpectedEqual;
    const sha_lookup = std.mem.find(u8, generated_lookup, "generatedIteratorInternDigest(public_def, item_root)") orelse
        return error.TestExpectedEqual;
    try std.testing.expect(dense_lookup < sha_lookup);

    const reservation = sourceSliceBetween(
        solve_source,
        "pub fn addRecursiveGeneratedIterator(",
        "pub fn registerGeneratedIteratorAtDigest(",
    );
    const publish_identity = std.mem.find(u8, reservation, "entry.value_ptr.* = reserved") orelse
        return error.TestExpectedEqual;
    const fill_backing = std.mem.find(u8, reservation, "try fill(context, reserved)") orelse
        return error.TestExpectedEqual;
    try std.testing.expect(publish_identity < fill_backing);
}

test "Monotype graph nodes cannot become TypeId views before freeze" {
    const solve_source = @embedFile("monotype/solve.zig");
    try expectNotContains(solve_source, "activeTypeViewForNode");
    try expectNotContains(solve_source, "activeIdentityViewForNode");
    try expectNotContains(solve_source, "activeSnapshotNode");
    try expectNotContains(solve_source, "node_snapshots");
    try expectNotContains(solve_source, "current_snapshots");
    try expectNotContains(solve_source, "typeHasActiveSnapshots");
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

test "Monotype direct uninhabited calls lower argument through graph cell" {
    const lower_source = @embedFile("monotype/lower.zig");
    const direct_call = sourceSliceBetween(
        lower_source,
        "fn lowerDirectCallWithUninhabitedArgument",
        "fn directCallInstantiationSourceFnType",
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
    try expectContains(statement, "try value_cell.toGraphNode(self.graph)");
    try expectContains(statement, "try self.lowerPatternAtNode(");
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
    try expectContains(record_rest, "const value = try self.lowerExpr(expr)");
    try expectContains(record_rest, "const value_cell = self.exprTypeCell(value)");
    try expectContains(record_rest, "const value_node = try value_cell.toGraphNode(self.graph)");
    try expectContains(record_rest, "addLocalWithBinderCell(self.builder.symbols.fresh(), value_cell, null)");
    try expectContains(record_rest, "self.graph.recordFieldNode(value_node, name)");
    try expectContains(record_rest, "self.lowerPatternAtNode(child, field_node)");
    try expectContains(record_rest, "lowerRecordRestValueWithTypeCell");
    try expectContains(record_rest, "self.lowerPatternAtNode(child, rest_node)");
    try expectNotContains(record_rest, "activeTypeFromNode(rest_node)");
    try expectNotContains(record_rest, "lowerPatternAtType(");
}

test "Monotype gates divergent relations and crash dispatches before type instantiation" {
    const lower_source = @embedFile("monotype/lower.zig");
    const divergent_call = sourceSliceBetween(
        lower_source,
        "fn lowerDivergentCallOperand",
        "fn hostedTryWidenedRequestNode",
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
    try expectContains(contextual_gate, "lowerDivergentExprAtTypeCell(checked_expr, graph_cell)");

    try expectNotContains(lower_source, "dispatchResultTypeNodeInPhase");
    try expectNotContains(lower_source, "callableDispatchResultTypeNodeInPhase");

    try expectNotContains(lower_source, "fn relateExprAtNode(");
    try expectNotContains(lower_source, "fn relateTagExprAtNode(");
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
    try expectContains(lower_source, "const lowered = try body_ctx.lowerTemplateBodyAtNode(");
    try expectContains(lower_source, "signature_relation,");
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

test "Monotype structural equality consumes exact lowered operand roots" {
    const lower_source = @embedFile("monotype/lower.zig");
    const equality_source = sourceSliceBetween(
        lower_source,
        "fn lowerDirectStructuralEqAtNode(",
        "fn lowerStructuralEqFromOperands(",
    );
    try expectContains(equality_source, "const lhs = try self.lowerExpr(eq.lhs)");
    try expectContains(equality_source, "const rhs = try self.lowerExpr(eq.rhs)");
    try expectContains(equality_source, "requireSameExactProducedValue(lhs_node, rhs_node)");
    try expectContains(equality_source, "deferStructuralDerivationOperandsAtNode(");
    try expectNotContains(lower_source, "structuralEqualityExprResultNode");
    try expectNotContains(lower_source, "structuralEqualityOperandNode");

    const dispatch_equality = sourceSliceBetween(
        lower_source,
        "fn lowerStructuralEqualityAtNode(",
        "const StructuralBinaryOperands = struct",
    );
    try expectContains(dispatch_equality, "self.graph.functionNodes(callable_node)");
    try expectContains(dispatch_equality, "deferStructuralEqOperandsAtNode");
    try expectNotContains(dispatch_equality, "self.graph.typeIsResolved(fn_nodes.args[0])");
    try expectNotContains(dispatch_equality, "activeTypeFromNode(fn_nodes.args[0])");
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

test "Monotype calls retain graph-native function provenance and lower operands once" {
    const lower_source = @embedFile("monotype/lower.zig");
    const call_source = sourceSliceBetween(
        lower_source,
        "fn lowerCallAtType(",
        "fn lowerDirectCallWithUninhabitedArgument(",
    );
    try expectContains(call_source, "const checked_fn_node = try self.persistentCheckedBaseNode(source_fn_ty);");
    try expectContains(call_source, "const planned = try self.lowerDirectCallOperandsByPlan(");
    try expectContains(call_source, "var fn_node = planned.request;");
    try expectContains(call_source, "const lowered_args = try self.lowerCallOperandsAtNodes(");
    try expectContains(call_source, "const checked_callee_node = try self.persistentCheckedBaseNode(call.source_fn_ty_payload)");
    try expectContains(call_source, "const checked_callee = try self.graph.functionNodes(checked_callee_node)");
    try expectNotContains(call_source, "lowerExprTypeNode(call.func)");
    try expectContains(call_source, "const callee = try self.lowerExprAtCallConsumerRequest(");
    try expectContains(call_source, ".ret_ty = DraftTypeCell.fromGraphNode(callee_fn.ret)");
    try expectNotContains(call_source, "producedCallableNode");
    try expectNotContains(call_source, "lowerExprAtTypeCell(\n            call.func");
    try expectNotContains(call_source, "prepareExprSpanAtNodes(call.args");
    try expectNotContains(call_source, "lowerPreparedExprSpanAtNodes(call.args");
    try expectNotContains(call_source, "indirectCalleeMonoType");
    try expectNotContains(call_source, "functionRequestFromAvailableProducedArgumentsWithGeneratedInterner");
    try expectNotContains(call_source, "applyProducedTypeToRequest");
    try expectNotContains(call_source, "BodyContext.initWithMethodScope");
    try expectNotContains(call_source, "call_ctx");

    const direct_request = std.mem.find(u8, call_source, "const planned = try self.lowerDirectCallOperandsByPlan(").?;
    const direct_specialize = std.mem.find(u8, call_source, "const callee = try self.fnTemplateForDirectCallAtNode").?;
    const direct_finish = direct_specialize + std.mem.find(
        u8,
        call_source[direct_specialize..],
        "const lowered_args = try self.lowerCallOperandsAtNodes(",
    ).?;
    try std.testing.expect(direct_request < direct_specialize);
    try std.testing.expect(direct_specialize < direct_finish);
}

test "Monotype open specialization lookup reuses only exact function interfaces" {
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
        try expectContains(lookup_source, "draftSelectionRequestKey(source_ctx.graph, request_fn_node)");
        try expectContains(lookup_source, "sameDirectRequestSelections(spec.body_request_fn_node, request_fn_node)");
        try expectContains(lookup_source, "spec.runtime_demand_guard_frames");
        try expectContains(lookup_source, "source_ctx.runtimeDemandGuardFrameAddresses()");
        try expectContains(lookup_source, "selection.add(raw_spec);");
        try expectContains(lookup_source, "if (selection.selected()) |raw_spec|");
        try expectNotContains(lookup_source, "functionInterfaceIterator(");
        try expectNotContains(lookup_source, "classMemberIterator(");
        try expectNotContains(lookup_source, "indexed_nodes");
        try expectNotContains(lookup_source, "seen_specs");
        try expectNotContains(lookup_source, "draftOpenCandidateQualifies(");
        try expectNotContains(lookup_source, "joinRecursiveFunctionInterface(");
        try expectNotContains(lookup_source, "initial_request_arg_classes");
        try expectNotContains(lookup_source, "partial_recursive");
        try expectNotContains(lookup_source, "functionInterfaceAnchor");
        try expectNotContains(lookup_source, "sameFunctionInterface");
    }
    try expectContains(template_source, "template_spec_lookup.get(lookup_address)");
    try expectContains(nested_source, "nested_spec_lookup.get(lookup_address)");
    try expectContains(nested_source, "std.meta.eql(spec.lexical_owner, source_ctx.draft.current_owner)");
}

test "Monotype prepared codec reuse requires the complete exact function request" {
    const lower_source = @embedFile("monotype/lower.zig");
    const lookup = sourceSliceBetween(
        lower_source,
        "fn preparedCodecCalleeAtNode(",
        "fn methodTargetCalleeAtNode(",
    );
    try expectContains(lookup, "sameExactFunctionRequest(prepared.callable_node, callable_node)");
    try expectContains(lookup, "prepared_codec_calls_by_lookup.get(PreparedCodecLookupAddress.init(lookup))");
    try expectContains(lookup, "for (ids.items) |id|");
    try expectNotContains(lookup, "for (self.draft.prepared_codec_calls.items)");
    try expectNotContains(lookup, "sameFunctionInterface(prepared.callable_node, callable_node)");
    try expectNotContains(lookup, "sameDirectRequestSelections(prepared.callable_node, callable_node)");
}

test "Monotype method type instantiation does not construct body contexts" {
    const lower_source = @embedFile("monotype/lower.zig");
    try expectNotContains(lower_source, "fn methodTargetContext(");

    const type_only = sourceSliceBetween(
        lower_source,
        "const TypeOnlyInstantiationScope = struct",
        "fn localMethodOwnerTemplate(",
    );
    try expectContains(type_only, "TypeInstantiationContext.init(");
    try expectContains(type_only, "fn typeOnlyCheckedNode(");
    try expectNotContains(type_only, "BodyContext.init");
    try expectNotContains(type_only, "BinderMap.init");

    const signature = sourceSliceBetween(
        lower_source,
        "fn methodTargetSignatureNode(",
        "const DispatchCrashReason",
    );
    try expectContains(signature, "self.typeOnlyCheckedNode(");
    try expectNotContains(signature, "BodyContext.init");

    const template_request = sourceSliceBetween(
        lower_source,
        "fn lowerDraftTemplateFromContext(",
        "if (partial_evidence.len > template.evidence_params.len)",
    );
    try expectContains(template_request, "enterTypeOnlyInstantiation(");
    try expectNotContains(template_request, "BodyContext.init");
}

test "Monotype match lowering projects exact scrutinee cells without checked root relations" {
    const lower_source = @embedFile("monotype/lower.zig");
    const match_source = sourceSliceBetween(
        lower_source,
        "fn lowerMatch(",
        "fn savePatternBinders(",
    );
    try expectContains(match_source, "const produced_scrutinee = try self.lowerExpr(match.cond)");
    try expectNotContains(match_source, "try_scrutinee_request");
    try expectNotContains(match_source, "lowerExprAtExactRequest(match.cond");
    try expectContains(match_source, "const destination = try self.producedConstructorNode(checked_nominal, scrutinee_node)");
    try expectContains(match_source, "scrutinee = try self.applyProducedExprToExactDestination(scrutinee, destination)");
    try expectContains(match_source, "try self.exprTypeCell(scrutinee).toGraphNode(self.graph)");
    try expectNotContains(match_source, "try self.graph.applyCheckedTypeMapping(");
    try expectContains(match_source, "try entry.ctx.preRegisterPatternBindersAtNode");
    try expectContains(match_source, "entry.ctx.runtime_demand_guard_frames = try entry.ctx.withMatchBranchRuntimeDemandGuardFrame");
    try expectContains(match_source, "try entry.ctx.lowerMatchBranchBody");
    try expectContains(match_source, "try entry.ctx.lowerMatchPatternAtNode");
    try expectNotContains(match_source, "resolvedTypeViewForNode(scrutinee_node)");
    try expectNotContains(match_source, "lowerPatternAtType(entry.pattern.pattern");
    try expectNotContains(lower_source, "rebindPreRegisteredPatternBindersAtNode");

    const prepare_binders = std.mem.find(u8, match_source, "try entry.ctx.preRegisterPatternBindersAtNode").?;
    const guards = std.mem.find(u8, match_source, "entry.ctx.runtime_demand_guard_frames =").?;
    const lower_body = std.mem.find(u8, match_source, "try entry.ctx.lowerMatchBranchBody").?;
    const lower_pattern = std.mem.find(u8, match_source, "try entry.ctx.lowerMatchPatternAtNode").?;
    try std.testing.expect(prepare_binders < guards);
    try std.testing.expect(guards < lower_body);
    try std.testing.expect(lower_body < lower_pattern);
    try expectNotContains(match_source, "prepareControlFlowResultSelection");

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
    try expectContains(cell_boundary, "const expected_node = try cell.toGraphNode(self.graph)");
    try expectContains(cell_boundary, "const graph_cell = DraftTypeCell.fromGraphNode(expected_node)");
    try expectContains(cell_boundary, "self.requireLoweredExprAtCell(checked_expr, expr, graph_cell, demand, lowered)");
    try expectNotContains(cell_boundary, "return switch (cell)");
    try expectNotContains(cell_boundary, ".sealed => |ty|");
    try expectNotContains(cell_boundary, "self.requireLoweredExpr(");

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
    try expectContains(lower_source, "body_ctx.runtime_demand_guard_frames = spec.demand_frame_floor");
    try expectContains(lower_source, ".demand_frame_floor = source_ctx.runtime_demand_guard_frames");
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
    try expectContains(lower_source, "graph.completeProducedSelection(boundary.witness_node, restored_node)");
    try expectNotContains(lower_source, "graph.unify(boundary.witness_node, restored_node)");
    try expectNotContains(lower_source, "body_draft.exprs.items[reservation_index].ty = DraftTypeCell.fromGraphNode(boundary.request_node)");
    try expectNotContains(lower_source, "runtimeResultProducerForDraftCallee");
    try expectNotContains(lower_source, "runtimeDemandHasUninhabitedProducerGuard");
}

test "Monotype closed direct dispatch preserves produced operand graphs" {
    const lower_source = @embedFile("monotype/lower.zig");

    const closed_low_level = sourceSliceBetween(
        lower_source,
        "fn lowerClosedDirectLowLevelDispatch(",
        "fn lowerDispatchWithUninhabitedArgument(",
    );
    try expectContains(closed_low_level, "const planned = try self.lowerDispatchOperandsByPlan(");
    try expectContains(closed_low_level, "planned.request");
    try expectContains(closed_low_level, "try self.publishCompletedCallResult(");
    try expectContains(closed_low_level, "try self.lowerDispatchOperandsAtNodes(");
    try expectNotContains(closed_low_level, "prepareDispatchOperandsAtNodes");
    try expectNotContains(closed_low_level, "lowerPreparedDispatchOperandsAtNodes");
    try expectNotContains(closed_low_level, "applyProducedTypeToRequest");
    try expectNotContains(closed_low_level, "lowerClosedDispatchOperandsAtTypes");
    try expectNotContains(closed_low_level, "functionRequestFromAvailableProducedArgumentsWithGeneratedInterner");

    const dispatch = sourceSliceBetween(
        lower_source,
        "fn lowerDispatchExprAtType(",
        "fn lowerClosedDirectLowLevelDispatch(",
    );
    try expectContains(dispatch, ".procedure => direct_graph_call = true");
    try expectNotContains(lower_source, "fn lowerClosedDirectProcedureDispatch(");
    try expectNotContains(lower_source, "closed_direct_specializations");

    try expectNotContains(lower_source, "fn lowerClosedDispatchOperandsAtNode(");

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
        "fn checkedExprOccurrenceNode(",
    );
    try expectNotContains(inst_node, "self.builder.lowerType(self.view, checked_ty)");
    try expectNotContains(inst_node, "self.graph.importMono(closed_ty)");
    try expectContains(inst_node, "if (self.scopedNodeState(scoped_ty)) |state|");
    try expectContains(inst_node, "try self.putScopedNodeState(scoped_ty, .{ .building = null })");
    try expectContains(inst_node, ".content => |content| try self.graph.newNode(content)");
    try expectContains(inst_node, ".content => |content| try self.graph.completeReservedProducedNode(reserved, content)");
    try expectNotContains(inst_node, "self.graph.unify(");

    const child_context = sourceSliceBetween(
        lower_source,
        "fn childContextWithTypeCells(",
        "fn enterRestoredLocalProcScope(",
    );
    try expectContains(child_context, "entry.value_ptr.* = .{ .building = node }");
    try expectContains(child_context, ".{ .ready = inherited }");
    try expectNotContains(child_context, "node_map.put(entry.key_ptr.*, entry.value_ptr.*)");

    const nominal_backing = sourceSliceBetween(
        lower_source,
        "fn ordinaryNominalNode(",
        "const NominalInstantiationSource = struct",
    );
    const reserve_identity = std.mem.find(u8, nominal_backing, "reserveOrdinaryNamedBacking(identity, backing_use)") orelse
        return error.TestExpectedEqual;
    const fill_backing = std.mem.find(u8, nominal_backing, "fillNominalDeclarationBackingNode(") orelse
        return error.TestExpectedEqual;
    try std.testing.expect(reserve_identity < fill_backing);
    try expectContains(nominal_backing, "try self.putScopedNode(backing, placeholder)");
    try expectContains(nominal_backing, ".content => |content| try self.graph.completeReservedProducedNode(placeholder, content)");
    try expectNotContains(nominal_backing, "self.graph.unify(");
    try expectNotContains(nominal_backing, "attachCheckedBaseAlias");

    const solve_source = @embedFile("monotype/solve.zig");
    try expectNotContains(solve_source, "nominal_backings");
    try expectNotContains(solve_source, "nominalBackingNode(");
    const ordinary_reservation = sourceSliceBetween(
        solve_source,
        "pub fn reserveOrdinaryNamedBacking(",
        "fn registerRowParent(",
    );
    const reserve_backing = std.mem.find(u8, ordinary_reservation, "self.appendDistinctNode(.{ .unresolved = InstVariable.placeholder() })") orelse
        return error.TestExpectedEqual;
    const register_named = std.mem.find(u8, ordinary_reservation, "self.newNode(.{ .named = completed })") orelse
        return error.TestExpectedEqual;
    try expectContains(ordinary_reservation, "if (self.existingNamedIdentity(canonical)) |existing|");
    try expectContains(ordinary_reservation, ".backing = backing");
    try std.testing.expect(reserve_backing < register_named);

    const complete_reserved = sourceSliceBetween(
        solve_source,
        "pub fn completeReservedProducedNode(",
        "pub fn addRecursiveNode(",
    );
    try expectContains(complete_reserved, "try self.redirectRoot(node, root)");
    try expectNotContains(complete_reserved, "invalidateActiveSnapshots");
    try expectNotContains(complete_reserved, "try self.setContent(root");

    const produced_occurrence = sourceSliceBetween(
        lower_source,
        "fn producedOccurrenceNode(",
        "fn checkedTypeContainsError(",
    );
    try expectContains(produced_occurrence, "self.instantiation = self.produced_instantiation");
    try expectContains(produced_occurrence, "return try self.instNode(checked_ty)");
    try expectNotContains(produced_occurrence, "BodyContext.initWithMethodScope");

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
        "fn lowerLambdaTemplateAtNodeWithReturnRelation(",
    );
    try expectContains(inspect_source, "try self.deferInspectAtNode(local_expr, arg_node, ret_ty)");
    try expectContains(inspect_source, ".impossibility_proof = try self.currentRuntimeImpossibilityProof(null)");
    try expectNotContains(inspect_source, "self.graph.typeIsResolved(arg_node)");
    try expectNotContains(inspect_source, "activeTypeFromNode(arg_node)");

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
    try expectContains(inspect_call, ".sealed = try self.builder.closedFunctionType(&.{value_ty}, str_ty)");
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
    try expectContains(iterator, "self.publishExactCheckedPatternAtCell(for_.pattern, item_cell)");
    try expectContains(iterator, "self.withIteratorOneRuntimeDemandGuardFrame(for_.pattern, step)");
    try expectContains(iterator, "defer self.runtime_demand_guard_frames = previous_runtime_demand_guard_frames");
    const relate = std.mem.find(u8, iterator, "self.publishExactCheckedPatternAtCell").?;
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
    try expectNotContains(materialized, "relateRecordRestNodeToSource");
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
    try expectContains(predicate, ".as => |as| self.patternNeedsExplicitBinding(as.pattern)");
    try expectContains(predicate, ".applied_tag => |tag|");
    try expectContains(predicate, ".nominal => |nominal| self.patternNeedsExplicitBinding(nominal.backing_pattern)");
    try expectContains(predicate, ".tuple => |items|");
    try expectContains(predicate, "patternRequiresOwnMaterialization");

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

test "Monotype returns share the active specialization result selection" {
    const lower_source = @embedFile("monotype/lower.zig");
    const lower_return = sourceSliceBetween(
        lower_source,
        "fn lowerReturn(",
        "fn lowerComptimeRootExprAtCell(",
    );
    try expectContains(lower_source, "self.current_return_target = .{ .lambda = lambda_id, .selection = &return_selection }");
    try expectContains(lower_return, "ret.lambda != target.lambda");
    try expectContains(lower_return, "self.lowerExprAtExactRequest(ret.expr, target.selection.declared)");
    try expectContains(lower_return, ".checked_mapping, .exact_producer => try self.lowerExpr(ret.expr)");
    try expectContains(lower_return, "self.includeControlFlowResult(target.selection, value)");
    try expectContains(lower_return, ".target = target.selection.selected");
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
    try expectContains(intrinsic_call, "const request_ret = callable.args[index]");
    try expectContains(intrinsic_call, "functionRequestNode(self.graph, callable_node, callable.args, request_ret)");
    try expectContains(intrinsic_call, "self.finalTypeForNode(callable.ret)");
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
        "fn lookupExprTypeNode",
    );
    const iterator = sourceSliceBetween(
        lower_source,
        "fn lowerIteratorDispatch(",
        "fn lowerGeneratedIteratorDispatch(",
    );

    try expectContains(full_request, "directCallSelectionsFromPublishedPlan(");
    try expectContains(full_request, "materializeCallSelectionSpan(");
    try expectNotContains(full_request, "functionRequestNode(");
    try expectNotContains(lower_source, "instantiateTargetCallNodeFromMonoArgAtIndex");
    try expectNotContains(lower_source, "methodTargetMonoTypeFromArgAtIndexIsolated");
    try expectContains(iterator, "produced_nodes[operand_index] = try self.exprTypeCell(produced_exprs[operand_index]).toGraphNode(self.graph)");
    try expectContains(iterator, "directCallSelectionsFromPublishedPlan(");
    try expectContains(iterator, "materializeCallSelectionSpan(");
    try expectNotContains(iterator, "functionRequestNode(");
    try expectNotContains(lower_source, "checkedMonoRequestNode");
    try expectNotContains(lower_source, "functionRequestFromProducedArgumentsWithGeneratedInterner");
    try expectNotContains(lower_source, "instantiateIteratorPlanCallNodeFromCaller");
}

test "Monotype lowering never reconciles complete runtime type graphs" {
    const lower_source = @embedFile("monotype/lower.zig");
    const runtime_lowering = sourceSliceBetween(
        lower_source,
        "const std = @import(\"std\");",
        "test \"draft runtime guard frames retain exact ancestry\"",
    );
    try expectNotContains(runtime_lowering, ".graph.unify(");
    try expectContains(runtime_lowering, "completeProducedSelection(");
    try expectContains(runtime_lowering, "completeOpenTagRowExtension(");
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

test "post-check invariant helper is failure-only" {
    const fn_info = @typeInfo(@TypeOf(Common.invariant)).@"fn";
    try std.testing.expect(fn_info.return_type.? == noreturn);
}
