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

test "Monotype has direct calls and no checked-only expression forms" {
    try std.testing.expect(@hasField(Mono.ExprData, "call_proc"));
    try std.testing.expect(@hasField(Mono.ExprData, "call_value"));
    try std.testing.expect(@hasField(Mono.ExprData, "structural_eq"));
    try std.testing.expect(@hasField(Mono.ExprData, "structural_hash"));
    try std.testing.expect(@hasField(Mono.ExprData, "loop_"));

    try std.testing.expect(!@hasField(Mono.ExprData, "dispatch_call"));
    try std.testing.expect(!@hasField(Mono.ExprData, "type_dispatch_call"));
    try std.testing.expect(!@hasField(Mono.ExprData, "method_call"));
    try std.testing.expect(!@hasField(Mono.ExprData, "method_eq"));
    try std.testing.expect(!@hasField(Mono.ExprData, "anno_only"));
    try std.testing.expect(!@hasField(Mono.ExprData, "for_"));
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
    try expectContains(lower_record_expr, "self.builder.program.types.fieldSpan(target_fields)[i]");
    try std.testing.expect(std.mem.find(u8, lower_record_expr, "for (target_fields") == null);
}

test "Monotype lookup lowering uses explicit resolved use types" {
    const lower_source = @embedFile("monotype/lower.zig");
    const lower_call = sourceSliceBetween(lower_source, "fn lowerCall", "fn directCallInstantiationSourceFnType");
    const lower_expr_type = sourceSliceBetween(lower_source, "fn lowerExprType", "fn lowerExpr(self:");
    const lower_expr_at_type = sourceSliceBetween(lower_source, "fn lowerExprAtType", "fn sameType");
    const lower_lookup_at_type = sourceSliceBetween(lower_source, "fn lowerLookupExprAtType", "fn lowerProcedureUseValue");

    try expectContains(lower_call, "const fn_ty = (try self.indirectCalleeMonoType(call.func, call.args, expected_ret_ty)) orelse fn_ty: {");
    try expectContains(lower_call, "break :fn_ty try call_ctx.instantiateCallTypeFromCallerAtType(call.source_fn_ty_payload, self, checked_ret_ty, call.args, expected_ret_ty);");
    try std.testing.expect(std.mem.find(u8, lower_call, "try self.lowerExprType(call.func)") == null);
    try std.testing.expect(std.mem.find(u8, lower_call, "try self.lowerType(call.source_fn_ty_payload)") == null);

    try expectContains(lower_expr_type, ".lookup_required => |resolved| try self.activeNodeFromType(try self.lookupExprMonoType(expr.ty, resolved))");
    try expectContains(lower_expr_at_type, ".lookup_required => |resolved| return try self.lowerLookupExprAtType(expr.ty, resolved, ty)");
    try expectContains(lower_lookup_at_type, ".platform_required_const => |required| return try self.restoreConstUseAtType(required.const_use, ty)");
    try expectContains(lower_lookup_at_type, ".platform_required_proc => |proc| return try self.lowerProcedureUseValue(proc.procedure, ty)");
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

    try std.testing.expect(!@hasField(Lifted.ExprData, "dispatch_call"));
    try std.testing.expect(!@hasField(Lifted.ExprData, "anno_only"));
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
        else => {},
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
    try expectContains(rewrite_expr, "self.output.exprs.items[index].data = .{ .fn_ref");
    try expectContains(rewrite_expr, "self.output.exprs.items[index].data = .{ .call_proc");

    const lift_lambda = sourceSliceBetween(lifted_source, "fn liftLambda", "fn reserveFn");
    try expectContains(lift_lambda, "self.output.exprs.items[@intFromEnum(expr_id)].data = .{ .fn_ref = .{");

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

test "post-check invariant helper is failure-only" {
    const fn_info = @typeInfo(@TypeOf(Common.invariant)).@"fn";
    try std.testing.expect(fn_info.return_type.? == noreturn);
}
