//! Structural tests for MIR-family type-state boundaries.
//!
//! These are intentionally shape tests, not end-to-end eval tests. They guard
//! against accidentally reintroducing obsolete post-check nodes or name-based
//! row lookup into later MIR stages.

const std = @import("std");

const Mono = @import("mono/mod.zig");
const MonoRow = @import("mono_row/mod.zig");
const Lifted = @import("lifted/mod.zig");
const LambdaSolved = @import("lambda_solved/mod.zig");
const Executable = @import("executable/mod.zig");

fn unionHasField(comptime Union: type, comptime name: []const u8) bool {
    inline for (@typeInfo(Union).@"union".fields) |field| {
        if (std.mem.eql(u8, field.name, name)) return true;
    }
    return false;
}

fn unionFieldType(comptime Union: type, comptime name: []const u8) type {
    inline for (@typeInfo(Union).@"union".fields) |field| {
        if (std.mem.eql(u8, field.name, name)) return field.type;
    }
    @compileError("missing union field: " ++ name);
}

fn structHasField(comptime Struct: type, comptime name: []const u8) bool {
    inline for (@typeInfo(Struct).@"struct".fields) |field| {
        if (std.mem.eql(u8, field.name, name)) return true;
    }
    return false;
}

fn structFieldType(comptime Struct: type, comptime name: []const u8) type {
    inline for (@typeInfo(Struct).@"struct".fields) |field| {
        if (std.mem.eql(u8, field.name, name)) return field.type;
    }
    @compileError("missing struct field: " ++ name);
}

test "mono MIR output has resolved call/equality nodes and no dispatch survival nodes" {
    const ExprData = Mono.Ast.Expr.Data;

    try std.testing.expect(unionHasField(ExprData, "call_proc"));
    try std.testing.expect(unionHasField(ExprData, "proc_value"));
    try std.testing.expect(unionHasField(ExprData, "call_value"));
    try std.testing.expect(unionHasField(ExprData, "structural_eq"));
    try std.testing.expect(unionHasField(ExprData, "bool_not"));

    try std.testing.expect(!unionHasField(ExprData, "dispatch_call"));
    try std.testing.expect(!unionHasField(ExprData, "type_dispatch_call"));
    try std.testing.expect(!unionHasField(ExprData, "method_eq"));
}

test "row-finalized mono MIR uses finalized row ids in runtime row operations" {
    const ExprData = MonoRow.Ast.Expr.Data;
    const PatData = MonoRow.Ast.Pat.Data;

    const access = unionFieldType(ExprData, "access");
    try std.testing.expect(structFieldType(access, "field") == MonoRow.RecordFieldId);

    const record_expr = unionFieldType(ExprData, "record");
    try std.testing.expect(structFieldType(record_expr, "shape") == MonoRow.RecordShapeId);
    try std.testing.expect(structHasField(record_expr, "eval_order"));
    try std.testing.expect(structHasField(record_expr, "assembly_order"));

    const tag_expr = unionFieldType(ExprData, "tag");
    try std.testing.expect(structFieldType(tag_expr, "union_shape") == MonoRow.TagUnionShapeId);
    try std.testing.expect(structFieldType(tag_expr, "tag") == MonoRow.TagId);
    try std.testing.expect(structHasField(tag_expr, "eval_order"));
    try std.testing.expect(structHasField(tag_expr, "assembly_order"));

    const tag_payload = unionFieldType(ExprData, "tag_payload");
    try std.testing.expect(structFieldType(tag_payload, "payload") == MonoRow.TagPayloadId);

    const record_pat = unionFieldType(PatData, "record");
    try std.testing.expect(structFieldType(record_pat, "shape") == MonoRow.RecordShapeId);

    const tag_pat = unionFieldType(PatData, "tag");
    try std.testing.expect(structFieldType(tag_pat, "union_shape") == MonoRow.TagUnionShapeId);
    try std.testing.expect(structFieldType(tag_pat, "tag") == MonoRow.TagId);
}

test "lifted MIR has explicit capture refs and procedure-owned direct-call metadata" {
    const ExprData = Lifted.Ast.Expr.Data;
    const Proc = Lifted.Lift.Proc;
    const Program = Lifted.Lift.Program;

    try std.testing.expect(unionHasField(ExprData, "capture_ref"));
    try std.testing.expect(unionHasField(ExprData, "proc_value"));
    try std.testing.expect(unionHasField(ExprData, "call_value"));
    try std.testing.expect(structHasField(Proc, "direct_calls"));
    try std.testing.expect(structHasField(Program, "direct_call_targets"));
}

test "lambda-solved MIR preserves explicit callable values before executable lowering" {
    const ExprData = LambdaSolved.Ast.Expr.Data;

    try std.testing.expect(unionHasField(ExprData, "capture_ref"));
    try std.testing.expect(unionHasField(ExprData, "proc_value"));
    try std.testing.expect(unionHasField(ExprData, "call_value"));
    try std.testing.expect(unionHasField(ExprData, "call_proc"));
}

test "executable MIR has final callable operations and no source dispatch nodes" {
    const ExprData = Executable.Ast.Expr.Data;

    try std.testing.expect(unionHasField(ExprData, "call_direct"));
    try std.testing.expect(unionHasField(ExprData, "call_erased"));
    try std.testing.expect(unionHasField(ExprData, "callable_set_value"));
    try std.testing.expect(unionHasField(ExprData, "callable_match"));
    try std.testing.expect(unionHasField(ExprData, "packed_erased_fn"));
    try std.testing.expect(unionHasField(ExprData, "source_match"));

    try std.testing.expect(!unionHasField(ExprData, "dispatch_call"));
    try std.testing.expect(!unionHasField(ExprData, "type_dispatch_call"));
    try std.testing.expect(!unionHasField(ExprData, "method_eq"));
    try std.testing.expect(!unionHasField(ExprData, "call_proc"));
    try std.testing.expect(!unionHasField(ExprData, "call_value"));
}

test "Bool runtime values are represented by ordinary executable tag construction" {
    const ExprData = Executable.Ast.Expr.Data;

    try std.testing.expect(unionHasField(ExprData, "tag"));
    try std.testing.expect(!unionHasField(ExprData, "bool_value"));
    try std.testing.expect(!unionHasField(ExprData, "bool_bridge"));
    try std.testing.expect(!unionHasField(ExprData, "bool_runtime"));
}

test "annotation-only checked expressions are not representable in post-check MIR" {
    const mono_expr_data = Mono.Ast.Expr.Data;
    const row_expr_data = MonoRow.Ast.Expr.Data;
    const lifted_expr_data = Lifted.Ast.Expr.Data;
    const lambda_solved_expr_data = LambdaSolved.Ast.Expr.Data;
    const executable_expr_data = Executable.Ast.Expr.Data;

    try std.testing.expect(!unionHasField(mono_expr_data, "anno_only"));
    try std.testing.expect(!unionHasField(row_expr_data, "anno_only"));
    try std.testing.expect(!unionHasField(lifted_expr_data, "anno_only"));
    try std.testing.expect(!unionHasField(lambda_solved_expr_data, "anno_only"));
    try std.testing.expect(!unionHasField(executable_expr_data, "anno_only"));

    const mono_specialize_source = @embedFile("mono/specialize.zig");
    try std.testing.expect(std.mem.indexOf(
        u8,
        mono_specialize_source,
        "mono body lowering reached annotation-only procedure body without checked backing expression",
    ) != null);
    try std.testing.expect(std.mem.indexOf(
        u8,
        mono_specialize_source,
        "mono body lowering received a non-runtime checked expression form",
    ) != null);
}
