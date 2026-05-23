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

test "Monotype has direct calls and no checked-only expression forms" {
    try std.testing.expect(@hasField(Mono.ExprData, "call_proc"));
    try std.testing.expect(@hasField(Mono.ExprData, "call_value"));
    try std.testing.expect(@hasField(Mono.ExprData, "structural_eq"));
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

test "Lifted functions own captures and expression lambdas are gone" {
    try std.testing.expect(@hasField(Lifted.Fn, "captures"));
    try std.testing.expect(@hasField(Lifted.ExprData, "fn_ref"));
    try std.testing.expect(@hasField(Lifted.ExprData, "call_proc"));
    try std.testing.expect(@hasField(Lifted.ExprData, "call_value"));

    try std.testing.expect(!@hasField(Lifted.ExprData, "lambda"));
    try std.testing.expect(!@hasField(Lifted.ExprData, "fn_def"));
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

test "post-check invariant helper is failure-only" {
    const fn_info = @typeInfo(@TypeOf(Common.invariant)).@"fn";
    try std.testing.expect(fn_info.return_type.? == noreturn);
}
