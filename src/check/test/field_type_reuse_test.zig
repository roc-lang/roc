//! Test that field access on polymorphic records reuses the same type variable
//!
//! For a function like `|record| record.x`, the type should be `{ x: a } -> a`
//! where the SAME type variable `a` appears in both the parameter and return type.

const std = @import("std");
const testing = std.testing;
const Check = @import("../Check.zig");
const can = @import("can");
const ModuleEnv = can.ModuleEnv;
const parse = @import("parse");
const types = @import("types");
const CIR = can.CIR;

test "field access reuses type variable" {
    const allocator = testing.allocator;

    // Simple function that accesses a field
    const source =
        \\module [getX]
        \\
        \\getX = |record| record.x
    ;

    // Parse and canonicalize
    var module_env = try ModuleEnv.init(allocator, source);
    defer module_env.deinit();
    module_env.module_name = "Test";
    module_env.common.source = source;
    try module_env.common.calcLineStarts(allocator);

    var module_parse = try parse.parse(&module_env.common, allocator);
    defer module_parse.deinit(allocator);

    try module_env.initCIRFields(allocator, "Test");
    var module_czer = try can.Can.init(&module_env, &module_parse, null);
    defer module_czer.deinit();
    try module_czer.canonicalizeFile();

    // Type check
    var module_checker = try Check.init(allocator, &module_env.types, &module_env, &.{}, &module_env.store.regions);
    defer module_checker.deinit();
    try module_checker.checkDefs();

    // Find the getX function
    const defs = module_env.store.sliceDefs(module_env.all_defs);
    var getX_expr_idx: ?CIR.Expr.Idx = null;
    for (defs) |def_idx| {
        const def = module_env.store.getDef(def_idx);
        const pattern = module_env.store.getPattern(def.pattern);
        if (pattern == .assign) {
            const ident_text = module_env.getIdent(pattern.assign.ident);
            if (std.mem.eql(u8, ident_text, "getX")) {
                getX_expr_idx = def.expr;
                break;
            }
        }
    }
    try testing.expect(getX_expr_idx != null);

    // Check the type of getX
    const getX_var = ModuleEnv.varFrom(getX_expr_idx.?);
    const getX_resolved = module_env.types.resolveVar(getX_var);

    // It should be a function
    try testing.expect(getX_resolved.desc.content == .structure);
    const func_type = switch (getX_resolved.desc.content.structure) {
        .fn_pure => |f| f,
        .fn_effectful => |f| f,
        .fn_unbound => |f| f,
        else => unreachable,
    };

    // Get the parameter type (should be a record with field x)
    const args = module_env.types.sliceVars(func_type.args);
    try testing.expect(args.len == 1);
    const param_var = args[0];
    const param_resolved = module_env.types.resolveVar(param_var);

    std.debug.print("\n=== Field Type Variable Reuse Test ===\n", .{});
    std.debug.print("Function: getX = |record| record.x\n", .{});
    std.debug.print("Expected: {{ x: a }} -> a (same 'a' in both places)\n", .{});

    // The parameter should be a record
    try testing.expect(param_resolved.desc.content == .structure);
    try testing.expect(param_resolved.desc.content.structure == .record_unbound);

    // Get the field type from the record
    const record_fields = module_env.types.getRecordFieldsSlice(param_resolved.desc.content.structure.record_unbound);
    try testing.expect(record_fields.len == 1);

    const field_name = record_fields.items(.name)[0];
    const field_var = record_fields.items(.var_)[0];
    const field_name_str = module_env.getIdent(field_name);
    try testing.expectEqualStrings("x", field_name_str);

    // Get the return type
    const return_var = func_type.ret;

    // THE KEY TEST: The field variable should BE the return variable!
    std.debug.print("Parameter type: {{ x: Var({}) }}\n", .{@intFromEnum(field_var)});
    std.debug.print("Return type: Var({})\n", .{@intFromEnum(return_var)});

    if (@intFromEnum(field_var) != @intFromEnum(return_var)) {
        std.debug.print("\nBUG: Different type variables used!\n", .{});
        std.debug.print("Field 'x' has type Var({})\n", .{@intFromEnum(field_var)});
        std.debug.print("Return has type Var({})\n", .{@intFromEnum(return_var)});
        std.debug.print("These should be the SAME variable for {{ x: a }} -> a\n", .{});
        return error.TypeVariableNotReused;
    }

    std.debug.print("\nSUCCESS: Same type variable used in both places!\n", .{});
    std.debug.print("Type is correctly: {{ x: a }} -> a\n", .{});
}