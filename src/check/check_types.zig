const std = @import("std");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const types_mod = @import("../types.zig");
const can = @import("canonicalize.zig");
const unifier = @import("check_types/unify.zig");
const occurs = @import("check_types/occurs.zig");
const problem = @import("check_types/problem.zig");
const snapshot = @import("check_types/snapshot.zig");
const CIR = @import("./canonicalize/CIR.zig");
const ModuleEnv = @import("../base/ModuleEnv.zig");

const testing = std.testing;
const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const Region = base.Region;
const ModuleWork = base.ModuleWork;

const Var = types_mod.Var;
const exitOnOom = collections.utils.exitOnOom;

const Self = @This();

gpa: std.mem.Allocator,
// not owned
types: *types_mod.Store,
can_ir: *const CIR,
// owned
snapshots: snapshot.Store,
problems: problem.Store,
unify_scratch: unifier.Scratch,
occurs_scratch: occurs.Scratch,

/// Init type solver
/// Does *not* own types_store or can_ir, but *does* own other fields
pub fn init(
    gpa: std.mem.Allocator,
    types: *types_mod.Store,
    can_ir: *const CIR,
) std.mem.Allocator.Error!Self {
    return .{
        .gpa = gpa,
        .types = types,
        .can_ir = can_ir,
        .snapshots = snapshot.Store.initCapacity(gpa, 512),
        .problems = problem.Store.initCapacity(gpa, 64),
        .unify_scratch = unifier.Scratch.init(gpa),
        .occurs_scratch = occurs.Scratch.init(gpa),
    };
}

/// Deinit owned fields
pub fn deinit(self: *Self) void {
    self.problems.deinit(self.gpa);
    self.snapshots.deinit();
    self.unify_scratch.deinit();
    self.occurs_scratch.deinit();
}

/// Unify two types
pub fn unify(self: *Self, a: Var, b: Var) unifier.Result {
    return unifier.unify(
        self.can_ir.env,
        self.types,
        &self.problems,
        &self.snapshots,
        &self.unify_scratch,
        &self.occurs_scratch,
        a,
        b,
    );
}

/// Check the types for all defs
pub fn checkDefs(self: *Self) void {
    const defs_slice = self.can_ir.store.sliceDefs(self.can_ir.all_defs);
    for (defs_slice) |def_idx| {
        self.checkDef(def_idx);
    }
}

/// Check the types for a single definition
fn checkDef(self: *Self, def_idx: CIR.Def.Idx) void {
    const def = self.can_ir.store.getDef(def_idx);
    // TODO: Check patterns
    self.checkExpr(def.expr);

    // If there's a type annotation, unify the expression with the annotation's signature
    if (def.annotation) |anno_idx| {
        const annotation = self.can_ir.store.getAnnotation(anno_idx);

        _ = self.unify(@enumFromInt(@intFromEnum(def.expr)), annotation.signature);
        _ = self.unify(@enumFromInt(@intFromEnum(def_idx)), annotation.signature);
    } else {
        _ = self.unify(@enumFromInt(@intFromEnum(def_idx)), @enumFromInt(@intFromEnum(def.expr)));
    }
}

test "minimum signed values fit in their respective types" {
    const test_cases = .{
        .{ .value = -128, .type = types_mod.Num.Int.Precision.i8, .should_fit = true },
        .{ .value = -129, .type = types_mod.Num.Int.Precision.i8, .should_fit = false },
        .{ .value = -32768, .type = types_mod.Num.Int.Precision.i16, .should_fit = true },
        .{ .value = -32769, .type = types_mod.Num.Int.Precision.i16, .should_fit = false },
        .{ .value = -2147483648, .type = types_mod.Num.Int.Precision.i32, .should_fit = true },
        .{ .value = -2147483649, .type = types_mod.Num.Int.Precision.i32, .should_fit = false },
        .{ .value = -9223372036854775808, .type = types_mod.Num.Int.Precision.i64, .should_fit = true },
        .{ .value = -9223372036854775809, .type = types_mod.Num.Int.Precision.i64, .should_fit = false },
        .{ .value = -170141183460469231731687303715884105728, .type = types_mod.Num.Int.Precision.i128, .should_fit = true },
    };

    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var problems = problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var unify_scratch = unifier.Scratch.init(gpa);
    defer unify_scratch.deinit();

    var occurs_scratch = occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    inline for (test_cases) |tc| {
        // Calculate the magnitude
        const u128_val: u128 = if (tc.value < 0) @as(u128, @intCast(-(tc.value + 1))) + 1 else @as(u128, @intCast(tc.value));

        // Apply the branchless adjustment for minimum signed values
        const is_negative = @as(u1, @intFromBool(tc.value < 0));
        const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
        const is_minimum_signed = is_negative & is_power_of_2;
        const adjusted_val = u128_val - is_minimum_signed;

        // Create requirements based on adjusted value
        const requirements = types_mod.Num.IntRequirements{
            .sign_needed = tc.value < 0,
            .bits_needed = @intFromEnum(types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val)),
        };

        const literal_var = module_env.types.freshFromContent(types_mod.Content{ .structure = .{ .num = .{ .num_unbound = requirements } } });
        const type_var = module_env.types.freshFromContent(types_mod.Content{ .structure = .{ .num = .{ .num_compact = .{ .int = tc.type } } } });

        const result = unifier.unify(
            &module_env,
            &module_env.types,
            &problems,
            &snapshots,
            &unify_scratch,
            &occurs_scratch,
            literal_var,
            type_var,
        );

        if (tc.should_fit) {
            try std.testing.expect(result == .ok);
        } else {
            try std.testing.expect(result == .problem);
        }
    }
}

test "minimum signed values have correct bits_needed" {
    const test_cases = .{
        .{ .value = -128, .expected_bits = types_mod.Num.Int.BitsNeeded.@"7" },
        .{ .value = -129, .expected_bits = types_mod.Num.Int.BitsNeeded.@"8" },
        .{ .value = -32768, .expected_bits = types_mod.Num.Int.BitsNeeded.@"9_to_15" },
        .{ .value = -32769, .expected_bits = types_mod.Num.Int.BitsNeeded.@"16" },
        .{ .value = -2147483648, .expected_bits = types_mod.Num.Int.BitsNeeded.@"17_to_31" },
        .{ .value = -2147483649, .expected_bits = types_mod.Num.Int.BitsNeeded.@"32" },
        .{ .value = -9223372036854775808, .expected_bits = types_mod.Num.Int.BitsNeeded.@"33_to_63" },
        .{ .value = -9223372036854775809, .expected_bits = types_mod.Num.Int.BitsNeeded.@"64" },
        .{ .value = -170141183460469231731687303715884105728, .expected_bits = types_mod.Num.Int.BitsNeeded.@"65_to_127" },
    };

    inline for (test_cases) |tc| {
        // Calculate the magnitude
        const u128_val: u128 = if (tc.value < 0) @as(u128, @intCast(-(tc.value + 1))) + 1 else @as(u128, @intCast(tc.value));

        // Apply the branchless adjustment for minimum signed values
        const is_negative = @as(u1, if (tc.value < 0) 1 else 0);
        const is_power_of_2 = @as(u1, if (u128_val != 0 and (u128_val & (u128_val - 1)) == 0) 1 else 0);
        const is_minimum_signed = is_negative & is_power_of_2;
        const adjusted_val = u128_val - is_minimum_signed;

        const bits_needed = types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val);
        try std.testing.expectEqual(tc.expected_bits, bits_needed);
    }
}

test "branchless minimum signed value detection" {
    const test_cases = .{
        // Minimum signed values (negative powers of 2)
        .{ .value = -1, .is_minimum = true }, // magnitude 1 = 2^0
        .{ .value = -2, .is_minimum = true }, // magnitude 2 = 2^1
        .{ .value = -4, .is_minimum = true }, // magnitude 4 = 2^2
        .{ .value = -8, .is_minimum = true }, // magnitude 8 = 2^3
        .{ .value = -16, .is_minimum = true }, // magnitude 16 = 2^4
        .{ .value = -32, .is_minimum = true }, // magnitude 32 = 2^5
        .{ .value = -64, .is_minimum = true }, // magnitude 64 = 2^6
        .{ .value = -128, .is_minimum = true }, // magnitude 128 = 2^7
        .{ .value = -256, .is_minimum = true }, // magnitude 256 = 2^8
        .{ .value = -32768, .is_minimum = true }, // magnitude 32768 = 2^15
        .{ .value = -2147483648, .is_minimum = true }, // magnitude 2^31

        // Not minimum signed values
        .{ .value = 128, .is_minimum = false }, // positive
        .{ .value = -3, .is_minimum = false }, // magnitude 3 (not power of 2)
        .{ .value = -5, .is_minimum = false }, // magnitude 5 (not power of 2)
        .{ .value = -127, .is_minimum = false }, // magnitude 127 (not power of 2)
        .{ .value = -129, .is_minimum = false }, // magnitude 129 (not power of 2)
        .{ .value = -130, .is_minimum = false }, // magnitude 130 (not power of 2)
        .{ .value = 0, .is_minimum = false }, // zero
    };

    inline for (test_cases) |tc| {
        const value: i128 = tc.value;
        const u128_val: u128 = if (value < 0) @as(u128, @intCast(-(value + 1))) + 1 else @as(u128, @intCast(value));

        const is_negative = @as(u1, @intFromBool(value < 0));
        const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
        const is_minimum_signed = is_negative & is_power_of_2;

        const expected: u1 = @intFromBool(tc.is_minimum);
        try std.testing.expectEqual(expected, is_minimum_signed);
    }
}

test "verify -128 produces 7 bits needed" {
    const value: i128 = -128;
    const u128_val: u128 = if (value < 0) @as(u128, @intCast(-(value + 1))) + 1 else @as(u128, @intCast(value));

    // Check intermediate values
    try std.testing.expectEqual(@as(u128, 128), u128_val);

    const is_negative = @as(u1, @intFromBool(value < 0));
    const is_power_of_2 = @as(u1, @intFromBool(u128_val != 0 and (u128_val & (u128_val - 1)) == 0));
    const is_minimum_signed = is_negative & is_power_of_2;

    try std.testing.expectEqual(@as(u1, 1), is_negative);
    try std.testing.expectEqual(@as(u1, 1), is_power_of_2);
    try std.testing.expectEqual(@as(u1, 1), is_minimum_signed);

    const adjusted_val = u128_val - is_minimum_signed;
    try std.testing.expectEqual(@as(u128, 127), adjusted_val);

    // Test that 127 maps to 7 bits
    const bits_needed = types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val);
    try std.testing.expectEqual(types_mod.Num.Int.BitsNeeded.@"7", bits_needed);
    try std.testing.expectEqual(@as(u8, 7), bits_needed.toBits());
}

/// Check the types for the provided expr
pub fn checkExpr(self: *Self, expr_idx: CIR.Expr.Idx) void {
    const expr = self.can_ir.store.getExpr(expr_idx);
    switch (expr) {
        .e_num => |_| {},
        .e_int => |_| {},
        .e_frac_f64 => |_| {},
        .e_frac_dec => |_| {},
        .e_dec_small => |_| {},
        .e_str_segment => |_| {},
        .e_str => |_| {},
        .e_lookup => |_| {},
        .e_list => |list| {
            const elem_var = @as(Var, @enumFromInt(@intFromEnum(list.elem_var)));
            const elems = self.can_ir.store.exprSlice(list.elems);

            std.debug.assert(elems.len > 0); // Should never be 0 here, because this is not an .empty_list

            // We need to type-check the first element, but we don't need to unify it with
            // anything because we already pre-unified the list's elem var with it.
            const first_elem_idx = elems[0];
            var last_unified_idx: CIR.Expr.Idx = first_elem_idx;
            var last_unified_index: usize = 0; // Track the index for error messages
            self.checkExpr(first_elem_idx);

            for (elems[1..], 1..) |elem_expr_id, i| {
                self.checkExpr(elem_expr_id);

                // Unify each element's var with the list's elem var
                const result = self.unify(elem_var, @enumFromInt(@intFromEnum(elem_expr_id)));

                switch (result) {
                    .ok => {},
                    .problem => |problem_idx| {
                        // Unification failed, so we know it appended a type mismatch to self.problems.
                        // We'll translate that generic type mismatch between the two elements into
                        // a more helpful list-specific error report.

                        // Extract info from the type mismatch problem
                        var elem_var_snapshot: snapshot.SnapshotContentIdx = undefined;
                        var incompatible_snapshot: snapshot.SnapshotContentIdx = undefined;

                        // Extract snapshots from the type mismatch problem
                        switch (self.problems.problems.get(problem_idx)) {
                            .type_mismatch => |mismatch| {
                                // The expected type is elem_var, actual is the incompatible element
                                elem_var_snapshot = mismatch.expected;
                                incompatible_snapshot = mismatch.actual;
                            },
                            else => {
                                // For other problem types (e.g., number_does_not_fit), the original
                                // problem is already more specific than our generic "incompatible list
                                // elements" message, so we should keep it as-is and not replace it.
                                // Note: if an element has an error type (e.g., from a nested heterogeneous
                                // list), unification would succeed, not fail, so we wouldn't reach this branch.
                                break;
                            },
                        }

                        // Include the previous element in the error message, since it's the one
                        // that the current element failed to unify with.
                        const prev_elem_expr = self.can_ir.store.getExpr(last_unified_idx);
                        const incompatible_elem_expr = self.can_ir.store.getExpr(elem_expr_id);
                        const prev_region = prev_elem_expr.toRegion();
                        const incomp_region = incompatible_elem_expr.toRegion();

                        // Replace the generic Problem in the MultiArrayList with a list-specific one
                        self.problems.problems.set(problem_idx, .{
                            .incompatible_list_elements = .{
                                .list_region = list.region,
                                .first_elem_region = prev_region orelse list.region,
                                .first_elem_var = @enumFromInt(@intFromEnum(last_unified_idx)),
                                .first_elem_snapshot = elem_var_snapshot,
                                .first_elem_index = last_unified_index,
                                .incompatible_elem_region = incomp_region orelse list.region,
                                .incompatible_elem_var = @enumFromInt(@intFromEnum(elem_expr_id)),
                                .incompatible_elem_snapshot = incompatible_snapshot,
                                .incompatible_elem_index = i,
                                .list_length = elems.len,
                            },
                        });

                        // Break to avoid cascading errors
                        break;
                    },
                }

                // Track the last successfully unified element
                last_unified_idx = elem_expr_id;
                last_unified_index = i;
            }
        },
        .e_empty_list => |_| {},
        .e_when => |_| {},
        .e_if => |_| {},
        .e_call => |call| {
            // Get all expressions - first is function, rest are arguments
            const all_exprs = self.can_ir.store.sliceExpr(call.args);

            if (all_exprs.len == 0) return; // No function to call

            // First expression is the function being called
            const func_expr_idx = all_exprs[0];
            self.checkExpr(func_expr_idx);

            // Rest are arguments
            const args = all_exprs[1..];
            for (args) |arg_expr_idx| {
                self.checkExpr(arg_expr_idx);
            }

            // For function calls, we need to create a proper function type expectation
            // But we need to be careful about runtime errors in the function position
            const func_expr = self.can_ir.store.getExpr(func_expr_idx);
            if (func_expr != .e_runtime_error) {
                // Create type variables for proper function type checking
                const call_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
                const func_var = @as(Var, @enumFromInt(@intFromEnum(func_expr_idx)));
                const return_var = call_var;

                // Create argument type variables
                var arg_vars = std.ArrayList(Var).init(self.gpa);
                defer arg_vars.deinit();

                for (args) |arg_expr_idx| {
                    const arg_var = @as(Var, @enumFromInt(@intFromEnum(arg_expr_idx)));
                    arg_vars.append(arg_var) catch |err| exitOnOom(err);
                }

                // Use the pre-created effect variable from canonicalization instead of creating temporary ones
                const func_content = self.types.mkFunc(arg_vars.items, return_var, call.effect_var);
                _ = self.types.setVarContent(func_var, func_content);
            }
        },

        .e_record => |e| {

            // ## RECORD TYPE CHECKING IMPLEMENTATION
            //
            // This implementation performs field-by-field unification between the record
            // structure's field type variables and the actual field value expression types.
            //
            // ### Process
            // 1. Resolve the expression var to get the record structure
            // 2. Type check each field value expression (to get concrete types)
            // 3. For each field, unify the field type var with the field value type var
            // 4. Unification propagates concrete types through the type system

            const expr_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));
            const record_var_resolved = self.types.resolveVar(expr_var);
            const record_var_content = record_var_resolved.desc.content;

            // Process each field
            for (self.can_ir.store.sliceRecordFields(e.fields)) |field_idx| {
                const field = self.can_ir.store.getRecordField(field_idx);

                // STEP 1: Check the field value expression first
                // This ensures the field value has a concrete type to unify with
                self.checkExpr(field.value);

                // STEP 2: Find the corresponding field type in the record structure
                // This only works if record_var_content is .structure.record
                if (record_var_content == .structure and record_var_content.structure == .record) {
                    const record_fields = self.types.getRecordFieldsSlice(record_var_content.structure.record.fields);

                    // STEP 3: Find the field with matching name and unify types
                    const field_names = record_fields.items(.name);
                    const field_vars = record_fields.items(.var_);
                    for (field_names, field_vars) |type_field_name, type_field_var| {
                        if (self.can_ir.env.idents.identsHaveSameText(type_field_name, field.name)) {
                            // Extract the type variable from the field value expression
                            // Different expression types store their type variables in different places
                            const field_expr_type_var = @as(Var, @enumFromInt(@intFromEnum(field.value)));

                            // STEP 4: Unify field type variable with field value type variable
                            // This is where concrete types (like Str, Num) get propagated
                            // from field values to the record structure
                            _ = self.unify(type_field_var, field_expr_type_var);
                            break;
                        }
                    }
                }
                // If record_var_content is NOT .structure.record, unification is skipped
                // This typically happens when canonicalization didn't set the record structure properly
            }
        },
        .e_empty_record => |_| {},
        .e_record_access => |_| {},
        .e_tag => |_| {},
        .e_zero_argument_tag => |_| {},
        .e_binop => |_| {},
        .e_block => |block| {
            // Check all statements in the block (safely)
            const statements = self.can_ir.store.sliceStatements(block.stmts);
            for (statements) |stmt_idx| {
                const stmt = self.can_ir.store.getStatement(stmt_idx);
                switch (stmt) {
                    .s_decl => |decl_stmt| {
                        // Just check the expression, don't try to unify with pattern
                        self.checkExpr(decl_stmt.expr);
                    },
                    .s_reassign => |reassign| {
                        self.checkExpr(reassign.expr);
                    },
                    .s_expr => |expr_stmt| {
                        self.checkExpr(expr_stmt.expr);
                    },
                    else => {
                        // Other statement types don't need expression checking
                    },
                }
            }

            // Check the final expression
            self.checkExpr(block.final_expr);
        },
        .e_lambda => |lambda| {
            // Check the lambda body
            self.checkExpr(lambda.body);

            // Improved lambda type inference: handle actual arguments
            const lambda_var = @as(Var, @enumFromInt(@intFromEnum(expr_idx)));

            // Get the actual lambda arguments
            const arg_patterns = self.can_ir.store.slicePatterns(lambda.args);

            // Create type variables for each argument pattern
            var arg_vars = std.ArrayList(Var).init(self.gpa);
            defer arg_vars.deinit();

            for (arg_patterns) |pattern_idx| {
                const pattern_var = @as(Var, @enumFromInt(@intFromEnum(pattern_idx)));
                arg_vars.append(pattern_var) catch |err| exitOnOom(err);
            }

            // Create return type variable from lambda body
            const return_var = @as(Var, @enumFromInt(@intFromEnum(lambda.body)));

            // Create function type using pre-created effect variable from canonicalization
            const func_content = self.types.mkFunc(arg_vars.items, return_var, lambda.effect_var);
            _ = self.types.setVarContent(lambda_var, func_content);
        },
        .e_tuple => |tuple| {
            for (self.can_ir.store.exprSlice(tuple.elems)) |single_elem_expr_idx| {
                // Check tuple elements
                self.checkExpr(single_elem_expr_idx);
            }
        },
        .e_dot_access => |dot_access| {
            // Check the receiver expression
            self.checkExpr(dot_access.receiver);
            // TODO: Implement proper field type checking
            // For now, just check the receiver to avoid crashes
        },
        .e_runtime_error => |_| {},
    }
}
