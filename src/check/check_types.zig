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

/// Deinit owned fields
pub fn unify(self: *Self, a: Var, b: Var) void {
    _ = unifier.unify(
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
        const def = self.can_ir.store.getDef(def_idx);
        // TODO: Check patterns
        self.checkExpr(def.expr);

        // If there's a type annotation, unify the expression with the annotation's signature
        if (def.annotation) |anno_idx| {
            const annotation = self.can_ir.store.getAnnotation(anno_idx);

            self.unify(@enumFromInt(@intFromEnum(def.expr)), annotation.signature);
            self.unify(@enumFromInt(@intFromEnum(def_idx)), annotation.signature);
        } else {
            self.unify(@enumFromInt(@intFromEnum(def_idx)), @enumFromInt(@intFromEnum(def.expr)));
        }
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
            .var_ = module_env.types.fresh(),
            .sign_needed = tc.value < 0,
            .bits_needed = @intFromEnum(types_mod.Num.Int.BitsNeeded.fromValue(adjusted_val)),
        };

        const literal_var = module_env.types.freshFromContent(types_mod.Content{ .structure = .{ .num = .{ .num_poly = requirements } } });
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
        .num => |_| {},
        .int => |_| {},
        .frac_f64 => |_| {},
        .frac_dec => |_| {},
        .dec_small => |_| {},
        .str_segment => |_| {},
        .str => |_| {},
        .single_quote => |_| {},
        .lookup => |_| {},
        .list => |list| {
            const elem_var = list.elem_var;
            for (self.can_ir.store.exprSlice(list.elems)) |single_elem_expr_idx| {
                self.checkExpr(single_elem_expr_idx);
                self.unify(
                    @enumFromInt(@intFromEnum(elem_var)),
                    @enumFromInt(@intFromEnum(single_elem_expr_idx)),
                );
            }
        },
        .when => |_| {},
        .@"if" => |_| {},
        .call => |_| {},

        .record => |e| {

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
                            const field_expr = self.can_ir.store.getExpr(field.value);
                            const field_expr_type_var = switch (field_expr) {
                                .str, .str_segment => @as(Var, @enumFromInt(@intFromEnum(field.value))),
                                .int => |int_expr| int_expr.num_var,
                                .num => |num_expr| num_expr.num_var,
                                .frac_f64 => |frac_expr| frac_expr.frac_var,
                                .frac_dec => |frac_expr| frac_expr.frac_var,
                                .dec_small => |dec_expr| dec_expr.num_var,
                                else => @as(Var, @enumFromInt(@intFromEnum(field.value))),
                            };

                            // STEP 4: Unify field type variable with field value type variable
                            // This is where concrete types (like Str, Num) get propagated
                            // from field values to the record structure
                            self.unify(type_field_var, field_expr_type_var);
                            break;
                        }
                    }
                }
                // If record_var_content is NOT .structure.record, unification is skipped
                // This typically happens when canonicalization didn't set the record structure properly
            }
        },
        .empty_record => |_| {},
        .record_access => |_| {},
        .tag => |_| {},
        .zero_argument_tag => |_| {},
        .binop => |_| {},
        .block => |_| {},
        .lambda => |_| {},
        .tuple => |tuple| {
            for (self.can_ir.store.exprSlice(tuple.elems)) |single_elem_expr_idx| {
                self.checkExpr(single_elem_expr_idx);
            }
        },
        .dot_access => |_| {
            // TODO: Implement type checking for dot access
            // This will need to:
            // 1. Check the receiver type
            // 2. Determine if it's record field access or static dispatch
            // 3. Validate the field/method exists and has correct type
            // 4. Type check any arguments if it's a method call
        },
        .runtime_error => |_| {},
    }
}
