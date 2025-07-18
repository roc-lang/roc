//! Tests for numeric literal size and type unification logic.

const std = @import("std");
const types = @import("../../types/types.zig");
const base = @import("base");
const unify = @import("../unify.zig");
const TypesStore = @import("../../types/store.zig").TypesStore;
const Content = types.Content;
const Var = types.Var;
const Num = types.Num;

test "integer literal 255 fits in U8" {
    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = @import("../../problem.zig").Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = @import("../snapshot.zig").Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = @import("../occurs.zig").Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value 255
    const literal_requirements = Num.IntRequirements{
        .var_ = module_env.types.fresh(),
        .sign_needed = false,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
    };
    const literal_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = literal_requirements } } });

    // Create U8 type
    const u8_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });

    // They should unify successfully
    const result = unify.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        u8_var,
    );

    try std.testing.expect(result == .success);
}

test "integer literal 256 does not fit in U8" {
    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = @import("../../problem.zig").Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = @import("../snapshot.zig").Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = @import("../occurs.zig").Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value 256
    const literal_requirements = Num.IntRequirements{
        .var_ = module_env.types.fresh(),
        .sign_needed = false,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"9_to_15"),
    };
    const literal_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = literal_requirements } } });

    // Create U8 type
    const u8_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });

    // They should NOT unify - type mismatch expected
    const result = unify.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        u8_var,
    );

    try std.testing.expect(result == .type_mismatch);
}

test "integer literal -128 fits in I8" {
    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = @import("../../problem.zig").Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = @import("../snapshot.zig").Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = @import("../occurs.zig").Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value -128
    const literal_requirements = Num.IntRequirements{
        .var_ = module_env.types.fresh(),
        .sign_needed = true,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
    };
    const literal_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = literal_requirements } } });

    // Create I8 type
    const i8_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i8 } } } });

    // They should unify successfully
    const result = unify.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        i8_var,
    );

    try std.testing.expect(result == .success);
}

test "integer literal -129 does not fit in I8" {
    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = @import("../../problem.zig").Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = @import("../snapshot.zig").Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = @import("../occurs.zig").Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value -129
    const literal_requirements = Num.IntRequirements{
        .var_ = module_env.types.fresh(),
        .sign_needed = true,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
    };
    const literal_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = literal_requirements } } });

    // Create I8 type
    const i8_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i8 } } } });

    // They should NOT unify - type mismatch expected
    const result = unify.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        i8_var,
    );

    try std.testing.expect(result == .type_mismatch);
}

test "negative literal cannot unify with unsigned type" {
    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = @import("../../problem.zig").Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = @import("../snapshot.zig").Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = @import("../occurs.zig").Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value -1
    const literal_requirements = Num.IntRequirements{
        .var_ = module_env.types.fresh(),
        .sign_needed = true,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
    };
    const literal_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = literal_requirements } } });

    // Create U8 type
    const u8_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });

    // They should NOT unify - type mismatch expected
    const result = unify.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        u8_var,
    );

    try std.testing.expect(result == .type_mismatch);
}

test "float literal that fits in F32" {
    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = @import("../../problem.zig").Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = @import("../snapshot.zig").Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = @import("../occurs.zig").Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal that fits in F32
    const literal_requirements = Num.FracRequirements{
        .var_ = module_env.types.fresh(),
        .fits_in_f32 = true,
        .fits_in_dec = true,
    };
    const literal_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = literal_requirements } } });

    // Create F32 type
    const f32_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f32 } } } });

    // They should unify successfully
    const result = unify.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        f32_var,
    );

    try std.testing.expect(result == .success);
}

test "float literal that doesn't fit in F32" {
    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = @import("../../problem.zig").Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = @import("../snapshot.zig").Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = @import("../occurs.zig").Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal that doesn't fit in F32 (e.g., requires F64 precision)
    const literal_requirements = Num.FracRequirements{
        .var_ = module_env.types.fresh(),
        .fits_in_f32 = false,
        .fits_in_dec = true,
    };
    const literal_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = literal_requirements } } });

    // Create F32 type
    const f32_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f32 } } } });

    // They should NOT unify - type mismatch expected
    const result = unify.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        f32_var,
    );

    try std.testing.expect(result == .type_mismatch);
}

test "float literal NaN doesn't fit in Dec" {
    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = @import("../../problem.zig").Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = @import("../snapshot.zig").Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = @import("../occurs.zig").Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal like NaN that doesn't fit in Dec
    const literal_requirements = Num.FracRequirements{
        .var_ = module_env.types.fresh(),
        .fits_in_f32 = true,
        .fits_in_dec = false,
    };
    const literal_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = literal_requirements } } });

    // Create Dec type
    const dec_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .frac = .dec } } } });

    // They should NOT unify - type mismatch expected
    const result = unify.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        dec_var,
    );

    try std.testing.expect(result == .type_mismatch);
}

test "two integer literals with different requirements unify to most restrictive" {
    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = @import("../../problem.zig").Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = @import("../snapshot.zig").Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = @import("../occurs.zig").Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value 100 (7 bits, no sign)
    const literal1_requirements = Num.IntRequirements{
        .var_ = module_env.types.fresh(),
        .sign_needed = false,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
    };
    const literal1_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = literal1_requirements } } });

    // Create a literal with value 200 (8 bits, no sign)
    const literal2_requirements = Num.IntRequirements{
        .var_ = module_env.types.fresh(),
        .sign_needed = false,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
    };
    const literal2_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = literal2_requirements } } });

    // They should unify successfully
    const result = unify.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal1_var,
        literal2_var,
    );

    try std.testing.expect(result == .success);

    // Verify that the result has bits_needed = 8
    // After unification, both variables should have the most restrictive requirements
    const resolved1 = module_env.types.resolveVar(literal1_var);
    switch (resolved1.desc.content) {
        .structure => |structure| switch (structure) {
            .num => |num| switch (num) {
                .num_poly => |requirements| {
                    try std.testing.expectEqual(@as(u8, 8), requirements.bits_needed);
                    try std.testing.expectEqual(false, requirements.sign_needed);
                },
                .int_poly => |requirements| {
                    try std.testing.expectEqual(@as(u8, 8), requirements.bits_needed);
                    try std.testing.expectEqual(false, requirements.sign_needed);
                },
                else => return error.UnexpectedNumType,
            },
            else => return error.UnexpectedStructureType,
        },
        else => return error.UnexpectedContentType,
    }
}

test "positive and negative literals unify with sign requirement" {
    const gpa = std.testing.allocator;

    var module_env = base.ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = @import("../../problem.zig").Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = @import("../snapshot.zig").Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = unify.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = @import("../occurs.zig").Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value 100 (no sign needed)
    const literal1_requirements = Num.IntRequirements{
        .var_ = module_env.types.fresh(),
        .sign_needed = false,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
    };
    const literal1_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = literal1_requirements } } });

    // Create a literal with value -100 (sign needed)
    const literal2_requirements = Num.IntRequirements{
        .var_ = module_env.types.fresh(),
        .sign_needed = true,
        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
    };
    const literal2_var = module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = literal2_requirements } } });

    // They should unify successfully
    const result = unify.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal1_var,
        literal2_var,
    );

    try std.testing.expect(result == .success);

    // Verify that the result has sign_needed = true
    // After unification, both variables should have the most restrictive requirements
    const resolved1 = module_env.types.resolveVar(literal1_var);
    switch (resolved1.desc.content) {
        .structure => |structure| switch (structure) {
            .num => |num| switch (num) {
                .num_poly => |requirements| {
                    try std.testing.expectEqual(true, requirements.sign_needed);
                    try std.testing.expectEqual(@as(u8, 7), requirements.bits_needed);
                },
                .int_poly => |requirements| {
                    try std.testing.expectEqual(true, requirements.sign_needed);
                    try std.testing.expectEqual(@as(u8, 7), requirements.bits_needed);
                },
                else => return error.UnexpectedNumType,
            },
            else => return error.UnexpectedStructureType,
        },
        else => return error.UnexpectedContentType,
    }
}
