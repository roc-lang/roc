//! Tests for numeric literal size and type unification logic.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const Check = @import("check");
const compile = @import("compile");

const ModuleEnv = compile.ModuleEnv;
const TypesStore = types.TypesStore;
const Content = types.Content;
const Var = types.Var;
const Num = types.Num;
const unify = Check.unify;
const problem = Check.problem;
const snapshot = Check.snapshot;
const occurs = Check.occurs;
const FlatType = types.FlatType;

test "integer literal 255 fits in U8" {
    const gpa = std.testing.allocator;

    var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = try Check.problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = try Check.snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = try Check.unifier.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = try Check.occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value 255
    const literal_var = try module_env.types.freshFromContent(Content{
        .structure = .{
            .num = .{
                .num_poly = .{
                    .requirements = Num.IntRequirements{
                        .sign_needed = false,
                        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
                    },
                    .var_ = try module_env.types.fresh(),
                },
            },
        },
    });

    // Create U8 type
    const u8_var = try module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });

    // They should unify successfully
    const result = try Check.unifier.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        u8_var,
    );

    try std.testing.expect(result == .ok);
}

test "integer literal 256 does not fit in U8" {
    const gpa = std.testing.allocator;

    var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = try Check.problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = try Check.snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = try Check.unifier.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = try Check.occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value 256
    const literal_var = try module_env.types.freshFromContent(Content{
        .structure = .{
            .num = .{
                .num_poly = .{
                    .requirements = Num.IntRequirements{
                        .sign_needed = false,
                        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"9_to_15"),
                    },
                    .var_ = try module_env.types.fresh(),
                },
            },
        },
    });

    // Create U8 type
    const u8_var = try module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });

    // They should NOT unify - type mismatch expected
    const result = try Check.unifier.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        u8_var,
    );

    try std.testing.expect(result == .problem);
}

test "integer literal -128 fits in I8" {
    const gpa = std.testing.allocator;

    var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = try Check.problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = try Check.snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = try Check.unifier.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = try Check.occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value -128
    const literal_var = try module_env.types.freshFromContent(Content{
        .structure = .{
            .num = .{
                .num_poly = .{
                    .requirements = Num.IntRequirements{
                        .sign_needed = true,
                        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
                    },
                    .var_ = try module_env.types.fresh(),
                },
            },
        },
    });

    // Create I8 type
    const i8_var = try module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i8 } } } });

    // They should unify successfully
    const result = try Check.unifier.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        i8_var,
    );

    try std.testing.expect(result == .ok);
}

test "integer literal -129 does not fit in I8" {
    const gpa = std.testing.allocator;

    var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = try Check.problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = try Check.snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = try Check.unifier.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = try Check.occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value -129
    const literal_var = try module_env.types.freshFromContent(Content{
        .structure = .{
            .num = .{
                .num_poly = .{
                    .requirements = Num.IntRequirements{
                        .sign_needed = true,
                        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
                    },
                    .var_ = try module_env.types.fresh(),
                },
            },
        },
    });

    // Create I8 type
    const i8_var = try module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .i8 } } } });

    // They should NOT unify - type mismatch expected
    const result = try Check.unifier.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        i8_var,
    );

    try std.testing.expect(result == .problem);
}

test "negative literal cannot unify with unsigned type" {
    const gpa = std.testing.allocator;

    var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = try Check.problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = try Check.snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = try Check.unifier.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = try Check.occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value -1
    const literal_var = try module_env.types.freshFromContent(Content{
        .structure = .{
            .num = .{
                .num_poly = .{
                    .var_ = try module_env.types.fresh(),
                    .requirements = Num.IntRequirements{
                        .sign_needed = true,
                        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
                    },
                },
            },
        },
    });

    // Create U8 type
    const u8_var = try module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .int = .u8 } } } });

    // They should NOT unify - type mismatch expected
    const result = try Check.unifier.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        u8_var,
    );

    try std.testing.expect(result == .problem);
}

test "float literal that fits in F32" {
    const gpa = std.testing.allocator;

    var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = try Check.problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = try Check.snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = try Check.unifier.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = try Check.occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal that fits in F32
    const literal_var = try module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = .{
        .requirements = Num.FracRequirements{
            .fits_in_f32 = true,
            .fits_in_dec = true,
        },
        .var_ = try module_env.types.fresh(),
    } } } });

    // Create F32 type
    const f32_var = try module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f32 } } } });

    // They should unify successfully
    const result = try Check.unifier.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        f32_var,
    );

    try std.testing.expect(result == .ok);
}

test "float literal that doesn't fit in F32" {
    const gpa = std.testing.allocator;

    var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = try Check.problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = try Check.snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = try Check.unifier.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = try Check.occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal that doesn't fit in F32 (e.g., requires F64 precision)
    const literal_var = try module_env.types.freshFromContent(Content{
        .structure = .{
            .num = .{
                .frac_poly = .{
                    .requirements = Num.FracRequirements{
                        .fits_in_f32 = false,
                        .fits_in_dec = true,
                    },
                    .var_ = try module_env.types.fresh(),
                },
            },
        },
    });

    // Create F32 type
    const f32_var = try module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .frac = .f32 } } } });

    // They should NOT unify - type mismatch expected
    const result = try Check.unifier.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        f32_var,
    );

    try std.testing.expect(result == .problem);
}

test "float literal NaN doesn't fit in Dec" {
    const gpa = std.testing.allocator;

    var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = try Check.problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = try Check.snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = try Check.unifier.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = try Check.occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal like NaN that doesn't fit in Dec
    const literal_var = try module_env.types.freshFromContent(Content{
        .structure = .{
            .num = .{
                .frac_poly = .{
                    .var_ = try module_env.types.fresh(),
                    .requirements = Num.FracRequirements{
                        .fits_in_f32 = true,
                        .fits_in_dec = false,
                    },
                },
            },
        },
    });

    // Create Dec type
    const dec_var = try module_env.types.freshFromContent(Content{ .structure = .{ .num = .{ .num_compact = .{ .frac = .dec } } } });

    // They should NOT unify - type mismatch expected
    const result = try Check.unifier.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal_var,
        dec_var,
    );

    try std.testing.expect(result == .problem);
}

test "two integer literals with different requirements unify to most restrictive" {
    const gpa = std.testing.allocator;

    var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = try Check.problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = try Check.snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = try Check.unifier.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = try Check.occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value 100 (7 bits, no sign)
    const literal1_var = try module_env.types.freshFromContent(Content{
        .structure = .{
            .num = .{
                .num_poly = .{
                    .var_ = try module_env.types.fresh(),
                    .requirements = Num.IntRequirements{
                        .sign_needed = false,
                        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
                    },
                },
            },
        },
    });

    // Create a literal with value 200 (8 bits, no sign)
    const literal2_var = try module_env.types.freshFromContent(Content{
        .structure = .{
            .num = .{
                .num_poly = .{
                    .var_ = try module_env.types.fresh(),
                    .requirements = Num.IntRequirements{
                        .sign_needed = false,
                        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
                    },
                },
            },
        },
    });

    // They should unify successfully
    const result = try Check.unifier.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal1_var,
        literal2_var,
    );

    try std.testing.expect(result == .ok);

    // Verify that the result has bits_needed = 8
    // After unification, both variables should have the most restrictive requirements
    const resolved1 = module_env.types.resolveVar(literal1_var);
    switch (resolved1.desc.content) {
        .structure => |structure| switch (structure) {
            .num => |num| switch (num) {
                .num_poly => |requirements| {
                    try std.testing.expectEqual(@as(u8, @intFromEnum(Num.Int.BitsNeeded.@"8")), requirements.requirements.bits_needed);
                    try std.testing.expectEqual(false, requirements.requirements.sign_needed);
                },
                .int_poly => |requirements| {
                    try std.testing.expectEqual(@as(u8, @intFromEnum(Num.Int.BitsNeeded.@"8")), requirements.requirements.bits_needed);
                    try std.testing.expectEqual(false, requirements.requirements.sign_needed);
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

    var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
    defer module_env.deinit();

    var problems = try Check.problem.Store.initCapacity(gpa, 16);
    defer problems.deinit(gpa);

    var snapshots = try Check.snapshot.Store.initCapacity(gpa, 16);
    defer snapshots.deinit();

    var scratch = try Check.unifier.Scratch.init(gpa);
    defer scratch.deinit();

    var occurs_scratch = try Check.occurs.Scratch.init(gpa);
    defer occurs_scratch.deinit();

    // Create a literal with value 100 (no sign needed)
    const literal1_var = try module_env.types.freshFromContent(Content{
        .structure = FlatType{
            .num = Num{
                .num_poly = .{
                    .var_ = try module_env.types.fresh(),
                    .requirements = Num.IntRequirements{
                        .sign_needed = false,
                        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
                    },
                },
            },
        },
    });

    // Create a literal with value -100 (sign needed)
    const literal2_var = try module_env.types.freshFromContent(Content{
        .structure = .{
            .num = .{
                .num_poly = .{
                    .var_ = try module_env.types.fresh(),
                    .requirements = Num.IntRequirements{
                        .sign_needed = true,
                        .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
                    },
                },
            },
        },
    });

    // They should unify successfully
    const result = try Check.unifier.unify(
        &module_env,
        &module_env.types,
        &problems,
        &snapshots,
        &scratch,
        &occurs_scratch,
        literal1_var,
        literal2_var,
    );

    try std.testing.expect(result == .ok);

    // Verify that the result has sign_needed = true
    // After unification, both variables should have the most restrictive requirements
    const resolved1 = module_env.types.resolveVar(literal1_var);
    switch (resolved1.desc.content) {
        .structure => |structure| switch (structure) {
            .num => |num| switch (num) {
                .num_poly => |requirements| {
                    try std.testing.expectEqual(true, requirements.requirements.sign_needed);
                    try std.testing.expectEqual(@as(u8, @intFromEnum(Num.Int.BitsNeeded.@"7")), requirements.requirements.bits_needed);
                },
                .int_poly => |requirements| {
                    try std.testing.expectEqual(true, requirements.requirements.sign_needed);
                    try std.testing.expectEqual(@as(u8, @intFromEnum(Num.Int.BitsNeeded.@"7")), requirements.requirements.bits_needed);
                },
                else => return error.UnexpectedNumType,
            },
            else => return error.UnexpectedStructureType,
        },
        else => return error.UnexpectedContentType,
    }
}
