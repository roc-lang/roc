//! Tests for numeric literal size and type unification logic.

const std = @import("std");
const base = @import("base");
const types = @import("types");
const can = @import("can");
const Check = @import("../Check.zig");
const TestEnv = @import("./TestEnv.zig");

const TypesStore = types.TypesStore;
const Content = types.Content;
const ModuleEnv = can.ModuleEnv;
const Var = types.Var;
const Num = types.Num;
const problem = @import("../problem.zig");
const snapshot = @import("../snapshot.zig");
const occurs = @import("../occurs.zig");
const FlatType = types.FlatType;
const ProblemStore = problem.Store;
const SnapshotStore = snapshot.Store;
const UnifierScratch = @import("../unify.zig").Scratch;
const OccursScratch = occurs.Scratch;
const unify = @import("../unify.zig").unify;

test "U8: 255 fits" {
    const source =
        \\{
        \\  x : U8
        \\  x = 50
        \\  
        \\  x + 255
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefType("Num(Int(Unsigned8))");
}

test "U8: 256 does not fit" {
    const source =
        \\{
        \\  x : U8
        \\  x = 50
        \\  
        \\  x + 256
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("NUMBER DOES NOT FIT IN TYPE");
}

test "U8: negative does not fit" {
    const source =
        \\{
        \\  x : U8
        \\  x = 50
        \\  
        \\  x + -1
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("NEGATIVE UNSIGNED INTEGER");
}

test "I8: -128 fits" {
    const source =
        \\{
        \\  x : I8
        \\  x = 1
        \\  
        \\  x + -128
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefType("Num(Int(Signed8))");
}

test "I8: -129 does not fit" {
    const source =
        \\{
        \\  x : I8
        \\  x = 1
        \\  
        \\  x + -129
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertOneTypeError("NUMBER DOES NOT FIT IN TYPE");
}

test "F32: fits" {
    const source =
        \\{
        \\  x : F32
        \\  x = 1
        \\  
        \\  x + 10.1
        \\}
    ;

    var test_env = try TestEnv.initExpr("Test", source);
    defer test_env.deinit();
    try test_env.assertLastDefType("Num(Frac(Float32))");
}

// TODO: Move these to unify

// test "two integer literals with different requirements unify to most restrictive" {
//     const gpa = std.testing.allocator;

//     var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
//     defer module_env.deinit();

//     var problems = try ProblemStore.initCapacity(gpa, 16);
//     defer problems.deinit(gpa);

//     var snapshots = try SnapshotStore.initCapacity(gpa, 16);
//     defer snapshots.deinit();

//     var scratch = try UnifierScratch.init(gpa);
//     defer scratch.deinit();

//     var occurs_scratch = try OccursScratch.init(gpa);
//     defer occurs_scratch.deinit();

//     // Create a literal with value 100 (7 bits, no sign)
//     const literal1_var = try module_env.types.freshFromContent(Content{
//         .structure = .{
//             .num = .{
//                 .num_poly = .{
//                     .var_ = try module_env.types.fresh(),
//                     .requirements = Num.IntRequirements{
//                         .sign_needed = false,
//                         .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
//                     },
//                 },
//             },
//         },
//     });

//     // Create a literal with value 200 (8 bits, no sign)
//     const literal2_var = try module_env.types.freshFromContent(Content{
//         .structure = .{
//             .num = .{
//                 .num_poly = .{
//                     .var_ = try module_env.types.fresh(),
//                     .requirements = Num.IntRequirements{
//                         .sign_needed = false,
//                         .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"8"),
//                     },
//                 },
//             },
//         },
//     });

//     // They should unify successfully
//     const result = try unify(
//         &module_env,
//         &module_env.types,
//         &problems,
//         &snapshots,
//         &scratch,
//         &occurs_scratch,
//         literal1_var,
//         literal2_var,
//     );

//     try std.testing.expect(result == .ok);

//     // Verify that the result has bits_needed = 8
//     // After unification, both variables should have the most restrictive requirements
//     const resolved1 = module_env.types.resolveVar(literal1_var);
//     switch (resolved1.desc.content) {
//         .structure => |structure| switch (structure) {
//             .num => |num| switch (num) {
//                 .num_poly => |requirements| {
//                     try std.testing.expectEqual(@as(u8, @intFromEnum(Num.Int.BitsNeeded.@"8")), requirements.requirements.bits_needed);
//                     try std.testing.expectEqual(false, requirements.requirements.sign_needed);
//                 },
//                 .int_poly => |requirements| {
//                     try std.testing.expectEqual(@as(u8, @intFromEnum(Num.Int.BitsNeeded.@"8")), requirements.requirements.bits_needed);
//                     try std.testing.expectEqual(false, requirements.requirements.sign_needed);
//                 },
//                 else => return error.UnexpectedNumType,
//             },
//             else => return error.UnexpectedStructureType,
//         },
//         else => return error.UnexpectedContentType,
//     }
// }

// test "positive and negative literals unify with sign requirement" {
//     const gpa = std.testing.allocator;

//     var module_env = try ModuleEnv.init(gpa, try gpa.dupe(u8, ""));
//     defer module_env.deinit();

//     var problems = try ProblemStore.initCapacity(gpa, 16);
//     defer problems.deinit(gpa);

//     var snapshots = try SnapshotStore.initCapacity(gpa, 16);
//     defer snapshots.deinit();

//     var scratch = try UnifierScratch.init(gpa);
//     defer scratch.deinit();

//     var occurs_scratch = try OccursScratch.init(gpa);
//     defer occurs_scratch.deinit();

//     // Create a literal with value 100 (no sign needed)
//     const literal1_var = try module_env.types.freshFromContent(Content{
//         .structure = FlatType{
//             .num = Num{
//                 .num_poly = .{
//                     .var_ = try module_env.types.fresh(),
//                     .requirements = Num.IntRequirements{
//                         .sign_needed = false,
//                         .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
//                     },
//                 },
//             },
//         },
//     });

//     // Create a literal with value -100 (sign needed)
//     const literal2_var = try module_env.types.freshFromContent(Content{
//         .structure = .{
//             .num = .{
//                 .num_poly = .{
//                     .var_ = try module_env.types.fresh(),
//                     .requirements = Num.IntRequirements{
//                         .sign_needed = true,
//                         .bits_needed = @intFromEnum(Num.Int.BitsNeeded.@"7"),
//                     },
//                 },
//             },
//         },
//     });

//     // They should unify successfully
//     const result = try unify(
//         &module_env,
//         &module_env.types,
//         &problems,
//         &snapshots,
//         &scratch,
//         &occurs_scratch,
//         literal1_var,
//         literal2_var,
//     );

//     try std.testing.expect(result == .ok);

//     // Verify that the result has sign_needed = true
//     // After unification, both variables should have the most restrictive requirements
//     const resolved1 = module_env.types.resolveVar(literal1_var);
//     switch (resolved1.desc.content) {
//         .structure => |structure| switch (structure) {
//             .num => |num| switch (num) {
//                 .num_poly => |requirements| {
//                     try std.testing.expectEqual(true, requirements.requirements.sign_needed);
//                     try std.testing.expectEqual(@as(u8, @intFromEnum(Num.Int.BitsNeeded.@"7")), requirements.requirements.bits_needed);
//                 },
//                 .int_poly => |requirements| {
//                     try std.testing.expectEqual(true, requirements.requirements.sign_needed);
//                     try std.testing.expectEqual(@as(u8, @intFromEnum(Num.Int.BitsNeeded.@"7")), requirements.requirements.bits_needed);
//                 },
//                 else => return error.UnexpectedNumType,
//             },
//             else => return error.UnexpectedStructureType,
//         },
//         else => return error.UnexpectedContentType,
//     }
// }
