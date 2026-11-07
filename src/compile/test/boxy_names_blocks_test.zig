//! A Boxy program carries a type's field labels and tag names only where it
//! prints a value of that type: descriptors hold no names, and a names block
//! is static data only an inspection reads (design.md "Names Blocks").
const std = @import("std");
const lir = @import("lir");
const harness = @import("lower_to_lir_harness.zig");

/// A generic constructor whose record and tag union have dynamic layouts, so
/// their descriptors are static data of the program.
const constructor =
    \\wrap = |x| { zebra_field: x, other: Quokka(x) }
    \\count = |x| List.len([wrap(x), wrap(x)])
    \\
;

/// Whether any static data export's bytes hold `text`.
fn staticDataHolds(lowered: *const lir.CheckedPipeline.LoweredProgram, text: []const u8) bool {
    const frozen = lowered.frozen_static_data orelse return false;
    for (frozen.exports) |item| {
        if (std.mem.find(u8, item.bytes, text) != null) return true;
    }
    return false;
}

/// The program builds descriptors for the record and the tag union.
fn expectDescriptors(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    const frozen = lowered.frozen_static_data orelse return error.TestUnexpectedResult;
    try std.testing.expect(frozen.exports.len != 0);
}

fn expectNoNames(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    try expectDescriptors(lowered);
    try std.testing.expect(!staticDataHolds(lowered, "zebra_field"));
    try std.testing.expect(!staticDataHolds(lowered, "Quokka"));
}

fn expectNames(lowered: *const lir.CheckedPipeline.LoweredProgram) harness.LowerToLirHarnessError!void {
    try std.testing.expect(staticDataHolds(lowered, "zebra_field"));
    try std.testing.expect(staticDataHolds(lowered, "Quokka"));
}

test "a record and a tag union no value of which is printed carry no names" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.runLoweredInspection(constructor ++
            \\main! = |args| {
            \\    echo!(Str.inspect(count(args)))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .target_usize = width }, expectNoNames);
    }
}

test "printing a record and a tag union carries their names" {
    for ([_]@import("base").target.TargetUsize{ .u32, .u64 }) |width| {
        try harness.runLoweredInspection(constructor ++
            \\main! = |args| {
            \\    echo!(Str.inspect(count(args)))
            \\    echo!(Str.inspect(wrap(args)))
            \\    Ok({})
            \\}
        , .{ .specialization_strategy = .boxy, .target_usize = width }, expectNames);
    }
}
