//! Facade from lambdamono representation planning to source-blind emission.

const std = @import("std");
const exec_plan = @import("exec_plan.zig");
const emit = @import("emit.zig");
const symbol_mod = @import("symbol");

pub const Result = emit.Result;
const Symbol = symbol_mod.Symbol;

/// Lower a solved program into executable lambdamono form.
pub fn run(allocator: std.mem.Allocator, input: *exec_plan.Input) std.mem.Allocator.Error!Result {
    return runWithEntrypoints(allocator, input, &.{});
}

/// Lower a solved program while generating wrappers for the requested entrypoints.
pub fn runWithEntrypoints(
    allocator: std.mem.Allocator,
    input: *exec_plan.Input,
    entrypoints: []const Symbol,
) std.mem.Allocator.Error!Result {
    var planned = try exec_plan.buildWithEntrypoints(allocator, input, entrypoints);
    errdefer planned.deinit();
    return try emit.run(allocator, &planned);
}
