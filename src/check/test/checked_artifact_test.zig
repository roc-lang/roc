const std = @import("std");

test "shared call shapes are looked up before compilation" {
    const source = @embedFile("../checked_artifact.zig");
    const function_start = std.mem.find(u8, source, "fn publishSpecializationCallShape(").?;
    const function_source = source[function_start..];
    const cache_hit = std.mem.find(u8, function_source, "if (by_key.get(key)) |existing| return existing;").?;
    const compile_shape = std.mem.find(u8, function_source, "try compileSpecializationCallShape(").?;
    try std.testing.expect(cache_hit < compile_shape);
}
