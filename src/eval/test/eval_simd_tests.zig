//! Full integer-SIMD differential coverage for the wasm evaluator.

const std = @import("std");
const TestCase = @import("parallel_runner.zig").TestCase;
const simd_test_sources = @import("simd_test_sources");

const oracle_source = simd_test_sources.oracle;
const differential_source_with_package_import = simd_test_sources.differential;

const LaneBoundsSpec = struct {
    type_name: []const u8,
    lane_count: u8,
};

const lane_bounds_specs = [_]LaneBoundsSpec{
    .{ .type_name = "U8x16", .lane_count = 16 },
    .{ .type_name = "I8x16", .lane_count = 16 },
    .{ .type_name = "U16x8", .lane_count = 8 },
    .{ .type_name = "I16x8", .lane_count = 8 },
    .{ .type_name = "U32x4", .lane_count = 4 },
    .{ .type_name = "I32x4", .lane_count = 4 },
    .{ .type_name = "U64x2", .lane_count = 2 },
    .{ .type_name = "I64x2", .lane_count = 2 },
};

fn laneBoundsTests() [lane_bounds_specs.len * 3]TestCase {
    var result: [lane_bounds_specs.len * 3]TestCase = undefined;
    inline for (lane_bounds_specs, 0..) |spec, i| {
        result[i * 3] = .{
            .name = std.fmt.comptimePrint("SIMD {s}.get_lane rejects index {d}", .{ spec.type_name, spec.lane_count }),
            .source = std.fmt.comptimePrint("{s}.get_lane({s}.default(), {d})", .{ spec.type_name, spec.type_name, spec.lane_count }),
            .expected = .{ .crash = {} },
        };
        result[i * 3 + 1] = .{
            .name = std.fmt.comptimePrint("SIMD {s}.with_lane rejects index {d}", .{ spec.type_name, spec.lane_count }),
            .source = std.fmt.comptimePrint("{s}.with_lane({s}.default(), {d}, 0).to_u128_bits()", .{ spec.type_name, spec.type_name, spec.lane_count }),
            .expected = .{ .crash = {} },
        };
        result[i * 3 + 2] = .{
            .name = std.fmt.comptimePrint("SIMD {s}.broadcast_lane rejects index {d}", .{ spec.type_name, spec.lane_count }),
            .source = std.fmt.comptimePrint("{s}.broadcast_lane({s}.default(), {d}).to_u128_bits()", .{ spec.type_name, spec.type_name, spec.lane_count }),
            .expected = .{ .crash = {} },
        };
    }
    return result;
}

fn differentialSource() []const u8 {
    const package_import = "import oracle.SimdOracle";
    const direct_import = "import SimdOracle";
    const start = comptime std.mem.find(u8, differential_source_with_package_import, package_import) orelse
        @compileError("SIMD differential module no longer imports oracle.SimdOracle");
    return differential_source_with_package_import[0..start] ++
        direct_import ++
        differential_source_with_package_import[start + package_import.len ..];
}

/// The dedicated SIMD gate runs this single source through every eval backend;
/// the runtime app and its `expect` supply the standalone and CTFE lanes.
pub const tests = [_]TestCase{
    .{
        .name = "SIMD typed loads preserve numeric lanes at unit offsets",
        .source =
        \\{
        \\    words : List(U16)
        \\    words = [9, 256, 32768, 65535, 127, 128, 4660, 43981, 1]
        \\    dwords : List(U32)
        \\    dwords = [9, 65536, 2147483648, 4294967295, 305419896]
        \\    match (U16x8.load_units(words, 1), U32x4.load_units(dwords, 1)) {
        \\        (Ok(a), Ok(b)) => a.to_list() == [256, 32768, 65535, 127, 128, 4660, 43981, 1] and b.to_list() == [65536, 2147483648, 4294967295, 305419896]
        \\        _ => False
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "SIMD typed loads reject short lists and overflowing unit offsets",
        .source =
        \\{
        \\    short16 = U16x8.load_units([1, 2, 3, 4, 5, 6, 7], 0)
        \\    short32 = U32x4.load_units([1, 2, 3], 0)
        \\    past16 = U16x8.load_units([1, 2, 3, 4, 5, 6, 7, 8], 1)
        \\    past32 = U32x4.load_units([1, 2, 3, 4], 1)
        \\    huge16 = U16x8.load_units([1, 2, 3, 4, 5, 6, 7, 8], U64.highest)
        \\    huge32 = U32x4.load_units([1, 2, 3, 4], U64.highest)
        \\    match (short16, short32, past16, past32, huge16, huge32) {
        \\        (Err(OutOfBounds), Err(OutOfBounds), Err(OutOfBounds), Err(OutOfBounds), Err(OutOfBounds), Err(OutOfBounds)) => True
        \\        _ => False
        \\    }
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
    .{
        .name = "SIMD full differential corpus",
        .source = "SimdDifferential.run_corpus(21345817372864405881847059188222722561)",
        .imports = &.{
            .{ .name = "SimdOracle", .source = oracle_source },
            .{ .name = "SimdDifferential", .source = differentialSource() },
        },
        .expected = .{ .inspect_str = "True" },
        .opt_in = true,
    },
    .{
        .name = "SIMD concat shift rejects counts above sixteen",
        .source = "U8x16.concat_shift_bytes(U8x16.splat(1), U8x16.splat(2), 17).to_u128_bits()",
        .expected = .{ .crash = {} },
    },
    .{
        .name = "SIMD structural equality compares all 128 bits",
        .source =
        \\{
        \\    left = { vector: U8x16.from_u128_bits(18446744073709551616) }
        \\    right = { vector: U8x16.from_u128_bits(36893488147419103232) }
        \\    left != right
        \\}
        ,
        .expected = .{ .inspect_str = "True" },
    },
} ++ laneBoundsTests();
