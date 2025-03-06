/// Note: Compiling the fuzz tests requires llvm and does not currently work in our nix shell on all systems.
///
/// To run:
///  1. zig build fuzz-parse
///  2. ./zig-out/AFLplusplus/bin/afl-fuzz -i src/fuzz-corpus/parse/ -o /tmp/parse-out/ zig-out/bin/fuzz-parse
///
/// Other afl commands also available in `./zig-out/AFLplusplus/bin`
///
const std = @import("std");
const fmt = @import("fmt.zig");
const parse = @import("check/parse.zig");
const base = @import("base.zig");

pub export fn zig_fuzz_init() void {}

pub export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    zig_fuzz_test_inner(buf, len, false);
}

pub fn zig_fuzz_test_inner(buf: [*]u8, len: isize, debug: bool) void {
    // We reinitialize the gpa on every loop of the fuzzer.
    // This enables the gpa to do leak checking on each iteration.
    var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        _ = gpa_impl.deinit();
    }
    const gpa = gpa_impl.allocator();

    const input = buf[0..@intCast(len)];

    if (debug) {
        std.debug.print("Original:\n==========\n{s}\n==========\n\n", .{input});
    }

    const formatted = parse_and_format(gpa, input, debug);
    if (formatted == null) {
        // Failed to parse, nothing else to do.
        return;
    }
    defer gpa.free(formatted.?);

    const formatted_twice = parse_and_format(gpa, formatted.?, debug);
    std.debug.assert(formatted_twice != null);
    defer gpa.free(formatted_twice.?);

    std.testing.expectEqualStrings(formatted.?, formatted_twice.?) catch @panic("Input does not format same on second try");
}

fn parse_and_format(gpa: std.mem.Allocator, input: []const u8, debug: bool) ?[]const u8 {
    var module_env = base.ModuleEnv.init(gpa);
    defer module_env.deinit();

    var parse_ast = parse.parse(&module_env, input);
    defer parse_ast.deinit();

    if (debug) {
        std.debug.print("Parsed SExpr:\n==========\n", .{});
        parse_ast.toSExprStr(&module_env, std.io.getStdErr().writer().any()) catch @panic("Failed to print SExpr");
        std.debug.print("\n==========\n\n", .{});
    }

    if (parse_ast.errors.len > 0) {
        // Failed to parse, nothing else to do.
        if (debug) {
            std.debug.print("Errors: {any}", .{parse_ast.errors});
        }
        return null;
    }

    var formatter = fmt.init(parse_ast);
    defer formatter.deinit();

    const formatted = formatter.formatFile();

    if (debug) {
        std.debug.print("Formatted:\n==========\n{s}\n==========\n\n", .{formatted});
    }
    return formatted;
}
