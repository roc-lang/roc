const std = @import("std");
const testing = std.testing;
const base = @import("../../../base.zig");
const check = @import("../../mod.zig");
const reporting = @import("../../../reporting/mod.zig");
const CIR = @import("../CIR.zig");
const Diagnostic = @import("../Diagnostic.zig");

test "invalid number literal - too large for any numeric type" {
    const source =
        \\app [main] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\main = 999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
    ;

    var test_arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer test_arena.deinit();
    const arena = test_arena.allocator();

    // Parse the source
    var env = base.Env.init(arena, null, null);
    defer env.deinit();

    const parse_result = check.parse.parse(arena, &env, source, "test.roc");
    try testing.expect(parse_result == .success);

    // Canonicalize
    var can = check.canonicalize.Canonicalize.init(arena, &env, parse_result.success);
    defer can.deinit();

    can.run();

    // Check that we have diagnostics
    const diagnostics = can.can_ir.getDiagnostics();
    try testing.expect(diagnostics.len > 0);

    // Find the invalid number literal diagnostic
    var found_invalid_num = false;
    for (diagnostics) |diag| {
        switch (diag) {
            .invalid_num_literal => |data| {
                found_invalid_num = true;

                // Verify the region captures the entire number
                const literal_text = source[data.region.start..data.region.end];
                try testing.expect(std.mem.startsWith(u8, literal_text, "9999999"));

                // Test that the diagnostic report includes the literal text
                const report = try can.can_ir.diagnosticToReport(diag, arena, source, "test.roc");

                // The report should contain the actual literal
                var buf: [1024]u8 = undefined;
                var stream = std.io.fixedBufferStream(&buf);
                try report.render(stream.writer(), .plain_text);
                const rendered = stream.getWritten();

                try testing.expect(std.mem.indexOf(u8, rendered, "999999999") != null);
            },
            else => {},
        }
    }

    try testing.expect(found_invalid_num);
}

test "invalid number literal - negative number too large" {
    const source =
        \\app [main] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\main = -999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
    ;

    var test_arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer test_arena.deinit();
    const arena = test_arena.allocator();

    // Parse the source
    var env = base.Env.init(arena, null, null);
    defer env.deinit();

    const parse_result = check.parse.parse(arena, &env, source, "test.roc");
    try testing.expect(parse_result == .success);

    // Canonicalize
    var can = check.canonicalize.Canonicalize.init(arena, &env, parse_result.success);
    defer can.deinit();

    can.run();

    // Check that we have diagnostics
    const diagnostics = can.can_ir.getDiagnostics();
    try testing.expect(diagnostics.len > 0);

    // Find the invalid number literal diagnostic
    var found_invalid_num = false;
    for (diagnostics) |diag| {
        switch (diag) {
            .invalid_num_literal => |data| {
                found_invalid_num = true;

                // Verify the region captures the entire number including the minus
                const literal_text = source[data.region.start..data.region.end];
                try testing.expect(std.mem.startsWith(u8, literal_text, "-9999999"));

                // Test that the diagnostic report includes the literal text
                const report = try can.can_ir.diagnosticToReport(diag, arena, source, "test.roc");

                // The report should contain the actual literal
                var buf: [1024]u8 = undefined;
                var stream = std.io.fixedBufferStream(&buf);
                try report.render(stream.writer(), .plain_text);
                const rendered = stream.getWritten();

                try testing.expect(std.mem.indexOf(u8, rendered, "-999999999") != null);
            },
            else => {},
        }
    }

    try testing.expect(found_invalid_num);
}

test "invalid number literal - malformed float" {
    const source =
        \\app [main] { pf: platform "../basic-cli/platform.roc" }
        \\
        \\main = 1e999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
    ;

    var test_arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer test_arena.deinit();
    const arena = test_arena.allocator();

    // Parse the source
    var env = base.Env.init(arena, null, null);
    defer env.deinit();

    const parse_result = check.parse.parse(arena, &env, source, "test.roc");
    try testing.expect(parse_result == .success);

    // Canonicalize
    var can = check.canonicalize.Canonicalize.init(arena, &env, parse_result.success);
    defer can.deinit();

    can.run();

    // Check that we have diagnostics
    const diagnostics = can.can_ir.getDiagnostics();
    try testing.expect(diagnostics.len > 0);

    // Find the invalid number literal diagnostic
    var found_invalid_num = false;
    for (diagnostics) |diag| {
        switch (diag) {
            .invalid_num_literal => |data| {
                found_invalid_num = true;

                // Verify the region captures the entire number
                const literal_text = source[data.region.start..data.region.end];
                try testing.expect(std.mem.startsWith(u8, literal_text, "1e9999999"));

                // Test that the diagnostic report includes the literal text
                const report = try can.can_ir.diagnosticToReport(diag, arena, source, "test.roc");

                // The report should contain the actual literal with its scientific notation
                var buf: [1024]u8 = undefined;
                var stream = std.io.fixedBufferStream(&buf);
                try report.render(stream.writer(), .plain_text);
                const rendered = stream.getWritten();

                try testing.expect(std.mem.indexOf(u8, rendered, "1e999999") != null);
            },
            else => {},
        }
    }

    try testing.expect(found_invalid_num);
}
