//! Test for duplicate exposed item diagnostics

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const canonicalize = @import("../canonicalize.zig");
const CIR = canonicalize.CIR;
const parse = @import("../../parse.zig");

test "duplicate exposed items generate diagnostics" {
    const allocator = testing.allocator;

    // Test source with duplicate exposed items
    const source =
        \\module [foo, bar, foo, baz]
        \\
        \\foo = 42
        \\bar = "hello"
        \\baz = 3.14
    ;

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    var ast = try parse.parse(&env, source);
    defer ast.deinit(allocator);

    var cir = try CIR.init(&env, "Test");
    defer cir.deinit();

    var canonicalizer = try canonicalize.init(&cir, &ast, null);
    defer canonicalizer.deinit();

    try canonicalizer.canonicalizeFile();

    // Get diagnostics from the CIR
    const diagnostics = try cir.getDiagnostics();
    defer allocator.free(diagnostics);

    // Check that we got diagnostics for duplicate exposed items
    var found_duplicate_foo = false;

    for (diagnostics) |diagnostic| {
        switch (diagnostic) {
            .redundant_exposed => |data| {
                const ident_text = env.idents.getText(data.ident);
                if (std.mem.eql(u8, ident_text, "foo")) {
                    found_duplicate_foo = true;
                }
            },
            .duplicate_exposed_item => |data| {
                const ident_text = env.idents.getText(data.item_name);
                if (std.mem.eql(u8, ident_text, "foo")) {
                    found_duplicate_foo = true;
                }
            },
            else => {},
        }
    }

    // We should find a diagnostic for the duplicate "foo"
    // Note: The existing redundant_exposed diagnostic already handles this case
    try testing.expect(found_duplicate_foo);
}

test "duplicate_exposed_item diagnostic end-to-end reporting" {
    const allocator = testing.allocator;

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, "module [foo] \nfoo = 42"));
    defer env.deinit();

    var cir = try CIR.init(&env, "TestModule");
    defer cir.deinit();

    // Create test identifiers and regions
    const ident_idx = try env.idents.insert(allocator, base.Ident.for_text("duplicateItem"), base.Region.zero());
    const duplicate_region = base.Region{ .start = .{ .offset = 15 }, .end = .{ .offset = 27 } };
    const original_region = base.Region{ .start = .{ .offset = 8 }, .end = .{ .offset = 20 } };

    // Create and push the diagnostic
    const diagnostic = CIR.Diagnostic{
        .duplicate_exposed_item = .{
            .item_name = ident_idx,
            .duplicate_region = duplicate_region,
            .original_region = original_region,
        },
    };

    try cir.pushDiagnostic(diagnostic);

    // Verify diagnostic can be converted to a report
    cir.temp_source_for_sexpr = "module [duplicateItem, duplicateItem]";
    const diagnostics = try cir.getDiagnostics();
    defer allocator.free(diagnostics);

    try testing.expectEqual(@as(usize, 1), diagnostics.len);

    const retrieved_diagnostic = diagnostics[0];
    try testing.expect(retrieved_diagnostic == .duplicate_exposed_item);

    // Test report generation
    const report = try cir.diagnosticToReport(allocator, retrieved_diagnostic, "test.roc");
    defer report.deinit();

    // Verify the report contains expected information
    try testing.expect(std.mem.indexOf(u8, report.title, "DUPLICATE EXPOSED ITEM") != null);
}

test "duplicate_exposed_item diagnostic creation and reporting" {
    const allocator = testing.allocator;

    var env = try base.ModuleEnv.init(allocator, try allocator.dupe(u8, ""));
    defer env.deinit();

    var cir = try CIR.init(&env, "Test");
    defer cir.deinit();

    // Create a test identifier
    const ident_idx = try env.idents.insert(allocator, base.Ident.for_text("test_item"), base.Region.zero());
    const duplicate_region = base.Region{ .start = .{ .offset = 10 }, .end = .{ .offset = 20 } };
    const original_region = base.Region{ .start = .{ .offset = 5 }, .end = .{ .offset = 15 } };

    // Create and push the diagnostic
    const diagnostic = CIR.Diagnostic{
        .duplicate_exposed_item = .{
            .item_name = ident_idx,
            .duplicate_region = duplicate_region,
            .original_region = original_region,
        },
    };

    try cir.pushDiagnostic(diagnostic);

    // Get diagnostics and verify our diagnostic was added
    const diagnostics = try cir.getDiagnostics();
    defer allocator.free(diagnostics);

    try testing.expectEqual(@as(usize, 1), diagnostics.len);

    const retrieved_diagnostic = diagnostics[0];
    try testing.expect(retrieved_diagnostic == .duplicate_exposed_item);

    const data = retrieved_diagnostic.duplicate_exposed_item;
    try testing.expectEqual(ident_idx, data.item_name);
    try testing.expectEqual(duplicate_region.start.offset, data.duplicate_region.start.offset);
    try testing.expectEqual(duplicate_region.end.offset, data.duplicate_region.end.offset);
    try testing.expectEqual(original_region.start.offset, data.original_region.start.offset);
    try testing.expectEqual(original_region.end.offset, data.original_region.end.offset);
}

test "SortedArrayBuilder detectDuplicates functionality" {
    const allocator = testing.allocator;
    const SortedArrayBuilder = @import("collections").SortedArrayBuilder;

    var builder = SortedArrayBuilder(u32, u16).init();
    defer builder.deinit(allocator);

    // Add some duplicate keys
    try builder.put(allocator, 100, 1);
    try builder.put(allocator, 200, 2);
    try builder.put(allocator, 100, 3); // duplicate key
    try builder.put(allocator, 300, 4);
    try builder.put(allocator, 200, 5); // duplicate key

    // Detect duplicates
    const duplicates = try builder.detectDuplicates(allocator);
    defer allocator.free(duplicates);

    // Should find duplicates for keys 100 and 200
    try testing.expectEqual(@as(usize, 2), duplicates.len);

    // Sort the duplicates for predictable testing
    std.sort.heap(u32, duplicates, {}, std.sort.asc(u32));
    try testing.expectEqual(@as(u32, 100), duplicates[0]);
    try testing.expectEqual(@as(u32, 200), duplicates[1]);
}

test "ExposedItems with duplicates" {
    const allocator = testing.allocator;
    const ExposedItems = @import("collections").ExposedItems;

    var exposed = ExposedItems.init();
    defer exposed.deinit(allocator);

    // Add some items including duplicates
    try exposed.put(allocator, 100, 42);
    try exposed.put(allocator, 200, 84);
    try exposed.put(allocator, 100, 99); // duplicate key - should keep last value

    // Initial count includes duplicates
    try testing.expectEqual(@as(usize, 3), exposed.count());

    // After ensuring sorted, duplicates should be removed
    exposed.ensureSorted(allocator);
    try testing.expectEqual(@as(usize, 2), exposed.count());

    // Should return the last value for the duplicate key
    try testing.expectEqual(@as(?u16, 99), exposed.getNodeIndex(allocator, 100));
    try testing.expectEqual(@as(?u16, 84), exposed.getNodeIndex(allocator, 200));
}
