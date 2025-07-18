//! Tests for displaying nominal type origins in error messages

const std = @import("std");
const testing = std.testing;
const base = @import("base");
const types_mod = @import("types");
const snapshot = @import("../snapshot.zig");
const Ident = base.Ident;

const test_allocator = testing.allocator;

test "nominal type origin - displays origin in snapshot writer" {
    // Create a simple test environment
    var idents = try Ident.Store.initCapacity(test_allocator, 16);
    defer idents.deinit(test_allocator);

    // Create module name identifiers
    const current_module_ident = try idents.insert(test_allocator, Ident.for_text("CurrentModule"), base.Region.zero());
    const other_module_ident = try idents.insert(test_allocator, Ident.for_text("Data.Types"), base.Region.zero());
    const type_name_ident = try idents.insert(test_allocator, Ident.for_text("Person"), base.Region.zero());

    // Create a snapshot store
    var snapshots = try snapshot.Store.initCapacity(test_allocator, 16);
    defer snapshots.deinit();

    // Create a nominal type snapshot with origin from a different module
    const nominal_type_backing = snapshot.SnapshotContent{ .structure = .str };
    const nominal_type_backing_idx = try snapshots.contents.append(test_allocator, nominal_type_backing);
    const vars_range = try snapshots.content_indexes.appendSlice(test_allocator, &.{nominal_type_backing_idx});

    const nominal_type = snapshot.SnapshotNominalType{
        .ident = types_mod.TypeIdent{ .ident_idx = type_name_ident },
        .vars = vars_range,
        .origin_module = other_module_ident,
    };

    // Test 1: Origin shown when type is from different module
    {
        var buf = std.ArrayList(u8).init(test_allocator);
        defer buf.deinit();

        var writer = snapshot.SnapshotWriter.init(
            buf.writer(),
            &snapshots,
            &idents,
        );
        writer.current_module_name = "CurrentModule";

        try writer.writeNominalType(nominal_type, nominal_type_backing_idx);

        const result = buf.items;
        // Should show "Person (from Data.Types)"
        try testing.expect(std.mem.indexOf(u8, result, "Person") != null);
        try testing.expect(std.mem.indexOf(u8, result, "(from Data.Types)") != null);
    }

    // Test 2: Origin NOT shown when type is from same module
    {
        var buf = std.ArrayList(u8).init(test_allocator);
        defer buf.deinit();

        // Create a nominal type from the current module
        const same_module_nominal = snapshot.SnapshotNominalType{
            .ident = types_mod.TypeIdent{ .ident_idx = type_name_ident },
            .vars = vars_range,
            .origin_module = current_module_ident,
        };

        var writer = snapshot.SnapshotWriter.init(
            buf.writer(),
            &snapshots,
            &idents,
        );
        writer.current_module_name = "CurrentModule";

        try writer.writeNominalType(same_module_nominal, nominal_type_backing_idx);

        const result = buf.items;
        // Should show just "Person" without origin
        try testing.expect(std.mem.indexOf(u8, result, "Person") != null);
        try testing.expect(std.mem.indexOf(u8, result, "(from CurrentModule)") == null);
    }

    // Test 3: Origin shown with type arguments
    {
        var buf = std.ArrayList(u8).init(test_allocator);
        defer buf.deinit();

        // Create type arguments
        const str_content = snapshot.SnapshotContent{ .structure = .{ .str = {} } };
        const str_idx = try snapshots.contents.append(test_allocator, str_content);
        const args_range = try snapshots.content_indexes.appendSlice(test_allocator, &.{ nominal_type_backing_idx, str_idx });

        // Create a nominal type with args from a different module
        const generic_nominal = snapshot.SnapshotNominalType{
            .ident = types_mod.TypeIdent{ .ident_idx = type_name_ident },
            .vars = args_range,
            .origin_module = other_module_ident,
        };

        var writer = snapshot.SnapshotWriter.init(
            buf.writer(),
            &snapshots,
            &idents,
        );
        writer.current_module_name = "CurrentModule";

        try writer.writeNominalType(generic_nominal, nominal_type_backing_idx);

        const result = buf.items;
        // Should show "Person(Str) (from Data.Types)"
        try testing.expect(std.mem.indexOf(u8, result, "Person(Str)") != null);
        try testing.expect(std.mem.indexOf(u8, result, "(from Data.Types)") != null);
    }
}

test "nominal type origin - works with no context" {
    // Test that the code doesn't crash when context is not provided
    var idents = try Ident.Store.initCapacity(test_allocator, 16);
    defer idents.deinit(test_allocator);

    const type_name_ident = try idents.insert(test_allocator, Ident.for_text("MyType"), base.Region.zero());
    const module_ident = try idents.insert(test_allocator, Ident.for_text("SomeModule"), base.Region.zero());

    var snapshots = try snapshot.Store.initCapacity(test_allocator, 16);
    defer snapshots.deinit();

    const nominal_type_backing = snapshot.SnapshotContent{ .structure = .str };
    const nominal_type_backing_idx = try snapshots.contents.append(test_allocator, nominal_type_backing);
    const vars_range = try snapshots.content_indexes.appendSlice(test_allocator, &.{nominal_type_backing_idx});

    const nominal_type = snapshot.SnapshotNominalType{
        .ident = types_mod.TypeIdent{ .ident_idx = type_name_ident },
        .vars = vars_range,
        .origin_module = module_ident,
    };

    var buf = std.ArrayList(u8).init(test_allocator);
    defer buf.deinit();

    // Use the basic init without context
    var writer = snapshot.SnapshotWriter.init(
        buf.writer(),
        &snapshots,
        &idents,
    );

    try writer.writeNominalType(nominal_type, nominal_type_backing_idx);

    const result = buf.items;
    // Should show just "MyType" without origin info
    try testing.expect(std.mem.indexOf(u8, result, "MyType") != null);
    try testing.expect(std.mem.indexOf(u8, result, "(from") == null);
}
