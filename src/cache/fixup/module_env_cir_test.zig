//! Test serialization and deserialization of ModuleEnv and CIR using FixupCache strategy

const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");
const types = @import("../../types.zig");
const CIR = @import("../../check/canonicalize.zig").CIR;
const writeAlignedData = @import("../../base/write_aligned.zig").writeAlignedData;

test "ModuleEnv serialization and relocation" {
    const allocator = std.testing.allocator;

    // Create a ModuleEnv with test data
    var env = try base.ModuleEnv.init(allocator, "test module source code");
    defer env.deinit();

    // Add some idents
    const region1 = base.Region{ .start = 0, .end = 4 };
    const region2 = base.Region{ .start = 5, .end = 10 };
    const ident1 = try env.idents.insert(allocator, base.Ident.for_text("test"), region1);
    const ident2 = try env.idents.insert(allocator, base.Ident.for_text("variable"), region2);
    _ = try env.idents.insert(allocator, base.Ident.for_text("test"), region1); // Duplicate

    // Add to ident_ids_for_slicing
    _ = try env.ident_ids_for_slicing.append(allocator, ident1);
    _ = try env.ident_ids_for_slicing.append(allocator, ident2);

    // Add some strings
    const str1 = try env.strings.insert(allocator, "hello world");
    const str2 = try env.strings.insert(allocator, "roc programming language");
    _ = str1;
    _ = str2;

    // Add some types
    const var1 = try env.types.fresh();
    const var2 = try env.types.fresh();
    _ = var1;
    _ = var2;

    // Add to exposed maps using intern indices
    const my_func_idx = env.idents.interner.outer_indices.items.len - 2; // ident1 was "myFunc"
    const other_func_idx = env.idents.interner.outer_indices.items.len - 1; // ident2 was "otherFunc"
    try env.exposed_items.put(allocator, @intCast(my_func_idx), 42);
    try env.exposed_items.put(allocator, @intCast(other_func_idx), 123);

    // Freeze exposed maps before serialization
    try env.freeze();

    // Add line starts
    _ = try env.line_starts.append(allocator, 0);
    _ = try env.line_starts.append(allocator, 10);
    _ = try env.line_starts.append(allocator, 25);

    // Calculate size and allocate buffer
    const size = env.serializedSize();
    const buffer = try allocator.alignedAlloc(u8, @alignOf(base.ModuleEnv), size);
    defer allocator.free(buffer);

    // Serialize
    const bytes_written = try env.serializeInto(buffer);
    try std.testing.expect(bytes_written <= size);

    // Simulate writing to disk and reading back
    const file_content = buffer[0..bytes_written];
    const read_buffer = try allocator.alignedAlloc(u8, @alignOf(base.ModuleEnv), file_content.len);
    defer allocator.free(read_buffer);
    @memcpy(read_buffer, file_content);

    // Cast to ModuleEnv
    const loaded_env = @as(*base.ModuleEnv, @ptrCast(@alignCast(read_buffer.ptr)));

    // Apply relocations
    const base_offset = @as(isize, @intCast(@intFromPtr(read_buffer.ptr)));
    loaded_env.relocate(base_offset);

    // Set the allocator
    loaded_env.gpa = allocator;

    // Verify idents
    try std.testing.expectEqual(@as(u32, 2), loaded_env.idents.interner.outer_indices.items.len);
    try std.testing.expectEqualStrings("test", loaded_env.idents.interner.getText(ident1));
    try std.testing.expectEqualStrings("variable", loaded_env.idents.interner.getText(ident2));

    // Verify deduplication worked
    try std.testing.expect(loaded_env.idents.interner.indicesHaveSameText(ident1, @enumFromInt(2)));

    // Verify regions
    const loaded_region1 = loaded_env.idents.interner.getRegion(ident1);
    try std.testing.expectEqual(region1.start, loaded_region1.start);
    try std.testing.expectEqual(region1.end, loaded_region1.end);

    // Verify ident_ids_for_slicing
    try std.testing.expectEqual(@as(u32, 2), loaded_env.ident_ids_for_slicing.len());
    try std.testing.expectEqual(ident1, loaded_env.ident_ids_for_slicing.get(0));
    try std.testing.expectEqual(ident2, loaded_env.ident_ids_for_slicing.get(1));

    // Verify strings
    try std.testing.expectEqualStrings("hello world", loaded_env.strings.get(str1));
    try std.testing.expectEqualStrings("roc programming language", loaded_env.strings.get(str2));

    // Verify types store has correct number of variables
    try std.testing.expectEqual(@as(usize, 2), loaded_env.types.len());

    // Verify exposed maps
    try std.testing.expectEqual(@as(usize, 2), loaded_env.exposed_items.count());

    // Find the intern indices in the loaded environment
    var my_func_loaded_idx: ?u32 = null;
    var other_func_loaded_idx: ?u32 = null;
    for (loaded_env.idents.interner.outer_indices.items, 0..) |_, idx| {
        const text = loaded_env.idents.interner.getText(@enumFromInt(idx));
        if (std.mem.eql(u8, text, "myFunc")) {
            my_func_loaded_idx = @intCast(idx);
        } else if (std.mem.eql(u8, text, "otherFunc")) {
            other_func_loaded_idx = @intCast(idx);
        }
    }

    try std.testing.expect(my_func_loaded_idx != null);
    try std.testing.expect(other_func_loaded_idx != null);
    try std.testing.expect(loaded_env.exposed_items.isExposed(allocator, my_func_loaded_idx.?));
    try std.testing.expect(loaded_env.exposed_items.isExposed(allocator, other_func_loaded_idx.?));

    try std.testing.expectEqual(@as(usize, 2), loaded_env.exposed_items.count());
    try std.testing.expectEqual(@as(u16, 42), loaded_env.exposed_items.getNodeIndex(allocator, my_func_loaded_idx.?).?);
    try std.testing.expectEqual(@as(u16, 123), loaded_env.exposed_items.getNodeIndex(allocator, other_func_loaded_idx.?).?);

    // Verify line starts
    try std.testing.expectEqual(@as(u32, 3), loaded_env.line_starts.len());
    try std.testing.expectEqual(@as(u32, 0), loaded_env.line_starts.get(0));
    try std.testing.expectEqual(@as(u32, 10), loaded_env.line_starts.get(1));
    try std.testing.expectEqual(@as(u32, 25), loaded_env.line_starts.get(2));

    // Verify source
    try std.testing.expectEqualStrings("test module source code", loaded_env.source);
}

test "CIR serialization and relocation" {
    const allocator = std.testing.allocator;

    // Create ModuleEnv first
    var env = try base.ModuleEnv.init(allocator, "test source");
    defer env.deinit();

    // Create CIR
    var cir = try CIR.init(&env, "TestModule");
    defer cir.deinit();

    // Add some test data to the CIR

    // Add some nodes using the proper methods
    const expr1 = try cir.store.addExpr(.{ .e_int = .{ .bytes = @bitCast(@as(i128, 42)), .kind = .i128 } }, base.Region{ .start = 0, .end = 5 });
    const expr2 = try cir.store.addExpr(.{ .e_int = .{ .bytes = @bitCast(@as(i128, 100)), .kind = .i128 } }, base.Region{ .start = 6, .end = 8 });
    _ = expr1;
    _ = expr2;

    // Add some extra data
    try cir.store.extra_data.append(allocator, 100);
    try cir.store.extra_data.append(allocator, 200);
    try cir.store.extra_data.append(allocator, 300);

    // Add to scratch arrays
    try cir.store.scratch_exprs.append(allocator, @enumFromInt(1));
    try cir.store.scratch_exprs.append(allocator, @enumFromInt(2));

    // Add external decls
    _ = try cir.external_decls.append(allocator, .{
        .name = @enumFromInt(1),
        .module_name = @enumFromInt(2),
    });

    // Add imports
    const import1 = try cir.imports.getOrPut(allocator, "Base");
    const import2 = try cir.imports.getOrPut(allocator, "List");
    _ = import1;
    _ = import2;

    // Set spans
    cir.all_defs = .{ .span = .{ .start = 0, .len = 2 } };
    cir.all_statements = .{ .span = .{ .start = 2, .len = 3 } };

    // Calculate size and allocate buffer
    const size = cir.serializedSize();
    const buffer = try allocator.alignedAlloc(u8, @alignOf(CIR), size);
    defer allocator.free(buffer);

    // Serialize
    const bytes_written = try cir.serializeInto(buffer);
    try std.testing.expect(bytes_written <= size);

    // Simulate writing to disk and reading back
    const file_content = buffer[0..bytes_written];
    const read_buffer = try allocator.alignedAlloc(u8, @alignOf(CIR), file_content.len);
    defer allocator.free(read_buffer);
    @memcpy(read_buffer, file_content);

    // Cast to CIR
    const loaded_cir = @as(*CIR, @ptrCast(@alignCast(read_buffer.ptr)));

    // Apply relocations
    const base_offset = @as(isize, @intCast(@intFromPtr(read_buffer.ptr)));
    loaded_cir.relocate(base_offset);

    // Set the env and gpa
    loaded_cir.env = &env;
    loaded_cir.store.gpa = allocator;

    // Verify nodes (at least 2 - there may be more from internal operations)
    try std.testing.expect(loaded_cir.store.nodes.items.len >= 2);

    // Verify extra data
    try std.testing.expectEqual(@as(usize, 3), loaded_cir.store.extra_data.items.len);
    try std.testing.expectEqual(@as(u32, 100), loaded_cir.store.extra_data.items[0]);
    try std.testing.expectEqual(@as(u32, 200), loaded_cir.store.extra_data.items[1]);
    try std.testing.expectEqual(@as(u32, 300), loaded_cir.store.extra_data.items[2]);

    // Verify scratch arrays
    try std.testing.expectEqual(@as(usize, 2), loaded_cir.store.scratch_exprs.items.items.len);

    // Verify external decls
    try std.testing.expectEqual(@as(u32, 1), loaded_cir.external_decls.len());

    // Verify imports
    try std.testing.expectEqual(@as(usize, 2), loaded_cir.imports.imports.items.len);
    try std.testing.expect(loaded_cir.imports.contains(allocator, "Base"));
    try std.testing.expect(loaded_cir.imports.contains(allocator, "List"));

    // Verify module name
    try std.testing.expectEqualStrings("TestModule", loaded_cir.module_name);

    // Verify spans
    try std.testing.expectEqual(@as(u32, 0), loaded_cir.all_defs.span.start);
    try std.testing.expectEqual(@as(u32, 2), loaded_cir.all_defs.span.len);
    try std.testing.expectEqual(@as(u32, 2), loaded_cir.all_statements.span.start);
    try std.testing.expectEqual(@as(u32, 3), loaded_cir.all_statements.span.len);
}

test "Combined ModuleEnv and CIR serialization" {
    const allocator = std.testing.allocator;

    // Create ModuleEnv with data
    var env = try base.ModuleEnv.init(allocator, "module source code for testing");
    defer env.deinit();

    // Add idents
    const ident1 = try env.idents.insert(allocator, base.Ident.for_text("main"), base.Region{ .start = 0, .end = 4 });
    const ident2 = try env.idents.insert(allocator, base.Ident.for_text("helper"), base.Region{ .start = 10, .end = 16 });

    // Create CIR using the env
    var cir = try CIR.init(&env, "MainModule");
    defer cir.deinit();

    // Add CIR data that references env idents
    const var_expr1 = try cir.store.addExpr(.{ .e_lookup_local = ident1 }, base.Region{ .start = 0, .end = 4 });
    const var_expr2 = try cir.store.addExpr(.{ .e_lookup_local = ident2 }, base.Region{ .start = 10, .end = 16 });
    _ = var_expr1;
    _ = var_expr2;

    // Calculate total size needed
    const env_size = env.serializedSize();
    const cir_size = cir.serializedSize();
    const total_size = env_size + cir_size;

    // Allocate buffer
    const buffer = try allocator.alignedAlloc(u8, @alignOf(base.ModuleEnv), total_size);
    defer allocator.free(buffer);

    // Serialize both
    const env_bytes = try env.serializeInto(buffer);
    const cir_bytes = try cir.serializeInto(buffer[env_bytes..]);
    const total_bytes = env_bytes + cir_bytes;

    // Simulate disk I/O
    const read_buffer = try allocator.alignedAlloc(u8, @alignOf(base.ModuleEnv), total_bytes);
    defer allocator.free(read_buffer);
    @memcpy(read_buffer, buffer[0..total_bytes]);

    // Deserialize
    const loaded_env = @as(*base.ModuleEnv, @ptrCast(@alignCast(read_buffer.ptr)));
    const loaded_cir = @as(*CIR, @ptrCast(@alignCast(read_buffer.ptr + env_bytes)));

    // Apply relocations
    const base_offset = @as(isize, @intCast(@intFromPtr(read_buffer.ptr)));
    loaded_env.relocate(base_offset);
    loaded_cir.relocate(base_offset);

    // Fix up cross-references
    loaded_env.gpa = allocator;
    loaded_cir.env = loaded_env;
    loaded_cir.store.gpa = allocator;

    // Verify everything works
    try std.testing.expectEqualStrings("main", loaded_env.idents.interner.getText(ident1));
    try std.testing.expectEqualStrings("helper", loaded_env.idents.interner.getText(ident2));
    try std.testing.expect(loaded_cir.store.nodes.items.len >= 2);
    try std.testing.expectEqualStrings("MainModule", loaded_cir.module_name);

    // Verify the CIR expressions reference the correct idents
    const expr1_data = loaded_cir.store.getExpr(var_expr1);
    const expr2_data = loaded_cir.store.getExpr(var_expr2);
    try std.testing.expectEqual(base.Ident.Idx, @TypeOf(expr1_data.e_lookup_local));
    try std.testing.expectEqual(base.Ident.Idx, @TypeOf(expr2_data.e_lookup_local));
    try std.testing.expectEqual(ident1, expr1_data.e_lookup_local);
    try std.testing.expectEqual(ident2, expr2_data.e_lookup_local);
}
