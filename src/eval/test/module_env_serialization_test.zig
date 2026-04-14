//! ModuleEnv serialization roundtrip test using the LIR interpreter.
const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");
const collections = @import("collections");

const helpers = @import("helpers.zig");

const ModuleEnv = can.ModuleEnv;
const CompactWriter = collections.CompactWriter;
const testing = std.testing;

// Use interpreter_allocator for interpreter tests (doesn't track leaks).
const test_allocator = helpers.interpreter_allocator;

test "ModuleEnv serialization and LIR evaluation" {
    const source = "5 + 8";
    const gpa = test_allocator;

    var resources = try helpers.parseAndCanonicalizeExpr(gpa, source);
    defer helpers.cleanupParseAndCanonical(gpa, resources);

    {
        var lowered = try helpers.lowerParsedExprToLir(gpa, &resources);
        defer lowered.deinit();
        const int_value = try helpers.evalLoweredNumericI128(gpa, &lowered);
        try testing.expectEqual(@as(i128, 13), int_value);
    }

    var serialization_arena = std.heap.ArenaAllocator.init(gpa);
    defer serialization_arena.deinit();
    const arena_alloc = serialization_arena.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile("test_module_env.compact", .{ .read = true });
    defer tmp_file.close();

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
        .allocated_memory = .{},
    };
    defer writer.deinit(arena_alloc);

    const env_ptr = try writer.appendAlloc(arena_alloc, ModuleEnv.Serialized);
    const env_start_offset = writer.total_bytes - @sizeOf(ModuleEnv.Serialized);
    const serialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(env_ptr)));
    try serialized_ptr.serialize(resources.module_env, arena_alloc, &writer);

    try writer.writeGather(arena_alloc, tmp_file);

    const file_size = try tmp_file.getEndPos();
    const buffer = try gpa.alignedAlloc(u8, std.mem.Alignment.fromByteUnits(@alignOf(ModuleEnv)), @intCast(file_size));
    defer gpa.free(buffer);
    const read_len = try tmp_file.pread(buffer, 0);
    try testing.expectEqual(buffer.len, read_len);

    const deserialized_ptr = @as(*ModuleEnv.Serialized, @ptrCast(@alignCast(buffer.ptr + env_start_offset)));
    const deserialized_env = try deserialized_ptr.deserializeInto(@intFromPtr(buffer.ptr), gpa, source, "TestModule");
    defer {
        deserialized_env.imports.deinitMapOnly(gpa);
        deserialized_env.import_mapping.deinit();
        deserialized_env.rigid_vars.deinit(gpa);
        deserialized_env.common.idents.interner.deinit(gpa);
        gpa.destroy(deserialized_env);
    }

    try testing.expectEqualStrings("TestModule", deserialized_env.module_name);
    try testing.expectEqualStrings(source, deserialized_env.common.source);
    try testing.expect(deserialized_env.types.len() > 0);
    try testing.expect(deserialized_env.store.nodes.items.len > 0);
    try testing.expectEqual(resources.module_env.types.len(), deserialized_env.types.len());
    try testing.expectEqual(resources.module_env.store.nodes.items.len, deserialized_env.store.nodes.items.len);
    try testing.expectEqual(resources.module_env.common.idents.interner.bytes.len(), deserialized_env.common.idents.interner.bytes.len());

    try deserialized_env.common.idents.interner.enableRuntimeInserts(gpa);
    try @constCast(resources.builtin_module.env).common.idents.interner.enableRuntimeInserts(gpa);

    if (deserialized_env.display_module_name_idx.isNone() and deserialized_env.module_name.len > 0) {
        deserialized_env.display_module_name_idx = try deserialized_env.insertIdent(base.Ident.for_text(deserialized_env.module_name));
        deserialized_env.qualified_module_ident = deserialized_env.display_module_name_idx;
    }
    if (resources.builtin_module.env.display_module_name_idx.isNone() and resources.builtin_module.env.module_name.len > 0) {
        @constCast(resources.builtin_module.env).display_module_name_idx =
            try @constCast(resources.builtin_module.env).insertIdent(base.Ident.for_text(resources.builtin_module.env.module_name));
        @constCast(resources.builtin_module.env).qualified_module_ident = resources.builtin_module.env.display_module_name_idx;
    }

    var typed_cir_modules = try check.TypedCIR.Modules.publish(gpa, &.{
        .{ .precompiled = deserialized_env },
        .{ .precompiled = resources.builtin_module.env },
    });
    defer typed_cir_modules.deinit();

    const module_envs = [_]*const ModuleEnv{ deserialized_env, resources.builtin_module.env };
    var lowered = try helpers.lowerTypedCIRToLir(gpa, &typed_cir_modules, &module_envs);
    defer lowered.deinit();

    const int_value = try helpers.evalLoweredNumericI128(gpa, &lowered);
    try testing.expectEqual(@as(i128, 13), int_value);
}
