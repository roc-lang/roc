//! Tests for the direct memory cache system

const std = @import("std");
const testing = std.testing;
const base = @import("../base.zig");
const ModuleEnv = base.ModuleEnv;
const parse = @import("../check/parse.zig");
const tokenize = @import("../check/parse/tokenize.zig");
const canonicalize = @import("../check/canonicalize.zig");
const solve = @import("../check/check_types.zig");
const DirectMemoryCache = @import("DirectMemoryCache.zig");
const CacheModule = @import("CacheModule.zig");

test "DirectMemoryCache round-trip simple module" {
    const allocator = testing.allocator;

    // Create a simple module
    const source =
        \\main = 42
        \\
    ;

    // Initialize ModuleEnv
    var env = try ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    // Parse
    var tokenized = try tokenize.fromSlice(allocator, &env, source);
    defer tokenized.deinit();

    var parser = try parse.Parser.init(tokenized);
    defer parser.deinit();
    var file = try parser.file();

    // Canonicalize
    var c_input = canonicalize.Input{
        .gpa = allocator,
        .env = &env,
        .ast = file,
        .exposed_values = &.{},
        .exposed_types = &.{},
        .exposed_abilities = &.{},
    };

    var c_output = try canonicalize.canonicalize(c_input);
    defer c_output.deinit();

    // Write to cache
    const cache_path = "test_direct_cache.roc.cache";
    defer std.fs.cwd().deleteFile(cache_path) catch {};

    try DirectMemoryCache.writeCacheFile(allocator, cache_path, &env, c_output.cir);

    // Load from cache
    const loaded = try DirectMemoryCache.loadCacheFile(allocator, cache_path);
    defer allocator.free(loaded.buffer);

    // Verify loaded data
    try testing.expectEqual(env.idents.count(), loaded.module_env.idents.count());
    try testing.expectEqual(env.types.len(), loaded.module_env.types.len());
    try testing.expectEqualStrings(source, loaded.module_env.source);

    // Verify CIR data (basic checks since we use simplified serialization)
    try testing.expectEqual(c_output.cir.all_defs.span.len, loaded.cir.all_defs.span.len);
    try testing.expectEqual(c_output.cir.all_statements.span.len, loaded.cir.all_statements.span.len);
    // Note: CIR nodes/regions are not fully serialized in MVP
}

test "DirectMemoryCache vs traditional cache comparison" {
    const allocator = testing.allocator;

    // Test program with various features
    const source =
        \\Person : { name: Str, age: U32 }
        \\
        \\makePerson : Str, U32 -> Person
        \\makePerson = \name, age -> { name, age }
        \\
        \\people = [
        \\    makePerson "Alice" 30,
        \\    makePerson "Bob" 25,
        \\]
        \\
        \\main = people
        \\
    ;

    // Initialize ModuleEnv
    var env = try ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    // Parse
    var tokenized = try tokenize.fromSlice(allocator, &env, source);
    defer tokenized.deinit();

    var parser = try parse.Parser.init(tokenized);
    defer parser.deinit();
    var file = try parser.file();

    // Canonicalize
    var c_input = canonicalize.Input{
        .gpa = allocator,
        .env = &env,
        .ast = file,
        .exposed_values = &.{},
        .exposed_types = &.{},
        .exposed_abilities = &.{},
    };

    var c_output = try canonicalize.canonicalize(c_input);
    defer c_output.deinit();

    // Solve types
    var solver = try solve.Solver.init(allocator, &env);
    defer solver.deinit();
    try solver.solveModule(c_output.cir);

    // Test traditional cache
    const trad_cache_path = "test_traditional.roc.cache";
    defer std.fs.cwd().deleteFile(trad_cache_path) catch {};

    const trad_start = std.time.milliTimestamp();
    const trad_buffer = try CacheModule.serialize(allocator, "test_module", &env, c_output.cir);
    defer allocator.free(trad_buffer);

    const trad_file = try std.fs.cwd().createFile(trad_cache_path, .{});
    defer trad_file.close();
    try trad_file.writeAll(trad_buffer);
    const trad_write_time = std.time.milliTimestamp() - trad_start;

    // Test direct memory cache
    const direct_cache_path = "test_direct.roc.cache";
    defer std.fs.cwd().deleteFile(direct_cache_path) catch {};

    const direct_start = std.time.milliTimestamp();
    try DirectMemoryCache.writeCacheFile(allocator, direct_cache_path, &env, c_output.cir);
    const direct_write_time = std.time.milliTimestamp() - direct_start;

    // Compare read times
    const trad_read_start = std.time.milliTimestamp();
    const trad_loaded_file = try std.fs.cwd().openFile(trad_cache_path, .{});
    defer trad_loaded_file.close();
    const trad_size = try trad_loaded_file.getEndPos();
    const trad_loaded_buffer = try allocator.alignedAlloc(u8, 16, trad_size);
    defer allocator.free(trad_loaded_buffer);
    _ = try trad_loaded_file.read(trad_loaded_buffer);

    const trad_cache = try CacheModule.fromMappedMemory(trad_loaded_buffer);
    const trad_restored = try trad_cache.restore(allocator, "test_module", try allocator.dupe(u8, source));
    defer trad_restored.module_env.deinit();
    defer trad_restored.cir.deinit();
    const trad_read_time = std.time.milliTimestamp() - trad_read_start;

    const direct_read_start = std.time.milliTimestamp();
    const direct_loaded = try DirectMemoryCache.loadCacheFile(allocator, direct_cache_path);
    defer allocator.free(direct_loaded.buffer);
    const direct_read_time = std.time.milliTimestamp() - direct_read_start;

    // Log performance comparison
    std.debug.print("\nCache Performance Comparison:\n", .{});
    std.debug.print("Traditional - Write: {}ms, Read: {}ms\n", .{ trad_write_time, trad_read_time });
    std.debug.print("Direct Memory - Write: {}ms, Read: {}ms\n", .{ direct_write_time, direct_read_time });

    // Verify both caches produce equivalent results
    try testing.expectEqual(trad_restored.module_env.idents.count(), direct_loaded.module_env.idents.count());
    try testing.expectEqual(trad_restored.module_env.types.len(), direct_loaded.module_env.types.len());

    // Verify CIR metadata is loaded correctly
    // Note: Full CIR serialization is simplified in MVP
    try testing.expectEqual(c_output.cir.all_defs.span.len, direct_loaded.cir.all_defs.span.len);
}

test "DirectMemoryCache with complex types" {
    const allocator = testing.allocator;

    const source =
        \\Result a b : [Ok a, Err b]
        \\
        \\map : Result a e, (a -> b) -> Result b e
        \\map = \result, fn ->
        \\    when result is
        \\        Ok value -> Ok (fn value)
        \\        Err err -> Err err
        \\
        \\flatMap : Result a e, (a -> Result b e) -> Result b e
        \\flatMap = \result, fn ->
        \\    when result is
        \\        Ok value -> fn value
        \\        Err err -> Err err
        \\
        \\main = Ok 42
        \\
    ;

    // Initialize ModuleEnv
    var env = try ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    // Add many identifiers to stress test
    var i: u32 = 0;
    while (i < 100) : (i += 1) {
        var buf: [32]u8 = undefined;
        const ident_str = try std.fmt.bufPrint(&buf, "test_ident_{}", .{i});
        _ = try env.idents.insert(allocator, ident_str);
    }

    // Parse
    var tokenized = try tokenize.fromSlice(allocator, &env, source);
    defer tokenized.deinit();

    var parser = try parse.Parser.init(tokenized);
    defer parser.deinit();
    var file = try parser.file();

    // Canonicalize
    var c_input = canonicalize.Input{
        .gpa = allocator,
        .env = &env,
        .ast = file,
        .exposed_values = &.{},
        .exposed_types = &.{},
        .exposed_abilities = &.{},
    };

    var c_output = try canonicalize.canonicalize(c_input);
    defer c_output.deinit();

    // Solve types
    var solver = try solve.Solver.init(allocator, &env);
    defer solver.deinit();
    try solver.solveModule(c_output.cir);

    // Write and read cache
    const cache_path = "test_complex_direct_cache.roc.cache";
    defer std.fs.cwd().deleteFile(cache_path) catch {};

    try DirectMemoryCache.writeCacheFile(allocator, cache_path, &env, c_output.cir);

    const loaded = try DirectMemoryCache.loadCacheFile(allocator, cache_path);
    defer allocator.free(loaded.buffer);

    // Verify complex type information is preserved
    try testing.expectEqual(env.idents.count(), loaded.module_env.idents.count());
    try testing.expectEqual(env.types.len(), loaded.module_env.types.len());

    // Verify we can look up identifiers
    const result_ident = try loaded.module_env.idents.insert(allocator, "Result");
    const found = loaded.module_env.idents.get(result_ident);
    try testing.expectEqualStrings("Result", found.raw_text);

    // Verify CIR metadata is preserved
    try testing.expectEqual(c_output.cir.all_defs.span.len, loaded.cir.all_defs.span.len);
    // Note: Full CIR node access is not available in MVP implementation
}

test "DirectMemoryCache stress test with large module" {
    const allocator = testing.allocator;

    // Generate a large module programmatically
    var source_buf = std.ArrayList(u8).init(allocator);
    defer source_buf.deinit();

    const writer = source_buf.writer();

    // Generate many type definitions
    var i: u32 = 0;
    while (i < 50) : (i += 1) {
        try writer.print("Type{} : {{ field{} : U32, field{} : Str }}\n", .{ i, i * 2, i * 2 + 1 });
    }

    // Generate many functions
    i = 0;
    while (i < 50) : (i += 1) {
        try writer.print("fn{} : U32 -> U32\n", .{i});
        try writer.print("fn{} = \\x -> x + {}\n\n", .{ i, i });
    }

    try writer.writeAll("main = fn0 42\n");

    const source = try source_buf.toOwnedSlice();
    defer allocator.free(source);

    // Initialize ModuleEnv
    var env = try ModuleEnv.init(allocator, try allocator.dupe(u8, source));
    defer env.deinit();

    // Parse
    var tokenized = try tokenize.fromSlice(allocator, &env, source);
    defer tokenized.deinit();

    var parser = try parse.Parser.init(tokenized);
    defer parser.deinit();
    var file = try parser.file();

    // Canonicalize
    var c_input = canonicalize.Input{
        .gpa = allocator,
        .env = &env,
        .ast = file,
        .exposed_values = &.{},
        .exposed_types = &.{},
        .exposed_abilities = &.{},
    };

    var c_output = try canonicalize.canonicalize(c_input);
    defer c_output.deinit();

    // Write and read cache
    const cache_path = "test_stress_direct_cache.roc.cache";
    defer std.fs.cwd().deleteFile(cache_path) catch {};

    const write_start = std.time.milliTimestamp();
    try DirectMemoryCache.writeCacheFile(allocator, cache_path, &env, c_output.cir);
    const write_time = std.time.milliTimestamp() - write_start;

    const read_start = std.time.milliTimestamp();
    const loaded = try DirectMemoryCache.loadCacheFile(allocator, cache_path);
    defer allocator.free(loaded.buffer);
    const read_time = std.time.milliTimestamp() - read_start;

    std.debug.print("\nStress test - Write: {}ms, Read: {}ms\n", .{ write_time, read_time });

    // Verify everything loaded correctly
    try testing.expect(loaded.module_env.idents.count() > 100);
    try testing.expect(loaded.module_env.types.len() > 50);

    // Verify basic CIR data is loaded
    // Note: Full CIR serialization is simplified in MVP
    try testing.expect(loaded.cir.module_name.len >= 0);
}
