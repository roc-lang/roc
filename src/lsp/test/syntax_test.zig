//! Tests for the LSP syntax checker integration.

const std = @import("std");
const SyntaxChecker = @import("../syntax.zig").SyntaxChecker;
const uri_util = @import("../uri.zig");

fn platformPath(allocator: std.mem.Allocator) ![]u8 {
    const this_dir = std.fs.path.dirname(@src().file) orelse ".";
    const abs_dir = try std.fs.path.resolve(allocator, &.{this_dir});
    defer allocator.free(abs_dir);
    return std.fs.path.resolve(allocator, &.{ abs_dir, "..", "..", "..", "test", "str", "platform", "main.roc" });
}

test "syntax checker skips rebuild when content unchanged" {
    // Test the incremental invalidation: same content should skip rebuild
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const contents = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main = "hello"
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(contents);

    try tmp.dir.writeFile(.{ .sub_path = "test.roc", .data = contents });
    const file_path = try tmp.dir.realpathAlloc(allocator, "test.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    // First check with override text - should do full build
    const result1 = try checker.check(uri, contents, null);
    defer {
        for (result1) |*set| set.deinit(allocator);
        allocator.free(result1);
    }

    // Verify hash is stored after first build
    const hash_after_first = checker.dependency_graph.getContentHash(file_path);
    try std.testing.expect(hash_after_first != null);

    // Second check with same content - should skip rebuild (returns empty)
    const result2 = try checker.check(uri, contents, null);
    defer {
        for (result2) |*set| set.deinit(allocator);
        allocator.free(result2);
    }

    // When content is unchanged, check returns empty (skip rebuild)
    try std.testing.expectEqual(@as(usize, 0), result2.len);

    // Hash should still be the same
    const hash_after_second = checker.dependency_graph.getContentHash(file_path);
    try std.testing.expect(hash_after_second != null);
    try std.testing.expectEqualSlices(u8, &hash_after_first.?, &hash_after_second.?);
}

test "syntax checker rebuilds when content changes" {
    // Test that changed content triggers rebuild
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const contents1 = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main = "hello"
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(contents1);

    const contents2 = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main = "world"
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(contents2);

    try tmp.dir.writeFile(.{ .sub_path = "test2.roc", .data = contents1 });
    const file_path = try tmp.dir.realpathAlloc(allocator, "test2.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    // First check
    const result1 = try checker.check(uri, contents1, null);
    defer {
        for (result1) |*set| set.deinit(allocator);
        allocator.free(result1);
    }

    const hash_after_first = checker.dependency_graph.getContentHash(file_path);
    try std.testing.expect(hash_after_first != null);

    // Second check with different content - should trigger rebuild
    const result2 = try checker.check(uri, contents2, null);
    defer {
        for (result2) |*set| set.deinit(allocator);
        allocator.free(result2);
    }

    // Hash should be different
    const hash_after_second = checker.dependency_graph.getContentHash(file_path);
    try std.testing.expect(hash_after_second != null);
    try std.testing.expect(!std.mem.eql(u8, &hash_after_first.?, &hash_after_second.?));
}

test "syntax checker reports diagnostics for invalid source" {
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const contents = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\main =
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(contents);

    try tmp.dir.writeFile(.{ .sub_path = "bad.roc", .data = contents });
    const file_path = try tmp.dir.realpathAlloc(allocator, "bad.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    const publish_sets = try checker.check(uri, null, null);
    defer {
        for (publish_sets) |*set| set.deinit(allocator);
        allocator.free(publish_sets);
    }

    try std.testing.expect(publish_sets.len > 0);
    var total_diags: usize = 0;
    for (publish_sets) |set| {
        total_diags += set.diagnostics.len;
    }
    try std.testing.expect(total_diags > 0);
}

test "completion context detects after_record_dot for lowercase identifier" {
    // Test that detectCompletionContext correctly identifies record/method access
    const completion_context = @import("../completion/context.zig");

    // Test case: "my_var." should be detected as after_record_dot
    const source = "main = my_var.";
    const context = completion_context.detectCompletionContext(source, 0, 14);

    switch (context) {
        .after_record_dot => |access| {
            try std.testing.expectEqualStrings("my_var", access.variable_name);
        },
        else => try std.testing.expect(false), // Should be after_record_dot
    }
}

test "completion context detects after_module_dot for uppercase identifier" {
    // Test that detectCompletionContext correctly identifies module access
    const completion_context = @import("../completion/context.zig");

    // Test case: "Str." should be detected as after_module_dot
    const source = "main = Str.";
    const context = completion_context.detectCompletionContext(source, 0, 11);

    switch (context) {
        .after_module_dot => |module_name| {
            try std.testing.expectEqualStrings("Str", module_name);
        },
        else => try std.testing.expect(false), // Should be after_module_dot
    }
}

test "completion context detects expression context" {
    // Test that detectCompletionContext correctly identifies general expression context
    const completion_context = @import("../completion/context.zig");

    // Test case: "main = " should be detected as expression
    const source = "main = ";
    const context = completion_context.detectCompletionContext(source, 0, 7);

    switch (context) {
        .expression => {}, // Expected
        else => try std.testing.expect(false), // Should be expression
    }
}

test "completion context detects after_colon for type annotation" {
    // Test that detectCompletionContext correctly identifies type annotation context
    const completion_context = @import("../completion/context.zig");

    // Test case: "foo : " should be detected as after_colon
    const source = "foo : ";
    const context = completion_context.detectCompletionContext(source, 0, 6);

    switch (context) {
        .after_colon => {}, // Expected
        else => try std.testing.expect(false), // Should be after_colon
    }
}

test "getCompletionsAtPosition returns basic completions" {
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    // Create a simple app with a definition
    const contents = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\my_value = "hello"
        \\
        \\main = my_
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(contents);

    try tmp.dir.writeFile(.{ .sub_path = "completion.roc", .data = contents });
    const file_path = try tmp.dir.realpathAlloc(allocator, "completion.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    // Request completion at the position after "main = my_"
    // Line 4 (0-indexed), character 10
    const result = try checker.getCompletionsAtPosition(uri, contents, 4, 10);

    if (result) |completion_result| {
        defer {
            for (completion_result.items) |item| {
                if (item.detail) |d| allocator.free(d);
            }
            allocator.free(completion_result.items);
        }

        // Should have at least some completions (builtin modules and local defs)
        try std.testing.expect(completion_result.items.len > 0);
    }
}

test "record field completion works for modules" {
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    // First create a clean, valid module file (simpler than app for testing)
    // Explicitly annotate the record type to ensure all fields are known
    const clean_contents =
        \\module []
        \\
        \\my_record : { foo : Str, bar : I64 }
        \\my_record = { foo: "hello", bar: 42 }
        \\
        \\get_foo = my_record.foo
        \\get_bar = my_record.bar
        \\
    ;

    try tmp.dir.writeFile(.{ .sub_path = "record_completion.roc", .data = clean_contents });
    const file_path = try tmp.dir.realpathAlloc(allocator, "record_completion.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    std.debug.print("\n=== TEST: record field completion ===\n", .{});

    // First do a check with clean code to create snapshot
    const publish_sets = try checker.check(uri, clean_contents, null);
    defer {
        for (publish_sets) |*set| set.deinit(allocator);
        allocator.free(publish_sets);
    }

    // Now test completion with incomplete code (cursor after dot)
    const incomplete_contents =
        \\module []
        \\
        \\my_record = { foo: "hello", bar: 42 }
        \\
        \\get_foo = my_record.
        \\
    ;

    std.debug.print("Contents:\n{s}\n", .{incomplete_contents});

    // Request completion after "my_record."
    // Line 4: "get_foo = my_record."
    // Character positions: g(0) e(1) t(2) _(3) f(4) o(5) o(6) ' '(7) =(8) ' '(9) m(10) y(11) _(12) r(13) e(14) c(15) o(16) r(17) d(18) .(19)
    // So character 20 is right after the dot
    std.debug.print("Requesting completion at line 4, char 20\n", .{});
    const result = try checker.getCompletionsAtPosition(uri, incomplete_contents, 4, 20);

    if (result) |completion_result| {
        defer {
            for (completion_result.items) |item| {
                if (item.detail) |d| allocator.free(d);
            }
            allocator.free(completion_result.items);
        }

        std.debug.print("Got {d} completion items:\n", .{completion_result.items.len});
        for (completion_result.items) |item| {
            std.debug.print("  - {s} (kind={?})\n", .{ item.label, item.kind });
        }

        // Should have record field completions (foo, bar)
        var found_foo = false;
        var found_bar = false;
        for (completion_result.items) |item| {
            if (std.mem.eql(u8, item.label, "foo")) found_foo = true;
            if (std.mem.eql(u8, item.label, "bar")) found_bar = true;
        }

        std.debug.print("found_foo={}, found_bar={}\n", .{ found_foo, found_bar });

        // These should be true for record field completion to work
        try std.testing.expect(found_foo);
        try std.testing.expect(found_bar);
    } else {
        std.debug.print("Got null result\n", .{});
        try std.testing.expect(false); // Should have got a result
    }
}

test "record completion uses snapshot env when builds fail" {
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    const clean_contents = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\my_record = {{ foo: "hello", bar: 42 }}
        \\
        \\main = my_record.foo
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(clean_contents);

    try tmp.dir.writeFile(.{ .sub_path = "record_completion_snapshot.roc", .data = clean_contents });
    const file_path = try tmp.dir.realpathAlloc(allocator, "record_completion_snapshot.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    const publish_sets = try checker.check(uri, null, null);
    defer {
        for (publish_sets) |*set| set.deinit(allocator);
        allocator.free(publish_sets);
    }

    const error_contents = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\my_record = {{ foo: "hello", bar: 42 }}
        \\
        \\main = my_record.
        \\broken =
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(error_contents);

    const result = try checker.getCompletionsAtPosition(uri, error_contents, 4, 17);

    if (result) |completion_result| {
        defer {
            for (completion_result.items) |item| {
                if (item.detail) |d| allocator.free(d);
            }
            allocator.free(completion_result.items);
        }

        var found_foo = false;
        var found_bar = false;
        for (completion_result.items) |item| {
            if (std.mem.eql(u8, item.label, "foo")) found_foo = true;
            if (std.mem.eql(u8, item.label, "bar")) found_bar = true;
        }

        try std.testing.expect(found_foo);
        try std.testing.expect(found_bar);
    } else {
        try std.testing.expect(false);
    }
}

test "record field completion with partial field name" {
    // Tests that completion works when typing a partial field name: "my_record.fo|"
    // This is the common case when user is typing - should show all available fields
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    // First build with valid code to populate the snapshot env
    const clean_contents =
        \\module []
        \\
        \\my_record : { foo : Str, bar : I64 }
        \\my_record = { foo: "hello", bar: 42 }
        \\
        \\get_foo = my_record.foo
        \\get_bar = my_record.bar
        \\
    ;

    try tmp.dir.writeFile(.{ .sub_path = "partial_field.roc", .data = clean_contents });
    const file_path = try tmp.dir.realpathAlloc(allocator, "partial_field.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    // First check to build and cache env
    const publish_sets = try checker.check(uri, clean_contents, null);
    defer {
        for (publish_sets) |*set| set.deinit(allocator);
        allocator.free(publish_sets);
    }

    // Now request completion with partial field name "fo"
    // Line 4: "get_foo = my_record.fo"
    const partial_contents =
        \\module []
        \\
        \\my_record = { foo: "hello", bar: 42 }
        \\
        \\get_foo = my_record.fo
        \\
    ;

    const result = try checker.getCompletionsAtPosition(uri, partial_contents, 4, 22);

    if (result) |completion_result| {
        defer {
            for (completion_result.items) |item| {
                if (item.detail) |d| allocator.free(d);
            }
            allocator.free(completion_result.items);
        }

        std.debug.print("\n=== partial field completion: got {d} items ===\n", .{completion_result.items.len});
        for (completion_result.items) |item| {
            std.debug.print("  - {s}\n", .{item.label});
        }

        // Should have record field completions - at least foo and bar
        // Even with partial "fo", we should get all field options
        var found_foo = false;
        var found_bar = false;
        for (completion_result.items) |item| {
            if (std.mem.eql(u8, item.label, "foo")) found_foo = true;
            if (std.mem.eql(u8, item.label, "bar")) found_bar = true;
        }

        // Both should be found - filtering is done client-side
        try std.testing.expect(found_foo);
        try std.testing.expect(found_bar);
    } else {
        std.debug.print("\n=== partial field completion: got null result ===\n", .{});
        try std.testing.expect(false); // Should have got a result
    }
}

test "static dispatch completion for nominal type methods" {
    // Tests that static dispatch completion works when adding incomplete code to a working file
    // This simulates: user has working file, starts typing "new_result = val." and requests completion
    const allocator = std.testing.allocator;
    var checker = SyntaxChecker.init(allocator, .{}, null);
    defer checker.deinit();

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const platform_path = try platformPath(allocator);
    defer allocator.free(platform_path);

    // First create clean, working code with complete definition
    const clean_contents = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\Basic := [Val(Str)].{{
        \\  to_str : Basic -> Str
        \\  to_str = |Basic.Val(s)| s
        \\}}
        \\
        \\val : Basic
        \\val = Basic.Val("hello")
        \\
        \\main = val.to_str()
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(clean_contents);

    try tmp.dir.writeFile(.{ .sub_path = "static_dispatch.roc", .data = clean_contents });
    const file_path = try tmp.dir.realpathAlloc(allocator, "static_dispatch.roc");
    defer allocator.free(file_path);

    const uri = try uri_util.pathToUri(allocator, file_path);
    defer allocator.free(uri);

    std.debug.print("\n=== TEST: static dispatch completion ===\n", .{});
    std.debug.print("Clean contents:\n{s}\n", .{clean_contents});

    // First check to build and cache env - this should succeed with no errors
    const publish_sets = try checker.check(uri, clean_contents, null);
    defer {
        for (publish_sets) |*set| set.deinit(allocator);
        allocator.free(publish_sets);
    }

    // Check if snapshot was created
    std.debug.print("After check: snapshot_envs.count = {d}\n", .{checker.snapshot_envs.count()});

    // Now test completion with ADDED incomplete code (cursor after dot)
    // User has added a new line "result = val." and is requesting completion
    const incomplete_contents = try std.fmt.allocPrint(
        allocator,
        \\app [main] {{ pf: platform "{s}" }}
        \\
        \\Basic := [Val(Str)].{{
        \\  to_str : Basic -> Str
        \\  to_str = |Basic.Val(s)| s
        \\}}
        \\
        \\val : Basic
        \\val = Basic.Val("hello")
        \\
        \\main = val.to_str()
        \\
        \\result = val.
        \\
    ,
        .{platform_path},
    );
    defer allocator.free(incomplete_contents);

    std.debug.print("Incomplete contents:\n{s}\n", .{incomplete_contents});

    // Request completion after "val."
    // Line 12: "result = val."
    // Character 13 is right after the dot
    const result = try checker.getCompletionsAtPosition(uri, incomplete_contents, 12, 13);

    if (result) |completion_result| {
        defer {
            for (completion_result.items) |item| {
                if (item.detail) |d| allocator.free(d);
            }
            allocator.free(completion_result.items);
        }

        std.debug.print("Got {d} completion items:\n", .{completion_result.items.len});
        for (completion_result.items) |item| {
            std.debug.print("  - {s} (kind={?})\n", .{ item.label, item.kind });
        }

        // Should have the to_str method
        var found_to_str = false;
        for (completion_result.items) |item| {
            if (std.mem.eql(u8, item.label, "to_str")) found_to_str = true;
        }

        std.debug.print("found_to_str={}\n", .{found_to_str});
        try std.testing.expect(found_to_str);
    } else {
        std.debug.print("Got null result\n", .{});
        try std.testing.expect(false); // Should have got a result
    }
}
