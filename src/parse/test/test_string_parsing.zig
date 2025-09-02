const std = @import("std");
const testing = std.testing;
const base = @import("base");
const collections = @import("collections");
const AST = @import("../AST.zig");
const Parser = @import("../Parser.zig");
const tokenize_iter = @import("../tokenize.zig");

test "parse simple string literal" {
    const allocator = testing.allocator;

    const source = "\"hello world\"";

    var ast = try AST.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var messages: [128]tokenize_iter.Diagnostic = undefined;
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // First verify tokenization captures the string
    var tokenizer = try tokenize_iter.TokenIterator.init(&env, allocator, source, messages[0..], &byte_slices);
    defer tokenizer.deinit(allocator);

    var found_string = false;
    while (try tokenizer.next(allocator)) |token| {
        if (token.tag == .String) {
            switch (token.extra) {
                .bytes_idx => |idx| {
                    const content = byte_slices.slice(idx);
                    // std.debug.print("\nTokenized string: '{s}'\n", .{content});
                    try testing.expectEqualStrings("hello world", content);
                    found_string = true;
                },
                else => {},
            }
        }
        if (token.tag == .EndOfFile) break;
    }
    try testing.expect(found_string);

    // Now test parsing
    var parser = try Parser.init(&env, allocator, source, messages[0..], &ast, &byte_slices);
    defer parser.deinit();

    const expr = try parser.parseExprFromSource(messages[0..]);
    // Should parse as a string literal
    try testing.expect(ast.tag(expr) == .str_literal_small or ast.tag(expr) == .str_literal_big);
}

test "parse string with escapes" {
    const allocator = testing.allocator;

    const source = "\"hello\\nworld\"";

    var ast = try AST.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    var env = try base.CommonEnv.init(allocator, source);
    defer env.deinit(allocator);

    var messages: [128]tokenize_iter.Diagnostic = undefined;
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var parser = try Parser.init(&env, allocator, source, messages[0..], &ast, &byte_slices);
    defer parser.deinit();

    const expr = try parser.parseExprFromSource(messages[0..]);
    // Should parse as a string literal
    try testing.expect(ast.tag(expr) == .str_literal_small or ast.tag(expr) == .str_literal_big);
}
