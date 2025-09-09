const std = @import("std");
const testing = std.testing;
const base = @import("base");
const collections = @import("collections");
const AST = @import("../AST.zig");
const Parser = @import("../Parser.zig");
const tokens = @import("tokens");
const tokenize_iter = tokens.Tokenizer;

test "parse simple string literal" {
    const allocator = testing.allocator;

    const source = "\"hello world\"";

    var ast = try AST.initCapacity(allocator, 10);
    defer ast.deinit(allocator);

    const src_bytes_testing = try base.SrcBytes.Testing.initFromSlice(allocator, source);
    const src_bytes = src_bytes_testing.src;
    defer allocator.free(src_bytes.ptr[0..@intCast(src_bytes.len)]);

    var env = try base.CommonEnv.init(allocator, src_bytes);
    defer env.deinit(allocator);

    var messages: [128]tokenize_iter.Diagnostic = undefined;
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    // First verify tokenization captures the string
    var tokenizer = try tokenize_iter.init(allocator, src_bytes, &env.idents, messages[0..], &byte_slices);
    defer tokenizer.deinit(allocator);

    var found_string = false;
    while (true) {
        const token = try tokenizer.next();
        switch (token.tag) {
            .EndOfFile => break,
            .String => {
                const idx = token.payload.bytes_idx;
                const content = byte_slices.slice(idx);
                try testing.expectEqualStrings("hello world", content);
                found_string = true;
            },
            else => {},
        }
    }
    try testing.expect(found_string);

    // Now test parsing
    var parser = try Parser.init(&env, allocator, src_bytes, &ast, &byte_slices, &ast.parse_diagnostics);
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

    const src_bytes_testing = try base.SrcBytes.Testing.initFromSlice(allocator, source);
    const src_bytes = src_bytes_testing.src;
    defer allocator.free(src_bytes.ptr[0..@intCast(src_bytes.len)]);

    var env = try base.CommonEnv.init(allocator, src_bytes);
    defer env.deinit(allocator);

    var messages: [128]tokenize_iter.Diagnostic = undefined;
    var byte_slices = collections.ByteSlices{ .entries = .{} };
    defer byte_slices.entries.deinit(allocator);

    var parser = try Parser.init(&env, allocator, src_bytes, &ast, &byte_slices, &ast.parse_diagnostics);
    defer parser.deinit();

    const expr = try parser.parseExprFromSource(messages[0..]);
    // Should parse as a string literal
    try testing.expect(ast.tag(expr) == .str_literal_small or ast.tag(expr) == .str_literal_big);
}
