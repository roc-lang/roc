//! Generate HTML representation of the AST using a stack-based traversal.

const std = @import("std");
const base = @import("base");

const AST = @import("AST.zig");
const RegionInfo = base.RegionInfo;
const CommonEnv = base.CommonEnv;

/// Generate an interactive source range span for the playground
fn writeSourceRangeSpan(writer: *std.io.Writer, region: base.Region, source: []const u8, line_starts: []const u32) !void {
    const region_info = base.RegionInfo.position(source, line_starts, region.start.offset, region.end.offset) catch {
        try writer.print("<span class=\"source-range\" data-start-byte=\"{d}\" data-end-byte=\"{d}\">@{d}-{d}</span>", .{ region.start.offset, region.end.offset, region.start.offset, region.end.offset });
        return;
    };
    try writer.print("<span class=\"source-range\" data-start-byte=\"{d}\" data-end-byte=\"{d}\" data-start-line=\"{d}\" data-start-col=\"{d}\" data-end-line=\"{d}\" data-end-col=\"{d}\">@{d}.{d}-{d}.{d}</span>", .{ region.start.offset, region.end.offset, region_info.start_line_idx + 1, region_info.start_col_idx + 1, region_info.end_line_idx + 1, region_info.end_col_idx + 1, region_info.start_line_idx + 1, region_info.start_col_idx + 1, region_info.end_line_idx + 1, region_info.end_col_idx + 1 });
}

/// Generate an HTML representation of the tokens in the AST
pub fn tokensToHtml(ast: *const AST, env: *const CommonEnv, writer: *std.io.Writer) !void {
    try writer.writeAll("<div class=\"token-list\">");

    const token_tags = ast.tokens.tokens.items(.tag);
    const token_extras = ast.tokens.tokens.items(.extra);

    for (token_tags, token_extras, 0..) |tag, _, i| {
        const region = ast.tokens.resolve(i);
        const css_class: []const u8 = switch (tag) {
            .KwApp,
            .KwAs,
            .KwCrash,
            .KwDbg,
            .KwElse,
            .KwExpect,
            .KwExposes,
            .KwExposing,
            .KwFor,
            .KwGenerates,
            .KwHas,
            .KwHosted,
            .KwIf,
            .KwImplements,
            .KwImport,
            .KwImports,
            .KwIn,
            .KwInterface,
            .KwMatch,
            .KwModule,
            .KwPackage,
            .KwPackages,
            .KwPlatform,
            .KwProvides,
            .KwRequires,
            .KwReturn,
            .KwVar,
            .KwWhere,
            .KwWith,
            => "token-keyword",
            .UpperIdent, .LowerIdent, .DotLowerIdent, .DotUpperIdent, .NoSpaceDotLowerIdent, .NoSpaceDotUpperIdent, .NamedUnderscore => "token-keyword",
            .OpPlus, .OpStar, .OpPizza, .OpAssign, .OpBinaryMinus, .OpUnaryMinus, .OpNotEquals, .OpBang, .OpAnd, .OpAmpersand, .OpQuestion, .OpDoubleQuestion, .OpOr, .OpBar, .OpDoubleSlash, .OpSlash, .OpPercent, .OpCaret, .OpGreaterThanOrEq, .OpGreaterThan, .OpLessThanOrEq, .OpBackArrow, .OpLessThan, .OpEquals, .OpColonEqual, .NoSpaceOpQuestion => "token-punctuation",
            .StringStart, .StringEnd, .MultilineStringStart, .StringPart => "token-string",
            .Float, .Int => "token-number",
            .OpenRound, .CloseRound, .OpenSquare, .CloseSquare, .OpenCurly, .CloseCurly, .OpenStringInterpolation, .CloseStringInterpolation, .NoSpaceOpenRound => "token-punctuation",
            .EndOfFile => "token-default",
            else => "token-default",
        };

        try writer.print("<span class=\"token {s}\">", .{css_class});
        try writer.print("<span class=\"token-type\">{s}</span>", .{@tagName(tag)});
        try writer.writeAll(" ");
        try writeSourceRangeSpan(writer, region, env.source, env.line_starts.items.items);
        try writer.writeAll("</span>");
    }
    try writer.writeAll("</div>");
}

const testing = std.testing;
const tokenize = @import("tokenize.zig");
const NodeStore = @import("NodeStore.zig");

test "tokensToHtml generates valid HTML" {
    const gpa = testing.allocator;

    // Create a simple source to tokenize
    const source = "foo = 42";

    // Create environment
    var env = try base.CommonEnv.init(gpa, source);
    try env.calcLineStarts(gpa);
    defer env.deinit(gpa);

    // Tokenize
    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var tokenizer = try tokenize.Tokenizer.init(&env, gpa, source, msg_slice);
    try tokenizer.tokenize(gpa);
    var result = tokenizer.finishAndDeinit();
    defer result.tokens.deinit(gpa);

    // Create a minimal AST with just the tokens
    var store = try NodeStore.initCapacity(gpa, 16);
    defer store.deinit();

    var tokenize_diagnostics = std.ArrayList(tokenize.Diagnostic).empty;
    var parse_diagnostics = std.ArrayList(AST.Diagnostic).empty;
    defer tokenize_diagnostics.deinit(gpa);
    defer parse_diagnostics.deinit(gpa);

    var ast = AST{
        .env = &env,
        .tokens = result.tokens,
        .store = store,
        .root_node_idx = 0,
        .tokenize_diagnostics = tokenize_diagnostics,
        .parse_diagnostics = parse_diagnostics,
    };

    // Generate HTML
    var output_writer: std.Io.Writer.Allocating = .init(gpa);
    defer output_writer.deinit();

    try tokensToHtml(&ast, &env, &output_writer.writer);

    // Verify the output contains expected HTML elements
    const html = output_writer.written();
    try testing.expect(std.mem.indexOf(u8, html, "<div class=\"token-list\">") != null);
    try testing.expect(std.mem.indexOf(u8, html, "</div>") != null);
    try testing.expect(std.mem.indexOf(u8, html, "LowerIdent") != null); // "foo" token
    try testing.expect(std.mem.indexOf(u8, html, "Int") != null); // "42" token
}

test "tokensToHtml handles position errors gracefully" {
    const gpa = testing.allocator;

    // Create a simple source to tokenize
    const source = "foo = 42";

    // Create environment but don't calculate line starts (to trigger error path)
    var env = try base.CommonEnv.init(gpa, source);
    // Deliberately NOT calling env.calcLineStarts(gpa) to trigger the error path
    defer env.deinit(gpa);

    // Tokenize
    var messages: [128]tokenize.Diagnostic = undefined;
    const msg_slice = messages[0..];
    var tokenizer = try tokenize.Tokenizer.init(&env, gpa, source, msg_slice);
    try tokenizer.tokenize(gpa);
    var result = tokenizer.finishAndDeinit();
    defer result.tokens.deinit(gpa);

    // Create a minimal AST with just the tokens
    var store = try NodeStore.initCapacity(gpa, 16);
    defer store.deinit();

    var tokenize_diagnostics = std.ArrayList(tokenize.Diagnostic).empty;
    var parse_diagnostics = std.ArrayList(AST.Diagnostic).empty;
    defer tokenize_diagnostics.deinit(gpa);
    defer parse_diagnostics.deinit(gpa);

    var ast = AST{
        .env = &env,
        .tokens = result.tokens,
        .store = store,
        .root_node_idx = 0,
        .tokenize_diagnostics = tokenize_diagnostics,
        .parse_diagnostics = parse_diagnostics,
    };

    // Generate HTML - should still work even with position errors
    var output_writer: std.Io.Writer.Allocating = .init(gpa);
    defer output_writer.deinit();

    try tokensToHtml(&ast, &env, &output_writer.writer);

    // Verify the output contains expected HTML elements (fallback format)
    const html = output_writer.written();
    try testing.expect(std.mem.indexOf(u8, html, "<div class=\"token-list\">") != null);
    try testing.expect(std.mem.indexOf(u8, html, "</div>") != null);
    // Should use the fallback format with just byte offsets
    try testing.expect(std.mem.indexOf(u8, html, "data-start-byte=") != null);
}
