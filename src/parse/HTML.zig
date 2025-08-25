//! Generate HTML representation of the AST using a stack-based traversal.

const std = @import("std");
const base = @import("base");

const AST = @import("AST.zig");
const RegionInfo = base.RegionInfo;
const CommonEnv = base.CommonEnv;
const Token = @import("tokenize.zig").Token;

/// Generate an interactive source range span for the playground
fn writeSourceRangeSpan(writer: anytype, region: base.Region, source: []const u8, line_starts: []const u32) !void {
    const region_info = base.RegionInfo.position(source, line_starts, region.start.offset, region.end.offset) catch {
        try writer.print("<span class=\"source-range\" data-start-byte=\"{d}\" data-end-byte=\"{d}\">@{d}-{d}</span>", .{ region.start.offset, region.end.offset, region.start.offset, region.end.offset });
        return;
    };
    try writer.print("<span class=\"source-range\" data-start-byte=\"{d}\" data-end-byte=\"{d}\" data-start-line=\"{d}\" data-start-col=\"{d}\" data-end-line=\"{d}\" data-end-col=\"{d}\">@{d}.{d}-{d}.{d}</span>", .{ region.start.offset, region.end.offset, region_info.start_line_idx + 1, region_info.start_col_idx + 1, region_info.end_line_idx + 1, region_info.end_col_idx + 1, region_info.start_line_idx + 1, region_info.start_col_idx + 1, region_info.end_line_idx + 1, region_info.end_col_idx + 1 });
}

/// Generate an HTML representation of the tokens in the AST
pub fn tokensToHtml(ast: *const AST, env: *const CommonEnv, writer: anytype) !void {
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
            .OpPlus, .OpStar, .OpPizza, .OpAssign, .OpBinaryMinus, .OpUnaryMinus, .OpNotEquals, .OpBang, .OpAnd, .OpAmpersand, .OpQuestion, .OpDoubleQuestion, .OpOr, .OpBar, .OpDoubleSlash, .OpSlash, .OpCaret, .OpGreaterThanOrEq, .OpGreaterThan, .OpLessThanOrEq, .OpBackArrow, .OpLessThan, .OpEquals, .OpColonEqual, .NoSpaceOpQuestion => "token-punctuation",
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
