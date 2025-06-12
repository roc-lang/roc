//!
//! This file implements the Intermediate Representation (IR) for Roc's parser.
//!
//! The IR provides a structured, tree-based representation of Roc source code after parsing
//!
//! The design uses an arena-based memory allocation strategy with a "multi-list" approach where nodes
//! are stored in a flat list but cross-referenced via indices rather than pointers. This improves
//! memory locality and efficiency.
//!
//! The implementation includes comprehensive facilities for building, manipulating, and traversing
//! the IR, as well as converting it to S-expressions for debugging and visualization.

const std = @import("std");
const base = @import("../../base.zig");
const sexpr = @import("../../base/sexpr.zig");
const tokenize = @import("tokenize.zig");
const collections = @import("../../collections.zig");

const Node = @import("Node.zig");
const NodeStore = @import("NodeStore.zig");
const TokenIdx = tokenize.Token.Idx;
const TokenizedBuffer = tokenize.TokenizedBuffer;
const exitOnOom = @import("../../collections/utils.zig").exitOnOom;

const testing = std.testing;
const Ident = base.Ident;
const Allocator = std.mem.Allocator;

const IR = @This();

source: []const u8,
tokens: TokenizedBuffer,
store: NodeStore,
errors: []const Diagnostic,
root_node_idx: u32 = 0,

/// Calculate whether this region is - or will be - multiline
pub fn regionIsMultiline(self: *IR, region: Region) bool {
    var i = region.start;
    const tags = self.tokens.tokens.items(.tag);
    while (i <= region.end) {
        if (tags[i] == .Newline) {
            return true;
        }
        if (tags[i] == .Comma and (tags[i + 1] == .CloseSquare or
            tags[i + 1] == .CloseRound or
            tags[i + 1] == .CloseCurly))
        {
            return true;
        }
        i += 1;
    }
    return false;
}

/// Returns diagnostic position information for the given region.
pub fn regionInfo(self: *IR, region: Region, line_starts: std.ArrayList(u32)) base.DiagnosticPosition {
    const start = self.tokens.resolve(region.start);
    const end = self.tokens.resolve(region.end);
    const info = base.DiagnosticPosition.position(self.source, line_starts, start.start.offset, end.end.offset) catch {
        // std.debug.panic("failed to calculate position info for region {?}, start: {}, end: {}", .{ region, start, end });
        return .{
            .start_line_idx = 0,
            .start_col_idx = 0,
            .end_line_idx = 0,
            .end_col_idx = 0,
            .line_text = "",
        };
    };

    return info;
}

pub fn deinit(self: *IR) void {
    defer self.tokens.deinit();
    defer self.store.deinit();
    self.store.gpa.free(self.errors);
}

/// Diagnostics related to parsing
pub const Diagnostic = struct {
    tag: Tag,
    region: Region,

    /// different types of diagnostic errors
    pub const Tag = enum {
        bad_indent,
        multiple_platforms,
        no_platform,
        missing_header,
        list_not_closed,
        missing_arrow,
        expected_exposes,
        expected_exposes_close_square,
        expected_exposes_open_square,
        expected_imports,
        expected_imports_close_curly,
        expected_imports_open_curly,
        expected_package_or_platform_name,
        expected_package_or_platform_colon,
        expected_package_or_platform_string,
        expected_package_platform_close_curly,
        expected_package_platform_open_curly,
        expected_packages,
        expected_packages_close_curly,
        expected_packages_open_curly,
        expected_platform_name_end,
        expected_platform_name_start,
        expected_platform_name_string,
        expected_platform_string,
        expected_provides,
        expected_provides_close_square,
        expected_provides_open_square,
        expected_requires,
        expected_requires_rigids_close_curly,
        expected_requires_rigids_open_curly,
        expected_requires_signatures_close_curly,
        expected_requires_signatures_open_curly,
        expect_closing_paren,
        header_expected_open_square,
        header_expected_close_square,
        header_unexpected_token,
        pattern_unexpected_token,
        pattern_unexpected_eof,
        ty_anno_unexpected_token,
        statement_unexpected_eof,
        statement_unexpected_token,
        string_unexpected_token,
        string_expected_close_interpolation,
        expr_if_missing_else,
        expr_no_space_dot_int,
        import_exposing_no_open,
        import_exposing_no_close,
        no_else,
        expected_type_field_name,
        expected_colon_after_type_field_name,
        expected_arrow,
        expected_ty_close_curly_or_comma,
        expected_ty_close_square_or_comma,
        expected_lower_name_after_exposed_item_as,
        expected_upper_name_after_exposed_item_as,
        exposed_item_unexpected_token,
        expected_upper_name_after_import_as,
        expected_colon_after_type_annotation,
        expected_lower_ident_pat_field_name,
        expected_colon_after_pat_field_name,
        expected_expr_bar,
        expected_expr_close_curly_or_comma,
        expected_expr_close_round_or_comma,
        expected_expr_close_square_or_comma,
        expected_close_curly_at_end_of_match,
        expected_open_curly_after_match,
        expr_unexpected_token,
        expected_expr_record_field_name,
        expected_ty_apply_close_round,
        expected_ty_anno_end_of_function,
        expected_ty_anno_end,
        expected_expr_apply_close_round,
        where_expected_where,
        where_expected_mod_open,
        where_expected_var,
        where_expected_mod_close,
        where_expected_arg_open,
        where_expected_arg_close,
        where_expected_method_arrow,
        where_expected_method_or_alias_name,
        where_expected_var_or_module,
        import_must_be_top_level,
        invalid_type_arg,
        expr_arrow_expects_ident,
        var_only_allowed_in_a_body,
        var_must_have_ident,
        var_expected_equals,
        for_expected_in,
    };
};

/// The first and last token consumed by a Node
pub const Region = struct {
    start: TokenIdx,
    end: TokenIdx,

    pub fn spanAcross(self: Region, other: Region) Region {
        return .{ .start = self.start, .end = other.end };
    }

    pub fn toBase(self: Region) base.Region {
        return .{ .start = base.Region.Position{ .offset = self.start }, .end = base.Region.Position{ .offset = self.end } };
    }
};

/// Resolve a token index to a string slice from the source code.
pub fn resolve(self: *IR, token: TokenIdx) []const u8 {
    const range = self.tokens.resolve(token);
    return self.source[@intCast(range.start.offset)..@intCast(range.end.offset)];
}

/// Contains properties of the thing to the right of the `import` keyword.
pub const ImportRhs = packed struct {
    /// e.g. 1 in case we use import `as`: `import Module as Mod`
    aliased: u1,
    /// 1 in case the import is qualified, e.g. `pf` in `import pf.Stdout ...`
    qualified: u1,
    /// The number of things in the exposes list. e.g. 3 in `import SomeModule exposing [a1, a2, a3]`
    num_exposes: u30,
};

// Check that all packed structs are 4 bytes size as they as cast to
// and from a u32
comptime {
    std.debug.assert(@sizeOf(NodeStore.Header.AppHeaderRhs) == 4);
    std.debug.assert(@sizeOf(ImportRhs) == 4);
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}

/// Helper function to convert an IR to a string in S-expression format
/// and write it to the given writer.
pub fn toSExprStr(ir: *@This(), env: *base.ModuleEnv, writer: std.io.AnyWriter) !void {
    const file = ir.store.getFile();

    // calculate the offsets of line_starts once and save in the IR
    // for use in each toSExpr function
    var line_starts = try base.DiagnosticPosition.findLineStarts(env.gpa, ir.source);
    defer line_starts.deinit();

    var node = file.toSExpr(env, ir, line_starts);
    defer node.deinit(env.gpa);

    node.toStringPretty(writer);
}

/// Helper function to convert a specific IR node (i.e., not a file) to a string in S-expression format
/// and write it to the given writer
pub fn nodeToSExprStr(ir: *@This(), ir_node: anytype, env: *base.ModuleEnv, writer: std.io.AnyWriter) !void {
    // calculate the offsets of line_starts once and save in the IR
    // for use in each toSExpr function
    var line_starts = try base.DiagnosticPosition.findLineStarts(env.gpa, ir.source);
    defer line_starts.deinit();

    var node = ir_node.toSExpr(env, ir, line_starts);
    defer node.deinit(env.gpa);

    node.toStringPretty(writer);
}
