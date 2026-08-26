//! Handler for LSP `textDocument/selectionRange` requests.
//!
//! Provides semantic selection expansion for the editor.
//! Walks the AST to find all containing regions for proper expand/shrink selection.

const std = @import("std");
const Allocator = std.mem.Allocator;
const protocol = @import("../protocol.zig");
const parse = @import("parse");
const can = @import("can");
const pos = @import("../position.zig");
const AST = parse.AST;
const TokenizedRegion = AST.TokenizedRegion;

/// Handler for `textDocument/selectionRange` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) (Allocator.Error || error{WriteFailed})!void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "selectionRange requires params");
                return;
            };

            if (std.meta.activeTag(params) != .object) {
                try self.sendError(id, .invalid_params, "selectionRange params must be an object");
                return;
            }
            const obj = params.object;

            // Extract textDocument.uri
            const text_doc_value = obj.get("textDocument") orelse {
                try self.sendError(id, .invalid_params, "missing textDocument");
                return;
            };
            if (std.meta.activeTag(text_doc_value) != .object) {
                try self.sendError(id, .invalid_params, "textDocument must be an object");
                return;
            }
            const text_doc = text_doc_value.object;
            const uri_value = text_doc.get("uri") orelse {
                try self.sendError(id, .invalid_params, "missing uri");
                return;
            };
            if (std.meta.activeTag(uri_value) != .string) {
                try self.sendError(id, .invalid_params, "uri must be a string");
                return;
            }
            const uri = uri_value.string;

            // Extract positions array
            const positions_value = obj.get("positions") orelse {
                try self.sendError(id, .invalid_params, "missing positions");
                return;
            };
            if (std.meta.activeTag(positions_value) != .array) {
                try self.sendError(id, .invalid_params, "positions must be an array");
                return;
            }
            const positions = positions_value.array;

            // Get the document text from the store
            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else {
                try self.sendResponse(id, &[_]?SelectionRange{});
                return;
            };

            // Process each position
            var results: std.ArrayList(SelectionRange) = .empty;
            defer {
                // Free the linked list nodes
                for (results.items) |range| {
                    freeSelectionRange(self.allocator, range);
                }
                results.deinit(self.allocator);
            }

            for (positions.items) |pos_value| {
                if (std.meta.activeTag(pos_value) != .object) {
                    try self.sendError(id, .invalid_params, "position must be an object");
                    return;
                }
                const pos_obj = pos_value.object;

                const line_value = pos_obj.get("line") orelse {
                    try self.sendError(id, .invalid_params, "missing line");
                    return;
                };
                if (std.meta.activeTag(line_value) != .integer) {
                    try self.sendError(id, .invalid_params, "line must be an integer");
                    return;
                }
                const line = std.math.cast(u32, line_value.integer) orelse {
                    try self.sendError(id, .invalid_params, "line must be a non-negative integer");
                    return;
                };

                const character_value = pos_obj.get("character") orelse {
                    try self.sendError(id, .invalid_params, "missing character");
                    return;
                };
                if (std.meta.activeTag(character_value) != .integer) {
                    try self.sendError(id, .invalid_params, "character must be an integer");
                    return;
                }
                const character = std.math.cast(u32, character_value.integer) orelse {
                    try self.sendError(id, .invalid_params, "character must be a non-negative integer");
                    return;
                };

                const selection_range = computeSelectionRange(self.allocator, text, line, character) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    error.InvalidPosition,
                    error.NoRangeFound,
                    error.ParseFailed,
                    => SelectionRange{
                        .range = .{
                            .start = .{ .line = line, .character = character },
                            .end = .{ .line = line, .character = character },
                        },
                        .parent = null,
                    },
                };
                try results.append(self.allocator, selection_range);
            }

            try self.sendResponse(id, results.items);
        }
    };
}

const Position = struct {
    line: u32,
    character: u32,
};

const Range = struct {
    start: Position,
    end: Position,
};

const SelectionRange = struct {
    range: Range,
    parent: ?*const SelectionRange = null,
};

fn freeSelectionRange(allocator: std.mem.Allocator, range: SelectionRange) void {
    var current: ?*const SelectionRange = range.parent;
    while (current) |r| {
        const next = r.parent;
        allocator.destroy(r);
        current = next;
    }
}

/// Compute selection range hierarchy for a position.
/// Walks the AST to find all containing nodes (token, expression, statement, file).
fn computeSelectionRange(allocator: std.mem.Allocator, source: []const u8, line: u32, character: u32) (Allocator.Error || error{ InvalidPosition, ParseFailed, NoRangeFound })!SelectionRange {
    // Build line offset table
    const line_offsets = try pos.buildLineOffsets(allocator, source);
    defer line_offsets.deinit();

    // Convert position to offset
    const target_offset = line_offsets.offsetAt(line, character) orelse return error.InvalidPosition;

    // Parse to get AST
    var module_env = try can.ModuleEnv.init(allocator, source);
    defer module_env.deinit();

    const ast = try parse.file(allocator, &module_env.common);
    defer ast.deinit();

    // Collect all containing regions
    var containing_regions: std.ArrayList(ByteRange) = .empty;
    defer containing_regions.deinit(allocator);

    // 1. Find the token at the position (innermost)
    const token_regions = ast.tokens.tokens.items(.region);
    for (token_regions) |region| {
        const start = region.start.offset;
        const end = region.end.offset;

        if (start <= target_offset and target_offset < end) {
            try containing_regions.append(allocator, .{ .start = start, .end = end });
            break;
        }
    }

    // 2. Walk AST to find containing expressions and statements
    const file = ast.store.getFile();
    const file_region = ast.tokenizedRegionToRegion(file.region);

    // Walk all top-level statements
    const stmt_indices = ast.store.statementSlice(file.statements);
    for (stmt_indices) |stmt_idx| {
        try collectContainingRegionsFromStatement(allocator, ast, stmt_idx, target_offset, &containing_regions);
    }

    // 3. Add file region as outermost
    try containing_regions.append(allocator, .{
        .start = file_region.start.offset,
        .end = file_region.end.offset,
    });

    // Sort by size (smallest first) to build proper hierarchy
    std.mem.sort(ByteRange, containing_regions.items, {}, struct {
        fn lessThan(_: void, a: ByteRange, b: ByteRange) bool {
            const size_a = a.end - a.start;
            const size_b = b.end - b.start;
            return size_a < size_b;
        }
    }.lessThan);

    // Remove duplicates (regions with same start and end)
    var unique_regions: std.ArrayList(ByteRange) = .empty;
    defer unique_regions.deinit(allocator);

    var prev: ?ByteRange = null;
    for (containing_regions.items) |region| {
        if (prev) |p| {
            if (p.start == region.start and p.end == region.end) {
                continue; // Skip duplicate
            }
        }
        try unique_regions.append(allocator, region);
        prev = region;
    }

    if (unique_regions.items.len == 0) {
        return error.NoRangeFound;
    }

    // Convert to LSP Ranges and build linked list (innermost to outermost)
    // Start with the outermost range (no parent)
    var i: usize = unique_regions.items.len;
    var current_parent: ?*const SelectionRange = null;

    while (i > 1) {
        i -= 1;
        const byte_range = unique_regions.items[i];
        const parent_node = try allocator.create(SelectionRange);
        parent_node.* = .{
            .range = .{
                .start = positionAt(byte_range.start, &line_offsets),
                .end = positionAt(byte_range.end, &line_offsets),
            },
            .parent = current_parent,
        };
        current_parent = parent_node;
    }

    // Return the innermost range
    const innermost = unique_regions.items[0];
    return .{
        .range = .{
            .start = positionAt(innermost.start, &line_offsets),
            .end = positionAt(innermost.end, &line_offsets),
        },
        .parent = current_parent,
    };
}

/// Byte range for sorting before converting to LSP positions
const ByteRange = struct {
    start: u32,
    end: u32,
};

/// Check if a byte range contains the target offset
fn rangeContainsOffset(start: u32, end: u32, offset: u32) bool {
    return start <= offset and offset < end;
}

/// Collect containing regions from an AST statement
fn collectContainingRegionsFromStatement(
    allocator: std.mem.Allocator,
    ast: *AST,
    stmt_idx: AST.Statement.Idx,
    target_offset: u32,
    regions: *std.ArrayList(ByteRange),
) std.mem.Allocator.Error!void {
    const stmt = ast.store.getStatement(stmt_idx);

    // Get the statement's region and check if it contains the offset
    const stmt_region = getStatementRegion(stmt);
    if (stmt_region) |tr| {
        const region = ast.tokenizedRegionToRegion(tr);
        if (rangeContainsOffset(region.start.offset, region.end.offset, target_offset)) {
            try regions.append(allocator, .{ .start = region.start.offset, .end = region.end.offset });
        }
    }

    // Recurse into expressions within the statement
    switch (stmt) {
        .decl => |d| {
            try collectContainingRegionsFromExpr(allocator, ast, d.body, target_offset, regions);
        },
        .@"var" => |v| {
            if (v.body) |body| {
                try collectContainingRegionsFromExpr(allocator, ast, body, target_offset, regions);
            }
        },
        .expr => |e| {
            try collectContainingRegionsFromExpr(allocator, ast, e.expr, target_offset, regions);
        },
        .crash => |c| {
            try collectContainingRegionsFromExpr(allocator, ast, c.expr, target_offset, regions);
        },
        .dbg => |d| {
            try collectContainingRegionsFromExpr(allocator, ast, d.expr, target_offset, regions);
        },
        .expect => |e| {
            try collectContainingRegionsFromExpr(allocator, ast, e.body, target_offset, regions);
        },
        .@"for" => |f| {
            try collectContainingRegionsFromExpr(allocator, ast, f.expr, target_offset, regions);
            try collectContainingRegionsFromExpr(allocator, ast, f.body, target_offset, regions);
        },
        .@"while" => |w| {
            try collectContainingRegionsFromExpr(allocator, ast, w.cond, target_offset, regions);
            try collectContainingRegionsFromExpr(allocator, ast, w.body, target_offset, regions);
        },
        .@"return" => |r| {
            try collectContainingRegionsFromExpr(allocator, ast, r.expr, target_offset, regions);
        },
        .@"break", .import, .file_import, .type_anno, .type_decl, .malformed => {},
    }
}

/// Get the TokenizedRegion for a statement
fn getStatementRegion(stmt: AST.Statement) ?TokenizedRegion {
    return switch (stmt) {
        .decl => |d| d.region,
        .@"var" => |v| v.region,
        .expr => |e| e.region,
        .crash => |c| c.region,
        .dbg => |d| d.region,
        .expect => |e| e.region,
        .@"for" => |f| f.region,
        .@"while" => |w| w.region,
        .@"return" => |r| r.region,
        .@"break" => |b| b.region,
        .import => |i| i.region,
        .type_decl => |t| t.region,
        .malformed => |m| m.region,
        .file_import => |fi| fi.region,
        .type_anno => null, // Type annotations don't have a simple region
    };
}

/// Collect containing regions from an AST expression
fn collectContainingRegionsFromExpr(
    allocator: std.mem.Allocator,
    ast: *AST,
    expr_idx: AST.Expr.Idx,
    target_offset: u32,
    regions: *std.ArrayList(ByteRange),
) std.mem.Allocator.Error!void {
    const expr = ast.store.getExpr(expr_idx);

    // Get the expression's region using the built-in method
    const tr = expr.to_tokenized_region();
    const region = ast.tokenizedRegionToRegion(tr);
    if (rangeContainsOffset(region.start.offset, region.end.offset, target_offset)) {
        try regions.append(allocator, .{ .start = region.start.offset, .end = region.end.offset });
    } else {
        // If this expression doesn't contain the offset, no need to recurse
        return;
    }

    // Recurse into child expressions
    switch (expr) {
        .list => |l| {
            const items = ast.store.exprSlice(l.items);
            for (items) |item| {
                try collectContainingRegionsFromExpr(allocator, ast, item, target_offset, regions);
            }
        },
        .tuple => |t| {
            const items = ast.store.exprSlice(t.items);
            for (items) |item| {
                try collectContainingRegionsFromExpr(allocator, ast, item, target_offset, regions);
            }
        },
        .tuple_access => |ta| {
            try collectContainingRegionsFromExpr(allocator, ast, ta.expr, target_offset, regions);
        },
        .record => |r| {
            const fields = ast.store.recordFieldSlice(r.fields);
            for (fields) |field_idx| {
                const field = ast.store.getRecordField(field_idx);
                if (field.value) |value| {
                    try collectContainingRegionsFromExpr(allocator, ast, value, target_offset, regions);
                }
            }
            if (r.ext) |ext| {
                try collectContainingRegionsFromExpr(allocator, ast, ext, target_offset, regions);
            }
        },
        .lambda => |l| {
            try collectContainingRegionsFromExpr(allocator, ast, l.body, target_offset, regions);
        },
        .apply => |a| {
            try collectContainingRegionsFromExpr(allocator, ast, a.@"fn", target_offset, regions);
            const args = ast.store.exprSlice(a.args);
            for (args) |arg| {
                try collectContainingRegionsFromExpr(allocator, ast, arg, target_offset, regions);
            }
        },
        .bin_op => |b| {
            try collectContainingRegionsFromExpr(allocator, ast, b.left, target_offset, regions);
            try collectContainingRegionsFromExpr(allocator, ast, b.right, target_offset, regions);
        },
        .field_access => |f| {
            // A flat field-access node replaces the nested expression nodes that
            // previously represented each prefix of a chain. Preserve those
            // semantic selections directly from the receiver and segment token
            // boundaries. The complete chain was appended above, so visit the
            // proper prefixes from outermost to innermost before the receiver.
            const receiver_region = ast.store.getExpr(f.receiver).to_tokenized_region();
            const segments = ast.store.fieldAccessSegmentSlice(f.segments);

            var prefix_len = segments.len -| 1;
            while (prefix_len > 0) {
                prefix_len -= 1;
                const prefix_region = ast.tokenizedRegionToRegion(.{
                    .start = receiver_region.start,
                    .end = segments[prefix_len].field_token + 1,
                });

                if (rangeContainsOffset(prefix_region.start.offset, prefix_region.end.offset, target_offset)) {
                    try regions.append(allocator, .{
                        .start = prefix_region.start.offset,
                        .end = prefix_region.end.offset,
                    });
                }
            }

            try collectContainingRegionsFromExpr(allocator, ast, f.receiver, target_offset, regions);
        },
        .method_call => |m| {
            try collectContainingRegionsFromExpr(allocator, ast, m.receiver, target_offset, regions);
            for (ast.store.exprSlice(m.args)) |arg| {
                try collectContainingRegionsFromExpr(allocator, ast, arg, target_offset, regions);
            }
        },
        .arrow_call => |d| {
            try collectContainingRegionsFromExpr(allocator, ast, d.left, target_offset, regions);
        },
        .unary_op => |u| {
            try collectContainingRegionsFromExpr(allocator, ast, u.expr, target_offset, regions);
        },
        .suffix_single_question => |s| {
            try collectContainingRegionsFromExpr(allocator, ast, s.expr, target_offset, regions);
        },
        .if_then_else => |i| {
            try collectContainingRegionsFromExpr(allocator, ast, i.condition, target_offset, regions);
            try collectContainingRegionsFromExpr(allocator, ast, i.then, target_offset, regions);
            try collectContainingRegionsFromExpr(allocator, ast, i.@"else", target_offset, regions);
        },
        .if_without_else => |i| {
            try collectContainingRegionsFromExpr(allocator, ast, i.condition, target_offset, regions);
            try collectContainingRegionsFromExpr(allocator, ast, i.then, target_offset, regions);
        },
        .match => |m| {
            try collectContainingRegionsFromExpr(allocator, ast, m.expr, target_offset, regions);
            const branches = ast.store.matchBranchSlice(m.branches);
            for (branches) |branch_idx| {
                const branch = ast.store.getBranch(branch_idx);
                if (branch.guard) |guard| {
                    try collectContainingRegionsFromExpr(allocator, ast, guard, target_offset, regions);
                }
                try collectContainingRegionsFromExpr(allocator, ast, branch.body, target_offset, regions);
            }
        },
        .block => |b| {
            const stmts = ast.store.statementSlice(b.statements);
            for (stmts) |stmt_idx| {
                try collectContainingRegionsFromStatement(allocator, ast, stmt_idx, target_offset, regions);
            }
        },
        .dbg => |d| {
            try collectContainingRegionsFromExpr(allocator, ast, d.expr, target_offset, regions);
        },
        .crash => |c| {
            try collectContainingRegionsFromExpr(allocator, ast, c.expr, target_offset, regions);
        },
        .record_builder => |rb| {
            try collectContainingRegionsFromExpr(allocator, ast, rb.mapper, target_offset, regions);
            const fields = ast.store.recordFieldSlice(rb.fields);
            for (fields) |field_idx| {
                const field = ast.store.getRecordField(field_idx);
                if (field.value) |value| {
                    try collectContainingRegionsFromExpr(allocator, ast, value, target_offset, regions);
                }
            }
        },
        .nominal_record => |nr| {
            try collectContainingRegionsFromExpr(allocator, ast, nr.mapper, target_offset, regions);
            try collectContainingRegionsFromExpr(allocator, ast, nr.backing, target_offset, regions);
        },
        .nominal_apply => |na| {
            try collectContainingRegionsFromExpr(allocator, ast, na.mapper, target_offset, regions);
            for (ast.store.exprSlice(na.args)) |arg| {
                try collectContainingRegionsFromExpr(allocator, ast, arg, target_offset, regions);
            }
        },
        .for_expr => |f| {
            try collectContainingRegionsFromExpr(allocator, ast, f.expr, target_offset, regions);
            try collectContainingRegionsFromExpr(allocator, ast, f.body, target_offset, regions);
        },
        .@"return" => |r| {
            try collectContainingRegionsFromExpr(allocator, ast, r.expr, target_offset, regions);
        },
        // Leaf expressions - no children to recurse into
        .int, .frac, .typed_int, .typed_frac, .single_quote, .string_part, .string, .multiline_string, .typed_string, .typed_multiline_string, .tag, .ident, .record_updater, .ellipsis, .@"break", .malformed => {},
    }
}

/// Convert a byte offset into this handler's position shape.
fn positionAt(offset: u32, line_offsets: *const pos.LineOffsets) Position {
    const converted = pos.offsetToPosition(offset, line_offsets);
    return .{ .line = converted.line, .character = converted.character };
}

test "selection ranges preserve flat field access prefixes" {
    const source = "main = root.first.?second.third\n";
    const selection = try computeSelectionRange(std.testing.allocator, source, 0, 8);
    defer freeSelectionRange(std.testing.allocator, selection);

    try std.testing.expectEqual(@as(u32, 7), selection.range.start.character);
    try std.testing.expectEqual(@as(u32, 11), selection.range.end.character);

    const first_prefix = selection.parent.?;
    try std.testing.expectEqual(@as(u32, 7), first_prefix.range.start.character);
    try std.testing.expectEqual(@as(u32, 17), first_prefix.range.end.character);

    const second_prefix = first_prefix.parent.?;
    try std.testing.expectEqual(@as(u32, 7), second_prefix.range.start.character);
    try std.testing.expectEqual(@as(u32, 25), second_prefix.range.end.character);

    const full_chain = second_prefix.parent.?;
    try std.testing.expectEqual(@as(u32, 7), full_chain.range.start.character);
    try std.testing.expectEqual(@as(u32, 31), full_chain.range.end.character);
}
