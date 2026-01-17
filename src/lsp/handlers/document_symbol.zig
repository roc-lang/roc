//! Handler for LSP `textDocument/documentSymbol` requests.
//!
//! Provides document outline/symbols for the editor sidebar.

const std = @import("std");
const protocol = @import("../protocol.zig");
const parse = @import("parse");
const can = @import("can");

const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;

/// Handler for `textDocument/documentSymbol` requests.
pub fn handler(comptime ServerType: type) type {
    return struct {
        pub fn call(self: *ServerType, id: *protocol.JsonId, maybe_params: ?std.json.Value) !void {
            const params = maybe_params orelse {
                try self.sendError(id, .invalid_params, "documentSymbol requires params");
                return;
            };

            const obj = switch (params) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "documentSymbol params must be an object");
                    return;
                },
            };

            // Extract textDocument.uri
            const text_doc_value = obj.get("textDocument") orelse {
                try self.sendError(id, .invalid_params, "missing textDocument");
                return;
            };
            const text_doc = switch (text_doc_value) {
                .object => |o| o,
                else => {
                    try self.sendError(id, .invalid_params, "textDocument must be an object");
                    return;
                },
            };
            const uri_value = text_doc.get("uri") orelse {
                try self.sendError(id, .invalid_params, "missing uri");
                return;
            };
            const uri = switch (uri_value) {
                .string => |s| s,
                else => {
                    try self.sendError(id, .invalid_params, "uri must be a string");
                    return;
                },
            };

            // Get the document text from the store
            const doc = self.doc_store.get(uri);
            const text = if (doc) |d| d.text else {
                try self.sendResponse(id, &[_]DocumentSymbol{});
                return;
            };

            // Extract symbols from the document
            const symbols = extractSymbols(self.allocator, text) catch |err| {
                std.log.err("symbol extraction failed: {s}", .{@errorName(err)});
                try self.sendResponse(id, &[_]DocumentSymbol{});
                return;
            };
            defer {
                for (symbols) |*sym| {
                    self.allocator.free(sym.name);
                }
                self.allocator.free(symbols);
            }

            try self.sendResponse(id, symbols);
        }
    };
}

/// LSP SymbolKind values
const SymbolKind = enum(u32) {
    file = 1,
    module = 2,
    namespace = 3,
    package = 4,
    class = 5,
    method = 6,
    property = 7,
    field = 8,
    constructor = 9,
    @"enum" = 10,
    interface = 11,
    function = 12,
    variable = 13,
    constant = 14,
    string = 15,
    number = 16,
    boolean = 17,
    array = 18,
    object = 19,
    key = 20,
    null = 21,
    enum_member = 22,
    @"struct" = 23,
    event = 24,
    operator = 25,
    type_parameter = 26,
};

const Position = struct {
    line: u32,
    character: u32,
};

const Range = struct {
    start: Position,
    end: Position,
};

const DocumentSymbol = struct {
    name: []const u8,
    kind: SymbolKind,
    range: Range,
    selectionRange: Range,
};

/// Extract document symbols from source code.
fn extractSymbols(allocator: std.mem.Allocator, source: []const u8) ![]DocumentSymbol {
    // Create ModuleEnv for parsing
    var module_env = ModuleEnv.init(allocator, source) catch {
        return &[_]DocumentSymbol{};
    };
    defer module_env.deinit();

    // Parse the source
    var ast = parse.parse(&module_env.common, allocator) catch {
        return &[_]DocumentSymbol{};
    };
    defer ast.deinit(allocator);

    // Initialize CIR fields
    module_env.initCIRFields("document-symbols") catch {
        return &[_]DocumentSymbol{};
    };

    // Create canonicalizer and run
    var canonicalizer = can.Can.init(&module_env, &ast, null) catch {
        return &[_]DocumentSymbol{};
    };
    defer canonicalizer.deinit();

    canonicalizer.canonicalizeFile() catch {
        return &[_]DocumentSymbol{};
    };

    // Build line offset table for position conversion
    const line_offsets = buildLineOffsets(source);

    // Walk statements and collect symbols
    var symbols = std.ArrayList(DocumentSymbol){};
    errdefer {
        for (symbols.items) |*sym| {
            allocator.free(sym.name);
        }
        symbols.deinit(allocator);
    }

    const statements_slice = module_env.store.sliceStatements(module_env.all_statements);
    for (statements_slice) |stmt_idx| {
        if (extractSymbolFromStatement(&module_env, stmt_idx, source, &line_offsets)) |symbol| {
            // Duplicate the name to own it
            const owned_name = try allocator.dupe(u8, symbol.name);
            try symbols.append(allocator, .{
                .name = owned_name,
                .kind = symbol.kind,
                .range = symbol.range,
                .selectionRange = symbol.selectionRange,
            });
        }
    }

    return symbols.toOwnedSlice(allocator);
}

const LineOffsets = struct {
    offsets: [1024]u32,
    count: usize,
};

fn buildLineOffsets(source: []const u8) LineOffsets {
    var result = LineOffsets{ .offsets = undefined, .count = 0 };
    result.offsets[0] = 0;
    result.count = 1;

    for (source, 0..) |c, i| {
        if (c == '\n' and result.count < 1024) {
            result.offsets[result.count] = @intCast(i + 1);
            result.count += 1;
        }
    }
    return result;
}

fn offsetToPosition(offset: u32, line_offsets: *const LineOffsets) Position {
    var line: u32 = 0;
    for (0..line_offsets.count) |i| {
        if (line_offsets.offsets[i] > offset) break;
        line = @intCast(i);
    }
    const line_start = line_offsets.offsets[line];
    return .{
        .line = line,
        .character = offset - line_start,
    };
}

fn extractSymbolFromStatement(
    module_env: *const ModuleEnv,
    stmt_idx: CIR.Statement.Idx,
    source: []const u8,
    line_offsets: *const LineOffsets,
) ?DocumentSymbol {
    const stmt = module_env.store.getStatement(stmt_idx);

    switch (stmt) {
        .s_decl => |d| {
            return extractSymbolFromDecl(module_env, d.pattern, d.expr, source, line_offsets);
        },
        .s_decl_gen => |d| {
            return extractSymbolFromDecl(module_env, d.pattern, d.expr, source, line_offsets);
        },
        .s_var => |v| {
            return extractSymbolFromDecl(module_env, v.pattern_idx, v.expr, source, line_offsets);
        },
        else => return null,
    }
}

fn extractSymbolFromDecl(
    module_env: *const ModuleEnv,
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: CIR.Expr.Idx,
    source: []const u8,
    line_offsets: *const LineOffsets,
) ?DocumentSymbol {
    // Check if RHS is a function
    const expr = module_env.store.getExpr(expr_idx);
    const is_function = switch (expr) {
        .e_closure, .e_lambda, .e_hosted_lambda => true,
        else => false,
    };

    // Get the pattern region and extract name
    const pattern_region = module_env.store.getPatternRegion(pattern_idx);
    const start_offset = pattern_region.start.offset;
    const end_offset = pattern_region.end.offset;

    // Extract the name from source
    if (start_offset >= source.len or end_offset > source.len or end_offset <= start_offset) {
        return null;
    }

    const name = source[start_offset..end_offset];

    // Convert offsets to positions
    const start_pos = offsetToPosition(start_offset, line_offsets);
    const end_pos = offsetToPosition(end_offset, line_offsets);

    return .{
        .name = name,
        .kind = if (is_function) .function else .variable,
        .range = .{ .start = start_pos, .end = end_pos },
        .selectionRange = .{ .start = start_pos, .end = end_pos },
    };
}
