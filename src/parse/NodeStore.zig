//! Typesafe access to an underlying SoA of Nodes.
//! This - along with the types used in its API - should
//! be the only way that other modules interact with
//! the AST.

const std = @import("std");
const Allocator = std.mem.Allocator;
const builtin = @import("builtin");
const base = @import("base");

const AST = @import("AST.zig");
const Node = @import("Node.zig");
const NumericLiteral = @import("NumericLiteral.zig");
const Token = @import("tokenize.zig").Token;
const Region = AST.TokenizedRegion;
const Diagnostic = AST.Diagnostic;

const TargetConfigValueNodeTag = enum(u32) {
    int_literal,
    string_literal,
    tag_literal,
    ident,
    list,
    files,
    malformed,
};

/// Packed optional indices store null as 0 and non-null values as value + 1.
/// All producers must use the fallible helpers below so maxInt(u32) overflows
/// become OutOfMemory in every build mode.
const OPTIONAL_VALUE_OFFSET: u32 = 1;

/// The root node is always stored at index 0 in the node list.
pub const root_node_idx: Node.List.Idx = .first;

const NodeStore = @This();

gpa: std.mem.Allocator,
nodes: Node.List,
extra_data: std.ArrayList(u32),
scratch_statements: base.Scratch(AST.Statement.Idx),
scratch_tokens: base.Scratch(Token.Idx),
scratch_exprs: base.Scratch(AST.Expr.Idx),
scratch_patterns: base.Scratch(AST.Pattern.Idx),
scratch_pattern_string_parts: base.Scratch(AST.PatternStringPart.Idx),
scratch_record_fields: base.Scratch(AST.RecordField.Idx),
scratch_pattern_record_fields: base.Scratch(AST.PatternRecordField.Idx),
scratch_match_branches: base.Scratch(AST.MatchBranch.Idx),
scratch_type_annos: base.Scratch(AST.TypeAnno.Idx),
scratch_anno_record_fields: base.Scratch(AST.AnnoRecordField.Idx),
scratch_exposed_items: base.Scratch(AST.ExposedItem.Idx),
scratch_where_clauses: base.Scratch(AST.WhereClause.Idx),
scratch_target_entries: base.Scratch(AST.TargetEntry.Idx),
scratch_target_files: base.Scratch(AST.TargetFile.Idx),
scratch_target_config_entries: base.Scratch(AST.TargetConfigEntry.Idx),
scratch_target_config_values: base.Scratch(AST.TargetConfigValue.Idx),
scratch_symbol_map_entries: base.Scratch(AST.SymbolMapEntry.Idx),
scratch_for_clause_type_aliases: base.Scratch(AST.ForClauseTypeAlias.Idx),
scratch_requires_entries: base.Scratch(AST.RequiresEntry.Idx),
numeric_literals: std.ArrayList(NumericLiteral.Stored),
numeric_literal_bytes: std.ArrayList(u8),
pattern_string_parts: std.ArrayList(AST.PatternStringPart),

fn reserveExtraDataStart(store: *NodeStore, count: usize) std.mem.Allocator.Error!u32 {
    const start = std.math.cast(u32, store.extra_data.items.len) orelse return error.OutOfMemory;
    if (count != 0) {
        const last_offset = std.math.cast(u32, count - 1) orelse return error.OutOfMemory;
        if (start > std.math.maxInt(u32) - last_offset) return error.OutOfMemory;
    }
    try store.extra_data.ensureUnusedCapacity(store.gpa, count);
    return start;
}

fn reserveExtraDataToken(store: *NodeStore, count: usize) std.mem.Allocator.Error!u32 {
    const start = try store.reserveExtraDataStart(count);
    return packNonNullOptionalU32(start);
}

fn extraDataStart(token: u32) u32 {
    std.debug.assert(token != 0);
    return unpackNonNullOptionalU32(token);
}

fn packNonNullOptionalU32(value: u32) std.mem.Allocator.Error!u32 {
    if (value == std.math.maxInt(u32)) return error.OutOfMemory;
    return value + OPTIONAL_VALUE_OFFSET;
}

fn packOptionalIndex(value: anytype) std.mem.Allocator.Error!u32 {
    if (value) |idx| {
        return packNonNullOptionalU32(@intFromEnum(idx));
    }
    return 0;
}

fn unpackNonNullOptionalU32(value: u32) u32 {
    std.debug.assert(value != 0);
    return value - OPTIONAL_VALUE_OFFSET;
}

fn unpackOptionalIndex(comptime Idx: type, value: u32) ?Idx {
    if (value == 0) return null;
    return @enumFromInt(unpackNonNullOptionalU32(value));
}

const TypeDeclExtra = struct {
    where: ?AST.Collection.Idx,
    associated: ?AST.Associated,
};

fn decodeTypeDeclExtra(store: *const NodeStore, token: u32) TypeDeclExtra {
    if (token == 0) {
        return .{
            .where = null,
            .associated = null,
        };
    }

    const extra_start = extraDataStart(token);
    const has_where = store.extra_data.items[extra_start] != 0;
    const where_clause: ?AST.Collection.Idx = if (has_where)
        @enumFromInt(store.extra_data.items[extra_start + 1])
    else
        null;

    const has_associated = store.extra_data.items[extra_start + 2] != 0;
    const associated: ?AST.Associated = if (has_associated) blk: {
        const stmt_start = store.extra_data.items[extra_start + 3];
        const stmt_len = store.extra_data.items[extra_start + 4];
        const scope_idx = store.extra_data.items[extra_start + 5];
        const reg_start = store.extra_data.items[extra_start + 6];
        const reg_end = store.extra_data.items[extra_start + 7];
        break :blk AST.Associated{
            .statements = AST.Statement.Span{ .span = .{ .start = stmt_start, .len = stmt_len } },
            .scope = @enumFromInt(scope_idx),
            .region = .{ .start = reg_start, .end = reg_end },
        };
    } else null;

    return .{
        .where = where_clause,
        .associated = associated,
    };
}

/// Compile-time constants for union variant counts to ensure we don't miss cases
/// when adding/removing variants from AST unions. Update these when modifying the unions.
///
/// Count of the header nodes in the AST
pub const AST_HEADER_NODE_COUNT = 6;
/// Count of the statement nodes in the AST
pub const AST_STATEMENT_NODE_COUNT = 13;
/// Count of the pattern nodes in the AST
pub const AST_PATTERN_NODE_COUNT = 17;
/// Count of the type annotation nodes in the AST
pub const AST_TYPE_ANNO_NODE_COUNT = 11;
/// Count of the expression nodes in the AST
pub const AST_EXPR_NODE_COUNT = std.meta.fields(AST.Expr).len;

/// Initialize the store with an assumed capacity to
/// ensure resizing of underlying data structures happens
/// very rarely.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!NodeStore {
    var nodes = try Node.List.initCapacity(gpa, capacity);
    errdefer nodes.deinit(gpa);
    var extra_data = try std.ArrayList(u32).initCapacity(gpa, capacity / 2);
    errdefer extra_data.deinit(gpa);
    var scratch_statements = try base.Scratch(AST.Statement.Idx).init(gpa);
    errdefer scratch_statements.deinit();
    var scratch_tokens = try base.Scratch(Token.Idx).init(gpa);
    errdefer scratch_tokens.deinit();
    var scratch_exprs = try base.Scratch(AST.Expr.Idx).init(gpa);
    errdefer scratch_exprs.deinit();
    var scratch_patterns = try base.Scratch(AST.Pattern.Idx).init(gpa);
    errdefer scratch_patterns.deinit();
    var scratch_pattern_string_parts = try base.Scratch(AST.PatternStringPart.Idx).init(gpa);
    errdefer scratch_pattern_string_parts.deinit();
    var scratch_record_fields = try base.Scratch(AST.RecordField.Idx).init(gpa);
    errdefer scratch_record_fields.deinit();
    var scratch_pattern_record_fields = try base.Scratch(AST.PatternRecordField.Idx).init(gpa);
    errdefer scratch_pattern_record_fields.deinit();
    var scratch_match_branches = try base.Scratch(AST.MatchBranch.Idx).init(gpa);
    errdefer scratch_match_branches.deinit();
    var scratch_type_annos = try base.Scratch(AST.TypeAnno.Idx).init(gpa);
    errdefer scratch_type_annos.deinit();
    var scratch_anno_record_fields = try base.Scratch(AST.AnnoRecordField.Idx).init(gpa);
    errdefer scratch_anno_record_fields.deinit();
    var scratch_exposed_items = try base.Scratch(AST.ExposedItem.Idx).init(gpa);
    errdefer scratch_exposed_items.deinit();
    var scratch_where_clauses = try base.Scratch(AST.WhereClause.Idx).init(gpa);
    errdefer scratch_where_clauses.deinit();
    var scratch_target_entries = try base.Scratch(AST.TargetEntry.Idx).init(gpa);
    errdefer scratch_target_entries.deinit();
    var scratch_target_files = try base.Scratch(AST.TargetFile.Idx).init(gpa);
    errdefer scratch_target_files.deinit();
    var scratch_target_config_entries = try base.Scratch(AST.TargetConfigEntry.Idx).init(gpa);
    errdefer scratch_target_config_entries.deinit();
    var scratch_target_config_values = try base.Scratch(AST.TargetConfigValue.Idx).init(gpa);
    errdefer scratch_target_config_values.deinit();
    var scratch_symbol_map_entries = try base.Scratch(AST.SymbolMapEntry.Idx).init(gpa);
    errdefer scratch_symbol_map_entries.deinit();
    var scratch_for_clause_type_aliases = try base.Scratch(AST.ForClauseTypeAlias.Idx).init(gpa);
    errdefer scratch_for_clause_type_aliases.deinit();
    var scratch_requires_entries = try base.Scratch(AST.RequiresEntry.Idx).init(gpa);
    errdefer scratch_requires_entries.deinit();
    var numeric_literals = try std.ArrayList(NumericLiteral.Stored).initCapacity(gpa, capacity / 8);
    errdefer numeric_literals.deinit(gpa);
    var numeric_literal_bytes = try std.ArrayList(u8).initCapacity(gpa, capacity);
    errdefer numeric_literal_bytes.deinit(gpa);
    var pattern_string_parts = try std.ArrayList(AST.PatternStringPart).initCapacity(gpa, capacity / 8);
    errdefer pattern_string_parts.deinit(gpa);

    var store: NodeStore = .{
        .gpa = gpa,
        .nodes = nodes,
        .extra_data = extra_data,
        .scratch_statements = scratch_statements,
        .scratch_tokens = scratch_tokens,
        .scratch_exprs = scratch_exprs,
        .scratch_patterns = scratch_patterns,
        .scratch_pattern_string_parts = scratch_pattern_string_parts,
        .scratch_record_fields = scratch_record_fields,
        .scratch_pattern_record_fields = scratch_pattern_record_fields,
        .scratch_match_branches = scratch_match_branches,
        .scratch_type_annos = scratch_type_annos,
        .scratch_anno_record_fields = scratch_anno_record_fields,
        .scratch_exposed_items = scratch_exposed_items,
        .scratch_where_clauses = scratch_where_clauses,
        .scratch_target_entries = scratch_target_entries,
        .scratch_target_files = scratch_target_files,
        .scratch_target_config_entries = scratch_target_config_entries,
        .scratch_target_config_values = scratch_target_config_values,
        .scratch_symbol_map_entries = scratch_symbol_map_entries,
        .scratch_for_clause_type_aliases = scratch_for_clause_type_aliases,
        .scratch_requires_entries = scratch_requires_entries,
        .numeric_literals = numeric_literals,
        .numeric_literal_bytes = numeric_literal_bytes,
        .pattern_string_parts = pattern_string_parts,
    };

    const expected_idx = store.nodes.items.len;
    const idx = try store.nodes.append(gpa, .{
        .tag = .root,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = .{ .start = 0, .end = 0 },
    });
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(@intFromEnum(idx) == expected_idx);
    } else if (@intFromEnum(idx) != expected_idx) {
        unreachable;
    }
    return store;
}

/// Deinitializes all data owned by the store.
/// A caller should ensure that they have taken
/// ownership of all Node data before calling this
/// method.
pub fn deinit(store: *NodeStore) void {
    store.nodes.deinit(store.gpa);
    store.extra_data.deinit(store.gpa);
    store.scratch_statements.deinit();
    store.scratch_tokens.deinit();
    store.scratch_exprs.deinit();
    store.scratch_patterns.deinit();
    store.scratch_pattern_string_parts.deinit();
    store.scratch_record_fields.deinit();
    store.scratch_pattern_record_fields.deinit();
    store.scratch_match_branches.deinit();
    store.scratch_type_annos.deinit();
    store.scratch_anno_record_fields.deinit();
    store.scratch_exposed_items.deinit();
    store.scratch_where_clauses.deinit();
    store.scratch_target_entries.deinit();
    store.scratch_target_files.deinit();
    store.scratch_target_config_entries.deinit();
    store.scratch_target_config_values.deinit();
    store.scratch_symbol_map_entries.deinit();
    store.scratch_for_clause_type_aliases.deinit();
    store.scratch_requires_entries.deinit();
    store.numeric_literals.deinit(store.gpa);
    store.numeric_literal_bytes.deinit(store.gpa);
    store.pattern_string_parts.deinit(store.gpa);
}

/// Ensures that all scratch buffers in the store
/// are clear for use.
pub fn emptyScratch(store: *NodeStore) void {
    store.scratch_statements.clearFrom(0);
    store.scratch_tokens.clearFrom(0);
    store.scratch_exprs.clearFrom(0);
    store.scratch_patterns.clearFrom(0);
    store.scratch_pattern_string_parts.clearFrom(0);
    store.scratch_record_fields.clearFrom(0);
    store.scratch_pattern_record_fields.clearFrom(0);
    store.scratch_match_branches.clearFrom(0);
    store.scratch_type_annos.clearFrom(0);
    store.scratch_anno_record_fields.clearFrom(0);
    store.scratch_exposed_items.clearFrom(0);
    store.scratch_where_clauses.clearFrom(0);
    store.scratch_target_entries.clearFrom(0);
    store.scratch_target_files.clearFrom(0);
    store.scratch_for_clause_type_aliases.clearFrom(0);
    store.scratch_requires_entries.clearFrom(0);
}

/// Prints debug information about all nodes and scratch buffers in the store.
pub fn debug(store: *NodeStore, writer: *std.Io.Writer) void {
    store.debugTo(writer) catch {};
}

/// Writes debug information about all nodes and scratch buffers to the given writer.
pub fn debugTo(store: *NodeStore, writer: *std.Io.Writer) error{WriteFailed}!void {
    try writer.print("\n==> IR.NodeStore DEBUG <==\n", .{});
    try writer.print("Nodes:\n", .{});
    var nodes_iter = store.nodes.iterIndices();
    while (nodes_iter.next()) |idx| {
        try writer.print("{d}: {any}\n", .{ @intFromEnum(idx), store.nodes.get(idx) });
    }
    try writer.print("Extra Data: {any}\n", .{store.extra_data.items});
    try writer.print("Scratch statements: {any}\n", .{store.scratch_statements.items});
    try writer.print("Scratch tokens: {any}\n", .{store.scratch_tokens.items});
    try writer.print("Scratch exprs: {any}\n", .{store.scratch_exprs.items});
    try writer.print("Scratch patterns: {any}\n", .{store.scratch_patterns.items});
    try writer.print("Scratch record fields: {any}\n", .{store.scratch_record_fields.items});
    try writer.print("Scratch pattern record fields: {any}\n", .{store.scratch_pattern_record_fields.items});
    try writer.print("Scratch match branches: {any}\n", .{store.scratch_match_branches.items});
    try writer.print("Scratch type annos: {any}\n", .{store.scratch_type_annos.items});
    try writer.print("Scratch anno record fields: {any}\n", .{store.scratch_anno_record_fields.items});
    try writer.print("Scratch exposes items: {any}\n", .{store.scratch_exposed_items.items});
    try writer.print("Scratch where clauses: {any}\n", .{store.scratch_where_clauses.items});
    try writer.print("==> IR.NodeStore DEBUG <==\n\n", .{});
}

/// Any node type can be malformed, but must come with a diagnostic reason
pub fn addMalformed(store: *NodeStore, comptime T: type, reason: Diagnostic.Tag, region: Region) std.mem.Allocator.Error!T {
    const nid = try store.nodes.append(store.gpa, .{
        .tag = .malformed,
        .main_token = 0,
        .data = .{ .lhs = @intFromEnum(reason), .rhs = 0 },
        .region = region,
    });
    return @enumFromInt(@intFromEnum(nid));
}

/// Records parser-owned numeric literal facts and returns the id stored on the AST node.
pub fn addNumericLiteral(
    store: *NodeStore,
    text: []const u8,
    kind: NumericLiteral.Kind,
) std.mem.Allocator.Error!NumericLiteral.Idx {
    const parsed = try NumericLiteral.parse(store.gpa, text, kind);
    defer parsed.deinit(store.gpa);

    const digits_start: u32 = @intCast(store.numeric_literal_bytes.items.len);
    try store.numeric_literal_bytes.appendSlice(store.gpa, parsed.before);
    try store.numeric_literal_bytes.appendSlice(store.gpa, parsed.after);

    const idx: NumericLiteral.Idx = @enumFromInt(store.numeric_literals.items.len);
    try store.numeric_literals.append(store.gpa, .{
        .kind = parsed.kind,
        .compact = parsed.compact,
        .digits_start = digits_start,
        .before_len = @intCast(parsed.before.len),
        .after_len = @intCast(parsed.after.len),
        .after_decimal_digit_count = parsed.after_decimal_digit_count,
        .flags = .{
            .is_negative = parsed.is_negative,
            .had_decimal_point = parsed.had_decimal_point,
        },
    });
    return idx;
}

/// Return parsed numeric literal facts by index.
pub fn getNumericLiteral(store: *const NodeStore, idx: NumericLiteral.Idx) NumericLiteral.Stored {
    return store.numeric_literals.items[@intFromEnum(idx)];
}

/// Return base-256 digits before the decimal point for a numeric literal.
pub fn numericDigitsBefore(store: *const NodeStore, literal: NumericLiteral.Stored) []const u8 {
    return store.numeric_literal_bytes.items[literal.digits_start..][0..literal.before_len];
}

/// Return base-256 digits after the decimal point for a numeric literal.
pub fn numericDigitsAfter(store: *const NodeStore, literal: NumericLiteral.Stored) []const u8 {
    const start = literal.digits_start + literal.before_len;
    return store.numeric_literal_bytes.items[start..][0..literal.after_len];
}

/// Adds a file node to the store.
pub fn addFile(store: *NodeStore, file: AST.File) std.mem.Allocator.Error!void {
    try store.extra_data.append(store.gpa, @intFromEnum(file.header));
    store.nodes.set(root_node_idx, .{
        .tag = .root,
        .main_token = @intFromEnum(file.scope),
        .data = .{ .lhs = file.statements.span.start, .rhs = file.statements.span.len },
        .region = file.region,
    });
}

/// Adds a AST.Collection node with the specified tag and returns its index.
pub fn addCollection(store: *NodeStore, tag: Node.Tag, collection: AST.Collection) std.mem.Allocator.Error!AST.Collection.Idx {
    const nid = try store.nodes.append(store.gpa, Node{
        .tag = tag,
        .main_token = 0,
        .data = .{
            .lhs = collection.span.start,
            .rhs = collection.span.len,
        },
        .region = collection.region,
    });
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a header node of any type (app, module, hosted, package, platform) and returns its index.
pub fn addHeader(store: *NodeStore, header: AST.Header) std.mem.Allocator.Error!AST.Header.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };
    switch (header) {
        .app => |app| {
            node.tag = .app_header;
            // TODO this doesn't seem right, were giving it a record_field index instead of a token index
            node.main_token = @intFromEnum(app.platform_idx);
            // Store provides collection
            node.data.lhs = @intFromEnum(app.provides);
            node.data.rhs = @intFromEnum(app.packages);
            node.region = app.region;

            try store.extra_data.append(store.gpa, @intFromEnum(app.platform_idx));
        },
        .module => |mod| {
            node.tag = .module_header;
            node.data.lhs = @intFromEnum(mod.exposes);
            node.region = mod.region;
        },
        .hosted => |hosted| {
            node.tag = .hosted_header;
            node.data.lhs = @intFromEnum(hosted.exposes);
            node.region = hosted.region;
        },
        .package => |package| {
            node.tag = .package_header;
            node.data.lhs = @intFromEnum(package.exposes);
            node.data.rhs = @intFromEnum(package.packages);
            node.region = package.region;
        },
        .platform => |platform| {
            node.tag = .platform_header;
            node.main_token = platform.name;

            const ed_start = try store.reserveExtraDataStart(9);
            // Store requires_entries span (start and len)
            store.extra_data.appendAssumeCapacity(platform.requires_entries.span.start);
            store.extra_data.appendAssumeCapacity(platform.requires_entries.span.len);
            store.extra_data.appendAssumeCapacity(@intFromEnum(platform.exposes));
            store.extra_data.appendAssumeCapacity(@intFromEnum(platform.packages));
            store.extra_data.appendAssumeCapacity(platform.provides.span.start);
            store.extra_data.appendAssumeCapacity(platform.provides.span.len);
            store.extra_data.appendAssumeCapacity(platform.hosted.span.start);
            store.extra_data.appendAssumeCapacity(platform.hosted.span.len);
            store.extra_data.appendAssumeCapacity(try packOptionalIndex(platform.targets));

            node.data.lhs = ed_start;
            node.data.rhs = 9;

            node.region = platform.region;
        },
        .type_module => |type_module| {
            node.tag = .type_module_header;
            node.region = type_module.region;
        },
        .default_app => |default_app| {
            node.tag = .default_app_header;
            node.main_token = default_app.main_fn_idx;
            node.region = default_app.region;
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }

    const node_idx = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Adds an exposed item node (function, value, type, etc.) and returns its index.
pub fn addExposedItem(store: *NodeStore, item: AST.ExposedItem) std.mem.Allocator.Error!AST.ExposedItem.Idx {
    var node = Node{
        .tag = .malformed,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (item) {
        .lower_ident => |i| {
            node.tag = .exposed_item_lower;
            node.main_token = i.ident;
            if (i.as) |a| {
                std.debug.assert(a > 0);
                node.data.lhs = a;
                node.data.rhs = 1;
            }
            node.region = i.region;
        },
        .upper_ident => |i| {
            node.tag = .exposed_item_upper;
            node.main_token = i.ident;
            if (i.as) |a| {
                std.debug.assert(a > 0);
                node.data.lhs = a;
                node.data.rhs = 1;
            }
            node.region = i.region;
        },
        .upper_ident_star => |i| {
            node.tag = .exposed_item_upper_star;
            node.main_token = i.ident;
            node.region = i.region;
        },
        .malformed => |m| {
            node.tag = .malformed;
            node.data.lhs = @intFromEnum(m.reason);
            node.data.rhs = 0;
            node.region = m.region;
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a statement node (value def, type def, import, etc.) and returns its index.
pub fn addStatement(store: *NodeStore, statement: AST.Statement) std.mem.Allocator.Error!AST.Statement.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };
    switch (statement) {
        .decl => |d| {
            node.tag = .decl;
            node.data.lhs = @intFromEnum(d.pattern);
            node.data.rhs = @intFromEnum(d.body);
            node.region = d.region;
        },
        .@"var" => |v| {
            node.tag = .@"var";
            node.main_token = v.name;
            node.data.lhs = try packOptionalIndex(v.body);
            node.region = v.region;
        },
        .expr => |expr| {
            node.tag = .expr;
            node.data.lhs = @intFromEnum(expr.expr);
            node.region = expr.region;
        },
        .crash => |c| {
            node.tag = .crash;
            node.data.lhs = @intFromEnum(c.expr);
            node.region = c.region;
        },
        .dbg => |d| {
            node.tag = .dbg;
            node.data.lhs = @intFromEnum(d.expr);
            node.region = d.region;
        },
        .expect => |e| {
            node.tag = .expect;
            node.data.lhs = @intFromEnum(e.body);
            node.region = e.region;
        },
        .@"for" => |f| {
            node.tag = .@"for";
            node.main_token = @intFromEnum(f.patt);
            node.data.lhs = @intFromEnum(f.expr);
            node.data.rhs = @intFromEnum(f.body);
            node.region = f.region;
        },
        .@"while" => |w| {
            node.tag = .@"while";
            node.main_token = @intFromEnum(w.cond);
            node.data.lhs = @intFromEnum(w.cond);
            node.data.rhs = @intFromEnum(w.body);
            node.region = w.region;
        },
        .@"break" => |b| {
            node.tag = .@"break";
            node.region = b.region;
        },
        .@"return" => |r| {
            node.tag = .@"return";
            node.data.lhs = @intFromEnum(r.expr);
            node.region = r.region;
        },
        .import => |i| {
            node.tag = .import;
            node.region = i.region;
            node.main_token = i.module_name_tok;
            var rhs = AST.ImportRhs{
                .aliased = 0,
                .qualified = 0,
                .num_exposes = @as(u30, @intCast(i.exposes.span.len)),
            };

            // Store all import data in a flat format:
            // [exposes.span.start, exposes.span.len, qualifier_tok?, alias_tok?]
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, i.exposes.span.start);
            try store.extra_data.append(store.gpa, i.exposes.span.len);

            if (i.qualifier_tok) |tok| {
                rhs.qualified = 1;
                try store.extra_data.append(store.gpa, tok);
            }
            if (i.alias_tok) |tok| {
                rhs.aliased = 1;
                try store.extra_data.append(store.gpa, tok);
            }

            node.data.rhs = @as(u32, @bitCast(rhs));
            node.data.lhs = data_start;
        },
        .type_decl => |d| {
            node.tag = switch (d.kind) {
                .alias => .type_decl,
                .nominal => .type_decl_nominal,
                .@"opaque" => .type_decl_opaque,
            };
            node.region = d.region;
            node.data.lhs = @intFromEnum(d.header);
            node.data.rhs = @intFromEnum(d.anno);

            // Store optional where and associated in extra_data if either is present.
            // Collection.Idx 0 is valid, so presence is stored explicitly.
            if (d.where != null or d.associated != null) {
                // Format: [has_where, where_idx, has_associated, associated_data...]
                // where_idx is meaningful only when has_where is 1.
                // has_associated is 0 or 1
                // associated_data is [statements_start, statements_len, scope_idx, region_start, region_end] if has_associated == 1
                const extra_count: usize = if (d.associated != null) 8 else 3;
                node.main_token = try store.reserveExtraDataToken(extra_count);

                const has_where: u32 = @intFromBool(d.where != null);
                const where_idx: u32 = if (d.where) |w| @intFromEnum(w) else 0;
                store.extra_data.appendAssumeCapacity(has_where);
                store.extra_data.appendAssumeCapacity(where_idx);

                // Store associated data if present
                if (d.associated) |assoc| {
                    store.extra_data.appendAssumeCapacity(1); // has_associated = 1
                    store.extra_data.appendAssumeCapacity(assoc.statements.span.start);
                    store.extra_data.appendAssumeCapacity(assoc.statements.span.len);
                    store.extra_data.appendAssumeCapacity(@intFromEnum(assoc.scope));
                    store.extra_data.appendAssumeCapacity(assoc.region.start);
                    store.extra_data.appendAssumeCapacity(assoc.region.end);
                } else {
                    store.extra_data.appendAssumeCapacity(0); // has_associated = 0
                }
            } else {
                node.main_token = 0;
            }
        },
        .type_anno => |a| {
            node.tag = .type_anno;
            node.region = a.region;
            node.data.lhs = a.name;
            node.data.rhs = @intFromEnum(a.anno);
            if (a.where != null or a.is_var) {
                node.main_token = try store.reserveExtraDataToken(3);
                store.extra_data.appendAssumeCapacity(@intFromBool(a.where != null));
                store.extra_data.appendAssumeCapacity(if (a.where) |w| @intFromEnum(w) else 0);
                store.extra_data.appendAssumeCapacity(@intFromBool(a.is_var));
            } else {
                node.main_token = 0;
            }
        },
        .file_import => |fi| {
            node.tag = .file_import;
            node.region = fi.region;
            node.main_token = fi.path_tok;
            node.data.lhs = fi.name_tok;
            node.data.rhs = fi.type_tok | (if (fi.is_bytes) @as(u32, 1) << 31 else 0);
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a pattern node (identifier, literal, record destructure, etc.) and returns its index.
pub fn addPattern(store: *NodeStore, pattern: AST.Pattern) std.mem.Allocator.Error!AST.Pattern.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };
    switch (pattern) {
        .ident => |i| {
            node.tag = .ident_patt;
            node.region = i.region;
            node.main_token = i.ident_tok;
        },
        .var_ident => |i| {
            node.tag = .var_ident_patt;
            node.region = i.region;
            node.main_token = i.ident_tok;
        },
        .tag => |t| {
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, t.args.span.len);
            try store.extra_data.append(store.gpa, t.qualifiers.span.start);
            try store.extra_data.append(store.gpa, t.qualifiers.span.len);
            try store.extra_data.append(store.gpa, @intFromBool(t.backing_value));

            node.tag = .tag_patt;
            node.region = t.region;
            node.main_token = t.tag_tok;
            node.data.lhs = t.args.span.start;
            node.data.rhs = data_start;
        },
        .int => |n| {
            node.tag = .int_patt;
            node.region = n.region;
            node.main_token = n.number_tok;
            node.data.lhs = @intFromEnum(n.literal);
        },
        .frac => |n| {
            node.tag = .frac_patt;
            node.region = n.region;
            node.main_token = n.number_tok;
            node.data.lhs = @intFromEnum(n.literal);
        },
        .typed_int => |n| {
            node.tag = .typed_int_patt;
            node.region = n.region;
            node.main_token = n.number_tok;
            node.data.lhs = @bitCast(n.type_ident);
            node.data.rhs = @intFromEnum(n.literal);
        },
        .typed_frac => |n| {
            node.tag = .typed_frac_patt;
            node.region = n.region;
            node.main_token = n.number_tok;
            node.data.lhs = @bitCast(n.type_ident);
            node.data.rhs = @intFromEnum(n.literal);
        },
        .string => |s| {
            node.tag = .string_patt;
            node.region = s.region;
            node.main_token = s.string_tok;
            node.data.lhs = s.parts.span.start;
            node.data.rhs = s.parts.span.len;
        },
        .single_quote => |sq| {
            node.tag = .single_quote_patt;
            node.region = sq.region;
            node.main_token = sq.token;
        },
        .record => |r| {
            node.tag = .record_patt;
            node.region = r.region;
            node.data.lhs = r.fields.span.start;
            node.data.rhs = r.fields.span.len;
        },
        .list => |l| {
            node.tag = .list_patt;
            node.region = l.region;
            node.data.lhs = l.patterns.span.start;
            node.data.rhs = l.patterns.span.len;
        },
        .list_rest => |r| {
            node.tag = .list_rest_patt;
            node.region = r.region;
            if (r.name) |n| {
                node.data.lhs = n;
            }
        },
        .tuple => |t| {
            node.tag = .tuple_patt;
            node.region = t.region;
            node.data.lhs = t.patterns.span.start;
            node.data.rhs = t.patterns.span.len;
        },
        .underscore => |u| {
            node.tag = .underscore_patt;
            node.region = u.region;
        },
        .alternatives => |a| {
            std.debug.assert(a.patterns.span.len > 1);
            node.region = a.region;
            node.tag = .alternatives_patt;
            node.data.lhs = a.patterns.span.start;
            node.data.rhs = a.patterns.span.len;
        },
        .as => |a| {
            node.region = a.region;
            node.tag = .as_patt;
            node.main_token = a.name;
            node.data.lhs = @intFromEnum(a.pattern);
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an expression node (literal, variable, call, record, etc.) and returns its index.
pub fn addExpr(store: *NodeStore, expr: AST.Expr) std.mem.Allocator.Error!AST.Expr.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = AST.TokenizedRegion.empty(),
    };
    switch (expr) {
        .int => |e| {
            node.tag = .int;
            node.region = e.region;
            node.main_token = e.token;
            node.data.lhs = @intFromEnum(e.literal);
        },
        .frac => |e| {
            node.tag = .frac;
            node.region = e.region;
            node.main_token = e.token;
            node.data.lhs = @intFromEnum(e.literal);
        },
        .typed_int => |e| {
            node.tag = .typed_int;
            node.region = e.region;
            node.main_token = e.token;
            node.data.lhs = @bitCast(e.type_ident);
            node.data.rhs = @intFromEnum(e.literal);
        },
        .typed_frac => |e| {
            node.tag = .typed_frac;
            node.region = e.region;
            node.main_token = e.token;
            node.data.lhs = @bitCast(e.type_ident);
            node.data.rhs = @intFromEnum(e.literal);
        },
        .tag => |e| {
            node.tag = .tag;
            node.region = e.region;
            node.main_token = e.token;
            if (e.qualifiers.span.len > 0) {
                node.data.lhs = e.qualifiers.span.start;
                node.data.rhs = e.qualifiers.span.len;
            }
        },
        .single_quote => |e| {
            node.tag = .single_quote;
            node.region = e.region;
            node.main_token = e.token;
        },
        .string_part => |e| {
            node.tag = .string_part;
            node.region = e.region;
            node.main_token = e.token;
        },
        .string => |e| {
            node.tag = .string;
            node.region = e.region;
            node.main_token = e.token;
            node.data.lhs = e.parts.span.start;
            node.data.rhs = e.parts.span.len;
        },
        .multiline_string => |e| {
            node.tag = .multiline_string;
            node.region = e.region;
            node.main_token = e.token;
            node.data.lhs = e.parts.span.start;
            node.data.rhs = e.parts.span.len;
        },
        .typed_string => |e| {
            node.tag = .typed_string;
            node.region = e.region;
            node.main_token = e.token;
            node.data.lhs = @bitCast(e.type_ident);
            const parts_data_idx = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, e.parts.span.start);
            try store.extra_data.append(store.gpa, e.parts.span.len);
            node.data.rhs = @as(u32, @intCast(parts_data_idx));
        },
        .typed_multiline_string => |e| {
            node.tag = .typed_multiline_string;
            node.region = e.region;
            node.main_token = e.token;
            node.data.lhs = @bitCast(e.type_ident);
            const parts_data_idx = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, e.parts.span.start);
            try store.extra_data.append(store.gpa, e.parts.span.len);
            node.data.rhs = @as(u32, @intCast(parts_data_idx));
        },
        .list => |l| {
            node.tag = .list;
            node.region = l.region;
            node.main_token = l.region.start;
            node.data.lhs = l.items.span.start;
            node.data.rhs = l.items.span.len;
        },
        .tuple => |t| {
            node.tag = .tuple;
            node.region = t.region;
            node.data.lhs = t.items.span.start;
            node.data.rhs = t.items.span.len;
        },
        .record => |r| {
            node.tag = .record;
            node.region = r.region;

            // Store all record data in flat format:
            // [fields.span.start, fields.span.len, ext_or_zero]
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, r.fields.span.start);
            try store.extra_data.append(store.gpa, r.fields.span.len);

            // Store ext value or 0 for null
            const ext_value = if (r.ext) |ext| @intFromEnum(ext) else 0;
            try store.extra_data.append(store.gpa, ext_value);

            node.data.lhs = data_start;
            node.data.rhs = 0; // Not used
        },
        .lambda => |l| {
            node.tag = .lambda;
            node.region = l.region;
            node.data.lhs = l.args.span.start;
            node.data.rhs = l.args.span.len;
            const body_idx = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, @intFromEnum(l.body));
            node.main_token = @as(u32, @intCast(body_idx));
        },
        .apply => |app| {
            node.tag = .apply;
            node.region = app.region;
            node.data.lhs = app.args.span.start;
            node.data.rhs = app.args.span.len;
            const fn_ed_idx = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, @intFromEnum(app.@"fn"));
            node.main_token = @as(u32, @intCast(fn_ed_idx));
        },
        .record_updater => |updater| {
            node.tag = .record_update;
            node.region = updater.region;
            node.main_token = updater.token;
        },
        .field_access => |fa| {
            node.tag = .field_access;
            node.region = fa.region;
            node.main_token = fa.operator;
            node.data.lhs = @intFromEnum(fa.left);
            node.data.rhs = @intFromEnum(fa.right);
        },
        .method_call => |mc| {
            node.tag = .method_call;
            node.region = mc.region;
            node.main_token = mc.method_token;
            node.data.lhs = @intFromEnum(mc.receiver);
            const args_data_idx = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, mc.args.span.start);
            try store.extra_data.append(store.gpa, mc.args.span.len);
            node.data.rhs = @as(u32, @intCast(args_data_idx));
        },
        .tuple_access => |ta| {
            node.tag = .tuple_access;
            node.region = ta.region;
            node.main_token = ta.elem_token;
            node.data.lhs = @intFromEnum(ta.expr);
        },
        .arrow_call => |ld| {
            node.tag = .arrow_call;
            node.region = ld.region;
            node.main_token = ld.operator;
            node.data.lhs = @intFromEnum(ld.left);
            node.data.rhs = @intFromEnum(ld.right);
        },
        .bin_op => |op| {
            node.tag = .bin_op;
            node.region = op.region;
            node.main_token = op.operator;
            node.data.lhs = @intFromEnum(op.left);
            node.data.rhs = @intFromEnum(op.right);
        },
        .suffix_single_question => |op| {
            node.tag = .suffix_single_question;
            node.region = op.region;
            node.main_token = op.operator;
            node.data.lhs = @intFromEnum(op.expr);
        },
        .unary_op => |u| {
            node.tag = .unary_op;
            node.region = u.region;
            node.main_token = u.operator;
            node.data.lhs = @intFromEnum(u.expr);
        },
        .if_then_else => |i| {
            node.tag = .if_then_else;
            node.region = i.region;
            node.data.lhs = @intFromEnum(i.condition);
            node.data.rhs = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, @intFromEnum(i.then));
            try store.extra_data.append(store.gpa, @intFromEnum(i.@"else"));
        },
        .if_without_else => |i| {
            node.tag = .if_without_else;
            node.region = i.region;
            node.data.lhs = @intFromEnum(i.condition);
            node.data.rhs = @intFromEnum(i.then);
        },
        .match => |m| {
            node.tag = .match;
            node.region = m.region;
            node.data.lhs = m.branches.span.start;
            node.data.rhs = m.branches.span.len;
            const expr_idx = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, @intFromEnum(m.expr));
            node.main_token = @as(u32, @intCast(expr_idx));
        },
        .ident => |id| {
            node.tag = .ident;
            node.region = id.region;
            node.main_token = id.token;
            if (id.qualifiers.span.len > 0) {
                node.data.lhs = id.qualifiers.span.start;
                node.data.rhs = id.qualifiers.span.len;
            }
        },
        .dbg => |d| {
            node.tag = .dbg;
            node.region = d.region;
            node.data.lhs = @intFromEnum(d.expr);
        },
        .record_builder => |rb| {
            node.tag = .record_builder;
            node.region = rb.region;
            // Store [fields.span.start, fields.span.len, mapper] in extra_data
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, rb.fields.span.start);
            try store.extra_data.append(store.gpa, rb.fields.span.len);
            try store.extra_data.append(store.gpa, @intFromEnum(rb.mapper));
            node.data.lhs = data_start;
        },
        .nominal_record => |nr| {
            node.tag = .nominal_record;
            node.region = nr.region;
            node.data.lhs = @intFromEnum(nr.mapper);
            node.data.rhs = @intFromEnum(nr.backing);
        },
        .nominal_apply => |na| {
            node.tag = .nominal_apply;
            node.region = na.region;
            node.data.lhs = @intFromEnum(na.mapper);
            const args_data_idx = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, na.args.span.start);
            try store.extra_data.append(store.gpa, na.args.span.len);
            node.data.rhs = @as(u32, @intCast(args_data_idx));
        },
        .block => |body| {
            node.tag = .block;
            node.region = body.region;
            node.main_token = @intFromEnum(body.scope);
            node.data.lhs = body.statements.span.start;
            node.data.rhs = body.statements.span.len;
        },
        .for_expr => |f| {
            node.tag = .for_expr;
            node.region = f.region;
            node.main_token = @intFromEnum(f.patt);
            node.data.lhs = @intFromEnum(f.expr);
            node.data.rhs = @intFromEnum(f.body);
        },
        .@"break" => |b| {
            node.tag = .break_expr;
            node.region = b.region;
        },
        .@"return" => |r| {
            node.tag = .return_expr;
            node.region = r.region;
            node.data.lhs = @intFromEnum(r.expr);
        },
        .ellipsis => |e| {
            node.tag = .ellipsis;
            node.region = e.region;
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn addPatternRecordField(store: *NodeStore, field: AST.PatternRecordField) std.mem.Allocator.Error!AST.PatternRecordField.Idx {
    var node = Node{
        .tag = .record_field_patt,
        .main_token = field.name orelse 0,
        .data = .{
            .lhs = @intFromBool(field.rest),
            .rhs = 0,
        },
        .region = field.region,
    };
    if (field.value) |value| {
        node.data.rhs = @intFromEnum(value);
    }
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn getPatternRecordField(store: *const NodeStore, field: AST.PatternRecordField.Idx) AST.PatternRecordField {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(field)));
    return .{
        .name = if (node.main_token == 0) null else node.main_token,
        .value = if (node.data.rhs == 0) null else @enumFromInt(node.data.rhs),
        .rest = node.data.lhs == 1,
        .region = node.region,
    };
}

/// TODO
pub fn addRecordField(store: *NodeStore, field: AST.RecordField) std.mem.Allocator.Error!AST.RecordField.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = field.region,
    };
    node.tag = .record_field;
    node.main_token = field.name;
    if (field.value) |v| {
        node.data.lhs = @intFromEnum(v);
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn addMatchBranch(store: *NodeStore, branch: AST.MatchBranch) std.mem.Allocator.Error!AST.MatchBranch.Idx {
    const node = Node{
        .tag = .branch,
        .main_token = try packOptionalIndex(branch.guard),
        .data = .{
            .lhs = @intFromEnum(branch.pattern),
            .rhs = @intFromEnum(branch.body),
        },
        .region = branch.region,
    };

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn addTypeHeader(store: *NodeStore, header: AST.TypeHeader) std.mem.Allocator.Error!AST.TypeHeader.Idx {
    var node = Node{
        .tag = .ty_header,
        .main_token = header.name,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = header.region,
    };

    node.data.lhs = header.args.span.start;
    node.data.rhs = header.args.span.len;

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn addAnnoRecordField(store: *NodeStore, field: AST.AnnoRecordField) std.mem.Allocator.Error!AST.AnnoRecordField.Idx {
    const node = Node{
        .tag = .ty_record_field,
        .main_token = 0,
        .data = .{
            .lhs = field.name,
            .rhs = @intFromEnum(field.ty),
        },
        .region = field.region,
    };

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a WhereClause node to the store, returning a type-safe index to the node.
pub fn addWhereClause(store: *NodeStore, clause: AST.WhereClause) std.mem.Allocator.Error!AST.WhereClause.Idx {
    var node = Node{
        .tag = .where_mod_method,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (clause) {
        .mod_method => |c| {
            node.tag = .where_mod_method;
            node.region = c.region;
            node.main_token = c.var_tok;
            const ed_start = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, c.name_tok);
            try store.extra_data.append(store.gpa, @intFromEnum(c.args));
            try store.extra_data.append(store.gpa, @intFromEnum(c.ret_anno));
            node.data.lhs = @intCast(ed_start);
        },
        .mod_alias => |c| {
            node.tag = .where_mod_alias;
            node.region = c.region;
            node.main_token = c.var_tok;
            node.data.lhs = c.name_tok;
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn addTypeAnno(store: *NodeStore, anno: AST.TypeAnno) std.mem.Allocator.Error!AST.TypeAnno.Idx {
    var node = Node{
        .tag = .branch,
        .main_token = 0,
        .data = .{
            .lhs = 0,
            .rhs = 0,
        },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (anno) {
        .apply => |a| {
            node.tag = .ty_apply;
            node.region = a.region;
            node.data.lhs = a.args.span.start;
            node.data.rhs = a.args.span.len;
        },
        .ty_var => |v| {
            node.tag = .ty_var;
            node.region = v.region;
            node.main_token = v.tok;
        },
        .underscore_type_var => |utv| {
            node.tag = .ty_underscore_var;
            node.region = utv.region;
            node.main_token = utv.tok;
        },
        .underscore => |u| {
            node.tag = .ty_underscore;
            node.region = u.region;
        },
        .ty => |t| {
            node.tag = .ty_ty;
            node.region = t.region;
            node.main_token = t.token;
            node.data.lhs = t.qualifiers.span.start;
            node.data.rhs = t.qualifiers.span.len;
        },
        .tag_union => |tu| {
            node.tag = .ty_union;
            node.region = tu.region;

            // Store all tag_union data in flat format:
            // [tags.span.start, tags.span.len, ext_data...]
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, tu.tags.span.start);
            try store.extra_data.append(store.gpa, tu.tags.span.len);

            // ext_kind: 0 = closed, 1 = anonymous open, 2 = named open
            const ext_kind: u2 = switch (tu.ext) {
                .closed => 0,
                .open => 1,
                .named => 2,
            };
            const rhs = AST.TypeAnno.TagUnionRhs{
                .ext_kind = ext_kind,
            };
            switch (tu.ext) {
                .named => |named| {
                    try store.extra_data.append(store.gpa, @intFromEnum(named.anno));
                    try store.extra_data.append(store.gpa, named.region.start);
                    try store.extra_data.append(store.gpa, named.region.end);
                },
                .open => |double_dot_tok| {
                    try store.extra_data.append(store.gpa, double_dot_tok);
                },
                .closed => {},
            }

            node.data.lhs = data_start;
            node.data.rhs = @as(u32, @bitCast(rhs));
        },
        .tuple => |t| {
            node.tag = .ty_tuple;
            node.region = t.region;
            node.data.lhs = t.annos.span.start;
            node.data.rhs = t.annos.span.len;
        },
        .record => |r| {
            node.tag = .ty_record;
            node.region = r.region;

            // Store all data in extra_data:
            // [fields.span.start, fields.span.len, ext_data...]
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, r.fields.span.start);
            try store.extra_data.append(store.gpa, r.fields.span.len);

            // ext_kind: 0 = closed, 1 = anonymous open, 2 = named open
            const ext_kind: u2 = switch (r.ext) {
                .closed => 0,
                .open => 1,
                .named => 2,
            };
            const rhs = AST.TypeAnno.RecordRhs{
                .ext_kind = ext_kind,
            };
            switch (r.ext) {
                .named => |named| {
                    try store.extra_data.append(store.gpa, @intFromEnum(named.anno));
                    try store.extra_data.append(store.gpa, named.region.start);
                    try store.extra_data.append(store.gpa, named.region.end);
                },
                .open => |double_dot_tok| {
                    try store.extra_data.append(store.gpa, double_dot_tok);
                },
                .closed => {},
            }

            node.data.lhs = data_start;
            node.data.rhs = @as(u32, @bitCast(rhs));
        },
        .@"fn" => |f| {
            node.tag = .ty_fn;
            node.region = f.region;
            node.data.lhs = f.args.span.start;
            node.data.rhs = @bitCast(AST.TypeAnno.TypeAnnoFnRhs{
                .effectful = @intFromBool(f.effectful),
                .args_len = @intCast(f.args.span.len), // We hope a function has less than 2.147b args
            });
            const ret_idx = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, @intFromEnum(f.ret));
            node.main_token = @intCast(ret_idx);
        },
        .parens => |p| {
            node.tag = .ty_parens;
            node.region = p.region;
            node.data.lhs = @intFromEnum(p.anno);
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn getFile(store: *const NodeStore) AST.File {
    const node = store.nodes.get(root_node_idx);
    const header_ed_idx = @as(usize, @intCast(node.data.lhs + node.data.rhs));
    const header = store.extra_data.items[header_ed_idx];
    return .{
        .header = @enumFromInt(header),
        .statements = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
        .scope = @enumFromInt(node.main_token),
        .region = node.region,
    };
}

/// Retrieves collection data from a stored collection node.
pub fn getCollection(store: *const NodeStore, collection_idx: AST.Collection.Idx) AST.Collection {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(collection_idx)));
    return .{
        .span = .{
            .start = node.data.lhs,
            .len = node.data.rhs,
        },
        .region = node.region,
    };
}

/// Retrieves header data from a stored header node, reconstructing the appropriate header type.
pub fn getHeader(store: *const NodeStore, header_idx: AST.Header.Idx) AST.Header {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    switch (node.tag) {
        .app_header => {
            return .{ .app = .{
                .platform_idx = @enumFromInt(node.main_token),
                .provides = @enumFromInt(node.data.lhs),
                .packages = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .module_header => {
            return .{ .module = .{
                .exposes = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .hosted_header => {
            return .{ .hosted = .{
                .exposes = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .package_header => {
            return .{ .package = .{
                .exposes = @enumFromInt(node.data.lhs),
                .packages = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .platform_header => {
            const ed_start = node.data.lhs;
            std.debug.assert(node.data.rhs == 9);

            const targets_val = store.extra_data.items[ed_start + 8];
            const targets = unpackOptionalIndex(AST.TargetsSection.Idx, targets_val);

            return .{ .platform = .{
                .name = node.main_token,
                .requires_entries = .{ .span = .{
                    .start = store.extra_data.items[ed_start],
                    .len = store.extra_data.items[ed_start + 1],
                } },
                .exposes = @enumFromInt(store.extra_data.items[ed_start + 2]),
                .packages = @enumFromInt(store.extra_data.items[ed_start + 3]),
                .provides = .{ .span = .{
                    .start = store.extra_data.items[ed_start + 4],
                    .len = store.extra_data.items[ed_start + 5],
                } },
                .hosted = .{ .span = .{
                    .start = store.extra_data.items[ed_start + 6],
                    .len = store.extra_data.items[ed_start + 7],
                } },
                .targets = targets,
                .region = node.region,
            } };
        },
        .type_module_header => {
            return .{ .type_module = .{
                .region = node.region,
            } };
        },
        .default_app_header => {
            return .{ .default_app = .{
                .main_fn_idx = node.main_token,
                .region = node.region,
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid header tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves exposed item data from a stored exposed item node.
pub fn getExposedItem(store: *const NodeStore, exposed_item_idx: AST.ExposedItem.Idx) AST.ExposedItem {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(exposed_item_idx)));
    switch (node.tag) {
        .exposed_item_lower => {
            if (node.data.rhs == 1) {
                return .{ .lower_ident = .{
                    .region = node.region,
                    .ident = node.main_token,
                    .as = node.data.lhs,
                } };
            }
            return .{ .lower_ident = .{
                .region = node.region,
                .ident = node.main_token,
                .as = null,
            } };
        },
        .exposed_item_upper => {
            if (node.data.rhs == 1) {
                return .{ .upper_ident = .{
                    .region = node.region,
                    .ident = node.main_token,
                    .as = node.data.lhs,
                } };
            }
            return .{ .upper_ident = .{
                .region = node.region,
                .ident = node.main_token,
                .as = null,
            } };
        },
        .exposed_item_upper_star => {
            return .{ .upper_ident_star = .{
                .region = node.region,
                .ident = node.main_token,
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid exposed item tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves statement data from a stored statement node, reconstructing the appropriate statement type.
pub fn getStatement(store: *const NodeStore, statement_idx: AST.Statement.Idx) AST.Statement {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(statement_idx)));
    switch (node.tag) {
        .decl => {
            return .{ .decl = .{
                .pattern = @enumFromInt(node.data.lhs),
                .body = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .@"var" => {
            return .{ .@"var" = .{
                .name = node.main_token,
                .body = unpackOptionalIndex(AST.Expr.Idx, node.data.lhs),
                .region = node.region,
            } };
        },
        .expr => {
            return .{ .expr = .{
                .expr = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .import => {
            const rhs = @as(AST.ImportRhs, @bitCast(node.data.rhs));

            // Read flat data format: [exposes.span.start, exposes.span.len, qualifier_tok?, alias_tok?]
            var extra_data_pos = node.data.lhs;
            const exposes_start = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;
            const exposes_len = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;

            var qualifier_tok: ?Token.Idx = null;
            var alias_tok: ?Token.Idx = null;
            if (rhs.qualified == 1) {
                qualifier_tok = store.extra_data.items[extra_data_pos];
                extra_data_pos += 1;
            }
            if (rhs.aliased == 1) {
                alias_tok = store.extra_data.items[extra_data_pos];
            }

            return AST.Statement{ .import = .{
                .module_name_tok = node.main_token,
                .qualifier_tok = qualifier_tok,
                .alias_tok = alias_tok,
                .exposes = .{ .span = .{
                    .start = exposes_start,
                    .len = exposes_len,
                } },
                .nested_import = false,
                .region = node.region,
            } };
        },
        .expect => {
            return .{ .expect = .{
                .body = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .@"for" => {
            return .{ .@"for" = .{
                .patt = @enumFromInt(node.main_token),
                .expr = @enumFromInt(node.data.lhs),
                .body = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .@"while" => {
            return .{ .@"while" = .{
                .cond = @enumFromInt(node.data.lhs),
                .body = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .crash => {
            return .{ .crash = .{
                .expr = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .dbg => {
            return .{ .dbg = .{
                .expr = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .@"return" => {
            return .{ .@"return" = .{
                .expr = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .@"break" => {
            return .{ .@"break" = .{
                .region = node.region,
            } };
        },
        .type_decl => {
            const extra = store.decodeTypeDeclExtra(node.main_token);

            return .{ .type_decl = .{
                .region = node.region,
                .header = @enumFromInt(node.data.lhs),
                .anno = @enumFromInt(node.data.rhs),
                .kind = .alias,
                .where = extra.where,
                .associated = extra.associated,
            } };
        },
        .type_decl_nominal => {
            const extra = store.decodeTypeDeclExtra(node.main_token);

            return .{ .type_decl = .{
                .region = node.region,
                .header = @enumFromInt(node.data.lhs),
                .anno = @enumFromInt(node.data.rhs),
                .kind = .nominal,
                .where = extra.where,
                .associated = extra.associated,
            } };
        },
        .type_decl_opaque => {
            const extra = store.decodeTypeDeclExtra(node.main_token);

            return .{ .type_decl = .{
                .region = node.region,
                .header = @enumFromInt(node.data.lhs),
                .anno = @enumFromInt(node.data.rhs),
                .kind = .@"opaque",
                .where = extra.where,
                .associated = extra.associated,
            } };
        },
        .type_anno => {
            var where_clause: ?AST.Collection.Idx = null;
            var is_var = false;
            if (node.main_token != 0) {
                const extra_start = extraDataStart(node.main_token);
                if (store.extra_data.items[extra_start] != 0) {
                    where_clause = @enumFromInt(store.extra_data.items[extra_start + 1]);
                }
                is_var = store.extra_data.items[extra_start + 2] != 0;
            }
            return .{ .type_anno = .{
                .region = node.region,
                .name = node.data.lhs,
                .anno = @enumFromInt(node.data.rhs),
                .where = where_clause,
                .is_var = is_var,
            } };
        },
        .file_import => {
            const is_bytes = (node.data.rhs & (@as(u32, 1) << 31)) != 0;
            const type_tok = node.data.rhs & ~(@as(u32, 1) << 31);
            return .{ .file_import = .{
                .path_tok = node.main_token,
                .name_tok = node.data.lhs,
                .type_tok = type_tok,
                .is_bytes = is_bytes,
                .region = node.region,
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid statement tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves pattern data from a stored pattern node, reconstructing the appropriate pattern type.
pub fn getPattern(store: *const NodeStore, pattern_idx: AST.Pattern.Idx) AST.Pattern {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(pattern_idx)));
    switch (node.tag) {
        .ident_patt => {
            return .{ .ident = .{
                .ident_tok = node.main_token,
                .region = node.region,
            } };
        },
        .var_ident_patt => {
            return .{ .var_ident = .{
                .ident_tok = node.main_token,
                .region = node.region,
            } };
        },
        .tag_patt => {
            const args_start = node.data.lhs;

            const ed_start = @as(usize, @intCast(node.data.rhs));
            const args_len = store.extra_data.items[ed_start];
            const qualifiers_start = store.extra_data.items[ed_start + 1];
            const qualifiers_len = store.extra_data.items[ed_start + 2];
            const backing_value = store.extra_data.items[ed_start + 3] != 0;

            return .{ .tag = .{
                .tag_tok = node.main_token,
                .args = .{ .span = .{
                    .start = args_start,
                    .len = args_len,
                } },
                .qualifiers = .{ .span = .{
                    .start = qualifiers_start,
                    .len = qualifiers_len,
                } },
                .backing_value = backing_value,
                .region = node.region,
            } };
        },
        .string_patt => {
            return .{ .string = .{
                .string_tok = node.main_token,
                .region = node.region,
                .parts = .{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
            } };
        },
        .single_quote_patt => {
            return .{ .single_quote = .{
                .token = node.main_token,
                .region = node.region,
            } };
        },
        .int_patt => {
            return .{ .int = .{
                .number_tok = node.main_token,
                .literal = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .frac_patt => {
            return .{ .frac = .{
                .number_tok = node.main_token,
                .literal = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .typed_int_patt => {
            return .{ .typed_int = .{
                .number_tok = node.main_token,
                .type_ident = @bitCast(node.data.lhs),
                .literal = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .typed_frac_patt => {
            return .{ .typed_frac = .{
                .number_tok = node.main_token,
                .type_ident = @bitCast(node.data.lhs),
                .literal = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .record_patt => {
            return .{ .record = .{
                .region = node.region,
                .fields = .{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
            } };
        },
        .list_patt => {
            return .{ .list = .{
                .region = node.region,
                .patterns = .{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
            } };
        },
        .list_rest_patt => {
            return .{ .list_rest = .{
                .region = node.region,
                .name = if (node.data.lhs == 0) null else node.data.lhs,
            } };
        },
        .tuple_patt => {
            return .{ .tuple = .{
                .region = node.region,
                .patterns = .{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
            } };
        },
        .alternatives_patt => {
            return .{ .alternatives = .{
                .region = node.region,
                .patterns = .{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
            } };
        },
        .underscore_patt => {
            return .{ .underscore = .{
                .region = node.region,
            } };
        },
        .as_patt => {
            return .{ .as = .{
                .region = node.region,
                .name = node.main_token,
                .pattern = @enumFromInt(node.data.lhs),
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid pattern tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves expression data from a stored expression node, reconstructing the appropriate expression type.
pub fn getExpr(store: *const NodeStore, expr_idx: AST.Expr.Idx) AST.Expr {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(expr_idx)));
    switch (node.tag) {
        .int => {
            return .{ .int = .{
                .token = node.main_token,
                .literal = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .frac => {
            return .{ .frac = .{
                .token = node.main_token,
                .literal = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .typed_int => {
            return .{ .typed_int = .{
                .token = node.main_token,
                .type_ident = @bitCast(node.data.lhs),
                .literal = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .typed_frac => {
            return .{ .typed_frac = .{
                .token = node.main_token,
                .type_ident = @bitCast(node.data.lhs),
                .literal = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .single_quote => {
            return .{ .single_quote = .{
                .token = node.main_token,
                .region = node.region,
            } };
        },
        .ident => {
            // Retrieve qualifier span from stored data
            var qualifiers_span = Token.Span{ .span = .{ .start = 0, .len = 0 } };
            if (node.data.rhs > 0) {
                qualifiers_span = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } };
            }

            return .{ .ident = .{
                .token = node.main_token,
                .qualifiers = qualifiers_span,
                .region = node.region,
            } };
        },
        .tag => {
            // Retrieve qualifier span from stored data
            var qualifiers_span = Token.Span{ .span = .{ .start = 0, .len = 0 } };
            if (node.data.rhs > 0) {
                qualifiers_span = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } };
            }

            return .{ .tag = .{
                .token = node.main_token,
                .qualifiers = qualifiers_span,
                .region = node.region,
            } };
        },
        .string_part => {
            return .{ .string_part = .{
                .region = node.region,
                .token = node.main_token,
            } };
        },
        .string => {
            return .{ .string = .{
                .token = node.main_token,
                .parts = .{ .span = base.DataSpan{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
                .region = node.region,
            } };
        },
        .typed_string => {
            const parts_data_idx = node.data.rhs;
            return .{ .typed_string = .{
                .token = node.main_token,
                .type_ident = @bitCast(node.data.lhs),
                .parts = .{ .span = .{
                    .start = store.extra_data.items[parts_data_idx],
                    .len = store.extra_data.items[parts_data_idx + 1],
                } },
                .region = node.region,
            } };
        },
        .typed_multiline_string => {
            const parts_data_idx = node.data.rhs;
            return .{ .typed_multiline_string = .{
                .token = node.main_token,
                .type_ident = @bitCast(node.data.lhs),
                .parts = .{ .span = .{
                    .start = store.extra_data.items[parts_data_idx],
                    .len = store.extra_data.items[parts_data_idx + 1],
                } },
                .region = node.region,
            } };
        },
        .multiline_string => {
            return .{ .multiline_string = .{
                .token = node.main_token,
                .parts = .{ .span = base.DataSpan{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
                .region = node.region,
            } };
        },
        .list => {
            return .{ .list = .{
                .items = .{ .span = base.DataSpan{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
                .region = node.region,
            } };
        },
        .tuple => {
            return .{ .tuple = .{
                .items = .{ .span = base.DataSpan{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
                .region = node.region,
            } };
        },
        .record => {
            const extra_data_pos = node.data.lhs;
            const fields_start = store.extra_data.items[extra_data_pos];
            const fields_len = store.extra_data.items[extra_data_pos + 1];
            const ext_value = store.extra_data.items[extra_data_pos + 2];

            // Convert 0 back to null, otherwise create the Idx
            const ext = if (ext_value == 0) null else @as(AST.Expr.Idx, @enumFromInt(ext_value));

            return .{ .record = .{
                .fields = .{ .span = .{
                    .start = fields_start,
                    .len = fields_len,
                } },
                .ext = ext,
                .region = node.region,
            } };
        },
        .record_update => {
            return .{ .record_updater = .{
                .token = node.main_token,
                .region = node.region,
            } };
        },
        .field_access => {
            return .{ .field_access = .{
                .left = @enumFromInt(node.data.lhs),
                .right = @enumFromInt(node.data.rhs),
                .operator = node.main_token,
                .region = node.region,
            } };
        },
        .method_call => {
            const args_data_idx = node.data.rhs;
            return .{ .method_call = .{
                .receiver = @enumFromInt(node.data.lhs),
                .method_token = node.main_token,
                .args = .{ .span = .{
                    .start = store.extra_data.items[args_data_idx],
                    .len = store.extra_data.items[args_data_idx + 1],
                } },
                .region = node.region,
            } };
        },
        .tuple_access => {
            return .{ .tuple_access = .{
                .expr = @enumFromInt(node.data.lhs),
                .elem_token = node.main_token,
                .region = node.region,
            } };
        },
        .arrow_call => {
            return .{ .arrow_call = .{
                .left = @enumFromInt(node.data.lhs),
                .right = @enumFromInt(node.data.rhs),
                .operator = node.main_token,
                .region = node.region,
            } };
        },
        .lambda => {
            return .{ .lambda = .{
                .body = @enumFromInt(store.extra_data.items[node.main_token]),
                .args = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
                .region = node.region,
            } };
        },
        .apply => {
            return .{ .apply = .{
                .@"fn" = @enumFromInt(store.extra_data.items[node.main_token]),
                .args = .{ .span = base.DataSpan{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
                .region = node.region,
            } };
        },
        .suffix_single_question => {
            return .{ .suffix_single_question = .{
                .region = node.region,
                .operator = node.main_token,
                .expr = @enumFromInt(node.data.lhs),
            } };
        },
        .if_then_else => {
            const then_idx = @as(usize, @intCast(node.data.rhs));
            const else_idx = then_idx + 1;
            const then_ed = store.extra_data.items[then_idx];
            const else_ed = store.extra_data.items[else_idx];
            return .{ .if_then_else = .{
                .region = node.region,
                .condition = @enumFromInt(node.data.lhs),
                .then = @enumFromInt(then_ed),
                .@"else" = @enumFromInt(else_ed),
            } };
        },
        .if_without_else => {
            return .{ .if_without_else = .{
                .region = node.region,
                .condition = @enumFromInt(node.data.lhs),
                .then = @enumFromInt(node.data.rhs),
            } };
        },
        .match => {
            return .{ .match = .{
                .region = node.region,
                .expr = @enumFromInt(store.extra_data.items[node.main_token]),
                .branches = .{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
            } };
        },
        .dbg => {
            return .{ .dbg = .{
                .region = node.region,
                .expr = @enumFromInt(node.data.lhs),
            } };
        },
        .bin_op => {
            return .{ .bin_op = .{
                .left = @enumFromInt(node.data.lhs),
                .right = @enumFromInt(node.data.rhs),
                .operator = node.main_token,
                .region = node.region,
            } };
        },
        .block => {
            const statements = AST.Statement.Span{ .span = .{
                .start = node.data.lhs,
                .len = node.data.rhs,
            } };
            return .{ .block = .{
                .statements = statements,
                .scope = @enumFromInt(node.main_token),
                .region = node.region,
            } };
        },
        .ellipsis => {
            return .{ .ellipsis = .{
                .region = node.region,
            } };
        },
        .for_expr => {
            return .{ .for_expr = .{
                .patt = @enumFromInt(node.main_token),
                .expr = @enumFromInt(node.data.lhs),
                .body = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .break_expr => {
            return .{ .@"break" = .{
                .region = node.region,
            } };
        },
        .return_expr => {
            return .{ .@"return" = .{
                .expr = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        .record_builder => {
            const extra_data_pos = node.data.lhs;
            const fields_start = store.extra_data.items[extra_data_pos];
            const fields_len = store.extra_data.items[extra_data_pos + 1];
            const mapper_value = store.extra_data.items[extra_data_pos + 2];
            return .{ .record_builder = .{
                .mapper = @enumFromInt(mapper_value),
                .fields = .{ .span = .{
                    .start = fields_start,
                    .len = fields_len,
                } },
                .region = node.region,
            } };
        },
        .nominal_record => {
            return .{ .nominal_record = .{
                .mapper = @enumFromInt(node.data.lhs),
                .backing = @enumFromInt(node.data.rhs),
                .region = node.region,
            } };
        },
        .nominal_apply => {
            const args_data_idx = @as(usize, @intCast(node.data.rhs));
            return .{ .nominal_apply = .{
                .mapper = @enumFromInt(node.data.lhs),
                .args = .{ .span = .{
                    .start = store.extra_data.items[args_data_idx],
                    .len = store.extra_data.items[args_data_idx + 1],
                } },
                .region = node.region,
            } };
        },
        .unary_op => {
            return .{ .unary_op = .{
                .operator = node.main_token,
                .expr = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid expr tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves record field data from a stored record field node.
pub fn getRecordField(store: *const NodeStore, field_idx: AST.RecordField.Idx) AST.RecordField {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(field_idx)));
    const name = node.main_token;
    const value: ?AST.Expr.Idx = if (node.tag == .malformed) null else if (node.data.lhs > 0) @enumFromInt(node.data.lhs) else null;

    return .{
        .name = name,
        .value = value,
        .region = node.region,
    };
}

/// Retrieves match branch data from a stored match branch node.
pub fn getBranch(store: *const NodeStore, branch_idx: AST.MatchBranch.Idx) AST.MatchBranch {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(branch_idx)));
    return .{
        .region = node.region,
        .pattern = @enumFromInt(node.data.lhs),
        .body = @enumFromInt(node.data.rhs),
        .guard = unpackOptionalIndex(AST.Expr.Idx, node.main_token),
    };
}

/// Retrieves type header data from a stored type header node.
pub fn getTypeHeader(store: *const NodeStore, header_idx: AST.TypeHeader.Idx) error{MalformedNode}!AST.TypeHeader {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));

    if (node.tag != .ty_header) {
        return error.MalformedNode;
    }

    return .{
        .region = node.region,
        .name = node.main_token,
        .args = .{ .span = .{
            .start = node.data.lhs,
            .len = node.data.rhs,
        } },
    };
}

/// Retrieves annotation record field data from a stored annotation record field node.
pub fn getAnnoRecordField(store: *const NodeStore, anno_record_field_idx: AST.AnnoRecordField.Idx) error{MalformedNode}!AST.AnnoRecordField {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(anno_record_field_idx)));

    if (node.tag == .malformed) {
        return error.MalformedNode;
    }

    return .{
        .region = node.region,
        .name = node.data.lhs,
        .ty = @enumFromInt(node.data.rhs),
    };
}

/// Returns the source region for a stored type annotation without reconstructing the full AST union.
pub fn typeAnnoRegion(store: *const NodeStore, ty_anno_idx: AST.TypeAnno.Idx) AST.TokenizedRegion {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(ty_anno_idx)));
    return node.region;
}

/// Returns whether a stored type annotation is malformed without reconstructing the full AST union.
pub fn typeAnnoIsMalformed(store: *const NodeStore, ty_anno_idx: AST.TypeAnno.Idx) bool {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(ty_anno_idx)));
    return node.tag == .malformed;
}

/// Get a WhereClause node from the store, using a type-safe index to the node.
pub fn getWhereClause(store: *const NodeStore, where_clause_idx: AST.WhereClause.Idx) AST.WhereClause {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(where_clause_idx)));
    switch (node.tag) {
        .where_mod_method => {
            const ed_start = @as(usize, @intCast(node.data.lhs));
            const name_tok = store.extra_data.items[ed_start];
            const args = store.extra_data.items[ed_start + 1];
            const ret_anno = store.extra_data.items[ed_start + 2];
            return .{ .mod_method = .{
                .region = node.region,
                .var_tok = node.main_token,
                .name_tok = name_tok,
                .args = @enumFromInt(args),
                .ret_anno = @enumFromInt(ret_anno),
            } };
        },
        .where_mod_alias => {
            return .{ .mod_alias = .{
                .region = node.region,
                .var_tok = node.main_token,
                .name_tok = node.data.lhs,
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid where clause node, found {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves type annotation data from a stored type annotation node, reconstructing the appropriate annotation type.
pub fn getTypeAnno(store: *const NodeStore, ty_anno_idx: AST.TypeAnno.Idx) AST.TypeAnno {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(ty_anno_idx)));

    switch (node.tag) {
        .ty_apply => {
            return .{ .apply = .{
                .region = node.region,
                .args = .{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
            } };
        },
        .ty_var => {
            return .{ .ty_var = .{
                .tok = node.main_token,
                .region = node.region,
            } };
        },
        .ty_underscore_var => {
            return .{ .underscore_type_var = .{
                .tok = node.main_token,
                .region = node.region,
            } };
        },
        .ty_underscore => {
            return .{ .underscore = .{
                .region = node.region,
            } };
        },
        .ty_ty => {
            return .{ .ty = .{
                .token = node.main_token,
                .qualifiers = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
                .region = node.region,
            } };
        },
        .ty_union => {
            const rhs = @as(AST.TypeAnno.TagUnionRhs, @bitCast(node.data.rhs));

            // Read flat data format: [tags.span.start, tags.span.len, ext_data...]
            var extra_data_pos = node.data.lhs;
            const tags_start = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;
            const tags_len = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;

            // ext_kind: 0 = closed, 1 = anonymous open, 2 = named open
            const ext: AST.TypeAnno.TagUnionExt = switch (rhs.ext_kind) {
                0 => .closed,
                1 => .{ .open = store.extra_data.items[extra_data_pos] },
                2 => .{ .named = .{
                    .anno = @enumFromInt(store.extra_data.items[extra_data_pos]),
                    .region = .{
                        .start = store.extra_data.items[extra_data_pos + 1],
                        .end = store.extra_data.items[extra_data_pos + 2],
                    },
                } },
                3 => unreachable,
            };

            return .{ .tag_union = .{
                .region = node.region,
                .ext = ext,
                .tags = .{ .span = .{
                    .start = tags_start,
                    .len = tags_len,
                } },
            } };
        },
        .ty_tuple => {
            return .{ .tuple = .{
                .region = node.region,
                .annos = .{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
            } };
        },
        .ty_record => {
            const rhs = @as(AST.TypeAnno.RecordRhs, @bitCast(node.data.rhs));
            var extra_data_pos = node.data.lhs;
            const fields_start = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;
            const fields_len = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;

            // ext_kind: 0 = closed, 1 = anonymous open, 2 = named open
            const ext: AST.TypeAnno.RecordExt = switch (rhs.ext_kind) {
                0 => .closed,
                1 => .{ .open = store.extra_data.items[extra_data_pos] },
                2 => .{ .named = .{
                    .anno = @enumFromInt(store.extra_data.items[extra_data_pos]),
                    .region = .{
                        .start = store.extra_data.items[extra_data_pos + 1],
                        .end = store.extra_data.items[extra_data_pos + 2],
                    },
                } },
                3 => unreachable,
            };

            return .{ .record = .{
                .region = node.region,
                .fields = .{ .span = .{
                    .start = fields_start,
                    .len = fields_len,
                } },
                .ext = ext,
            } };
        },
        .ty_fn => {
            const rhs = @as(AST.TypeAnno.TypeAnnoFnRhs, @bitCast(node.data.rhs));
            return .{ .@"fn" = .{
                .region = node.region,
                .ret = @enumFromInt(store.extra_data.items[@as(usize, @intCast(node.main_token))]),
                .args = .{ .span = .{
                    .start = node.data.lhs,
                    .len = @intCast(rhs.args_len),
                } },
                .effectful = rhs.effectful == 1,
            } };
        },
        .ty_parens => {
            return .{ .parens = .{
                .region = node.region,
                .anno = @enumFromInt(node.data.lhs),
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        else => {
            // Return a malformed type annotation instead of panicking
            // This handles cases where an invalid node type is encountered
            return .{ .malformed = .{
                .reason = .ty_anno_unexpected_token,
                .region = node.region,
            } };
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Returns the start position for a new Span of AST.Expr.Idxs in scratch
pub fn scratchExprTop(store: *NodeStore) u32 {
    return store.scratch_exprs.top();
}

/// Places a new AST.Expr.Idx in the scratch.
pub fn addScratchExpr(store: *NodeStore, idx: AST.Expr.Idx) std.mem.Allocator.Error!void {
    try store.scratch_exprs.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn exprSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.Expr.Span {
    const end = store.scratch_exprs.top();
    defer store.scratch_exprs.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_exprs.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any ExprIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchExprsFrom(store: *NodeStore, start: u32) void {
    store.scratch_exprs.clearFrom(start);
}

/// Returns a new ExprIter so that the caller can iterate through
/// all items in the span.
pub fn exprSlice(store: *const NodeStore, span: AST.Expr.Span) []AST.Expr.Idx {
    return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
}

/// Returns the start position for a new Span of AST.Statement.Idxs in scratch
pub fn scratchStatementTop(store: *NodeStore) u32 {
    return store.scratch_statements.top();
}

/// Places a new AST.Statement.Idx in the scratch.
pub fn addScratchStatement(store: *NodeStore, idx: AST.Statement.Idx) std.mem.Allocator.Error!void {
    try store.scratch_statements.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn statementSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.Statement.Span {
    const end = store.scratch_statements.top();
    defer store.scratch_statements.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_statements.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any StatementIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchStatementsFrom(store: *NodeStore, start: u32) void {
    store.scratch_statements.clearFrom(start);
}

/// Returns a new Statement slice so that the caller can iterate through
/// all items in the span.
pub fn statementSlice(store: *const NodeStore, span: AST.Statement.Span) []AST.Statement.Idx {
    return store.sliceFromSpan(AST.Statement.Idx, span.span);
}

/// Returns the start position for a new Span of AST.Pattern.Idx in scratch
pub fn scratchPatternTop(store: *NodeStore) u32 {
    return store.scratch_patterns.top();
}

/// Places a new AST.Pattern.Idx in the scratch.
pub fn addScratchPattern(store: *NodeStore, idx: AST.Pattern.Idx) std.mem.Allocator.Error!void {
    try store.scratch_patterns.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn patternSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.Pattern.Span {
    const end = store.scratch_patterns.top();
    defer store.scratch_patterns.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_patterns.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any PatternIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchPatternsFrom(store: *NodeStore, start: u32) void {
    store.scratch_patterns.clearFrom(start);
}

/// Appends a pattern string part and returns its id.
pub fn addPatternStringPart(store: *NodeStore, part: AST.PatternStringPart) std.mem.Allocator.Error!AST.PatternStringPart.Idx {
    const id: AST.PatternStringPart.Idx = @enumFromInt(@as(u32, @intCast(store.pattern_string_parts.items.len)));
    try store.pattern_string_parts.append(store.gpa, part);
    return id;
}

/// Returns a pattern string part by id.
pub fn getPatternStringPart(store: *const NodeStore, idx: AST.PatternStringPart.Idx) AST.PatternStringPart {
    return store.pattern_string_parts.items[@intFromEnum(idx)];
}

/// Returns the start position for a new Span of AST.PatternStringPart.Idx in scratch.
pub fn scratchPatternStringPartTop(store: *NodeStore) u32 {
    return store.scratch_pattern_string_parts.top();
}

/// Places a pattern string part id in scratch.
pub fn addScratchPatternStringPart(store: *NodeStore, idx: AST.PatternStringPart.Idx) std.mem.Allocator.Error!void {
    try store.scratch_pattern_string_parts.append(idx);
}

/// Creates a pattern string part span from scratch.
pub fn patternStringPartSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.PatternStringPart.Span {
    const end = store.scratch_pattern_string_parts.top();
    defer store.scratch_pattern_string_parts.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_pattern_string_parts.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears pattern string part scratch entries from start.
pub fn clearScratchPatternStringPartsFrom(store: *NodeStore, start: u32) void {
    store.scratch_pattern_string_parts.clearFrom(start);
}

/// Returns pattern string part ids for a span.
pub fn patternStringPartSlice(store: *const NodeStore, span: AST.PatternStringPart.Span) []AST.PatternStringPart.Idx {
    return store.sliceFromSpan(AST.PatternStringPart.Idx, span.span);
}

/// Creates a slice corresponding to a span.
pub fn sliceFromSpan(store: *const NodeStore, comptime T: type, span: base.DataSpan) []T {
    if (span.len == 0) return &.{};
    return @ptrCast(store.extra_data.items[span.start..][0..span.len]);
}

/// Returns a new Pattern slice so that the caller can iterate through
/// all items in the span.
pub fn patternSlice(store: *const NodeStore, span: AST.Pattern.Span) []AST.Pattern.Idx {
    return store.sliceFromSpan(AST.Pattern.Idx, span.span);
}

/// Returns a new AST.PatternRecordFieldIter so that the caller can iterate through
/// all items in the span.
pub fn patternRecordFieldSlice(store: *const NodeStore, span: AST.PatternRecordField.Span) []AST.PatternRecordField.Idx {
    return store.sliceFromSpan(AST.PatternRecordField.Idx, span.span);
}
/// Returns the start position for a new Span of patternRecordFieldIdxs in scratch
pub fn scratchPatternRecordFieldTop(store: *NodeStore) u32 {
    return store.scratch_pattern_record_fields.top();
}

/// Places a new AST.PatternRecordField.Idx in the scratch.
pub fn addScratchPatternRecordField(store: *NodeStore, idx: AST.PatternRecordField.Idx) std.mem.Allocator.Error!void {
    try store.scratch_pattern_record_fields.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn patternRecordFieldSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.PatternRecordField.Span {
    const end = store.scratch_pattern_record_fields.top();
    defer store.scratch_pattern_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_pattern_record_fields.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any AST.PatternRecordFieldIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchPatternRecordFieldsFrom(store: *NodeStore, start: u32) void {
    store.scratch_pattern_record_fields.clearFrom(start);
}

/// Returns a new RecordField slice so that the caller can iterate through
/// all items in the span.
pub fn recordFieldSlice(store: *const NodeStore, span: AST.RecordField.Span) []AST.RecordField.Idx {
    return sliceFromSpan(store, AST.RecordField.Idx, span.span);
}
/// Returns the start position for a new Span of recordFieldIdxs in scratch
pub fn scratchRecordFieldTop(store: *NodeStore) u32 {
    return store.scratch_record_fields.top();
}

/// Places a new AST.RecordField.Idx in the scratch.
pub fn addScratchRecordField(store: *NodeStore, idx: AST.RecordField.Idx) std.mem.Allocator.Error!void {
    try store.scratch_record_fields.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn recordFieldSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.RecordField.Span {
    const end = store.scratch_record_fields.top();
    defer store.scratch_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_record_fields.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any RecordFieldIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchRecordFieldsFrom(store: *NodeStore, start: u32) void {
    store.scratch_record_fields.clearFrom(start);
}

/// Returns the start position for a new Span of _LOWER_Idxs in scratch
pub fn scratchMatchBranchTop(store: *NodeStore) u32 {
    return store.scratch_match_branches.top();
}

/// Places a new AST.WhenBranch.Idx in the scratch.
pub fn addScratchMatchBranch(store: *NodeStore, idx: AST.MatchBranch.Idx) std.mem.Allocator.Error!void {
    try store.scratch_match_branches.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn matchBranchSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.MatchBranch.Span {
    const end = store.scratch_match_branches.top();
    defer store.scratch_match_branches.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_match_branches.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any MatchBranchIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchMatchBranchesFrom(store: *NodeStore, start: u32) void {
    store.scratch_match_branches.clearFrom(start);
}

/// Returns a new WhenBranch slice so that the caller can iterate through
/// all items in the span.
pub fn matchBranchSlice(store: *const NodeStore, span: AST.MatchBranch.Span) []AST.MatchBranch.Idx {
    return store.sliceFromSpan(AST.MatchBranch.Idx, span.span);
}

/// Returns the start position for a new Span of typeAnnoIdxs in scratch
pub fn scratchTypeAnnoTop(store: *NodeStore) u32 {
    return store.scratch_type_annos.top();
}

/// Places a new AST.TypeAnno.Idx in the scratch.
pub fn addScratchTypeAnno(store: *NodeStore, idx: AST.TypeAnno.Idx) std.mem.Allocator.Error!void {
    try store.scratch_type_annos.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn typeAnnoSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.TypeAnno.Span {
    const end = store.scratch_type_annos.top();
    defer store.scratch_type_annos.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_type_annos.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any TypeAnnoIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchTypeAnnosFrom(store: *NodeStore, start: u32) void {
    store.scratch_type_annos.clearFrom(start);
}

/// Returns a new TypeAnno slice so that the caller can iterate through
/// all items in the span.
pub fn typeAnnoSlice(store: *const NodeStore, span: AST.TypeAnno.Span) []AST.TypeAnno.Idx {
    return store.sliceFromSpan(AST.TypeAnno.Idx, span.span);
}

/// Returns the start position for a new Span of annoRecordFieldIdxs in scratch
pub fn scratchAnnoRecordFieldTop(store: *NodeStore) u32 {
    return store.scratch_anno_record_fields.top();
}

/// Places a new AST.AnnoRecordField.Idx in the scratch.
pub fn addScratchAnnoRecordField(store: *NodeStore, idx: AST.AnnoRecordField.Idx) std.mem.Allocator.Error!void {
    try store.scratch_anno_record_fields.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn annoRecordFieldSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.AnnoRecordField.Span {
    const end = store.scratch_anno_record_fields.top();
    defer store.scratch_anno_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_anno_record_fields.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any AnnoRecordFieldIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchAnnoRecordFieldsFrom(store: *NodeStore, start: u32) void {
    store.scratch_anno_record_fields.clearFrom(start);
}

/// Returns a new AnnoRecordField slice so that the caller can iterate through
/// all items in the span.
pub fn annoRecordFieldSlice(store: *const NodeStore, span: AST.AnnoRecordField.Span) []AST.AnnoRecordField.Idx {
    return store.sliceFromSpan(AST.AnnoRecordField.Idx, span.span);
}

/// Returns the start position for a new Span of token_Idxs in scratch
pub fn scratchTokenTop(store: *NodeStore) u32 {
    return store.scratch_tokens.top();
}

/// Places a new Token.Idx in the scratch.
pub fn addScratchToken(store: *NodeStore, idx: Token.Idx) std.mem.Allocator.Error!void {
    try store.scratch_tokens.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn tokenSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!Token.Span {
    const end = store.scratch_tokens.top();
    defer store.scratch_tokens.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, store.scratch_tokens.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any TokenIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchTokensFrom(store: *NodeStore, start: u32) void {
    store.scratch_tokens.clearFrom(start);
}

/// Returns a new Token slice so that the caller can iterate through
/// all items in the span.
pub fn tokenSlice(store: *const NodeStore, span: Token.Span) []Token.Idx {
    return store.sliceFromSpan(Token.Idx, span.span);
}

/// Returns the start position for a new Span of exposedItemIdxs in scratch
pub fn scratchExposedItemTop(store: *NodeStore) u32 {
    return store.scratch_exposed_items.top();
}

/// Places a new AST.ExposedItem.Idx in the scratch.
pub fn addScratchExposedItem(store: *NodeStore, idx: AST.ExposedItem.Idx) std.mem.Allocator.Error!void {
    try store.scratch_exposed_items.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn exposedItemSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.ExposedItem.Span {
    const end = store.scratch_exposed_items.top();
    defer store.scratch_exposed_items.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_exposed_items.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any ExposedItemIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchExposedItemsFrom(store: *NodeStore, start: u32) void {
    store.scratch_exposed_items.clearFrom(start);
}

/// Returns a new ExposedItem slice so that the caller can iterate through
/// all items in the span.
pub fn exposedItemSlice(store: *const NodeStore, span: AST.ExposedItem.Span) []AST.ExposedItem.Idx {
    return store.sliceFromSpan(AST.ExposedItem.Idx, span.span);
}

/// Returns the start position for a new Span of whereClauseIdxs in scratch
pub fn scratchWhereClauseTop(store: *NodeStore) u32 {
    return store.scratch_where_clauses.top();
}

/// Places a new AST.WhereClause.Idx in the scratch.
pub fn addScratchWhereClause(store: *NodeStore, idx: AST.WhereClause.Idx) std.mem.Allocator.Error!void {
    try store.scratch_where_clauses.append(idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn whereClauseSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.WhereClause.Span {
    const end = store.scratch_where_clauses.top();
    defer store.scratch_where_clauses.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_where_clauses.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any WhereClauseIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchWhereClausesFrom(store: *NodeStore, start: u32) void {
    store.scratch_where_clauses.clearFrom(start);
}

/// Returns a new WhereClause slice so that the caller can iterate through
/// all items in the span.
pub fn whereClauseSlice(store: *const NodeStore, span: AST.WhereClause.Span) []AST.WhereClause.Idx {
    return store.sliceFromSpan(AST.WhereClause.Idx, span.span);
}

/// Adds a TargetsSection node and returns its index.
pub fn addTargetsSection(store: *NodeStore, section: AST.TargetsSection) std.mem.Allocator.Error!AST.TargetsSection.Idx {
    const node = Node{
        .tag = .targets_section,
        .main_token = section.inputs_dir orelse 0,
        .data = .{
            .lhs = section.entries.span.start,
            .rhs = section.entries.span.len,
        },
        .region = section.region,
    };
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a SymbolMapEntry node and returns its index.
pub fn addSymbolMapEntry(store: *NodeStore, entry: AST.SymbolMapEntry) std.mem.Allocator.Error!AST.SymbolMapEntry.Idx {
    const node = Node{
        .tag = .symbol_map_entry,
        .main_token = entry.symbol,
        .data = .{
            .lhs = if (entry.module) |module| try packNonNullOptionalU32(module) else 0,
            .rhs = entry.func,
        },
        .region = entry.region,
    };
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a TargetEntry node and returns its index.
pub fn addTargetEntry(store: *NodeStore, entry: AST.TargetEntry) std.mem.Allocator.Error!AST.TargetEntry.Idx {
    const node = Node{
        .tag = .target_entry,
        .main_token = entry.target,
        .data = .{
            .lhs = @intFromEnum(entry.config),
            .rhs = 0,
        },
        .region = entry.region,
    };
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a TargetFile node and returns its index.
pub fn addTargetFile(store: *NodeStore, file: AST.TargetFile) std.mem.Allocator.Error!AST.TargetFile.Idx {
    var node = Node{
        .tag = .malformed,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (file) {
        .string_literal => |tok| {
            node.tag = .target_file_string;
            node.main_token = tok;
        },
        .special_ident => |tok| {
            node.tag = .target_file_ident;
            node.main_token = tok;
        },
        .malformed => |m| {
            node.tag = .malformed;
            node.data.lhs = @intFromEnum(m.reason);
            node.region = m.region;
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a TargetConfig node and returns its index.
pub fn addTargetConfig(store: *NodeStore, config: AST.TargetConfig) std.mem.Allocator.Error!AST.TargetConfig.Idx {
    const node = Node{
        .tag = .target_config,
        .main_token = 0,
        .data = .{
            .lhs = config.entries.span.start,
            .rhs = config.entries.span.len,
        },
        .region = config.region,
    };
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a TargetConfigEntry node and returns its index.
pub fn addTargetConfigEntry(store: *NodeStore, entry: AST.TargetConfigEntry) std.mem.Allocator.Error!AST.TargetConfigEntry.Idx {
    const node = Node{
        .tag = .target_config_entry,
        .main_token = entry.name,
        .data = .{
            .lhs = @intFromEnum(entry.value),
            .rhs = 0,
        },
        .region = entry.region,
    };
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a TargetConfigValue node and returns its index.
pub fn addTargetConfigValue(store: *NodeStore, value: AST.TargetConfigValue) std.mem.Allocator.Error!AST.TargetConfigValue.Idx {
    var node = Node{
        .tag = .target_config_value,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (value) {
        .int_literal => |tok| {
            node.main_token = tok;
            node.data.lhs = @intFromEnum(TargetConfigValueNodeTag.int_literal);
        },
        .string_literal => |tok| {
            node.main_token = tok;
            node.data.lhs = @intFromEnum(TargetConfigValueNodeTag.string_literal);
        },
        .tag_literal => |tok| {
            node.main_token = tok;
            node.data.lhs = @intFromEnum(TargetConfigValueNodeTag.tag_literal);
        },
        .ident => |tok| {
            node.main_token = tok;
            node.data.lhs = @intFromEnum(TargetConfigValueNodeTag.ident);
        },
        .list => |span| {
            const extra_token = try store.reserveExtraDataToken(2);
            store.extra_data.appendAssumeCapacity(span.span.start);
            store.extra_data.appendAssumeCapacity(span.span.len);
            node.data.lhs = @intFromEnum(TargetConfigValueNodeTag.list);
            node.data.rhs = extra_token;
        },
        .files => |span| {
            const extra_token = try store.reserveExtraDataToken(2);
            store.extra_data.appendAssumeCapacity(span.span.start);
            store.extra_data.appendAssumeCapacity(span.span.len);
            node.data.lhs = @intFromEnum(TargetConfigValueNodeTag.files);
            node.data.rhs = extra_token;
        },
        .malformed => |m| {
            node.data.lhs = @intFromEnum(TargetConfigValueNodeTag.malformed);
            node.data.rhs = @intFromEnum(m.reason);
            node.region = m.region;
        },
    }

    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Returns the start position for a new Span of TargetEntry.Idxs in scratch
pub fn scratchTargetEntryTop(store: *NodeStore) u32 {
    return store.scratch_target_entries.top();
}

/// Places a new AST.TargetEntry.Idx in the scratch.
pub fn addScratchTargetEntry(store: *NodeStore, idx: AST.TargetEntry.Idx) std.mem.Allocator.Error!void {
    try store.scratch_target_entries.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch to extra_data.
pub fn targetEntrySpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.TargetEntry.Span {
    const end = store.scratch_target_entries.top();
    defer store.scratch_target_entries.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_target_entries.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any TargetEntry.Idxs added to scratch from start until the end.
pub fn clearScratchTargetEntriesFrom(store: *NodeStore, start: u32) void {
    store.scratch_target_entries.clearFrom(start);
}

/// Returns a new TargetEntry slice for iteration.
pub fn targetEntrySlice(store: *const NodeStore, span: AST.TargetEntry.Span) []AST.TargetEntry.Idx {
    return store.sliceFromSpan(AST.TargetEntry.Idx, span.span);
}

/// Returns the start position for a new Span of TargetFile.Idxs in scratch
pub fn scratchTargetFileTop(store: *NodeStore) u32 {
    return store.scratch_target_files.top();
}

/// Places a new AST.TargetFile.Idx in the scratch.
pub fn addScratchTargetFile(store: *NodeStore, idx: AST.TargetFile.Idx) std.mem.Allocator.Error!void {
    try store.scratch_target_files.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch to extra_data.
pub fn targetFileSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.TargetFile.Span {
    const end = store.scratch_target_files.top();
    defer store.scratch_target_files.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_target_files.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any TargetFile.Idxs added to scratch from start until the end.
pub fn clearScratchTargetFilesFrom(store: *NodeStore, start: u32) void {
    store.scratch_target_files.clearFrom(start);
}

/// Returns a new TargetFile slice for iteration.
pub fn targetFileSlice(store: *const NodeStore, span: AST.TargetFile.Span) []AST.TargetFile.Idx {
    return store.sliceFromSpan(AST.TargetFile.Idx, span.span);
}

/// Returns the start position for a new Span of TargetConfigEntry.Idxs in scratch.
pub fn scratchTargetConfigEntryTop(store: *NodeStore) u32 {
    return store.scratch_target_config_entries.top();
}

/// Places a new AST.TargetConfigEntry.Idx in the scratch.
pub fn addScratchTargetConfigEntry(store: *NodeStore, idx: AST.TargetConfigEntry.Idx) std.mem.Allocator.Error!void {
    try store.scratch_target_config_entries.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch to extra_data.
pub fn targetConfigEntrySpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.TargetConfigEntry.Span {
    const end = store.scratch_target_config_entries.top();
    defer store.scratch_target_config_entries.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_target_config_entries.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any TargetConfigEntry.Idxs added to scratch from start until the end.
pub fn clearScratchTargetConfigEntriesFrom(store: *NodeStore, start: u32) void {
    store.scratch_target_config_entries.clearFrom(start);
}

/// Returns a new TargetConfigEntry slice for iteration.
pub fn targetConfigEntrySlice(store: *const NodeStore, span: AST.TargetConfigEntry.Span) []AST.TargetConfigEntry.Idx {
    return store.sliceFromSpan(AST.TargetConfigEntry.Idx, span.span);
}

/// Returns the start position for a new Span of TargetConfigValue.Idxs in scratch.
pub fn scratchTargetConfigValueTop(store: *NodeStore) u32 {
    return store.scratch_target_config_values.top();
}

/// Places a new AST.TargetConfigValue.Idx in the scratch.
pub fn addScratchTargetConfigValue(store: *NodeStore, idx: AST.TargetConfigValue.Idx) std.mem.Allocator.Error!void {
    try store.scratch_target_config_values.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch to extra_data.
pub fn targetConfigValueSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.TargetConfigValue.Span {
    const end = store.scratch_target_config_values.top();
    defer store.scratch_target_config_values.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_target_config_values.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Returns the current top of the SymbolMapEntry scratch list.
pub fn scratchSymbolMapEntryTop(store: *NodeStore) u32 {
    return store.scratch_symbol_map_entries.top();
}

/// Adds a SymbolMapEntry index to scratch.
pub fn addScratchSymbolMapEntry(store: *NodeStore, idx: AST.SymbolMapEntry.Idx) std.mem.Allocator.Error!void {
    try store.scratch_symbol_map_entries.append(idx);
}

/// Clears any SymbolMapEntry.Idxs added to scratch from start until the end.
pub fn clearScratchSymbolMapEntriesFrom(store: *NodeStore, start: u32) void {
    store.scratch_symbol_map_entries.clearFrom(start);
}

/// Creates a SymbolMapEntry span from scratch entries added since start.
pub fn symbolMapEntrySpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.SymbolMapEntry.Span {
    const end = store.scratch_symbol_map_entries.top();
    defer store.scratch_symbol_map_entries.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_symbol_map_entries.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Returns a SymbolMapEntry slice for iteration over a span.
pub fn symbolMapEntrySlice(store: *const NodeStore, span: AST.SymbolMapEntry.Span) []AST.SymbolMapEntry.Idx {
    return store.sliceFromSpan(AST.SymbolMapEntry.Idx, span.span);
}

/// Clears any TargetConfigValue.Idxs added to scratch from start until the end.
pub fn clearScratchTargetConfigValuesFrom(store: *NodeStore, start: u32) void {
    store.scratch_target_config_values.clearFrom(start);
}

/// Returns a new TargetConfigValue slice for iteration.
pub fn targetConfigValueSlice(store: *const NodeStore, span: AST.TargetConfigValue.Span) []AST.TargetConfigValue.Idx {
    return store.sliceFromSpan(AST.TargetConfigValue.Idx, span.span);
}

/// Retrieves a TargetsSection from a stored node.
pub fn getTargetsSection(store: *const NodeStore, idx: AST.TargetsSection.Idx) AST.TargetsSection {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .targets_section);

    const inputs_dir: ?Token.Idx = if (node.main_token == 0) null else node.main_token;

    return .{
        .inputs_dir = inputs_dir,
        .entries = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
        .region = node.region,
    };
}

/// Retrieves a SymbolMapEntry from a stored node.
pub fn getSymbolMapEntry(store: *const NodeStore, idx: AST.SymbolMapEntry.Idx) AST.SymbolMapEntry {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .symbol_map_entry);

    return .{
        .symbol = node.main_token,
        .module = if (node.data.lhs == 0) null else unpackNonNullOptionalU32(node.data.lhs),
        .func = node.data.rhs,
        .region = node.region,
    };
}

/// Retrieves a TargetEntry from a stored node.
pub fn getTargetEntry(store: *const NodeStore, idx: AST.TargetEntry.Idx) AST.TargetEntry {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .target_entry);

    return .{
        .target = node.main_token,
        .config = @enumFromInt(node.data.lhs),
        .region = node.region,
    };
}

/// Retrieves a TargetFile from a stored node.
pub fn getTargetFile(store: *const NodeStore, idx: AST.TargetFile.Idx) AST.TargetFile {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));

    switch (node.tag) {
        .target_file_string => {
            return .{ .string_literal = node.main_token };
        },
        .target_file_ident => {
            return .{ .special_ident = node.main_token };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid target_file tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves a TargetConfig from a stored node.
pub fn getTargetConfig(store: *const NodeStore, idx: AST.TargetConfig.Idx) AST.TargetConfig {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .target_config);

    return .{
        .entries = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
        .region = node.region,
    };
}

/// Retrieves a TargetConfigEntry from a stored node.
pub fn getTargetConfigEntry(store: *const NodeStore, idx: AST.TargetConfigEntry.Idx) AST.TargetConfigEntry {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .target_config_entry);

    return .{
        .name = node.main_token,
        .value = @enumFromInt(node.data.lhs),
        .region = node.region,
    };
}

/// Retrieves a TargetConfigValue from a stored node.
pub fn getTargetConfigValue(store: *const NodeStore, idx: AST.TargetConfigValue.Idx) AST.TargetConfigValue {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .target_config_value);

    const tag: TargetConfigValueNodeTag = @enumFromInt(node.data.lhs);
    return switch (tag) {
        .int_literal => .{ .int_literal = node.main_token },
        .string_literal => .{ .string_literal = node.main_token },
        .tag_literal => .{ .tag_literal = node.main_token },
        .ident => .{ .ident = node.main_token },
        .list => blk: {
            const extra_start = extraDataStart(node.data.rhs);
            break :blk .{ .list = .{ .span = .{
                .start = store.extra_data.items[extra_start],
                .len = store.extra_data.items[extra_start + 1],
            } } };
        },
        .files => blk: {
            const extra_start = extraDataStart(node.data.rhs);
            break :blk .{ .files = .{ .span = .{
                .start = store.extra_data.items[extra_start],
                .len = store.extra_data.items[extra_start + 1],
            } } };
        },
        .malformed => .{ .malformed = .{
            .reason = @enumFromInt(node.data.rhs),
            .region = node.region,
        } },
    };
}

/// Adds a ForClauseTypeAlias node and returns its index.
pub fn addForClauseTypeAlias(store: *NodeStore, alias: AST.ForClauseTypeAlias) std.mem.Allocator.Error!AST.ForClauseTypeAlias.Idx {
    const node = Node{
        .tag = .for_clause_type_alias,
        .main_token = alias.alias_name,
        .data = .{
            .lhs = alias.rigid_name,
            .rhs = 0,
        },
        .region = alias.region,
    };
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Returns the start position for a new Span of ForClauseTypeAlias.Idxs in scratch
pub fn scratchForClauseTypeAliasTop(store: *NodeStore) u32 {
    return store.scratch_for_clause_type_aliases.top();
}

/// Places a new AST.ForClauseTypeAlias.Idx in the scratch.
pub fn addScratchForClauseTypeAlias(store: *NodeStore, idx: AST.ForClauseTypeAlias.Idx) std.mem.Allocator.Error!void {
    try store.scratch_for_clause_type_aliases.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch to extra_data.
pub fn forClauseTypeAliasSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.ForClauseTypeAlias.Span {
    const end = store.scratch_for_clause_type_aliases.top();
    defer store.scratch_for_clause_type_aliases.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_for_clause_type_aliases.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any ForClauseTypeAlias.Idxs added to scratch from start until the end.
pub fn clearScratchForClauseTypeAliasesFrom(store: *NodeStore, start: u32) void {
    store.scratch_for_clause_type_aliases.clearFrom(start);
}

/// Returns a new ForClauseTypeAlias slice for iteration.
pub fn forClauseTypeAliasSlice(store: *const NodeStore, span: AST.ForClauseTypeAlias.Span) []AST.ForClauseTypeAlias.Idx {
    return store.sliceFromSpan(AST.ForClauseTypeAlias.Idx, span.span);
}

/// Retrieves a ForClauseTypeAlias from a stored node.
pub fn getForClauseTypeAlias(store: *const NodeStore, idx: AST.ForClauseTypeAlias.Idx) AST.ForClauseTypeAlias {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .for_clause_type_alias);

    return .{
        .alias_name = node.main_token,
        .rigid_name = node.data.lhs,
        .region = node.region,
    };
}

/// Adds a RequiresEntry node and returns its index.
pub fn addRequiresEntry(store: *NodeStore, entry: AST.RequiresEntry) std.mem.Allocator.Error!AST.RequiresEntry.Idx {
    // Pack type_aliases len and type_anno idx into rhs
    const rhs_packed: u32 = (@as(u32, entry.type_aliases.span.len) << 16) | @as(u32, @intFromEnum(entry.type_anno));
    const node = Node{
        .tag = .requires_entry,
        .main_token = entry.entrypoint_name,
        .data = .{
            .lhs = entry.type_aliases.span.start,
            .rhs = rhs_packed,
        },
        .region = entry.region,
    };
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Returns the start position for a new Span of RequiresEntry.Idxs in scratch
pub fn scratchRequiresEntryTop(store: *NodeStore) u32 {
    return store.scratch_requires_entries.top();
}

/// Places a new AST.RequiresEntry.Idx in the scratch.
pub fn addScratchRequiresEntry(store: *NodeStore, idx: AST.RequiresEntry.Idx) std.mem.Allocator.Error!void {
    try store.scratch_requires_entries.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch to extra_data.
pub fn requiresEntrySpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.RequiresEntry.Span {
    const end = store.scratch_requires_entries.top();
    defer store.scratch_requires_entries.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        try store.extra_data.append(store.gpa, @intFromEnum(store.scratch_requires_entries.items.items[i]));
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any RequiresEntry.Idxs added to scratch from start until the end.
pub fn clearScratchRequiresEntriesFrom(store: *NodeStore, start: u32) void {
    store.scratch_requires_entries.clearFrom(start);
}

/// Returns a new RequiresEntry slice for iteration.
pub fn requiresEntrySlice(store: *const NodeStore, span: AST.RequiresEntry.Span) []AST.RequiresEntry.Idx {
    return store.sliceFromSpan(AST.RequiresEntry.Idx, span.span);
}

/// Retrieves a RequiresEntry from a stored node.
pub fn getRequiresEntry(store: *const NodeStore, idx: AST.RequiresEntry.Idx) AST.RequiresEntry {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .requires_entry);

    // Unpack type_aliases len and type_anno idx from rhs
    const type_aliases_len: u32 = node.data.rhs >> 16;
    const type_anno_idx: u16 = @truncate(node.data.rhs);

    return .{
        .type_aliases = .{ .span = .{ .start = node.data.lhs, .len = type_aliases_len } },
        .entrypoint_name = node.main_token,
        .type_anno = @enumFromInt(type_anno_idx),
        .region = node.region,
    };
}
