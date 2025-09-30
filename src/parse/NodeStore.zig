//! Typesafe access to an underlying SoA of Nodes.
//! This - along with the types used in its API - should
//! be the only way that other modules interact with
//! the AST.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const collections = @import("collections");

const AST = @import("AST.zig");
const Node = @import("Node.zig");
const Token = @import("tokenize.zig").Token;
const Region = AST.TokenizedRegion;
const Diagnostic = AST.Diagnostic;

const sexpr = base.sexpr;

const NodeStore = @This();

gpa: std.mem.Allocator,
nodes: Node.List,
extra_data: std.ArrayListUnmanaged(u32),
scratch_statements: base.Scratch(AST.Statement.Idx),
scratch_tokens: base.Scratch(Token.Idx),
scratch_exprs: base.Scratch(AST.Expr.Idx),
scratch_patterns: base.Scratch(AST.Pattern.Idx),
scratch_record_fields: base.Scratch(AST.RecordField.Idx),
scratch_pattern_record_fields: base.Scratch(AST.PatternRecordField.Idx),
scratch_match_branches: base.Scratch(AST.MatchBranch.Idx),
scratch_type_annos: base.Scratch(AST.TypeAnno.Idx),
scratch_anno_record_fields: base.Scratch(AST.AnnoRecordField.Idx),
scratch_exposed_items: base.Scratch(AST.ExposedItem.Idx),
scratch_where_clauses: base.Scratch(AST.WhereClause.Idx),

/// Compile-time constants for union variant counts to ensure we don't miss cases
/// when adding/removing variants from AST unions. Update these when modifying the unions.
///
/// Count of the header nodes in the AST
pub const AST_HEADER_NODE_COUNT = 6;
/// Count of the statement nodes in the AST
pub const AST_STATEMENT_NODE_COUNT = 12;
/// Count of the pattern nodes in the AST
pub const AST_PATTERN_NODE_COUNT = 14;
/// Count of the type annotation nodes in the AST
pub const AST_TYPE_ANNO_NODE_COUNT = 10;
/// Count of the expression nodes in the AST
pub const AST_EXPR_NODE_COUNT = 24;

/// Initialize the store with an assumed capacity to
/// ensure resizing of underlying data structures happens
/// very rarely.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!NodeStore {
    var store: NodeStore = .{
        .gpa = gpa,
        .nodes = try Node.List.initCapacity(gpa, capacity),
        .extra_data = try std.ArrayListUnmanaged(u32).initCapacity(gpa, capacity / 2),
        .scratch_statements = try base.Scratch(AST.Statement.Idx).init(gpa),
        .scratch_tokens = try base.Scratch(Token.Idx).init(gpa),
        .scratch_exprs = try base.Scratch(AST.Expr.Idx).init(gpa),
        .scratch_patterns = try base.Scratch(AST.Pattern.Idx).init(gpa),
        .scratch_record_fields = try base.Scratch(AST.RecordField.Idx).init(gpa),
        .scratch_pattern_record_fields = try base.Scratch(AST.PatternRecordField.Idx).init(gpa),
        .scratch_match_branches = try base.Scratch(AST.MatchBranch.Idx).init(gpa),
        .scratch_type_annos = try base.Scratch(AST.TypeAnno.Idx).init(gpa),
        .scratch_anno_record_fields = try base.Scratch(AST.AnnoRecordField.Idx).init(gpa),
        .scratch_exposed_items = try base.Scratch(AST.ExposedItem.Idx).init(gpa),
        .scratch_where_clauses = try base.Scratch(AST.WhereClause.Idx).init(gpa),
    };

    _ = try store.nodes.append(gpa, .{
        .tag = .root,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = .{ .start = 0, .end = 0 },
    });
    return store;
}

/// Default capacity for scratch buffers, sized generously since they're typically small.
///
/// TODO: tune this base on real roc code.
const scratch_90th_percentile_capacity = std.math.ceilPowerOfTwoAssert(usize, 64);

/// Deinitializes all data owned by the store.
/// A caller should ensure that they have taken
/// ownership of all Node data before calling this
/// method.
pub fn deinit(store: *NodeStore) void {
    store.nodes.deinit(store.gpa);
    store.extra_data.deinit(store.gpa);
    store.scratch_statements.deinit(store.gpa);
    store.scratch_tokens.deinit(store.gpa);
    store.scratch_exprs.deinit(store.gpa);
    store.scratch_patterns.deinit(store.gpa);
    store.scratch_record_fields.deinit(store.gpa);
    store.scratch_pattern_record_fields.deinit(store.gpa);
    store.scratch_match_branches.deinit(store.gpa);
    store.scratch_type_annos.deinit(store.gpa);
    store.scratch_anno_record_fields.deinit(store.gpa);
    store.scratch_exposed_items.deinit(store.gpa);
    store.scratch_where_clauses.deinit(store.gpa);
}

/// Ensures that all scratch buffers in the store
/// are clear for use.
pub fn emptyScratch(store: *NodeStore) void {
    store.scratch_statements.clearFrom(0);
    store.scratch_tokens.clearFrom(0);
    store.scratch_exprs.clearFrom(0);
    store.scratch_patterns.clearFrom(0);
    store.scratch_record_fields.clearFrom(0);
    store.scratch_pattern_record_fields.clearFrom(0);
    store.scratch_match_branches.clearFrom(0);
    store.scratch_type_annos.clearFrom(0);
    store.scratch_anno_record_fields.clearFrom(0);
    store.scratch_exposed_items.clearFrom(0);
    store.scratch_where_clauses.clearFrom(0);
}

/// Prints debug information about all nodes and scratch buffers in the store.
pub fn debug(store: *NodeStore) void {
    if (comptime builtin.target.os.tag != .freestanding) {
        std.debug.print("\n==> IR.NodeStore DEBUG <==\n", .{});
        std.debug.print("Nodes:\n", .{});
        var nodes_iter = store.nodes.iterIndices();
        while (nodes_iter.next()) |idx| {
            std.debug.print("{d}: {any}\n", .{ @intFromEnum(idx), store.nodes.get(idx) });
        }
        std.debug.print("Extra Data: {any}\n", .{store.extra_data.items});
        std.debug.print("Scratch statements: {any}\n", .{store.scratch_statements.items});
        std.debug.print("Scratch tokens: {any}\n", .{store.scratch_tokens.items});
        std.debug.print("Scratch exprs: {any}\n", .{store.scratch_exprs.items});
        std.debug.print("Scratch patterns: {any}\n", .{store.scratch_patterns.items});
        std.debug.print("Scratch record fields: {any}\n", .{store.scratch_record_fields.items});
        std.debug.print("Scratch pattern record fields: {any}\n", .{store.scratch_pattern_record_fields.items});
        std.debug.print("Scratch match branches: {any}\n", .{store.scratch_match_branches.items});
        std.debug.print("Scratch type annos: {any}\n", .{store.scratch_type_annos.items});
        std.debug.print("Scratch anno record fields: {any}\n", .{store.scratch_anno_record_fields.items});
        std.debug.print("Scratch exposes items: {any}\n", .{store.scratch_exposed_items.items});
        std.debug.print("Scratch where clauses: {any}\n", .{store.scratch_where_clauses.items});
        std.debug.print("==> IR.NodeStore DEBUG <==\n\n", .{});
    }
}

// ------------------------------------------------------------------------
// Creation API - All nodes should be added using these functions
// ------------------------------------------------------------------------

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

/// Adds a file node to the store.
pub fn addFile(store: *NodeStore, file: AST.File) std.mem.Allocator.Error!void {
    try store.extra_data.append(store.gpa, @intFromEnum(file.header));
    store.nodes.set(@enumFromInt(0), .{
        .tag = .root,
        .main_token = 0,
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

            const ed_start = store.extra_data.items.len;
            try store.extra_data.append(store.gpa, @intFromEnum(platform.requires_rigids));
            try store.extra_data.append(store.gpa, @intFromEnum(platform.requires_signatures));
            try store.extra_data.append(store.gpa, @intFromEnum(platform.exposes));
            try store.extra_data.append(store.gpa, @intFromEnum(platform.packages));
            try store.extra_data.append(store.gpa, @intFromEnum(platform.provides));
            const ed_len = store.extra_data.items.len - ed_start;

            node.data.lhs = @intCast(ed_start);
            node.data.rhs = @intCast(ed_len);

            node.region = platform.region;
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
            node.data.lhs = @intFromEnum(v.body);
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
            node.tag = .type_decl;
            if (d.kind == .nominal) {
                node.tag = .type_decl_nominal;
            }
            node.region = d.region;
            node.data.lhs = @intFromEnum(d.header);
            node.data.rhs = @intFromEnum(d.anno);

            // Store where clause and optional block in extra_data
            // Format: [where_idx (0 if null), has_block (0/1), block_data if has_block]
            const extra_start = @as(u32, @intCast(store.extra_data.items.len));
            const where_idx = if (d.where) |w| @intFromEnum(w) else 0;
            try store.extra_data.append(store.gpa, where_idx);

            if (d.block) |blk| {
                try store.extra_data.append(store.gpa, 1); // has_block = true
                // Store block: statements span start, span len, region start, region end
                try store.extra_data.append(store.gpa, blk.statements.span.start);
                try store.extra_data.append(store.gpa, blk.statements.span.len);
                try store.extra_data.append(store.gpa, blk.region.start);
                try store.extra_data.append(store.gpa, blk.region.end);
            } else {
                try store.extra_data.append(store.gpa, 0); // has_block = false
            }

            node.main_token = extra_start;
        },
        .type_anno => |a| {
            node.tag = .type_anno;
            node.region = a.region;
            node.data.lhs = a.name;
            node.data.rhs = @intFromEnum(a.anno);
            if (a.where) |w| {
                node.main_token = @intFromEnum(w);
            }
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
        .tag => |t| {
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, t.args.span.len);
            try store.extra_data.append(store.gpa, t.qualifiers.span.start);
            try store.extra_data.append(store.gpa, t.qualifiers.span.len);

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
        },
        .frac => |n| {
            node.tag = .frac_patt;
            node.region = n.region;
            node.main_token = n.number_tok;
        },
        .string => |s| {
            node.tag = .string_patt;
            node.region = s.region;
            node.main_token = s.string_tok;
            node.data.lhs = @intFromEnum(s.expr);
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
        },
        .frac => |e| {
            node.tag = .frac;
            node.region = e.region;
            node.main_token = e.token;
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
        .record_updater => |_| {},
        .field_access => |fa| {
            node.tag = .field_access;
            node.region = fa.region;
            node.main_token = fa.operator;
            node.data.lhs = @intFromEnum(fa.left);
            node.data.rhs = @intFromEnum(fa.right);
        },
        .local_dispatch => |ld| {
            node.tag = .local_dispatch;
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
            node.data.lhs = @intFromEnum(rb.mapper);
            node.data.rhs = @intFromEnum(rb.fields);
        },
        .block => |body| {
            node.tag = .block;
            node.region = body.region;
            node.main_token = 0;
            node.data.lhs = body.statements.span.start;
            node.data.rhs = body.statements.span.len;
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
        .main_token = field.name,
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
        .name = node.main_token,
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
        .main_token = 0,
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
            // [tags.span.start, tags.span.len, open_anno?]
            const data_start = @as(u32, @intCast(store.extra_data.items.len));
            try store.extra_data.append(store.gpa, tu.tags.span.start);
            try store.extra_data.append(store.gpa, tu.tags.span.len);

            var rhs = AST.TypeAnno.TagUnionRhs{
                .open = 0,
                .tags_len = @as(u31, @intCast(tu.tags.span.len)),
            };
            if (tu.open_anno) |a| {
                rhs.open = 1;
                try store.extra_data.append(store.gpa, @intFromEnum(a));
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
            node.data.lhs = r.fields.span.start;
            node.data.rhs = r.fields.span.len;
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

// ------------------------------------------------------------------------
// Read API - All nodes should be accessed using these functions
// ------------------------------------------------------------------------

/// TODO
pub fn getFile(store: *const NodeStore) AST.File {
    const node = store.nodes.get(@enumFromInt(0));
    const header_ed_idx = @as(usize, @intCast(node.data.lhs + node.data.rhs));
    const header = store.extra_data.items[header_ed_idx];
    return .{
        .header = @enumFromInt(header),
        .statements = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
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
            std.debug.assert(node.data.rhs == 5);

            return .{ .platform = .{
                .name = node.main_token,
                .requires_rigids = @enumFromInt(store.extra_data.items[ed_start]),
                .requires_signatures = @enumFromInt(store.extra_data.items[ed_start + 1]),
                .exposes = @enumFromInt(store.extra_data.items[ed_start + 2]),
                .packages = @enumFromInt(store.extra_data.items[ed_start + 3]),
                .provides = @enumFromInt(store.extra_data.items[ed_start + 4]),
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
                .body = @enumFromInt(node.data.lhs),
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
        .type_decl => {
            // Read where clause and block from extra_data
            // Format: [where_idx (0 if null), has_block (0/1), block_data if has_block]
            const extra_start = node.main_token;
            const where_idx = store.extra_data.items[extra_start];
            const has_block = store.extra_data.items[extra_start + 1];

            var block: ?AST.Block = null;
            if (has_block != 0) {
                const stmt_start = store.extra_data.items[extra_start + 2];
                const stmt_len = store.extra_data.items[extra_start + 3];
                const reg_start = store.extra_data.items[extra_start + 4];
                const reg_end = store.extra_data.items[extra_start + 5];
                block = AST.Block{
                    .statements = AST.Statement.Span{ .span = .{ .start = stmt_start, .len = stmt_len } },
                    .region = .{ .start = reg_start, .end = reg_end },
                };
            }

            return .{ .type_decl = .{
                .region = node.region,
                .header = @enumFromInt(node.data.lhs),
                .anno = @enumFromInt(node.data.rhs),
                .kind = .alias,
                .where = if (where_idx != 0) @enumFromInt(where_idx) else null,
                .block = block,
            } };
        },
        .type_decl_nominal => {
            // Read where clause and block from extra_data
            // Format: [where_idx (0 if null), has_block (0/1), block_data if has_block]
            const extra_start = node.main_token;
            const where_idx = store.extra_data.items[extra_start];
            const has_block = store.extra_data.items[extra_start + 1];

            var block: ?AST.Block = null;
            if (has_block != 0) {
                const stmt_start = store.extra_data.items[extra_start + 2];
                const stmt_len = store.extra_data.items[extra_start + 3];
                const reg_start = store.extra_data.items[extra_start + 4];
                const reg_end = store.extra_data.items[extra_start + 5];
                block = AST.Block{
                    .statements = AST.Statement.Span{ .span = .{ .start = stmt_start, .len = stmt_len } },
                    .region = .{ .start = reg_start, .end = reg_end },
                };
            }

            return .{ .type_decl = .{
                .region = node.region,
                .header = @enumFromInt(node.data.lhs),
                .anno = @enumFromInt(node.data.rhs),
                .kind = .nominal,
                .where = if (where_idx != 0) @enumFromInt(where_idx) else null,
                .block = block,
            } };
        },
        .type_anno => {
            return .{ .type_anno = .{
                .region = node.region,
                .name = node.data.lhs,
                .anno = @enumFromInt(node.data.rhs),
                .where = if (node.main_token != 0) @enumFromInt(node.main_token) else null,
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
        .tag_patt => {
            const args_start = node.data.lhs;

            const ed_start = @as(usize, @intCast(node.data.rhs));
            const args_len = store.extra_data.items[ed_start];
            const qualifiers_start = store.extra_data.items[ed_start + 1];
            const qualifiers_len = store.extra_data.items[ed_start + 2];

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
                .region = node.region,
            } };
        },
        .string_patt => {
            return .{ .string = .{
                .string_tok = node.main_token,
                .region = node.region,
                .expr = @enumFromInt(node.data.lhs),
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
                .region = node.region,
            } };
        },
        .frac_patt => {
            return .{ .frac = .{
                .number_tok = node.main_token,
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
                .region = node.region,
            } };
        },
        .frac => {
            return .{ .frac = .{
                .token = node.main_token,
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
        .field_access => {
            return .{ .field_access = .{
                .left = @enumFromInt(node.data.lhs),
                .right = @enumFromInt(node.data.rhs),
                .operator = node.main_token,
                .region = node.region,
            } };
        },
        .local_dispatch => {
            return .{ .local_dispatch = .{
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
                .region = node.region,
            } };
        },
        .ellipsis => {
            return .{ .ellipsis = .{
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
            return .{ .record_builder = .{
                .mapper = @enumFromInt(node.data.lhs),
                .fields = @enumFromInt(node.data.rhs),
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

/// Retrieves when branch data from a stored when branch node.
pub fn getBranch(store: *const NodeStore, branch_idx: AST.MatchBranch.Idx) AST.MatchBranch {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(branch_idx)));
    return .{
        .region = node.region,
        .pattern = @enumFromInt(node.data.lhs),
        .body = @enumFromInt(node.data.rhs),
    };
}

/// Retrieves type header data from a stored type header node.
pub fn getTypeHeader(store: *const NodeStore, header_idx: AST.TypeHeader.Idx) !AST.TypeHeader {
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
pub fn getAnnoRecordField(store: *const NodeStore, anno_record_field_idx: AST.AnnoRecordField.Idx) !AST.AnnoRecordField {
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

            // Read flat data format: [tags.span.start, tags.span.len, open_anno?]
            var extra_data_pos = node.data.lhs;
            const tags_start = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;
            const tags_len = store.extra_data.items[extra_data_pos];
            extra_data_pos += 1;

            const open_anno = if (rhs.open == 1) @as(AST.TypeAnno.Idx, @enumFromInt(store.extra_data.items[extra_data_pos])) else null;

            return .{ .tag_union = .{
                .region = node.region,
                .open_anno = open_anno,
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
            return .{ .record = .{
                .region = node.region,
                .fields = .{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } },
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

// ------------------------------------------------------------------------
// Node types - these are the constituent types used in the Node Store API
// ------------------------------------------------------------------------

/// Returns the start position for a new Span of AST.Expr.Idxs in scratch
pub fn scratchExprTop(store: *NodeStore) u32 {
    return store.scratch_exprs.top();
}

/// Places a new AST.Expr.Idx in the scratch.
pub fn addScratchExpr(store: *NodeStore, idx: AST.Expr.Idx) std.mem.Allocator.Error!void {
    try store.scratch_exprs.append(store.gpa, idx);
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
    try store.scratch_statements.append(store.gpa, idx);
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
    try store.scratch_patterns.append(store.gpa, idx);
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

/// Creates a slice corresponding to a span.
pub fn sliceFromSpan(store: *const NodeStore, comptime T: type, span: base.DataSpan) []T {
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
    try store.scratch_pattern_record_fields.append(store.gpa, idx);
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
    try store.scratch_record_fields.append(store.gpa, idx);
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
    try store.scratch_match_branches.append(store.gpa, idx);
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
    try store.scratch_type_annos.append(store.gpa, idx);
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
    try store.scratch_anno_record_fields.append(store.gpa, idx);
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
    try store.scratch_tokens.append(store.gpa, idx);
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
    try store.scratch_exposed_items.append(store.gpa, idx);
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
    try store.scratch_where_clauses.append(store.gpa, idx);
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
