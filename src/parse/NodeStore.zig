//! Typesafe access to an underlying SoA of Nodes.
//! This - along with the types used in its API - should
//! be the only way that other modules interact with
//! the AST.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");

const AST = @import("AST.zig");
const Node = @import("Node.zig");
const Token = @import("tokenize.zig").Token;
const Region = AST.TokenizedRegion;
const Diagnostic = AST.Diagnostic;

/// When storing optional indices/values where 0 is a valid value, we add this offset
/// to distinguish "value is 0" from "value is null". This is a common pattern when
/// packing optional data into u32 fields where 0 would otherwise be ambiguous.
const OPTIONAL_VALUE_OFFSET: u32 = 1;

/// Bit flag for is_var in type_anno statement's main_token field.
/// Uses the high bit to store whether this is a var declaration.
const TYPE_ANNO_IS_VAR_BIT: u32 = 0x80000000;

/// The root node is always stored at index 0 in the node list.
pub const root_node_idx: Node.List.Idx = .first;

const NodeStore = @This();

gpa: std.mem.Allocator,
nodes: Node.List,
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
scratch_target_entries: base.Scratch(AST.TargetEntry.Idx),
scratch_target_files: base.Scratch(AST.TargetFile.Idx),
scratch_for_clause_type_aliases: base.Scratch(AST.ForClauseTypeAlias.Idx),
scratch_requires_entries: base.Scratch(AST.RequiresEntry.Idx),

// Typed extra data lists for nodes that need more data than fits in the Node struct
platform_header_extra: std.ArrayList(PlatformHeaderExtra),
import_extra: std.ArrayList(ImportExtra),
type_decl_extra: std.ArrayList(TypeDeclExtra),
tag_patt_extra: std.ArrayList(TagPattExtra),
where_mod_method_extra: std.ArrayList(WhereModMethodExtra),

// Typed span content lists for storing index spans
expr_span_data: std.ArrayList(AST.Expr.Idx),
statement_span_data: std.ArrayList(AST.Statement.Idx),
pattern_span_data: std.ArrayList(AST.Pattern.Idx),
pattern_record_field_span_data: std.ArrayList(AST.PatternRecordField.Idx),
record_field_span_data: std.ArrayList(AST.RecordField.Idx),
match_branch_span_data: std.ArrayList(AST.MatchBranch.Idx),
type_anno_span_data: std.ArrayList(AST.TypeAnno.Idx),
anno_record_field_span_data: std.ArrayList(AST.AnnoRecordField.Idx),
token_span_data: std.ArrayList(Token.Idx),
exposed_item_span_data: std.ArrayList(AST.ExposedItem.Idx),
where_clause_span_data: std.ArrayList(AST.WhereClause.Idx),
target_entry_span_data: std.ArrayList(AST.TargetEntry.Idx),
target_file_span_data: std.ArrayList(AST.TargetFile.Idx),
for_clause_type_alias_span_data: std.ArrayList(AST.ForClauseTypeAlias.Idx),
requires_entry_span_data: std.ArrayList(AST.RequiresEntry.Idx),

/// Compile-time constants for union variant counts to ensure we don't miss cases
/// when adding/removing variants from AST unions. Update these when modifying the unions.
///
/// Count of the header nodes in the AST
pub const AST_HEADER_NODE_COUNT = 6;
/// Count of the statement nodes in the AST
pub const AST_STATEMENT_NODE_COUNT = 13;
/// Count of the pattern nodes in the AST
pub const AST_PATTERN_NODE_COUNT = 15;
/// Count of the type annotation nodes in the AST
pub const AST_TYPE_ANNO_NODE_COUNT = 10;
/// Count of the expression nodes in the AST
pub const AST_EXPR_NODE_COUNT = 26;

/// Typed extra data for platform header nodes
pub const PlatformHeaderExtra = struct {
    requires_start: u32,
    requires_len: u32,
    exposes: u32, // AST.Collection.Idx
    packages: u32, // AST.PackagesSection.Idx
    provides: u32, // AST.Collection.Idx
    targets: u32, // optional: 0 = null, otherwise idx + OPTIONAL_VALUE_OFFSET
};

/// Typed extra data for import statement nodes
pub const ImportExtra = struct {
    exposes_start: u32,
    exposes_len: u32,
    qualifier_tok: u32, // optional: 0 = null
    alias_tok: u32, // optional: 0 = null
};

/// Typed extra data for type declaration nodes with where/associated
pub const TypeDeclExtra = struct {
    where_idx: u32, // 0 = null, otherwise Collection.Idx
    has_associated: u32, // 0 = false, 1 = true
    statements_start: u32,
    statements_len: u32,
    region_start: u32,
    region_end: u32,
};

/// Typed extra data for tag pattern nodes
pub const TagPattExtra = struct {
    args_len: u32,
    qualifiers_start: u32,
    qualifiers_len: u32,
};

/// Typed extra data for where_mod_method clause nodes
pub const WhereModMethodExtra = struct {
    name_tok: u32,
    args: u32, // AST.TypeAnno.Span.Idx
    ret_anno: u32, // AST.TypeAnno.Idx
};

/// Initialize the store with an assumed capacity to
/// ensure resizing of underlying data structures happens
/// very rarely.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) std.mem.Allocator.Error!NodeStore {
    var store: NodeStore = .{
        .gpa = gpa,
        .nodes = try Node.List.initCapacity(gpa, capacity),
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
        .scratch_target_entries = try base.Scratch(AST.TargetEntry.Idx).init(gpa),
        .scratch_target_files = try base.Scratch(AST.TargetFile.Idx).init(gpa),
        .scratch_for_clause_type_aliases = try base.Scratch(AST.ForClauseTypeAlias.Idx).init(gpa),
        .scratch_requires_entries = try base.Scratch(AST.RequiresEntry.Idx).init(gpa),
        // Typed extra data lists
        .platform_header_extra = try std.ArrayList(PlatformHeaderExtra).initCapacity(gpa, 0),
        .import_extra = try std.ArrayList(ImportExtra).initCapacity(gpa, 0),
        .type_decl_extra = try std.ArrayList(TypeDeclExtra).initCapacity(gpa, 0),
        .tag_patt_extra = try std.ArrayList(TagPattExtra).initCapacity(gpa, 0),
        .where_mod_method_extra = try std.ArrayList(WhereModMethodExtra).initCapacity(gpa, 0),
        // Typed span content lists
        .expr_span_data = try std.ArrayList(AST.Expr.Idx).initCapacity(gpa, 0),
        .statement_span_data = try std.ArrayList(AST.Statement.Idx).initCapacity(gpa, 0),
        .pattern_span_data = try std.ArrayList(AST.Pattern.Idx).initCapacity(gpa, 0),
        .pattern_record_field_span_data = try std.ArrayList(AST.PatternRecordField.Idx).initCapacity(gpa, 0),
        .record_field_span_data = try std.ArrayList(AST.RecordField.Idx).initCapacity(gpa, 0),
        .match_branch_span_data = try std.ArrayList(AST.MatchBranch.Idx).initCapacity(gpa, 0),
        .type_anno_span_data = try std.ArrayList(AST.TypeAnno.Idx).initCapacity(gpa, 0),
        .anno_record_field_span_data = try std.ArrayList(AST.AnnoRecordField.Idx).initCapacity(gpa, 0),
        .token_span_data = try std.ArrayList(Token.Idx).initCapacity(gpa, 0),
        .exposed_item_span_data = try std.ArrayList(AST.ExposedItem.Idx).initCapacity(gpa, 0),
        .where_clause_span_data = try std.ArrayList(AST.WhereClause.Idx).initCapacity(gpa, 0),
        .target_entry_span_data = try std.ArrayList(AST.TargetEntry.Idx).initCapacity(gpa, 0),
        .target_file_span_data = try std.ArrayList(AST.TargetFile.Idx).initCapacity(gpa, 0),
        .for_clause_type_alias_span_data = try std.ArrayList(AST.ForClauseTypeAlias.Idx).initCapacity(gpa, 0),
        .requires_entry_span_data = try std.ArrayList(AST.RequiresEntry.Idx).initCapacity(gpa, 0),
    };

    _ = try store.nodes.append(gpa, .{
        .tag = .root,
        .main_token = 0,
        .data = .{ .lhs = 0, .rhs = 0 },
        .region = .{ .start = 0, .end = 0 },
    });
    return store;
}

/// Deinitializes all data owned by the store.
/// A caller should ensure that they have taken
/// ownership of all Node data before calling this
/// method.
pub fn deinit(store: *NodeStore) void {
    store.nodes.deinit(store.gpa);
    store.scratch_statements.deinit();
    store.scratch_tokens.deinit();
    store.scratch_exprs.deinit();
    store.scratch_patterns.deinit();
    store.scratch_record_fields.deinit();
    store.scratch_pattern_record_fields.deinit();
    store.scratch_match_branches.deinit();
    store.scratch_type_annos.deinit();
    store.scratch_anno_record_fields.deinit();
    store.scratch_exposed_items.deinit();
    store.scratch_where_clauses.deinit();
    store.scratch_target_entries.deinit();
    store.scratch_target_files.deinit();
    store.scratch_for_clause_type_aliases.deinit();
    store.scratch_requires_entries.deinit();
    // Typed extra data lists
    store.platform_header_extra.deinit(store.gpa);
    store.import_extra.deinit(store.gpa);
    store.type_decl_extra.deinit(store.gpa);
    store.tag_patt_extra.deinit(store.gpa);
    store.where_mod_method_extra.deinit(store.gpa);
    // Typed span content lists
    store.expr_span_data.deinit(store.gpa);
    store.statement_span_data.deinit(store.gpa);
    store.pattern_span_data.deinit(store.gpa);
    store.pattern_record_field_span_data.deinit(store.gpa);
    store.record_field_span_data.deinit(store.gpa);
    store.match_branch_span_data.deinit(store.gpa);
    store.type_anno_span_data.deinit(store.gpa);
    store.anno_record_field_span_data.deinit(store.gpa);
    store.token_span_data.deinit(store.gpa);
    store.exposed_item_span_data.deinit(store.gpa);
    store.where_clause_span_data.deinit(store.gpa);
    store.target_entry_span_data.deinit(store.gpa);
    store.target_file_span_data.deinit(store.gpa);
    store.for_clause_type_alias_span_data.deinit(store.gpa);
    store.requires_entry_span_data.deinit(store.gpa);
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
    store.scratch_target_entries.clearFrom(0);
    store.scratch_target_files.clearFrom(0);
    store.scratch_for_clause_type_aliases.clearFrom(0);
    store.scratch_requires_entries.clearFrom(0);
}

/// Prints debug information about all nodes and scratch buffers in the store.
/// Note: This debug function is excluded from coverage via --exclude-line=std.debug.print
pub fn debug(store: *NodeStore) void {
    if (comptime builtin.target.os.tag != .freestanding) {
        std.debug.print("\n==> IR.NodeStore DEBUG <==\n", .{});
        std.debug.print("Nodes:\n", .{});
        var nodes_iter = store.nodes.iterIndices();
        while (nodes_iter.next()) |idx| {
            std.debug.print("{d}: {any}\n", .{ @intFromEnum(idx), store.nodes.get(idx) });
        }
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
    store.nodes.set(root_node_idx, .{
        .tag = .root,
        .main_token = @intFromEnum(file.header),
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

            // Store extra data in typed list
            const extra_idx = store.platform_header_extra.items.len;
            try store.platform_header_extra.append(store.gpa, .{
                .requires_start = platform.requires_entries.span.start,
                .requires_len = platform.requires_entries.span.len,
                .exposes = @intFromEnum(platform.exposes),
                .packages = @intFromEnum(platform.packages),
                .provides = @intFromEnum(platform.provides),
                .targets = if (platform.targets) |t| @intFromEnum(t) + OPTIONAL_VALUE_OFFSET else 0,
            });

            node.data.lhs = @intCast(extra_idx);
            node.data.rhs = 0; // unused now

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
            const rhs = AST.ImportRhs{
                .aliased = if (i.alias_tok != null) 1 else 0,
                .qualified = if (i.qualifier_tok != null) 1 else 0,
                .num_exposes = @as(u30, @intCast(i.exposes.span.len)),
            };

            // Store extra data in typed list
            const extra_idx = store.import_extra.items.len;
            try store.import_extra.append(store.gpa, .{
                .exposes_start = i.exposes.span.start,
                .exposes_len = i.exposes.span.len,
                .qualifier_tok = if (i.qualifier_tok) |tok| tok else 0,
                .alias_tok = if (i.alias_tok) |tok| tok else 0,
            });

            node.data.rhs = @as(u32, @bitCast(rhs));
            node.data.lhs = @intCast(extra_idx);
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

            // Store optional where and associated in typed list if either is present
            if (d.where != null or d.associated != null) {
                const extra_idx = store.type_decl_extra.items.len;
                try store.type_decl_extra.append(store.gpa, .{
                    .where_idx = if (d.where) |w| @intFromEnum(w) else 0,
                    .has_associated = if (d.associated != null) 1 else 0,
                    .statements_start = if (d.associated) |assoc| assoc.statements.span.start else 0,
                    .statements_len = if (d.associated) |assoc| assoc.statements.span.len else 0,
                    .region_start = if (d.associated) |assoc| assoc.region.start else 0,
                    .region_end = if (d.associated) |assoc| assoc.region.end else 0,
                });
                // Use OPTIONAL_VALUE_OFFSET so 0 means "no extra data"
                node.main_token = @intCast(extra_idx + OPTIONAL_VALUE_OFFSET);
            } else {
                node.main_token = 0;
            }
        },
        .type_anno => |a| {
            node.tag = .type_anno;
            node.region = a.region;
            node.data.lhs = a.name;
            node.data.rhs = @intFromEnum(a.anno);
            const where_val: u32 = if (a.where) |w| @intFromEnum(w) + OPTIONAL_VALUE_OFFSET else 0;
            const is_var_bit: u32 = if (a.is_var) TYPE_ANNO_IS_VAR_BIT else 0;
            node.main_token = where_val | is_var_bit;
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
            // Store extra data in typed list
            const extra_idx = store.tag_patt_extra.items.len;
            try store.tag_patt_extra.append(store.gpa, .{
                .args_len = t.args.span.len,
                .qualifiers_start = t.qualifiers.span.start,
                .qualifiers_len = t.qualifiers.span.len,
            });

            node.tag = .tag_patt;
            node.region = t.region;
            node.main_token = t.tag_tok;
            node.data.lhs = t.args.span.start;
            node.data.rhs = @intCast(extra_idx);
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
        .typed_int => |e| {
            node.tag = .typed_int;
            node.region = e.region;
            node.main_token = e.token;
            node.data.lhs = e.type_token;
        },
        .typed_frac => |e| {
            node.tag = .typed_frac;
            node.region = e.region;
            node.main_token = e.token;
            node.data.lhs = e.type_token;
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
            node.data.lhs = r.fields.span.start;
            node.data.rhs = r.fields.span.len;
            // Store ext with OPTIONAL_VALUE_OFFSET: 0 = null, otherwise idx + 1
            node.main_token = if (r.ext) |ext| @intFromEnum(ext) + OPTIONAL_VALUE_OFFSET else 0;
        },
        .lambda => |l| {
            node.tag = .lambda;
            node.region = l.region;
            node.data.lhs = l.args.span.start;
            node.data.rhs = l.args.span.len;
            node.main_token = @intFromEnum(l.body);
        },
        .apply => |app| {
            node.tag = .apply;
            node.region = app.region;
            node.data.lhs = app.args.span.start;
            node.data.rhs = app.args.span.len;
            node.main_token = @intFromEnum(app.@"fn");
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
            node.data.rhs = @intFromEnum(i.then);
            node.main_token = @intFromEnum(i.@"else");
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
            node.main_token = @intFromEnum(m.expr);
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
        .for_expr => |f| {
            node.tag = .for_expr;
            node.region = f.region;
            node.main_token = @intFromEnum(f.patt);
            node.data.lhs = @intFromEnum(f.expr);
            node.data.rhs = @intFromEnum(f.body);
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
            // Store extra data in typed list
            const extra_idx = store.where_mod_method_extra.items.len;
            try store.where_mod_method_extra.append(store.gpa, .{
                .name_tok = c.name_tok,
                .args = @intFromEnum(c.args),
                .ret_anno = @intFromEnum(c.ret_anno),
            });
            node.data.lhs = @intCast(extra_idx);
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
            // ext_kind: 0 = closed, 1 = anonymous open, 2 = named open
            const ext_kind: u2 = switch (tu.ext) {
                .closed => 0,
                .open => 1,
                .named => 2,
            };
            const rhs = AST.TypeAnno.TagUnionRhs{
                .ext_kind = ext_kind,
                .tags_len = @as(u30, @intCast(tu.tags.span.len)),
            };

            node.data.lhs = tu.tags.span.start;
            node.data.rhs = @as(u32, @bitCast(rhs));
            // Store named ext with OPTIONAL_VALUE_OFFSET, or 0 if not named
            node.main_token = if (tu.ext == .named) @intFromEnum(tu.ext.named) + OPTIONAL_VALUE_OFFSET else 0;
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

            // Use packed struct similar to TagUnionRhs
            const RecordRhs = packed struct { has_ext: u1, fields_len: u31 };
            const rhs = RecordRhs{
                .has_ext = if (r.ext != null) 1 else 0,
                .fields_len = @as(u31, @intCast(r.fields.span.len)),
            };

            node.data.lhs = r.fields.span.start;
            node.data.rhs = @as(u32, @bitCast(rhs));
            // Store ext with OPTIONAL_VALUE_OFFSET, or 0 if no ext
            node.main_token = if (r.ext) |ext| @intFromEnum(ext) + OPTIONAL_VALUE_OFFSET else 0;
        },
        .@"fn" => |f| {
            node.tag = .ty_fn;
            node.region = f.region;
            node.data.lhs = f.args.span.start;
            node.data.rhs = @bitCast(AST.TypeAnno.TypeAnnoFnRhs{
                .effectful = @intFromBool(f.effectful),
                .args_len = @intCast(f.args.span.len), // We hope a function has less than 2.147b args
            });
            node.main_token = @intFromEnum(f.ret);
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
    const node = store.nodes.get(root_node_idx);
    return .{
        .header = @enumFromInt(node.main_token),
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
            const extra = store.platform_header_extra.items[node.data.lhs];

            // Decode optional targets (0 = null, val = val - OPTIONAL_VALUE_OFFSET)
            const targets: ?AST.TargetsSection.Idx = if (extra.targets == 0) null else @enumFromInt(extra.targets - OPTIONAL_VALUE_OFFSET);

            return .{ .platform = .{
                .name = node.main_token,
                .requires_entries = .{ .span = .{
                    .start = extra.requires_start,
                    .len = extra.requires_len,
                } },
                .exposes = @enumFromInt(extra.exposes),
                .packages = @enumFromInt(extra.packages),
                .provides = @enumFromInt(extra.provides),
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
            const extra = store.import_extra.items[node.data.lhs];

            return AST.Statement{ .import = .{
                .module_name_tok = node.main_token,
                .qualifier_tok = if (rhs.qualified == 1) extra.qualifier_tok else null,
                .alias_tok = if (rhs.aliased == 1) extra.alias_tok else null,
                .exposes = .{ .span = .{
                    .start = extra.exposes_start,
                    .len = extra.exposes_len,
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
            // Read where and associated from typed list if present (main_token != 0)
            var where_clause: ?AST.Collection.Idx = null;
            var associated: ?AST.Associated = null;
            if (node.main_token != 0) {
                const extra = store.type_decl_extra.items[node.main_token - OPTIONAL_VALUE_OFFSET];
                if (extra.where_idx != 0) {
                    where_clause = @enumFromInt(extra.where_idx);
                }
                if (extra.has_associated == 1) {
                    associated = AST.Associated{
                        .statements = AST.Statement.Span{ .span = .{ .start = extra.statements_start, .len = extra.statements_len } },
                        .region = .{ .start = extra.region_start, .end = extra.region_end },
                    };
                }
            }

            return .{ .type_decl = .{
                .region = node.region,
                .header = @enumFromInt(node.data.lhs),
                .anno = @enumFromInt(node.data.rhs),
                .kind = .alias,
                .where = where_clause,
                .associated = associated,
            } };
        },
        .type_decl_nominal => {
            // Read where and associated from typed list if present (main_token != 0)
            var where_clause: ?AST.Collection.Idx = null;
            var associated: ?AST.Associated = null;
            if (node.main_token != 0) {
                const extra = store.type_decl_extra.items[node.main_token - OPTIONAL_VALUE_OFFSET];
                if (extra.where_idx != 0) {
                    where_clause = @enumFromInt(extra.where_idx);
                }
                if (extra.has_associated == 1) {
                    associated = AST.Associated{
                        .statements = AST.Statement.Span{ .span = .{ .start = extra.statements_start, .len = extra.statements_len } },
                        .region = .{ .start = extra.region_start, .end = extra.region_end },
                    };
                }
            }

            return .{ .type_decl = .{
                .region = node.region,
                .header = @enumFromInt(node.data.lhs),
                .anno = @enumFromInt(node.data.rhs),
                .kind = .nominal,
                .where = where_clause,
                .associated = associated,
            } };
        },
        .type_decl_opaque => {
            // Read where and associated from typed list if present (main_token != 0)
            var where_clause: ?AST.Collection.Idx = null;
            var associated: ?AST.Associated = null;
            if (node.main_token != 0) {
                const extra = store.type_decl_extra.items[node.main_token - OPTIONAL_VALUE_OFFSET];
                if (extra.where_idx != 0) {
                    where_clause = @enumFromInt(extra.where_idx);
                }
                if (extra.has_associated == 1) {
                    associated = AST.Associated{
                        .statements = AST.Statement.Span{ .span = .{ .start = extra.statements_start, .len = extra.statements_len } },
                        .region = .{ .start = extra.region_start, .end = extra.region_end },
                    };
                }
            }

            return .{ .type_decl = .{
                .region = node.region,
                .header = @enumFromInt(node.data.lhs),
                .anno = @enumFromInt(node.data.rhs),
                .kind = .@"opaque",
                .where = where_clause,
                .associated = associated,
            } };
        },
        .type_anno => {
            const is_var = (node.main_token & TYPE_ANNO_IS_VAR_BIT) != 0;
            const where_val = node.main_token & ~TYPE_ANNO_IS_VAR_BIT;
            return .{ .type_anno = .{
                .region = node.region,
                .name = node.data.lhs,
                .anno = @enumFromInt(node.data.rhs),
                .where = if (where_val != 0) @enumFromInt(where_val - OPTIONAL_VALUE_OFFSET) else null,
                .is_var = is_var,
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
            const extra = store.tag_patt_extra.items[node.data.rhs];

            return .{ .tag = .{
                .tag_tok = node.main_token,
                .args = .{ .span = .{
                    .start = node.data.lhs,
                    .len = extra.args_len,
                } },
                .qualifiers = .{ .span = .{
                    .start = extra.qualifiers_start,
                    .len = extra.qualifiers_len,
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
        .typed_int => {
            return .{ .typed_int = .{
                .token = node.main_token,
                .type_token = node.data.lhs,
                .region = node.region,
            } };
        },
        .typed_frac => {
            return .{ .typed_frac = .{
                .token = node.main_token,
                .type_token = node.data.lhs,
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
            // Convert OPTIONAL_VALUE_OFFSET back: 0 = null, otherwise idx = value - 1
            const ext = if (node.main_token == 0) null else @as(AST.Expr.Idx, @enumFromInt(node.main_token - OPTIONAL_VALUE_OFFSET));

            return .{ .record = .{
                .fields = .{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
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
                .body = @enumFromInt(node.main_token),
                .args = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
                .region = node.region,
            } };
        },
        .apply => {
            return .{ .apply = .{
                .@"fn" = @enumFromInt(node.main_token),
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
            return .{ .if_then_else = .{
                .region = node.region,
                .condition = @enumFromInt(node.data.lhs),
                .then = @enumFromInt(node.data.rhs),
                .@"else" = @enumFromInt(node.main_token),
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
                .expr = @enumFromInt(node.main_token),
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
        .for_expr => {
            return .{ .for_expr = .{
                .patt = @enumFromInt(node.main_token),
                .expr = @enumFromInt(node.data.lhs),
                .body = @enumFromInt(node.data.rhs),
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
            const extra = store.where_mod_method_extra.items[node.data.lhs];
            return .{ .mod_method = .{
                .region = node.region,
                .var_tok = node.main_token,
                .name_tok = extra.name_tok,
                .args = @enumFromInt(extra.args),
                .ret_anno = @enumFromInt(extra.ret_anno),
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

            // ext_kind: 0 = closed, 1 = anonymous open, 2 = named open
            const ext: AST.TypeAnno.TagUnionExt = switch (rhs.ext_kind) {
                0 => .closed,
                1 => .open,
                2 => .{ .named = @enumFromInt(node.main_token - OPTIONAL_VALUE_OFFSET) },
                3 => unreachable,
            };

            return .{ .tag_union = .{
                .region = node.region,
                .ext = ext,
                .tags = .{ .span = .{
                    .start = node.data.lhs,
                    .len = @intCast(rhs.tags_len),
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
            const RecordRhs = packed struct { has_ext: u1, fields_len: u31 };
            const rhs = @as(RecordRhs, @bitCast(node.data.rhs));
            const ext: ?AST.TypeAnno.Idx = if (rhs.has_ext == 1)
                @enumFromInt(node.main_token - OPTIONAL_VALUE_OFFSET)
            else
                null;

            return .{ .record = .{
                .region = node.region,
                .fields = .{ .span = .{
                    .start = node.data.lhs,
                    .len = @intCast(rhs.fields_len),
                } },
                .ext = ext,
            } };
        },
        .ty_fn => {
            const rhs = @as(AST.TypeAnno.TypeAnnoFnRhs, @bitCast(node.data.rhs));
            return .{ .@"fn" = .{
                .region = node.region,
                .ret = @enumFromInt(node.main_token),
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
    try store.scratch_exprs.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn exprSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.Expr.Span {
    const end = store.scratch_exprs.top();
    defer store.scratch_exprs.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.expr_span_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        try store.expr_span_data.append(store.gpa, store.scratch_exprs.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any ExprIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchExprsFrom(store: *NodeStore, start: u32) void {
    store.scratch_exprs.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn exprSlice(store: *const NodeStore, span: AST.Expr.Span) []AST.Expr.Idx {
    return store.expr_span_data.items[span.span.start..][0..span.span.len];
}

/// Returns the start position for a new Span of AST.Statement.Idxs in scratch
pub fn scratchStatementTop(store: *NodeStore) u32 {
    return store.scratch_statements.top();
}

/// Places a new AST.Statement.Idx in the scratch.
pub fn addScratchStatement(store: *NodeStore, idx: AST.Statement.Idx) std.mem.Allocator.Error!void {
    try store.scratch_statements.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn statementSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.Statement.Span {
    const end = store.scratch_statements.top();
    defer store.scratch_statements.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.statement_span_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        try store.statement_span_data.append(store.gpa, store.scratch_statements.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any StatementIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchStatementsFrom(store: *NodeStore, start: u32) void {
    store.scratch_statements.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn statementSlice(store: *const NodeStore, span: AST.Statement.Span) []AST.Statement.Idx {
    return store.statement_span_data.items[span.span.start..][0..span.span.len];
}

/// Returns the start position for a new Span of AST.Pattern.Idx in scratch
pub fn scratchPatternTop(store: *NodeStore) u32 {
    return store.scratch_patterns.top();
}

/// Places a new AST.Pattern.Idx in the scratch.
pub fn addScratchPattern(store: *NodeStore, idx: AST.Pattern.Idx) std.mem.Allocator.Error!void {
    try store.scratch_patterns.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn patternSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.Pattern.Span {
    const end = store.scratch_patterns.top();
    defer store.scratch_patterns.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.pattern_span_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        try store.pattern_span_data.append(store.gpa, store.scratch_patterns.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any PatternIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchPatternsFrom(store: *NodeStore, start: u32) void {
    store.scratch_patterns.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn patternSlice(store: *const NodeStore, span: AST.Pattern.Span) []AST.Pattern.Idx {
    return store.pattern_span_data.items[span.span.start..][0..span.span.len];
}

/// Returns a slice of the span items.
pub fn patternRecordFieldSlice(store: *const NodeStore, span: AST.PatternRecordField.Span) []AST.PatternRecordField.Idx {
    return store.pattern_record_field_span_data.items[span.span.start..][0..span.span.len];
}
/// Returns the start position for a new Span of patternRecordFieldIdxs in scratch
pub fn scratchPatternRecordFieldTop(store: *NodeStore) u32 {
    return store.scratch_pattern_record_fields.top();
}

/// Places a new AST.PatternRecordField.Idx in the scratch.
pub fn addScratchPatternRecordField(store: *NodeStore, idx: AST.PatternRecordField.Idx) std.mem.Allocator.Error!void {
    try store.scratch_pattern_record_fields.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn patternRecordFieldSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.PatternRecordField.Span {
    const end = store.scratch_pattern_record_fields.top();
    defer store.scratch_pattern_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.pattern_record_field_span_data.items.len));
    while (i < end) {
        try store.pattern_record_field_span_data.append(store.gpa, store.scratch_pattern_record_fields.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any AST.PatternRecordFieldIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchPatternRecordFieldsFrom(store: *NodeStore, start: u32) void {
    store.scratch_pattern_record_fields.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn recordFieldSlice(store: *const NodeStore, span: AST.RecordField.Span) []AST.RecordField.Idx {
    return store.record_field_span_data.items[span.span.start..][0..span.span.len];
}

/// Returns the start position for a new Span of recordFieldIdxs in scratch
pub fn scratchRecordFieldTop(store: *NodeStore) u32 {
    return store.scratch_record_fields.top();
}

/// Places a new AST.RecordField.Idx in the scratch.
pub fn addScratchRecordField(store: *NodeStore, idx: AST.RecordField.Idx) std.mem.Allocator.Error!void {
    try store.scratch_record_fields.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn recordFieldSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.RecordField.Span {
    const end = store.scratch_record_fields.top();
    defer store.scratch_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.record_field_span_data.items.len));
    while (i < end) {
        try store.record_field_span_data.append(store.gpa, store.scratch_record_fields.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any RecordFieldIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchRecordFieldsFrom(store: *NodeStore, start: u32) void {
    store.scratch_record_fields.clearFrom(start);
}

/// Returns the start position for a new Span of MatchBranchIdxs in scratch
pub fn scratchMatchBranchTop(store: *NodeStore) u32 {
    return store.scratch_match_branches.top();
}

/// Places a new AST.MatchBranch.Idx in the scratch.
pub fn addScratchMatchBranch(store: *NodeStore, idx: AST.MatchBranch.Idx) std.mem.Allocator.Error!void {
    try store.scratch_match_branches.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn matchBranchSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.MatchBranch.Span {
    const end = store.scratch_match_branches.top();
    defer store.scratch_match_branches.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.match_branch_span_data.items.len));
    while (i < end) {
        try store.match_branch_span_data.append(store.gpa, store.scratch_match_branches.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any MatchBranchIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchMatchBranchesFrom(store: *NodeStore, start: u32) void {
    store.scratch_match_branches.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn matchBranchSlice(store: *const NodeStore, span: AST.MatchBranch.Span) []AST.MatchBranch.Idx {
    return store.match_branch_span_data.items[span.span.start..][0..span.span.len];
}

/// Returns the start position for a new Span of typeAnnoIdxs in scratch
pub fn scratchTypeAnnoTop(store: *NodeStore) u32 {
    return store.scratch_type_annos.top();
}

/// Places a new AST.TypeAnno.Idx in the scratch.
pub fn addScratchTypeAnno(store: *NodeStore, idx: AST.TypeAnno.Idx) std.mem.Allocator.Error!void {
    try store.scratch_type_annos.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn typeAnnoSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.TypeAnno.Span {
    const end = store.scratch_type_annos.top();
    defer store.scratch_type_annos.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.type_anno_span_data.items.len));
    while (i < end) {
        try store.type_anno_span_data.append(store.gpa, store.scratch_type_annos.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any TypeAnnoIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchTypeAnnosFrom(store: *NodeStore, start: u32) void {
    store.scratch_type_annos.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn typeAnnoSlice(store: *const NodeStore, span: AST.TypeAnno.Span) []AST.TypeAnno.Idx {
    return store.type_anno_span_data.items[span.span.start..][0..span.span.len];
}

/// Returns the start position for a new Span of annoRecordFieldIdxs in scratch
pub fn scratchAnnoRecordFieldTop(store: *NodeStore) u32 {
    return store.scratch_anno_record_fields.top();
}

/// Places a new AST.AnnoRecordField.Idx in the scratch.
pub fn addScratchAnnoRecordField(store: *NodeStore, idx: AST.AnnoRecordField.Idx) std.mem.Allocator.Error!void {
    try store.scratch_anno_record_fields.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn annoRecordFieldSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.AnnoRecordField.Span {
    const end = store.scratch_anno_record_fields.top();
    defer store.scratch_anno_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.anno_record_field_span_data.items.len));
    while (i < end) {
        try store.anno_record_field_span_data.append(store.gpa, store.scratch_anno_record_fields.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any AnnoRecordFieldIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchAnnoRecordFieldsFrom(store: *NodeStore, start: u32) void {
    store.scratch_anno_record_fields.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn annoRecordFieldSlice(store: *const NodeStore, span: AST.AnnoRecordField.Span) []AST.AnnoRecordField.Idx {
    return store.anno_record_field_span_data.items[span.span.start..][0..span.span.len];
}

/// Returns the start position for a new Span of token_Idxs in scratch
pub fn scratchTokenTop(store: *NodeStore) u32 {
    return store.scratch_tokens.top();
}

/// Places a new Token.Idx in the scratch.
pub fn addScratchToken(store: *NodeStore, idx: Token.Idx) std.mem.Allocator.Error!void {
    try store.scratch_tokens.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn tokenSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!Token.Span {
    const end = store.scratch_tokens.top();
    defer store.scratch_tokens.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.token_span_data.items.len));
    while (i < end) {
        try store.token_span_data.append(store.gpa, store.scratch_tokens.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any TokenIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchTokensFrom(store: *NodeStore, start: u32) void {
    store.scratch_tokens.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn tokenSlice(store: *const NodeStore, span: Token.Span) []Token.Idx {
    return store.token_span_data.items[span.span.start..][0..span.span.len];
}

/// Returns the start position for a new Span of exposedItemIdxs in scratch
pub fn scratchExposedItemTop(store: *NodeStore) u32 {
    return store.scratch_exposed_items.top();
}

/// Places a new AST.ExposedItem.Idx in the scratch.
pub fn addScratchExposedItem(store: *NodeStore, idx: AST.ExposedItem.Idx) std.mem.Allocator.Error!void {
    try store.scratch_exposed_items.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn exposedItemSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.ExposedItem.Span {
    const end = store.scratch_exposed_items.top();
    defer store.scratch_exposed_items.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.exposed_item_span_data.items.len));
    while (i < end) {
        try store.exposed_item_span_data.append(store.gpa, store.scratch_exposed_items.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any ExposedItemIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchExposedItemsFrom(store: *NodeStore, start: u32) void {
    store.scratch_exposed_items.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn exposedItemSlice(store: *const NodeStore, span: AST.ExposedItem.Span) []AST.ExposedItem.Idx {
    return store.exposed_item_span_data.items[span.span.start..][0..span.span.len];
}

/// Returns the start position for a new Span of whereClauseIdxs in scratch
pub fn scratchWhereClauseTop(store: *NodeStore) u32 {
    return store.scratch_where_clauses.top();
}

/// Places a new AST.WhereClause.Idx in the scratch.
pub fn addScratchWhereClause(store: *NodeStore, idx: AST.WhereClause.Idx) std.mem.Allocator.Error!void {
    try store.scratch_where_clauses.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch
/// to the typed span data list.
pub fn whereClauseSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.WhereClause.Span {
    const end = store.scratch_where_clauses.top();
    defer store.scratch_where_clauses.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.where_clause_span_data.items.len));
    while (i < end) {
        try store.where_clause_span_data.append(store.gpa, store.scratch_where_clauses.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any WhereClauseIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchWhereClausesFrom(store: *NodeStore, start: u32) void {
    store.scratch_where_clauses.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn whereClauseSlice(store: *const NodeStore, span: AST.WhereClause.Span) []AST.WhereClause.Idx {
    return store.where_clause_span_data.items[span.span.start..][0..span.span.len];
}

// -----------------------------------------------------------------
// Target section functions
// -----------------------------------------------------------------

/// Adds a TargetsSection node and returns its index.
pub fn addTargetsSection(store: *NodeStore, section: AST.TargetsSection) std.mem.Allocator.Error!AST.TargetsSection.Idx {
    const node = Node{
        .tag = .targets_section,
        .main_token = section.files_path orelse 0,
        .data = .{
            .lhs = if (section.exe) |e| @intFromEnum(e) + OPTIONAL_VALUE_OFFSET else 0,
            .rhs = if (section.static_lib) |s| @intFromEnum(s) + OPTIONAL_VALUE_OFFSET else 0,
        },
        .region = section.region,
    };
    const nid = try store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a TargetLinkType node and returns its index.
pub fn addTargetLinkType(store: *NodeStore, link_type: AST.TargetLinkType) std.mem.Allocator.Error!AST.TargetLinkType.Idx {
    const node = Node{
        .tag = .target_link_type,
        .main_token = 0,
        .data = .{
            .lhs = link_type.entries.span.start,
            .rhs = link_type.entries.span.len,
        },
        .region = link_type.region,
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
            .lhs = entry.files.span.start,
            .rhs = entry.files.span.len,
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

/// Returns the start position for a new Span of TargetEntry.Idxs in scratch
pub fn scratchTargetEntryTop(store: *NodeStore) u32 {
    return store.scratch_target_entries.top();
}

/// Places a new AST.TargetEntry.Idx in the scratch.
pub fn addScratchTargetEntry(store: *NodeStore, idx: AST.TargetEntry.Idx) std.mem.Allocator.Error!void {
    try store.scratch_target_entries.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch to the typed span data list.
pub fn targetEntrySpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.TargetEntry.Span {
    const end = store.scratch_target_entries.top();
    defer store.scratch_target_entries.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.target_entry_span_data.items.len));
    while (i < end) {
        try store.target_entry_span_data.append(store.gpa, store.scratch_target_entries.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any TargetEntry.Idxs added to scratch from start until the end.
pub fn clearScratchTargetEntriesFrom(store: *NodeStore, start: u32) void {
    store.scratch_target_entries.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn targetEntrySlice(store: *const NodeStore, span: AST.TargetEntry.Span) []AST.TargetEntry.Idx {
    return store.target_entry_span_data.items[span.span.start..][0..span.span.len];
}

/// Returns the start position for a new Span of TargetFile.Idxs in scratch
pub fn scratchTargetFileTop(store: *NodeStore) u32 {
    return store.scratch_target_files.top();
}

/// Places a new AST.TargetFile.Idx in the scratch.
pub fn addScratchTargetFile(store: *NodeStore, idx: AST.TargetFile.Idx) std.mem.Allocator.Error!void {
    try store.scratch_target_files.append(idx);
}

/// Creates a new span starting at start. Moves the items from scratch to the typed span data list.
pub fn targetFileSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.TargetFile.Span {
    const end = store.scratch_target_files.top();
    defer store.scratch_target_files.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.target_file_span_data.items.len));
    while (i < end) {
        try store.target_file_span_data.append(store.gpa, store.scratch_target_files.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any TargetFile.Idxs added to scratch from start until the end.
pub fn clearScratchTargetFilesFrom(store: *NodeStore, start: u32) void {
    store.scratch_target_files.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn targetFileSlice(store: *const NodeStore, span: AST.TargetFile.Span) []AST.TargetFile.Idx {
    return store.target_file_span_data.items[span.span.start..][0..span.span.len];
}

/// Retrieves a TargetsSection from a stored node.
pub fn getTargetsSection(store: *const NodeStore, idx: AST.TargetsSection.Idx) AST.TargetsSection {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .targets_section);

    const files_path: ?Token.Idx = if (node.main_token == 0) null else node.main_token;
    const exe: ?AST.TargetLinkType.Idx = if (node.data.lhs == 0) null else @enumFromInt(node.data.lhs - OPTIONAL_VALUE_OFFSET);
    const static_lib: ?AST.TargetLinkType.Idx = if (node.data.rhs == 0) null else @enumFromInt(node.data.rhs - OPTIONAL_VALUE_OFFSET);

    return .{
        .files_path = files_path,
        .exe = exe,
        .static_lib = static_lib,
        .region = node.region,
    };
}

/// Retrieves a TargetLinkType from a stored node.
pub fn getTargetLinkType(store: *const NodeStore, idx: AST.TargetLinkType.Idx) AST.TargetLinkType {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .target_link_type);

    return .{
        .entries = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
        .region = node.region,
    };
}

/// Retrieves a TargetEntry from a stored node.
pub fn getTargetEntry(store: *const NodeStore, idx: AST.TargetEntry.Idx) AST.TargetEntry {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(idx)));
    std.debug.assert(node.tag == .target_entry);

    return .{
        .target = node.main_token,
        .files = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
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

/// Creates a new span starting at start. Moves the items from scratch to the typed span data list.
pub fn forClauseTypeAliasSpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.ForClauseTypeAlias.Span {
    const end = store.scratch_for_clause_type_aliases.top();
    defer store.scratch_for_clause_type_aliases.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.for_clause_type_alias_span_data.items.len));
    while (i < end) {
        try store.for_clause_type_alias_span_data.append(store.gpa, store.scratch_for_clause_type_aliases.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any ForClauseTypeAlias.Idxs added to scratch from start until the end.
pub fn clearScratchForClauseTypeAliasesFrom(store: *NodeStore, start: u32) void {
    store.scratch_for_clause_type_aliases.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn forClauseTypeAliasSlice(store: *const NodeStore, span: AST.ForClauseTypeAlias.Span) []AST.ForClauseTypeAlias.Idx {
    return store.for_clause_type_alias_span_data.items[span.span.start..][0..span.span.len];
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

/// Creates a new span starting at start. Moves the items from scratch to the typed span data list.
pub fn requiresEntrySpanFrom(store: *NodeStore, start: u32) std.mem.Allocator.Error!AST.RequiresEntry.Span {
    const end = store.scratch_requires_entries.top();
    defer store.scratch_requires_entries.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const span_start = @as(u32, @intCast(store.requires_entry_span_data.items.len));
    while (i < end) {
        try store.requires_entry_span_data.append(store.gpa, store.scratch_requires_entries.items.items[i]);
        i += 1;
    }
    return .{ .span = .{ .start = span_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any RequiresEntry.Idxs added to scratch from start until the end.
pub fn clearScratchRequiresEntriesFrom(store: *NodeStore, start: u32) void {
    store.scratch_requires_entries.clearFrom(start);
}

/// Returns a slice of the span items.
pub fn requiresEntrySlice(store: *const NodeStore, span: AST.RequiresEntry.Span) []AST.RequiresEntry.Idx {
    return store.requires_entry_span_data.items[span.span.start..][0..span.span.len];
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
