//! Typesafe access to an underlying SoA of Nodes.
//! This - along with the types used in its API - should
//! be the only way that other modules interact with
//! the AST.

const std = @import("std");
const base = @import("../../base.zig");
const collections = @import("../../collections.zig");

const AST = @import("AST.zig");
const Node = @import("Node.zig");
const Token = @import("tokenize.zig").Token;
const Region = AST.TokenizedRegion;
const Diagnostic = AST.Diagnostic;

const sexpr = base.sexpr;
const exitOnOom = collections.utils.exitOnOom;

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
scratch_when_branches: base.Scratch(AST.WhenBranch.Idx),
scratch_type_annos: base.Scratch(AST.TypeAnno.Idx),
scratch_anno_record_fields: base.Scratch(AST.AnnoRecordField.Idx),
scratch_exposed_items: base.Scratch(AST.ExposedItem.Idx),
scratch_where_clauses: base.Scratch(AST.WhereClause.Idx),

/// Initialize the store with an assumed capacity to
/// ensure resizing of underlying data structures happens
/// very rarely.
pub fn initCapacity(gpa: std.mem.Allocator, capacity: usize) NodeStore {
    var store: NodeStore = .{
        .gpa = gpa,
        .nodes = Node.List.initCapacity(gpa, capacity),
        .extra_data = std.ArrayListUnmanaged(u32).initCapacity(gpa, capacity / 2) catch |err| exitOnOom(err),
        .scratch_statements = base.Scratch(AST.Statement.Idx).init(gpa),
        .scratch_tokens = base.Scratch(Token.Idx).init(gpa),
        .scratch_exprs = base.Scratch(AST.Expr.Idx).init(gpa),
        .scratch_patterns = base.Scratch(AST.Pattern.Idx).init(gpa),
        .scratch_record_fields = base.Scratch(AST.RecordField.Idx).init(gpa),
        .scratch_pattern_record_fields = base.Scratch(AST.PatternRecordField.Idx).init(gpa),
        .scratch_when_branches = base.Scratch(AST.WhenBranch.Idx).init(gpa),
        .scratch_type_annos = base.Scratch(AST.TypeAnno.Idx).init(gpa),
        .scratch_anno_record_fields = base.Scratch(AST.AnnoRecordField.Idx).init(gpa),
        .scratch_exposed_items = base.Scratch(AST.ExposedItem.Idx).init(gpa),
        .scratch_where_clauses = base.Scratch(AST.WhereClause.Idx).init(gpa),
    };

    _ = store.nodes.append(gpa, .{
        .tag = .root,
        .main_token = 0,
        .data = .{ .untyped = .{ .lhs = 0, .rhs = 0 } },
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
    store.scratch_when_branches.deinit(store.gpa);
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
    store.scratch_when_branches.clearFrom(0);
    store.scratch_type_annos.clearFrom(0);
    store.scratch_anno_record_fields.clearFrom(0);
    store.scratch_exposed_items.clearFrom(0);
    store.scratch_where_clauses.clearFrom(0);
}

/// Prints debug information about all nodes and scratch buffers in the store.
pub fn debug(store: *NodeStore) void {
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
    std.debug.print("Scratch when branches: {any}\n", .{store.scratch_when_branches.items});
    std.debug.print("Scratch type annos: {any}\n", .{store.scratch_type_annos.items});
    std.debug.print("Scratch anno record fields: {any}\n", .{store.scratch_anno_record_fields.items});
    std.debug.print("Scratch exposes items: {any}\n", .{store.scratch_exposed_items.items});
    std.debug.print("Scratch where clauses: {any}\n", .{store.scratch_where_clauses.items});
    std.debug.print("==> IR.NodeStore DEBUG <==\n\n", .{});
}

// ------------------------------------------------------------------------
// Creation API - All nodes should be added using these functions
// ------------------------------------------------------------------------

/// Any node type can be malformed, but must come with a diagnostic reason
pub fn addMalformed(store: *NodeStore, comptime t: type, reason: Diagnostic.Tag, region: Region) t {
    const nid = store.nodes.append(store.gpa, .{
        .tag = .malformed,
        .main_token = 0,
        .data = .{ .untyped = .{ .lhs = @intFromEnum(reason), .rhs = 0 } },
        .region = region,
    });
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a file node to the store.
pub fn addFile(store: *NodeStore, file: AST.File) void {
    store.extra_data.append(store.gpa, @intFromEnum(file.header)) catch |err| exitOnOom(err);
    store.nodes.set(@enumFromInt(0), .{
        .tag = .root,
        .main_token = 0,
        .data = .{ .untyped = .{ .lhs = file.statements.span.start, .rhs = file.statements.span.len } },
        .region = file.region,
    });
}

/// Adds a AST.Collection node with the specified tag and returns its index.
pub fn addCollection(store: *NodeStore, tag: Node.Tag, collection: AST.Collection) AST.Collection.Idx {
    const nid = store.nodes.append(store.gpa, Node{
        .tag = tag,
        .main_token = 0,
        .data = .{
            .untyped = .{
                .lhs = collection.span.start,
                .rhs = collection.span.len,
            },
        },
        .region = collection.region,
    });
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a header node of any type (app, module, hosted, package, platform) and returns its index.
pub fn addHeader(store: *NodeStore, header: AST.Header) AST.Header.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .untyped = .{
                .lhs = 0,
                .rhs = 0,
            },
        },
        .region = AST.TokenizedRegion.empty(),
    };
    switch (header) {
        .app => |app| {
            node.tag = .app_header;
            // TODO this doesn't seem right, were giving it a record_field index instead of a token index
            node.main_token = @intFromEnum(app.platform_idx);
            // Store provides collection
            node.data.untyped.lhs = @intFromEnum(app.provides);
            node.data.untyped.rhs = @intFromEnum(app.packages);
            node.region = app.region;

            store.extra_data.append(store.gpa, @intFromEnum(app.platform_idx)) catch |err| exitOnOom(err);
        },
        .module => |mod| {
            node.tag = .module_header;
            node.data.untyped.lhs = @intFromEnum(mod.exposes);
            node.region = mod.region;
        },
        .hosted => |hosted| {
            node.tag = .hosted_header;
            node.data.untyped.lhs = @intFromEnum(hosted.exposes);
            node.region = hosted.region;
        },
        .package => |package| {
            node.tag = .package_header;
            node.data.untyped.lhs = @intFromEnum(package.exposes);
            node.data.untyped.rhs = @intFromEnum(package.packages);
            node.region = package.region;
        },
        .platform => |platform| {
            node.tag = .platform_header;
            node.main_token = platform.name;

            const ed_start = store.extra_data.items.len;
            store.extra_data.append(store.gpa, @intFromEnum(platform.requires_rigids)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(platform.requires_signatures)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(platform.exposes)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(platform.packages)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(platform.provides)) catch |err| exitOnOom(err);
            const ed_len = store.extra_data.items.len - ed_start;

            node.data.untyped.lhs = @intCast(ed_start);
            node.data.untyped.rhs = @intCast(ed_len);

            node.region = platform.region;
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }

    const node_idx = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(node_idx));
}

/// Adds an exposed item node (function, value, type, etc.) and returns its index.
pub fn addExposedItem(store: *NodeStore, item: AST.ExposedItem) AST.ExposedItem.Idx {
    var node = Node{
        .tag = .malformed,
        .main_token = 0,
        .data = .{
            .untyped = .{
                .lhs = 0,
                .rhs = 0,
            },
        },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (item) {
        .lower_ident => |i| {
            node.tag = .exposed_item_lower;
            node.main_token = i.ident;
            if (i.as) |a| {
                std.debug.assert(a > 0);
                node.data.untyped.lhs = a;
                node.data.untyped.rhs = 1;
            }
            node.region = i.region;
        },
        .upper_ident => |i| {
            node.tag = .exposed_item_upper;
            node.main_token = i.ident;
            if (i.as) |a| {
                std.debug.assert(a > 0);
                node.data.untyped.lhs = a;
                node.data.untyped.rhs = 1;
            }
            node.region = i.region;
        },
        .upper_ident_star => |i| {
            node.tag = .exposed_item_upper_star;
            node.main_token = i.ident;
            node.region = i.region;
        },
    }

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a statement node (value def, type def, import, etc.) and returns its index.
pub fn addStatement(store: *NodeStore, statement: AST.Statement) AST.Statement.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .untyped = .{
                .lhs = 0,
                .rhs = 0,
            },
        },
        .region = AST.TokenizedRegion.empty(),
    };
    switch (statement) {
        .decl => |d| {
            node.tag = .decl;
            node.data = .{ .decl = .{
                .pattern = @intFromEnum(d.pattern),
                .body = @intFromEnum(d.body),
            } };
            node.region = d.region;
        },
        .@"var" => |v| {
            node.tag = .@"var";
            node.main_token = v.name;
            node.data = .{ .var_stmt = .{
                .body = @intFromEnum(v.body),
            } };
            node.region = v.region;
        },
        .expr => |expr| {
            node.tag = .expr;
            node.data.untyped.lhs = @intFromEnum(expr.expr);
            node.region = expr.region;
        },
        .crash => |c| {
            node.tag = .crash;
            node.data.untyped.lhs = @intFromEnum(c.expr);
            node.region = c.region;
        },
        .expect => |e| {
            node.tag = .expect;
            node.data.untyped.lhs = @intFromEnum(e.body);
            node.region = e.region;
        },
        .@"for" => |f| {
            node.tag = .@"for";
            node.main_token = @intFromEnum(f.patt);
            node.data.untyped.lhs = @intFromEnum(f.expr);
            node.data.untyped.rhs = @intFromEnum(f.body);
            node.region = f.region;
        },
        .@"return" => |r| {
            node.tag = .@"return";
            node.data.untyped.lhs = @intFromEnum(r.expr);
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
            var ed_start: u32 = i.exposes.span.start;
            if (i.qualifier_tok) |tok| {
                rhs.qualified = 1;
                if (ed_start == 0) {
                    ed_start = @intCast(store.extra_data.items.len);
                }

                store.extra_data.append(store.gpa, tok) catch |err| exitOnOom(err);
            }
            if (i.alias_tok) |tok| {
                rhs.aliased = 1;
                if (ed_start == 0) {
                    ed_start = @intCast(store.extra_data.items.len);
                }
                store.extra_data.append(store.gpa, tok) catch |err| exitOnOom(err);
            }
            const packed_info = @as(u32, @bitCast(rhs));
            node.data = .{ .import_stmt = .{
                .extra_data_start = if (packed_info > 0) ed_start else 0,
                .packed_info = packed_info,
            } };
        },
        .type_decl => |d| {
            node.tag = .type_decl;
            if (d.kind == .nominal) {
                node.tag = .type_decl_nominal;
            }
            node.region = d.region;
            node.data = .{ .type_decl = .{
                .header = @intFromEnum(d.header),
                .anno = @intFromEnum(d.anno),
            } };
            if (d.where) |w| {
                node.main_token = @intFromEnum(w);
            }
        },
        .type_anno => |a| {
            node.tag = .type_anno;
            node.region = a.region;
            node.data.untyped.lhs = a.name;
            node.data.untyped.rhs = @intFromEnum(a.anno);
            if (a.where) |w| {
                node.main_token = @intFromEnum(w);
            }
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }
    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a pattern node (identifier, literal, record destructure, etc.) and returns its index.
pub fn addPattern(store: *NodeStore, pattern: AST.Pattern) AST.Pattern.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .untyped = .{
                .lhs = 0,
                .rhs = 0,
            },
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
            node.tag = .tag_patt;
            node.region = t.region;
            node.main_token = t.tag_tok;
            node.data = .{
                .tag = .{
                    .args_start = t.args.span.start,
                    .args_len = t.args.span.len,
                },
            };
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
            node.data.untyped.lhs = @intFromEnum(s.expr);
        },
        .record => |r| {
            node.tag = .record_patt;
            node.region = r.region;
            node.data = .{
                .record = .{
                    .fields_start = r.fields.span.start,
                    .fields_len = r.fields.span.len,
                },
            };
        },
        .list => |l| {
            node.tag = .list_patt;
            node.region = l.region;
            node.data = .{
                .list = .{
                    .items_start = l.patterns.span.start,
                    .items_len = l.patterns.span.len,
                },
            };
        },
        .list_rest => |r| {
            node.tag = .list_rest_patt;
            node.region = r.region;
            if (r.name) |n| {
                node.data.untyped.lhs = n;
            }
        },
        .tuple => |t| {
            node.tag = .tuple_patt;
            node.region = t.region;
            node.data = .{
                .tuple = .{
                    .items_start = t.patterns.span.start,
                    .items_len = t.patterns.span.len,
                },
            };
        },
        .underscore => |u| {
            node.tag = .underscore_patt;
            node.region = u.region;
        },
        .alternatives => |a| {
            std.debug.assert(a.patterns.span.len > 1);
            node.region = a.region;
            node.tag = .alternatives_patt;
            node.data.untyped.lhs = a.patterns.span.start;
            node.data.untyped.rhs = a.patterns.span.len;
        },
        .as => |a| {
            node.region = a.region;
            node.tag = .as_patt;
            node.main_token = a.name;
            node.data.untyped.lhs = @intFromEnum(a.pattern);
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }
    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds an expression node (literal, variable, call, record, etc.) and returns its index.
pub fn addExpr(store: *NodeStore, expr: AST.Expr) AST.Expr.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{ .untyped = .{ .lhs = 0, .rhs = 0 } },
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

        .string_part => |e| {
            node.tag = .string_part;
            node.region = e.region;
            node.main_token = e.token;
        },
        .string => |e| {
            node.tag = .string;
            node.region = e.region;
            node.main_token = e.token;
            node.data.untyped.lhs = e.parts.span.start;
            node.data.untyped.rhs = e.parts.span.len;
        },
        .list => |l| {
            node.tag = .list;
            node.region = l.region;
            node.main_token = l.region.start;
            node.data = .{
                .list = .{
                    .items_start = l.items.span.start,
                    .items_len = l.items.span.len,
                },
            };
        },
        .tuple => |t| {
            node.tag = .tuple;
            node.region = t.region;
            node.data = .{
                .tuple = .{
                    .items_start = t.items.span.start,
                    .items_len = t.items.span.len,
                },
            };
        },
        .record => |r| {
            node.tag = .record;
            node.region = r.region;
            node.data = .{
                .record = .{
                    .fields_start = r.fields.span.start,
                    .fields_len = r.fields.span.len,
                },
            };
        },
        .tag => |e| {
            node.tag = .tag;
            node.region = e.region;
            node.main_token = e.token;
            node.data = .{
                .tag = .{
                    .args_start = 0,
                    .args_len = 0,
                },
            };
        },
        .lambda => |l| {
            node.tag = .lambda;
            node.region = l.region;
            node.data.untyped.lhs = l.args.span.start;
            node.data.untyped.rhs = l.args.span.len;
            const body_idx = store.extra_data.items.len;
            store.extra_data.append(store.gpa, @intFromEnum(l.body)) catch |err| exitOnOom(err);
            node.main_token = @as(u32, @intCast(body_idx));
        },
        .apply => |app| {
            node.tag = .apply;
            node.region = app.region;
            node.data.untyped.lhs = app.args.span.start;
            node.data.untyped.rhs = app.args.span.len;
            const fn_ed_idx = store.extra_data.items.len;
            store.extra_data.append(store.gpa, @intFromEnum(app.@"fn")) catch |err| exitOnOom(err);
            node.main_token = @as(u32, @intCast(fn_ed_idx));
        },
        .record_updater => |_| {},
        .field_access => |fa| {
            node.tag = .field_access;
            node.region = fa.region;
            node.data = .{
                .field_access = .{
                    .left = @intFromEnum(fa.left),
                    .right = @intFromEnum(fa.right),
                },
            };
        },
        .local_dispatch => |ld| {
            node.tag = .local_dispatch;
            node.region = ld.region;
            node.main_token = ld.operator;
            node.data.untyped.lhs = @intFromEnum(ld.left);
            node.data.untyped.rhs = @intFromEnum(ld.right);
        },
        .bin_op => |op| {
            node.tag = .bin_op;
            node.region = op.region;
            node.main_token = op.operator;
            node.data = .{
                .bin_op = .{
                    .left = @intFromEnum(op.left),
                    .right = @intFromEnum(op.right),
                },
            };
        },
        .suffix_single_question => |op| {
            node.tag = .suffix_single_question;
            node.region = op.region;
            node.data.untyped.lhs = @intFromEnum(op.expr);
        },
        .unary_op => |u| {
            node.tag = .unary_op;
            node.region = u.region;
            node.main_token = u.operator;
            node.data = .{
                .unary_op = .{
                    .expr = @intFromEnum(u.expr),
                },
            };
        },
        .if_then_else => |i| {
            node.tag = .if_then_else;
            node.region = i.region;
            node.data = .{
                .if_then_else = .{
                    .condition = @intFromEnum(i.condition),
                    .extra_data_idx = @as(u32, @intCast(store.extra_data.items.len)),
                },
            };
            store.extra_data.append(store.gpa, @intFromEnum(i.then)) catch |err| exitOnOom(err);
            store.extra_data.append(store.gpa, @intFromEnum(i.@"else")) catch |err| exitOnOom(err);
        },
        .match => |m| {
            node.tag = .match;
            node.region = m.region;
            node.data = .{
                .match_expr = .{
                    .expr_extra_data_idx = @as(u32, @intCast(store.extra_data.items.len)),
                    .branches_start = m.branches.span.start,
                    .branches_len = m.branches.span.len,
                },
            };
            store.extra_data.append(store.gpa, @intFromEnum(m.expr)) catch |err| exitOnOom(err);
        },
        .ident => |id| {
            node.tag = .ident;
            node.region = id.region;
            node.main_token = id.token;
            if (id.qualifier) |qualifier| {
                node.data.untyped.lhs = qualifier;
                node.data.untyped.rhs = 1;
            }
        },
        .dbg => |d| {
            node.tag = .dbg;
            node.region = d.region;
            node.data.untyped.lhs = @intFromEnum(d.expr);
        },
        .record_builder => |_| {},
        .block => |body| {
            node.tag = .block;
            node.region = body.region;
            node.main_token = 0;
            node.data.untyped.lhs = body.statements.span.start;
            node.data.untyped.rhs = body.statements.span.len;
        },
        .ellipsis => |e| {
            node.tag = .ellipsis;
            node.region = e.region;
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }
    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn addPatternRecordField(store: *NodeStore, field: AST.PatternRecordField) AST.PatternRecordField.Idx {
    var node = Node{
        .tag = .record_field_patt,
        .main_token = field.name,
        .data = .{
            .untyped = .{
                .lhs = @intFromBool(field.rest),
                .rhs = 0,
            },
        },
        .region = field.region,
    };
    if (field.value) |value| {
        node.data.untyped.rhs = @intFromEnum(value);
    }
    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn getPatternRecordField(store: *NodeStore, field: AST.PatternRecordField.Idx) AST.PatternRecordField {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(field)));
    return .{
        .name = node.main_token,
        .value = if (node.data.untyped.rhs == 0) null else @enumFromInt(node.data.untyped.rhs),
        .rest = node.data.untyped.lhs == 1,
        .region = node.region,
    };
}

/// TODO
pub fn addRecordField(store: *NodeStore, field: AST.RecordField) AST.RecordField.Idx {
    var node = Node{
        .tag = .statement,
        .main_token = 0,
        .data = .{
            .untyped = .{
                .lhs = 0,
                .rhs = 0,
            },
        },
        .region = field.region,
    };
    node.tag = .record_field;
    node.main_token = field.name;
    if (field.value) |v| {
        node.data.untyped.lhs = @intFromEnum(v);
    }
    if (field.optional) {
        node.data.untyped.rhs = 1;
    }

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn addWhenBranch(store: *NodeStore, branch: AST.WhenBranch) AST.WhenBranch.Idx {
    const node = Node{
        .tag = .branch,
        .main_token = 0,
        .data = .{
            .untyped = .{
                .lhs = @intFromEnum(branch.pattern),
                .rhs = @intFromEnum(branch.body),
            },
        },
        .region = branch.region,
    };

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn addTypeHeader(store: *NodeStore, header: AST.TypeHeader) AST.TypeHeader.Idx {
    var node = Node{
        .tag = .ty_header,
        .main_token = header.name,
        .data = .{
            .untyped = .{
                .lhs = 0,
                .rhs = 0,
            },
        },
        .region = header.region,
    };

    node.data.untyped.lhs = header.args.span.start;
    node.data.untyped.rhs = header.args.span.len;

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn addAnnoRecordField(store: *NodeStore, field: AST.AnnoRecordField) AST.AnnoRecordField.Idx {
    const node = Node{
        .tag = .ty_record_field,
        .main_token = 0,
        .data = .{
            .untyped = .{
                .lhs = field.name,
                .rhs = @intFromEnum(field.ty),
            },
        },
        .region = field.region,
    };

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// Adds a WhereClause node to the store, returning a type-safe index to the node.
pub fn addWhereClause(store: *NodeStore, clause: AST.WhereClause) AST.WhereClause.Idx {
    var node = Node{
        .tag = .where_alias,
        .main_token = 0,
        .data = .{
            .untyped = .{
                .lhs = 0,
                .rhs = 0,
            },
        },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (clause) {
        .alias => |c| {
            node.tag = .where_alias;
            node.region = c.region;
            node.main_token = c.var_tok;
            node.data.untyped.lhs = c.alias_tok;
        },
        .method => |c| {
            node.tag = .where_method;
            node.region = c.region;
            node.main_token = c.var_tok;
            const ed_start = store.extra_data.items.len;
            store.extra_data.append(store.gpa, c.name_tok) catch |e| exitOnOom(e);
            store.extra_data.append(store.gpa, @intFromEnum(c.args)) catch |e| exitOnOom(e);
            store.extra_data.append(store.gpa, @intFromEnum(c.ret_anno)) catch |e| exitOnOom(e);
            node.data.untyped.lhs = @intCast(ed_start);
        },
        .mod_method => |c| {
            node.tag = .where_mod_method;
            node.region = c.region;
            node.main_token = c.var_tok;
            const ed_start = store.extra_data.items.len;
            store.extra_data.append(store.gpa, c.name_tok) catch |e| exitOnOom(e);
            store.extra_data.append(store.gpa, @intFromEnum(c.args)) catch |e| exitOnOom(e);
            store.extra_data.append(store.gpa, @intFromEnum(c.ret_anno)) catch |e| exitOnOom(e);
            node.data.untyped.lhs = @intCast(ed_start);
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

/// TODO
pub fn addTypeAnno(store: *NodeStore, anno: AST.TypeAnno) AST.TypeAnno.Idx {
    var node = Node{
        .tag = .branch,
        .main_token = 0,
        .data = .{
            .untyped = .{
                .lhs = 0,
                .rhs = 0,
            },
        },
        .region = AST.TokenizedRegion.empty(),
    };

    switch (anno) {
        .apply => |a| {
            node.tag = .ty_apply;
            node.region = a.region;
            node.data.untyped.lhs = a.args.span.start;
            node.data.untyped.rhs = a.args.span.len;
        },
        .ty_var => |v| {
            node.tag = .ty_var;
            node.region = v.region;
            node.main_token = v.tok;
        },
        .underscore => |u| {
            node.tag = .ty_underscore;
            node.region = u.region;
        },
        .ty => |t| {
            node.tag = .ty_ty;
            node.region = t.region;
            node.main_token = t.region.start;
            node.data.untyped.rhs = @bitCast(t.ident);
        },
        .mod_ty => |t| {
            node.tag = .ty_mod_ty;
            node.region = t.region;
            node.main_token = t.region.start;
            node.data.untyped.lhs = @bitCast(t.mod_ident);
            node.data.untyped.rhs = @bitCast(t.ty_ident);
        },
        .tag_union => |tu| {
            node.tag = .ty_union;
            node.region = tu.region;
            node.data.untyped.lhs = tu.tags.span.start;
            var rhs = AST.TypeAnno.TagUnionRhs{
                .open = 0,
                .tags_len = @as(u31, @intCast(tu.tags.span.len)),
            };
            if (tu.open_anno) |a| {
                rhs.open = 1;
                store.extra_data.append(store.gpa, @intFromEnum(a)) catch |err| exitOnOom(err);
            }
            node.data.untyped.rhs = @as(u32, @bitCast(rhs));
        },
        .tuple => |t| {
            node.tag = .ty_tuple;
            node.region = t.region;
            node.data.untyped.lhs = t.annos.span.start;
            node.data.untyped.rhs = t.annos.span.len;
        },
        .record => |r| {
            node.tag = .ty_record;
            node.region = r.region;
            node.data.untyped.lhs = r.fields.span.start;
            node.data.untyped.rhs = r.fields.span.len;
        },
        .@"fn" => |f| {
            node.tag = .ty_fn;
            node.region = f.region;
            node.data.untyped.lhs = f.args.span.start;
            node.data.untyped.rhs = @bitCast(AST.TypeAnno.TypeAnnoFnRhs{
                .effectful = @intFromBool(f.effectful),
                .args_len = @intCast(f.args.span.len), // We hope a function has less than 2.147b args
            });
            const ret_idx = store.extra_data.items.len;
            store.extra_data.append(store.gpa, @intFromEnum(f.ret)) catch |err| exitOnOom(err);
            node.main_token = @intCast(ret_idx);
        },
        .parens => |p| {
            node.tag = .ty_parens;
            node.region = p.region;
            node.data.untyped.lhs = @intFromEnum(p.anno);
        },
        .malformed => {
            @panic("Use addMalformed instead");
        },
    }

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

// ------------------------------------------------------------------------
// Read API - All nodes should be accessed using these functions
// ------------------------------------------------------------------------

/// TODO
pub fn getFile(store: *NodeStore) AST.File {
    const node = store.nodes.get(@enumFromInt(0));
    const header_ed_idx = @as(usize, @intCast(node.data.untyped.lhs + node.data.untyped.rhs));
    const header = store.extra_data.items[header_ed_idx];
    return .{
        .header = @enumFromInt(header),
        .statements = .{ .span = .{ .start = node.data.untyped.lhs, .len = node.data.untyped.rhs } },
        .region = node.region,
    };
}

/// Retrieves collection data from a stored collection node.
pub fn getCollection(store: *NodeStore, collection_idx: AST.Collection.Idx) AST.Collection {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(collection_idx)));
    return .{
        .span = .{
            .start = node.data.untyped.lhs,
            .len = node.data.untyped.rhs,
        },
        .region = node.region,
    };
}

/// Retrieves header data from a stored header node, reconstructing the appropriate header type.
pub fn getHeader(store: *NodeStore, header_idx: AST.Header.Idx) AST.Header {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    switch (node.tag) {
        .app_header => {
            return .{ .app = .{
                .platform_idx = @enumFromInt(node.main_token),
                .provides = @enumFromInt(node.data.untyped.lhs),
                .packages = @enumFromInt(node.data.untyped.rhs),
                .region = node.region,
            } };
        },
        .module_header => {
            return .{ .module = .{
                .exposes = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        .hosted_header => {
            return .{ .hosted = .{
                .exposes = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        .package_header => {
            return .{ .package = .{
                .exposes = @enumFromInt(node.data.untyped.lhs),
                .packages = @enumFromInt(node.data.untyped.rhs),
                .region = node.region,
            } };
        },
        .platform_header => {
            const ed_start = node.data.untyped.lhs;
            std.debug.assert(node.data.untyped.rhs == 5);

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
                .reason = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid header tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves exposed item data from a stored exposed item node.
pub fn getExposedItem(store: *NodeStore, exposed_item_idx: AST.ExposedItem.Idx) AST.ExposedItem {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(exposed_item_idx)));
    switch (node.tag) {
        .exposed_item_lower => {
            if (node.data.untyped.rhs == 1) {
                return .{ .lower_ident = .{
                    .region = node.region,
                    .ident = node.main_token,
                    .as = node.data.untyped.lhs,
                } };
            }
            return .{ .lower_ident = .{
                .region = node.region,
                .ident = node.main_token,
                .as = null,
            } };
        },
        .exposed_item_upper => {
            if (node.data.untyped.rhs == 1) {
                return .{ .upper_ident = .{
                    .region = node.region,
                    .ident = node.main_token,
                    .as = node.data.untyped.lhs,
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
        else => {
            std.debug.panic("Expected a valid exposed item tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves statement data from a stored statement node, reconstructing the appropriate statement type.
pub fn getStatement(store: *NodeStore, statement_idx: AST.Statement.Idx) AST.Statement {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(statement_idx)));
    switch (node.tag) {
        .decl => {
            return .{ .decl = .{
                .pattern = @enumFromInt(node.data.decl.pattern),
                .body = @enumFromInt(node.data.decl.body),
                .region = node.region,
            } };
        },
        .@"var" => {
            return .{ .@"var" = .{
                .name = node.main_token,
                .body = @enumFromInt(node.data.var_stmt.body),
                .region = node.region,
            } };
        },
        .expr => {
            return .{ .expr = .{
                .expr = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        .import => {
            const rhs = @as(AST.ImportRhs, @bitCast(node.data.import_stmt.packed_info));
            var extra_data_pos = node.data.import_stmt.extra_data_start + rhs.num_exposes;
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
                    .start = node.data.import_stmt.extra_data_start,
                    .len = rhs.num_exposes,
                } },
                .region = node.region,
            } };
        },
        .expect => {
            return .{ .expect = .{
                .body = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        .@"for" => {
            return .{ .@"for" = .{
                .patt = @enumFromInt(node.main_token),
                .expr = @enumFromInt(node.data.untyped.lhs),
                .body = @enumFromInt(node.data.untyped.rhs),
                .region = node.region,
            } };
        },
        .crash => {
            return .{ .crash = .{
                .expr = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        .@"return" => {
            return .{ .@"return" = .{
                .expr = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        .type_decl => {
            return .{ .type_decl = .{
                .region = node.region,
                .header = @enumFromInt(node.data.type_decl.header),
                .anno = @enumFromInt(node.data.type_decl.anno),
                .kind = .alias,
                .where = if (node.main_token != 0) @enumFromInt(node.main_token) else null,
            } };
        },
        .type_decl_nominal => {
            return .{ .type_decl = .{
                .region = node.region,
                .header = @enumFromInt(node.data.type_decl.header),
                .anno = @enumFromInt(node.data.type_decl.anno),
                .kind = .nominal,
                .where = if (node.main_token != 0) @enumFromInt(node.main_token) else null,
            } };
        },
        .type_anno => {
            return .{ .type_anno = .{
                .region = node.region,
                .name = node.data.untyped.lhs,
                .anno = @enumFromInt(node.data.untyped.rhs),
                .where = if (node.main_token != 0) @enumFromInt(node.main_token) else null,
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid statement tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves pattern data from a stored pattern node, reconstructing the appropriate pattern type.
pub fn getPattern(store: *NodeStore, pattern_idx: AST.Pattern.Idx) AST.Pattern {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(pattern_idx)));
    switch (node.tag) {
        .ident_patt => {
            return .{ .ident = .{
                .ident_tok = node.main_token,
                .region = node.region,
            } };
        },
        .tag_patt => {
            return .{ .tag = .{
                .tag_tok = node.main_token,
                .args = .{ .span = .{
                    .start = node.data.tag.args_start,
                    .len = node.data.tag.args_len,
                } },
                .region = node.region,
            } };
        },
        .string_patt => {
            return .{ .string = .{
                .string_tok = node.main_token,
                .region = node.region,
                .expr = @enumFromInt(node.data.untyped.lhs),
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
                    .start = node.data.record.fields_start,
                    .len = node.data.record.fields_len,
                } },
            } };
        },
        .list_patt => {
            return .{ .list = .{
                .region = node.region,
                .patterns = .{ .span = .{
                    .start = node.data.list.items_start,
                    .len = node.data.list.items_len,
                } },
            } };
        },
        .list_rest_patt => {
            return .{ .list_rest = .{
                .region = node.region,
                .name = if (node.data.untyped.lhs == 0) null else node.data.untyped.lhs,
            } };
        },
        .tuple_patt => {
            return .{ .tuple = .{
                .region = node.region,
                .patterns = .{ .span = .{
                    .start = node.data.tuple.items_start,
                    .len = node.data.tuple.items_len,
                } },
            } };
        },
        .alternatives_patt => {
            return .{ .alternatives = .{
                .region = node.region,
                .patterns = .{ .span = .{
                    .start = node.data.untyped.lhs,
                    .len = node.data.untyped.rhs,
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
                .pattern = @enumFromInt(node.data.untyped.lhs),
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid pattern tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves expression data from a stored expression node, reconstructing the appropriate expression type.
pub fn getExpr(store: *NodeStore, expr_idx: AST.Expr.Idx) AST.Expr {
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
        .ident => {
            var qualifier: ?Token.Idx = null;
            if (node.data.untyped.rhs == 1) {
                qualifier = node.data.untyped.lhs;
            }
            return .{ .ident = .{
                .token = node.main_token,
                .qualifier = qualifier,
                .region = node.region,
            } };
        },
        .tag => {
            return .{ .tag = .{
                .region = node.region,
                .token = node.main_token,
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
                    .start = node.data.untyped.lhs,
                    .len = node.data.untyped.rhs,
                } },
                .region = node.region,
            } };
        },
        .list => {
            return .{ .list = .{
                .items = .{ .span = base.DataSpan{
                    .start = node.data.list.items_start,
                    .len = node.data.list.items_len,
                } },
                .region = node.region,
            } };
        },
        .tuple => {
            return .{ .tuple = .{
                .items = .{ .span = base.DataSpan{
                    .start = node.data.tuple.items_start,
                    .len = node.data.tuple.items_len,
                } },
                .region = node.region,
            } };
        },
        .record => {
            return .{ .record = .{
                .fields = .{ .span = .{
                    .start = node.data.record.fields_start,
                    .len = node.data.record.fields_len,
                } },
                .region = node.region,
            } };
        },
        .field_access => {
            return .{ .field_access = .{
                .left = @enumFromInt(node.data.field_access.left),
                .right = @enumFromInt(node.data.field_access.right),
                .operator = node.main_token,
                .region = node.region,
            } };
        },
        .local_dispatch => {
            return .{ .local_dispatch = .{
                .left = @enumFromInt(node.data.untyped.lhs),
                .right = @enumFromInt(node.data.untyped.rhs),
                .operator = node.main_token,
                .region = node.region,
            } };
        },
        .lambda => {
            return .{ .lambda = .{
                .body = @enumFromInt(store.extra_data.items[node.main_token]),
                .args = .{ .span = .{ .start = node.data.untyped.lhs, .len = node.data.untyped.rhs } },
                .region = node.region,
            } };
        },
        .apply => {
            return .{ .apply = .{
                .@"fn" = @enumFromInt(store.extra_data.items[node.main_token]),
                .args = .{ .span = base.DataSpan{
                    .start = node.data.untyped.lhs,
                    .len = node.data.untyped.rhs,
                } },
                .region = node.region,
            } };
        },
        .suffix_single_question => {
            return .{ .suffix_single_question = .{
                .region = node.region,
                .operator = node.main_token,
                .expr = @enumFromInt(node.data.untyped.lhs),
            } };
        },
        .unary_op => {
            return .{ .unary_op = .{
                .region = node.region,
                .operator = node.main_token,
                .expr = @enumFromInt(node.data.unary_op.expr),
            } };
        },
        .if_then_else => {
            const then_idx = @as(usize, @intCast(node.data.if_then_else.extra_data_idx));
            const else_idx = then_idx + 1;
            const then_ed = store.extra_data.items[then_idx];
            const else_ed = store.extra_data.items[else_idx];
            return .{ .if_then_else = .{
                .region = node.region,
                .condition = @enumFromInt(node.data.if_then_else.condition),
                .then = @enumFromInt(then_ed),
                .@"else" = @enumFromInt(else_ed),
            } };
        },
        .match => {
            const idx = @as(usize, @intCast(node.data.match_expr.expr_extra_data_idx));
            return .{ .match = .{
                .region = node.region,
                .expr = @enumFromInt(store.extra_data.items[idx]),
                .branches = .{ .span = .{
                    .start = node.data.match_expr.branches_start,
                    .len = node.data.match_expr.branches_len,
                } },
            } };
        },
        .dbg => {
            return .{ .dbg = .{
                .region = node.region,
                .expr = @enumFromInt(node.data.untyped.lhs),
            } };
        },
        .bin_op => {
            return .{ .bin_op = .{
                .left = @enumFromInt(node.data.bin_op.left),
                .right = @enumFromInt(node.data.bin_op.right),
                .operator = node.main_token,
                .region = node.region,
            } };
        },
        .block => {
            const statements = AST.Statement.Span{ .span = .{
                .start = node.data.untyped.lhs,
                .len = node.data.untyped.rhs,
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
                .reason = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid expr tag, got {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves record field data from a stored record field node.
pub fn getRecordField(store: *NodeStore, field_idx: AST.RecordField.Idx) AST.RecordField {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(field_idx)));
    const name = node.main_token;
    const value: ?AST.Expr.Idx = if (node.data.untyped.lhs > 0) @enumFromInt(node.data.untyped.lhs) else null;
    const optional = node.data.untyped.rhs == 1;

    return .{
        .name = name,
        .value = value,
        .optional = optional,
        .region = node.region,
    };
}

/// Retrieves when branch data from a stored when branch node.
pub fn getBranch(store: *NodeStore, branch_idx: AST.WhenBranch.Idx) AST.WhenBranch {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(branch_idx)));
    return .{
        .region = node.region,
        .pattern = @enumFromInt(node.data.untyped.lhs),
        .body = @enumFromInt(node.data.untyped.rhs),
    };
}

/// Retrieves type header data from a stored type header node.
pub fn getTypeHeader(store: *NodeStore, header_idx: AST.TypeHeader.Idx) AST.TypeHeader {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(header_idx)));
    std.debug.assert(node.tag == .ty_header);
    return .{
        .region = node.region,
        .name = node.main_token,
        .args = .{ .span = .{
            .start = node.data.untyped.lhs,
            .len = node.data.untyped.rhs,
        } },
    };
}

/// Retrieves annotation record field data from a stored annotation record field node.
pub fn getAnnoRecordField(store: *NodeStore, anno_record_field_idx: AST.AnnoRecordField.Idx) AST.AnnoRecordField {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(anno_record_field_idx)));
    return .{
        .region = node.region,
        .name = node.data.untyped.lhs,
        .ty = @enumFromInt(node.data.untyped.rhs),
    };
}

/// Get a WhereClause node from the store, using a type-safe index to the node.
pub fn getWhereClause(store: *NodeStore, where_clause_idx: AST.WhereClause.Idx) AST.WhereClause {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(where_clause_idx)));
    switch (node.tag) {
        .where_alias => {
            return .{ .alias = .{
                .region = node.region,
                .var_tok = node.main_token,
                .alias_tok = node.data.untyped.lhs,
            } };
        },
        .where_method => {
            const ed_start = @as(usize, @intCast(node.data.untyped.lhs));
            const name_tok = store.extra_data.items[ed_start];
            const args = store.extra_data.items[ed_start + 1];
            const ret_anno = store.extra_data.items[ed_start + 2];
            return .{ .method = .{
                .region = node.region,
                .var_tok = node.main_token,
                .name_tok = name_tok,
                .args = @enumFromInt(args),
                .ret_anno = @enumFromInt(ret_anno),
            } };
        },
        .where_mod_method => {
            const ed_start = @as(usize, @intCast(node.data.untyped.lhs));
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
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid where clause node, found {s}", .{@tagName(node.tag)});
        },
    }
}

/// Retrieves type annotation data from a stored type annotation node, reconstructing the appropriate annotation type.
pub fn getTypeAnno(store: *NodeStore, ty_anno_idx: AST.TypeAnno.Idx) AST.TypeAnno {
    const node = store.nodes.get(@enumFromInt(@intFromEnum(ty_anno_idx)));

    switch (node.tag) {
        .ty_apply => {
            return .{ .apply = .{
                .region = node.region,
                .args = .{ .span = .{
                    .start = node.data.untyped.lhs,
                    .len = node.data.untyped.rhs,
                } },
            } };
        },
        .ty_var => {
            return .{ .ty_var = .{
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
                .ident = @bitCast(node.data.untyped.rhs),
                .region = node.region,
            } };
        },
        .ty_mod_ty => {
            return .{ .mod_ty = .{
                .mod_ident = @bitCast(node.data.untyped.lhs),
                .ty_ident = @bitCast(node.data.untyped.rhs),
                .region = node.region,
            } };
        },
        .ty_union => {
            const rhs = @as(AST.TypeAnno.TagUnionRhs, @bitCast(node.data.untyped.rhs));
            const tags_ed_end = node.data.untyped.lhs + rhs.tags_len;

            return .{ .tag_union = .{
                .region = node.region,
                .open_anno = if (rhs.open == 1) @enumFromInt(store.extra_data.items[tags_ed_end]) else null,
                .tags = .{ .span = .{
                    .start = node.data.untyped.lhs,
                    .len = @as(u32, @intCast(rhs.tags_len)),
                } },
            } };
        },
        .ty_tuple => {
            return .{ .tuple = .{
                .region = node.region,
                .annos = .{ .span = .{
                    .start = node.data.untyped.lhs,
                    .len = node.data.untyped.rhs,
                } },
            } };
        },
        .ty_record => {
            return .{ .record = .{
                .region = node.region,
                .fields = .{ .span = .{
                    .start = node.data.untyped.lhs,
                    .len = node.data.untyped.rhs,
                } },
            } };
        },
        .ty_fn => {
            const rhs = @as(AST.TypeAnno.TypeAnnoFnRhs, @bitCast(node.data.untyped.rhs));
            return .{ .@"fn" = .{
                .region = node.region,
                .ret = @enumFromInt(store.extra_data.items[@as(usize, @intCast(node.main_token))]),
                .args = .{ .span = .{
                    .start = node.data.untyped.lhs,
                    .len = @intCast(rhs.args_len),
                } },
                .effectful = rhs.effectful == 1,
            } };
        },
        .ty_parens => {
            return .{ .parens = .{
                .region = node.region,
                .anno = @enumFromInt(node.data.untyped.lhs),
            } };
        },
        .malformed => {
            return .{ .malformed = .{
                .reason = @enumFromInt(node.data.untyped.lhs),
                .region = node.region,
            } };
        },
        else => {
            std.debug.panic("Expected a valid type annotation node, found {s}", .{@tagName(node.tag)});
        },
    }

    const nid = store.nodes.append(store.gpa, node);
    return @enumFromInt(@intFromEnum(nid));
}

// ------------------------------------------------------------------------
// Node types - these are the constituent types used in the Node Store API
// ------------------------------------------------------------------------

/// Returns the start position for a new Span of AST.Expr.Idxs in scratch
pub fn scratchExprTop(store: *NodeStore) u32 {
    return store.scratch_exprs.top();
}

/// Places a new AST.Expr.Idx in the scratch.  Will panic on OOM.
pub fn addScratchExpr(store: *NodeStore, idx: AST.Expr.Idx) void {
    store.scratch_exprs.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn exprSpanFrom(store: *NodeStore, start: u32) AST.Expr.Span {
    const end = store.scratch_exprs.top();
    defer store.scratch_exprs.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_exprs.items.items[i])) catch |err| exitOnOom(err);
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
pub fn exprSlice(store: *NodeStore, span: AST.Expr.Span) []AST.Expr.Idx {
    return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
}

/// Returns the start position for a new Span of AST.Statement.Idxs in scratch
pub fn scratchStatementTop(store: *NodeStore) u32 {
    return store.scratch_statements.top();
}

/// Places a new AST.Statement.Idx in the scratch.  Will panic on OOM.
pub fn addScratchStatement(store: *NodeStore, idx: AST.Statement.Idx) void {
    store.scratch_statements.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn statementSpanFrom(store: *NodeStore, start: u32) AST.Statement.Span {
    const end = store.scratch_statements.top();
    defer store.scratch_statements.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_statements.items.items[i])) catch |err| exitOnOom(err);
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
pub fn statementSlice(store: *NodeStore, span: AST.Statement.Span) []AST.Statement.Idx {
    return store.sliceFromSpan(AST.Statement.Idx, span.span);
}

/// Returns the start position for a new Span of AST.Pattern.Idx in scratch
pub fn scratchPatternTop(store: *NodeStore) u32 {
    return store.scratch_patterns.top();
}

/// Places a new AST.Pattern.Idx in the scratch.  Will panic on OOM.
pub fn addScratchPattern(store: *NodeStore, idx: AST.Pattern.Idx) void {
    store.scratch_patterns.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn patternSpanFrom(store: *NodeStore, start: u32) AST.Pattern.Span {
    const end = store.scratch_patterns.top();
    defer store.scratch_patterns.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    std.debug.assert(end >= i);
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_patterns.items.items[i])) catch |err| exitOnOom(err);
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
pub fn sliceFromSpan(store: *NodeStore, comptime T: type, span: base.DataSpan) []T {
    return @ptrCast(store.extra_data.items[span.start..][0..span.len]);
}

/// Returns a new Pattern slice so that the caller can iterate through
/// all items in the span.
pub fn patternSlice(store: *NodeStore, span: AST.Pattern.Span) []AST.Pattern.Idx {
    return store.sliceFromSpan(AST.Pattern.Idx, span.span);
}

/// Returns a new AST.PatternRecordFieldIter so that the caller can iterate through
/// all items in the span.
pub fn patternRecordFieldSlice(store: *NodeStore, span: AST.PatternRecordField.Span) []AST.PatternRecordField.Idx {
    return store.sliceFromSpan(AST.PatternRecordField.Idx, span.span);
}
/// Returns the start position for a new Span of patternRecordFieldIdxs in scratch
pub fn scratchPatternRecordFieldTop(store: *NodeStore) u32 {
    return store.scratch_pattern_record_fields.top();
}

/// Places a new AST.PatternRecordField.Idx in the scratch.  Will panic on OOM.
pub fn addScratchPatternRecordField(store: *NodeStore, idx: AST.PatternRecordField.Idx) void {
    store.scratch_pattern_record_fields.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn patternRecordFieldSpanFrom(store: *NodeStore, start: u32) AST.PatternRecordField.Span {
    const end = store.scratch_pattern_record_fields.top();
    defer store.scratch_pattern_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_pattern_record_fields.items.items[i])) catch |err| exitOnOom(err);
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
pub fn recordFieldSlice(store: *NodeStore, span: AST.RecordField.Span) []AST.RecordField.Idx {
    return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
}
/// Returns the start position for a new Span of recordFieldIdxs in scratch
pub fn scratchRecordFieldTop(store: *NodeStore) u32 {
    return store.scratch_record_fields.top();
}

/// Places a new AST.RecordField.Idx in the scratch.  Will panic on OOM.
pub fn addScratchRecordField(store: *NodeStore, idx: AST.RecordField.Idx) void {
    store.scratch_record_fields.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn recordFieldSpanFrom(store: *NodeStore, start: u32) AST.RecordField.Span {
    const end = store.scratch_record_fields.top();
    defer store.scratch_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_record_fields.items.items[i])) catch |err| exitOnOom(err);
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
pub fn scratchWhenBranchTop(store: *NodeStore) u32 {
    return store.scratch_when_branches.top();
}

/// Places a new AST.WhenBranch.Idx in the scratch.  Will panic on OOM.
pub fn addScratchWhenBranch(store: *NodeStore, idx: AST.WhenBranch.Idx) void {
    store.scratch_when_branches.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn whenBranchSpanFrom(store: *NodeStore, start: u32) AST.WhenBranch.Span {
    const end = store.scratch_when_branches.top();
    defer store.scratch_when_branches.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_when_branches.items.items[i])) catch |err| exitOnOom(err);
        i += 1;
    }
    return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
}

/// Clears any WhenBranchIds added to scratch from start until the end.
/// Should be used wherever the scratch items will not be used,
/// as in when parsing fails.
pub fn clearScratchWhenBranchesFrom(store: *NodeStore, start: u32) void {
    store.scratch_when_branches.clearFrom(start);
}

/// Returns a new WhenBranch slice so that the caller can iterate through
/// all items in the span.
pub fn whenBranchSlice(store: *NodeStore, span: AST.WhenBranch.Span) []AST.WhenBranch.Idx {
    return store.sliceFromSpan(AST.WhenBranch.Idx, span.span);
}

/// Returns the start position for a new Span of typeAnnoIdxs in scratch
pub fn scratchTypeAnnoTop(store: *NodeStore) u32 {
    return store.scratch_type_annos.top();
}

/// Places a new AST.TypeAnno.Idx in the scratch.  Will panic on OOM.
pub fn addScratchTypeAnno(store: *NodeStore, idx: AST.TypeAnno.Idx) void {
    store.scratch_type_annos.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn typeAnnoSpanFrom(store: *NodeStore, start: u32) AST.TypeAnno.Span {
    const end = store.scratch_type_annos.top();
    defer store.scratch_type_annos.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_type_annos.items.items[i])) catch |err| exitOnOom(err);
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
pub fn typeAnnoSlice(store: *NodeStore, span: AST.TypeAnno.Span) []AST.TypeAnno.Idx {
    return store.sliceFromSpan(AST.TypeAnno.Idx, span.span);
}

/// Returns the start position for a new Span of annoRecordFieldIdxs in scratch
pub fn scratchAnnoRecordFieldTop(store: *NodeStore) u32 {
    return store.scratch_anno_record_fields.top();
}

/// Places a new AST.AnnoRecordField.Idx in the scratch.  Will panic on OOM.
pub fn addScratchAnnoRecordField(store: *NodeStore, idx: AST.AnnoRecordField.Idx) void {
    store.scratch_anno_record_fields.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn annoRecordFieldSpanFrom(store: *NodeStore, start: u32) AST.AnnoRecordField.Span {
    const end = store.scratch_anno_record_fields.top();
    defer store.scratch_anno_record_fields.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_anno_record_fields.items.items[i])) catch |err| exitOnOom(err);
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
pub fn annoRecordFieldSlice(store: *NodeStore, span: AST.AnnoRecordField.Span) []AST.AnnoRecordField.Idx {
    return store.sliceFromSpan(AST.AnnoRecordField.Idx, span.span);
}

/// Returns the start position for a new Span of token_Idxs in scratch
pub fn scratchTokenTop(store: *NodeStore) u32 {
    return store.scratch_tokens.top();
}

/// Places a new Token.Idx in the scratch.  Will panic on OOM.
pub fn addScratchToken(store: *NodeStore, idx: Token.Idx) void {
    store.scratch_tokens.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn tokenSpanFrom(store: *NodeStore, start: u32) Token.Span {
    const end = store.scratch_tokens.top();
    defer store.scratch_tokens.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_tokens.items.items[i])) catch |err| exitOnOom(err);
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
pub fn tokenSlice(store: *NodeStore, span: Token.Span) []Token.Idx {
    return store.sliceFromSpan(Token.Idx, span.span);
}

/// Returns the start position for a new Span of exposedItemIdxs in scratch
pub fn scratchExposedItemTop(store: *NodeStore) u32 {
    return store.scratch_anno_record_fields.top();
}

/// Places a new AST.ExposedItem.Idx in the scratch.  Will panic on OOM.
pub fn addScratchExposedItem(store: *NodeStore, idx: AST.ExposedItem.Idx) void {
    store.scratch_exposed_items.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn exposedItemSpanFrom(store: *NodeStore, start: u32) AST.ExposedItem.Span {
    const end = store.scratch_exposed_items.top();
    defer store.scratch_exposed_items.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_exposed_items.items.items[i])) catch |err| exitOnOom(err);
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
pub fn exposedItemSlice(store: *NodeStore, span: AST.ExposedItem.Span) []AST.ExposedItem.Idx {
    return store.sliceFromSpan(AST.ExposedItem.Idx, span.span);
}

/// Returns the start position for a new Span of whereClauseIdxs in scratch
pub fn scratchWhereClauseTop(store: *NodeStore) u32 {
    return store.scratch_where_clauses.top();
}

/// Places a new AST.WhereClause.Idx in the scratch.  Will panic on OOM.
pub fn addScratchWhereClause(store: *NodeStore, idx: AST.WhereClause.Idx) void {
    store.scratch_where_clauses.append(store.gpa, idx);
}

/// Creates a new span starting at start.  Moves the items from scratch
/// to extra_data as appropriate.
pub fn whereClauseSpanFrom(store: *NodeStore, start: u32) AST.WhereClause.Span {
    const end = store.scratch_where_clauses.top();
    defer store.scratch_where_clauses.clearFrom(start);
    var i = @as(usize, @intCast(start));
    const ed_start = @as(u32, @intCast(store.extra_data.items.len));
    while (i < end) {
        store.extra_data.append(store.gpa, @intFromEnum(store.scratch_where_clauses.items.items[i])) catch |err| exitOnOom(err);
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
pub fn whereClauseSlice(store: *NodeStore, span: AST.WhereClause.Span) []AST.WhereClause.Idx {
    return store.sliceFromSpan(AST.WhereClause.Idx, span.span);
}

// Unified Record Helper Functions
fn addRecordShape(store: *NodeStore, fields_start: u32, fields_len: u32, region: AST.TokenizedRegion, tag: Node.Tag) Node.Idx {
    const node = Node{
        .tag = tag,
        .main_token = 0,
        .data = .{
            .record = .{
                .fields_start = fields_start,
                .fields_len = fields_len,
            },
        },
        .region = region,
    };
    return store.nodes.append(store.gpa, node);
}

fn getRecordShape(store: *NodeStore, node_idx: Node.Idx) struct {
    fields_start: u32,
    fields_len: u32,
    region: AST.TokenizedRegion,
} {
    const node = store.nodes.get(node_idx);
    return .{
        .fields_start = node.data.record.fields_start,
        .fields_len = node.data.record.fields_len,
        .region = node.region,
    };
}

fn addListShape(store: *NodeStore, items_start: u32, items_len: u32, region: AST.TokenizedRegion, tag: Node.Tag) Node.Idx {
    const node = Node{
        .tag = tag,
        .main_token = 0,
        .data = .{
            .list = .{
                .items_start = items_start,
                .items_len = items_len,
            },
        },
        .region = region,
    };
    return store.nodes.append(store.gpa, node);
}

fn getListShape(store: *NodeStore, node_idx: Node.Idx) struct {
    items_start: u32,
    items_len: u32,
    region: AST.TokenizedRegion,
} {
    const node = store.nodes.get(node_idx);
    return .{
        .items_start = node.data.list.items_start,
        .items_len = node.data.list.items_len,
        .region = node.region,
    };
}

fn addTupleShape(store: *NodeStore, items_start: u32, items_len: u32, region: AST.TokenizedRegion, tag: Node.Tag) Node.Idx {
    const node = Node{
        .tag = tag,
        .main_token = 0,
        .data = .{
            .tuple = .{
                .items_start = items_start,
                .items_len = items_len,
            },
        },
        .region = region,
    };
    return store.nodes.append(store.gpa, node);
}

fn getTupleShape(store: *NodeStore, node_idx: Node.Idx) struct {
    items_start: u32,
    items_len: u32,
    region: AST.TokenizedRegion,
} {
    const node = store.nodes.get(node_idx);
    return .{
        .items_start = node.data.tuple.items_start,
        .items_len = node.data.tuple.items_len,
        .region = node.region,
    };
}

fn addTagShape(store: *NodeStore, args_start: u32, args_len: u32, region: AST.TokenizedRegion, tag: Node.Tag) Node.Idx {
    const node = Node{
        .tag = tag,
        .main_token = 0,
        .data = .{
            .tag = .{
                .args_start = args_start,
                .args_len = args_len,
            },
        },
        .region = region,
    };
    return store.nodes.append(node);
}

fn getTagShape(store: *NodeStore, node_idx: Node.Idx) struct {
    args_start: u32,
    args_len: u32,
    region: AST.TokenizedRegion,
} {
    const node = store.nodes.get(node_idx);
    return .{
        .args_start = node.data.tag.args_start,
        .args_len = node.data.tag.args_len,
        .region = node.region,
    };
}
