const std = @import("std");
const base = @import("../../base.zig");
const sexpr = @import("../../base/sexpr.zig");
const tokenize = @import("tokenize.zig");
const collections = @import("../../collections.zig");

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

/// deinit the IR's memory
pub fn deinit(self: *IR) void {
    defer self.tokens.deinit();
    defer self.store.deinit();
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
        unexpected_token,
        missing_header,
        list_not_closed,
        missing_arrow,
        expected_provides_open_square,
        expected_provides,
        expected_provides_close_square,
        expected_package_platform_open_curly,
        expected_package_or_platform_name,
        expected_package_or_platform_colon,
        expected_platform_string,
        expected_package_or_platform_string,
        expected_package_platform_close_curly,
        import_exposing_no_open,
        import_exposing_no_close,
        no_else,
    };
};

/// The first and last token consumed by a Node
pub const Region = struct {
    start: TokenIdx,
    end: TokenIdx,
};

/// Unstructured information about a Node.  These
/// can actually point to either Tokens or other Nodes,
/// or represent lengths or be packed structs with more
/// densely represented information.
///
/// The conventions should be documented for each Node
/// Tag.
pub const Data = struct {
    lhs: u32,
    rhs: u32,
};

/// A single meaningful node in the Abstract Syntax Tree.
/// Should always be inserted and fetched from a Node Store.
///
/// The Tag represents what type of Node it is, and
/// therefore how it's data and main_token fields should
/// be interpreted.
pub const Node = struct {
    tag: Tag,
    data: Data,
    main_token: TokenIdx,

    pub const List = collections.SafeMultiList(Node);

    /// Internal representation for where a node is stored
    /// in the tree.
    pub const Idx = List.Idx;

    /// This is the tag associated with a raw Node in the list
    pub const Tag = enum {
        /// lhs - error code
        /// rhs - ignored
        malformed,

        /// lhs - first token
        /// rhs - last token
        root,

        /// TODO: Find a way to remove this
        /// This is a hack _only_ for the formatter - it should never be emitted by the parser
        /// * lhs - node that caused this node to be added by formatter
        /// * rhs - ignored
        emit_ws,

        /// Module header
        /// * lhs - module type
        /// * rhs - extra data pointer
        app_header,
        module_header,

        // Statements

        /// The marker for statements
        /// * main_token - ignored
        /// * lhs - points to actual statement node should be one of the following tags
        /// * rhs - ignored
        statement,

        /// A declaration of a value
        /// Example: `a = some_expr`
        /// * lhs - pattern node index
        /// * rhs - value node index
        decl,
        /// Any plain expression - see Exprs below
        /// * lhs - node index to actual expr node
        /// * rhs - ignored
        expr,
        /// A crash statement
        /// Example: `crash "A message"`
        /// * lhs - node index to message(Should be a str_lit)
        /// * rhs - ignored
        crash,
        /// Any plain expression - see Exprs below
        /// * lhs - node index for block or expr
        /// * rhs - ignored
        expect,
        /// An early return statement
        /// * lhs - node index for expr
        /// * rhs - ignored
        @"return",
        /// An import statement
        /// Example: `import pf.Stdout`
        /// * main_token - first token in module ident
        /// * lhs - extra_data description - struct(packed){ aliased: u1, num_exposes: u31 }
        /// * rhs - extra_data index or 0 if lhs is 0
        /// * extra_data format(if aliased == 1): [alias upper_ident node index, [exposed node index]{num_exposes}]
        /// * extra_data format(if aliased == 0): [[exposed node index]{num_exposes}]
        import,
        /// A Type declaration
        /// Example: `Color := { red : U8, green: U8, blue: U8 }`
        /// Example: `Color := [Red, Green, Blue]`
        /// * main_token - upper_ident for type ident
        /// * lhs - describes extra_data: struct(packed){ num_type_args: u31, has_where: u1 }
        /// * rhs - extra_data index
        /// * extra_data format (if has_where == 1): [where node index, [type_arg node index]{num_type_args}, type_term node_index]
        /// * extra_data format (if has_where == 0): [[type_arg node index]{num_type_args}, type_term node_index]
        type_decl,
        /// A Type annotation
        /// Example: `main! : List Str => Result {} _`
        /// Example: `colors : List Color`
        /// Example: `color : { red : U8, green: U8, blue: U8 }`
        /// * main_token - lower_ident token index
        /// * lhs - extra_data description - 1 if has where clause, 0 otherwise
        /// * rhs - extra_data index
        /// * extra_data format: [[where node index]?, type_term node index]
        type_anno,

        // Exposed items
        exposed_item_lower,
        exposed_item_upper,
        exposed_item_upper_star,

        // Type terms

        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ty_var,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ty_tag,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ty_record,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ty_tuple,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ty_union,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ty_fn,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ty_underscore,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ty_parens,

        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ty_record_field,

        // Type Header

        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ty_header,

        // Patterns

        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ident_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        tag_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        number_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        string_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        record_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        record_field_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        list_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        list_rest_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        tuple_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        underscore_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        alternatives_patt,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        as_patt,
        // TODO: Add the rest of the patterns

        // Exprs

        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        int,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        float,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        string_part,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        string,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        list,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        tuple,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        record,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        record_field,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        tag,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        lambda,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        apply,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        record_update,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        field_access,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        bin_op,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        suffix_single_question,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        unary_op,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - node index of expr
        /// * rhs - RHS DESCRIPTION
        if_then_else,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - start index of extra_data
        /// * rhs - end index of extra_data
        /// * extra_data format - expr node index,[pattern node index, block/expr node index]*
        match,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - start index of extra_data
        /// * rhs - end index of extra_data
        /// * extra_data format - expr node index,[pattern node index, block/expr node index]*
        ident,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        dbg,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        record_builder,
        /// A block of statements
        /// Main token is newline preceding the block
        /// * lhs - first statement node
        /// * rhs - number of statements
        block,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        ellipsis,

        /// A branch is a when expression
        /// Main token is ignored
        /// * lhs - Pattern index
        /// * rhs - Body index
        branch,
    };
};

/// Typesafe access to an underlying SoA of Nodes.
/// This - along with the types used in its API - should
/// be the only way that other modules interact with
/// the IR (AST).
pub const NodeStore = struct {
    gpa: std.mem.Allocator,
    nodes: Node.List,
    extra_data: std.ArrayListUnmanaged(u32),
    scratch_statements: std.ArrayListUnmanaged(StatementIdx),
    scratch_tokens: std.ArrayListUnmanaged(TokenIdx),
    scratch_exprs: std.ArrayListUnmanaged(ExprIdx),
    scratch_patterns: std.ArrayListUnmanaged(PatternIdx),
    scratch_record_fields: std.ArrayListUnmanaged(RecordFieldIdx),
    scratch_pattern_record_fields: std.ArrayListUnmanaged(PatternRecordFieldIdx),
    scratch_when_branches: std.ArrayListUnmanaged(WhenBranchIdx),
    scratch_type_annos: std.ArrayListUnmanaged(TypeAnnoIdx),
    scratch_anno_record_fields: std.ArrayListUnmanaged(AnnoRecordFieldIdx),
    scratch_exposed_items: std.ArrayListUnmanaged(ExposedItemIdx),

    /// Initialize the store with an assumed capacity to
    /// ensure resizing of underlying data structures happens
    /// very rarely.
    pub fn initWithCapacity(gpa: std.mem.Allocator, capacity: usize) NodeStore {
        var store: NodeStore = .{
            .gpa = gpa,
            .nodes = .{},
            .extra_data = .{},
            .scratch_statements = .{},
            .scratch_tokens = .{},
            .scratch_exprs = .{},
            .scratch_patterns = .{},
            .scratch_record_fields = .{},
            .scratch_pattern_record_fields = .{},
            .scratch_when_branches = .{},
            .scratch_type_annos = .{},
            .scratch_anno_record_fields = .{},
            .scratch_exposed_items = .{},
        };

        store.nodes.ensureTotalCapacity(gpa, capacity);
        _ = store.nodes.append(gpa, .{
            .tag = .root,
            .main_token = 0,
            .data = .{ .lhs = 0, .rhs = 0 },
        });
        store.extra_data.ensureTotalCapacity(gpa, capacity / 2) catch |err| exitOnOom(err);
        store.scratch_statements.ensureTotalCapacity(gpa, scratch_90th_percentile_capacity) catch |err| exitOnOom(err);
        store.scratch_tokens.ensureTotalCapacity(gpa, scratch_90th_percentile_capacity) catch |err| exitOnOom(err);
        store.scratch_exprs.ensureTotalCapacity(gpa, scratch_90th_percentile_capacity) catch |err| exitOnOom(err);
        store.scratch_patterns.ensureTotalCapacity(gpa, scratch_90th_percentile_capacity) catch |err| exitOnOom(err);
        store.scratch_record_fields.ensureTotalCapacity(gpa, scratch_90th_percentile_capacity) catch |err| exitOnOom(err);
        store.scratch_pattern_record_fields.ensureTotalCapacity(gpa, scratch_90th_percentile_capacity) catch |err| exitOnOom(err);
        store.scratch_when_branches.ensureTotalCapacity(gpa, scratch_90th_percentile_capacity) catch |err| exitOnOom(err);
        store.scratch_type_annos.ensureTotalCapacity(gpa, scratch_90th_percentile_capacity) catch |err| exitOnOom(err);
        store.scratch_anno_record_fields.ensureTotalCapacity(gpa, scratch_90th_percentile_capacity) catch |err| exitOnOom(err);
        store.scratch_exposed_items.ensureTotalCapacity(gpa, scratch_90th_percentile_capacity) catch |err| exitOnOom(err);

        return store;
    }

    // This value is based on some observed characteristics of Roc code.
    // Having an initial capacity of 10 ensures that the scratch slice
    // will only have to be resized in >90th percentile case.
    // It is not scientific, and should be tuned when we have enough
    // Roc code to instrument this and determine a real 90th percentile.
    const scratch_90th_percentile_capacity = 10;

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
    }

    /// Ensures that all scratch buffers in the store
    /// are clear for use.
    pub fn emptyScratch(store: *NodeStore) void {
        store.scratch_statements.shrinkRetainingCapacity(0);
        store.scratch_tokens.shrinkRetainingCapacity(0);
        store.scratch_exprs.shrinkRetainingCapacity(0);
        store.scratch_patterns.shrinkRetainingCapacity(0);
        store.scratch_record_fields.shrinkRetainingCapacity(0);
        store.scratch_pattern_record_fields.shrinkRetainingCapacity(0);
        store.scratch_when_branches.shrinkRetainingCapacity(0);
        store.scratch_type_annos.shrinkRetainingCapacity(0);
        store.scratch_anno_record_fields.shrinkRetainingCapacity(0);
        store.scratch_exposed_items.shrinkRetainingCapacity(0);
    }

    // Node Type Idx types

    /// An index for a Body node. Should not be constructed externally.
    pub const BodyIdx = struct { id: u32 };
    /// An index for a Header node. Should not be constructed externally.
    pub const HeaderIdx = struct { id: u32 };
    /// An index for a ExposedItem node. Should not be constructed externally.
    pub const ExposedItemIdx = struct { id: u32 };
    /// An index for a Statement node. Should not be constructed externally.
    pub const StatementIdx = struct { id: u32 };
    /// An index for a Pattern node. Should not be constructed externally.
    pub const PatternIdx = struct { id: u32 };
    /// An index for a Expr node. Should not be constructed externally.
    pub const ExprIdx = struct { id: u32 };
    /// An index for a IfElse node. Should not be constructed externally.
    pub const IfElseIdx = struct { id: u32 };
    /// An index for a WhenBranch node. Should not be constructed externally.
    pub const WhenBranchIdx = struct { id: u32 };
    /// An index for a RecordField node. Should not be constructed externally.
    pub const RecordFieldIdx = struct { id: u32 };
    /// An index for a PatternRecordField node. Should not be constructed externally.
    pub const PatternRecordFieldIdx = struct { id: u32 };
    /// An index for a TypeHeader node. Should not be constructed externally.
    pub const TypeHeaderIdx = struct { id: u32 };
    /// An index for a TypeAnno node. Should not be constructed externally.
    pub const TypeAnnoIdx = struct { id: u32 };
    /// An index for a AnnoRecordField node. Should not be constructed externally.
    pub const AnnoRecordFieldIdx = struct { id: u32 };

    // ------------------------------------------------------------------------
    // Creation API - All nodes should be added using these functions
    // ------------------------------------------------------------------------

    /// Any node type can be malformed, but must come with a diagnostic reason
    pub fn addMalformed(store: *NodeStore, comptime t: type, reason: Diagnostic.Tag, token: TokenIdx) t {
        const nid = store.nodes.append(store.gpa, .{
            .tag = .malformed,
            .main_token = token,
            .data = .{ .lhs = @intFromEnum(reason), .rhs = 0 },
        });
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addFile(store: *NodeStore, file: File) void {
        store.extra_data.append(store.gpa, file.header.id) catch |err| exitOnOom(err);
        store.nodes.set(@enumFromInt(0), .{
            .tag = .root,
            .main_token = 0,
            .data = .{
                .lhs = file.statements.span.start,
                .rhs = file.statements.span.len,
            },
        });
    }

    pub fn addHeader(store: *NodeStore, header: Header) HeaderIdx {
        var node = Node{
            .tag = .statement,
            .main_token = 0,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        };
        switch (header) {
            .app => |app| {
                // struct {
                //    provides: TokenSpan, // This should probably be a Interned Ident token
                //    platform: TokenIdx,
                //    platform_name: TokenIdx,
                //    packages: RecordFieldSpan,
                //    region: Region,
                // }
                node.tag = .app_header;
                node.main_token = app.platform_name;
                node.data.lhs = app.provides.span.start;
                node.data.rhs = @as(u32, @bitCast(Header.AppHeaderRhs{
                    .num_packages = @as(u10, @intCast(app.packages.span.len)),
                    .num_provides = @as(u22, @intCast(app.provides.span.len)),
                }));

                store.extra_data.append(store.gpa, app.platform.id) catch |err| exitOnOom(err);
            },
            .module => |mod| {
                node.tag = .module_header;
                node.data.lhs = mod.exposes.span.start;
                node.data.rhs = mod.exposes.span.len;
            },
            else => {},
        }
        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addExposedItem(store: *NodeStore, item: ExposedItem) ExposedItemIdx {
        var node = Node{
            .tag = .malformed,
            .main_token = 0,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
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
            },
            .upper_ident => |i| {
                node.tag = .exposed_item_upper;
                node.main_token = i.ident;
                if (i.as) |a| {
                    std.debug.assert(a > 0);
                    node.data.lhs = a;
                    node.data.rhs = 1;
                }
            },
            .upper_ident_star => |i| {
                node.tag = .exposed_item_upper_star;
                node.main_token = i.ident;
            },
        }

        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addStatement(store: *NodeStore, statement: Statement) StatementIdx {
        var node = Node{
            .tag = .statement,
            .main_token = 0,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        };
        switch (statement) {
            .decl => |d| {
                node.tag = .decl;
                node.data.lhs = d.pattern.id;
                node.data.rhs = d.body.id;
            },
            .expr => |expr| {
                node.tag = .expr;
                node.data.lhs = expr.expr.id;
            },
            .crash => |c| {
                node.tag = .crash;
                node.data.lhs = c.expr.id;
            },
            .expect => |e| {
                node.tag = .expect;
                node.data.lhs = e.body.id;
            },
            .@"return" => |r| {
                node.tag = .@"return";
                node.data.lhs = r.expr.id;
            },
            .import => |i| {
                node.tag = .import;
                node.main_token = i.module_name_tok;
                var rhs = ImportRhs{
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
                node.data.rhs = @as(u32, @bitCast(rhs));
                if (node.data.rhs > 0) {
                    node.data.lhs = ed_start;
                }
            },
            .type_decl => |d| {
                node.tag = .type_decl;
                node.data.lhs = d.header.id;
                node.data.rhs = d.anno.id;
            },
            .type_anno => |a| {
                node.tag = .type_anno;
                node.data.lhs = a.name;
                node.data.rhs = a.anno.id;
            },
        }
        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addPattern(store: *NodeStore, pattern: Pattern) PatternIdx {
        var node = Node{
            .tag = .statement,
            .main_token = 0,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        };
        switch (pattern) {
            .ident => |i| {
                node.tag = .ident_patt;
                node.main_token = i.ident_tok;
            },
            .tag => |t| {
                node.tag = .tag_patt;
                node.main_token = t.tag_tok;
                node.data.lhs = t.args.span.start;
                node.data.rhs = t.args.span.len;
            },
            .number => |n| {
                node.tag = .number_patt;
                node.main_token = n.number_tok;
            },
            .string => |s| {
                node.tag = .string_patt;
                node.main_token = s.string_tok;
                node.data.lhs = s.expr.id;
            },
            .record => |r| {
                node.tag = .record_patt;
                node.data.lhs = r.fields.span.start;
                node.data.rhs = r.fields.span.len;
            },
            .list => |l| {
                node.tag = .list_patt;
                node.data.lhs = l.patterns.span.start;
                node.data.rhs = l.patterns.span.len;
            },
            .list_rest => |r| {
                node.tag = .list_rest_patt;
                if (r.name) |n| {
                    node.data.lhs = n;
                }
            },
            .tuple => |t| {
                node.tag = .tuple_patt;
                node.data.lhs = t.patterns.span.start;
                node.data.rhs = t.patterns.span.len;
            },
            .underscore => |_| {
                node.tag = .underscore_patt;
            },
            .alternatives => |a| {
                std.debug.assert(a.patterns.span.len > 1);
                node.tag = .alternatives_patt;
                node.data.lhs = a.patterns.span.start;
                node.data.rhs = a.patterns.span.len;
            },
        }
        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addExpr(store: *NodeStore, expr: Expr) ExprIdx {
        var node = Node{
            .tag = .statement,
            .main_token = 0,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        };
        switch (expr) {
            .int => |e| {
                node.tag = .int;
                node.main_token = e.token;
            },
            .float => |e| {
                node.tag = .float;
                node.main_token = e.token;
            },
            .string_part => |e| {
                node.tag = .string_part;
                node.main_token = e.token;
            },
            .string => |e| {
                node.tag = .string;
                node.main_token = e.token;
                node.data.lhs = e.parts.span.start;
                node.data.rhs = e.parts.span.len;
            },
            .list => |l| {
                node.tag = .list;
                node.main_token = l.region.start;
                node.data.lhs = l.items.span.start;
                node.data.rhs = l.items.span.len;
            },
            .tuple => |t| {
                node.tag = .tuple;
                node.data.lhs = t.items.span.start;
                node.data.rhs = t.items.span.len;
            },
            .record => |r| {
                node.tag = .record;
                node.data.lhs = r.fields.span.start;
                node.data.rhs = r.fields.span.len;
            },
            .tag => |e| {
                node.tag = .tag;
                node.main_token = e.token;
            },
            .lambda => |l| {
                node.tag = .lambda;
                node.data.lhs = l.args.span.start;
                node.data.rhs = l.args.span.len;
                const body_idx = store.extra_data.items.len;
                store.extra_data.append(store.gpa, l.body.id) catch |err| exitOnOom(err);
                node.main_token = @as(u32, @intCast(body_idx));
            },
            .apply => |app| {
                node.tag = .apply;
                node.data.lhs = app.args.span.start;
                node.data.rhs = app.args.span.len;
                const fn_ed_idx = store.extra_data.items.len;
                store.extra_data.append(store.gpa, app.@"fn".id) catch |err| exitOnOom(err);
                node.main_token = @as(u32, @intCast(fn_ed_idx));
            },
            .record_updater => |_| {},
            .field_access => |fa| {
                node.tag = .field_access;
                node.data.lhs = fa.left.id;
                node.data.rhs = fa.right.id;
            },
            .bin_op => |op| {
                node.tag = .bin_op;
                node.main_token = op.operator;
                node.data.lhs = op.left.id;
                node.data.rhs = op.right.id;
            },
            .suffix_single_question => |op| {
                node.tag = .suffix_single_question;
                node.data.lhs = op.expr.id;
            },
            .unary_op => |u| {
                node.tag = .unary_op;
                node.main_token = u.operator;
                node.data.lhs = u.expr.id;
            },
            .if_then_else => |i| {
                node.tag = .if_then_else;
                node.data.lhs = i.condition.id;
                node.data.rhs = @as(u32, @intCast(store.extra_data.items.len));
                store.extra_data.append(store.gpa, i.then.id) catch |err| exitOnOom(err);
                store.extra_data.append(store.gpa, i.@"else".id) catch |err| exitOnOom(err);
            },
            .match => |m| {
                node.tag = .match;
                node.data.lhs = m.branches.span.start;
                node.data.rhs = m.branches.span.len;
                store.extra_data.append(store.gpa, m.expr.id) catch |err| exitOnOom(err);
            },
            .ident => |id| {
                node.tag = .ident;
                node.main_token = id.token;
                if (id.qualifier) |qualifier| {
                    node.data.lhs = qualifier;
                    node.data.rhs = 1;
                }
            },
            .dbg => |d| {
                node.tag = .dbg;
                node.data.lhs = d.expr.id;
            },
            .record_builder => |_| {},
            .block => |body| {
                node.tag = .block;
                node.main_token = 0;
                node.data.lhs = body.statements.span.start;
                node.data.rhs = body.statements.span.len;
            },
            .ellipsis => |_| {
                node.tag = .ellipsis;
            },
        }
        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addPatternRecordField(store: *NodeStore, field: PatternRecordField) PatternRecordFieldIdx {
        var node = Node{
            .tag = .record_field_patt,
            .main_token = field.name,
            .data = .{
                .lhs = if (field.rest) 1 else 0,
                .rhs = 0,
            },
        };
        if (field.value) |value| {
            node.data.rhs = value.id;
        }
        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn getPatternRecordField(store: *NodeStore, field: PatternRecordFieldIdx) PatternRecordField {
        const node = store.nodes.get(@enumFromInt(field.id));
        return .{
            .name = node.main_token,
            .value = if (node.data.rhs == 0) null else .{ .id = node.data.rhs },
            .rest = node.data.lhs == 1,
            .region = emptyRegion(),
        };
    }

    pub fn addRecordField(store: *NodeStore, field: RecordField) RecordFieldIdx {
        var node = Node{
            .tag = .statement,
            .main_token = 0,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        };
        node.tag = .record_field;
        node.main_token = field.name;
        if (field.value) |v| {
            node.data.lhs = v.id;
        }
        if (field.optional) {
            node.data.rhs = 1;
        }

        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addWhenBranch(store: *NodeStore, branch: WhenBranch) WhenBranchIdx {
        const node = Node{
            .tag = .branch,
            .main_token = 0,
            .data = .{
                .lhs = branch.pattern.id,
                .rhs = branch.body.id,
            },
        };

        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addTypeHeader(store: *NodeStore, header: TypeHeader) TypeHeaderIdx {
        var node = Node{
            .tag = .ty_header,
            .main_token = header.name,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        };

        node.data.lhs = header.args.span.start;
        node.data.rhs = header.args.span.len;

        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addAnnoRecordField(store: *NodeStore, field: AnnoRecordField) AnnoRecordFieldIdx {
        const node = Node{
            .tag = .ty_record_field,
            .main_token = 0,
            .data = .{
                .lhs = @as(u32, @intCast(field.name)),
                .rhs = @as(u32, @intCast(field.ty.id)),
            },
        };

        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addTypeAnno(store: *NodeStore, anno: TypeAnno) TypeAnnoIdx {
        var node = Node{
            .tag = .branch,
            .main_token = 0,
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        };

        switch (anno) {
            .ty_var => |v| {
                node.tag = .ty_var;
                node.main_token = v.tok;
            },
            .underscore => |_| {
                node.tag = .ty_underscore;
            },
            .tag => |t| {
                node.tag = .ty_tag;
                node.main_token = t.tok;
                node.data.lhs = t.args.span.start;
                node.data.rhs = t.args.span.len;
            },
            .tag_union => |tu| {
                node.tag = .ty_union;
                node.data.lhs = tu.tags.span.start;
                var rhs = TypeAnno.TagUnionRhs{
                    .open = 0,
                    .tags_len = @as(u31, @intCast(tu.tags.span.len)),
                };
                if (tu.open_anno) |a| {
                    rhs.open = 1;
                    store.extra_data.append(store.gpa, a.id) catch |err| exitOnOom(err);
                }
                node.data.rhs = @as(u32, @bitCast(rhs));
            },
            .tuple => |t| {
                node.tag = .ty_tuple;
                node.data.lhs = t.annos.span.start;
                node.data.rhs = t.annos.span.len;
            },
            .record => |r| {
                node.tag = .ty_record;
                node.data.lhs = r.fields.span.start;
                node.data.rhs = r.fields.span.len;
            },
            .@"fn" => |f| {
                node.tag = .ty_fn;
                node.data.lhs = f.args.span.start;
                node.data.rhs = f.args.span.len;
                const ret_idx = store.extra_data.items.len;
                store.extra_data.append(store.gpa, f.ret.id) catch |err| exitOnOom(err);
                node.main_token = @as(u32, @intCast(ret_idx));
            },
            .parens => |p| {
                node.tag = .ty_parens;
                node.data.lhs = p.anno.id;
            },
        }

        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    // ------------------------------------------------------------------------
    // Read API - All nodes should be accessed using these functions
    // ------------------------------------------------------------------------

    pub fn getFile(store: *NodeStore) File {
        const node = store.nodes.get(@enumFromInt(0));
        const header_ed_idx = @as(usize, @intCast(node.data.lhs + node.data.rhs));
        const header = store.extra_data.items[header_ed_idx];
        return .{
            .header = .{ .id = header },
            .statements = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
            .region = .{ .start = 0, .end = 0 },
        };
    }
    pub fn getHeader(store: *NodeStore, header: HeaderIdx) Header {
        const node = store.nodes.get(@enumFromInt(header.id));
        switch (node.tag) {
            .app_header => {
                const extra_data_start = node.data.lhs;
                const rhs = @as(Header.AppHeaderRhs, @bitCast(node.data.rhs));
                var platform_idx = @as(usize, @intCast(extra_data_start + rhs.num_packages + rhs.num_provides)) + 1;
                if (platform_idx < extra_data_start) {
                    platform_idx = @intCast(extra_data_start);
                }
                const platform = store.extra_data.items[platform_idx];
                return .{
                    .app = .{
                        .platform = .{ .id = platform },
                        .platform_name = node.main_token,
                        .packages = .{ .span = .{
                            .start = extra_data_start + rhs.num_provides,
                            .len = rhs.num_packages,
                        } },
                        .provides = .{ .span = .{
                            .start = extra_data_start,
                            .len = rhs.num_provides,
                        } },
                        .region = emptyRegion(),
                    },
                };
            },
            .module_header => {
                return .{ .module = .{
                    .exposes = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                    .region = emptyRegion(),
                } };
            },
            else => {
                std.debug.panic("Expected a valid header tag, got {s}", .{@tagName(node.tag)});
            },
        }
    }

    pub fn getExposedItem(store: *NodeStore, item: ExposedItemIdx) ExposedItem {
        const node = store.nodes.get(@enumFromInt(item.id));
        switch (node.tag) {
            .exposed_item_lower => {
                if (node.data.rhs == 1) {
                    return .{ .lower_ident = .{
                        .region = emptyRegion(),
                        .ident = node.main_token,
                        .as = node.data.lhs,
                    } };
                }
                return .{ .lower_ident = .{
                    .region = emptyRegion(),
                    .ident = node.main_token,
                    .as = null,
                } };
            },
            .exposed_item_upper => {
                if (node.data.rhs == 1) {
                    return .{ .upper_ident = .{
                        .region = emptyRegion(),
                        .ident = node.main_token,
                        .as = node.data.lhs,
                    } };
                }
                return .{ .upper_ident = .{
                    .region = emptyRegion(),
                    .ident = node.main_token,
                    .as = null,
                } };
            },
            .exposed_item_upper_star => {
                return .{ .upper_ident_star = .{
                    .region = emptyRegion(),
                    .ident = node.main_token,
                } };
            },
            else => {
                std.debug.panic("Expected a valid exposed item tag, got {s}", .{@tagName(node.tag)});
            },
        }
    }

    pub fn emptyRegion() Region {
        return .{ .start = 0, .end = 0 };
    }

    pub fn getStatement(store: *NodeStore, statement: StatementIdx) Statement {
        const node = store.nodes.get(@enumFromInt(statement.id));
        switch (node.tag) {
            .decl => {
                return .{ .decl = .{
                    .pattern = .{ .id = node.data.lhs },
                    .body = .{ .id = node.data.rhs },
                    .region = emptyRegion(),
                } };
            },
            .expr => {
                return .{ .expr = .{
                    .expr = .{ .id = node.data.lhs },
                    .region = emptyRegion(),
                } };
            },
            .import => {
                const rhs = @as(ImportRhs, @bitCast(node.data.rhs));
                var extra_data_pos = node.data.lhs + rhs.num_exposes;
                var qualifier_tok: ?TokenIdx = null;
                var alias_tok: ?TokenIdx = null;
                if (rhs.qualified == 1) {
                    qualifier_tok = store.extra_data.items[extra_data_pos];
                    extra_data_pos += 1;
                }
                if (rhs.aliased == 1) {
                    alias_tok = store.extra_data.items[extra_data_pos];
                }
                const i = Statement.Import{
                    .module_name_tok = node.main_token,
                    .qualifier_tok = qualifier_tok,
                    .alias_tok = alias_tok,
                    .exposes = .{ .span = .{
                        .start = node.data.lhs,
                        .len = rhs.num_exposes,
                    } },
                    .region = emptyRegion(),
                };
                const imp = Statement{ .import = i };
                return imp;
            },
            .expect => {
                return .{ .expect = .{
                    .body = .{ .id = node.data.lhs },
                    .region = emptyRegion(),
                } };
            },
            .crash => {
                return .{ .crash = .{
                    .expr = .{ .id = node.data.lhs },
                    .region = emptyRegion(),
                } };
            },
            .@"return" => {
                return .{ .@"return" = .{
                    .expr = .{ .id = node.data.lhs },
                    .region = emptyRegion(),
                } };
            },
            .type_decl => {
                return .{ .type_decl = .{
                    .region = emptyRegion(),
                    .header = .{ .id = node.data.lhs },
                    .anno = .{ .id = node.data.rhs },
                } };
            },
            .type_anno => {
                return .{ .type_anno = .{
                    .region = emptyRegion(),
                    .name = node.data.lhs,
                    .anno = .{ .id = node.data.rhs },
                } };
            },
            else => {
                std.debug.panic("Expected a valid statement tag, got {s}", .{@tagName(node.tag)});
            },
        }
    }

    pub fn getPattern(store: *NodeStore, pattern: PatternIdx) Pattern {
        const node = store.nodes.get(@enumFromInt(pattern.id));
        switch (node.tag) {
            .ident_patt => {
                return .{ .ident = .{
                    .ident_tok = node.main_token,
                    .region = emptyRegion(),
                } };
            },
            .tag_patt => {
                return .{ .tag = .{
                    .tag_tok = node.main_token,
                    .args = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                    .region = emptyRegion(),
                } };
            },
            .string_patt => {
                return .{ .string = .{
                    .string_tok = node.main_token,
                    .region = emptyRegion(),
                    .expr = .{ .id = node.data.lhs },
                } };
            },
            .number_patt => {
                return .{ .number = .{
                    .number_tok = node.main_token,
                    .region = emptyRegion(),
                } };
            },
            .record_patt => {
                return .{ .record = .{
                    .region = emptyRegion(),
                    .fields = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                } };
            },
            .list_patt => {
                return .{ .list = .{
                    .region = emptyRegion(),
                    .patterns = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                } };
            },
            .list_rest_patt => {
                return .{ .list_rest = .{
                    .region = emptyRegion(),
                    .name = if (node.data.lhs == 0) null else node.data.lhs,
                } };
            },
            .tuple_patt => {
                return .{ .tuple = .{
                    .region = emptyRegion(),
                    .patterns = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                } };
            },
            .alternatives_patt => {
                return .{ .alternatives = .{
                    .region = emptyRegion(),
                    .patterns = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                } };
            },
            .underscore_patt => {
                return .{ .underscore = .{
                    .region = emptyRegion(),
                } };
            },
            else => {
                std.debug.panic("Expected a valid pattern tag, got {s}", .{@tagName(node.tag)});
            },
        }
    }

    pub fn getExpr(store: *NodeStore, expr: ExprIdx) Expr {
        const node = store.nodes.get(@enumFromInt(expr.id));
        switch (node.tag) {
            .int => {
                return .{ .int = .{
                    .token = node.main_token,
                    .region = emptyRegion(),
                } };
            },
            .ident => {
                var qualifier: ?TokenIdx = null;
                if (node.data.rhs == 1) {
                    qualifier = node.data.lhs;
                }
                return .{ .ident = .{
                    .token = node.main_token,
                    .qualifier = qualifier,
                    .region = emptyRegion(),
                } };
            },
            .tag => {
                return .{ .tag = .{
                    .region = emptyRegion(),
                    .token = node.main_token,
                } };
            },
            .string_part => {
                return .{ .string_part = .{
                    .region = emptyRegion(),
                    .token = node.main_token,
                } };
            },
            .string => {
                return .{ .string = .{
                    .token = node.main_token,
                    .parts = .{ .span = DataSpan{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                    .region = emptyRegion(),
                } };
            },
            .list => {
                return .{ .list = .{
                    .items = .{ .span = DataSpan{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                    .region = emptyRegion(),
                } };
            },
            .tuple => {
                return .{ .tuple = .{
                    .items = .{ .span = DataSpan{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                    .region = emptyRegion(),
                } };
            },
            .record => {
                return .{ .record = .{
                    .fields = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                    .region = emptyRegion(),
                } };
            },
            .field_access => {
                return .{ .field_access = .{
                    .left = .{ .id = node.data.lhs },
                    .right = .{ .id = node.data.rhs },
                    .operator = node.main_token,
                    .region = emptyRegion(),
                } };
            },
            .lambda => {
                return .{ .lambda = .{
                    .body = .{
                        .id = @as(u32, @intCast(store.extra_data.items[@as(usize, @intCast(node.main_token))])),
                    },
                    .args = .{ .span = .{ .start = node.data.lhs, .len = node.data.rhs } },
                    .region = emptyRegion(),
                } };
            },
            .apply => {
                const function = store.extra_data.items[@as(usize, @intCast(node.main_token))];
                return .{ .apply = .{
                    .@"fn" = .{ .id = function },
                    .args = .{ .span = DataSpan{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                    .region = emptyRegion(),
                } };
            },
            .suffix_single_question => {
                return .{ .suffix_single_question = .{
                    .region = emptyRegion(),
                    .operator = node.main_token,
                    .expr = .{ .id = node.data.lhs },
                } };
            },
            .if_then_else => {
                const then_idx = @as(usize, @intCast(node.data.rhs));
                const else_idx = then_idx + 1;
                const then_ed = store.extra_data.items[then_idx];
                const else_ed = store.extra_data.items[else_idx];
                return .{ .if_then_else = .{
                    .region = emptyRegion(),
                    .condition = .{ .id = node.data.lhs },
                    .then = .{ .id = then_ed },
                    .@"else" = .{ .id = else_ed },
                } };
            },
            .match => {
                const expr_idx = @as(usize, @intCast(node.data.lhs + node.data.rhs));
                return .{ .match = .{
                    .region = emptyRegion(),
                    .expr = .{ .id = store.extra_data.items[expr_idx] },
                    .branches = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                } };
            },
            .dbg => {
                return .{ .dbg = .{
                    .region = emptyRegion(),
                    .expr = .{ .id = node.data.lhs },
                } };
            },
            .bin_op => {
                return .{ .bin_op = .{
                    .left = .{ .id = node.data.lhs },
                    .right = .{ .id = node.data.rhs },
                    .operator = node.main_token,
                    .region = emptyRegion(),
                } };
            },
            .block => {
                const statements = StatementSpan{ .span = .{
                    .start = node.data.lhs,
                    .len = node.data.rhs,
                } };
                return .{ .block = .{
                    .statements = statements,
                    .region = emptyRegion(),
                } };
            },
            .ellipsis => {
                return .{ .ellipsis = .{
                    .region = emptyRegion(),
                } };
            },
            else => {
                std.debug.panic("Expected a valid expr tag, got {s}", .{@tagName(node.tag)});
            },
        }
    }

    pub fn getRecordField(store: *NodeStore, fieldIdx: RecordFieldIdx) RecordField {
        const node = store.nodes.get(@enumFromInt(fieldIdx.id));
        const name = node.main_token;
        const value = if (node.data.lhs > 0) ExprIdx{ .id = node.data.lhs } else null;
        const optional = node.data.rhs == 1;
        return .{
            .name = name,
            .value = value,
            .optional = optional,
            .region = emptyRegion(),
        };
    }

    pub fn getBranch(store: *NodeStore, branch: WhenBranchIdx) WhenBranch {
        const node = store.nodes.get(@enumFromInt(branch.id));
        return .{
            .region = emptyRegion(),
            .pattern = .{ .id = node.data.lhs },
            .body = .{ .id = node.data.rhs },
        };
    }

    pub fn getTypeHeader(store: *NodeStore, header: TypeHeaderIdx) TypeHeader {
        const node = store.nodes.get(@enumFromInt(header.id));
        std.debug.assert(node.tag == .ty_header);
        return .{
            .region = emptyRegion(),
            .name = node.main_token,
            .args = .{ .span = .{
                .start = node.data.lhs,
                .len = node.data.rhs,
            } },
        };
    }

    pub fn getAnnoRecordField(store: *NodeStore, idx: AnnoRecordFieldIdx) AnnoRecordField {
        const node = store.nodes.get(@enumFromInt(idx.id));
        return .{
            .region = emptyRegion(),
            .name = node.data.lhs,
            .ty = .{ .id = node.data.rhs },
        };
    }

    pub fn getTypeAnno(store: *NodeStore, anno: TypeAnnoIdx) TypeAnno {
        const node = store.nodes.get(@enumFromInt(anno.id));

        switch (node.tag) {
            .ty_var => {
                return .{ .ty_var = .{
                    .tok = node.main_token,
                    .region = emptyRegion(),
                } };
            },
            .ty_underscore => {
                return .{ .underscore = .{
                    .region = emptyRegion(),
                } };
            },
            .ty_tag => {
                return .{ .tag = .{
                    .tok = node.main_token,
                    .args = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                    .region = emptyRegion(),
                } };
            },
            .ty_union => {
                const rhs = @as(TypeAnno.TagUnionRhs, @bitCast(node.data.rhs));
                const tags_ed_end = node.data.lhs + rhs.tags_len;

                return .{ .tag_union = .{
                    .region = emptyRegion(),
                    .open_anno = if (rhs.open == 1) .{ .id = store.extra_data.items[tags_ed_end] } else null,
                    .tags = .{ .span = .{
                        .start = node.data.lhs,
                        .len = @as(u32, @intCast(rhs.tags_len)),
                    } },
                } };
            },
            .ty_tuple => {
                return .{ .tuple = .{
                    .region = emptyRegion(),
                    .annos = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                } };
            },
            .ty_record => {
                return .{ .record = .{
                    .region = emptyRegion(),
                    .fields = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                } };
            },
            .ty_fn => {
                return .{ .@"fn" = .{
                    .region = emptyRegion(),
                    .ret = .{ .id = store.extra_data.items[@as(usize, @intCast(node.main_token))] },
                    .args = .{ .span = .{
                        .start = node.data.lhs,
                        .len = node.data.rhs,
                    } },
                } };
            },
            .ty_parens => {
                return .{ .parens = .{
                    .region = emptyRegion(),
                    .anno = .{ .id = node.data.lhs },
                } };
            },
            else => {
                std.debug.panic("Expected a valid type annotation node, found {s}", .{@tagName(node.tag)});
            },
        }

        const nid = store.nodes.append(store.gpa, node);
        return .{ .id = @intFromEnum(nid) };
    }

    // ------------------------------------------------------------------------
    // Node types - these are the constituent types used in the Node Store API
    // ------------------------------------------------------------------------

    /// Represents a Roc file.
    pub const File = struct {
        header: HeaderIdx,
        statements: StatementSpan,
        region: Region,

        pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *IR) sexpr.Expr {
            var file_node = sexpr.Expr.init(env.gpa, "file");

            const header = ir.store.getHeader(self.header);
            var header_node = header.toSExpr(env, ir);

            file_node.appendNodeChild(env.gpa, &header_node);

            for (ir.store.statementSlice(self.statements)) |stmt_id| {
                const stmt = ir.store.getStatement(stmt_id);
                var stmt_node = stmt.toSExpr(env, ir);
                file_node.appendNodeChild(env.gpa, &stmt_node);
            }

            return file_node;
        }
    };

    /// Represents a Body, or a block of statements.
    pub const Body = struct {
        /// The statements that constitute the block
        statements: StatementSpan,
        region: Region,

        pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *IR) sexpr.Expr {
            var block_node = sexpr.Expr.init(env.gpa, "block");

            for (ir.store.statementSlice(self.statements)) |stmt_idx| {
                const stmt = ir.store.getStatement(stmt_idx);

                var stmt_node = stmt.toSExpr(env, ir);

                block_node.appendNodeChild(env.gpa, &stmt_node);
            }

            return block_node;
        }
    };

    /// Represents a module header.
    pub const Header = union(enum) {
        app: struct {
            provides: ExposedItemSpan, // This should probably be a Interned Ident token
            platform: ExprIdx,
            platform_name: TokenIdx,
            packages: RecordFieldSpan,
            region: Region,
        },
        module: struct {
            exposes: ExposedItemSpan,
            region: Region,
        },
        package: struct {
            provides: ExposedItemSpan,
            packages: RecordFieldSpan,
            region: Region,
        },
        platform: struct {
            // TODO: complete this
            region: Region,
        },
        hosted: struct {
            // TODO: complete this
            region: Region,
        },

        const AppHeaderRhs = packed struct { num_packages: u10, num_provides: u22 };

        pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *IR) sexpr.Expr {
            switch (self) {
                .module => |module| {
                    var header_node = sexpr.Expr.init(env.gpa, "header");

                    for (ir.store.exposedItemSlice(module.exposes)) |exposed| {
                        const item = ir.store.getExposedItem(exposed);
                        var item_node = item.toSExpr(env, ir);
                        header_node.appendNodeChild(env.gpa, &item_node);
                    }

                    return header_node;
                },
                else => @panic("not implemented"),
            }
        }
    };

    pub const ExposedItem = union(enum) {
        lower_ident: struct {
            as: ?TokenIdx,
            ident: TokenIdx,
            region: Region,
        },
        upper_ident: struct {
            as: ?TokenIdx,
            ident: TokenIdx,
            region: Region,
        },
        upper_ident_star: struct {
            ident: TokenIdx,
            region: Region,
        },

        pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *IR) sexpr.Expr {
            var node = sexpr.Expr.init(env.gpa, "exposed_item");
            var inner_node = sexpr.Expr.init(env.gpa, @tagName(self));
            switch (self) {
                .lower_ident => |i| {
                    const token = ir.tokens.tokens.get(i.ident);
                    const text = env.idents.getText(token.extra.interned);
                    inner_node.appendStringChild(env.gpa, text);
                    if (i.as) |a| {
                        const as_tok = ir.tokens.tokens.get(a);
                        const as_text = env.idents.getText(as_tok.extra.interned);
                        inner_node.appendStringChild(env.gpa, as_text);
                    }
                },
                .upper_ident => |i| {
                    const token = ir.tokens.tokens.get(i.ident);
                    const text = env.idents.getText(token.extra.interned);
                    inner_node.appendStringChild(env.gpa, text);
                    if (i.as) |a| {
                        const as_tok = ir.tokens.tokens.get(a);
                        const as_text = env.idents.getText(as_tok.extra.interned);
                        inner_node.appendStringChild(env.gpa, as_text);
                    }
                },
                .upper_ident_star => |i| {
                    const token = ir.tokens.tokens.get(i.ident);
                    const text = env.idents.getText(token.extra.interned);
                    inner_node.appendStringChild(env.gpa, text);
                },
            }
            node.appendNodeChild(env.gpa, &inner_node);
            return node;
        }
    };

    /// Represents a statement.  Not all statements are valid in all positions.
    pub const Statement = union(enum) {
        decl: struct {
            pattern: PatternIdx,
            body: ExprIdx,
            region: Region,
        },
        expr: struct {
            expr: ExprIdx,
            region: Region,
        },
        crash: struct {
            expr: ExprIdx,
            region: Region,
        },
        expect: struct {
            body: ExprIdx,
            region: Region,
        },
        @"return": struct {
            expr: ExprIdx,
            region: Region,
        },
        import: Import,
        type_decl: struct {
            header: TypeHeaderIdx,
            anno: TypeAnnoIdx,
            region: Region,
        },
        type_anno: struct {
            name: TokenIdx,
            anno: TypeAnnoIdx,
            region: Region,
        },

        pub const Import = struct {
            module_name_tok: TokenIdx,
            qualifier_tok: ?TokenIdx,
            alias_tok: ?TokenIdx,
            exposes: ExposedItemSpan,
            region: Region,
        };

        pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *IR) sexpr.Expr {
            switch (self) {
                .decl => |decl| {
                    var decl_node = sexpr.Expr.init(env.gpa, "decl");

                    const pattern = ir.store.getPattern(decl.pattern);
                    const body = ir.store.getExpr(decl.body);

                    var pattern_node = pattern.toSExpr(env, ir);
                    var body_node = body.toSExpr(env, ir);

                    decl_node.appendNodeChild(env.gpa, &pattern_node);
                    decl_node.appendNodeChild(env.gpa, &body_node);

                    return decl_node;
                },
                .expr => |expr| {
                    return ir.store.getExpr(expr.expr).toSExpr(env, ir);
                },
                .import => |import| {
                    var node = sexpr.Expr.init(env.gpa, "import");

                    // Module Qualifier e.g. `pf` in `import pf.Stdout`
                    node.appendStringChild(
                        env.gpa,
                        if (import.qualifier_tok) |tok| ir.resolve(tok) else "",
                    );

                    // Module Name e.g. `Stdout` in `import pf.Stdout`
                    node.appendStringChild(
                        env.gpa,
                        ir.resolve(import.module_name_tok),
                    );

                    // Module Alias e.g. `OUT` in `import pf.Stdout as OUT`
                    node.appendStringChild(
                        env.gpa,
                        if (import.alias_tok) |tok| ir.resolve(tok) else "",
                    );

                    // Each exposed identifier e.g. [foo, bar] in `import pf.Stdout exposing [foo, bar]`
                    const exposed_slice = ir.store.exposedItemSlice(import.exposes);
                    if (exposed_slice.len > 0) {
                        var exposed = sexpr.Expr.init(env.gpa, "exposing");
                        for (ir.store.exposedItemSlice(import.exposes)) |e| {
                            var exposed_item = &ir.store.getExposedItem(e);
                            var exposed_item_sexpr = exposed_item.toSExpr(env, ir);
                            exposed.appendNodeChild(env.gpa, &exposed_item_sexpr);
                        }
                        node.appendNodeChild(env.gpa, &exposed);
                    }

                    return node;
                },
                // (type_decl (header <name> [<args>]) <annotation>)
                .type_decl => |a| {
                    var node = sexpr.Expr.init(env.gpa, "type_decl");
                    var header = sexpr.Expr.init(env.gpa, "header");

                    // pattern
                    {
                        const ty_header = ir.store.getTypeHeader(a.header);

                        header.appendStringChild(env.gpa, ir.resolve(ty_header.name));

                        for (ir.store.tokenSlice(ty_header.args)) |b| {
                            header.appendStringChild(env.gpa, ir.resolve(b));
                        }

                        node.appendNodeChild(env.gpa, &header);
                    }
                    // annotation
                    {
                        var annotation = ir.store.getTypeAnno(a.anno).toSExpr(env, ir);
                        node.appendNodeChild(env.gpa, &annotation);
                    }

                    return node;
                },
                // (crash <expr>)
                .crash => |a| {
                    var node = sexpr.Expr.init(env.gpa, "crash");
                    var child = ir.store.getExpr(a.expr).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                    return node;
                },
                // (expect <body>)
                .expect => |a| {
                    var node = sexpr.Expr.init(env.gpa, "expect");
                    var child = ir.store.getExpr(a.body).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                    return node;
                },
                // (return <expr>)
                .@"return" => |a| {
                    var node = sexpr.Expr.init(env.gpa, "return");
                    var child = ir.store.getExpr(a.expr).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                    return node;
                },
                // (type_anno <annotation>)
                .type_anno => |a| {
                    var node = sexpr.Expr.init(env.gpa, "type_anno");
                    node.appendStringChild(env.gpa, ir.resolve(a.name));
                    var child = ir.store.getTypeAnno(a.anno).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &child);
                    return node;
                },
            }
        }
    };

    pub const TypeHeader = struct {
        name: TokenIdx,
        args: TokenSpan,
        region: Region,
    };

    pub const TypeAnno = union(enum) {
        ty_var: struct {
            tok: TokenIdx,
            region: Region,
        },
        underscore: struct {
            region: Region,
        },
        tag: struct {
            tok: TokenIdx,
            args: TypeAnnoSpan,
            region: Region,
        },
        tag_union: struct {
            tags: TypeAnnoSpan,
            open_anno: ?TypeAnnoIdx,
            region: Region,
        },
        tuple: struct {
            annos: TypeAnnoSpan,
            region: Region,
        },
        record: struct {
            fields: AnnoRecordFieldSpan,
            region: Region,
        },
        @"fn": struct {
            args: TypeAnnoSpan,
            ret: TypeAnnoIdx,
            region: Region,
        },
        parens: struct {
            anno: TypeAnnoIdx,
            region: Region,
        },

        const TagUnionRhs = packed struct { open: u1, tags_len: u31 };

        pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *IR) sexpr.Expr {
            switch (self) {
                // (ty_var <var>)
                .ty_var => |a| {
                    var node = sexpr.Expr.init(env.gpa, "ty_var");
                    node.appendStringChild(env.gpa, ir.resolve(a.tok));
                    return node;
                },
                // (_)
                .underscore => {
                    return sexpr.Expr.init(env.gpa, "_");
                },
                // (tag [<args>])
                .tag => |a| {
                    var node = sexpr.Expr.init(env.gpa, "tag");
                    node.appendStringChild(env.gpa, ir.resolve(a.tok));
                    for (ir.store.typeAnnoSlice(a.args)) |b| {
                        var child = ir.store.getTypeAnno(b).toSExpr(env, ir);
                        node.appendNodeChild(env.gpa, &child);
                    }
                    return node;
                },
                .tag_union => {
                    var node = sexpr.Expr.init(env.gpa, "tag_union");
                    node.appendStringChild(env.gpa, "TODO tags");
                    node.appendStringChild(env.gpa, "TODO open_anno");
                    return node;
                },
                // (tuple [<elems>])
                .tuple => |a| {
                    var node = sexpr.Expr.init(env.gpa, "tuple");
                    for (ir.store.typeAnnoSlice(a.annos)) |b| {
                        var child = ir.store.getTypeAnno(b).toSExpr(env, ir);
                        node.appendNodeChild(env.gpa, &child);
                    }
                    return node;
                },
                // (record [<fields>])
                .record => |a| {
                    var node = sexpr.Expr.init(env.gpa, "record");
                    for (ir.store.annoRecordFieldSlice(a.fields)) |_| {
                        // TODO print S-expression for each field
                        node.appendStringChild(env.gpa, "<field>");
                    }
                    return node;
                },
                // (fn <ret> [<args>])
                .@"fn" => |a| {
                    var node = sexpr.Expr.init(env.gpa, "fn");

                    // return value
                    var ret = ir.store.getTypeAnno(a.ret).toSExpr(env, ir);
                    node.appendNodeChild(env.gpa, &ret);

                    // arguments
                    for (ir.store.typeAnnoSlice(a.args)) |b| {
                        var child = ir.store.getTypeAnno(b).toSExpr(env, ir);
                        node.appendNodeChild(env.gpa, &child);
                    }

                    return node;
                },
                // ignore parens... use inner
                .parens => |a| {
                    return ir.store.getTypeAnno(a.anno).toSExpr(env, ir);
                },
            }
        }
    };

    pub const AnnoRecordField = struct {
        name: TokenIdx,
        ty: TypeAnnoIdx,
        region: Region,
    };

    /// Represents a Pattern used in pattern matching.
    pub const Pattern = union(enum) {
        ident: struct {
            ident_tok: TokenIdx,
            region: Region,
        },
        tag: struct {
            tag_tok: TokenIdx,
            args: PatternSpan,
            region: Region,
        },
        number: struct {
            number_tok: TokenIdx,
            region: Region,
        },
        string: struct {
            string_tok: TokenIdx,
            region: Region,
            expr: ExprIdx,
        },
        record: struct {
            fields: PatternRecordFieldSpan,
            region: Region,
        },
        list: struct {
            patterns: PatternSpan,
            region: Region,
        },
        list_rest: struct {
            name: ?TokenIdx,
            region: Region,
        },
        tuple: struct {
            patterns: PatternSpan,
            region: Region,
        },
        underscore: struct {
            region: Region,
        },
        alternatives: struct {
            patterns: PatternSpan,
            region: Region,
        },

        pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *IR) sexpr.Expr {
            switch (self) {
                .ident => |ident| {
                    var node = sexpr.Expr.init(env.gpa, "ident");

                    node.appendStringChild(env.gpa, ir.resolve(ident.ident_tok));

                    return node;
                },
                else => @panic("formatting for this pattern not yet implemented"),
            }
        }
    };

    /// Represents an expression.
    pub const Expr = union(enum) {
        int: struct {
            token: TokenIdx,
            region: Region,
        },
        float: struct {
            token: TokenIdx,
            region: Region,
        },
        string_part: struct { // TODO: this should be more properly represented in its own union enum
            token: TokenIdx,
            region: Region,
        },
        string: struct {
            token: TokenIdx,
            region: Region,
            parts: ExprSpan,
        },
        list: struct {
            items: ExprSpan,
            region: Region,
        },
        tuple: struct {
            items: ExprSpan,
            region: Region,
        },
        record: struct {
            fields: RecordFieldSpan,
            region: Region,
        },
        tag: struct {
            token: TokenIdx,
            region: Region,
        },
        lambda: struct {
            args: PatternSpan,
            body: ExprIdx,
            region: Region,
        },
        apply: struct {
            args: ExprSpan,
            @"fn": ExprIdx,
            region: Region,
        },
        record_updater: struct {
            token: TokenIdx,
            region: Region,
        },
        field_access: BinOp,
        bin_op: BinOp,
        suffix_single_question: Unary,
        unary_op: Unary,
        if_then_else: struct {
            condition: ExprIdx,
            then: ExprIdx,
            @"else": ExprIdx,
            region: Region,
        },
        match: struct {
            expr: ExprIdx,
            branches: WhenBranchSpan,
            region: Region,
        },
        ident: struct {
            token: TokenIdx,
            qualifier: ?TokenIdx,
            region: Region,
        },
        dbg: struct {
            expr: ExprIdx,
            region: Region,
        },
        record_builder: struct {
            mapper: ExprIdx,
            fields: RecordFieldIdx,
        },
        ellipsis: struct {
            region: Region,
        },
        block: Body,

        pub fn as_string_part_region(self: @This()) !Region {
            switch (self) {
                .string_part => |part| return part.region,
                else => return error.ExpectedStringPartRegion,
            }
        }

        pub fn toSExpr(self: @This(), env: *base.ModuleEnv, ir: *IR) sexpr.Expr {
            switch (self) {
                .int => |int| {
                    var expr = sexpr.Expr.init(env.gpa, "int");
                    expr.appendStringChild(env.gpa, ir.resolve(int.token));
                    return expr;
                },
                .string => |str| {
                    var sexpr_str = sexpr.Expr.init(env.gpa, "string");
                    for (ir.store.exprSlice(str.parts)) |part_id| {
                        const part_expr = ir.store.getExpr(part_id);
                        var part_sexpr = part_expr.toSExpr(env, ir);
                        sexpr_str.appendNodeChild(env.gpa, &part_sexpr);
                    }
                    return sexpr_str;
                },
                .string_part => |sp| {
                    const text = ir.resolve(sp.token);
                    const owned_str: []u8 = env.gpa.dupe(u8, text) catch |err| exitOnOom(err);
                    return sexpr.Expr{ .string = owned_str };
                },
                // (tag <tag>)
                .tag => |tag| {
                    var node = sexpr.Expr.init(env.gpa, "tag");
                    node.appendStringChild(env.gpa, ir.resolve(tag.token));
                    return node;
                },
                .block => |block| {
                    return block.toSExpr(env, ir);
                },
                // (binop <op> <lhs> <rhs>)
                .bin_op => |binop| {
                    var binop_sexpr = sexpr.Expr.init(env.gpa, "binop");

                    binop_sexpr.appendStringChild(env.gpa, ir.resolve(binop.operator));

                    var lhs = ir.store.getExpr(binop.left).toSExpr(env, ir);
                    var rhs = ir.store.getExpr(binop.right).toSExpr(env, ir);
                    binop_sexpr.appendNodeChild(env.gpa, &lhs);
                    binop_sexpr.appendNodeChild(env.gpa, &rhs);

                    return binop_sexpr;
                },
                // (if_then_else <condition> <then> <else>)
                .if_then_else => |stmt| {
                    var node = sexpr.Expr.init(env.gpa, "if_then_else");

                    var condition = ir.store.getExpr(stmt.condition).toSExpr(env, ir);
                    var then = ir.store.getExpr(stmt.then).toSExpr(env, ir);
                    var else_ = ir.store.getExpr(stmt.@"else").toSExpr(env, ir);

                    node.appendNodeChild(env.gpa, &condition);
                    node.appendNodeChild(env.gpa, &then);
                    node.appendNodeChild(env.gpa, &else_);

                    return node;
                },
                .ident => |ident| {
                    var ident_sexpr = sexpr.Expr.init(env.gpa, "ident");
                    ident_sexpr.appendStringChild(env.gpa, if (ident.qualifier != null) ir.resolve(ident.qualifier.?) else "");
                    ident_sexpr.appendStringChild(env.gpa, ir.resolve(ident.token));
                    return ident_sexpr;
                },
                else => {
                    std.debug.print("Format for Expr {}", .{self});
                    @panic("not implemented yet");
                },
            }
        }
    };

    pub const PatternRecordField = struct {
        name: TokenIdx,
        value: ?PatternIdx,
        rest: bool,
        region: Region,
    };

    pub const RecordField = struct {
        name: TokenIdx,
        value: ?ExprIdx,
        optional: bool,
        region: Region,
    };

    pub const IfElse = struct {
        condition: ExprIdx,
        body: ExprIdx,
        region: Region,
    };

    pub const WhenBranch = struct {
        pattern: PatternIdx,
        body: ExprIdx,
        region: Region,
    };

    pub const BinOp = struct {
        left: ExprIdx,
        right: ExprIdx,
        operator: TokenIdx,
        region: Region,
    };

    pub const Unary = struct {
        operator: TokenIdx,
        expr: ExprIdx,
        region: Region,
    };

    pub const DataSpan = struct {
        start: u32,
        len: u32,
    };

    pub const ExprSpan = struct { span: DataSpan };
    pub const StatementSpan = struct { span: DataSpan };
    pub const TokenSpan = struct { span: DataSpan };
    pub const PatternSpan = struct { span: DataSpan };
    pub const PatternRecordFieldSpan = struct { span: DataSpan };
    pub const RecordFieldSpan = struct { span: DataSpan };
    pub const WhenBranchSpan = struct { span: DataSpan };
    pub const TypeAnnoSpan = struct { span: DataSpan };
    pub const AnnoRecordFieldSpan = struct { span: DataSpan };
    pub const ExposedItemSpan = struct { span: DataSpan };

    /// Returns the start position for a new Span of ExprIdxs in scratch
    pub fn scratchExprTop(store: *NodeStore) u32 {
        return @as(u32, @intCast(store.scratch_exprs.items.len));
    }

    /// Places a new ExprIdx in the scratch.  Will panic on OOM.
    pub fn addScratchExpr(store: *NodeStore, idx: ExprIdx) void {
        store.scratch_exprs.append(store.gpa, idx) catch |err| exitOnOom(err);
    }

    /// Creates a new span starting at start.  Moves the items from scratch
    /// to extra_data as appropriate.
    pub fn exprSpanFrom(store: *NodeStore, start: u32) ExprSpan {
        const end = store.scratch_exprs.items.len;
        defer store.scratch_exprs.shrinkRetainingCapacity(start);
        var i = @as(usize, @intCast(start));
        const ed_start = @as(u32, @intCast(store.extra_data.items.len));
        std.debug.assert(end >= i);
        while (i < end) {
            store.extra_data.append(store.gpa, store.scratch_exprs.items[i].id) catch |err| exitOnOom(err);
            i += 1;
        }
        return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
    }

    /// Clears any ExprIds added to scratch from start until the end.
    /// Should be used wherever the scratch items will not be used,
    /// as in when parsing fails.
    pub fn clearScratchExprsFrom(store: *NodeStore, start: u32) void {
        store.scratch_exprs.shrinkRetainingCapacity(start);
    }

    /// Returns a new ExprIter so that the caller can iterate through
    /// all items in the span.
    pub fn exprSlice(store: *NodeStore, span: ExprSpan) []ExprIdx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }

    /// Returns the start position for a new Span of StatementIdxs in scratch
    pub fn scratchStatementTop(store: *NodeStore) u32 {
        return @as(u32, @intCast(store.scratch_statements.items.len));
    }

    /// Places a new StatementIdx in the scratch.  Will panic on OOM.
    pub fn addScratchStatement(store: *NodeStore, idx: StatementIdx) void {
        store.scratch_statements.append(store.gpa, idx) catch |err| exitOnOom(err);
    }

    /// Creates a new span starting at start.  Moves the items from scratch
    /// to extra_data as appropriate.
    pub fn statementSpanFrom(store: *NodeStore, start: u32) StatementSpan {
        const end = store.scratch_statements.items.len;
        defer store.scratch_statements.shrinkRetainingCapacity(start);
        var i = @as(usize, @intCast(start));
        const ed_start = @as(u32, @intCast(store.extra_data.items.len));
        std.debug.assert(end >= i);
        while (i < end) {
            store.extra_data.append(store.gpa, store.scratch_statements.items[i].id) catch |err| exitOnOom(err);
            i += 1;
        }
        return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
    }

    /// Clears any StatementIds added to scratch from start until the end.
    /// Should be used wherever the scratch items will not be used,
    /// as in when parsing fails.
    pub fn clearScratchStatementsFrom(store: *NodeStore, start: u32) void {
        store.scratch_statements.shrinkRetainingCapacity(start);
    }

    /// Returns a new Statement slice so that the caller can iterate through
    /// all items in the span.
    pub fn statementSlice(store: *NodeStore, span: StatementSpan) []StatementIdx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }

    /// Returns the start position for a new Span of patternIdxs in scratch
    pub fn scratchPatternTop(store: *NodeStore) u32 {
        return @as(u32, @intCast(store.scratch_patterns.items.len));
    }

    /// Places a new PatternIdx in the scratch.  Will panic on OOM.
    pub fn addScratchPattern(store: *NodeStore, idx: PatternIdx) void {
        store.scratch_patterns.append(store.gpa, idx) catch |err| exitOnOom(err);
    }

    /// Creates a new span starting at start.  Moves the items from scratch
    /// to extra_data as appropriate.
    pub fn patternSpanFrom(store: *NodeStore, start: u32) PatternSpan {
        const end = store.scratch_patterns.items.len;
        defer store.scratch_patterns.shrinkRetainingCapacity(start);
        var i = @as(usize, @intCast(start));
        const ed_start = @as(u32, @intCast(store.extra_data.items.len));
        std.debug.assert(end >= i);
        while (i < end) {
            store.extra_data.append(store.gpa, store.scratch_patterns.items[i].id) catch |err| exitOnOom(err);
            i += 1;
        }
        return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
    }

    /// Clears any PatternIds added to scratch from start until the end.
    /// Should be used wherever the scratch items will not be used,
    /// as in when parsing fails.
    pub fn clearScratchPatternsFrom(store: *NodeStore, start: u32) void {
        store.scratch_patterns.shrinkRetainingCapacity(start);
    }

    /// Returns a new Pattern slice so that the caller can iterate through
    /// all items in the span.
    pub fn patternSlice(store: *NodeStore, span: PatternSpan) []PatternIdx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }

    /// Returns a new PatternRecordFieldIter so that the caller can iterate through
    /// all items in the span.
    pub fn patternRecordFieldSlice(store: *NodeStore, span: PatternRecordFieldSpan) []PatternRecordFieldIdx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }
    /// Returns the start position for a new Span of patternRecordFieldIdxs in scratch
    pub fn scratchPatternRecordFieldTop(store: *NodeStore) u32 {
        return @as(u32, @intCast(store.scratch_pattern_record_fields.items.len));
    }

    /// Places a new PatternRecordFieldIdx in the scratch.  Will panic on OOM.
    pub fn addScratchPatternRecordField(store: *NodeStore, idx: PatternRecordFieldIdx) void {
        store.scratch_pattern_record_fields.append(store.gpa, idx) catch |err| exitOnOom(err);
    }

    /// Creates a new span starting at start.  Moves the items from scratch
    /// to extra_data as appropriate.
    pub fn patternRecordFieldSpanFrom(store: *NodeStore, start: u32) PatternRecordFieldSpan {
        const end = store.scratch_pattern_record_fields.items.len;
        defer store.scratch_pattern_record_fields.shrinkRetainingCapacity(start);
        var i = @as(usize, @intCast(start));
        const ed_start = @as(u32, @intCast(store.extra_data.items.len));
        while (i < end) {
            store.extra_data.append(store.gpa, store.scratch_pattern_record_fields.items[i].id) catch |err| exitOnOom(err);
            i += 1;
        }
        return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
    }

    /// Clears any PatternRecordFieldIds added to scratch from start until the end.
    /// Should be used wherever the scratch items will not be used,
    /// as in when parsing fails.
    pub fn clearScratchPatternRecordFieldsFrom(store: *NodeStore, start: u32) void {
        store.scratch_pattern_record_fields.shrinkRetainingCapacity(start);
    }

    /// Returns a new RecordField slice so that the caller can iterate through
    /// all items in the span.
    pub fn recordFieldSlice(store: *NodeStore, span: RecordFieldSpan) []RecordFieldIdx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }
    /// Returns the start position for a new Span of recordFieldIdxs in scratch
    pub fn scratchRecordFieldTop(store: *NodeStore) u32 {
        return @as(u32, @intCast(store.scratch_record_fields.items.len));
    }

    /// Places a new RecordFieldIdx in the scratch.  Will panic on OOM.
    pub fn addScratchRecordField(store: *NodeStore, idx: RecordFieldIdx) void {
        store.scratch_record_fields.append(store.gpa, idx) catch |err| exitOnOom(err);
    }

    /// Creates a new span starting at start.  Moves the items from scratch
    /// to extra_data as appropriate.
    pub fn recordFieldSpanFrom(store: *NodeStore, start: u32) RecordFieldSpan {
        const end = store.scratch_record_fields.items.len;
        defer store.scratch_record_fields.shrinkRetainingCapacity(start);
        var i = @as(usize, @intCast(start));
        const ed_start = @as(u32, @intCast(store.extra_data.items.len));
        while (i < end) {
            store.extra_data.append(store.gpa, store.scratch_record_fields.items[i].id) catch |err| exitOnOom(err);
            i += 1;
        }
        return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
    }

    /// Clears any RecordFieldIds added to scratch from start until the end.
    /// Should be used wherever the scratch items will not be used,
    /// as in when parsing fails.
    pub fn clearScratchRecordFieldsFrom(store: *NodeStore, start: u32) void {
        store.scratch_record_fields.shrinkRetainingCapacity(start);
    }

    /// Returns the start position for a new Span of _LOWER_Idxs in scratch
    pub fn scratchWhenBranchTop(store: *NodeStore) u32 {
        return @as(u32, @intCast(store.scratch_when_branches.items.len));
    }

    /// Places a new WhenBranchIdx in the scratch.  Will panic on OOM.
    pub fn addScratchWhenBranch(store: *NodeStore, idx: WhenBranchIdx) void {
        store.scratch_when_branches.append(store.gpa, idx) catch |err| exitOnOom(err);
    }

    /// Creates a new span starting at start.  Moves the items from scratch
    /// to extra_data as appropriate.
    pub fn whenBranchSpanFrom(store: *NodeStore, start: u32) WhenBranchSpan {
        const end = store.scratch_when_branches.items.len;
        defer store.scratch_when_branches.shrinkRetainingCapacity(start);
        var i = @as(usize, @intCast(start));
        const ed_start = @as(u32, @intCast(store.extra_data.items.len));
        while (i < end) {
            store.extra_data.append(store.gpa, store.scratch_when_branches.items[i].id) catch |err| exitOnOom(err);
            i += 1;
        }
        return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
    }

    /// Clears any WhenBranchIds added to scratch from start until the end.
    /// Should be used wherever the scratch items will not be used,
    /// as in when parsing fails.
    pub fn clearScratchWhenBranchsFrom(store: *NodeStore, start: u32) void {
        store.scratch_when_branches.shrinkRetainingCapacity(start);
    }

    /// Returns a new WhenBranch slice so that the caller can iterate through
    /// all items in the span.
    pub fn whenBranchSlice(store: *NodeStore, span: WhenBranchSpan) []WhenBranchIdx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }

    /// Returns the start position for a new Span of typeAnnoIdxs in scratch
    pub fn scratchTypeAnnoTop(store: *NodeStore) u32 {
        return @as(u32, @intCast(store.scratch_type_annos.items.len));
    }

    /// Places a new TypeAnnoIdx in the scratch.  Will panic on OOM.
    pub fn addScratchTypeAnno(store: *NodeStore, idx: TypeAnnoIdx) void {
        store.scratch_type_annos.append(store.gpa, idx) catch |err| exitOnOom(err);
    }

    /// Creates a new span starting at start.  Moves the items from scratch
    /// to extra_data as appropriate.
    pub fn typeAnnoSpanFrom(store: *NodeStore, start: u32) TypeAnnoSpan {
        const end = store.scratch_type_annos.items.len;
        defer store.scratch_type_annos.shrinkRetainingCapacity(start);
        var i = @as(usize, @intCast(start));
        const ed_start = @as(u32, @intCast(store.extra_data.items.len));
        while (i < end) {
            store.extra_data.append(store.gpa, store.scratch_type_annos.items[i].id) catch |err| exitOnOom(err);
            i += 1;
        }
        return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
    }

    /// Clears any TypeAnnoIds added to scratch from start until the end.
    /// Should be used wherever the scratch items will not be used,
    /// as in when parsing fails.
    pub fn clearScratchTypeAnnosFrom(store: *NodeStore, start: u32) void {
        store.scratch_type_annos.shrinkRetainingCapacity(start);
    }

    /// Returns a new TypeAnno slice so that the caller can iterate through
    /// all items in the span.
    pub fn typeAnnoSlice(store: *NodeStore, span: TypeAnnoSpan) []TypeAnnoIdx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }

    /// Returns the start position for a new Span of annoRecordFieldIdxs in scratch
    pub fn scratchAnnoRecordFieldTop(store: *NodeStore) u32 {
        return @as(u32, @intCast(store.scratch_anno_record_fields.items.len));
    }

    /// Places a new AnnoRecordFieldIdx in the scratch.  Will panic on OOM.
    pub fn addScratchAnnoRecordField(store: *NodeStore, idx: AnnoRecordFieldIdx) void {
        store.scratch_anno_record_fields.append(store.gpa, idx) catch |err| exitOnOom(err);
    }

    /// Creates a new span starting at start.  Moves the items from scratch
    /// to extra_data as appropriate.
    pub fn annoRecordFieldSpanFrom(store: *NodeStore, start: u32) AnnoRecordFieldSpan {
        const end = store.scratch_anno_record_fields.items.len;
        defer store.scratch_anno_record_fields.shrinkRetainingCapacity(start);
        var i = @as(usize, @intCast(start));
        const ed_start = @as(u32, @intCast(store.extra_data.items.len));
        while (i < end) {
            store.extra_data.append(store.gpa, store.scratch_anno_record_fields.items[i].id) catch |err| exitOnOom(err);
            i += 1;
        }
        return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
    }

    /// Clears any AnnoRecordFieldIds added to scratch from start until the end.
    /// Should be used wherever the scratch items will not be used,
    /// as in when parsing fails.
    pub fn clearScratchAnnoRecordFieldsFrom(store: *NodeStore, start: u32) void {
        store.scratch_anno_record_fields.shrinkRetainingCapacity(start);
    }

    /// Returns a new AnnoRecordField slice so that the caller can iterate through
    /// all items in the span.
    pub fn annoRecordFieldSlice(store: *NodeStore, span: AnnoRecordFieldSpan) []AnnoRecordFieldIdx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }

    /// Returns the start position for a new Span of token_Idxs in scratch
    pub fn scratchTokenTop(store: *NodeStore) u32 {
        return @as(u32, @intCast(store.scratch_tokens.items.len));
    }

    /// Places a new TokenIdx in the scratch.  Will panic on OOM.
    pub fn addScratchToken(store: *NodeStore, idx: TokenIdx) void {
        store.scratch_tokens.append(store.gpa, idx) catch |err| exitOnOom(err);
    }

    /// Creates a new span starting at start.  Moves the items from scratch
    /// to extra_data as appropriate.
    pub fn tokenSpanFrom(store: *NodeStore, start: u32) TokenSpan {
        const end = store.scratch_tokens.items.len;
        defer store.scratch_tokens.shrinkRetainingCapacity(start);
        var i = @as(usize, @intCast(start));
        const ed_start = @as(u32, @intCast(store.extra_data.items.len));
        while (i < end) {
            store.extra_data.append(store.gpa, store.scratch_tokens.items[i]) catch |err| exitOnOom(err);
            i += 1;
        }
        return .{ .span = .{ .start = ed_start, .len = @as(u32, @intCast(end)) - start } };
    }

    /// Clears any TokenIds added to scratch from start until the end.
    /// Should be used wherever the scratch items will not be used,
    /// as in when parsing fails.
    pub fn clearScratchTokensFrom(store: *NodeStore, start: u32) void {
        store.scratch_tokens.shrinkRetainingCapacity(start);
    }

    /// Returns a new Token slice so that the caller can iterate through
    /// all items in the span.
    pub fn tokenSlice(store: *NodeStore, span: TokenSpan) []TokenIdx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }

    /// Returns the start position for a new Span of exposedItemIdxs in scratch
    pub fn scratchExposedItemTop(store: *NodeStore) u32 {
        return @as(u32, @intCast(store.scratch_anno_record_fields.items.len));
    }

    /// Places a new ExposedItemIdx in the scratch.  Will panic on OOM.
    pub fn addScratchExposedItem(store: *NodeStore, idx: ExposedItemIdx) void {
        store.scratch_exposed_items.append(store.gpa, idx) catch |err| exitOnOom(err);
    }

    /// Creates a new span starting at start.  Moves the items from scratch
    /// to extra_data as appropriate.
    pub fn exposedItemSpanFrom(store: *NodeStore, start: u32) ExposedItemSpan {
        const end = store.scratch_exposed_items.items.len;
        defer store.scratch_exposed_items.shrinkRetainingCapacity(start);
        var i = @as(usize, @intCast(start));
        const ed_start = @as(u32, @intCast(store.extra_data.items.len));
        while (i < end) {
            store.extra_data.append(store.gpa, store.scratch_exposed_items.items[i].id) catch |err| exitOnOom(err);
            i += 1;
        }
        const span = DataSpan{ .start = ed_start, .len = @as(u32, @intCast(end)) - start };
        return .{ .span = span };
    }

    /// Clears any ExposedItemIds added to scratch from start until the end.
    /// Should be used wherever the scratch items will not be used,
    /// as in when parsing fails.
    pub fn clearScratchExposedItemsFrom(store: *NodeStore, start: u32) void {
        store.scratch_exposed_items.shrinkRetainingCapacity(start);
    }

    /// Returns a new ExposedItem slice so that the caller can iterate through
    /// all items in the span.
    pub fn exposedItemSlice(store: *NodeStore, span: ExposedItemSpan) []ExposedItemIdx {
        return @ptrCast(store.extra_data.items[span.span.start..(span.span.start + span.span.len)]);
    }
};

/// Resolve a token index to a string slice from the source code.
pub fn resolve(self: *IR, token: TokenIdx) []const u8 {
    const range = self.tokens.resolve(token);
    return self.source[@intCast(range.start.offset)..@intCast(range.end.offset)];
}

/// todo -- I'm not sure what this is
pub const ImportRhs = packed struct { aliased: u1, qualified: u1, num_exposes: u30 };

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

    var node = file.toSExpr(env, ir);
    defer node.deinit(env.gpa);

    node.toStringPretty(writer);
}
