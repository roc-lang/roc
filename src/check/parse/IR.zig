const IR = @This();

pub fn deinit(self: *IR) void {
    defer self.tokens.deinit(self.store.gpa);
    defer self.store.deinit();
}

source: []const u8,
tokens: TokenizedBuffer,
store: NodeStore,
errors: []const Diagnostic,

/// Diagnostics related to parsing
pub const Diagnostic = struct {
    tag: Tag,
    region: Region,

    pub const Tag = enum {
        bad_indent,
        multiple_platforms,
        no_platform,
        unexpected_token,
        missing_header,
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

        // Type terms

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
        ty_star,
        ty_underscore,

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
        bin_op_add,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        bin_op_sub,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        bin_op_div,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        bin_op_mul,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        bin_op_or,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        bin_op_and,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        bin_op_dbl_question,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        bin_op_single_question,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        suffix_single_question,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - LHS DESCRIPTION
        /// * rhs - RHS DESCRIPTION
        unary_neg,
        /// DESCRIPTION
        /// Example: EXAMPLE
        /// * lhs - node index of expr
        /// * rhs - ignored
        unary_not,
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
        when,
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
        // TODO: Add the rest of the expressions

        /// A block of statements
        /// Main token is newline preceeding the block
        /// * lhs - first statement node
        /// * rhs - number of statements
        block,
        /// The end of a block of states
        /// Main token is final newline of block
        /// * lhs - ignored
        /// * rgs - ignored
        block_end,
    };
};

/// Typesafe access to an underlying SoA of Nodes.
/// This - along with the types used in its API - should
/// be the only way that other modules interact with
/// the IR (AST).
pub const NodeStore = struct {
    nodes: Node.List,
    extra_data: std.ArrayList(u32),
    gpa: std.mem.Allocator,
    scratch_statements: std.ArrayList(StatementIdx),
    scratch_tokens: std.ArrayList(TokenIdx),
    scratch_exprs: std.ArrayList(ExprIdx),
    scratch_patterns: std.ArrayList(PatternIdx),
    scratch_record_fields: std.ArrayList(RecordFieldIdx),
    scratch_pattern_record_fields: std.ArrayList(PatternRecordFieldIdx),

    /// Initialize the store with an assumed capacity to
    /// ensure resizing of underlying data structures happens
    /// very rarely.
    pub fn initWithCapacity(gpa: std.mem.Allocator, capacity: usize) NodeStore {
        var store: NodeStore = .{
            .nodes = collections.SafeMultiList(Node).init(gpa),
            .extra_data = std.ArrayList(u32).init(gpa),
            .gpa = gpa,
            .scratch_statements = std.ArrayList(StatementIdx).init(gpa),
            .scratch_tokens = std.ArrayList(TokenIdx).init(gpa),
            .scratch_exprs = std.ArrayList(ExprIdx).init(gpa),
            .scratch_patterns = std.ArrayList(PatternIdx).init(gpa),
            .scratch_record_fields = std.ArrayList(RecordFieldIdx).init(gpa),
            .scratch_pattern_record_fields = std.ArrayList(PatternRecordFieldIdx).init(gpa),
        };

        store.nodes.ensureTotalCapacity(capacity);
        _ = store.nodes.append(.{
            .tag = .root,
            .main_token = 0,
            .data = .{ .lhs = 0, .rhs = 0 },
        });
        store.extra_data.ensureTotalCapacity(capacity / 2) catch exitOnOom();
        store.scratch_statements.ensureTotalCapacity(10) catch exitOnOom();
        store.scratch_tokens.ensureTotalCapacity(10) catch exitOnOom();
        store.scratch_exprs.ensureTotalCapacity(10) catch exitOnOom();
        store.scratch_patterns.ensureTotalCapacity(10) catch exitOnOom();
        store.scratch_record_fields.ensureTotalCapacity(10) catch exitOnOom();
        store.scratch_pattern_record_fields.ensureTotalCapacity(10) catch exitOnOom();

        return store;
    }

    /// Deinitializes all data owned by the store.
    /// A caller should ensure that they have taken
    /// ownership of all Node data before calling this
    /// method.
    pub fn deinit(store: *NodeStore) void {
        store.nodes.deinit();
        store.extra_data.deinit();
        store.scratch_statements.deinit();
        store.scratch_tokens.deinit();
        store.scratch_exprs.deinit();
        store.scratch_patterns.deinit();
        store.scratch_record_fields.deinit();
        store.scratch_pattern_record_fields.deinit();
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
    }

    // Idx types

    /// An index for a File node.  Should not be constructed externally.
    pub const FileIdx = struct { id: u32 };
    /// An index for a Body node.  Should not be constructed externally.
    pub const BodyIdx = struct { id: u32 };
    /// An index for a Header node.  Should not be constructed externally.
    pub const HeaderIdx = struct { id: u32 };
    /// An index for a Statement node.  Should not be constructed externally.
    pub const StatementIdx = struct { id: u32 };
    /// An index for a Pattern node.  Should not be constructed externally.
    pub const PatternIdx = struct { id: u32 };
    /// An index for a Expr node.  Should not be constructed externally.
    pub const ExprIdx = struct { id: u32 };
    /// An index for a IfElse node.  Should not be constructed externally.
    pub const IfElseIdx = struct { id: u32 };
    /// An index for a WhenBranch node.  Should not be constructed externally.
    pub const WhenBranchIdx = struct { id: u32 };
    /// An index for a RecordField node.  Should not be constructed externally.
    pub const RecordFieldIdx = struct { id: u32 };
    /// An index for a PatternRecordField node.  Should not be constructed externally.
    pub const PatternRecordFieldIdx = struct { id: u32 };

    // ------------------------------------------------------------------------
    // Creation API - All nodes should be added using these functions
    // ------------------------------------------------------------------------

    /// Any node type can be malformed, but must come with a diagnostic reason
    pub fn addMalformed(store: *NodeStore, comptime t: type, reason: Diagnostic.Tag, token: TokenIdx) t {
        const nid = store.nodes.append(.{
            .tag = .malformed,
            .main_token = token,
            .data = .{ .lhs = @intFromEnum(reason), .rhs = 0 },
        });
        return .{ .id = @intFromEnum(nid) };
    }

    pub fn addFile(store: *NodeStore, file: File) FileIdx {
        const start = store.extra_data.items.len;
        store.extra_data.append(file.header.id) catch exitOnOom();
        for (file.statements) |statement| {
            store.extra_data.append(statement.id) catch exitOnOom();
        }

        store.nodes.set(@enumFromInt(0), .{
            .tag = .root,
            .main_token = 0,
            .data = .{
                .lhs = @as(u32, @intCast(start)),
                .rhs = @as(u32, @intCast(file.statements.len + 1)),
            },
        });

        return FileIdx{ .id = 0 };
    }

    pub fn addBody(store: *NodeStore, body: Body) BodyIdx {
        const start = store.extra_data.items.len;
        const len = @as(u31, @intCast(body.statements.len));
        if (body.whitespace) |ws| {
            store.extra_data.append(ws) catch exitOnOom();
        }
        for (body.statements) |statement| {
            store.extra_data.append(statement.id) catch exitOnOom();
        }

        const rhs = BodyRhs{
            .has_whitespace = if (body.whitespace != null) 1 else 0,
            .num_statements = @as(u31, len),
        };
        const nid = store.nodes.append(.{
            .tag = .block,
            .main_token = 0,
            .data = .{
                .lhs = @as(u32, @intCast(start)),
                .rhs = @as(u32, @bitCast(rhs)),
            },
        });
        return .{ .id = @intFromEnum(nid) };
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
                //    provides: []const TokenIdx, // This should probably be a Interned Ident token
                //    platform: TokenIdx,
                //    platform_name: TokenIdx,
                //    packages: []const RecordFieldIdx,
                //    region: Region,
                // }
                node.tag = .app_header;
                node.main_token = app.platform_name;
                node.data.lhs = @as(u32, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(u32, @bitCast(Header.AppHeaderRhs{
                    .num_packages = @as(u10, @intCast(app.packages.len)),
                    .num_provides = @as(u22, @intCast(app.provides.len)),
                }));

                store.extra_data.append(app.platform) catch exitOnOom();

                for (app.packages) |p| {
                    store.extra_data.append(p.id) catch exitOnOom();
                }
                for (app.provides) |p| {
                    store.extra_data.append(p) catch exitOnOom();
                }
            },
            else => {},
        }
        const nid = store.nodes.append(node);
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
                var lhs = ImportLhs{
                    .aliased = 0,
                    .qualified = 0,
                    .num_exposes = @as(u30, @intCast(i.exposes.len)),
                };
                const extra_data_start = store.extra_data.items.len;
                if (i.qualifier_tok) |tok| {
                    lhs.qualified = 1;
                    store.extra_data.append(tok) catch exitOnOom();
                }
                if (i.alias_tok) |tok| {
                    lhs.aliased = 1;
                    store.extra_data.append(tok) catch exitOnOom();
                }
                node.data.lhs = @as(u32, @bitCast(lhs));
                if (node.data.lhs > 0) {
                    node.data.rhs = @as(u32, @intCast(extra_data_start));
                }
                for (i.exposes) |e| {
                    store.extra_data.append(@as(u32, @intCast(e))) catch exitOnOom();
                }
            },
            .type_decl => |_| {},
            .type_anno => |_| {},
        }
        const nid = store.nodes.append(node);
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
            },
            .number => |n| {
                node.tag = .number_patt;
                node.main_token = n.number_tok;
            },
            .string => |s| {
                node.tag = .string_patt;
                node.main_token = s.string_tok;
            },
            .record => |r| {
                std.debug.assert(r.fields.len > 1);
                node.tag = .list_patt;
                node.data.lhs = @as(u32, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(u32, @intCast(r.fields.len));

                for (r.fields) |f| {
                    store.extra_data.append(f.id) catch exitOnOom();
                }
            },
            .list => |l| {
                std.debug.assert(l.patterns.len > 1);
                node.tag = .list_patt;
                node.data.lhs = @as(u32, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(u32, @intCast(l.patterns.len));

                for (l.patterns) |p| {
                    store.extra_data.append(p.id) catch exitOnOom();
                }
            },
            .list_rest => |_| {
                node.tag = .list_rest_patt;
            },
            .tuple => |t| {
                std.debug.assert(t.patterns.len > 1);
                node.tag = .tuple_patt;
                node.data.lhs = @as(u32, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(u32, @intCast(t.patterns.len));

                for (t.patterns) |p| {
                    store.extra_data.append(p.id) catch exitOnOom();
                }
            },
            .underscore => |_| {
                node.tag = .underscore_patt;
            },
            .alternatives => |a| {
                std.debug.assert(a.patterns.len > 1);
                node.tag = .alternatives_patt;
                node.data.lhs = @as(u32, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(u32, @intCast(a.patterns.len));

                for (a.patterns) |p| {
                    store.extra_data.append(p.id) catch exitOnOom();
                }
            },
            .as => |a| {
                node.tag = .as_patt;
                node.data.lhs = a.pattern.id;
            },
        }
        const nid = store.nodes.append(node);
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
            .string => |e| {
                node.tag = .string;
                node.main_token = e.token;
            },
            .list => |_| {},
            .tuple => |_| {},
            .record => |_| {},
            .tag => |e| {
                node.tag = .tag;
                node.main_token = e.token;
            },
            .lambda => |l| {
                node.data.lhs = @as(u32, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(u32, @intCast(l.args.len));
                store.extra_data.append(l.body.id) catch exitOnOom();
                for (l.args) |arg| {
                    store.extra_data.append(arg.id) catch exitOnOom();
                }
            },
            .apply => |app| {
                node.tag = .apply;
                node.data.lhs = @as(u32, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(u32, @intCast(app.args.len + 1));
                store.extra_data.append(app.@"fn".id) catch exitOnOom();
                for (app.args) |arg| {
                    store.extra_data.append(arg.id) catch exitOnOom();
                }
            },
            .record_updater => |_| {},
            .field_access => |_| {},
            .bin_op_add => |op| {
                node.tag = .bin_op_add;
                node.data.lhs = op.left.id;
                node.data.rhs = op.right.id;
            },
            .bin_op_sub => |op| {
                node.tag = .bin_op_sub;
                node.data.lhs = op.left.id;
                node.data.rhs = op.right.id;
            },
            .bin_op_div => |op| {
                node.tag = .bin_op_div;
                node.data.lhs = op.left.id;
                node.data.rhs = op.right.id;
            },
            .bin_op_mul => |op| {
                node.tag = .bin_op_mul;
                node.data.lhs = op.left.id;
                node.data.rhs = op.right.id;
            },
            .bin_op_or => |op| {
                node.tag = .bin_op_or;
                node.data.lhs = op.left.id;
                node.data.rhs = op.right.id;
            },
            .bin_op_and => |op| {
                node.tag = .bin_op_and;
                node.data.lhs = op.left.id;
                node.data.rhs = op.right.id;
            },
            .bin_op_dbl_question => |op| {
                node.tag = .bin_op_dbl_question;
                node.data.lhs = op.left.id;
                node.data.rhs = op.right.id;
            },
            .bin_op_single_question => |op| {
                node.tag = .bin_op_single_question;
                node.data.lhs = op.left.id;
                node.data.rhs = op.right.id;
            },
            .suffix_single_question => |_| {},
            .unary_neg => |_| {},
            .unary_not => |_| {},
            .if_then_else => |_| {},
            .when => |_| {},
            .ident => |id| {
                node.tag = .ident;
                node.main_token = id.token;
                if (id.qualifier) |qualifier| {
                    node.data.lhs = qualifier;
                    node.data.rhs = 1;
                }
            },
            .dbg => |_| {},
            .record_builder => |_| {},
        }
        const nid = store.nodes.append(node);
        return .{ .id = @intFromEnum(nid) };
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

        const nid = store.nodes.append(node);
        return .{ .id = @intFromEnum(nid) };
    }

    // ------------------------------------------------------------------------
    // Read API - All nodes should be accessed using these functions
    // ------------------------------------------------------------------------

    pub fn getFile(store: *NodeStore, file: FileIdx) File {
        const node = store.nodes.get(@enumFromInt(file.id));
        const header = store.extra_data.items[node.data.lhs];
        const stmt_idxs = store.extra_data.items[(node.data.lhs + 1)..(node.data.lhs + node.data.rhs)];
        std.debug.assert(store.scratch_statements.items.len == 0);
        const scratch_top = store.scratch_statements.items.len;
        for (stmt_idxs) |idx| {
            store.scratch_statements.append(StatementIdx{ .id = idx }) catch exitOnOom();
        }
        const statements = store.scratch_statements.items[scratch_top..];
        store.scratch_statements.shrinkRetainingCapacity(scratch_top);

        return .{
            .header = .{ .id = header },
            .statements = statements,
            .region = .{ .start = 0, .end = 0 },
        };
    }
    pub fn getHeader(store: *NodeStore, header: HeaderIdx) Header {
        const node = store.nodes.get(@enumFromInt(header.id));
        switch (node.tag) {
            .app_header => {
                const extra_data_start = node.data.lhs;
                const rhs = @as(Header.AppHeaderRhs, @bitCast(node.data.rhs));
                const data = store.extra_data.items[extra_data_start..(extra_data_start + rhs.num_packages + rhs.num_provides + 1)];
                var position: u32 = 0;
                const platform = data[0];
                position += 1;
                std.debug.assert(store.scratch_statements.items.len == 0);
                const scratch_rf_top = store.scratch_record_fields.items.len;
                const scratch_tok_top = store.scratch_tokens.items.len;
                while (position < (rhs.num_packages + 1)) {
                    store.scratch_record_fields.append(.{ .id = @as(u32, @intCast(data[position])) }) catch exitOnOom();
                    store.scratch_tokens.append(data[position]) catch exitOnOom();
                    position += 1;
                }
                const packages = store.scratch_record_fields.items[scratch_rf_top..];
                store.scratch_record_fields.shrinkRetainingCapacity(scratch_rf_top);
                while (position < (rhs.num_provides + rhs.num_packages + 1)) {
                    store.scratch_tokens.append(data[position]) catch exitOnOom();
                    position += 1;
                }
                const provides = store.scratch_tokens.items[scratch_tok_top..];
                store.scratch_tokens.shrinkRetainingCapacity(scratch_tok_top);
                return .{
                    .app = .{
                        .platform = platform,
                        .platform_name = node.main_token,
                        .packages = packages,
                        .provides = provides,
                        .region = .{ .start = 0, .end = 0 },
                    },
                };
            },
            else => {
                std.debug.panic("Expected a valid header tag, got {s}", .{@tagName(node.tag)});
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
                const lhs = @as(ImportLhs, @bitCast(node.data.lhs));
                var extra_data_pos = node.data.rhs;
                const start = @as(usize, @intCast(extra_data_pos));
                const optional_fields_len = @as(usize, @intCast(lhs.qualified + lhs.aliased));
                const num_exposes_len = @as(usize, @intCast(lhs.num_exposes));
                const extra_data_end = start + optional_fields_len + num_exposes_len;
                var qualifier_tok: ?TokenIdx = null;
                var alias_tok: ?TokenIdx = null;
                if (lhs.qualified == 1) {
                    qualifier_tok = store.extra_data.items[extra_data_pos];
                    extra_data_pos += 1;
                }
                if (lhs.aliased == 1) {
                    alias_tok = store.extra_data.items[extra_data_pos];
                    extra_data_pos += 1;
                }
                const scratch_tok_top = store.scratch_tokens.items.len;
                while (extra_data_pos < extra_data_end) {
                    store.scratch_tokens.append(store.extra_data.items[extra_data_pos]) catch exitOnOom();
                }
                const exposes = store.scratch_tokens.items[scratch_tok_top..];
                store.scratch_tokens.shrinkRetainingCapacity(scratch_tok_top);
                return .{ .import = .{
                    .module_name_tok = node.main_token,
                    .qualifier_tok = qualifier_tok,
                    .alias_tok = alias_tok,
                    .exposes = exposes,
                    .region = emptyRegion(),
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
            else => {
                std.debug.panic("Expected a valid pattern tag, got {s}", .{@tagName(node.tag)});
            },
        }
    }

    pub fn getExpr(store: *NodeStore, expr: ExprIdx) Expr {
        const node = store.nodes.get(@enumFromInt(expr.id));
        switch (node.tag) {
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
            .string => {
                return .{ .string = .{
                    .token = node.main_token,
                    .region = emptyRegion(),
                } };
            },
            .lambda => {
                var extra_data_pos = @as(usize, @intCast(node.data.lhs));
                const body_len = 1;
                const args_len = @as(usize, @intCast(node.data.rhs));
                const extra_data_end = extra_data_pos + args_len + body_len;
                const body = BodyIdx{
                    .id = @as(u32, @intCast(store.extra_data.items[extra_data_pos])),
                };
                extra_data_pos += 1;
                while (extra_data_pos < extra_data_end) {
                    store.scratch_patterns.append(.{ .id = @as(u32, @intCast(store.extra_data.items[extra_data_pos])) }) catch exitOnOom();
                }
                return .{ .lambda = .{
                    .body = body,
                    .args = store.scratch_patterns.toOwnedSlice() catch exitOnOom(),
                    .region = emptyRegion(),
                } };
            },
            .apply => {
                const extra_data_start = @as(usize, @intCast(node.data.lhs));
                const extra_data_len = @as(usize, @intCast(node.data.rhs));
                const function = store.extra_data.items[extra_data_start];
                const scratch_top = store.scratch_exprs.items.len;
                const data = store.extra_data.items[(extra_data_start + 1)..(extra_data_start + extra_data_len)];
                for (data) |arg| {
                    store.scratch_exprs.append(.{ .id = arg }) catch exitOnOom();
                }
                const args = store.scratch_exprs.items[scratch_top..];
                store.scratch_exprs.shrinkRetainingCapacity(scratch_top);
                return .{ .apply = .{
                    .@"fn" = .{ .id = function },
                    .args = args,
                    .region = emptyRegion(),
                } };
            },
            else => {
                std.debug.panic("Expected a valid expr tag, got {s}", .{@tagName(node.tag)});
            },
        }
    }

    pub fn getBody(store: *NodeStore, body: BodyIdx) Body {
        const node = store.nodes.get(@enumFromInt(body.id));
        const rhs = @as(BodyRhs, @bitCast(node.data.rhs));
        const start = if (rhs.has_whitespace == 1) node.data.lhs + 1 else node.data.lhs;
        const whitespace: ?TokenIdx = if (rhs.has_whitespace == 1) store.extra_data.items[node.data.lhs] else null;
        const statement_data = store.extra_data.items[start..(start + rhs.num_statements)];
        const scratch_top = store.scratch_statements.items.len;
        for (statement_data) |i| {
            store.scratch_statements.append(.{ .id = i }) catch exitOnOom();
        }
        const statements = store.scratch_statements.items[scratch_top..];
        store.scratch_statements.shrinkRetainingCapacity(scratch_top);
        return .{
            .statements = statements,
            .whitespace = whitespace,
        };
    }

    // ------------------------------------------------------------------------
    // Node types - these are the constituent types used in the Node Store API
    // ------------------------------------------------------------------------

    /// Represents a Roc file.
    pub const File = struct {
        header: HeaderIdx,
        statements: []const StatementIdx,
        region: Region,
    };

    /// Represents a Body, or a block of statements.
    pub const Body = struct {
        /// The statements that constitute the block
        statements: []const StatementIdx,
        /// The token that represents the newline preceeding this block, if any
        whitespace: ?TokenIdx,
    };

    /// Represents a module header.
    pub const Header = union(enum) {
        app: struct {
            provides: []const TokenIdx, // This should probably be a Interned Ident token
            platform: TokenIdx,
            platform_name: TokenIdx,
            packages: []const RecordFieldIdx,
            region: Region,
        },
        module: struct {
            exposes: []const TokenIdx,
            region: Region,
        },
        package: struct {
            provides: []const TokenIdx,
            packages: []const RecordFieldIdx,
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
    };

    /// Represents a statement.  Not all statements are valid in all positions.
    pub const Statement = union(enum) {
        decl: struct {
            pattern: PatternIdx,
            body: BodyIdx,
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
            body: BodyIdx,
            region: Region,
        },
        @"return": struct {
            expr: ExprIdx,
            region: Region,
        },
        import: struct {
            module_name_tok: TokenIdx,
            qualifier_tok: ?TokenIdx,
            alias_tok: ?TokenIdx,
            exposes: []const TokenIdx,
            region: Region,
        },
        type_decl: struct {
            // TODO: complete this
            region: Region,
        },
        type_anno: struct {
            // TODO: complete this
            region: Region,
        },
    };

    /// Represents a Pattern used in pattern matching.
    pub const Pattern = union(enum) {
        ident: struct {
            ident_tok: TokenIdx,
            region: Region,
        },
        tag: struct {
            tag_tok: TokenIdx,
            region: Region,
        },
        number: struct {
            number_tok: TokenIdx,
            region: Region,
        },
        string: struct {
            string_tok: TokenIdx,
            region: Region,
        },
        record: struct {
            fields: []const PatternRecordFieldIdx,
        },
        list: struct {
            patterns: []const PatternIdx,
            region: Region,
        },
        list_rest: struct {
            region: Region,
        },
        tuple: struct {
            patterns: []const PatternIdx,
            region: Region,
        },
        underscore: struct {
            region: Region,
        },
        alternatives: struct {
            patterns: []const PatternIdx,
            region: Region,
        },
        as: struct {
            pattern: PatternIdx,
            region: Region,
        },
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
        string: struct {
            token: TokenIdx,
            region: Region,
        },
        list: struct {
            items: []const ExprIdx,
            region: Region,
        },
        tuple: struct {
            items: []const ExprIdx,
            region: Region,
        },
        record: struct {
            fields: []const RecordFieldIdx,
        },
        tag: struct {
            token: TokenIdx,
            region: Region,
        },
        lambda: struct {
            args: []const PatternIdx,
            body: BodyIdx,
            region: Region,
        },
        apply: struct {
            args: []const ExprIdx,
            @"fn": ExprIdx,
            region: Region,
        },
        record_updater: struct {
            token: TokenIdx,
            region: Region,
        },
        field_access: struct {
            ident_tok: TokenIdx,
            @"struct": ExprIdx,
            region: Region,
        },
        bin_op_add: BinOp,
        bin_op_sub: BinOp,
        bin_op_div: BinOp,
        bin_op_mul: BinOp,
        bin_op_or: BinOp,
        bin_op_and: BinOp,
        bin_op_dbl_question: BinOp,
        bin_op_single_question: BinOp,
        suffix_single_question: Unary,
        unary_neg: Unary,
        unary_not: Unary,
        if_then_else: struct {
            condition: ExprIdx,
            then: BodyIdx,
            @"else": BodyIdx,
            ifelses: []const IfElseIdx,
            region: Region,
        },
        when: struct {
            expr: ExprIdx,
            branches: []const WhenBranchIdx,
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
    };

    pub const PatternRecordField = struct {
        name: TokenIdx,
        value: ?PatternIdx,
        optional: bool,
    };
    pub const RecordField = struct {
        name: TokenIdx,
        value: ?ExprIdx,
        optional: bool,
    };

    pub const IfElse = struct {
        condition: ExprIdx,
        body: BodyIdx,
        region: Region,
    };
    pub const WhenBranch = struct {
        pattern: PatternIdx,
        region: Region,
    };

    pub const BinOp = struct {
        left: ExprIdx,
        right: ExprIdx,
        region: Region,
    };
    pub const Unary = struct {
        expr: ExprIdx,
        region: Region,
    };
};

pub fn resolve(self: *IR, token: TokenIdx) []const u8 {
    const range = self.tokens.resolve(token);
    return self.source[@intCast(range.start.offset)..@intCast(range.end.offset)];
}

pub const ImportLhs = packed struct { aliased: u1, qualified: u1, num_exposes: u30 };
pub const BodyRhs = packed struct { has_whitespace: u1, num_statements: u31 };

// Check that all packed structs are 4 bytes size as they as cast to
// and from a u32
comptime {
    std.debug.assert(@sizeOf(BodyRhs) == 4);
    std.debug.assert(@sizeOf(NodeStore.Header.AppHeaderRhs) == 4);
    std.debug.assert(@sizeOf(ImportLhs) == 4);
}

test {
    _ = std.testing.refAllDeclsRecursive(@This());
}

const std = @import("std");
const tokenize = @import("tokenize.zig");
const TokenizedBuffer = tokenize.TokenizedBuffer;
const TokenIdx = tokenize.Token.Idx;
const collections = @import("../../collections.zig");
const exitOnOom = @import("../../collections/utils.zig").exitOnOom;
