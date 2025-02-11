const IR = @This();

pub fn deinit(self: *IR) void {
    defer self.store.deinit();
    defer self.tokens.deinit();
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
        // TODO
    };
};

/// The first and last token consumed by a Node
pub const Region = struct {
    start: TokenIndex,
    end: TokenIndex,
};

/// Unstructured information about a Node.  These
/// can actually point to either Tokens or other Nodes,
/// or represent lengths or be packed structs with more
/// densely represented information.
///
/// The conventions should be documented for each Node
/// Tag.
pub const Data = struct {
    lhs: Node.Index,
    rhs: Node.Index,
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
    main_token: TokenIndex,

    /// Internal representation for where a node is stored
    /// in the tree.
    pub const Index = u32;

    /// This is the tag associated with a raw Node in the list
    pub const Tag = enum {
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
    nodes: NodeList,
    extra_data: std.ArrayList(Node.Index),
    gpa: std.mem.Allocator,
    scratch_statements: std.ArrayList(StatementIndex),
    scratch_tokens: std.ArrayList(TokenIndex),
    scratch_exprs: std.ArrayList(ExprIndex),
    scratch_patterns: std.ArrayList(PatternIndex),
    scratch_record_fields: std.ArrayList(RecordFieldIndex),
    scratch_pattern_record_fields: std.ArrayList(PatternRecordFieldIndex),

    /// Initialize the store with an assumed capacity to
    /// ensure resizing of underlying data structures happens
    /// very rarely.
    pub fn initWithCapacity(gpa: std.mem.Allocator, capacity: u64) !NodeStore {
        var store: NodeStore = .{
            .nodes = .{},
            .extra_data = std.ArrayList(Node.Index).init(gpa),
            .gpa = gpa,
            .scratch_statements = std.ArrayList(StatementIndex).init(gpa),
            .scratch_tokens = std.ArrayList(TokenIndex).init(gpa),
            .scratch_exprs = std.ArrayList(ExprIndex).init(gpa),
            .scratch_patterns = std.ArrayList(PatternIndex).init(gpa),
            .scratch_record_fields = std.ArrayList(RecordFieldIndex).init(gpa),
            .scratch_pattern_record_fields = std.ArrayList(PatternRecordFieldIndex).init(gpa),
        };

        try store.nodes.ensureTotalCapacity(gpa, capacity);
        try store.nodes.append(gpa, .{
            .tag = .root,
            .main_token = TokenIndex{ .id = 0 },
            .data = .{ .lhs = 0, .rhs = 0 },
        });
        try store.extra_data.ensureTotalCapacity(capacity / 2);
        try store.scratch_statements.ensureTotalCapacity(10);
        try store.scratch_tokens.ensureTotalCapacity(10);
        try store.scratch_exprs.ensureTotalCapacity(10);
        try store.scratch_patterns.ensureTotalCapacity(10);
        try store.scratch_record_fields.ensureTotalCapacity(10);
        try store.scratch_pattern_record_fields.ensureTotalCapacity(10);

        return store;
    }

    /// Deinitializes all data owned by the store.
    /// A caller should ensure that they have taken
    /// ownership of all Node data before calling this
    /// method.
    pub fn deinit(store: *NodeStore) void {
        store.nodes.deinit(store.gpa);
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

    // Index types

    /// An index for a File node.  Should not be constructed externally.
    pub const FileIndex = struct { file: u32 };
    /// An index for a Body node.  Should not be constructed externally.
    pub const BodyIndex = struct { body: u32 };
    /// An index for a Header node.  Should not be constructed externally.
    pub const HeaderIndex = struct { header: u32 };
    /// An index for a Statement node.  Should not be constructed externally.
    pub const StatementIndex = struct { statement: u32 };
    /// An index for a Pattern node.  Should not be constructed externally.
    pub const PatternIndex = struct { pattern: u32 };
    /// An index for a Expr node.  Should not be constructed externally.
    pub const ExprIndex = struct { expr: u32 };
    /// An index for a IfElse node.  Should not be constructed externally.
    pub const IfElseIndex = struct { id: u32 };
    /// An index for a WhenBranch node.  Should not be constructed externally.
    pub const WhenBranchIndex = struct { id: u32 };
    /// An index for a RecordField node.  Should not be constructed externally.
    pub const RecordFieldIndex = struct { id: u32 };
    /// An index for a PatternRecordField node.  Should not be constructed externally.
    pub const PatternRecordFieldIndex = struct { id: u32 };

    // ------------------------------------------------------------------------
    // Creation API - All nodes should be added using these functions
    // ------------------------------------------------------------------------

    pub fn addFile(store: *NodeStore, file: File) !FileIndex {
        const start = store.extra_data.items.len;
        try store.extra_data.append(file.header.header);
        for (file.statements) |statement| {
            try store.extra_data.append(statement.statement);
        }

        store.nodes.set(0, .{
            .tag = .root,
            .main_token = .{ .id = 0 },
            .data = .{
                .lhs = @as(u32, @intCast(start)),
                .rhs = @as(u32, @intCast(file.statements.len + 1)),
            },
        });

        return .{ .file = 0 };
    }

    pub fn addBody(store: *NodeStore, body: Body) !BodyIndex {
        const start = store.extra_data.items.len;
        const len = @as(u31, @intCast(body.statements.len));
        if (body.whitespace) |ws| {
            try store.extra_data.append(ws.id);
        }
        for (body.statements) |statement| {
            try store.extra_data.append(statement.statement);
        }

        const idx = @as(u32, @intCast(store.nodes.len));
        const rhs = BodyRhs{
            .has_whitespace = if (body.whitespace != null) 1 else 0,
            .num_statements = @as(u31, len),
        };
        try store.nodes.append(store.gpa, .{
            .tag = .block,
            .main_token = .{ .id = 0 },
            .data = .{
                .lhs = @as(Node.Index, @intCast(start)),
                .rhs = @as(Node.Index, @bitCast(rhs)),
            },
        });
        return .{ .body = idx };
    }

    pub fn addHeader(store: *NodeStore, header: Header) !HeaderIndex {
        const idx = @as(u32, @intCast(store.nodes.len));
        var node = Node{
            .tag = .statement,
            .main_token = .{ .id = 0 },
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        };
        switch (header) {
            .app => |app| {
                // struct {
                //    provides: []const TokenIndex, // This should probably be a Interned Ident token
                //    platform: TokenIndex,
                //    platform_name: TokenIndex,
                //    packages: []const RecordFieldIndex,
                //    region: Region,
                // }
                node.tag = .app_header;
                node.main_token = app.platform_name;
                node.data.lhs = @as(Node.Index, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(Node.Index, @bitCast(Header.AppHeaderRhs{
                    .num_packages = @as(u10, @intCast(app.packages.len)),
                    .num_provides = @as(u22, @intCast(app.provides.len)),
                }));
                // node.data.rhs = @as(Node.Index, @intCast((app.provides.len + app.packages.len))) + app.platform;
                try store.extra_data.append(app.platform.id);

                for (app.packages) |p| {
                    try store.extra_data.append(@as(Node.Index, @intCast(p.id)));
                }
                for (app.provides) |p| {
                    try store.extra_data.append(@as(Node.Index, @intCast(p.id)));
                }
            },
            else => {},
        }
        try store.nodes.append(store.gpa, node);
        return .{ .header = idx };
    }

    pub fn addStatement(store: *NodeStore, statement: Statement) !StatementIndex {
        const idx = @as(u32, @intCast(store.nodes.len));
        var node = Node{
            .tag = .statement,
            .main_token = .{ .id = 0 },
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        };
        switch (statement) {
            .decl => |d| {
                node.tag = .decl;
                node.data.lhs = @as(Node.Index, @intCast(d.pattern.pattern));
                node.data.rhs = @as(Node.Index, @intCast(d.body.body));
            },
            .expr => |expr| {
                node.tag = .expr;
                node.data.lhs = @as(Node.Index, @intCast(expr.expr.expr));
            },
            .crash => |c| {
                node.tag = .crash;
                node.data.lhs = @as(Node.Index, @intCast(c.expr.expr));
            },
            .expect => |e| {
                node.tag = .expect;
                node.data.lhs = @as(Node.Index, @intCast(e.body.body));
            },
            .@"return" => |r| {
                node.tag = .@"return";
                node.data.lhs = @as(Node.Index, @intCast(r.expr.expr));
            },
            .import => |i| {
                node.tag = .import;
                node.main_token = i.module_name_tok;
                var lhs = ImportLhs{
                    .aliased = 0,
                    .qualified = 0,
                    .num_exposes = @as(u30, @intCast(i.exposes.len)),
                };
                const extra_data_start = @as(Node.Index, @intCast(store.extra_data.items.len));
                if (i.qualifier_tok) |tok| {
                    lhs.qualified = 1;
                    try store.extra_data.append(@as(Node.Index, @intCast(tok.id)));
                }
                if (i.alias_tok) |tok| {
                    lhs.aliased = 1;
                    try store.extra_data.append(@as(Node.Index, @intCast(tok.id)));
                }
                node.data.lhs = @as(Node.Index, @bitCast(lhs));
                if (node.data.lhs > 0) {
                    node.data.rhs = extra_data_start;
                }
                for (i.exposes) |e| {
                    try store.extra_data.append(@as(Node.Index, @intCast(e.id)));
                }
            },
            .type_decl => |_| {},
            .type_anno => |_| {},
        }
        try store.nodes.append(store.gpa, node);
        return .{ .statement = idx };
    }

    pub fn addPattern(store: *NodeStore, pattern: Pattern) !PatternIndex {
        const idx = @as(u32, @intCast(store.nodes.len));
        var node = Node{
            .tag = .statement,
            .main_token = .{ .id = 0 },
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
                node.data.lhs = @as(Node.Index, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(Node.Index, @intCast(r.fields.len));

                for (r.fields) |f| {
                    try store.extra_data.append(@as(Node.Index, @intCast(f.id)));
                }
            },
            .list => |l| {
                std.debug.assert(l.patterns.len > 1);
                node.tag = .list_patt;
                node.data.lhs = @as(Node.Index, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(Node.Index, @intCast(l.patterns.len));

                for (l.patterns) |p| {
                    try store.extra_data.append(@as(Node.Index, @intCast(p.pattern)));
                }
            },
            .list_rest => |_| {
                node.tag = .list_rest_patt;
            },
            .tuple => |t| {
                std.debug.assert(t.patterns.len > 1);
                node.tag = .tuple_patt;
                node.data.lhs = @as(Node.Index, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(Node.Index, @intCast(t.patterns.len));

                for (t.patterns) |p| {
                    try store.extra_data.append(@as(Node.Index, @intCast(p.pattern)));
                }
            },
            .underscore => |_| {
                node.tag = .underscore_patt;
            },
            .alternatives => |a| {
                std.debug.assert(a.patterns.len > 1);
                node.tag = .alternatives_patt;
                node.data.lhs = @as(Node.Index, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(Node.Index, @intCast(a.patterns.len));

                for (a.patterns) |p| {
                    try store.extra_data.append(@as(Node.Index, @intCast(p.pattern)));
                }
            },
            .as => |a| {
                node.tag = .as_patt;
                node.data.lhs = @as(Node.Index, @intCast(a.pattern.pattern));
            },
        }
        try store.nodes.append(store.gpa, node);
        return .{ .pattern = idx };
    }

    pub fn addExpr(store: *NodeStore, expr: Expr) !ExprIndex {
        const idx = @as(u32, @intCast(store.nodes.len));
        var node = Node{
            .tag = .statement,
            .main_token = .{ .id = 0 },
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
                node.data.lhs = @as(Node.Index, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(Node.Index, @intCast(l.args.len));
                try store.extra_data.append(@as(Node.Index, @intCast(l.body.body)));
                for (l.args) |arg| {
                    try store.extra_data.append(@as(Node.Index, @intCast(arg.pattern)));
                }
            },
            .apply => |app| {
                node.tag = .apply;
                node.data.lhs = @as(Node.Index, @intCast(store.extra_data.items.len));
                node.data.rhs = @as(Node.Index, @intCast(app.args.len + 1));
                try store.extra_data.append(@as(Node.Index, @intCast(app.@"fn".expr)));
                for (app.args) |arg| {
                    try store.extra_data.append(@as(Node.Index, @intCast(arg.expr)));
                }
            },
            .record_updater => |_| {},
            .field_access => |_| {},
            .bin_op_add => |op| {
                node.tag = .bin_op_add;
                node.data.lhs = @as(Node.Index, @intCast(op.left.expr));
                node.data.rhs = @as(Node.Index, @intCast(op.right.expr));
            },
            .bin_op_sub => |op| {
                node.tag = .bin_op_sub;
                node.data.lhs = @as(Node.Index, @intCast(op.left.expr));
                node.data.rhs = @as(Node.Index, @intCast(op.right.expr));
            },
            .bin_op_div => |op| {
                node.tag = .bin_op_div;
                node.data.lhs = @as(Node.Index, @intCast(op.left.expr));
                node.data.rhs = @as(Node.Index, @intCast(op.right.expr));
            },
            .bin_op_mul => |op| {
                node.tag = .bin_op_mul;
                node.data.lhs = @as(Node.Index, @intCast(op.left.expr));
                node.data.rhs = @as(Node.Index, @intCast(op.right.expr));
            },
            .bin_op_or => |op| {
                node.tag = .bin_op_or;
                node.data.lhs = @as(Node.Index, @intCast(op.left.expr));
                node.data.rhs = @as(Node.Index, @intCast(op.right.expr));
            },
            .bin_op_and => |op| {
                node.tag = .bin_op_and;
                node.data.lhs = @as(Node.Index, @intCast(op.left.expr));
                node.data.rhs = @as(Node.Index, @intCast(op.right.expr));
            },
            .bin_op_dbl_question => |op| {
                node.tag = .bin_op_dbl_question;
                node.data.lhs = @as(Node.Index, @intCast(op.left.expr));
                node.data.rhs = @as(Node.Index, @intCast(op.right.expr));
            },
            .bin_op_single_question => |op| {
                node.tag = .bin_op_single_question;
                node.data.lhs = @as(Node.Index, @intCast(op.left.expr));
                node.data.rhs = @as(Node.Index, @intCast(op.right.expr));
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
                    node.data.lhs = qualifier.id;
                    node.data.rhs = 1;
                }
            },
            .dbg => |_| {},
            .record_builder => |_| {},
        }
        try store.nodes.append(store.gpa, node);
        return .{ .expr = idx };
    }

    pub fn addRecordField(store: *NodeStore, field: RecordField) !RecordFieldIndex {
        const idx = @as(u32, @intCast(store.nodes.len));
        var node = Node{
            .tag = .statement,
            .main_token = .{ .id = 0 },
            .data = .{
                .lhs = 0,
                .rhs = 0,
            },
        };
        node.tag = .record_field;
        node.main_token = field.name;
        if (field.value) |v| {
            node.data.lhs = v.expr;
        }
        if (field.optional) {
            node.data.rhs = 1;
        }

        try store.nodes.append(store.gpa, node);
        return .{ .id = idx };
    }

    // ------------------------------------------------------------------------
    // Read API - All nodes should be accessed using these functions
    // ------------------------------------------------------------------------

    pub fn getFile(store: *NodeStore, file: FileIndex) !File {
        const node = store.nodes.get(file.file);
        const header = store.extra_data.items[node.data.lhs];
        const stmt_idxs = store.extra_data.items[(node.data.lhs + 1)..(node.data.lhs + node.data.rhs)];
        std.debug.assert(store.scratch_statements.items.len == 0);
        const scratch_top = store.scratch_statements.items.len;
        for (stmt_idxs) |idx| {
            try store.scratch_statements.append(StatementIndex{ .statement = idx });
        }
        const statements = store.scratch_statements.items[scratch_top..];
        store.scratch_statements.shrinkRetainingCapacity(scratch_top);

        return .{
            .header = .{ .header = header },
            .statements = statements,
            .region = .{ .start = .{ .id = 0 }, .end = .{ .id = 0 } },
        };
    }
    pub fn getHeader(store: *NodeStore, header: HeaderIndex) !Header {
        const node = store.nodes.get(header.header);
        switch (node.tag) {
            .app_header => {
                const extra_data_start = node.data.lhs;
                const rhs = @as(Header.AppHeaderRhs, @bitCast(node.data.rhs));
                const data = store.extra_data.items[extra_data_start..(extra_data_start + rhs.num_packages + rhs.num_provides + 1)];
                var position: u32 = 0;
                const platform = TokenIndex{ .id = @as(u32, @intCast(data[0])) };
                position += 1;
                std.debug.assert(store.scratch_statements.items.len == 0);
                const scratch_rf_top = store.scratch_record_fields.items.len;
                const scratch_tok_top = store.scratch_tokens.items.len;
                while (position < (rhs.num_packages + 1)) {
                    try store.scratch_record_fields.append(.{ .id = @as(u32, @intCast(data[position])) });
                    try store.scratch_tokens.append(.{ .id = @as(u32, data[position]) });
                    position += 1;
                }
                const packages = store.scratch_record_fields.items[scratch_rf_top..];
                store.scratch_record_fields.shrinkRetainingCapacity(scratch_rf_top);
                while (position < (rhs.num_provides + rhs.num_packages + 1)) {
                    try store.scratch_tokens.append(.{ .id = @as(u32, data[position]) });
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
                        .region = .{ .start = .{ .id = 0 }, .end = .{ .id = 0 } },
                    },
                };
            },
            else => {
                std.debug.panic("Expected a valid header tag, got {s}", .{@tagName(node.tag)});
            },
        }
    }

    pub fn emptyRegion() Region {
        return .{ .start = .{ .id = 0 }, .end = .{ .id = 0 } };
    }

    pub fn getStatement(store: *NodeStore, statement: StatementIndex) !Statement {
        const node = store.nodes.get(statement.statement);
        switch (node.tag) {
            .decl => {
                return .{ .decl = .{
                    .pattern = .{ .pattern = node.data.lhs },
                    .body = .{ .body = node.data.rhs },
                    .region = emptyRegion(),
                } };
            },
            .expr => {
                return .{ .expr = .{
                    .expr = .{ .expr = node.data.lhs },
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
                var qualifier_tok: ?TokenIndex = null;
                var alias_tok: ?TokenIndex = null;
                if (lhs.qualified == 1) {
                    qualifier_tok = .{ .id = @intCast(store.extra_data.items[extra_data_pos]) };
                    extra_data_pos += 1;
                }
                if (lhs.aliased == 1) {
                    alias_tok = .{ .id = @intCast(store.extra_data.items[extra_data_pos]) };
                    extra_data_pos += 1;
                }
                const scratch_tok_top = store.scratch_tokens.items.len;
                while (extra_data_pos < extra_data_end) {
                    try store.scratch_tokens.append(.{ .id = @intCast(store.extra_data.items[extra_data_pos]) });
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

    pub fn getPattern(store: *NodeStore, pattern: PatternIndex) !Pattern {
        const node = store.nodes.get(pattern.pattern);
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

    pub fn getExpr(store: *NodeStore, expr: ExprIndex) !Expr {
        const node = store.nodes.get(expr.expr);
        switch (node.tag) {
            .ident => {
                var qualifier: ?TokenIndex = null;
                if (node.data.rhs == 1) {
                    qualifier = .{ .id = node.data.lhs };
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
                const body = BodyIndex{
                    .body = @as(u32, @intCast(store.extra_data.items[extra_data_pos])),
                };
                extra_data_pos += 1;
                while (extra_data_pos < extra_data_end) {
                    try store.scratch_patterns.append(.{ .pattern = @as(u32, @intCast(store.extra_data.items[extra_data_pos])) });
                }
                return .{ .lambda = .{
                    .body = body,
                    .args = try store.scratch_patterns.toOwnedSlice(),
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
                    try store.scratch_exprs.append(.{ .expr = arg });
                }
                const args = store.scratch_exprs.items[scratch_top..];
                store.scratch_exprs.shrinkRetainingCapacity(scratch_top);
                return .{ .apply = .{
                    .@"fn" = .{ .expr = function },
                    .args = args,
                    .region = emptyRegion(),
                } };
            },
            else => {
                std.debug.panic("Expected a valid expr tag, got {s}", .{@tagName(node.tag)});
            },
        }
    }

    pub fn getBody(store: *NodeStore, body: BodyIndex) !Body {
        const node = store.nodes.get(body.body);
        const rhs = @as(BodyRhs, @bitCast(node.data.rhs));
        const start = if (rhs.has_whitespace == 1) node.data.lhs + 1 else node.data.lhs;
        const whitespace: ?TokenIndex = if (rhs.has_whitespace == 1) .{ .id = store.extra_data.items[node.data.lhs] } else null;
        const statement_data = store.extra_data.items[start..(start + rhs.num_statements)];
        const scratch_top = store.scratch_statements.items.len;
        for (statement_data) |i| {
            try store.scratch_statements.append(.{ .statement = i });
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
        header: HeaderIndex,
        statements: []const StatementIndex,
        region: Region,
    };

    /// Represents a Body, or a block of statements.
    pub const Body = struct {
        /// The statements that constitute the block
        statements: []const StatementIndex,
        /// The token that represents the newline preceeding this block, if any
        whitespace: ?TokenIndex,
    };

    /// Represents a module header.
    pub const Header = union(enum) {
        app: struct {
            provides: []const TokenIndex, // This should probably be a Interned Ident token
            platform: TokenIndex,
            platform_name: TokenIndex,
            packages: []const RecordFieldIndex,
            region: Region,
        },
        module: struct {
            exposes: []const TokenIndex,
            region: Region,
        },
        package: struct {
            provides: []const TokenIndex,
            packages: []const RecordFieldIndex,
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
            pattern: PatternIndex,
            body: BodyIndex,
            region: Region,
        },
        expr: struct {
            expr: ExprIndex,
            region: Region,
        },
        crash: struct {
            expr: ExprIndex,
            region: Region,
        },
        expect: struct {
            body: BodyIndex,
            region: Region,
        },
        @"return": struct {
            expr: ExprIndex,
            region: Region,
        },
        import: struct {
            module_name_tok: TokenIndex,
            qualifier_tok: ?TokenIndex,
            alias_tok: ?TokenIndex,
            exposes: []const TokenIndex,
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
            ident_tok: TokenIndex,
            region: Region,
        },
        tag: struct {
            tag_tok: TokenIndex,
            region: Region,
        },
        number: struct {
            number_tok: TokenIndex,
            region: Region,
        },
        string: struct {
            string_tok: TokenIndex,
            region: Region,
        },
        record: struct {
            fields: []const PatternRecordFieldIndex,
        },
        list: struct {
            patterns: []const PatternIndex,
            region: Region,
        },
        list_rest: struct {
            region: Region,
        },
        tuple: struct {
            patterns: []const PatternIndex,
            region: Region,
        },
        underscore: struct {
            region: Region,
        },
        alternatives: struct {
            patterns: []const PatternIndex,
            region: Region,
        },
        as: struct {
            pattern: PatternIndex,
            region: Region,
        },
    };

    /// Represents an expression.
    pub const Expr = union(enum) {
        int: struct {
            token: TokenIndex,
            region: Region,
        },
        float: struct {
            token: TokenIndex,
            region: Region,
        },
        string: struct {
            token: TokenIndex,
            region: Region,
        },
        list: struct {
            items: []const ExprIndex,
            region: Region,
        },
        tuple: struct {
            items: []const ExprIndex,
            region: Region,
        },
        record: struct {
            fields: []const RecordFieldIndex,
        },
        tag: struct {
            token: TokenIndex,
            region: Region,
        },
        lambda: struct {
            args: []const PatternIndex,
            body: BodyIndex,
            region: Region,
        },
        apply: struct {
            args: []const ExprIndex,
            @"fn": ExprIndex,
            region: Region,
        },
        record_updater: struct {
            token: TokenIndex,
            region: Region,
        },
        field_access: struct {
            ident_tok: TokenIndex,
            @"struct": ExprIndex,
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
            condition: ExprIndex,
            then: BodyIndex,
            @"else": BodyIndex,
            ifelses: []const IfElseIndex,
            region: Region,
        },
        when: struct {
            expr: ExprIndex,
            branches: []const WhenBranchIndex,
            region: Region,
        },
        ident: struct {
            token: TokenIndex,
            qualifier: ?TokenIndex,
            region: Region,
        },
        dbg: struct {
            expr: ExprIndex,
            region: Region,
        },
        record_builder: struct {
            mapper: ExprIndex,
            fields: RecordFieldIndex,
        },
    };

    pub const PatternRecordField = struct {
        name: TokenIndex,
        value: ?PatternIndex,
        optional: bool,
    };
    pub const RecordField = struct {
        name: TokenIndex,
        value: ?ExprIndex,
        optional: bool,
    };

    pub const IfElse = struct {
        condition: ExprIndex,
        body: BodyIndex,
        region: Region,
    };
    pub const WhenBranch = struct {
        pattern: PatternIndex,
        region: Region,
    };

    pub const BinOp = struct {
        left: ExprIndex,
        right: ExprIndex,
        region: Region,
    };
    pub const Unary = struct {
        expr: ExprIndex,
        region: Region,
    };
};

pub const NodeList = std.MultiArrayList(Node);

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
const TokenizedBuffer = @import("tokenize.zig").TokenizedBuffer;
const TokenIndex = @import("tokenize.zig").Token.List.Idx;
