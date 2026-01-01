//! A single meaningful node in the Abstract Syntax Tree.
//! Should always be inserted and fetched from a Node Store.
//!
//! The Tag represents what type of Node it is, and
//! therefore how it's data and main_token fields should
//! be interpreted.

const collections = @import("collections");

const AST = @import("AST.zig");
const TokenIdx = @import("tokenize.zig").Token.Idx;

const Node = @This();

tag: Tag,
data: Data,
main_token: TokenIdx,
region: AST.TokenizedRegion,

/// A SafeMultiList of Nodes
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
    hosted_header,
    package_header,
    platform_header,
    type_module_header,
    default_app_header,

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
    /// A declaration of a reassignable binding
    /// Example: `var a_ = some_expr`
    /// * main_token - pattern node index
    /// * lhs - value node index
    @"var",
    /// Any plain expression - see Exprs below
    /// * lhs - node index to actual expr node
    /// * rhs - ignored
    expr,
    /// A crash statement
    /// Example: `crash "A message"`
    /// * lhs - node index to message(Should be a str_lit)
    /// * rhs - ignored
    crash,
    /// A debug statement
    /// Example: `dbg someValue`
    /// * lhs - node index to expression being debugged
    /// * rhs - ignored
    dbg,
    /// Any plain expression - see Exprs below
    /// * lhs - node index for block or expr
    /// * rhs - ignored
    expect,
    /// A for statement
    /// * main_token - node index for pattern for loop variable
    /// * lhs - node index for loop initializing expression
    /// * rhs - node index for loop body expression
    @"for",
    /// A while statement
    /// * main_token - node index for condition expression
    /// * lhs - node index for condition expression
    /// * rhs - node index for loop body expression
    @"while",
    /// A break statement
    /// * lhs - ignored
    /// * rhs - ignored
    @"break",
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
    /// A Type declaration for aliases
    /// Example: `Color := { red : U8, green: U8, blue: U8 }`
    /// Example: `Color := [Red, Green, Blue]`
    /// * main_token - upper_ident for type ident
    /// * lhs - describes extra_data: struct(packed){ num_type_args: u31, has_where: u1 }
    /// * rhs - extra_data index
    /// * extra_data format (if has_where == 1): [where node index, [type_arg node index]{num_type_args}, type_term node_index]
    /// * extra_data format (if has_where == 0): [[type_arg node index]{num_type_args}, type_term node_index]
    type_decl,
    /// A Type declaration for nominal types
    /// Example: `Color := { red : U8, green: U8, blue: U8 }`
    /// Example: `Color := [Red, Green, Blue]`
    /// * main_token - upper_ident for type ident
    /// * lhs - describes extra_data: struct(packed){ num_type_args: u31, has_where: u1 }
    /// * rhs - extra_data index
    /// * extra_data format (if has_where == 1): [where node index, [type_arg node index]{num_type_args}, type_term node_index]
    /// * extra_data format (if has_where == 0): [[type_arg node index]{num_type_args}, type_term node_index]
    type_decl_nominal,
    /// A Type declaration for opaque types
    /// Example: `Color :: { red : U8, green: U8, blue: U8 }`
    /// Example: `Color :: [Red, Green, Blue]`
    /// * main_token - upper_ident for type ident
    /// * lhs - describes extra_data: struct(packed){ num_type_args: u31, has_where: u1 }
    /// * rhs - extra_data index
    /// * extra_data format (if has_where == 1): [where node index, [type_arg node index]{num_type_args}, type_term node_index]
    /// * extra_data format (if has_where == 0): [[type_arg node index]{num_type_args}, type_term node_index]
    type_decl_opaque,
    /// A Type annotation
    /// Example: `main! : List Str => Try {} _`
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

    /// `List(a, b, c)` for example
    /// * lhs - offset of func (List, in this case)
    /// * rhs - number of type args + 1, to account for the function itself
    ty_apply,

    /// `a` in `MyType(a) : List(a)`
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    ty_var,

    /// `_a` in `MyType(_a) : List(_a)`
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    ty_underscore_var,

    /// `List` in `MyType(a) : List(a)`
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    ty_ty,

    /// `MyMod.MyType` in `Foo : MyMod.MyType`
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    ty_mod_ty,

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

    // Where Clauses

    /// DESCRIPTION
    /// Example: EXAMPLE
    /// DESCRIPTION
    /// Example: EXAMPLE
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    where_mod_alias,
    /// DESCRIPTION
    /// Example: EXAMPLE
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    where_mod_method,

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
    /// Mutable variable binding in pattern
    /// Example: `var $x` in `|var $x, y|`
    /// * main_token - the identifier token
    var_ident_patt,
    /// DESCRIPTION
    /// Example: EXAMPLE
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    tag_patt,
    /// DESCRIPTION
    /// Example: EXAMPLE
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    int_patt,
    /// DESCRIPTION
    /// Example: EXAMPLE
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    frac_patt,
    /// DESCRIPTION
    /// Example: EXAMPLE
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    string_patt,
    /// DESCRIPTION
    /// Example: EXAMPLE
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    single_quote_patt,
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
    frac,
    /// A character literal enclosed in single quotes
    /// Example: 'a'
    /// * main_token - Token index containing the character
    /// * region - Source region containing the single quote literal
    /// * lhs - Unused
    /// * rhs - Unused
    single_quote,
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
    multiline_string,
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
    local_dispatch,
    /// DESCRIPTION
    /// Example: EXAMPLE
    /// * lhs - node index of left expression
    /// * rhs - node index of right expression
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
    /// If-statement (no else branch) - statement form of if, returns unit type
    /// Example: if Bool.true {}
    /// * lhs - node index of condition expr
    /// * rhs - node index of then branch expr
    if_without_else,
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
    record_builder,
    /// A block of statements
    /// Main token is newline preceding the block
    /// * lhs - first statement node
    /// * rhs - number of statements
    block,
    /// A for expression (for loop used as an expression, evaluates to {})
    /// * main_token - node index for pattern for loop variable
    /// * lhs - node index for loop initializing expression
    /// * rhs - node index for loop body expression
    for_expr,
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

    /// Collections
    /// Collection of exposed items
    collection_exposed,

    /// Collection of packages fields
    collection_packages,

    /// Collection of record fields
    collection_record_fields,

    /// Collection of where clauses
    collection_where_clause,

    /// Collection of type annotations
    collection_ty_anno,

    // Target section nodes

    /// A targets section in a platform header
    /// * main_token - files string token (or 0 if no files directive)
    /// * lhs - exe TargetLinkType index (or 0 if none)
    /// * rhs - reserved for future (static_lib, shared_lib)
    targets_section,

    /// A target link type section (exe, static_lib, shared_lib)
    /// * lhs - start of entries span
    /// * rhs - length of entries span
    target_link_type,

    /// A single target entry: x64musl: ["crt1.o", "host.o", app]
    /// * main_token - target name identifier token
    /// * lhs - start of files span
    /// * rhs - length of files span
    target_entry,

    /// A string literal file in a target list: "crt1.o"
    /// * main_token - string token
    target_file_string,

    /// A special identifier in a target list: app, win_gui
    /// * main_token - identifier token
    target_file_ident,

    /// A for-clause type alias: Model : model
    /// * main_token - alias name token (UpperIdent)
    /// * lhs - rigid name token index
    for_clause_type_alias,

    /// A requires entry: [Model : model] for main : () -> { ... }
    /// * main_token - entrypoint name token
    /// * lhs - start of type_aliases span
    /// * rhs - packed: type_aliases len (16 bits) + type_anno idx (16 bits)
    requires_entry,
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

/// Typed payload union for accessing node data in a type-safe manner.
/// This is an extern union that overlays the Data struct (8 bytes = 2 × u32).
/// Each variant corresponds to a Node.Tag and provides typed access to the data.
///
/// For backward compatibility, the Data struct (lhs, rhs) remains the primary
/// interface. The Payload can be accessed via getPayload()/setPayload() helpers.
pub const Payload = extern union {
    /// Raw access to lhs/rhs fields (for backward compatibility)
    raw: Raw,

    // Module headers - lhs: module type, rhs: extra data pointer
    header: Header,

    // Statement nodes
    statement: Statement,
    decl: Decl,
    @"var": Var,
    crash: Crash,
    dbg: Dbg,
    expect: Expect,
    @"for": For,
    @"while": While,
    @"return": Return,
    import: Import,
    type_decl: TypeDecl,
    type_anno: TypeAnno,

    // Type terms
    ty_apply: TyApply,
    ty_span: TySpan,

    // Patterns
    pattern_node: PatternNode,
    pattern_span: PatternSpan,

    // Expressions
    expr_node: ExprNode,
    expr_span: ExprSpan,
    bin_op: BinOp,
    block: Block,
    branch: Branch,
    match: Match,

    // Collections
    collection: Collection,

    // Target section nodes
    targets_section: TargetsSection,
    target_link_type: TargetLinkType,
    target_entry: TargetEntry,
    for_clause_type_alias: ForClauseTypeAlias,
    requires_entry: RequiresEntry,

    /// Raw data fields for direct access
    pub const Raw = extern struct {
        lhs: u32,
        rhs: u32,
    };

    /// Module header payload: lhs = module type, rhs = extra data pointer
    pub const Header = extern struct {
        module_type: u32,
        extra_data_ptr: u32,
    };

    /// Statement wrapper: lhs = actual statement node index, rhs = ignored
    pub const Statement = extern struct {
        actual_node: u32, // Node.Idx
        _unused: u32,
    };

    /// Declaration: lhs = pattern node, rhs = value node
    pub const Decl = extern struct {
        pattern: u32, // Node.Idx
        value: u32, // Node.Idx
    };

    /// Var declaration: lhs = value node, rhs = ignored (main_token = pattern)
    pub const Var = extern struct {
        value: u32, // Node.Idx
        _unused: u32,
    };

    /// Crash statement: lhs = message node, rhs = ignored
    pub const Crash = extern struct {
        msg: u32, // Node.Idx
        _unused: u32,
    };

    /// Dbg statement: lhs = expression node, rhs = ignored
    pub const Dbg = extern struct {
        expr: u32, // Node.Idx
        _unused: u32,
    };

    /// Expect statement: lhs = block or expr node, rhs = ignored
    pub const Expect = extern struct {
        block_or_expr: u32, // Node.Idx
        _unused: u32,
    };

    /// For statement: lhs = init expr, rhs = body expr (main_token = pattern)
    pub const For = extern struct {
        init_expr: u32, // Node.Idx
        body_expr: u32, // Node.Idx
    };

    /// While statement: lhs = condition expr, rhs = body expr
    pub const While = extern struct {
        cond_expr: u32, // Node.Idx
        body_expr: u32, // Node.Idx
    };

    /// Return statement: lhs = expr node, rhs = ignored
    pub const Return = extern struct {
        expr: u32, // Node.Idx
        _unused: u32,
    };

    /// Import: lhs = packed(aliased: u1, num_exposes: u31), rhs = extra_data index
    pub const Import = extern struct {
        packed_info: u32, // aliased (bit 0), num_exposes (bits 1-31)
        extra_data_idx: u32,
    };

    /// Type declaration: lhs = packed(num_type_args: u31, has_where: u1), rhs = extra_data index
    pub const TypeDecl = extern struct {
        packed_info: u32, // num_type_args (bits 0-30), has_where (bit 31)
        extra_data_idx: u32,
    };

    /// Type annotation: lhs = has_where (0 or 1), rhs = extra_data index
    pub const TypeAnno = extern struct {
        has_where: u32,
        extra_data_idx: u32,
    };

    /// Type application: lhs = func offset, rhs = num_type_args + 1
    pub const TyApply = extern struct {
        func_offset: u32,
        num_args_plus_one: u32,
    };

    /// Generic type span: lhs = start, rhs = length
    pub const TySpan = extern struct {
        start: u32,
        len: u32,
    };

    /// Single pattern node reference: lhs = pattern index, rhs = varies
    pub const PatternNode = extern struct {
        pattern: u32, // Node.Idx
        extra: u32,
    };

    /// Pattern span: lhs = start, rhs = length
    pub const PatternSpan = extern struct {
        start: u32,
        len: u32,
    };

    /// Single expression node reference
    pub const ExprNode = extern struct {
        expr: u32, // Node.Idx
        extra: u32,
    };

    /// Expression span: lhs = start, rhs = length
    pub const ExprSpan = extern struct {
        start: u32,
        len: u32,
    };

    /// Binary operation: lhs = left expr, rhs = right expr
    pub const BinOp = extern struct {
        left: u32, // Node.Idx
        right: u32, // Node.Idx
    };

    /// Block: lhs = first statement node, rhs = number of statements
    pub const Block = extern struct {
        first_statement: u32, // Node.Idx
        num_statements: u32,
    };

    /// Match branch: lhs = pattern index, rhs = body index
    pub const Branch = extern struct {
        pattern: u32, // Node.Idx
        body: u32, // Node.Idx
    };

    /// Match expression: lhs = extra_data start, rhs = extra_data end
    pub const Match = extern struct {
        extra_data_start: u32,
        extra_data_end: u32,
    };

    /// Collection: lhs = start, rhs = length
    pub const Collection = extern struct {
        start: u32,
        len: u32,
    };

    /// Targets section: lhs = exe TargetLinkType index, rhs = reserved
    pub const TargetsSection = extern struct {
        exe_target_link_type: u32, // 0 if none
        reserved: u32,
    };

    /// Target link type: lhs = start of entries span, rhs = length
    pub const TargetLinkType = extern struct {
        entries_start: u32,
        entries_len: u32,
    };

    /// Target entry: lhs = start of files span, rhs = length
    pub const TargetEntry = extern struct {
        files_start: u32,
        files_len: u32,
    };

    /// For-clause type alias: lhs = rigid name token index, rhs = unused
    pub const ForClauseTypeAlias = extern struct {
        rigid_name_token: u32, // Token.Idx
        _unused: u32,
    };

    /// Requires entry: lhs = type_aliases span start, rhs = packed(len: u16, anno_idx: u16)
    pub const RequiresEntry = extern struct {
        type_aliases_start: u32,
        packed_len_and_anno: u32, // len (bits 0-15), type_anno_idx (bits 16-31)
    };

    comptime {
        const std = @import("std");
        std.debug.assert(@sizeOf(Payload) == 8); // Must be exactly 2 u32s
    }
};

/// Get the payload as a typed union for type-safe access to node data.
/// This reinterprets the data field as a Payload union.
pub fn getPayload(self: *const Node) Payload {
    return @as(*const Payload, @ptrCast(&self.data)).*;
}

/// Set the payload from a typed union value.
/// This writes the payload data to the node's data field.
pub fn setPayload(self: *Node, p: Payload) void {
    const raw = @as(*const [2]u32, @ptrCast(&p)).*;
    self.data.lhs = raw[0];
    self.data.rhs = raw[1];
}
