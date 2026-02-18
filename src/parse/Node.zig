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
    /// An integer with explicit type annotation: 123.U64
    /// * main_token - Token index of the integer literal
    /// * lhs - Token index of the type (e.g., .U64)
    /// * rhs - Unused
    typed_int,
    /// A fractional with explicit type annotation: 3.14.Dec
    /// * main_token - Token index of the fractional literal
    /// * lhs - Token index of the type (e.g., .Dec)
    /// * rhs - Unused
    typed_frac,
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
    /// Tuple element access: tuple.0, tuple.1, etc.
    /// * lhs - node index of tuple expression
    /// * main_token - the element index token (NoSpaceDotInt or DotInt)
    tuple_access,
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
