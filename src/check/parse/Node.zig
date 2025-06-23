//! A single meaningful node in the Abstract Syntax Tree.
//! Should always be inserted and fetched from a Node Store.
//!
//! The Tag represents what type of Node it is, and
//! therefore how it's data and main_token fields should
//! be interpreted.

const collections = @import("../../collections.zig");
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
    /// Any plain expression - see Exprs below
    /// * lhs - node index for block or expr
    /// * rhs - ignored
    expect,
    /// A for statement
    /// * main_token - node index for pattern for loop variable
    /// * lhs - node index for loop initializing expression
    /// * rhs - node index for loop body expression
    @"for",
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

    /// `List(a, b, c)` for example
    /// * lhs - offset of func (List, in this case)
    /// * rhs - number of type args + 1, to account for the function itself
    ty_apply,

    /// `a` in `MyType(a) : List(a)`
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    ty_var,

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
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    where_alias,
    /// DESCRIPTION
    /// Example: EXAMPLE
    /// * lhs - LHS DESCRIPTION
    /// * rhs - RHS DESCRIPTION
    where_method,
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

    /// Collections
    /// Collection of exposed items
    collection_exposed,

    /// Collection of packages fields
    collection_packages,

    /// Collection of where clauses
    collection_where_clause,

    /// Collection of type annotations
    collection_ty_anno,
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
