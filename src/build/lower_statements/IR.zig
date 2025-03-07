const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");

const Ident = base.Ident;
const StringLiteral = base.StringLiteral;

const Self = @This();

env: *base.ModuleEnv,
procedures: std.AutoHashMap(Ident.Idx, Procedure),
constants: std.AutoHashMap(Ident.Idx, StmtWithLayout),
exprs: Expr.List,
layouts: Layout.List,
stmts: Stmt.List,
idents_with_layouts: IdentWithLayout.List,
list_literal_elems: ListLiteralElem.List,

/// todo
pub fn init(env: *base.ModuleEnv) Self {
    return Self{
        .env = env,
        .procedures = std.AutoHashMap(Ident.Idx, Procedure).init(env.gpa),
        .constants = std.AutoHashMap(Ident.Idx, StmtWithLayout).init(env.gpa),
        .exprs = .{},
        .layouts = .{},
        .stmts = .{},
        .idents_with_layouts = .{},
        .list_literal_elems = .{},
    };
}

/// todo
pub fn deinit(self: *Self) void {
    self.procedures.deinit(self.env.gpa);
    self.constants.deinit(self.env.gpa);
    self.exprs.deinit(self.env.gpa);
    self.layouts.deinit(self.env.gpa);
    self.stmts.deinit(self.env.gpa);
    self.idents_with_layouts.deinit(self.env.gpa);
    self.list_literal_elems.deinit(self.env.gpa);
}

/// todo
pub const Procedure = struct {
    arguments: IdentWithLayout.Slice,
    body: Stmt.Idx,
    return_layout: Layout.Idx,
};

// TODO: is this necessary?
/// todo
pub const TagIdIntType = u16;

/// todo
pub const Layout = union(enum) {
    primitive: types.Primitive,
    box: Layout.Idx,
    list: Layout.Idx,
    @"struct": Layout.NonEmptySlice,
    tag_union: Layout.NonEmptySlice,
    // probably necessary for returning empty structs, but would be good to remove this if that's not the case
    unit,

    /// todo
    pub const List = collections.SafeList(@This());
    /// todo
    pub const Idx = List.Idx;
    /// todo
    pub const Slice = List.Slice;
    /// todo
    pub const NonEmptySlice = List.NonEmptySlice;
};

/// todo
pub const IdentWithLayout = struct {
    ident: Ident.Idx,
    layout: Layout.Idx,

    /// todo
    pub const List = collections.SafeList(@This());
    /// todo
    pub const Idx = List.Idx;
    /// todo
    pub const Slice = List.Slice;
};

/// todo
pub const StmtWithLayout = struct {
    stmt: Stmt.Idx,
    layout: Layout.Idx,
};

// TODO: should these use `NonEmptySlice`s?
//
// Copied (and adapted) from:
// https://github.com/roc-lang/roc/blob/689c58f35e0a39ca59feba549f7fcf375562a7a6/crates/compiler/mono/src/layout.rs#L733
/// todo
pub const UnionLayout = union(enum) {
    // TODO: 3 types:
    // - Unwrapped (1 variant converted to the inner type)
    // - Flat (compile normally)
    // - Recursive ("box" the recursion point)
};

/// todo
pub const Expr = union(enum) {
    literal: base.Literal,

    // Functions
    call: Call,

    tag: struct {
        // TODO: should this be an index instead?
        tag_layout: UnionLayout,
        tag_id: TagIdIntType,
        arguments: collections.SafeList(Ident.Idx).Slice,
    },
    @"struct": collections.SafeList(Ident.Idx).NonEmptySlice,
    null_pointer,
    struct_at_index: struct {
        index: u64,
        field_layouts: Layout.Slice,
        structure: Ident.Idx,
    },

    get_gag_id: struct {
        structure: Ident.Idx,
        union_layout: UnionLayout,
    },

    union_at_index: struct {
        structure: Ident.Idx,
        tag_id: TagIdIntType,
        union_layout: UnionLayout,
        index: u64,
    },

    get_element_pointer: struct {
        structure: Ident.Idx,
        union_layout: UnionLayout,
        indices: []u64,
    },

    array: struct {
        elem_layout: Layout.Idx,
        elems: ListLiteralElem.Slice,
    },

    empty_array,

    /// Returns a pointer to the given function.
    function_pointer: struct {
        module_ident: Ident.Idx,
    },

    alloca: struct {
        element_layout: Layout.Idx,
        initializer: ?Ident.Idx,
    },

    reset: struct {
        module_ident: Ident.Idx,
    },

    // Just like Reset, but does not recursively decrement the children.
    // Used in reuse analysis to replace a decref with a resetRef to avoid decrementing when the dec ref didn't.
    reset_ref: struct {
        module_ident: Ident.Idx,
    },

    /// todo
    pub const List = collections.SafeList(@This());
    /// todo
    pub const Idx = List.Idx;
    /// todo
    pub const Slice = List.Slice;
    /// todo
    pub const NonEmptySlice = List.NonEmptySlice;
};

/// todo
pub const ListLiteralElem = union(enum) {
    string_literal_id: []const u8,
    number: base.Literal.Num,
    ident: Ident.Idx,

    /// todo
    pub const List = collections.SafeList(@This());
    /// todo
    pub const Slice = List.Slice;
};

/// todo
pub const Call = struct {
    kind: Kind,
    arguments: IdentWithLayout.Slice,

    /// todo
    pub const Kind = union(enum) {
        by_name: struct {
            ident: Ident.Idx,
            ret_layout: Layout.Idx,
            arg_layouts: Layout.Slice,
        },
        by_pointer: struct {
            pointer: Ident.Idx,
            ret_layout: Layout.Idx,
            arg_layouts: []Layout.Idx,
        },
        // foreign: struct {
        //     foreign_symbol: usize, //ForeignSymbol.Idx,
        //     ret_layout: Layout.Idx,
        // },
        // low_level: struct {
        //     op: usize, //LowLevel,
        // },
        // TODO: presumably these should be removed in an earlier stage
        // HigherOrder(&'a HigherOrderLowLevel<'a>),
    };
};

/// todo
pub const Stmt = union(enum) {
    let: struct {
        ident: Ident.Idx,
        expr: Expr.Idx,
        layout: Expr.Idx,
        continuation: Stmt.Idx,
    },
    @"switch": struct {
        /// This *must* stand for an integer, because Switch potentially compiles to a jump table.
        cond_ident: Ident.Idx,
        // TODO: can we make this layout a number type?
        cond_layout: Layout.Idx,
        /// The u64 in the tuple will be compared directly to the condition Expr.
        /// If they are equal, this branch will be taken.
        branches: Branch,
        /// If no other branches pass, this default branch will be taken.
        default_branch: struct {
            info: Branch.Kind,
            stmt: Stmt.Idx,
        },
        /// Each branch must return a value of this type.
        ret_layout: Layout.Idx,
    },
    ret: Ident.Idx,
    /// a join point `join f <params> = <continuation> in remainder`
    join: struct {
        id: JoinPoint.Idx,
        parameters: IdentWithLayout.Slice,
        /// body of the join point
        /// what happens after _jumping to_ the join point
        body: Stmt.Idx,
        /// what happens after _defining_ the join point
        remainder: Stmt.Idx,
    },
    jump: struct {
        join_point: JoinPoint.Idx,
        idents: collections.SafeList(Ident.Idx).Slice,
    },
    crash: struct {
        message: Ident.Idx,
    },

    /// todo
    pub const List = collections.SafeList(@This());
    /// todo
    pub const Idx = List.Idx;
    /// todo
    pub const Slice = List.Slice;
    /// todo
    pub const NonEmptySlice = List.NonEmptySlice;
};

/// todo
pub const Branch = struct {
    discriminant: u64,
    kind: Kind,
    stmt: Stmt.Idx,

    /// in the block below, symbol `scrutinee` is assumed be be of shape `tag_id`
    pub const Kind = union(enum) {
        none,
        constructor: struct {
            scrutinee: Ident.Idx,
            layout: Layout.Idx,
            tag_id: TagIdIntType,
        },
        list: struct {
            scrutinee: Ident.Idx,
            len: u64,
        },
        unique: struct {
            scrutinee: Ident.Idx,
            unique: bool,
        },
    };
};

/// todo
pub const JoinPoint = struct {
    /// todo
    pub const Idx = Ident.Idx;
};
