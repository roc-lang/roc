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

pub fn deinit(self: *Self) void {
    self.procedures.deinit(self.env.gpa);
    self.constants.deinit(self.env.gpa);
    self.exprs.deinit(self.env.gpa);
    self.layouts.deinit(self.env.gpa);
    self.stmts.deinit(self.env.gpa);
    self.idents_with_layouts.deinit(self.env.gpa);
    self.list_literal_elems.deinit(self.env.gpa);
}

pub const Procedure = struct {
    arguments: IdentWithLayout.Slice,
    body: Stmt.Idx,
    return_layout: Layout.Idx,
};

// TODO: is this necessary?
pub const TagIdIntType = u16;

pub const Layout = union(enum) {
    primitive: types.Primitive,
    box: Layout.Idx,
    list: Layout.Idx,
    @"struct": Layout.NonEmptySlice,
    tag_union: Layout.NonEmptySlice,
    // probably necessary for returning empty structs, but would be good to remove this if that's not the case
    unit,

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const IdentWithLayout = struct {
    ident: Ident.Idx,
    layout: Layout.Idx,

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
};

pub const StmtWithLayout = struct {
    stmt: Stmt.Idx,
    layout: Layout.Idx,
};

// TODO: should these use `NonEmptySlice`s?
//
// Copied (and adapted) from:
// https://github.com/roc-lang/roc/blob/689c58f35e0a39ca59feba549f7fcf375562a7a6/crates/compiler/mono/src/layout.rs#L733
pub const UnionLayout = union(enum) {
    // TODO: 3 types:
    // - Unwrapped (1 variant converted to the inner type)
    // - Flat (compile normally)
    // - Recursive ("box" the recursion point)
};

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

    get_tag_id: struct {
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

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const ListLiteralElem = union(enum) {
    string_literal_id: []const u8,
    number: base.Literal.Num,
    ident: Ident.Idx,

    pub const List = collections.SafeList(@This());
    pub const Slice = List.Slice;
};

pub const Call = struct {
    kind: Kind,
    arguments: IdentWithLayout.Slice,

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
        //     foreign_symbol: usize, //ForeignSymbolId,
        //     ret_layout: LayoutId,
        // },
        // low_level: struct {
        //     op: usize, //LowLevel,
        // },
        // TODO: presumably these should be removed in an earlier stage
        // HigherOrder(&'a HigherOrderLowLevel<'a>),
    };
};

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
    ref_count: struct {
        symbol: Ident.Idx,
        change: ModifyRefCount,
    },
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

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

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

pub const ModifyRefCount = union(enum) {
    /// Increment a reference count
    inc: struct {
        target: Ident.Idx,
        count: u64,
    },

    /// Decrement a reference count
    dec: Ident.Idx,

    /// A DecRef is a non-recursive reference count decrement
    /// e.g. If we Dec a list of lists, then if the reference count of the outer list is one,
    /// a Dec will recursively decrement all elements, then free the memory of the outer list.
    /// A DecRef would just free the outer list.
    /// That is dangerous because you may not free the elements, but in our Zig builtins,
    /// sometimes we know we already dealt with the elements (e.g. by copying them all over
    /// to a new list) and so we can just do a DecRef, which is much cheaper in such a case.
    dec_ref: Ident.Idx,

    /// Unconditionally deallocate the memory. For tag union that do pointer tagging (store the tag
    /// id in the pointer) the backend has to clear the tag id!
    free: Ident.Idx,
};

pub const JoinPoint = struct {
    pub const Idx = Ident.Idx;
};
