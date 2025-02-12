const std = @import("std");
const base = @import("../../base.zig");
const types = @import("../../types.zig");
const problem = @import("../../problem.zig");
const collections = @import("../../collections.zig");

const Ident = base.Ident;
const TagName = collections.TagName;
const FieldName = collections.FieldName;
const StringLiteral = collections.StringLiteral;

const Self = @This();

env: *base.ModuleEnv,
procedures: std.AutoHashMap(Ident.Idx, Procedure),
constants: std.AutoHashMap(Ident.Idx, StmtWithLayout),
exprs: Expr.List,
layouts: Layout.List,
stmts: Stmt.List,
idents_with_layouts: IdentWithLayout.List,
list_literal_elems: ListLiteralElem.List,

pub fn init(env: *base.ModuleEnv, allocator: std.mem.Allocator) Self {
    return Self{
        .env = env,
        .procedures = std.AutoHashMap(Ident.Idx, Procedure).init(allocator),
        .constants = std.AutoHashMap(Ident.Idx, StmtWithLayout).init(allocator),
        .exprs = Expr.List.init(allocator),
        .layouts = Layout.List.init(allocator),
        .stmts = Stmt.List.init(allocator),
        .idents_with_layouts = IdentWithLayout.List.init(allocator),
        .list_literal_elems = ListLiteralElem.List.init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.procedures.deinit();
    self.constants.deinit();
    self.exprs.deinit();
    self.layouts.deinit();
    self.stmts.deinit();
    self.idents_with_layouts.deinit();
    self.list_literal_elems.deinit();
}

pub const Procedure = struct {
    arguments: IdentWithLayout.Slice,
    body: Stmt.Idx,
    return_layout: Layout.Idx,
};

// TODO: is this necessary?
pub const TagIdIntType = u16;

pub const Layout = union(enum) {
    Primitive: types.Primitive,
    Box: Layout.Idx,
    List: Layout.Idx,
    Struct: Layout.NonEmptySlice,
    TagUnion: Layout.NonEmptySlice,
    // probably necessary for returning empty structs, but would be good to remove this if that's not the case
    Unit,

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
    Literal: base.Literal,

    // Functions
    Call: Call,

    Tag: struct {
        // TODO: should this be an index instead?
        tag_layout: UnionLayout,
        tag_id: TagIdIntType,
        arguments: collections.SafeList(Ident.Idx).Slice,
    },
    Struct: collections.SafeList(Ident.Idx).NonEmptySlice,
    NullPointer,
    StructAtIndex: struct {
        index: u64,
        field_layouts: Layout.Slice,
        structure: Ident.Idx,
    },

    GetTagId: struct {
        structure: Ident.Idx,
        union_layout: UnionLayout,
    },

    UnionAtIndex: struct {
        structure: Ident.Idx,
        tag_id: TagIdIntType,
        union_layout: UnionLayout,
        index: u64,
    },

    GetElementPointer: struct {
        structure: Ident.Idx,
        union_layout: UnionLayout,
        indices: []u64,
    },

    Array: struct {
        elem_layout: Layout.Idx,
        elems: ListLiteralElem.Slice,
    },

    EmptyArray,

    /// Returns a pointer to the given function.
    FunctionPointer: struct {
        module_ident: Ident.Idx,
    },

    Alloca: struct {
        element_layout: Layout.Idx,
        initializer: ?Ident.Idx,
    },

    Reset: struct {
        module_ident: Ident.Idx,
    },

    // Just like Reset, but does not recursively decrement the children.
    // Used in reuse analysis to replace a decref with a resetRef to avoid decrementing when the dec ref didn't.
    ResetRef: struct {
        module_ident: Ident.Idx,
    },

    pub const List = collections.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const ListLiteralElem = union(enum) {
    StringLiteralId: []const u8,
    Number: base.Literal.Num,
    Ident: Ident.Idx,

    pub const List = collections.SafeList(@This());
    pub const Slice = List.Slice;
};

pub const Call = struct {
    kind: Kind,
    arguments: IdentWithLayout.Slice,

    pub const Kind = union(enum) {
        ByName: struct {
            ident: Ident.Idx,
            ret_layout: Layout.Idx,
            arg_layouts: Layout.Slice,
        },
        ByPointer: struct {
            pointer: Ident.Idx,
            ret_layout: Layout.Idx,
            arg_layouts: []Layout.Idx,
        },
        // Foreign: struct {
        //     foreign_symbol: usize, //ForeignSymbol.Idx,
        //     ret_layout: Layout.Idx,
        // },
        // LowLevel: struct {
        //     op: usize, //LowLevel,
        // },
        // TODO: presumably these should be removed in an earlier stage
        // HigherOrder(&'a HigherOrderLowLevel<'a>),
    };
};

pub const Stmt = union(enum) {
    Let: struct {
        ident: Ident.Idx,
        expr: Expr.Idx,
        layout: Expr.Idx,
        continuation: Stmt.Idx,
    },
    Switch: struct {
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
    Ret: Ident.Idx,
    /// a join point `join f <params> = <continuation> in remainder`
    Join: struct {
        id: JoinPoint.Idx,
        parameters: IdentWithLayout.Slice,
        /// body of the join point
        /// what happens after _jumping to_ the join point
        body: Stmt.Idx,
        /// what happens after _defining_ the join point
        remainder: Stmt.Idx,
    },
    Jump: struct {
        join_point: JoinPoint.Idx,
        idents: collections.SafeList(Ident.Idx).Slice,
    },
    Crash: struct {
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
        None,
        Constructor: struct {
            scrutinee: Ident.Idx,
            layout: Layout.Idx,
            tag_id: TagIdIntType,
        },
        List: struct {
            scrutinee: Ident.Idx,
            len: u64,
        },
        Unique: struct {
            scrutinee: Ident.Idx,
            unique: bool,
        },
    };
};

pub const JoinPoint = struct {
    pub const Idx = Ident.Idx;
};
