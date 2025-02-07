const std = @import("std");
const base = @import("../../base.zig");
const cols = @import("../../collections.zig");
const problem = @import("../../problem.zig");
const types = @import("../../types.zig");

pub const IR = @This();

env: *base.ModuleEnv,
procs: std.AutoHashMap(base.Ident.Id, Procedure),
exprs: Expr.List,
layouts: Layout.List,
stmts: Stmt.List,
idents_with_layouts: IdentWithLayout.List,

pub fn init(env: *base.ModuleEnv, allocator: std.mem.Allocator) IR {
    return IR{
        .env = env,
        .procs = std.AutoHashMap(base.Ident.Id, Procedure).init(allocator),
        .exprs = Expr.List.init(allocator),
        .layouts = Layout.List.init(allocator),
        .stmts = Stmt.List.init(allocator),
        .idents_with_layouts = IdentWithLayout.List.init(allocator),
    };
}

pub fn deinit(self: *IR) void {
    self.procs.deinit();
    self.exprs.deinit();
    self.layouts.deinit();
    self.stmts.deinit();
    self.idents_with_layouts.deinit();
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

    pub const List = cols.SafeList(@This());
    pub const Idx = List.Idx;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const IdentWithLayout = struct {
    ident: base.Ident.Idx,
    layout: Layout.Idx,
};

pub const SymbolWithLayout = struct {
    module_ident: base.ModuleIdent,
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
        arguments: cols.SafeList(base.Ident.Idx).Slice,
    },
    Struct: cols.SafeList(base.Ident.Idx).NonEmptySlice,
    NullPointer,
    StructAtIndex: struct {
        index: u64,
        field_layouts: Layout.Slice,
        structure: base.Ident.Idx,
    },

    GetTagId: struct {
        structure: base.ModuleIdent,
        union_layout: UnionLayout,
    },

    UnionAtIndex: struct {
        structure: base.ModuleIdent,
        tag_id: TagIdIntType,
        union_layout: UnionLayout,
        index: u64,
    },

    GetElementPointer: struct {
        structure: base.ModuleIdent,
        union_layout: UnionLayout,
        indices: []u64,
    },

    Array: struct {
        elem_layout: Layout.Idx,
        elems: cols.SafeList(ListLiteralElem).Slice,
    },

    EmptyArray,

    /// Returns a pointer to the given function.
    FunctionPointer: struct {
        module_ident: base.ModuleIdent,
    },

    Alloca: struct {
        element_layout: Layout.Idx,
        initializer: ?base.ModuleIdent,
    },

    Reset: struct {
        module_ident: base.ModuleIdent,
    },

    // Just like Reset, but does not recursively decrement the children.
    // Used in reuse analysis to replace a decref with a resetRef to avoid decrementing when the dec ref didn't.
    ResetRef: struct {
        module_ident: base.ModuleIdent,
    },

    pub const List = cols.SafeList(@This());
    pub const Id = List.Id;
    pub const Slice = List.Slice;
    pub const NonEmptySlice = List.NonEmptySlice;
};

pub const ListLiteralElem = union(enum) {
    StringLiteralId: []const u8,
    Number: base.NumberLiteral,
    Ident: base.ModuleIdent,
};

pub const CallType = union(enum) {
    ByName: struct {
        ident: base.ModuleIdent,
        ret_layout: Layout.Idx,
        arg_layouts: Layout.Slice,
    },
    ByPointer: struct {
        pointer: base.ModuleIdent,
        ret_layout: Layout.Idx,
        arg_layouts: []Layout.Idx,
    },
    // Foreign: struct {
    //     foreign_symbol: usize, //ForeignSymbolId,
    //     ret_layout: LayoutId,
    // },
    // LowLevel: struct {
    //     op: usize, //LowLevel,
    // },
    // TODO: presumably these should be removed in an earlier stage
    // HigherOrder(&'a HigherOrderLowLevel<'a>),
};

pub const Call = struct {
    // TODO: consider putting `call_type` in a `Vec` in `IR`
    call_type: CallType,
    arguments: cols.SafeList(base.Ident.Idx).Slice,
};

pub const Stmt = union(enum) {
    Let: struct {
        ident: base.Ident.Idx,
        expr: Expr.Idx,
        layout: Expr.Idx,
        continuation: Stmt.Idx,
    },
    Switch: struct {
        /// This *must* stand for an integer, because Switch potentially compiles to a jump table.
        cond_ident: base.Ident.Idx,
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
    Ret: base.Ident.Idx,
    /// a join point `join f <params> = <continuation> in remainder`
    Join: struct {
        id: JoinPointId,
        parameters: cols.SafeList(Param).Slice,
        /// body of the join point
        /// what happens after _jumping to_ the join point
        body: Stmt.Idx,
        /// what happens after _defining_ the join point
        remainder: Stmt.Idx,
    },
    Jump: struct {
        join_point: JoinPointId,
        idents: cols.SafeList(base.IdentId).Slice,
    },
    Crash: struct {
        ident: base.Ident.Idx,
        tag: base.CrashOrigin,
    },

    pub const List = cols.SafeList(@This());
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
            scrutinee: base.ModuleIdent,
            layout: Layout.Idx,
            tag_id: TagIdIntType,
        },
        List: struct {
            scrutinee: base.ModuleIdent,
            len: u64,
        },
        Unique: struct {
            scrutinee: base.ModuleIdent,
            unique: bool,
        },
    };
};

pub const JoinPointId = base.Ident.Idx;

pub const Param = struct {
    ident: base.Ident.Idx,
    layout: Layout.Idx,
};
