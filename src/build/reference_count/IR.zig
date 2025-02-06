const std = @import("std");
const base = @import("../../base.zig");
const cols = @import("../../collections.zig");
const problem = @import("../../problem.zig");

pub const IR = struct {
    env: *base.ModuleEnv,
    procs: std.AutoHashMap(base.Ident.Idx, Procedure),
    exprs: cols.SafeList(Expr),
    layouts: cols.SafeList(Layout),
    stmts: cols.SafeList(Stmt),

    pub fn init(env: *base.ModuleEnv, allocator: std.mem.Allocator) IR {
        return IR{
            .env = env,
            .procs = std.AutoHashMap(base.Ident.Idx, Procedure).init(allocator),
            .exprs = cols.SafeList(Expr).init(allocator),
            .layouts = cols.SafeList(Layout).init(allocator),
            .stmts = cols.SafeList(Stmt).init(allocator),
        };
    }

    pub fn deinit(self: *IR) void {
        self.procs.deinit();
        self.exprs.deinit();
        self.layouts.deinit();
        self.stmts.deinit();
    }
};

pub const Procedure = struct {
    arguments: cols.SafeMultiList(IdentWithLayout).Slice,
    body: Stmt.Idx,
    return_layout: Layout.Idx,
};

// TODO: is this necessary?
pub const TagIdIntType = u16;

pub const Layout = union(enum) {
    Primitive: base.Primitive,
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

pub const ModuleIdentWithLayout = struct {
    ident: base.ModuleIdent,
    layout: Layout.Idx,
};

// TODO: should these use `NonEmptySlice`s?
//
// Copied (and adapted) from:
// https://github.com/roc-lang/roc/blob/689c58f35e0a39ca59feba549f7fcf375562a7a6/crates/compiler/mono/src/layout.rs#L733
pub const UnionLayout = union(enum) {
    // TODO
};

pub const Expr = union(enum) {
    Literal: base.Literal,

    // Functions
    Call: Call,

    Tag: struct {
        // TODO: should this be an index instead?
        tag_layout: UnionLayout,
        tag_id: TagIdIntType,
        arguments: cols.SafeList(base.IdentId).Slice,
    },
    Struct: cols.SafeList(base.IdentId).NonEmptySlice,
    NullPointer,
    StructAtIndex: struct {
        index: u64,
        field_layouts: []Layout.Idx,
        structure: base.IdentId,
    },

    GetTagId: struct {
        structure: usize, //Symbol,
        union_layout: usize, //UnionLayout,
    },

    UnionAtIndex: struct {
        structure: usize, //Symbol,
        tag_id: usize, //TagIdIntType,
        union_layout: usize, //UnionLayout,
        index: u64,
    },

    GetElementPointer: struct {
        structure: usize, //Symbol
        union_layout: usize, //UnionLayout,
        indices: []u64,
    },

    Array: struct {
        elem_layout: Layout.Idx,
        elems: cols.SafeList(ListLiteralElem).Slice,
    },

    EmptyArray,

    /// Returns a pointer to the given function.
    FunctionPointer: struct {
        symbol: base.ModuleIdent,
    },

    Alloca: struct {
        element_layout: Layout.Idx,
        initializer: ?base.ModuleIdent,
    },

    Reset: struct {
        symbol: base.ModuleIdent,
    },

    // Just like Reset, but does not recursively decrement the children.
    // Used in reuse analysis to replace a decref with a resetRef to avoid decrementing when the dec ref didn't.
    ResetRef: struct {
        symbol: base.ModuleIdent,
    },

    // CompilerTag: LowerIrProblem,

    pub const List = cols.SafeList(@This());
    pub const Idx = List.Id;
    pub const ExprSlice = List.Slice;
    pub const ExprNonEmptySlice = List.NonEmptySlice;
};

pub const ListLiteralElem = union(enum) {
    StringLiteralId: []const u8,
    Number: base.NumberLiteral,
    Symbol: base.ModuleIdent,
};

pub const CallType = union(enum) {
    ByName: struct {
        symbol: base.ModuleIdent,
        ret_layout: Layout.Idx,
        arg_layouts: []Layout.Idx,
    },
    ByPointer: struct {
        pointer: usize, //Symbol,
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
    arguments: cols.SafeList(base.IdentId).Slice,
};

pub const Stmt = union(enum) {
    Let: struct {
        ident: base.Ident.Idx,
        expr: Expr.Idx,
        layout: Layout.Idx,
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
    Ret: base.IdentId,
    RefCount: struct {
        symbol: base.ModuleIdent,
        change: ModifyRefCount,
    },
    /// a join point `join f <params> = <continuation> in remainder`
    Join: struct {
        id: JoinPoint.Idx,
        parameters: cols.SafeList(Param).Slice,
        /// body of the join point
        /// what happens after _jumping to_ the join point
        body: Stmt.Idx,
        /// what happens after _defining_ the join point
        remainder: Stmt.Idx,
    },
    Jump: struct {
        join_point: JoinPoint.Idx,
        idents: cols.SafeList(base.Ident.Idx).Slice,
    },
    Crash: struct {
        ident: base.Ident.Idx,
        tag: base.CrashOrigin,
    },

    // CompilerTag: LowerIrProblem,

    pub const Idx = cols.SafeList(Stmt).Idx;
    pub const Slice = cols.SafeList(Stmt).Slice;
    pub const NonEmptySlice = cols.SafeList(Stmt).NonEmptySlice;
};

pub const ModifyRefCount = union(enum) {
    /// Increment a reference count
    Inc: .{ base.ModuleIdent, u64 },

    /// Decrement a reference count
    Dec: base.ModuleIdent,

    /// A DecRef is a non-recursive reference count decrement
    /// e.g. If we Dec a list of lists, then if the reference count of the outer list is one,
    /// a Dec will recursively decrement all elements, then free the memory of the outer list.
    /// A DecRef would just free the outer list.
    /// That is dangerous because you may not free the elements, but in our Zig builtins,
    /// sometimes we know we already dealt with the elements (e.g. by copying them all over
    /// to a new list) and so we can just do a DecRef, which is much cheaper in such a case.
    DecRef: base.ModuleIdent,

    /// Unconditionally deallocate the memory. For tag union that do pointer tagging (store the tag
    /// id in the pointer) the backend has to clear the tag id!
    Free: base.ModuleIdent,
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

pub const JoinPoint = struct {
    pub const Idx = base.Ident.Idx;
};

pub const Param = struct {
    ident: base.Ident.Idx,
    layout: Layout.Idx,
};
