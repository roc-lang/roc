//! This module implements Hindley-Milner style type unification with extensions for:
//! * flex/rigid variables
//! * type aliases
//! * tuples
//! * builtins (List, Str, Box, etc)
//! * functions
//! * extensible row-based record types
//! * extensible tag unions
//! * custom, nominal types
//! * numbers (polymorphic & compacted)
//! * effect and purity tracking
//!
//! The primary entrypoint is `unify`, which takes two type variables (`Var`) and attempts
//! to unify their structure in the given `Store`, using a mutable `Scratch` context
//! to hold intermediate buffers and allocations.
//!
//! The `Scratch` struct is designed to be reused across many unification calls.
//! It owns internal scratch buffers for record fields, shared fields, and fresh variables.
//!
//! Each call to `unify` will reset the scratch buffer. If the unification succeeds,
//! the caller can access `scratch.fresh_vars` to retrieve all type variables created
//! during that unification pass. These fresh variables are useful for later type
//! generalization, let-binding, or monomorphization.
//!
//! NOTE: Subsequent calls to `unify` will reset `fresh_vars`. It is up to the caller
//! to use/store them if necessary.
//!
//! ### Example
//!
//! ```zig
//! const result = unify(&types_store, &scratch, a_var, b_var);
//! if (result == .ok) {
//!     for (scratch.fresh_vars.items) |v| {
//!         // handle fresh type variable `v`
//!     }
//! }
//! ```
//!
//! After use, the scratch buffer should either be deinited or reused in
//! subsequent unification runs.

const std = @import("std");

const base = @import("../../base.zig");
const collections = @import("../../collections.zig");
const types = @import("../../types/types.zig");
const store = @import("../../types/store.zig");
const occurs = @import("./occurs.zig");

const Region = base.Region;
const Ident = base.Ident;

const exitOnOutOfMemory = collections.utils.exitOnOom;
const SmallStringInterner = collections.SmallStringInterner;

const Slot = store.Slot;
const ResolvedVarDesc = store.ResolvedVarDesc;
const ResolvedVarDescs = store.ResolvedVarDescs;

const TypeIdent = types.TypeIdent;
const Var = types.Var;
const Desc = types.Descriptor;
const Rank = types.Rank;
const Mark = types.Mark;
const Content = types.Content;
const Alias = types.Alias;
const CustomType = types.CustomType;
const FlatType = types.FlatType;
const Builtin = types.Builtin;
const Tuple = types.Tuple;
const Num = types.Num;
const NumCompact = types.Num.Compact;
const Func = types.Func;
const Record = types.Record;
const RecordField = types.RecordField;
const TwoRecordFields = types.TwoRecordFields;
const TagUnion = types.TagUnion;
const Tag = types.Tag;
const TwoTags = types.TwoTags;

const VarSafeList = Var.SafeList;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const RecordFieldSafeList = RecordField.SafeList;
const TwoRecordFieldsSafeMultiList = TwoRecordFields.SafeMultiList;
const TwoRecordFieldsSafeList = TwoRecordFields.SafeList;
const TagSafeList = Tag.SafeList;
const TagSafeMultiList = Tag.SafeMultiList;
const TwoTagsSafeList = TwoTags.SafeList;

/// Unify two type variables
///
/// This function
/// * Resolves type variables & compresses paths
/// * Compares variable contents for equality
/// * Merges unified variables so 1 is "root" and the other is "redirect"
pub fn unify(
    module_env: *const base.ModuleEnv,
    types_store: *store.Store,
    scratch: *Scratch,
    occurs_scratch: *occurs.Scratch,
    a: Var,
    b: Var,
) Result {
    // First reset the scratch store
    scratch.reset();

    // Unify
    var unifier = Unifier.init(module_env, types_store, scratch, occurs_scratch);
    unifier.unifyGuarded(a, b) catch |err| {
        types_store.union_(a, b, .{
            .content = .err,
            .rank = Rank.generalized,
            .mark = Mark.none,
        });
        switch (err) {
            error.TypeMismatch => {
                return .type_mismatch;
            },
            error.UnifyErr => {
                // Unify can error in the following ways:
                //
                // 1. Encountering illegal recursion (infinite or anonymous)
                // 2. Encountering an invalid polymorphic number type
                //
                // In these cases, before throwing, we set error state in
                // `scratch.occurs_err`. This is necessary becuaes you cannot
                // associated an error payload when throwing.
                //
                // If we threw but there is no error data, it is a bug
                if (scratch.err) |unify_err| {
                    switch (unify_err.type_) {
                        .recursion_anonymous => {
                            return Result{ .anonmyous_recursion = unify_err.side };
                        },
                        .recursion_infinite => {
                            return Result{ .infinite_recursion = unify_err.side };
                        },
                        .invalid_number_type => {
                            return Result{ .invalid_number_type = unify_err.side };
                        },
                    }
                } else {
                    return .bug;
                }
            },
        }
    };
    return .ok;
}

/// The result of unification
pub const Result = union(enum) {
    const Self = @This();

    ok,
    type_mismatch,
    invalid_number_type: Side,
    infinite_recursion: Side,
    anonmyous_recursion: Side,
    bug,

    /// The error types that occurred during unification
    /// TODO: Make these full error types
    pub const Err = struct { a: Var, b: Var };

    pub fn isOk(self: Self) bool {
        return self == .ok;
    }
};

/// Used to indicate which side unify argument was problematics in errors
pub const Side = enum { a, b };

/// A temporary unification context used to unify two type variables within a `Store`.
///
/// `Unifier` is created per unification call and:
/// * Resolves and compresses type variables
/// * Applies unification logic across all supported `Content` variants
/// * Allocates new variables into the store (via `fresh`)
/// * Writes unified structure back into the store (via `merge`)
///
/// It works in tandem with `Scratch`, which owns reusable memory buffers
/// for intermediate field lists, shared record partitions, and fresh variables.
///
/// `Unifier` supports:
/// * flexible and rigid type variables
/// * type aliases
/// * extensible records with row polymorphism
/// * basic support for function, tuple, and number types
///
/// Callers are not expected to construct `Unifier`. Instead  call `unify(...)`.
const Unifier = struct {
    const Self = @This();

    module_env: *const base.ModuleEnv,
    types_store: *store.Store,
    scratch: *Scratch,
    occurs_scratch: *occurs.Scratch,
    depth: u8,
    skip_depth_check: bool,

    /// Init a unifier
    /// Caller owns the memory of the provided values
    fn init(
        module_env: *const base.ModuleEnv,
        types_store: *store.Store,
        scratch: *Scratch,
        occurs_scratch: *occurs.Scratch,
    ) Self {
        return .{
            .module_env = module_env,
            .types_store = types_store,
            .scratch = scratch,
            .occurs_scratch = occurs_scratch,
            .depth = 0,
            .skip_depth_check = false,
        };
    }

    // merge

    /// Link the variables & updated the content in the type_store
    /// In the old compiler, this function was called "merge"
    fn merge(self: *Self, vars: *const ResolvedVarDescs, new_content: Content) void {
        self.types_store.union_(vars.a.var_, vars.b.var_, .{
            .content = new_content,
            .rank = Rank.min(vars.a.desc.rank, vars.b.desc.rank),
            .mark = Mark.none,
        });
    }

    /// Create a new type variable *in this pool*
    fn fresh(self: *Self, vars: *const ResolvedVarDescs, new_content: Content) Var {
        const var_ = self.types_store.register(.{
            .content = new_content,
            .rank = Rank.min(vars.a.desc.rank, vars.b.desc.rank),
            .mark = Mark.none,
        });
        _ = self.scratch.fresh_vars.append(self.scratch.gpa, var_);
        return var_;
    }

    // unification

    /// Error thrown during unification when there's a type mismatch
    const Error = error{
        TypeMismatch,
        UnifyErr,
    };

    /// Unify checking for equivalence
    fn unifyGuarded(self: *Self, a_var: Var, b_var: Var) Error!void {
        switch (self.types_store.checkVarsEquiv(a_var, b_var)) {
            .equiv => {
                // this means that the vars point to the same exact type
                // descriptor, so nothing needs to happen
                return;
            },
            .not_equiv => |vars| {
                if (self.skip_depth_check or self.depth < 8) {
                    self.depth += 1;
                    const result = self.unifyVars(&vars);
                    self.depth -= 1;
                    _ = try result;
                } else {
                    try self.checkRecursive(&vars);

                    self.skip_depth_check = true;
                    try self.unifyVars(&vars);
                    self.skip_depth_check = false;
                }
            },
        }
    }

    /// Unify two vars
    /// Internal entry point for unification logic. Use `unifyGuarded` to ensure
    /// proper depth tracking and occurs checking.
    fn unifyVars(self: *Self, vars: *const ResolvedVarDescs) Error!void {
        switch (vars.a.desc.content) {
            .flex_var => |mb_a_ident| {
                self.unifyFlex(vars, mb_a_ident, vars.b.desc.content);
            },
            .rigid_var => |_| {
                try self.unifyRigid(vars, vars.b.desc.content);
            },
            .alias => |a_alias| {
                try self.unifyAlias(vars, a_alias, vars.b.desc.content);
            },
            .effectful => {
                try self.unifyEffectful(vars, vars.b.desc.content);
            },
            .pure => {
                try self.unifyPure(vars, vars.b.desc.content);
            },
            .structure => |a_flat_type| {
                try self.unifyStructure(vars, a_flat_type, vars.b.desc.content);
            },
            .err => return error.TypeMismatch,
        }
    }

    /// Run a full occurs check on each variable, erroring if it is infinite
    /// or anonymous recursion
    ///
    /// This function is called when unify has recursed a sufficient depth that
    /// a recursive type seems likely.
    fn checkRecursive(self: *Self, vars: *const ResolvedVarDescs) Error!void {
        const a_occurs = occurs.occurs(self.types_store, self.occurs_scratch, vars.a.var_);
        switch (a_occurs) {
            .not_recursive => {},
            .recursive_nominal => {},
            .recursive_anonymous => {
                return self.setUnifyErrAndThrow(UnifyErrCtx{ .type_ = .recursion_anonymous, .side = .a });
            },
            .infinite => {
                return self.setUnifyErrAndThrow(UnifyErrCtx{ .type_ = .recursion_infinite, .side = .a });
            },
        }

        const b_occurs = occurs.occurs(self.types_store, self.occurs_scratch, vars.b.var_);
        switch (b_occurs) {
            .not_recursive => {},
            .recursive_nominal => {},
            .recursive_anonymous => {
                return self.setUnifyErrAndThrow(UnifyErrCtx{ .type_ = .recursion_anonymous, .side = .b });
            },
            .infinite => {
                return self.setUnifyErrAndThrow(UnifyErrCtx{ .type_ = .recursion_infinite, .side = .b });
            },
        }
    }

    // Unify flex //

    /// Unify when `a` was a flex
    fn unifyFlex(self: *Self, vars: *const ResolvedVarDescs, mb_a_ident: ?Ident.Idx, b_content: Content) void {
        switch (b_content) {
            .flex_var => |mb_b_ident| {
                if (mb_a_ident) |a_ident| {
                    self.merge(vars, Content{ .flex_var = a_ident });
                } else {
                    self.merge(vars, Content{ .flex_var = mb_b_ident });
                }
            },
            .rigid_var => self.merge(vars, b_content),
            .alias => self.merge(vars, b_content),
            .effectful => self.merge(vars, b_content),
            .pure => self.merge(vars, b_content),
            .structure => self.merge(vars, b_content),
            .err => self.merge(vars, .err),
        }
    }

    // Unify rigid //

    /// Unify when `a` was a rigid
    fn unifyRigid(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) Error!void {
        switch (b_content) {
            .flex_var => self.merge(vars, vars.a.desc.content),
            .rigid_var => return error.TypeMismatch,
            .alias => return error.TypeMismatch,
            .effectful => return error.TypeMismatch,
            .pure => return error.TypeMismatch,
            .structure => return error.TypeMismatch,
            .err => self.merge(vars, .err),
        }
    }

    // Unify alias //

    /// Unify when `a` was a alias
    fn unifyAlias(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_content: Content) Error!void {
        switch (b_content) {
            .flex_var => |_| {
                self.merge(vars, Content{ .alias = a_alias });
            },
            .rigid_var => |_| {
                try self.unifyGuarded(a_alias.backing_var, vars.b.var_);
            },
            .alias => |b_alias| {
                if (TypeIdent.eql(&self.module_env.idents, a_alias.ident, b_alias.ident)) {
                    try self.unifyTwoAliases(vars, a_alias, b_alias);
                } else {
                    try self.unifyGuarded(a_alias.backing_var, b_alias.backing_var);
                }
            },
            .effectful => return error.TypeMismatch,
            .pure => return error.TypeMismatch,
            .structure => try self.unifyGuarded(a_alias.backing_var, vars.b.var_),
            .err => self.merge(vars, .err),
        }
    }

    /// Unify two aliases
    ///
    /// This function assumes the caller has already checked that the alias names match
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    ///
    /// NOTE: the rust version of this function `unify_two_aliases` is *significantly* more
    /// complicated than the version here
    fn unifyTwoAliases(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_alias: Alias) Error!void {
        if (a_alias.args.len() != b_alias.args.len()) {
            return error.TypeMismatch;
        }

        const a_args = self.types_store.getAliasArgsSlice(a_alias.args);
        const b_args = self.types_store.getAliasArgsSlice(b_alias.args);
        for (a_args, b_args) |a_arg, b_arg| {
            try self.unifyGuarded(a_arg, b_arg);
        }

        // Rust compiler comment:
        // Don't report real_var mismatches, because they must always be surfaced higher, from the argument types.
        self.unifyGuarded(a_alias.backing_var, b_alias.backing_var) catch {};

        self.merge(vars, vars.b.desc.content);
    }

    // Unify effectful //

    /// Unify when `a` was a effectful
    fn unifyEffectful(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) Error!void {
        switch (b_content) {
            .flex_var => self.merge(vars, .effectful),
            .effectful => self.merge(vars, .effectful),
            .err => self.merge(vars, .err),
            else => return error.TypeMismatch,
        }
    }

    // Unify pure //

    /// Unify when `a` was a pure
    fn unifyPure(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) Error!void {
        switch (b_content) {
            .flex_var => self.merge(vars, .pure),
            .pure => self.merge(vars, .pure),
            .effectful => self.merge(vars, .effectful),
            .err => self.merge(vars, .err),
            else => return error.TypeMismatch,
        }
    }

    // Unify structure //

    /// Unify when `a` is a structure type
    fn unifyStructure(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_flat_type: FlatType,
        b_content: Content,
    ) Error!void {
        switch (b_content) {
            .flex_var => |_| {
                self.merge(vars, Content{ .structure = a_flat_type });
            },
            .rigid_var => return error.TypeMismatch,
            .alias => |alias| {
                try self.unifyGuarded(vars.a.var_, alias.backing_var);
            },
            .effectful => return error.TypeMismatch,
            .pure => return error.TypeMismatch,
            .structure => |b_flat_type| {
                try self.unifyFlatType(vars, a_flat_type, b_flat_type);
            },
            .err => self.merge(vars, .err),
        }
    }

    /// Unify when `a` is a structure type
    fn unifyFlatType(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_flat_type: FlatType,
        b_flat_type: FlatType,
    ) Error!void {
        switch (a_flat_type) {
            .str => {
                switch (b_flat_type) {
                    .str => {
                        self.merge(vars, vars.b.desc.content);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .box => |a_var| {
                switch (b_flat_type) {
                    .box => |b_var| {
                        try self.unifyGuarded(a_var, b_var);
                        self.merge(vars, vars.b.desc.content);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .list => |a_var| {
                switch (b_flat_type) {
                    .list => |b_var| {
                        try self.unifyGuarded(a_var, b_var);
                        self.merge(vars, vars.b.desc.content);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .tuple => |a_tuple| {
                switch (b_flat_type) {
                    .tuple => |b_tuple| {
                        try self.unifyTuple(vars, a_tuple, b_tuple);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .num => |a_num| {
                switch (b_flat_type) {
                    .num => |b_num| {
                        try self.unifyNum(vars, a_num, b_num);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .custom_type => |a_type| {
                switch (b_flat_type) {
                    .custom_type => |b_type| {
                        try self.unifyCustomType(vars, a_type, b_type);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .func => |a_func| {
                switch (b_flat_type) {
                    .func => |b_func| {
                        try self.unifyFunc(vars, a_func, b_func);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .record => |a_record| {
                switch (b_flat_type) {
                    .empty_record => {
                        if (a_record.fields.len() == 0) {
                            try self.unifyGuarded(a_record.ext, vars.b.var_);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .record => |b_record| {
                        try self.unifyTwoRecords(vars, a_record, b_record);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .empty_record => {
                switch (b_flat_type) {
                    .empty_record => {
                        self.merge(vars, Content{ .structure = .empty_record });
                    },
                    .record => |b_record| {
                        if (b_record.fields.len() == 0) {
                            try self.unifyGuarded(vars.a.var_, b_record.ext);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
            .tag_union => |a_tag_union| {
                switch (b_flat_type) {
                    .empty_tag_union => {
                        if (a_tag_union.tags.len() == 0) {
                            try self.unifyGuarded(a_tag_union.ext, vars.b.var_);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .tag_union => |b_tag_union| {
                        try self.unifyTwoTagUnions(vars, a_tag_union, b_tag_union);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .empty_tag_union => {
                switch (b_flat_type) {
                    .empty_tag_union => {
                        self.merge(vars, Content{ .structure = .empty_tag_union });
                    },
                    .tag_union => |b_tag_union| {
                        if (b_tag_union.tags.len() == 0) {
                            try self.unifyGuarded(vars.a.var_, b_tag_union.ext);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
        }
    }

    /// unify tuples
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    fn unifyTuple(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_tuple: Tuple,
        b_tuple: Tuple,
    ) Error!void {
        if (a_tuple.elems.len() != b_tuple.elems.len()) {
            return error.TypeMismatch;
        }

        const a_elems = self.types_store.getTupleElemsSlice(a_tuple.elems);
        const b_elems = self.types_store.getTupleElemsSlice(b_tuple.elems);
        for (a_elems, b_elems) |a_elem, b_elem| {
            try self.unifyGuarded(a_elem, b_elem);
        }

        self.merge(vars, vars.b.desc.content);
    }

    fn unifyNum(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_num: Num,
        b_num: Num,
    ) Error!void {
        switch (a_num) {
            .num_poly => |a_var| {
                switch (b_num) {
                    .num_poly => |b_var| {
                        // TODO: Error if sub vars are not numeric
                        try self.unifyGuarded(a_var, b_var);
                        self.merge(vars, vars.b.desc.content);
                    },
                    .num_compact => |b_num_compact| {
                        try self.unifyPolyAndCompactNums(vars, a_var, b_num_compact);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .int_poly => |a_var| {
                switch (b_num) {
                    .int_poly => |b_var| {
                        // TODO: Error if sub vars are not numeric
                        try self.unifyGuarded(a_var, b_var);
                        self.merge(vars, vars.b.desc.content);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .frac_poly => |a_var| {
                switch (b_num) {
                    .frac_poly => |b_var| {
                        // TODO: Error if sub vars are not numeric
                        try self.unifyGuarded(a_var, b_var);
                        self.merge(vars, vars.b.desc.content);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .int_precision => |a_prec| {
                switch (b_num) {
                    .int_precision => |b_prec| {
                        if (a_prec == b_prec) {
                            self.merge(vars, vars.b.desc.content);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
            .frac_precision => |a_prec| {
                switch (b_num) {
                    .frac_precision => |b_prec| {
                        if (a_prec == b_prec) {
                            self.merge(vars, vars.b.desc.content);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
            .num_compact => |a_num_compact| {
                switch (b_num) {
                    .num_compact => |b_num_compact| {
                        try self.unifyTwoCompactNums(vars, a_num_compact, b_num_compact);
                    },
                    .num_poly => |b_var| {
                        try self.unifyCompactAndPolyNums(vars, a_num_compact, b_var);
                    },
                    else => return error.TypeMismatch,
                }
            },
        }
    }

    /// unify compact numbers
    fn unifyTwoCompactNums(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_num: NumCompact,
        b_num: NumCompact,
    ) Error!void {
        switch (a_num) {
            .int => |a_int| {
                switch (b_num) {
                    .int => |b_int| if (a_int == b_int) {
                        self.merge(vars, vars.b.desc.content);
                    } else {
                        return error.TypeMismatch;
                    },
                    else => return error.TypeMismatch,
                }
            },
            .frac => |a_frac| {
                switch (b_num) {
                    .frac => |b_frac| if (a_frac == b_frac) {
                        self.merge(vars, vars.b.desc.content);
                    } else {
                        return error.TypeMismatch;
                    },
                    else => return error.TypeMismatch,
                }
            },
        }
    }

    /// Unify when a is compact and b is polymorphic
    /// Since `a` is compact, we must merge with it
    fn unifyCompactAndPolyNums(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_num: NumCompact,
        b_num_var: Var,
    ) Error!void {
        const b_num_resolved = self.resolvePolyNum(b_num_var);
        switch (a_num) {
            .int => |a_int| {
                switch (b_num_resolved) {
                    .flex_resolved => self.merge(vars, vars.a.desc.content),
                    .int_resolved => |b_int| if (@intFromEnum(a_int) == @intFromEnum(b_int)) {
                        self.merge(vars, vars.a.desc.content);
                    } else {
                        return error.TypeMismatch;
                    },
                    .frac_resolved => return error.TypeMismatch,
                    .err => |_| {
                        return self.setUnifyErrAndThrow(.{ .type_ = .invalid_number_type, .side = .b });
                    },
                }
            },
            .frac => |a_frac| {
                switch (b_num_resolved) {
                    .flex_resolved => self.merge(vars, vars.a.desc.content),
                    .frac_resolved => |b_frac| if (@intFromEnum(a_frac) == @intFromEnum(b_frac)) {
                        self.merge(vars, vars.a.desc.content);
                    } else {
                        return error.TypeMismatch;
                    },
                    .int_resolved => return error.TypeMismatch,
                    .err => |_| {
                        return self.setUnifyErrAndThrow(.{ .type_ = .invalid_number_type, .side = .b });
                    },
                }
            },
        }
    }

    /// Unify when a is polymorphic and b is compact
    /// Since `b` is compact, we must merge with it
    fn unifyPolyAndCompactNums(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_num_var: Var,
        b_num: NumCompact,
    ) Error!void {
        const a_num_resolved = self.resolvePolyNum(a_num_var);
        switch (a_num_resolved) {
            .flex_resolved => self.merge(vars, vars.b.desc.content),
            .int_resolved => |a_int| switch (b_num) {
                .int => |b_int| if (@intFromEnum(a_int) == @intFromEnum(b_int)) {
                    self.merge(vars, vars.b.desc.content);
                } else {
                    return error.TypeMismatch;
                },
                .frac => return error.TypeMismatch,
            },
            .frac_resolved => |a_frac| switch (b_num) {
                .frac => |b_frac| if (@intFromEnum(a_frac) == @intFromEnum(b_frac)) {
                    self.merge(vars, vars.b.desc.content);
                } else {
                    return error.TypeMismatch;
                },
                .int => return error.TypeMismatch,
            },
            .err => |_| {
                return self.setUnifyErrAndThrow(.{ .type_ = .invalid_number_type, .side = .a });
            },
        }
    }

    /// The result of attempting to resolve a polymorphic number
    const ResolvedNum = union(enum) {
        flex_resolved,
        int_resolved: Num.Int.Precision,
        frac_resolved: Num.Frac.Precision,
        err: Var,
    };

    /// Attempts to resolve a polymorphic number variable to a concrete precision.
    ///
    /// This function recursively follows the structure of a number type,
    /// unwrapping any intermediate `.num_poly`, `.int_poly`, or `.frac_poly`
    /// variants until it reaches a concrete representation:
    /// either `.int_precision` or `.frac_precision`.
    ///
    /// For example:
    ///   Given a type like `Num(Int(U8))`, this function returns `.int_resolved(.u8)`.
    ///
    /// If resolution reaches a `.flex_var`, it returns `.flex_resolved`,
    /// indicating the number is still unspecialized.
    ///
    /// If the chain ends in an invalid structure (e.g. `Num(Str)`),
    /// it returns `.err`, along with the offending variable.
    /// TODO: Do we want the chain of offending variables on error?
    ///
    /// Note that this function will work on the "tail" of a polymorphic number.
    /// That is, if you pass in `Frac(Dec)` (without the outer `Num`), this
    /// function will still resolve successfully.
    fn resolvePolyNum(
        self: *Self,
        initial_num_var: Var,
    ) ResolvedNum {
        var num_var = initial_num_var;
        while (true) {
            const resolved = self.types_store.resolveVar(num_var);
            switch (resolved.desc.content) {
                .flex_var => return .flex_resolved,
                .structure => |flat_type| {
                    switch (flat_type) {
                        .num => |num| switch (num) {
                            .num_poly => |var_| {
                                num_var = var_;
                            },
                            .int_poly => |var_| {
                                num_var = var_;
                            },
                            .frac_poly => |var_| {
                                num_var = var_;
                            },
                            .int_precision => |prec| {
                                return .{ .int_resolved = prec };
                            },
                            .frac_precision => |prec| {
                                return .{ .frac_resolved = prec };
                            },
                            .num_compact => return .{ .err = num_var },
                        },
                        else => return .{ .err = num_var },
                    }
                },
                else => return .{ .err = num_var },
            }
        }
    }

    // Unify custom type //

    /// Unify when `a` was a custom type
    fn unifyCustomType(self: *Self, vars: *const ResolvedVarDescs, a_type: CustomType, b_type: CustomType) Error!void {
        if (!TypeIdent.eql(&self.module_env.idents, a_type.ident, b_type.ident)) {
            return error.TypeMismatch;
        }

        const a_args = self.types_store.getCustomTypeArgsSlice(a_type.args);
        const b_args = self.types_store.getCustomTypeArgsSlice(b_type.args);
        for (a_args, b_args) |a_arg, b_arg| {
            try self.unifyGuarded(a_arg, b_arg);
        }

        // Now that we've confirm the nominal types are the same and have thes
        // same args, we proceed to unify the backing vars
        // try self.unifyGuarded(a_type.backing_var, b_type.backing_var);

        self.merge(vars, vars.b.desc.content);
    }

    /// unify func
    ///
    /// this checks:
    /// * that the arg arities are the same
    /// * that parallel args unify
    /// * that ret unifies
    fn unifyFunc(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_func: Func,
        b_func: Func,
    ) Error!void {
        if (a_func.args.len() != b_func.args.len()) {
            return error.TypeMismatch;
        }

        const a_args = self.types_store.getFuncArgsSlice(a_func.args);
        const b_args = self.types_store.getFuncArgsSlice(b_func.args);
        for (a_args, b_args) |a_arg, b_arg| {
            try self.unifyGuarded(a_arg, b_arg);
        }

        try self.unifyGuarded(a_func.ret, b_func.ret);
        try self.unifyGuarded(a_func.eff, b_func.eff);

        self.merge(vars, vars.b.desc.content);
    }

    /// Unify two extensible records.
    ///
    /// This function implements Elm-style record unification.
    ///
    /// Each record consists of:
    /// - a fixed set of known fields (`fields`)
    /// - an extensible tail variable (`ext`) that may point to additional unknown fields
    ///
    /// Given two records `a` and `b`, we:
    ///   1. Collect all known fields by unwrapping their `ext` chains.
    ///   2. Partition the field sets into:
    ///      - `in_both`: shared fields present in both `a` and `b`
    ///      - `only_in_a`: fields only present in `a`
    ///      - `only_in_b`: fields only present in `b`
    ///   3. Determine the relationship between the two records based on these partitions.
    ///
    /// Four cases follow:
    ///
    /// ---
    ///
    /// **Case 1: Exactly the Same Fields**
    ///
    /// a = { x, y }ext_a
    /// b = { x, y }ext_b
    ///
    /// - All fields are shared
    /// - We unify `ext_a ~ ext_b`
    /// - Then unify each shared field pair
    ///
    /// ---
    ///
    /// **Case 2: `a` Extends `b`**
    ///
    /// a = { x, y, z }ext_a
    /// b = { x, y }ext_b
    ///
    /// - `a` has additional fields not in `b`
    /// - We generate a new var `only_in_a_var = { z }ext_a`
    /// - Unify `only_in_a_var ~ ext_b`
    /// - Then unify shared fields
    ///
    /// ---
    ///
    /// **Case 3: `b` Extends `a`**
    ///
    /// a = { x, y }ext_a
    /// b = { x, y, z }ext_b
    ///
    /// - Same as Case 2, but reversed
    /// - `b` has additional fields not in `a`
    /// - We generate a new var `only_in_b_var = { z }ext_b`
    /// - Unify `ext_a ~ only_in_b_var`
    /// - Then unify shared fields
    ///
    /// ---
    ///
    /// **Case 4: Both Extend Each Other**
    ///
    /// a = { x, y, z }ext_a
    /// b = { x, y, w }ext_b
    ///
    /// - Each has unique fields the other lacks
    /// - Generate:
    ///     - shared_ext = fresh flex_var
    ///     - only_in_a_var = { z }shared_ext
    ///     - only_in_b_var = { w }shared_ext
    /// - Unify:
    ///     - `ext_a ~ only_in_b_var`
    ///     - `only_in_a_var ~ ext_b`
    /// - Then unify shared fields into `{ x, y }shared_ext`
    ///
    /// ---
    ///
    /// All field unification is done using `unifySharedFields`, and new variables are created using `fresh`.
    ///
    /// This function does not attempt to deduplicate fields or reorder them — callers are responsible
    /// for providing consistent field names.
    fn unifyTwoRecords(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_record: Record,
        b_record: Record,
    ) Error!void {

        // First, unwrap all fields for record a, panicaing with various non-recoverable error
        // These pancis will likely be changed/removed in the future
        const a_gathered_fields = try self.gatherRecordFields(a_record);
        const b_gathered_fields = try self.gatherRecordFields(b_record);

        // Then partition the fields
        const partitioned = partitionFields(
            &self.module_env.idents,
            self.scratch,
            a_gathered_fields.range,
            b_gathered_fields.range,
        );

        // Determine how the fields of a & b extend
        const a_has_uniq_fields = partitioned.only_in_a.len() > 0;
        const b_has_uniq_fields = partitioned.only_in_b.len() > 0;

        var fields_ext: FieldsExtension = .exactly_the_same;
        if (a_has_uniq_fields and b_has_uniq_fields) {
            fields_ext = .both_extend;
        } else if (a_has_uniq_fields) {
            fields_ext = .a_extends_b;
        } else if (b_has_uniq_fields) {
            fields_ext = .b_extends_a;
        }

        // Unify fields
        switch (fields_ext) {
            .exactly_the_same => {
                // Unify exts
                try self.unifyGuarded(a_gathered_fields.ext, b_gathered_fields.ext);

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    self.scratch.in_both_fields.rangeToSlice(partitioned.in_both),
                    null,
                    null,
                    a_gathered_fields.ext,
                );
            },
            .a_extends_b => {
                // Create a new variable of a record with only a's uniq fields
                // This copies fields from scratch into type_store
                const only_in_a_fields_range = self.types_store.appendRecordFields(
                    self.scratch.only_in_a_fields.rangeToSlice(partitioned.only_in_a),
                );
                const only_in_a_var = self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_a_fields_range,
                    .ext = a_gathered_fields.ext,
                } } });

                // Unify the sub record with b's ext
                try self.unifyGuarded(only_in_a_var, b_gathered_fields.ext);

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    self.scratch.in_both_fields.rangeToSlice(partitioned.in_both),
                    null,
                    null,
                    only_in_a_var,
                );
            },
            .b_extends_a => {
                // Create a new variable of a record with only b's uniq fields
                // This copies fields from scratch into type_store
                const only_in_b_fields_range = self.types_store.appendRecordFields(
                    self.scratch.only_in_b_fields.rangeToSlice(partitioned.only_in_b),
                );
                const only_in_b_var = self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_b_fields_range,
                    .ext = b_gathered_fields.ext,
                } } });

                // Unify the sub record with a's ext
                try self.unifyGuarded(a_gathered_fields.ext, only_in_b_var);

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    self.scratch.in_both_fields.rangeToSlice(partitioned.in_both),
                    null,
                    null,
                    only_in_b_var,
                );
            },
            .both_extend => {
                // Create a new variable of a record with only a's uniq fields
                // This copies fields from scratch into type_store
                const only_in_a_fields_range = self.types_store.appendRecordFields(
                    self.scratch.only_in_a_fields.rangeToSlice(partitioned.only_in_a),
                );
                const only_in_a_var = self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_a_fields_range,
                    .ext = a_gathered_fields.ext,
                } } });

                // Create a new variable of a record with only b's uniq fields
                // This copies fields from scratch into type_store
                const only_in_b_fields_range = self.types_store.appendRecordFields(
                    self.scratch.only_in_b_fields.rangeToSlice(partitioned.only_in_b),
                );
                const only_in_b_var = self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_b_fields_range,
                    .ext = b_gathered_fields.ext,
                } } });

                // Create a new ext var
                const new_ext_var = self.fresh(vars, .{ .flex_var = null });

                // Unify the sub records with exts
                try self.unifyGuarded(a_gathered_fields.ext, only_in_b_var);
                try self.unifyGuarded(only_in_a_var, b_gathered_fields.ext);

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    self.scratch.in_both_fields.rangeToSlice(partitioned.in_both),
                    self.scratch.only_in_a_fields.rangeToSlice(partitioned.only_in_a),
                    self.scratch.only_in_b_fields.rangeToSlice(partitioned.only_in_b),
                    new_ext_var,
                );
            },
        }
    }

    const FieldsExtension = enum { exactly_the_same, a_extends_b, b_extends_a, both_extend };

    const GatheredFields = struct { ext: Var, range: RecordFieldSafeList.Range };

    /// Recursively unwraps the fields of an extensible record, flattening all visible fields
    /// into `scratch.gathered_fields` and following through:
    /// * aliases (by chasing `.backing_var`)
    /// * record extension chains (via nested `.record.ext`)
    ///
    /// Returns:
    /// * a `Range` indicating the location of the gathered fields in `gathered_fields`
    /// * the final tail extension variable, which is either a flex var or an empty record
    ///
    /// Errors if it encounters a malformed or invalid extension (e.g. a non-record type).
    fn gatherRecordFields(self: *Self, record: Record) error{TypeMismatch}!GatheredFields {
        // first, copy from the store's MultiList record fields array into scratch's
        // regular list, capturing the insertion range
        var range = self.scratch.copyGatherFieldsFromMultiList(
            &self.types_store.record_fields,
            record.fields,
        );

        // then recursiv
        var ext_var = record.ext;
        while (true) {
            switch (self.types_store.resolveVar(ext_var).desc.content) {
                .flex_var => {
                    return .{ .ext = ext_var, .range = range };
                },
                .rigid_var => {
                    return .{ .ext = ext_var, .range = range };
                },
                .alias => |alias| {
                    ext_var = alias.backing_var;
                },
                .structure => |flat_type| {
                    switch (flat_type) {
                        .record => |ext_record| {
                            const next_range = self.scratch.copyGatherFieldsFromMultiList(
                                &self.types_store.record_fields,
                                ext_record.fields,
                            );
                            range.end = next_range.end;
                            ext_var = ext_record.ext;
                        },
                        .empty_record => {
                            return .{ .ext = ext_var, .range = range };
                        },
                        else => return error.TypeMismatch,
                    }
                },
                else => return error.TypeMismatch,
            }
        }
    }

    const PartitionedRecordFields = struct {
        only_in_a: RecordFieldSafeList.Range,
        only_in_b: RecordFieldSafeList.Range,
        in_both: TwoRecordFieldsSafeList.Range,
    };

    /// Given two ranges of record fields stored in `scratch.gathered_fields`, this function:
    /// * sorts both slices in-place by field name
    /// * partitions them into three disjoint groups:
    ///     - fields only in `a`
    ///     - fields only in `b`
    ///     - fields present in both (by name)
    ///
    /// These groups are stored into dedicated scratch buffers:
    /// * `only_in_a_fields`
    /// * `only_in_b_fields`
    /// * `in_both_fields`
    ///
    /// The result is a set of ranges that can be used to slice those buffers.
    ///
    /// The caller must not mutate the field ranges between `gatherRecordFields` and `partitionFields`.
    pub fn partitionFields(
        ident_store: *const Ident.Store,
        scratch: *Scratch,
        a_fields_range: RecordFieldSafeList.Range,
        b_fields_range: RecordFieldSafeList.Range,
    ) PartitionedRecordFields {
        // First sort the fields
        const a_fields = scratch.gathered_fields.rangeToSlice(a_fields_range);
        std.mem.sort(RecordField, a_fields, ident_store, comptime RecordField.sortByNameAsc);
        const b_fields = scratch.gathered_fields.rangeToSlice(b_fields_range);
        std.mem.sort(RecordField, b_fields, ident_store, comptime RecordField.sortByNameAsc);

        // Get the start of index of the new range
        const a_fields_start: RecordFieldSafeList.Idx = @enumFromInt(scratch.only_in_a_fields.len());
        const b_fields_start: RecordFieldSafeList.Idx = @enumFromInt(scratch.only_in_b_fields.len());
        const both_fields_start: RecordFieldSafeList.Idx = @enumFromInt(scratch.in_both_fields.len());

        // Iterate over the fields in order, grouping them
        var a_i: usize = 0;
        var b_i: usize = 0;
        while (a_i < a_fields.len and b_i < b_fields.len) {
            const a_next = a_fields[a_i];
            const b_next = b_fields[b_i];
            const ord = RecordField.orderByName(ident_store, a_next, b_next);
            switch (ord) {
                .eq => {
                    _ = scratch.in_both_fields.append(scratch.gpa, TwoRecordFields{
                        .a = a_next,
                        .b = b_next,
                    });
                    a_i = a_i + 1;
                    b_i = b_i + 1;
                },
                .lt => {
                    _ = scratch.only_in_a_fields.append(scratch.gpa, a_next);
                    a_i = a_i + 1;
                },
                .gt => {
                    _ = scratch.only_in_b_fields.append(scratch.gpa, b_next);
                    b_i = b_i + 1;
                },
            }
        }

        // If b was shorter, add the extra a elems
        while (a_i < a_fields.len) {
            const a_next = a_fields[a_i];
            _ = scratch.only_in_a_fields.append(scratch.gpa, a_next);
            a_i = a_i + 1;
        }

        // If a was shorter, add the extra b elems
        while (b_i < b_fields.len) {
            const b_next = b_fields[b_i];
            _ = scratch.only_in_b_fields.append(scratch.gpa, b_next);
            b_i = b_i + 1;
        }

        // Get the end index of the new range
        const a_fields_end: RecordFieldSafeList.Idx = @enumFromInt(scratch.only_in_a_fields.len());
        const b_fields_end: RecordFieldSafeList.Idx = @enumFromInt(scratch.only_in_b_fields.len());
        const both_fields_end: RecordFieldSafeList.Idx = @enumFromInt(scratch.in_both_fields.len());

        // Return the ranges
        return .{
            .only_in_a = .{ .start = a_fields_start, .end = a_fields_end },
            .only_in_b = .{ .start = b_fields_start, .end = b_fields_end },
            .in_both = .{ .start = both_fields_start, .end = both_fields_end },
        };
    }

    /// Given a list of shared fields & a list of extended fields, unify the shared
    /// Then merge a new record with both shared+extended fields
    fn unifySharedFields(
        self: *Self,
        vars: *const ResolvedVarDescs,
        shared_fields: TwoRecordFieldsSafeList.Slice,
        mb_a_extended_fields: ?RecordFieldSafeList.Slice,
        mb_b_extended_fields: ?RecordFieldSafeList.Slice,
        ext: Var,
    ) Error!void {
        const range_start: RecordFieldSafeMultiList.Idx = @enumFromInt(self.types_store.record_fields.len());

        // Here, iterate over shared fields, sub unifying the field variables.
        // At this point, the fields are know to be identical, so we arbitrary choose b
        for (shared_fields) |shared| {
            try self.unifyGuarded(shared.a.var_, shared.b.var_);
            _ = self.types_store.appendRecordFields(&[_]RecordField{.{
                .name = shared.b.name,
                .var_ = shared.b.var_,
            }});
        }

        // Append combined fields
        if (mb_a_extended_fields) |extended_fields| {
            _ = self.types_store.appendRecordFields(extended_fields);
        }
        if (mb_b_extended_fields) |extended_fields| {
            _ = self.types_store.appendRecordFields(extended_fields);
        }

        const range_end: RecordFieldSafeMultiList.Idx = @enumFromInt(self.types_store.record_fields.len());

        // Merge vars
        self.merge(vars, Content{ .structure = FlatType{ .record = .{
            .fields = .{ .start = range_start, .end = range_end },
            .ext = ext,
        } } });
    }

    /// Unify two extensible tag union.
    ///
    /// This function implements Elm-style record unification, but for tag unions.
    ///
    /// Each tag union consists of:
    /// - a fixed set of known tags (`tags`)
    /// - an extensible tail variable (`ext`) that may point to additional unknown tags
    ///
    /// Given two tag unions `a` and `b`, we:
    ///   1. Collect all known tags by unwrapping their `ext` chains.
    ///   2. Partition the tags sets into:
    ///      - `in_both`: shared fields present in both `a` and `b`
    ///      - `only_in_a`: fields only present in `a`
    ///      - `only_in_b`: fields only present in `b`
    ///   3. Determine the relationship between the two tag unions based on these partitions.
    ///
    /// Four cases follow:
    ///
    /// ---
    ///
    /// **Case 1: Exactly the Same Tags**
    ///
    /// a = [ X ]ext_a
    /// b = [ X ]ext_b
    ///
    /// - All tags are shared
    /// - We unify `ext_a ~ ext_b`
    /// - Then unify each shared tag pair
    ///
    /// ---
    ///
    /// **Case 2: `a` Extends `b`**
    ///
    /// a = [ X, Y, Z ]ext_a
    /// b = [ X, Y ]ext_b
    ///
    /// - `a` has additional tags not in `b`
    /// - We generate a new var `only_in_a_var = [ Z ]ext_a`
    /// - Unify `only_in_a_var ~ ext_b`
    /// - Then unify shared tags into `[ X, Y ]only_in_a_var`
    ///
    /// ---
    ///
    /// **Case 3: `b` Extends `a`**
    ///
    /// a = [ X, Y ]ext_a
    /// b = [ X, Y, Z ]ext_b
    ///
    /// - Same as Case 2, but reversed
    /// - `b` has additional tags not in `a`
    /// - We generate a new var `only_in_b_var = [ Z ]ext_b`
    /// - Unify `ext_a ~ only_in_b_var`
    /// - Then unify shared tags into `[ X, Y ]only_in_b_var`
    ///
    /// ---
    ///
    /// **Case 4: Both Extend Each Other**
    ///
    /// a = [ X, Y, Z ]ext_a
    /// b = [ X, Y, W ]ext_b
    ///
    /// - Each has unique tags the other lacks
    /// - Generate:
    ///     - shared_ext = fresh flex_var
    ///     - only_in_a_var = [ Z ]shared_ext
    ///     - only_in_b_var = [ W ]shared_ext
    /// - Unify:
    ///     - `ext_a ~ only_in_b_var`
    ///     - `only_in_a_var ~ ext_b`
    /// - Then unify shared tags into `[ X, Y ]shared_ext`
    ///
    /// ---
    ///
    /// All tag unification is done using `unifySharedTags`, and new variables are created using `fresh`.
    ///
    /// This function does not attempt to deduplicate tags or reorder them — callers are responsible
    /// for providing consistent tag names.
    fn unifyTwoTagUnions(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_tag_union: TagUnion,
        b_tag_union: TagUnion,
    ) Error!void {

        // First, unwrap all tags for tag_union a, panicaing with various non-recoverable error
        // These pancis will likely be changed/removed in the future
        const a_gathered_tags = try self.gatherTagUnionTags(a_tag_union);
        const b_gathered_tags = try self.gatherTagUnionTags(b_tag_union);

        // Then partition the tags
        const partitioned = partitionTags(
            &self.module_env.idents,
            self.scratch,
            a_gathered_tags.range,
            b_gathered_tags.range,
        );

        // Determine how the tags of a & b extend
        const a_has_uniq_tags = partitioned.only_in_a.len() > 0;
        const b_has_uniq_tags = partitioned.only_in_b.len() > 0;

        var tags_ext: TagsExtension = .exactly_the_same;
        if (a_has_uniq_tags and b_has_uniq_tags) {
            tags_ext = .both_extend;
        } else if (a_has_uniq_tags) {
            tags_ext = .a_extends_b;
        } else if (b_has_uniq_tags) {
            tags_ext = .b_extends_a;
        }

        // Unify tags
        switch (tags_ext) {
            .exactly_the_same => {
                // Unify exts
                try self.unifyGuarded(a_gathered_tags.ext, b_gathered_tags.ext);

                // Unify shared tags
                // This copies tags from scratch into type_store
                try self.unifySharedTags(
                    vars,
                    self.scratch.in_both_tags.rangeToSlice(partitioned.in_both),
                    null,
                    null,
                    a_gathered_tags.ext,
                );
            },
            .a_extends_b => {
                // Create a new variable of a tag_union with only a's uniq tags
                // This copies tags from scratch into type_store
                const only_in_a_tags_range = self.types_store.appendTags(
                    self.scratch.only_in_a_tags.rangeToSlice(partitioned.only_in_a),
                );
                const only_in_a_var = self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_a_tags_range,
                    .ext = a_gathered_tags.ext,
                } } });

                // Unify the sub tag_union with b's ext
                try self.unifyGuarded(only_in_a_var, b_gathered_tags.ext);

                // Unify shared tags
                // This copies tags from scratch into type_store
                try self.unifySharedTags(
                    vars,
                    self.scratch.in_both_tags.rangeToSlice(partitioned.in_both),
                    null,
                    null,
                    only_in_a_var,
                );
            },
            .b_extends_a => {
                // Create a new variable of a tag_union with only b's uniq tags
                // This copies tags from scratch into type_store
                const only_in_b_tags_range = self.types_store.appendTags(
                    self.scratch.only_in_b_tags.rangeToSlice(partitioned.only_in_b),
                );
                const only_in_b_var = self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_b_tags_range,
                    .ext = b_gathered_tags.ext,
                } } });

                // Unify the sub tag_union with a's ext
                try self.unifyGuarded(a_gathered_tags.ext, only_in_b_var);

                // Unify shared tags
                // This copies tags from scratch into type_store
                try self.unifySharedTags(
                    vars,
                    self.scratch.in_both_tags.rangeToSlice(partitioned.in_both),
                    null,
                    null,
                    only_in_b_var,
                );
            },
            .both_extend => {
                // Create a new variable of a tag_union with only a's uniq tags
                // This copies tags from scratch into type_store
                const only_in_a_tags_range = self.types_store.appendTags(
                    self.scratch.only_in_a_tags.rangeToSlice(partitioned.only_in_a),
                );
                const only_in_a_var = self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_a_tags_range,
                    .ext = a_gathered_tags.ext,
                } } });

                // Create a new variable of a tag_union with only b's uniq tags
                // This copies tags from scratch into type_store
                const only_in_b_tags_range = self.types_store.appendTags(
                    self.scratch.only_in_b_tags.rangeToSlice(partitioned.only_in_b),
                );
                const only_in_b_var = self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_b_tags_range,
                    .ext = b_gathered_tags.ext,
                } } });

                // Create a new ext var
                const new_ext_var = self.fresh(vars, .{ .flex_var = null });

                // Unify the sub tag_unions with exts
                try self.unifyGuarded(a_gathered_tags.ext, only_in_b_var);
                try self.unifyGuarded(only_in_a_var, b_gathered_tags.ext);

                // Unify shared tags
                // This copies tags from scratch into type_store
                try self.unifySharedTags(
                    vars,
                    self.scratch.in_both_tags.rangeToSlice(partitioned.in_both),
                    self.scratch.only_in_a_tags.rangeToSlice(partitioned.only_in_a),
                    self.scratch.only_in_b_tags.rangeToSlice(partitioned.only_in_b),
                    new_ext_var,
                );
            },
        }
    }

    const TagsExtension = enum { exactly_the_same, a_extends_b, b_extends_a, both_extend };

    const GatheredTags = struct { ext: Var, range: TagSafeList.Range };

    /// Recursively unwraps the tags of an extensible tag_union, flattening all visible tags
    /// into `scratch.gathered_tags` and following through:
    /// * aliases (by chasing `.backing_var`)
    /// * tag_union extension chains (via nested `.tag_union.ext`)
    ///
    /// Returns:
    /// * a `Range` indicating the location of the gathered tags in `gathered_tags`
    /// * the final tail extension variable, which is either a flex var or an empty tag_union
    ///
    /// Errors if it encounters a malformed or invalid extension (e.g. a non-tag_union type).
    fn gatherTagUnionTags(self: *Self, tag_union: TagUnion) error{TypeMismatch}!GatheredTags {
        // first, copy from the store's MultiList record fields array into scratch's
        // regular list, capturing the insertion range
        var range = self.scratch.copyGatherTagsFromMultiList(
            &self.types_store.tags,
            tag_union.tags,
        );

        // then loop gathering extensible tags
        var ext_var = tag_union.ext;
        while (true) {
            switch (self.types_store.resolveVar(ext_var).desc.content) {
                .flex_var => {
                    return .{ .ext = ext_var, .range = range };
                },
                .rigid_var => {
                    return .{ .ext = ext_var, .range = range };
                },
                .alias => |alias| {
                    ext_var = alias.backing_var;
                },
                .structure => |flat_type| {
                    switch (flat_type) {
                        .tag_union => |ext_tag_union| {
                            const next_range = self.scratch.copyGatherTagsFromMultiList(
                                &self.types_store.tags,
                                ext_tag_union.tags,
                            );
                            range.end = next_range.end;
                            ext_var = ext_tag_union.ext;
                        },
                        .empty_tag_union => {
                            return .{ .ext = ext_var, .range = range };
                        },
                        else => return error.TypeMismatch,
                    }
                },
                else => return error.TypeMismatch,
            }
        }
    }

    const PartitionedTags = struct {
        only_in_a: TagSafeList.Range,
        only_in_b: TagSafeList.Range,
        in_both: TwoTagsSafeList.Range,
    };

    /// Given two ranges of tag_union tags stored in `scratch.gathered_tags`, this function:
    /// * sorts both slices in-place by field name
    /// * partitions them into three disjoint groups:
    ///     - tags only in `a`
    ///     - tags only in `b`
    ///     - tags present in both (by name)
    ///
    /// These groups are stored into dedicated scratch buffers:
    /// * `only_in_a_tags`
    /// * `only_in_b_tags`
    /// * `in_both_tags`
    ///
    /// The result is a set of ranges that can be used to slice those buffers.
    ///
    /// The caller must not mutate the field ranges between `gatherTagUnionTags` and `partitionTags`.
    pub fn partitionTags(
        ident_store: *const Ident.Store,
        scratch: *Scratch,
        a_tags_range: TagSafeList.Range,
        b_tags_range: TagSafeList.Range,
    ) PartitionedTags {
        // First sort the tags
        const a_tags = scratch.gathered_tags.rangeToSlice(a_tags_range);
        std.mem.sort(Tag, a_tags, ident_store, comptime Tag.sortByNameAsc);
        const b_tags = scratch.gathered_tags.rangeToSlice(b_tags_range);
        std.mem.sort(Tag, b_tags, ident_store, comptime Tag.sortByNameAsc);

        // Get the start of index of the new range
        const a_tags_start: TagSafeList.Idx = @enumFromInt(scratch.only_in_a_tags.len());
        const b_tags_start: TagSafeList.Idx = @enumFromInt(scratch.only_in_b_tags.len());
        const both_tags_start: TagSafeList.Idx = @enumFromInt(scratch.in_both_tags.len());

        // Iterate over the tags in order, grouping them
        var a_i: usize = 0;
        var b_i: usize = 0;
        while (a_i < a_tags.len and b_i < b_tags.len) {
            const a_next = a_tags[a_i];
            const b_next = b_tags[b_i];
            const ord = Tag.orderByName(ident_store, a_next, b_next);
            switch (ord) {
                .eq => {
                    _ = scratch.in_both_tags.append(scratch.gpa, TwoTags{ .a = a_next, .b = b_next });
                    a_i = a_i + 1;
                    b_i = b_i + 1;
                },
                .lt => {
                    _ = scratch.only_in_a_tags.append(scratch.gpa, a_next);
                    a_i = a_i + 1;
                },
                .gt => {
                    _ = scratch.only_in_b_tags.append(scratch.gpa, b_next);
                    b_i = b_i + 1;
                },
            }
        }

        // If b was shorter, add the extra a elems
        while (a_i < a_tags.len) {
            const a_next = a_tags[a_i];
            _ = scratch.only_in_a_tags.append(scratch.gpa, a_next);
            a_i = a_i + 1;
        }

        // If a was shorter, add the extra b elems
        while (b_i < b_tags.len) {
            const b_next = b_tags[b_i];
            _ = scratch.only_in_b_tags.append(scratch.gpa, b_next);
            b_i = b_i + 1;
        }

        // Get the end index of the new range
        const a_tags_end: TagSafeList.Idx = @enumFromInt(scratch.only_in_a_tags.len());
        const b_tags_end: TagSafeList.Idx = @enumFromInt(scratch.only_in_b_tags.len());
        const both_tags_end: TagSafeList.Idx = @enumFromInt(scratch.in_both_tags.len());

        // Return the ranges
        return .{
            .only_in_a = .{ .start = a_tags_start, .end = a_tags_end },
            .only_in_b = .{ .start = b_tags_start, .end = b_tags_end },
            .in_both = .{ .start = both_tags_start, .end = both_tags_end },
        };
    }

    /// Given a list of shared tags & a list of extended tags, unify the shared tags.
    /// Then merge a new tag_union with both shared+extended tags
    fn unifySharedTags(
        self: *Self,
        vars: *const ResolvedVarDescs,
        shared_tags: []TwoTags,
        mb_a_extended_tags: ?[]Tag,
        mb_b_extended_tags: ?[]Tag,
        ext: Var,
    ) Error!void {
        const range_start: TagSafeMultiList.Idx = @enumFromInt(self.types_store.tags.len());

        for (shared_tags) |tags| {
            const tag_a_args = self.types_store.getTagArgsSlice(tags.a.args);
            const tag_b_args = self.types_store.getTagArgsSlice(tags.b.args);

            if (tag_a_args.len != tag_b_args.len) return error.TypeMismatch;

            for (tag_a_args, tag_b_args) |a_arg, b_arg| {
                try self.unifyGuarded(a_arg, b_arg);
            }

            _ = self.types_store.appendTags(&[_]Tag{.{
                .name = tags.b.name,
                .args = tags.b.args,
            }});
        }

        // Append combined tags
        if (mb_a_extended_tags) |extended_tags| {
            _ = self.types_store.appendTags(extended_tags);
        }
        if (mb_b_extended_tags) |extended_tags| {
            _ = self.types_store.appendTags(extended_tags);
        }

        const range_end: TagSafeMultiList.Idx = @enumFromInt(self.types_store.tags.len());

        // Merge vars
        self.merge(vars, Content{ .structure = FlatType{ .tag_union = .{
            .tags = .{ .start = range_start, .end = range_end },
            .ext = ext,
        } } });
    }

    /// Set error data in scratch & throw
    fn setUnifyErrAndThrow(self: *Self, err: UnifyErrCtx) Error!void {
        self.scratch.setUnifyErr(err);
        return error.UnifyErr;
    }
};

/// A fatal occurs error
pub const UnifyErrCtx = struct {
    type_: Type,
    side: Side,

    pub const Type = enum {
        recursion_infinite,
        recursion_anonymous,
        invalid_number_type,
    };
};

/// A reusable memory arena used across unification calls to avoid per-call allocations.
///
/// `Scratch` owns several typed scratch arrays, each designed to hold a specific type of
/// temporary data needed during unification. These include:
///
/// * `fresh_vars`: type variables created during a unification pass
/// * For records
///   * `gathered_fields`: accumulated record fields from recursive extensions
///   * `only_in_a_fields`, `only_in_b_fields`: disjoint fields after `partitionFields`
///   * `in_both_fields`: shared fields with matching names
/// * For tag unions
///   * `gathered_tags`: accumulated tags from recursive extensions
///   * `only_in_a_tags`, `only_in_b_tags`: disjoint tags after `partitionTags`
///   * `in_both_tags`: shared tags with matching names
/// * For occurs:
///   * `occurs_scratch`: Scratch data need by occurs
/// * For errors:
///   * `err`: Data about the error thrown
///
/// `Scratch` should be initialized once and reused for many unification runs.
/// Each call to `unify` will reset the scratch buffer at the start.
///
/// Note that while the types store uses MultiLists for record fields & tags, this
/// struct uses regular safe list. There are several reasons for this:
/// 1. We have to sort tags/fields during unification, and MultiList doesn't
///    have a great way to do this
/// 2. These allocations are freed after this stage of compilation completes, so
///    while SafeLists waste some space compared to MultiList, the cost isn't too
///    high
///
/// TODO: If canonicalization can ensure that record fields/tags are always sorted
/// then we could switch these to use multi lists.
pub const Scratch = struct {
    const Self = @This();

    // a scratch allocator.
    gpa: std.mem.Allocator,

    // used by caller of unify
    fresh_vars: VarSafeList,

    // records - used internal by unification
    gathered_fields: RecordFieldSafeList,
    only_in_a_fields: RecordFieldSafeList,
    only_in_b_fields: RecordFieldSafeList,
    in_both_fields: TwoRecordFieldsSafeList,

    // records - used internal by unification
    gathered_tags: TagSafeList,
    only_in_a_tags: TagSafeList,
    only_in_b_tags: TagSafeList,
    in_both_tags: TwoTagsSafeList,

    // occurs
    occurs_scratch: occurs.Scratch,

    // err
    err: ?UnifyErrCtx,

    /// Init scratch
    pub fn init(gpa: std.mem.Allocator) Self {
        // TODO: Set these based on the heuristics
        return .{
            .gpa = gpa,
            .fresh_vars = VarSafeList.initCapacity(gpa, 8),
            .gathered_fields = RecordFieldSafeList.initCapacity(gpa, 32),
            .only_in_a_fields = RecordFieldSafeList.initCapacity(gpa, 32),
            .only_in_b_fields = RecordFieldSafeList.initCapacity(gpa, 32),
            .in_both_fields = TwoRecordFieldsSafeList.initCapacity(gpa, 32),
            .gathered_tags = TagSafeList.initCapacity(gpa, 32),
            .only_in_a_tags = TagSafeList.initCapacity(gpa, 32),
            .only_in_b_tags = TagSafeList.initCapacity(gpa, 32),
            .in_both_tags = TwoTagsSafeList.initCapacity(gpa, 32),
            .occurs_scratch = occurs.Scratch.init(gpa),
            .err = null,
        };
    }

    /// Deinit scratch
    pub fn deinit(self: *Self) void {
        self.fresh_vars.deinit(self.gpa);
        self.gathered_fields.deinit(self.gpa);
        self.only_in_a_fields.deinit(self.gpa);
        self.only_in_b_fields.deinit(self.gpa);
        self.in_both_fields.deinit(self.gpa);
        self.gathered_tags.deinit(self.gpa);
        self.only_in_a_tags.deinit(self.gpa);
        self.only_in_b_tags.deinit(self.gpa);
        self.in_both_tags.deinit(self.gpa);
        self.occurs_scratch.deinit();
    }

    /// Reset the scratch arrays, retaining the allocated memory
    pub fn reset(self: *Scratch) void {
        self.gathered_fields.items.clearRetainingCapacity();
        self.only_in_a_fields.items.clearRetainingCapacity();
        self.only_in_b_fields.items.clearRetainingCapacity();
        self.in_both_fields.items.clearRetainingCapacity();
        self.gathered_tags.items.clearRetainingCapacity();
        self.only_in_a_tags.items.clearRetainingCapacity();
        self.only_in_b_tags.items.clearRetainingCapacity();
        self.in_both_tags.items.clearRetainingCapacity();
        self.occurs_scratch.reset();
    }

    // helpers //

    /// Given a multi list of record fields and a range, copy from the multi list
    /// into scratch's gathered fields array
    fn copyGatherFieldsFromMultiList(
        self: *Self,
        multi_list: *const RecordFieldSafeMultiList,
        range: RecordFieldSafeMultiList.Range,
    ) RecordFieldSafeList.Range {
        const start: RecordFieldSafeList.Idx = @enumFromInt(self.gathered_fields.len());
        const record_fields_slice = multi_list.rangeToSlice(range);
        for (record_fields_slice.items(.name), record_fields_slice.items(.var_)) |name, var_| {
            _ = self.gathered_fields.append(
                self.gpa,
                RecordField{ .name = name, .var_ = var_ },
            );
        }
        const end: RecordFieldSafeList.Idx = @enumFromInt(self.gathered_fields.len());
        return .{ .start = start, .end = end };
    }
    /// Given a multi list of tag and a range, copy from the multi list
    /// into scratch's gathered fields array
    fn copyGatherTagsFromMultiList(
        self: *Self,
        multi_list: *const TagSafeMultiList,
        range: TagSafeMultiList.Range,
    ) TagSafeList.Range {
        const start: TagSafeList.Idx = @enumFromInt(self.gathered_tags.len());
        const tag_slice = multi_list.rangeToSlice(range);
        for (tag_slice.items(.name), tag_slice.items(.args)) |ident, args| {
            _ = self.gathered_tags.append(
                self.gpa,
                Tag{ .name = ident, .args = args },
            );
        }
        const end: TagSafeList.Idx = @enumFromInt(self.gathered_tags.len());
        return .{ .start = start, .end = end };
    }

    fn appendSliceGatheredFields(self: *Self, fields: []const RecordField) RecordFieldSafeList.Range {
        return self.gathered_fields.appendSlice(self.gpa, fields);
    }

    fn appendSliceGatheredTags(self: *Self, fields: []const Tag) TagSafeList.Range {
        return self.gathered_tags.appendSlice(self.gpa, fields);
    }

    fn setUnifyErr(self: *Self, err: UnifyErrCtx) void {
        self.err = err;
    }
};

// tests //

/// A lightweight test harness used in unification and type inference tests.
///
/// `TestEnv` bundles together the following components:
/// * a module env for holding things like idents
/// * a type store for registering and resolving types
/// * a reusable `Scratch` buffer for managing field partitions and temporary variables
///
/// This is intended to simplify unit test setup, particularly for unifying records,
/// functions, aliases, and other structured types.
const TestEnv = struct {
    const Self = @This();

    module_env: *base.ModuleEnv,
    types_store: *store.Store,
    scratch: Scratch,
    occurs_scratch: occurs.Scratch,

    /// Init everything needed to test unify
    /// This includes allocating module_env on the heap
    ///
    /// TODO: Is heap allocation unideal here? If we want to optimize tests, we
    /// could pull module_env's initialization out of here, but this results in
    /// slight more verbose setup for each test
    fn init(gpa: std.mem.Allocator) Self {
        const module_env = gpa.create(base.ModuleEnv) catch |e| exitOnOutOfMemory(e);
        module_env.* = base.ModuleEnv.init(gpa);
        return .{
            .module_env = module_env,
            .types_store = &module_env.types_store,
            .scratch = Scratch.init(module_env.gpa),
            .occurs_scratch = occurs.Scratch.init(module_env.gpa),
        };
    }

    /// Deinit the test env, including deallocing the module_env from the heap
    fn deinit(self: *Self) void {
        self.module_env.deinit();
        self.module_env.gpa.destroy(self.module_env);
        self.scratch.deinit();
        self.occurs_scratch.deinit();
    }

    const Error = error{ VarIsNotRoot, IsNotRecord, IsNotTagUnion };

    /// Get a desc from a root var
    fn getDescForRootVar(self: *Self, var_: Var) error{VarIsNotRoot}!Desc {
        switch (self.types_store.getSlot(var_)) {
            .root => |desc_idx| return self.types_store.getDesc(desc_idx),
            .redirect => return error.VarIsNotRoot,
        }
    }

    /// Unwrap a record or throw
    fn getRecordOrErr(desc: Desc) error{IsNotRecord}!Record {
        return desc.content.unwrapRecord() orelse error.IsNotRecord;
    }

    /// Unwrap a record or throw
    fn getTagUnionOrErr(desc: Desc) error{IsNotTagUnion}!TagUnion {
        return desc.content.unwrapTagUnion() orelse error.IsNotTagUnion;
    }

    fn mkTypeIdent(self: *Self, name: []const u8) TypeIdent {
        const ident_idx = self.module_env.idents.insert(self.module_env.gpa, Ident.for_text(name), Region.zero());
        return TypeIdent{ .ident_idx = ident_idx };
    }

    // helpers - rigid var

    fn mkRigidVar(self: *Self, name: []const u8) Content {
        const ident_idx = self.module_env.idents.insert(self.module_env.gpa, Ident.for_text(name), Region.zero());
        return Self.mkRigidVarFromIdent(ident_idx);
    }

    fn mkRigidVarFromIdent(ident_idx: Ident.Idx) Content {
        return .{ .rigid_var = ident_idx };
    }

    // helpers - alias

    fn mkAlias(self: *Self, name: []const u8, args: []const Var, backing_var: Var) Alias {
        const args_range = self.types_store.appendAliasArgs(args);
        return .{
            .ident = self.mkTypeIdent(name),
            .args = args_range,
            .backing_var = backing_var,
        };
    }

    // helpers - nums

    fn mkNum(self: *Self, var_: Var) Var {
        return self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = var_ } } });
    }

    fn mkNumFlex(self: *Self) Var {
        const prec_var = self.types_store.fresh();
        return self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = prec_var } } });
    }

    fn mkFrac(self: *Self, var_: Var) Var {
        const frac_var = self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = var_ } } });
        return self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = frac_var } } });
    }

    fn mkFracFlex(self: *Self) Var {
        const prec_var = self.types_store.fresh();
        const frac_var = self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = prec_var } } });
        return self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = frac_var } } });
    }

    fn mkFracRigid(self: *Self, name: []const u8) Var {
        const rigid = self.types_store.freshFromContent(self.mkRigidVar(name));
        const frac_var = self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = rigid } } });
        return self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = frac_var } } });
    }

    fn mkFracExact(self: *Self, prec: Num.Frac.Precision) Var {
        const prec_var = self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .frac_precision = prec } } });
        const frac_var = self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = prec_var } } });
        return self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = frac_var } } });
    }

    fn mkInt(self: *Self, var_: Var) Var {
        const int_var = self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .int_poly = var_ } } });
        return self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });
    }

    fn mkIntFlex(self: *Self) Var {
        const prec_var = self.types_store.fresh();
        const int_var = self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .int_poly = prec_var } } });
        return self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });
    }

    fn mkIntRigid(self: *Self, name: []const u8) Var {
        const rigid = self.types_store.freshFromContent(self.mkRigidVar(name));
        const int_var = self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .int_poly = rigid } } });
        return self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });
    }

    fn mkIntExact(self: *Self, prec: Num.Int.Precision) Var {
        const prec_var = self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .int_precision = prec } } });
        const int_var = self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .int_poly = prec_var } } });
        return self.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = int_var } } });
    }

    // helpers - structure - tuple

    fn mkTuple(self: *Self, slice: []const Var) Content {
        const elems_range = self.types_store.appendTupleElems(slice);
        return Content{ .structure = .{ .tuple = .{ .elems = elems_range } } };
    }

    // helpers - custom type

    fn mkCustomType(self: *Self, name: []const u8, args: []const Var, backing_var: Var) Content {
        const args_range = self.types_store.appendCustomTypeArgs(args);
        return Content{ .structure = .{ .custom_type = .{
            .ident = self.mkTypeIdent(name),
            .args = args_range,
            .backing_var = backing_var,
        } } };
    }

    // helpers - structure - func

    fn mkFunc(self: *Self, args: []const Var, ret: Var, eff: Var) Content {
        const args_range = self.types_store.appendFuncArgs(args);
        return Content{ .structure = .{ .func = .{ .args = args_range, .ret = ret, .eff = eff } } };
    }

    fn mkFuncFlex(self: *Self, args: []const Var, ret: Var) Content {
        const eff_var = self.types_store.freshFromContent(.{ .flex_var = null });
        return self.mkFunc(args, ret, eff_var);
    }

    fn mkFuncPure(self: *Self, args: []const Var, ret: Var) Content {
        const eff_var = self.types_store.freshFromContent(.pure);
        return self.mkFunc(args, ret, eff_var);
    }

    fn mkFuncEff(self: *Self, args: []const Var, ret: Var) Content {
        const eff_var = self.types_store.freshFromContent(.effectful);
        return self.mkFunc(args, ret, eff_var);
    }

    // helpers - structure - records

    fn mkRecordField(self: *Self, name: []const u8, var_: Var) RecordField {
        const ident_idx = self.module_env.idents.insert(self.module_env.gpa, Ident.for_text(name), Region.zero());
        return Self.mkRecordFieldFromIdent(ident_idx, var_);
    }

    fn mkRecordFieldFromIdent(ident_idx: Ident.Idx, var_: Var) RecordField {
        return RecordField{ .name = ident_idx, .var_ = var_ };
    }

    const RecordInfo = struct { record: Record, content: Content };

    fn mkRecord(self: *Self, fields: []const RecordField, ext_var: Var) RecordInfo {
        const fields_range = self.types_store.appendRecordFields(fields);
        const record = Record{ .fields = fields_range, .ext = ext_var };
        return .{ .content = Content{ .structure = .{ .record = record } }, .record = record };
    }

    fn mkRecordOpen(self: *Self, fields: []const RecordField) RecordInfo {
        const ext_var = self.types_store.freshFromContent(.{ .flex_var = null });
        return self.mkRecord(fields, ext_var);
    }

    fn mkRecordClosed(self: *Self, fields: []const RecordField) RecordInfo {
        const ext_var = self.types_store.freshFromContent(.{ .structure = .empty_record });
        return self.mkRecord(fields, ext_var);
    }

    // helpers - structure - tag union

    const TagUnionInfo = struct { tag_union: TagUnion, content: Content };

    fn mkTagArgs(self: *Self, args: []const Var) VarSafeList.Range {
        return self.types_store.appendTagArgs(args);
    }

    fn mkTag(self: *Self, name: []const u8, args: []const Var) Tag {
        const ident_idx = self.module_env.idents.insert(self.module_env.gpa, Ident.for_text(name), Region.zero());
        return Tag{ .name = ident_idx, .args = self.types_store.appendTagArgs(args) };
    }

    fn mkTagUnion(self: *Self, tags: []const Tag, ext_var: Var) TagUnionInfo {
        const tags_range = self.types_store.appendTags(tags);
        const tag_union = TagUnion{ .tags = tags_range, .ext = ext_var };
        return .{ .content = Content{ .structure = .{ .tag_union = tag_union } }, .tag_union = tag_union };
    }

    fn mkTagUnionOpen(self: *Self, tags: []const Tag) TagUnionInfo {
        const ext_var = self.types_store.freshFromContent(.{ .flex_var = null });
        return self.mkTagUnion(tags, ext_var);
    }

    fn mkTagUnionClosed(self: *Self, tags: []const Tag) TagUnionInfo {
        const ext_var = self.types_store.freshFromContent(.{ .structure = .empty_tag_union });
        return self.mkTagUnion(tags, ext_var);
    }
};

// unification - flex_vars

test "unify - identical" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.fresh();
    const desc = try env.getDescForRootVar(a);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, a);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(desc, try env.getDescForRootVar(a));
}

test "unify - both flex vars" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.fresh();
    const b = env.types_store.fresh();

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
}

test "unify - a is flex_var and b is not" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.fresh();
    const b = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
}

// unification - rigid

test "rigid_var - unifies with flex_var" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid = env.mkRigidVar("a");
    const a = env.types_store.freshFromContent(.{ .flex_var = null });
    const b = env.types_store.freshFromContent(rigid);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);
    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(rigid, (try env.getDescForRootVar(b)).content);
}

test "rigid_var - unifies with flex_var (other way)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid = env.mkRigidVar("a");
    const a = env.types_store.freshFromContent(rigid);
    const b = env.types_store.freshFromContent(.{ .flex_var = null });

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);
    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(rigid, (try env.getDescForRootVar(b)).content);
}

test "rigid_var - cannot unify with alias (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const alias = env.types_store.freshFromContent(Content{ .structure = .str });
    const rigid = env.types_store.freshFromContent(env.mkRigidVar("a"));

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, alias, rigid);
    try std.testing.expectEqual(false, result.isOk());
}

test "rigid_var - cannot unify with identical ident str (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid1 = env.types_store.freshFromContent(env.mkRigidVar("a"));
    const rigid2 = env.types_store.freshFromContent(env.mkRigidVar("a"));

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, rigid1, rigid2);
    try std.testing.expectEqual(false, result.isOk());
}

// unification - aliases

test "unify - alias with same args" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const bool_ = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{ str, bool_ }));
    const alias = Content{ .alias = env.mkAlias("AliasName", &[_]Var{ str, bool_ }, backing) };

    const a = env.types_store.freshFromContent(alias);
    const b = env.types_store.freshFromContent(alias);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(alias, (try env.getDescForRootVar(b)).content);
}

test "unify - aliases with different names but same backing" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{str}));
    const a_alias = Content{ .alias = env.mkAlias("AliasA", &[_]Var{str}, backing) };
    const b_alias = Content{ .alias = env.mkAlias("AliasB", &[_]Var{str}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(a_alias, (try env.getDescForRootVar(a)).content);
    try std.testing.expectEqual(b_alias, (try env.getDescForRootVar(b)).content);
}

test "unify - alias with different args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const bool_ = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{ str, bool_ }));

    const a_alias = Content{ .alias = env.mkAlias("Alias", &[_]Var{str}, backing) };
    const b_alias = Content{ .alias = env.mkAlias("Alias", &[_]Var{bool_}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - alias with flex" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const bool_ = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });
    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{ str, bool_ }));

    const a_alias = Content{ .alias = env.mkAlias("Alias", &[_]Var{bool_}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.fresh();

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(a_alias, (try env.getDescForRootVar(b)).content);
}

// unification - pure/effectful

test "unify - pure with pure" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.freshFromContent(.pure);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.pure, (try env.getDescForRootVar(b)).content);
}

test "unify - effectful with effectful" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.freshFromContent(.effectful);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.effectful, (try env.getDescForRootVar(b)).content);
}

test "unify - pure with flex_var" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.fresh();

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.pure, (try env.getDescForRootVar(b)).content);
}

test "unify - effectful with flex_var" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.fresh();

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.effectful, (try env.getDescForRootVar(b)).content);
}

test "unify - pure with effectful" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.freshFromContent(.effectful);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.effectful, (try env.getDescForRootVar(b)).content);
}

test "unify - effectful with pure (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.freshFromContent(.pure);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - pure with err (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.freshFromContent(.err);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - effectful with err (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.freshFromContent(.err);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - pure with structure type fails" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.freshFromContent(str);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
}

test "unify - effectful with structure type fails" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.freshFromContent(str);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
}

// unification - structure/flex_vars

test "unify - a is builtin and b is flex_var" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };

    const a = env.types_store.freshFromContent(str);
    const b = env.types_store.fresh();

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a is flex_var and b is builtin" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };

    const a = env.types_store.fresh();
    const b = env.types_store.freshFromContent(str);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - builtin

test "unify - a & b are both str" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };

    const a = env.types_store.freshFromContent(str);
    const b = env.types_store.freshFromContent(str);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are diff (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const int = Content{ .structure = .{ .num = Num.int_i8 } };

    const a = env.types_store.freshFromContent(int);
    const b = env.types_store.freshFromContent(str);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b box with same arg unify" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = env.types_store.freshFromContent(str);

    const box_str = Content{ .structure = .{ .box = str_var } };

    const a = env.types_store.freshFromContent(box_str);
    const b = env.types_store.freshFromContent(box_str);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(box_str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b box with diff args (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = env.types_store.freshFromContent(str);

    const i64_ = Content{ .structure = .{ .num = Num.int_i64 } };
    const i64_var = env.types_store.freshFromContent(i64_);

    const box_str = Content{ .structure = .{ .box = str_var } };
    const box_i64 = Content{ .structure = .{ .box = i64_var } };

    const a = env.types_store.freshFromContent(box_str);
    const b = env.types_store.freshFromContent(box_i64);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b list with same arg unify" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = env.types_store.freshFromContent(str);

    const list_str = Content{ .structure = .{ .list = str_var } };

    const a = env.types_store.freshFromContent(list_str);
    const b = env.types_store.freshFromContent(list_str);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(list_str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b list with diff args (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = env.types_store.freshFromContent(str);

    const u8_ = Content{ .structure = .{ .num = Num.int_u8 } };
    const u8_var = env.types_store.freshFromContent(u8_);

    const list_str = Content{ .structure = .{ .list = str_var } };
    const list_u8 = Content{ .structure = .{ .list = u8_var } };

    const a = env.types_store.freshFromContent(list_str);
    const b = env.types_store.freshFromContent(list_u8);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - tuple

test "unify - a & b are same tuple" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = env.types_store.freshFromContent(str);

    const bool_ = Content{ .structure = .{ .num = Num.int_i8 } };
    const bool_var = env.types_store.freshFromContent(bool_);

    const tuple_str_bool = env.mkTuple(&[_]Var{ str_var, bool_var });

    const a = env.types_store.freshFromContent(tuple_str_bool);
    const b = env.types_store.freshFromContent(tuple_str_bool);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(tuple_str_bool, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are tuples with args flipped (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = Content{ .structure = .str };
    const str_var = env.types_store.freshFromContent(str);

    const bool_ = Content{ .structure = .{ .num = Num.int_i8 } };
    const bool_var = env.types_store.freshFromContent(bool_);

    const tuple_str_bool = env.mkTuple(&[_]Var{ str_var, bool_var });
    const tuple_bool_str = env.mkTuple(&[_]Var{ bool_var, str_var });

    const a = env.types_store.freshFromContent(tuple_str_bool);
    const b = env.types_store.freshFromContent(tuple_bool_str);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - compact/compact

test "unify - two compact ints" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = env.types_store.freshFromContent(int_i32);
    const b = env.types_store.freshFromContent(int_i32);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - two compact ints (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const b = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - two compact fracs" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = env.types_store.freshFromContent(frac_f32);
    const b = env.types_store.freshFromContent(frac_f32);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(frac_f32, (try env.getDescForRootVar(b)).content);
}

test "unify - two compact fracs (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.frac_f32 } });
    const b = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.frac_dec } });

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - poly/poly

test "unify - two poly ints" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.mkIntExact(Num.Int.Precision.u8);
    const b = env.mkIntExact(Num.Int.Precision.u8);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
}

test "unify - two poly ints (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.mkIntExact(Num.Int.Precision.u8);
    const b = env.mkIntExact(Num.Int.Precision.i128);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - two poly fracs" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.mkFracExact(Num.Frac.Precision.f64);
    const b = env.mkFracExact(Num.Frac.Precision.f64);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
}

test "unify - two poly fracs (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.mkFracExact(Num.Frac.Precision.f32);
    const b = env.mkFracExact(Num.Frac.Precision.f64);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - poly/compact_int

test "unify - Num(flex) and compact int" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = env.mkNumFlex();
    const b = env.types_store.freshFromContent(int_i32);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Int(flex)) and compact int" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = env.mkIntFlex();
    const b = env.types_store.freshFromContent(int_i32);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Int(U8)) and compact int U8" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_u8 = Content{ .structure = .{ .num = Num.int_u8 } };
    const a = env.mkIntExact(Num.Int.Precision.u8);
    const b = env.types_store.freshFromContent(int_u8);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(int_u8, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Int(U8)) and compact int I32 (fails)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = env.mkIntExact(Num.Int.Precision.u8);
    const b = env.types_store.freshFromContent(int_i32);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - poly/compact_frac

test "unify - Num(flex) and compact frac" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = env.mkNumFlex();
    const b = env.types_store.freshFromContent(frac_f32);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(frac_f32, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(flex)) and compact frac" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = env.mkFracFlex();
    const b = env.types_store.freshFromContent(frac_f32);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(frac_f32, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(Dec)) and compact frac Dec" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_dec = Content{ .structure = .{ .num = Num.frac_dec } };
    const a = env.mkFracExact(Num.Frac.Precision.dec);
    const b = env.types_store.freshFromContent(frac_dec);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(frac_dec, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(F32)) and compact frac Dec (fails)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = env.mkFracExact(Num.Frac.Precision.dec);
    const b = env.types_store.freshFromContent(frac_f32);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - compact_int/poly

test "unify - compact int and Num(flex)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = env.types_store.freshFromContent(int_i32);
    const b = env.mkNumFlex();

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - compact int and Num(Int(flex))" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = env.types_store.freshFromContent(int_i32);
    const b = env.mkIntFlex();

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - compact int and U8 Num(Int(U8))" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_u8 = Content{ .structure = .{ .num = Num.int_u8 } };
    const a = env.types_store.freshFromContent(int_u8);
    const b = env.mkIntExact(Num.Int.Precision.u8);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(int_u8, (try env.getDescForRootVar(b)).content);
}

test "unify - compact int U8 and  Num(Int(I32)) (fails)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = Content{ .structure = .{ .num = Num.int_i32 } };
    const a = env.types_store.freshFromContent(int_i32);
    const b = env.mkIntExact(Num.Int.Precision.u8);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - compact_frac/poly

test "unify - compact frac and Num(flex)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = env.types_store.freshFromContent(frac_f32);
    const b = env.mkNumFlex();

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(frac_f32, (try env.getDescForRootVar(b)).content);
}

test "unify - compact frac and Num(Frac(flex))" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = env.types_store.freshFromContent(frac_f32);
    const b = env.mkFracFlex();

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(frac_f32, (try env.getDescForRootVar(b)).content);
}

test "unify - compact frac and Dec Num(Frac(Dec))" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_dec = Content{ .structure = .{ .num = Num.frac_dec } };
    const a = env.types_store.freshFromContent(frac_dec);
    const b = env.mkFracExact(Num.Frac.Precision.dec);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(frac_dec, (try env.getDescForRootVar(b)).content);
}

test "unify - compact frac Dec and Num(Frac(F32)) (fails)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = env.types_store.freshFromContent(frac_f32);
    const b = env.mkFracExact(Num.Frac.Precision.dec);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - poly/poly rigid

test "unify - Num(rigid) and Num(rigid)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid = env.types_store.freshFromContent(env.mkRigidVar("b"));
    const num = Content{ .structure = .{ .num = .{ .num_poly = rigid } } };
    const a = env.types_store.freshFromContent(num);
    const b = env.types_store.freshFromContent(num);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(num, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(rigid_a) and Num(rigid_b)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid_a = env.types_store.freshFromContent(env.mkRigidVar("a"));
    const rigid_b = env.types_store.freshFromContent(env.mkRigidVar("b"));
    const a = env.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = rigid_a } } });
    const b = env.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .num_poly = rigid_b } } });

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Int(rigid)) and Num(Int(rigid))" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid = env.types_store.freshFromContent(env.mkRigidVar("b"));
    const int_var = env.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .int_poly = rigid } } });
    const num = Content{ .structure = .{ .num = .{ .num_poly = int_var } } };
    const a = env.types_store.freshFromContent(num);
    const b = env.types_store.freshFromContent(num);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(num, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(rigid)) and Num(Frac(rigid))" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid = env.types_store.freshFromContent(env.mkRigidVar("b"));
    const frac_var = env.types_store.freshFromContent(Content{ .structure = .{ .num = .{ .frac_poly = rigid } } });
    const num = Content{ .structure = .{ .num = .{ .num_poly = frac_var } } };
    const a = env.types_store.freshFromContent(num);
    const b = env.types_store.freshFromContent(num);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(num, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - compact/poly rigid

test "unify - compact int U8 and Num(Int(rigid)) (fails)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_u8 = Content{ .structure = .{ .num = Num.int_u8 } };
    const a = env.types_store.freshFromContent(int_u8);
    const b = env.mkFracRigid("a");

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - compact frac Dec and Num(Frac(rigid)) (fails)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = env.types_store.freshFromContent(frac_f32);
    const b = env.mkFracRigid("a");

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - poly/compact rigid

test "unify - Num(Int(rigid)) and compact int U8 (fails)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_u8 = Content{ .structure = .{ .num = Num.int_u8 } };
    const a = env.mkFracRigid("a");
    const b = env.types_store.freshFromContent(int_u8);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - Num(Frac(rigid)) and compact frac Dec (fails)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const frac_f32 = Content{ .structure = .{ .num = Num.frac_f32 } };
    const a = env.mkFracRigid("a");
    const b = env.types_store.freshFromContent(frac_f32);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - func

test "unify - func are same" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const num_flex = env.types_store.fresh();
    const num = env.types_store.freshFromContent(types.Content{ .structure = .{ .num = .{ .num_poly = num_flex } } });
    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const func = env.mkFuncFlex(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(func);
    const b = env.types_store.freshFromContent(func);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - funcs have diff return args (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const a = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{int_i32}, str));
    const b = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{str}, str));

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - funcs have diff return types (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const a = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{str}, int_i32));
    const b = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{str}, str));

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs pure" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const num_flex = env.types_store.fresh();
    const num = env.types_store.freshFromContent(types.Content{ .structure = .{ .num = .{ .num_poly = num_flex } } });
    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const func = env.mkFuncPure(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(func);
    const b = env.types_store.freshFromContent(func);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs effectful" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const num_flex = env.types_store.fresh();
    const num = env.types_store.freshFromContent(types.Content{ .structure = .{ .num = .{ .num_poly = num_flex } } });
    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const func = env.mkFuncEff(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(func);
    const b = env.types_store.freshFromContent(func);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first eff, second pure (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const num_flex = env.types_store.fresh();
    const num = env.types_store.freshFromContent(types.Content{ .structure = .{ .num = .{ .num_poly = num_flex } } });
    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const pure_func = env.mkFuncPure(&[_]Var{ str, num }, int_i32);
    const eff_func = env.mkFuncEff(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(eff_func);
    const b = env.types_store.freshFromContent(pure_func);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first pure, second eff" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i32 } });
    const num_flex = env.types_store.fresh();
    const num = env.types_store.freshFromContent(types.Content{ .structure = .{ .num = .{ .num_poly = num_flex } } });
    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const pure_func = env.mkFuncPure(&[_]Var{ str, num }, int_i32);
    const eff_func = env.mkFuncEff(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(pure_func);
    const b = env.types_store.freshFromContent(eff_func);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(eff_func, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - custom type

test "unify - a & b are both the same custom type" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const backing_var = env.types_store.freshFromContent(Content{ .structure = .str });
    const arg_var = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });
    const custom_type = env.mkCustomType("MyType", &[_]Var{arg_var}, backing_var);

    const a = env.types_store.freshFromContent(custom_type);
    const b = env.types_store.freshFromContent(custom_type);
    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(custom_type, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are diff custom types (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const backing_var = env.types_store.freshFromContent(Content{ .structure = .str });
    const arg_var = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const custom_type_a = env.mkCustomType("MyType", &[_]Var{arg_var}, backing_var);
    const a = env.types_store.freshFromContent(custom_type_a);

    const custom_type_b = env.mkCustomType("AnotherType", &[_]Var{arg_var}, backing_var);
    const b = env.types_store.freshFromContent(custom_type_b);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are both the same custom type with diff args (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const backing_var = env.types_store.freshFromContent(Content{ .structure = .str });
    const arg_var = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const custom_type_a = env.mkCustomType("MyType", &[_]Var{arg_var}, backing_var);
    const a = env.types_store.freshFromContent(custom_type_a);

    const custom_type_b = env.mkCustomType("MyType", &[_]Var{backing_var}, backing_var);
    const b = env.types_store.freshFromContent(custom_type_b);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - records - partition fields

test "partitionFields - same record" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const field_x = env.mkRecordField("field_x", @enumFromInt(0));
    const field_y = env.mkRecordField("field_y", @enumFromInt(1));

    const range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ field_x, field_y });

    const result = Unifier.partitionFields(&env.module_env.idents, &env.scratch, range, range);

    try std.testing.expectEqual(0, result.only_in_a.len());
    try std.testing.expectEqual(0, result.only_in_b.len());
    try std.testing.expectEqual(2, result.in_both.len());

    const both_slice = env.scratch.in_both_fields.rangeToSlice(result.in_both);
    try std.testing.expectEqual(field_x, both_slice[0].a);
    try std.testing.expectEqual(field_x, both_slice[0].b);
    try std.testing.expectEqual(field_y, both_slice[1].a);
    try std.testing.expectEqual(field_y, both_slice[1].b);
}

test "partitionFields - disjoint fields" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a1 = env.mkRecordField("a1", @enumFromInt(0));
    const a2 = env.mkRecordField("a2", @enumFromInt(1));
    const b1 = env.mkRecordField("b1", @enumFromInt(2));

    const a_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ a1, a2 });
    const b_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{b1});

    const result = Unifier.partitionFields(&env.module_env.idents, &env.scratch, a_range, b_range);

    try std.testing.expectEqual(2, result.only_in_a.len());
    try std.testing.expectEqual(1, result.only_in_b.len());
    try std.testing.expectEqual(0, result.in_both.len());

    const only_in_a_slice = env.scratch.only_in_a_fields.rangeToSlice(result.only_in_a);
    try std.testing.expectEqual(a1, only_in_a_slice[0]);
    try std.testing.expectEqual(a2, only_in_a_slice[1]);

    const only_in_b_slice = env.scratch.only_in_b_fields.rangeToSlice(result.only_in_b);
    try std.testing.expectEqual(b1, only_in_b_slice[0]);
}

test "partitionFields - overlapping fields" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a1 = env.mkRecordField("a1", @enumFromInt(0));
    const both = env.mkRecordField("both", @enumFromInt(1));
    const b1 = env.mkRecordField("b1", @enumFromInt(2));

    const a_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ a1, both });
    const b_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ b1, both });

    const result = Unifier.partitionFields(&env.module_env.idents, &env.scratch, a_range, b_range);

    try std.testing.expectEqual(1, result.only_in_a.len());
    try std.testing.expectEqual(1, result.only_in_b.len());
    try std.testing.expectEqual(1, result.in_both.len());

    const both_slice = env.scratch.in_both_fields.rangeToSlice(result.in_both);
    try std.testing.expectEqual(both, both_slice[0].a);
    try std.testing.expectEqual(both, both_slice[0].b);

    const only_in_a_slice = env.scratch.only_in_a_fields.rangeToSlice(result.only_in_a);
    try std.testing.expectEqual(a1, only_in_a_slice[0]);

    const only_in_b_slice = env.scratch.only_in_b_fields.rangeToSlice(result.only_in_b);
    try std.testing.expectEqual(b1, only_in_b_slice[0]);
}

test "partitionFields - reordering is normalized" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const f1 = env.mkRecordField("f1", @enumFromInt(0));
    const f2 = env.mkRecordField("f2", @enumFromInt(1));
    const f3 = env.mkRecordField("f3", @enumFromInt(2));

    const a_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ f3, f1, f2 });
    const b_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ f1, f2, f3 });

    const result = Unifier.partitionFields(&env.module_env.idents, &env.scratch, a_range, b_range);

    try std.testing.expectEqual(0, result.only_in_a.len());
    try std.testing.expectEqual(0, result.only_in_b.len());
    try std.testing.expectEqual(3, result.in_both.len());

    const both = env.scratch.in_both_fields.rangeToSlice(result.in_both);
    try std.testing.expectEqual(f1, both[0].a);
    try std.testing.expectEqual(f1, both[0].b);
    try std.testing.expectEqual(f2, both[1].a);
    try std.testing.expectEqual(f2, both[1].b);
    try std.testing.expectEqual(f3, both[2].a);
    try std.testing.expectEqual(f3, both[2].b);
}

// unification - structure/structure - records closed

test "unify - identical closed records" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const fields = [_]RecordField{env.mkRecordField("a", str)};
    const record_data = env.mkRecordClosed(&fields);
    const record_data_fields = env.types_store.record_fields.rangeToSlice(record_data.record.fields);

    const a = env.types_store.freshFromContent(record_data.content);
    const b = env.types_store.freshFromContent(record_data.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    const b_record_fields = env.types_store.record_fields.rangeToSlice(b_record.fields);
    try std.testing.expectEqualSlices(Ident.Idx, record_data_fields.items(.name), b_record_fields.items(.name));
    try std.testing.expectEqualSlices(Var, record_data_fields.items(.var_), b_record_fields.items(.var_));
}

test "unify - closed record mismatch on diff fields (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const field1 = env.mkRecordField("field1", str);
    const field2 = env.mkRecordField("field2", str);

    const a_record_data = env.mkRecordClosed(&[_]RecordField{ field1, field2 });
    const a = env.types_store.freshFromContent(a_record_data.content);

    const b_record_data = env.mkRecordClosed(&[_]RecordField{field1});
    const b = env.types_store.freshFromContent(b_record_data.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const desc_b = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc_b.content);
}

// unification - structure/structure - records open

test "unify - identical open records" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const field_shared = env.mkRecordField("x", str);

    const a_rec_data = env.mkRecordOpen(&[_]RecordField{field_shared});
    const a = env.types_store.freshFromContent(a_rec_data.content);
    const b_rec_data = env.mkRecordOpen(&[_]RecordField{field_shared});
    const b = env.types_store.freshFromContent(b_rec_data.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    // check that the update var at b is correct

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_record.fields.len());
    const b_record_fields = env.types_store.getRecordFieldsSlice(b_record.fields);
    try std.testing.expectEqual(field_shared.name, b_record_fields.items(.name)[0]);
    try std.testing.expectEqual(field_shared.var_, b_record_fields.items(.var_)[0]);

    const b_ext = env.types_store.resolveVar(b_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(0, env.scratch.fresh_vars.len());
}

test "unify - open record a extends b" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const field_shared = env.mkRecordField("x", str);
    const field_a_only = env.mkRecordField("y", int);

    const a_rec_data = env.mkRecordOpen(&[_]RecordField{ field_shared, field_a_only });
    const a = env.types_store.freshFromContent(a_rec_data.content);
    const b_rec_data = env.mkRecordOpen(&[_]RecordField{field_shared});
    const b = env.types_store.freshFromContent(b_rec_data.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    // check that the update var at b is correct

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_record.fields.len());
    const b_record_fields = env.types_store.getRecordFieldsSlice(b_record.fields);
    try std.testing.expectEqual(field_shared.name, b_record_fields.items(.name)[0]);
    try std.testing.expectEqual(field_shared.var_, b_record_fields.items(.var_)[0]);

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(env.scratch.fresh_vars.get(@enumFromInt(0)).*, b_record.ext);

    const b_ext_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(b_record.ext).desc);
    try std.testing.expectEqual(1, b_ext_record.fields.len());
    const b_ext_record_fields = env.types_store.getRecordFieldsSlice(b_ext_record.fields);
    try std.testing.expectEqual(field_a_only.name, b_ext_record_fields.items(.name)[0]);
    try std.testing.expectEqual(field_a_only.var_, b_ext_record_fields.items(.var_)[0]);

    const b_ext_ext = env.types_store.resolveVar(b_ext_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(b_record.ext, env.scratch.fresh_vars.items.items[0]);
}

test "unify - open record b extends a" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const field_shared = env.mkRecordField("field_shared", str);
    const field_b_only = env.mkRecordField("field_b_only", int);

    const a_rec_data = env.mkRecordOpen(&[_]RecordField{field_shared});
    const a = env.types_store.freshFromContent(a_rec_data.content);
    const b_rec_data = env.mkRecordOpen(&[_]RecordField{ field_shared, field_b_only });
    const b = env.types_store.freshFromContent(b_rec_data.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    // check that the update var at b is correct

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_record.fields.len());
    const b_record_fields = env.types_store.getRecordFieldsSlice(b_record.fields);
    try std.testing.expectEqual(field_shared.name, b_record_fields.items(.name)[0]);
    try std.testing.expectEqual(field_shared.var_, b_record_fields.items(.var_)[0]);

    const b_ext_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(b_record.ext).desc);
    try std.testing.expectEqual(1, b_ext_record.fields.len());
    const b_ext_record_fields = env.types_store.getRecordFieldsSlice(b_ext_record.fields);
    try std.testing.expectEqual(field_b_only.name, b_ext_record_fields.items(.name)[0]);
    try std.testing.expectEqual(field_b_only.var_, b_ext_record_fields.items(.var_)[0]);

    const b_ext_ext = env.types_store.resolveVar(b_ext_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(b_record.ext, env.scratch.fresh_vars.items.items[0]);
}

test "unify - both extend open record" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });
    const bool_ = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const field_shared = env.mkRecordField("x", str);
    const field_a_only = env.mkRecordField("y", int);
    const field_b_only = env.mkRecordField("z", bool_);

    const a_rec_data = env.mkRecordOpen(&[_]RecordField{ field_shared, field_a_only });
    const a = env.types_store.freshFromContent(a_rec_data.content);
    const b_rec_data = env.mkRecordOpen(&[_]RecordField{ field_shared, field_b_only });
    const b = env.types_store.freshFromContent(b_rec_data.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    // check that the update var at b is correct

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(3, b_record.fields.len());
    const b_record_fields = env.types_store.getRecordFieldsSlice(b_record.fields);
    try std.testing.expectEqual(field_shared, b_record_fields.get(0));
    try std.testing.expectEqual(field_a_only, b_record_fields.get(1));
    try std.testing.expectEqual(field_b_only, b_record_fields.get(2));

    const b_ext = env.types_store.resolveVar(b_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(3, env.scratch.fresh_vars.len());

    const only_a_var = env.scratch.fresh_vars.get(@enumFromInt(0)).*;
    const only_a_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(only_a_var).desc);
    try std.testing.expectEqual(1, only_a_record.fields.len());
    const only_a_record_fields = env.types_store.getRecordFieldsSlice(only_a_record.fields);
    try std.testing.expectEqual(field_a_only, only_a_record_fields.get(0));

    const only_b_var = env.scratch.fresh_vars.get(@enumFromInt(1)).*;
    const only_b_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(only_b_var).desc);
    try std.testing.expectEqual(1, only_b_record.fields.len());
    const only_b_record_fields = env.types_store.getRecordFieldsSlice(only_b_record.fields);
    try std.testing.expectEqual(field_b_only, only_b_record_fields.get(0));

    const ext_var = env.scratch.fresh_vars.get(@enumFromInt(2)).*;
    const ext_content = env.types_store.resolveVar(ext_var).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, ext_content);
}

test "unify - record mismatch on shared field (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const field_a = env.mkRecordField("x", str);
    const field_b = env.mkRecordField("x", int);

    const a_rec_data = env.mkRecordOpen(&[_]RecordField{field_a});
    const a = env.types_store.freshFromContent(a_rec_data.content);

    const b_rec_data = env.mkRecordOpen(&[_]RecordField{field_b});
    const b = env.types_store.freshFromContent(b_rec_data.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const desc_b = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc_b.content);
}

// unification - structure/structure - records open+closed

test "unify - open record extends closed (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const field_x = env.mkRecordField("field_x", str);
    const field_y = env.mkRecordField("field_y", str);

    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{ field_x, field_y }).content);
    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{field_x}).content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, open, closed);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.types_store.getSlot(open));
    try std.testing.expectEqual(Content.err, (try env.getDescForRootVar(closed)).content);
}

test "unify - closed record extends open" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const field_x = env.mkRecordField("field_x", str);
    const field_y = env.mkRecordField("field_y", str);

    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_x}).content);
    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{ field_x, field_y }).content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, open, closed);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.types_store.getSlot(open));
}

test "unify - open vs closed records with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const field_x_str = env.mkRecordField("field_x_str", str);
    const field_x_int = env.mkRecordField("field_x_int", int);

    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_x_str}).content);
    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{field_x_int}).content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, open, closed);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.types_store.getSlot(open));

    const desc = try env.getDescForRootVar(closed);
    try std.testing.expectEqual(Content.err, desc.content);
}

test "unify - closed vs open records with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const field_x_str = env.mkRecordField("field_x_str", str);
    const field_x_int = env.mkRecordField("field_x_int", int);

    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{field_x_int}).content);
    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_x_str}).content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, closed, open);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = open }, env.types_store.getSlot(closed));

    const desc = try env.getDescForRootVar(open);
    try std.testing.expectEqual(Content.err, desc.content);
}

// unification - tag unions - partition tags

test "partitionTags - same tags" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const tag_x = env.mkTag("X", &[_]Var{@enumFromInt(0)});
    const tag_y = env.mkTag("Y", &[_]Var{@enumFromInt(1)});

    const range = env.scratch.appendSliceGatheredTags(&[_]Tag{ tag_x, tag_y });

    const result = Unifier.partitionTags(&env.module_env.idents, &env.scratch, range, range);

    try std.testing.expectEqual(0, result.only_in_a.len());
    try std.testing.expectEqual(0, result.only_in_b.len());
    try std.testing.expectEqual(2, result.in_both.len());

    const both_slice = env.scratch.in_both_tags.rangeToSlice(result.in_both);
    try std.testing.expectEqual(tag_x, both_slice[0].a);
    try std.testing.expectEqual(tag_x, both_slice[0].b);
    try std.testing.expectEqual(tag_y, both_slice[1].a);
    try std.testing.expectEqual(tag_y, both_slice[1].b);
}

test "partitionTags - disjoint fields" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a1 = env.mkTag("A1", &[_]Var{@enumFromInt(0)});
    const a2 = env.mkTag("A2", &[_]Var{@enumFromInt(1)});
    const b1 = env.mkTag("B1", &[_]Var{@enumFromInt(2)});

    const a_range = env.scratch.appendSliceGatheredTags(&[_]Tag{ a1, a2 });
    const b_range = env.scratch.appendSliceGatheredTags(&[_]Tag{b1});

    const result = Unifier.partitionTags(&env.module_env.idents, &env.scratch, a_range, b_range);

    try std.testing.expectEqual(2, result.only_in_a.len());
    try std.testing.expectEqual(1, result.only_in_b.len());
    try std.testing.expectEqual(0, result.in_both.len());

    const only_in_a_slice = env.scratch.only_in_a_tags.rangeToSlice(result.only_in_a);
    try std.testing.expectEqual(a1, only_in_a_slice[0]);
    try std.testing.expectEqual(a2, only_in_a_slice[1]);

    const only_in_b_slice = env.scratch.only_in_b_tags.rangeToSlice(result.only_in_b);
    try std.testing.expectEqual(b1, only_in_b_slice[0]);
}

test "partitionTags - overlapping tags" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a1 = env.mkTag("A", &[_]Var{@enumFromInt(0)});
    const both = env.mkTag("Both", &[_]Var{@enumFromInt(1)});
    const b1 = env.mkTag("B", &[_]Var{@enumFromInt(2)});

    const a_range = env.scratch.appendSliceGatheredTags(&[_]Tag{ a1, both });
    const b_range = env.scratch.appendSliceGatheredTags(&[_]Tag{ b1, both });

    const result = Unifier.partitionTags(&env.module_env.idents, &env.scratch, a_range, b_range);

    try std.testing.expectEqual(1, result.only_in_a.len());
    try std.testing.expectEqual(1, result.only_in_b.len());
    try std.testing.expectEqual(1, result.in_both.len());

    const both_slice = env.scratch.in_both_tags.rangeToSlice(result.in_both);
    try std.testing.expectEqual(both, both_slice[0].a);
    try std.testing.expectEqual(both, both_slice[0].b);

    const only_in_a_slice = env.scratch.only_in_a_tags.rangeToSlice(result.only_in_a);
    try std.testing.expectEqual(a1, only_in_a_slice[0]);

    const only_in_b_slice = env.scratch.only_in_b_tags.rangeToSlice(result.only_in_b);
    try std.testing.expectEqual(b1, only_in_b_slice[0]);
}

test "partitionTags - reordering is normalized" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const f1 = env.mkTag("F1", &[_]Var{@enumFromInt(0)});
    const f2 = env.mkTag("F2", &[_]Var{@enumFromInt(1)});
    const f3 = env.mkTag("F3", &[_]Var{@enumFromInt(2)});

    const a_range = env.scratch.appendSliceGatheredTags(&[_]Tag{ f3, f1, f2 });
    const b_range = env.scratch.appendSliceGatheredTags(&[_]Tag{ f1, f2, f3 });

    const result = Unifier.partitionTags(&env.module_env.idents, &env.scratch, a_range, b_range);

    try std.testing.expectEqual(0, result.only_in_a.len());
    try std.testing.expectEqual(0, result.only_in_b.len());
    try std.testing.expectEqual(3, result.in_both.len());

    const both_slice = env.scratch.in_both_tags.rangeToSlice(result.in_both);
    try std.testing.expectEqual(f1, both_slice[0].a);
    try std.testing.expectEqual(f1, both_slice[0].b);
    try std.testing.expectEqual(f2, both_slice[1].a);
    try std.testing.expectEqual(f2, both_slice[1].b);
    try std.testing.expectEqual(f3, both_slice[2].a);
    try std.testing.expectEqual(f3, both_slice[2].b);
}

// unification - structure/structure - tag unions closed

test "unify - identical closed tag_unions" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const tag = env.mkTag("A", &[_]Var{str});
    const tags = [_]Tag{tag};
    const tag_union_data = env.mkTagUnionClosed(&tags);

    const a = env.types_store.freshFromContent(tag_union_data.content);
    const b = env.types_store.freshFromContent(tag_union_data.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    const b_tags = env.types_store.tags.rangeToSlice(b_tag_union.tags);
    const b_tags_names = b_tags.items(.name);
    const b_tags_args = b_tags.items(.args);
    try std.testing.expectEqual(1, b_tags.len);
    try std.testing.expectEqual(tag.name, b_tags_names[0]);
    try std.testing.expectEqual(tag.args, b_tags_args[0]);

    try std.testing.expectEqual(1, b_tags.len);

    const b_tag_args = env.types_store.tag_args.rangeToSlice(b_tags_args[0]);
    try std.testing.expectEqual(1, b_tag_args.len);
    try std.testing.expectEqual(str, b_tag_args[0]);
}

test "unify - closed tag_unions with diff args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const a_tag = env.mkTag("A", &[_]Var{str});
    const a_tags = [_]Tag{a_tag};
    const a_tag_union_data = env.mkTagUnionClosed(&a_tags);
    const a = env.types_store.freshFromContent(a_tag_union_data.content);

    const b_tag = env.mkTag("A", &[_]Var{int});
    const b_tags = [_]Tag{b_tag};
    const b_tag_union_data = env.mkTagUnionClosed(&b_tags);
    const b = env.types_store.freshFromContent(b_tag_union_data.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const desc = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc.content);
}

// unification - structure/structure - tag unions open

test "unify - identical open tag unions" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const tag_shared = env.mkTag("Shared", &[_]Var{ str, str });

    const tag_union_a = env.mkTagUnionOpen(&[_]Tag{tag_shared});
    const a = env.types_store.freshFromContent(tag_union_a.content);

    const tag_union_b = env.mkTagUnionOpen(&[_]Tag{tag_shared});
    const b = env.types_store.freshFromContent(tag_union_b.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    // check that the update var at b is correct

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_tag_union.tags.len());

    const b_tags = env.types_store.tags.rangeToSlice(b_tag_union.tags);
    const b_tags_names = b_tags.items(.name);
    const b_tags_args = b_tags.items(.args);
    try std.testing.expectEqual(1, b_tags.len);
    try std.testing.expectEqual(tag_shared.name, b_tags_names[0]);
    try std.testing.expectEqual(tag_shared.args, b_tags_args[0]);

    const b_ext = env.types_store.resolveVar(b_tag_union.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(0, env.scratch.fresh_vars.len());
}

test "unify - open tag union a extends b" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const tag_a_only = env.mkTag("A", &[_]Var{str});
    const tag_shared = env.mkTag("Shared", &[_]Var{ int, int });

    const tag_union_a = env.mkTagUnionOpen(&[_]Tag{ tag_a_only, tag_shared });
    const a = env.types_store.freshFromContent(tag_union_a.content);

    const tag_union_b = env.mkTagUnionOpen(&[_]Tag{tag_shared});
    const b = env.types_store.freshFromContent(tag_union_b.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    // check that the update var at b is correct

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_tag_union.tags.len());

    const b_tags = env.types_store.tags.rangeToSlice(b_tag_union.tags);
    const b_tags_names = b_tags.items(.name);
    const b_tags_args = b_tags.items(.args);
    try std.testing.expectEqual(1, b_tags.len);
    try std.testing.expectEqual(tag_shared.name, b_tags_names[0]);
    try std.testing.expectEqual(tag_shared.args, b_tags_args[0]);

    const b_ext_tag_union = try TestEnv.getTagUnionOrErr(env.types_store.resolveVar(b_tag_union.ext).desc);
    try std.testing.expectEqual(1, b_ext_tag_union.tags.len());

    const b_ext_tags = env.types_store.tags.rangeToSlice(b_ext_tag_union.tags);
    const b_ext_tags_names = b_ext_tags.items(.name);
    const b_ext_tags_args = b_ext_tags.items(.args);
    try std.testing.expectEqual(1, b_ext_tags.len);
    try std.testing.expectEqual(tag_a_only.name, b_ext_tags_names[0]);
    try std.testing.expectEqual(tag_a_only.args, b_ext_tags_args[0]);

    const b_ext_ext = env.types_store.resolveVar(b_ext_tag_union.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(b_tag_union.ext, env.scratch.fresh_vars.items.items[0]);
}

test "unify - open tag union b extends a" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const tag_b_only = env.mkTag("A", &[_]Var{ str, int });
    const tag_shared = env.mkTag("Shared", &[_]Var{int});

    const tag_union_a = env.mkTagUnionOpen(&[_]Tag{tag_shared});
    const a = env.types_store.freshFromContent(tag_union_a.content);

    const tag_union_b = env.mkTagUnionOpen(&[_]Tag{ tag_b_only, tag_shared });
    const b = env.types_store.freshFromContent(tag_union_b.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    // check that the update var at b is correct

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_tag_union.tags.len());

    const b_tags = env.types_store.tags.rangeToSlice(b_tag_union.tags);
    const b_tags_names = b_tags.items(.name);
    const b_tags_args = b_tags.items(.args);
    try std.testing.expectEqual(1, b_tags.len);
    try std.testing.expectEqual(tag_shared.name, b_tags_names[0]);
    try std.testing.expectEqual(tag_shared.args, b_tags_args[0]);

    const b_ext_tag_union = try TestEnv.getTagUnionOrErr(env.types_store.resolveVar(b_tag_union.ext).desc);
    try std.testing.expectEqual(1, b_ext_tag_union.tags.len());

    const b_ext_tags = env.types_store.tags.rangeToSlice(b_ext_tag_union.tags);
    const b_ext_tags_names = b_ext_tags.items(.name);
    const b_ext_tags_args = b_ext_tags.items(.args);
    try std.testing.expectEqual(1, b_ext_tags.len);
    try std.testing.expectEqual(tag_b_only.name, b_ext_tags_names[0]);
    try std.testing.expectEqual(tag_b_only.args, b_ext_tags_args[0]);

    const b_ext_ext = env.types_store.resolveVar(b_ext_tag_union.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(b_tag_union.ext, env.scratch.fresh_vars.items.items[0]);
}

test "unify - both extend open tag union" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });
    const bool_ = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const tag_a_only = env.mkTag("A", &[_]Var{bool_});
    const tag_b_only = env.mkTag("B", &[_]Var{ str, int });
    const tag_shared = env.mkTag("Shared", &[_]Var{int});

    const tag_union_a = env.mkTagUnionOpen(&[_]Tag{ tag_a_only, tag_shared });
    const a = env.types_store.freshFromContent(tag_union_a.content);

    const tag_union_b = env.mkTagUnionOpen(&[_]Tag{ tag_b_only, tag_shared });
    const b = env.types_store.freshFromContent(tag_union_b.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    // check that the update var at b is correct

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(3, b_tag_union.tags.len());

    const b_tags = env.types_store.tags.rangeToSlice(b_tag_union.tags);
    try std.testing.expectEqual(3, b_tags.len);
    try std.testing.expectEqual(tag_shared, b_tags.get(0));
    try std.testing.expectEqual(tag_a_only, b_tags.get(1));
    try std.testing.expectEqual(tag_b_only, b_tags.get(2));

    const b_ext = env.types_store.resolveVar(b_tag_union.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(3, env.scratch.fresh_vars.len());

    const only_a_var = env.scratch.fresh_vars.get(@enumFromInt(0)).*;
    const only_a_tag_union = try TestEnv.getTagUnionOrErr(env.types_store.resolveVar(only_a_var).desc);
    try std.testing.expectEqual(1, only_a_tag_union.tags.len());
    const only_a_tags = env.types_store.getTagsSlice(only_a_tag_union.tags);
    try std.testing.expectEqual(tag_a_only, only_a_tags.get(0));

    const only_b_var = env.scratch.fresh_vars.get(@enumFromInt(1)).*;
    const only_b_tag_union = try TestEnv.getTagUnionOrErr(env.types_store.resolveVar(only_b_var).desc);
    try std.testing.expectEqual(1, only_b_tag_union.tags.len());
    const only_b_tags = env.types_store.getTagsSlice(only_b_tag_union.tags);
    try std.testing.expectEqual(tag_b_only, only_b_tags.get(0));

    const ext_var = env.scratch.fresh_vars.get(@enumFromInt(2)).*;
    const ext_content = env.types_store.resolveVar(ext_var).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, ext_content);
}

test "unify - open tag unions a & b have same tag name with diff args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const int = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_u8 } });

    const tag_a_only = env.mkTag("A", &[_]Var{str});
    const tag_shared = env.mkTag("A", &[_]Var{ int, int });

    const tag_union_a = env.mkTagUnionOpen(&[_]Tag{ tag_a_only, tag_shared });
    const a = env.types_store.freshFromContent(tag_union_a.content);

    const tag_union_b = env.mkTagUnionOpen(&[_]Tag{tag_shared});
    const b = env.types_store.freshFromContent(tag_union_b.content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const desc = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc.content);
}

// unification - structure/structure - records open+closed

test "unify - open tag extends closed (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const tag_shared = env.mkTag("Shared", &[_]Var{str});
    const tag_a_only = env.mkTag("A", &[_]Var{str});

    const a = env.types_store.freshFromContent(env.mkTagUnionOpen(&[_]Tag{ tag_shared, tag_a_only }).content);
    const b = env.types_store.freshFromContent(env.mkTagUnionClosed(&[_]Tag{tag_shared}).content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(Content.err, (try env.getDescForRootVar(b)).content);
}

test "unify - closed tag union extends open" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });

    const tag_shared = env.mkTag("Shared", &[_]Var{str});
    const tag_b_only = env.mkTag("B", &[_]Var{str});

    const a = env.types_store.freshFromContent(env.mkTagUnionOpen(&[_]Tag{tag_shared}).content);
    const b = env.types_store.freshFromContent(env.mkTagUnionClosed(&[_]Tag{ tag_shared, tag_b_only }).content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    // check that the update var at b is correct

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_tag_union.tags.len());

    const b_tags = env.types_store.tags.rangeToSlice(b_tag_union.tags);
    const b_tags_names = b_tags.items(.name);
    const b_tags_args = b_tags.items(.args);
    try std.testing.expectEqual(1, b_tags.len);
    try std.testing.expectEqual(tag_shared.name, b_tags_names[0]);
    try std.testing.expectEqual(tag_shared.args, b_tags_args[0]);

    const b_ext_tag_union = try TestEnv.getTagUnionOrErr(env.types_store.resolveVar(b_tag_union.ext).desc);
    try std.testing.expectEqual(1, b_ext_tag_union.tags.len());

    const b_ext_tags = env.types_store.tags.rangeToSlice(b_ext_tag_union.tags);
    const b_ext_tags_names = b_ext_tags.items(.name);
    const b_ext_tags_args = b_ext_tags.items(.args);
    try std.testing.expectEqual(1, b_ext_tags.len);
    try std.testing.expectEqual(tag_b_only.name, b_ext_tags_names[0]);
    try std.testing.expectEqual(tag_b_only.args, b_ext_tags_args[0]);

    const b_ext_ext = env.types_store.resolveVar(b_ext_tag_union.ext).desc.content;
    try std.testing.expectEqual(Content{ .structure = .empty_tag_union }, b_ext_ext);

    // check that fresh vars are correct

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(b_tag_union.ext, env.scratch.fresh_vars.items.items[0]);
}

test "unify - open vs closed tag union with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const bool_ = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const tag_a = env.mkTag("A", &[_]Var{str});
    const tag_b = env.mkTag("A", &[_]Var{bool_});

    const a = env.types_store.freshFromContent(env.mkTagUnionOpen(&[_]Tag{tag_a}).content);
    const b = env.types_store.freshFromContent(env.mkTagUnionClosed(&[_]Tag{tag_b}).content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const desc = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc.content);
}

test "unify - closed vs open tag union with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(Content{ .structure = .str });
    const bool_ = env.types_store.freshFromContent(Content{ .structure = .{ .num = Num.int_i8 } });

    const tag_a = env.mkTag("A", &[_]Var{str});
    const tag_b = env.mkTag("B", &[_]Var{bool_});

    const a = env.types_store.freshFromContent(env.mkTagUnionClosed(&[_]Tag{tag_a}).content);
    const b = env.types_store.freshFromContent(env.mkTagUnionOpen(&[_]Tag{tag_b}).content);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const desc = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc.content);
}

// unification - recursion

test "unify - fails on infinite type" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str_var = env.types_store.freshFromContent(Content{ .structure = .str });

    const a = env.types_store.fresh();
    const a_elems_range = env.types_store.appendTupleElems(&[_]Var{ a, str_var });
    const a_tuple = types.Tuple{ .elems = a_elems_range };
    try env.types_store.setRootVarContent(a, Content{ .structure = .{ .tuple = a_tuple } });

    const b = env.types_store.fresh();
    const b_elems_range = env.types_store.appendTupleElems(&[_]Var{ b, str_var });
    const b_tuple = types.Tuple{ .elems = b_elems_range };
    try env.types_store.setRootVarContent(b, Content{ .structure = .{ .tuple = b_tuple } });

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a, b);

    try std.testing.expectEqual(Result{ .infinite_recursion = .a }, result);
}

test "unify - fails on anonymous recursion" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const list_var_a = env.types_store.fresh();
    const list_content_a = Content{
        .structure = .{ .list = list_var_a },
    };
    try env.types_store.setRootVarContent(list_var_a, list_content_a);

    const list_var_b = env.types_store.fresh();
    const list_content_b = Content{
        .structure = .{ .list = list_var_b },
    };
    try env.types_store.setRootVarContent(list_var_b, list_content_b);

    const result = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, list_var_a, list_var_b);

    try std.testing.expectEqual(Result{ .anonmyous_recursion = .a }, result);
}

test "unify - succeeds on nomminal, tag union recursion" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_custom_type_var = env.types_store.fresh();
    const a_elem_var = env.types_store.fresh();
    const a_nil_tag = env.mkTag("Nil", &[_]Var{});
    const a_cons_tag = env.mkTag("Cons", &[_]Var{ a_elem_var, a_custom_type_var });
    const a_tag_union_var = env.types_store.freshFromContent(env.mkTagUnionOpen(&[_]Tag{ a_nil_tag, a_cons_tag }).content);
    const a_custom_type = env.mkCustomType("List", &[_]Var{}, a_tag_union_var);
    try env.types_store.setRootVarContent(a_custom_type_var, a_custom_type);

    const b_custom_type_var = env.types_store.fresh();
    const b_elem_var = env.types_store.fresh();
    const b_nil_tag = env.mkTag("Nil", &[_]Var{});
    const b_cons_tag = env.mkTag("Cons", &[_]Var{ b_elem_var, b_custom_type_var });
    const b_tag_union_var = env.types_store.freshFromContent(env.mkTagUnionOpen(&[_]Tag{ b_nil_tag, b_cons_tag }).content);
    const b_custom_type = env.mkCustomType("List", &[_]Var{}, b_tag_union_var);
    try env.types_store.setRootVarContent(b_custom_type_var, b_custom_type);

    const result_custom_type = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a_custom_type_var, b_custom_type_var);
    try std.testing.expectEqual(.ok, result_custom_type);

    const result_tag_union = unify(env.module_env, env.types_store, &env.scratch, &env.occurs_scratch, a_tag_union_var, b_tag_union_var);
    try std.testing.expectEqual(.ok, result_tag_union);
}
