//! This module implements Hindley-Milner style type unification with extensions for:
//! * structural and opaque type aliases
//! * effect and purity tracking
//! * extensible row-based record types
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
//! Example:
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
//! After use, the scratch buffer should either be deinited or reused in a subsequent
//! unification run.

const std = @import("std");

const collections = @import("../collections.zig");
const Region = @import("../base/Region.zig");
const Ident = @import("../base/Ident.zig");
const store = @import("./store.zig");
const types = @import("./types.zig");

const exitOnOutOfMemory = collections.utils.exitOnOom;
const SmallStringInterner = collections.SmallStringInterner;

const Slot = store.Slot;
const ResolvedVarDesc = store.ResolvedVarDesc;
const ResolvedVarDescs = store.ResolvedVarDescs;

const TypeIdent = types.TypeIdent;
const Var = types.Var;
const Desc = types.Descriptor;
const Rank = types.Rank;
const Content = types.Content;
const Alias = types.Alias;
const FlatType = types.FlatType;
const TypeApply = types.TypeApply;
const Tuple = types.Tuple;
const Num = types.Num;
const Func = types.Func;
const Record = types.Record;
const RecordField = types.RecordField;
const TwoRecordFields = types.TwoRecordFields;
const TagUnion = types.TagUnion;
const Tag = types.Tag;
const TwoTags = types.TwoTags;

const VarSafeList = types.VarSafeList;
const RecordFieldSafeList = types.RecordFieldSafeList;
const TwoRecordFieldsSafeList = types.TwoRecordFieldsSafeList;
const TagSafeList = types.TagSafeList;
const TwoTagsSafeList = types.TwoTagsSafeList;

/// Unify two type variables
///
/// This function
/// * Resolve type variables & compresses paths
/// * Compares variable contents for equality
/// * Merges unified variables so 1 is "root" and the other is "redirect"
pub fn unify(
    types_store: *store.Store,
    scratch: *Scratch,
    a: Var,
    b: Var,
) Result {
    // First reset the scratch store
    scratch.reset();

    // Unify
    var unifier = Unifier.init(types_store, scratch);
    unifier.unifyGuarded(a, b) catch |err| switch (err) {
        error.TypeMismatch => {
            types_store.union_(a, b, .{
                .content = .err,
                .rank = Rank.generalized,
            });
            return Result{ .err = .{ .a = a, .b = b } };
        },
    };
    return .ok;
}

/// The result of unification
pub const Result = union(enum) {
    const Self = @This();

    ok,
    err: Err,

    /// The error types that occurred during unification
    /// TODO: Make these full error types
    pub const Err = struct { a: Var, b: Var };

    pub fn isOk(self: Self) bool {
        return self == .ok;
    }
};

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
/// * structural and opaque type aliases
/// * extensible records with row polymorphism
/// * basic support for function, tuple, and number types
///
/// Callers are not expected to construct `Unifier`. Instead  call `unify(...)`.
const Unifier = struct {
    const Self = @This();

    types_store: *store.Store,
    scratch: *Scratch,

    /// Init a unifier
    /// Caller owns the memory of the provided values
    fn init(types_store: *store.Store, scratch: *Scratch) Self {
        return .{ .types_store = types_store, .scratch = scratch };
    }

    // merge

    /// Link the variables & updated the content in the type_store
    /// In the old compiler, this function was called "merge"
    fn merge(self: *Self, vars: *const ResolvedVarDescs, new_content: Content) void {
        self.types_store.union_(vars.a.var_, vars.b.var_, .{
            .content = new_content,
            .rank = Rank.min(vars.a.desc.rank, vars.b.desc.rank),
        });
    }

    /// Create a new type variable *in this pool*
    fn fresh(self: *Self, vars: *const ResolvedVarDescs, new_content: Content) Var {
        const var_ = self.types_store.register(.{
            .content = new_content,
            .rank = Rank.min(vars.a.desc.rank, vars.b.desc.rank),
        });
        _ = self.scratch.fresh_vars.append(self.scratch.gpa, var_);
        return var_;
    }

    // unification

    /// Error thrown during unification when there's a type mismatch
    const Error = error{ TypeMismatch, InvalidRecordExt };

    /// Unify checking for equivalance
    fn unifyGuarded(self: *Self, a_var: Var, b_var: Var) error{TypeMismatch}!void {
        switch (self.types_store.checkVarsEquiv(a_var, b_var)) {
            .equiv => {
                // this means that the vars are identitcal, so nothing needs to happen
            },
            .not_equiv => |vars| {
                switch (vars.a.desc.content) {
                    .flex_var => |mb_a_ident| {
                        self.unifyFlex(&vars, mb_a_ident, vars.b.desc.content);
                    },
                    .rigid_var => |_| {
                        try self.unifyRigid(&vars, vars.b.desc.content);
                    },
                    .alias => |a_alias| {
                        switch (a_alias.type) {
                            .structural => {
                                try self.unifyStructuralAlias(&vars, a_alias, vars.b.desc.content);
                            },
                            .opaque_ => {
                                try self.unifyOpaqueAlias(&vars, a_alias, vars.b.desc.content);
                            },
                        }
                    },
                    .effectful => {
                        try self.unifyEffectful(&vars, vars.b.desc.content);
                    },
                    .pure => {
                        try self.unifyPure(&vars, vars.b.desc.content);
                    },
                    .structure => |a_flat_type| {
                        try self.unifyStructure(&vars, a_flat_type, vars.b.desc.content);
                    },
                    .err => return error.TypeMismatch,
                }
            },
        }
    }

    // Unify flex //

    /// Unify when `a` was a flex
    fn unifyFlex(self: *Self, vars: *const ResolvedVarDescs, mb_a_ident: ?SmallStringInterner.Idx, b_content: Content) void {
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
    fn unifyRigid(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) error{TypeMismatch}!void {
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

    // Unify structural alias //

    /// Unify when `a` was a structural alias
    fn unifyStructuralAlias(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_content: Content) error{TypeMismatch}!void {
        switch (b_content) {
            .flex_var => |_| {
                self.merge(vars, Content{ .alias = a_alias });
            },
            .rigid_var => |_| {
                try self.unifyGuarded(a_alias.backing_var, vars.b.var_);
            },
            .alias => |b_alias| {
                switch (b_alias.type) {
                    .structural => {
                        if (TypeIdent.eql(a_alias.ident, b_alias.ident)) {
                            try self.unifyTwoAliases(vars, a_alias, b_alias);
                        } else {
                            try self.unifyGuarded(a_alias.backing_var, b_alias.backing_var);
                        }
                    },
                    .opaque_ => {
                        try self.unifyGuarded(a_alias.backing_var, vars.b.var_);
                    },
                }
            },
            .effectful => return error.TypeMismatch,
            .pure => return error.TypeMismatch,
            .structure => try self.unifyGuarded(a_alias.backing_var, vars.b.var_),
            .err => self.merge(vars, .err),
        }
    }

    /// Unify when `a` was a structural alias
    fn unifyOpaqueAlias(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_content: Content) error{TypeMismatch}!void {
        switch (b_content) {
            .flex_var => |_| {
                self.merge(vars, Content{ .alias = a_alias });
            },
            .rigid_var => |_| {
                try self.unifyGuarded(a_alias.backing_var, vars.b.var_);
            },
            .alias => |b_alias| {
                switch (b_alias.type) {
                    .structural => {
                        try self.unifyGuarded(vars.a.var_, b_alias.backing_var);
                    },
                    .opaque_ => {
                        if (TypeIdent.eql(a_alias.ident, b_alias.ident)) {
                            try self.unifyTwoAliases(vars, a_alias, b_alias);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                }
            },
            .effectful => return error.TypeMismatch,
            .pure => return error.TypeMismatch,
            .structure => return error.TypeMismatch,
            .err => self.merge(vars, .err),
        }
    }

    /// Unify two aliases, either structural or opaque
    ///
    /// This function assumes the caller has already checked that the alias names match
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    ///
    /// NOTE: the rust version of this function `unify_two_aliases` is *signifgantly* more
    /// complicated than the version here
    fn unifyTwoAliases(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_alias: Alias) error{TypeMismatch}!void {
        if (a_alias.args.len() != b_alias.args.len()) {
            return error.TypeMismatch;
        }

        const a_args = self.types_store.alias_args.rangeToSlice(a_alias.args);
        const b_args = self.types_store.alias_args.rangeToSlice(b_alias.args);
        for (a_args, b_args) |a_arg, b_arg| {
            try self.unifyGuarded(a_arg, b_arg);
        }

        // TODO: Here, we'll need some special handling for recursion variables
        // For each argument variable, we need to prefer recursive vars. Then
        // we'll build a new structurla alias and pass that to self.merge
        // See unify_two_aliases and choose_merged_var in the rust compiler for details

        // Rust compiler comment:
        // Don't report real_var mismatches, because they must always be surfaced higher, from the argument types.
        self.unifyGuarded(a_alias.backing_var, b_alias.backing_var) catch {};

        self.merge(vars, vars.b.desc.content);
    }

    // Unify effectful //

    /// Unify when `a` was a effectful
    fn unifyEffectful(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) error{TypeMismatch}!void {
        switch (b_content) {
            .flex_var => self.merge(vars, .effectful),
            .effectful => self.merge(vars, .effectful),
            .err => self.merge(vars, .err),
            else => return error.TypeMismatch,
        }
    }

    // Unify pure //

    /// Unify when `a` was a pure
    fn unifyPure(self: *Self, vars: *const ResolvedVarDescs, b_content: Content) error{TypeMismatch}!void {
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
    ) error{TypeMismatch}!void {
        switch (b_content) {
            .flex_var => |_| {
                self.merge(vars, Content{ .structure = a_flat_type });
            },
            .rigid_var => return error.TypeMismatch,
            .alias => |alias| {
                switch (alias.type) {
                    .structural => {
                        try self.unifyGuarded(vars.a.var_, alias.backing_var);
                    },
                    .opaque_ => |_| return error.TypeMismatch,
                }
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
    ) error{TypeMismatch}!void {
        switch (a_flat_type) {
            .type_apply => |a_type_apply| {
                switch (b_flat_type) {
                    .type_apply => |b_type_apply| {
                        try self.unifyTypeApply(vars, a_type_apply, b_type_apply);
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
                        if (a_record.fields.len() == 0 or // this line is here for explicitness
                            a_record.areAllFieldsOptional(&self.types_store.record_fields))
                        {
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
                        if (b_record.fields.len() == 0 or // this line is here for explicitness
                            b_record.areAllFieldsOptional(&self.types_store.record_fields))
                        {
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

    /// unify type application (both with args like 'List Str' and without like 'Bool')
    ///
    /// this checks:
    /// * that the type names match
    /// * that the arities are the same
    /// * that parallel arguments unify
    fn unifyTypeApply(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_type_apply: TypeApply,
        b_type_apply: TypeApply,
    ) error{TypeMismatch}!void {
        if (!TypeIdent.eql(a_type_apply.ident, b_type_apply.ident)) {
            return error.TypeMismatch;
        }
        if (a_type_apply.args.len() != b_type_apply.args.len()) {
            return error.TypeMismatch;
        }

        const a_args = self.types_store.type_apply_args.rangeToSlice(a_type_apply.args);
        const b_args = self.types_store.type_apply_args.rangeToSlice(b_type_apply.args);
        for (a_args, b_args) |a_arg, b_arg| {
            try self.unifyGuarded(a_arg, b_arg);
        }

        // TODO: Here, we'll need some special handling for recursion variables
        // For each argument variable, we need to prefer recursive vars. Then
        // we'll build a new flat_type.type_apply with those aruguments and pass
        // that to self.merge
        // See unify_flat_type and choose_merged_var in the rust compiler for details

        self.merge(vars, vars.b.desc.content);
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
    ) error{TypeMismatch}!void {
        if (a_tuple.elems.len() != b_tuple.elems.len()) {
            return error.TypeMismatch;
        }

        const a_elems = self.types_store.tuple_elems.rangeToSlice(a_tuple.elems);
        const b_elems = self.types_store.tuple_elems.rangeToSlice(b_tuple.elems);
        for (a_elems, b_elems) |a_elem, b_elem| {
            try self.unifyGuarded(a_elem, b_elem);
        }

        self.merge(vars, vars.b.desc.content);
    }

    /// unify numbers
    ///
    /// this checks:
    /// * that the arities are the same
    /// * that parallel arguments unify
    fn unifyNum(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_num: Num,
        b_num: Num,
    ) error{TypeMismatch}!void {
        switch (a_num) {
            .flex_var => {
                // if a is flex, update to b's desc
                self.merge(vars, vars.b.desc.content);
            },
            .int => |a_int| switch (b_num) {
                .flex_var => {
                    // if b is flex, update to a's desc
                    self.merge(vars, vars.a.desc.content);
                },
                .frac => return error.TypeMismatch,
                .int => |b_int| {
                    if (a_int == .flex_var) {
                        // if a is flex, update to b's desc
                        self.merge(vars, vars.b.desc.content);
                    } else if (b_int == .flex_var) {
                        // if b is flex, update to a's desc
                        self.merge(vars, vars.a.desc.content);
                    } else if (a_int == b_int) {
                        self.merge(vars, vars.b.desc.content);
                    } else {
                        return error.TypeMismatch;
                    }
                },
            },

            .frac => |a_frac| switch (b_num) {
                .flex_var => {
                    // if b is flex, update to a's desc
                    self.merge(vars, vars.a.desc.content);
                },
                .int => return error.TypeMismatch,
                .frac => |b_frac| {
                    if (a_frac == .flex_var) {
                        // if a is flex, update to b's desc
                        self.merge(vars, vars.b.desc.content);
                    } else if (b_frac == .flex_var) {
                        // if b is flex, update to a's desc
                        self.merge(vars, vars.a.desc.content);
                    } else if (a_frac == b_frac) {
                        self.merge(vars, vars.b.desc.content);
                    } else {
                        return error.TypeMismatch;
                    }
                },
            },
        }
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
    ) error{TypeMismatch}!void {
        if (a_func.args.len() != b_func.args.len()) {
            return error.TypeMismatch;
        }

        const a_args = self.types_store.func_args.rangeToSlice(a_func.args);
        const b_args = self.types_store.func_args.rangeToSlice(b_func.args);
        for (a_args, b_args) |a_arg, b_arg| {
            try self.unifyGuarded(a_arg, b_arg);
        }

        try self.unifyGuarded(a_func.ret, b_func.ret);
        try self.unifyGuarded(a_func.eff, b_func.eff);

        self.merge(vars, vars.b.desc.content);
    }

    /// unify two records
    ///
    /// this:
    /// * unwraps extensible variable to get all records fields
    /// * checks that parallel args unify
    fn unifyTwoRecords(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_record: Record,
        b_record: Record,
    ) error{TypeMismatch}!void {

        // First, unwrap all fields for record a, panicaing with various non-recoverable error
        // These pancis will likely be changed/removed in the future
        const a_gathered_fields = self.gatherRecordFields(a_record) catch |err| switch (err) {
            error.InvalidRecordExt => @panic("unifyTwoRecords: Record a ext was invalid"),
        };

        const b_gathered_fields = self.gatherRecordFields(b_record) catch |err| switch (err) {
            error.InvalidRecordExt => @panic("unifyTwoRecords: Record b ext was invalid"),
        };

        // Then partition the fields
        const partitioned = partitionFields(self.scratch, a_gathered_fields.range, b_gathered_fields.range);

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
                // Unify exts (these will both be the empty record)
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
                const only_in_a_fields_range = self.types_store.record_fields.appendSlice(
                    self.types_store.gpa,
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
                const only_in_b_fields_range = self.types_store.record_fields.appendSlice(
                    self.types_store.gpa,
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
                const only_in_a_fields_range = self.types_store.record_fields.appendSlice(
                    self.types_store.gpa,
                    self.scratch.only_in_a_fields.rangeToSlice(partitioned.only_in_a),
                );
                const only_in_a_var = self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_a_fields_range,
                    .ext = a_gathered_fields.ext,
                } } });

                // Create a new variable of a record with only b's uniq fields
                // This copies fields from scratch into type_store
                const only_in_b_fields_range = self.types_store.record_fields.appendSlice(
                    self.types_store.gpa,
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
    /// * structural aliases (by chasing `.backing_var`)
    /// * record extension chains (via nested `.record.ext`)
    ///
    /// Returns:
    /// * a `Range` indicating the location of the gathered fields in `gathered_fields`
    /// * the final tail extension variable, which is either a flex var or an empty record
    ///
    /// Errors if it encounters a malformed or invalid extension (e.g. a non-record type).
    fn gatherRecordFields(self: *Self, record: Record) error{InvalidRecordExt}!GatheredFields {
        var range = self.scratch.appendSliceGatheredFields(
            self.types_store.record_fields.rangeToSlice(record.fields),
        );
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
                            const next_range = self.scratch.appendSliceGatheredFields(
                                self.types_store.record_fields.rangeToSlice(record.fields),
                            );
                            range.end = next_range.end;
                            ext_var = ext_record.ext;
                        },
                        .empty_record => {
                            return .{ .ext = ext_var, .range = range };
                        },
                        else => return error.InvalidRecordExt,
                    }
                },
                else => return error.InvalidRecordExt,
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
        scratch: *Scratch,
        a_fields_range: RecordFieldSafeList.Range,
        b_fields_range: RecordFieldSafeList.Range,
    ) PartitionedRecordFields {
        // First sort the fields
        const a_fields = scratch.gathered_fields.rangeToSlice(a_fields_range);
        std.mem.sort(RecordField, a_fields, .{}, comptime RecordField.sortByFieldNameAsc);
        const b_fields = scratch.gathered_fields.rangeToSlice(b_fields_range);
        std.mem.sort(RecordField, b_fields, .{}, comptime RecordField.sortByFieldNameAsc);

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

            if (@intFromEnum(a_next.name) == @intFromEnum(b_next.name)) {
                _ = scratch.in_both_fields.append(scratch.gpa, TwoRecordFields{ .a = a_next, .b = b_next });
                a_i = a_i + 1;
                b_i = b_i + 1;
            } else if (@intFromEnum(a_next.name) < @intFromEnum(b_next.name)) {
                _ = scratch.only_in_a_fields.append(scratch.gpa, a_next);
                a_i = a_i + 1;
            } else {
                _ = scratch.only_in_b_fields.append(scratch.gpa, b_next);
                b_i = b_i + 1;
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
        shared_fields: []TwoRecordFields,
        mb_a_extended_fields: ?[]RecordField,
        mb_b_extended_fields: ?[]RecordField,
        ext: Var,
    ) error{TypeMismatch}!void {
        const range_start: RecordFieldSafeList.Idx = @enumFromInt(self.types_store.record_fields.len());

        for (shared_fields) |fields| {
            // TODO: pick recursion variable?
            try self.unifyGuarded(fields.a.var_, fields.b.var_);
            _ = self.types_store.record_fields.append(self.types_store.gpa, fields.b);
        }

        // Append combined fields
        if (mb_a_extended_fields) |extended_fields| {
            _ = self.types_store.record_fields.appendSlice(self.types_store.gpa, extended_fields);
        }
        if (mb_b_extended_fields) |extended_fields| {
            _ = self.types_store.record_fields.appendSlice(self.types_store.gpa, extended_fields);
        }

        const range_end: RecordFieldSafeList.Idx = @enumFromInt(self.types_store.record_fields.len());

        // Merge vars
        self.merge(vars, Content{ .structure = FlatType{ .record = .{
            .fields = .{ .start = range_start, .end = range_end },
            .ext = ext,
        } } });
    }

    /// unify two tag_unions
    ///
    /// this:
    /// * unwraps extensible variable to get all tag_unions tags
    /// * checks that parallel args unify
    fn unifyTwoTagUnions(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_tag_union: TagUnion,
        b_tag_union: TagUnion,
    ) error{TypeMismatch}!void {

        // First, unwrap all tags for tag_union a, panicaing with various non-recoverable error
        // These pancis will likely be changed/removed in the future
        const a_gathered_tags = self.gatherTagUnionTags(a_tag_union) catch |err| switch (err) {
            error.InvalidTagUnionExt => @panic("unifyTwoTagUnions: TagUnion a ext was invalid"),
        };

        const b_gathered_tags = self.gatherTagUnionTags(b_tag_union) catch |err| switch (err) {
            error.InvalidTagUnionExt => @panic("unifyTwoTagUnions: TagUnion b ext was invalid"),
        };

        // Then partition the tags
        const partitioned = partitionTags(self.scratch, a_gathered_tags.range, b_gathered_tags.range);

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
                // Unify exts (these will both be the empty tag_union)
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
                const only_in_a_tags_range = self.types_store.tags.appendSlice(
                    self.types_store.gpa,
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
                const only_in_b_tags_range = self.types_store.tags.appendSlice(
                    self.types_store.gpa,
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
                const only_in_a_tags_range = self.types_store.tags.appendSlice(
                    self.types_store.gpa,
                    self.scratch.only_in_a_tags.rangeToSlice(partitioned.only_in_a),
                );
                const only_in_a_var = self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_a_tags_range,
                    .ext = a_gathered_tags.ext,
                } } });

                // Create a new variable of a tag_union with only b's uniq tags
                // This copies tags from scratch into type_store
                const only_in_b_tags_range = self.types_store.tags.appendSlice(
                    self.types_store.gpa,
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
    /// * structural aliases (by chasing `.backing_var`)
    /// * tag_union extension chains (via nested `.tag_union.ext`)
    ///
    /// Returns:
    /// * a `Range` indicating the location of the gathered tags in `gathered_tags`
    /// * the final tail extension variable, which is either a flex var or an empty tag_union
    ///
    /// Errors if it encounters a malformed or invalid extension (e.g. a non-tag_union type).
    fn gatherTagUnionTags(self: *Self, tag_union: TagUnion) error{InvalidTagUnionExt}!GatheredTags {
        var range = self.scratch.appendSliceGatheredTags(
            self.types_store.tags.rangeToSlice(tag_union.tags),
        );
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
                            const next_range = self.scratch.appendSliceGatheredTags(
                                self.types_store.tags.rangeToSlice(tag_union.tags),
                            );
                            range.end = next_range.end;
                            ext_var = ext_tag_union.ext;
                        },
                        .empty_tag_union => {
                            return .{ .ext = ext_var, .range = range };
                        },
                        else => return error.InvalidTagUnionExt,
                    }
                },
                else => return error.InvalidTagUnionExt,
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
        scratch: *Scratch,
        a_tags_range: TagSafeList.Range,
        b_tags_range: TagSafeList.Range,
    ) PartitionedTags {
        // First sort the tags
        const a_tags = scratch.gathered_tags.rangeToSlice(a_tags_range);
        std.mem.sort(Tag, a_tags, .{}, comptime Tag.sortByTagIdxAsc);
        const b_tags = scratch.gathered_tags.rangeToSlice(b_tags_range);
        std.mem.sort(Tag, b_tags, .{}, comptime Tag.sortByTagIdxAsc);

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

            if (@intFromEnum(a_next.name) == @intFromEnum(b_next.name)) {
                _ = scratch.in_both_tags.append(scratch.gpa, TwoTags{ .a = a_next, .b = b_next });
                a_i = a_i + 1;
                b_i = b_i + 1;
            } else if (@intFromEnum(a_next.name) < @intFromEnum(b_next.name)) {
                _ = scratch.only_in_a_tags.append(scratch.gpa, a_next);
                a_i = a_i + 1;
            } else {
                _ = scratch.only_in_b_tags.append(scratch.gpa, b_next);
                b_i = b_i + 1;
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

    /// Given a list of shared tags & a list of extended tags, unify the shared
    /// Then merge a new tag_union with both shared+extended tags
    fn unifySharedTags(
        self: *Self,
        vars: *const ResolvedVarDescs,
        shared_tags: []TwoTags,
        mb_a_extended_tags: ?[]Tag,
        mb_b_extended_tags: ?[]Tag,
        ext: Var,
    ) error{TypeMismatch}!void {
        const range_start: TagSafeList.Idx = @enumFromInt(self.types_store.tags.len());

        for (shared_tags) |tags| {
            const tag_a_args = self.types_store.tag_args.rangeToSlice(tags.a.args);
            const tag_b_args = self.types_store.tag_args.rangeToSlice(tags.b.args);

            if (tag_a_args.len != tag_b_args.len) return error.TypeMismatch;

            for (tag_a_args, tag_b_args) |a_arg, b_arg| {
                try self.unifyGuarded(a_arg, b_arg);
            }

            _ = self.types_store.tags.append(self.types_store.gpa, Tag{
                .name = tags.b.name,
                .args = tags.b.args,
            });
        }

        // Append combined tags
        if (mb_a_extended_tags) |extended_tags| {
            _ = self.types_store.tags.appendSlice(self.types_store.gpa, extended_tags);
        }
        if (mb_b_extended_tags) |extended_tags| {
            _ = self.types_store.tags.appendSlice(self.types_store.gpa, extended_tags);
        }

        const range_end: TagSafeList.Idx = @enumFromInt(self.types_store.tags.len());

        // Merge vars
        self.merge(vars, Content{ .structure = FlatType{ .tag_union = .{
            .tags = .{ .start = range_start, .end = range_end },
            .ext = ext,
        } } });
    }
};

/// A reusable memory arena used across unification calls to avoid per-call allocations.
///
/// `Scratch` owns several typed scratch arrays, each designed to hold a specific type of
/// temporary data needed during unification. These include:
/// * `gathered_fields`: accumulated record fields from recursive extensions
/// * `only_in_a_fields`, `only_in_b_fields`: disjoint fields after `partitionFields`
/// * `in_both_fields`: shared fields with matching names
/// * `fresh_vars`: type variables created during a unification pass
///
/// `Scratch` should be initialized once and reused for many unification runs.
/// Each call to `unify` will automatically reset the scratch buffer at the start.
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
    }

    // helpers //

    fn appendSliceGatheredFields(self: *Self, fields: []const RecordField) RecordFieldSafeList.Range {
        return self.gathered_fields.appendSlice(self.gpa, fields);
    }

    fn appendSliceGatheredTags(self: *Self, fields: []const Tag) TagSafeList.Range {
        return self.gathered_tags.appendSlice(self.gpa, fields);
    }
};

// tests //

/// A lightweight test harness used in unification and type inference tests.
///
/// `TestEnv` bundles together the following components:
/// * a type store (`types_store`) for registering and resolving types
/// * an identifier store for name interning (`ident_store`)
/// * a reusable `Scratch` buffer for managing field partitions and temporary variables
///
/// This is intended to simplify unit test setup, particularly for unifying records,
/// functions, aliases, and other structured types.
const TestEnv = struct {
    const Self = @This();

    gpa: std.mem.Allocator,
    ident_store: Ident.Store,
    types_store: store.Store,
    scratch: Scratch,

    pub fn init(gpa: std.mem.Allocator) Self {
        return .{
            .gpa = gpa,
            .ident_store = Ident.Store.initCapacity(gpa, 16),
            .types_store = store.Store.init(gpa),
            .scratch = Scratch.init(gpa),
        };
    }

    pub fn deinit(self: *Self) void {
        self.ident_store.deinit(self.gpa);
        self.types_store.deinit();
        self.scratch.deinit();
    }

    const Error = error{ VarIsNotRoot, DescIsNotRecord };

    /// Get a desc from a root var
    pub fn getDescForRootVar(self: *Self, var_: Var) error{VarIsNotRoot}!Desc {
        switch (self.types_store.getSlot(var_)) {
            .root => |desc_idx| return self.types_store.getDesc(desc_idx),
            .redirect => return error.VarIsNotRoot,
        }
    }

    /// Unwrap a record or throw
    pub fn getRecordOrErr(desc: Desc) error{DescIsNotRecord}!Record {
        switch (desc.content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .record => |record| {
                        return record;
                    },
                    else => return error.DescIsNotRecord,
                }
            },
            else => return error.DescIsNotRecord,
        }
    }

    /// Unwrap a record or throw
    pub fn getTagUnionOrErr(desc: Desc) error{DescIsNotRecord}!TagUnion {
        switch (desc.content) {
            .structure => |flat_type| {
                switch (flat_type) {
                    .tag_union => |tag_union| {
                        return tag_union;
                    },
                    else => return error.DescIsNotRecord,
                }
            },
            else => return error.DescIsNotRecord,
        }
    }

    fn mkTypeIdent(self: *Self, name: []const u8) TypeIdent {
        const ident_idx = self.ident_store.insert(self.gpa, Ident.for_text(name), Region.zero());
        return TypeIdent{ .ident_idx = ident_idx };
    }

    // helpers - structure - tuple

    fn mkAlias(self: *Self, typ: Alias.Type, name: []const u8, args: []const Var, backing_var: Var) Alias {
        const args_range = self.types_store.appendAliasArgs(args);
        return .{
            .type = typ,
            .ident = self.mkTypeIdent(name),
            .args = args_range,
            .backing_var = backing_var,
        };
    }

    fn mkAliasFromIdent(self: *Self, typ: Alias.Type, type_ident_idx: TypeIdent, args: []const Var, backing_var: Var) Alias {
        const args_range = self.types_store.appendAliasArgs(args);
        return .{
            .type = typ,
            .ident = type_ident_idx,
            .args = args_range,
            .backing_var = backing_var,
        };
    }

    // helpers - structure - type_apply

    fn mkTypeApplyNoArg(self: *Self, name: []const u8) Content {
        const ident_idx = self.ident_store.insert(self.gpa, Ident.for_text(name), Region.zero());
        return .{ .structure = .{ .type_apply = .{
            .ident = .{ .ident_idx = ident_idx },
            .args = types.VarSafeList.Range.empty,
        } } };
    }

    fn mkTypeApplyArgs(self: *Self, name: []const u8, args: []const Var) Content {
        const ident_idx = self.ident_store.insert(self.gpa, Ident.for_text(name), Region.zero());
        const args_range = self.types_store.appendTypeApplyArgs(args);
        return .{ .structure = .{ .type_apply = .{ .ident = .{ .ident_idx = ident_idx }, .args = args_range } } };
    }

    fn mkTypeApply1Arg(self: *Self, name: []const u8, arg: Var) Content {
        return self.mkTypeApplyArgs(name, &[_]Var{arg});
    }

    fn mkTypeApply2Args(self: *Self, name: []const u8, arg1: Var, arg2: Var) Content {
        return self.mkTypeApplyArgs(name, &[_]Var{ arg1, arg2 });
    }

    // helpers - structure - tuple

    fn mkTuple(self: *Self, slice: []const Var) Content {
        const elems_range = self.types_store.appendTupleElems(slice);
        return Content{ .structure = .{ .tuple = .{ .elems = elems_range } } };
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

    const result = unify(&env.types_store, &env.scratch, a, a);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(desc, try env.getDescForRootVar(a));
}

test "unify - both flex vars" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.fresh();
    const b = env.types_store.fresh();

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
}

test "unify - a is flex_var and b is not" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a = env.types_store.fresh();
    const b = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
}

// unification - rigid

test "rigid_var - unifies with flex_var" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid = Content{ .rigid_var = @enumFromInt(1) };
    const a = env.types_store.freshFromContent(.{ .flex_var = null });
    const b = env.types_store.freshFromContent(rigid);

    const result = unify(&env.types_store, &env.scratch, a, b);
    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(rigid, (try env.getDescForRootVar(b)).content);
}

test "rigid_var - unifies with flex_var (other way)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid = Content{ .rigid_var = @enumFromInt(1) };
    const a = env.types_store.freshFromContent(rigid);
    const b = env.types_store.freshFromContent(.{ .flex_var = null });

    const result = unify(&env.types_store, &env.scratch, a, b);
    try std.testing.expectEqual(true, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(rigid, (try env.getDescForRootVar(b)).content);
}

test "rigid_var - cannot unify with alias (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const alias = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const rigid = env.types_store.freshFromContent(.{ .rigid_var = @enumFromInt(1) });

    const result = unify(&env.types_store, &env.scratch, alias, rigid);
    try std.testing.expectEqual(false, result.isOk());
}

test "rigid_var - cannot unify with itself if different names (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid1 = env.types_store.freshFromContent(.{ .rigid_var = @enumFromInt(1) });
    const rigid2 = env.types_store.freshFromContent(.{ .rigid_var = @enumFromInt(2) });

    const result = unify(&env.types_store, &env.scratch, rigid1, rigid2);
    try std.testing.expectEqual(false, result.isOk());
}

test "rigid_var - cannot unify with identical rigid_var (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const rigid1 = env.types_store.freshFromContent(.{ .rigid_var = @enumFromInt(1) });
    const rigid2 = env.types_store.freshFromContent(.{ .rigid_var = @enumFromInt(1) });

    const result = unify(&env.types_store, &env.scratch, rigid1, rigid2);
    try std.testing.expectEqual(false, result.isOk());
}

// unification - aliases

test "unify - structural alias with same args" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{ str, bool_ }));
    const alias = Content{ .alias = env.mkAlias(.structural, "AliasName", &[_]Var{ str, bool_ }, backing) };

    const a = env.types_store.freshFromContent(alias);
    const b = env.types_store.freshFromContent(alias);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(alias, (try env.getDescForRootVar(b)).content);
}

test "unify - structural aliases with different names but same backing" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{str}));
    const a_alias = Content{ .alias = env.mkAlias(.structural, "AliasA", &[_]Var{str}, backing) };
    const b_alias = Content{ .alias = env.mkAlias(.structural, "AliasB", &[_]Var{str}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(a_alias, (try env.getDescForRootVar(a)).content);
    try std.testing.expectEqual(b_alias, (try env.getDescForRootVar(b)).content);
}

test "unify - structural alias with different args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .alias = env.mkAliasFromIdent(.structural, alias_ident, &[_]Var{str}, backing) };
    const b_alias = Content{ .alias = env.mkAliasFromIdent(.structural, alias_ident, &[_]Var{bool_}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - structural vs opaque alias with same name and args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{str}));

    const alias_ident = env.mkTypeIdent("Thing");
    const a_alias = Content{ .alias = env.mkAliasFromIdent(.structural, alias_ident, &[_]Var{str}, backing) };
    const b_alias = Content{ .alias = env.mkAliasFromIdent(.opaque_, alias_ident, &[_]Var{str}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - opaque alias with different args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .alias = env.mkAliasFromIdent(.opaque_, alias_ident, &[_]Var{str}, backing) };
    const b_alias = Content{ .alias = env.mkAliasFromIdent(.opaque_, alias_ident, &[_]Var{bool_}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - opaque alias with sam args" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .alias = env.mkAliasFromIdent(.opaque_, alias_ident, &[_]Var{bool_}, backing) };
    const b_alias = Content{ .alias = env.mkAliasFromIdent(.opaque_, alias_ident, &[_]Var{bool_}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.freshFromContent(b_alias);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(b_alias, (try env.getDescForRootVar(b)).content);
}

test "unify - opaque alias with flex" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));
    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .alias = env.mkAliasFromIdent(.opaque_, alias_ident, &[_]Var{bool_}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.fresh();

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(a_alias, (try env.getDescForRootVar(b)).content);
}

test "unify - structural alias with flex" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));
    const backing = env.types_store.freshFromContent(env.mkTuple(&[_]Var{ str, bool_ }));

    const alias_ident = env.mkTypeIdent("Alias");
    const a_alias = Content{ .alias = env.mkAliasFromIdent(.structural, alias_ident, &[_]Var{bool_}, backing) };

    const a = env.types_store.freshFromContent(a_alias);
    const b = env.types_store.fresh();

    const result = unify(&env.types_store, &env.scratch, a, b);

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

    const result = unify(&env.types_store, &env.scratch, a, b);

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

    const result = unify(&env.types_store, &env.scratch, a, b);

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

    const result = unify(&env.types_store, &env.scratch, a, b);

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

    const result = unify(&env.types_store, &env.scratch, a, b);

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

    const result = unify(&env.types_store, &env.scratch, a, b);

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

    const result = unify(&env.types_store, &env.scratch, a, b);

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

    const result = unify(&env.types_store, &env.scratch, a, b);

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

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - pure with structure type fails" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const a = env.types_store.freshFromContent(.pure);
    const b = env.types_store.freshFromContent(str);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
}

test "unify - effectful with structure type fails" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const a = env.types_store.freshFromContent(.effectful);
    const b = env.types_store.freshFromContent(str);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
}

// unification - structure/flex_vars

test "unify - a is type_apply and b is flex_var" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");

    const a = env.types_store.freshFromContent(str);
    const b = env.types_store.fresh();

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a is flex_var and b is type_apply" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");

    const a = env.types_store.fresh();
    const b = env.types_store.freshFromContent(str);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - type_apply

test "unify - a & b are same type_apply" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");

    const a = env.types_store.freshFromContent(str);
    const b = env.types_store.freshFromContent(str);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are diff type_apply (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const bool_ = env.mkTypeApplyNoArg("Bool");

    const a = env.types_store.freshFromContent(bool_);
    const b = env.types_store.freshFromContent(str);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are same type_apply with 1 args" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.types_store.freshFromContent(str);

    const maybe_str = env.mkTypeApply1Arg("Maybe", str_var);

    const a = env.types_store.freshFromContent(maybe_str);
    const b = env.types_store.freshFromContent(maybe_str);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(maybe_str, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are same type_apply with 2 args" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.types_store.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.types_store.freshFromContent(bool_);

    const result_str_bool = env.mkTypeApply2Args("Result", str_var, bool_var);

    const a = env.types_store.freshFromContent(result_str_bool);
    const b = env.types_store.freshFromContent(result_str_bool);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(result_str_bool, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are same type_apply with flipped args (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.types_store.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.types_store.freshFromContent(bool_);

    const result_str_bool = env.mkTypeApply2Args("Result", str_var, bool_var);
    const result_bool_str = env.mkTypeApply2Args("Result", bool_var, str_var);

    const a = env.types_store.freshFromContent(result_str_bool);
    const b = env.types_store.freshFromContent(result_bool_str);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - tuple

test "unify - a & b are same tuple" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.types_store.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.types_store.freshFromContent(bool_);

    const tuple_str_bool = env.mkTuple(&[_]Var{ str_var, bool_var });

    const a = env.types_store.freshFromContent(tuple_str_bool);
    const b = env.types_store.freshFromContent(tuple_str_bool);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(tuple_str_bool, (try env.getDescForRootVar(b)).content);
}

test "unify - a & b are tuples with args flipped (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.mkTypeApplyNoArg("Str");
    const str_var = env.types_store.freshFromContent(str);

    const bool_ = env.mkTypeApplyNoArg("Bool");
    const bool_var = env.types_store.freshFromContent(bool_);

    const tuple_str_bool = env.mkTuple(&[_]Var{ str_var, bool_var });
    const tuple_bool_str = env.mkTuple(&[_]Var{ bool_var, str_var });

    const a = env.types_store.freshFromContent(tuple_str_bool);
    const b = env.types_store.freshFromContent(tuple_bool_str);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - num

test "unify - num flex_var with int structure" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const flex = Content{ .structure = types.num_flex_var };
    const int_i32 = Content{ .structure = types.int_i32 };

    const a = env.types_store.freshFromContent(flex);
    const b = env.types_store.freshFromContent(int_i32);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(int_i32, (try env.getDescForRootVar(b)).content);
}

test "unify - num int(flex_var) with int structure" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .structure = types.int_flex_var };
    const b_content = Content{ .structure = types.int_i64 };

    const a = env.types_store.freshFromContent(a_content);
    const b = env.types_store.freshFromContent(b_content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(b_content, (try env.getDescForRootVar(b)).content);
}

test "unify - num frac(flex_var) with frac structure" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .structure = types.frac_flex_var };
    const b_content = Content{ .structure = types.frac_f64 };

    const a = env.types_store.freshFromContent(a_content);
    const b = env.types_store.freshFromContent(b_content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(b_content, (try env.getDescForRootVar(b)).content);
}

test "unify - num int structure == int structure" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const i64a = Content{ .structure = types.int_i64 };
    const i64b = Content{ .structure = types.int_i64 };

    const a = env.types_store.freshFromContent(i64a);
    const b = env.types_store.freshFromContent(i64b);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(i64b, (try env.getDescForRootVar(b)).content);
}

test "unify - num frac structure != frac structure (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .structure = types.frac_f32 };
    const b_content = Content{ .structure = types.frac_f64 };

    const a = env.types_store.freshFromContent(a_content);
    const b = env.types_store.freshFromContent(b_content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - num int structure != int structure (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .structure = types.int_i32 };
    const b_content = Content{ .structure = types.int_i64 };

    const a = env.types_store.freshFromContent(a_content);
    const b = env.types_store.freshFromContent(b_content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - num int vs frac (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_desc = Content{ .structure = types.int_i32 };
    const frac_desc = Content{ .structure = types.frac_f32 };

    const a = env.types_store.freshFromContent(int_desc);
    const b = env.types_store.freshFromContent(frac_desc);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - num frac(flex_var) with frac(flex_var)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a_content = Content{ .structure = types.frac_flex_var };
    const b_content = Content{ .structure = types.frac_flex_var };

    const a = env.types_store.freshFromContent(a_content);
    const b = env.types_store.freshFromContent(b_content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(b_content, (try env.getDescForRootVar(b)).content);
}

// unification - structure/structure - func

test "unify - func are same" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = types.int_i32 });
    const num = env.types_store.freshFromContent(Content{ .structure = types.num_flex_var });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const func = env.mkFuncFlex(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(func);
    const b = env.types_store.freshFromContent(func);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - funcs have diff return args (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = types.int_i32 });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const a = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{int_i32}, str));
    const b = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{str}, str));

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - funcs have diff return types (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = types.int_i32 });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const a = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{str}, int_i32));
    const b = env.types_store.freshFromContent(env.mkFuncFlex(&[_]Var{str}, str));

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs pure" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = types.int_i32 });
    const num = env.types_store.freshFromContent(Content{ .structure = types.num_flex_var });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const func = env.mkFuncPure(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(func);
    const b = env.types_store.freshFromContent(func);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs effectful" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = types.int_i32 });
    const num = env.types_store.freshFromContent(Content{ .structure = types.num_flex_var });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const func = env.mkFuncEff(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(func);
    const b = env.types_store.freshFromContent(func);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(func, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first eff, second pure (fail)" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = types.int_i32 });
    const num = env.types_store.freshFromContent(Content{ .structure = types.num_flex_var });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const pure_func = env.mkFuncPure(&[_]Var{ str, num }, int_i32);
    const eff_func = env.mkFuncEff(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(eff_func);
    const b = env.types_store.freshFromContent(pure_func);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(.err, (try env.getDescForRootVar(b)).content);
}

test "unify - same funcs first pure, second eff" {
    const gpa = std.testing.allocator;

    var env = TestEnv.init(gpa);
    defer env.deinit();

    const int_i32 = env.types_store.freshFromContent(Content{ .structure = types.int_i32 });
    const num = env.types_store.freshFromContent(Content{ .structure = types.num_flex_var });
    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const pure_func = env.mkFuncPure(&[_]Var{ str, num }, int_i32);
    const eff_func = env.mkFuncEff(&[_]Var{ str, num }, int_i32);

    const a = env.types_store.freshFromContent(pure_func);
    const b = env.types_store.freshFromContent(eff_func);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));
    try std.testing.expectEqual(eff_func, (try env.getDescForRootVar(b)).content);
}

// unification - records - partition fields

test "partitionFields - same record" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const field_x = RecordField{ .name = @enumFromInt(1), .typ = .required, .var_ = @enumFromInt(0) };
    const field_y = RecordField{ .name = @enumFromInt(2), .typ = .optional, .var_ = @enumFromInt(1) };

    const range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ field_x, field_y });

    const result = Unifier.partitionFields(&env.scratch, range, range);

    try std.testing.expectEqual(0, result.only_in_a.len());
    try std.testing.expectEqual(0, result.only_in_b.len());
    try std.testing.expectEqual(2, result.in_both.len());
}

test "partitionFields - disjoint fields" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a1 = RecordField{ .name = @enumFromInt(1), .typ = .required, .var_ = @enumFromInt(0) };
    const a2 = RecordField{ .name = @enumFromInt(2), .typ = .required, .var_ = @enumFromInt(1) };
    const b1 = RecordField{ .name = @enumFromInt(3), .typ = .required, .var_ = @enumFromInt(2) };

    const a_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ a1, a2 });
    const b_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{b1});

    const result = Unifier.partitionFields(&env.scratch, a_range, b_range);

    try std.testing.expectEqual(2, result.only_in_a.len());
    try std.testing.expectEqual(1, result.only_in_b.len());
    try std.testing.expectEqual(0, result.in_both.len());
}

test "partitionFields - overlapping fields" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const a1 = RecordField{ .name = @enumFromInt(1), .typ = .required, .var_ = @enumFromInt(0) };
    const both = RecordField{ .name = @enumFromInt(2), .typ = .optional, .var_ = @enumFromInt(1) };
    const b1 = RecordField{ .name = @enumFromInt(3), .typ = .required, .var_ = @enumFromInt(2) };

    const a_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ a1, both });
    const b_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ b1, both });

    const result = Unifier.partitionFields(&env.scratch, a_range, b_range);

    try std.testing.expectEqual(1, result.only_in_a.len());
    try std.testing.expectEqual(1, result.only_in_b.len());
    try std.testing.expectEqual(1, result.in_both.len());
}

test "partitionFields - reordering is normalized" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const f1 = RecordField{ .name = @enumFromInt(1), .typ = .required, .var_ = @enumFromInt(0) };
    const f2 = RecordField{ .name = @enumFromInt(2), .typ = .optional, .var_ = @enumFromInt(1) };
    const f3 = RecordField{ .name = @enumFromInt(3), .typ = .optional, .var_ = @enumFromInt(2) };

    const a_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ f3, f1, f2 });
    const b_range = env.scratch.appendSliceGatheredFields(&[_]RecordField{ f1, f2, f3 });

    const result = Unifier.partitionFields(&env.scratch, a_range, b_range);

    try std.testing.expectEqual(0, result.only_in_a.len());
    try std.testing.expectEqual(0, result.only_in_b.len());
    try std.testing.expectEqual(3, result.in_both.len());

    const both = env.scratch.in_both_fields.rangeToSlice(result.in_both);
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(both[0].a.name));
    try std.testing.expectEqual(@as(u32, 1), @intFromEnum(both[0].b.name));
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(both[1].a.name));
    try std.testing.expectEqual(@as(u32, 2), @intFromEnum(both[1].b.name));
    try std.testing.expectEqual(@as(u32, 3), @intFromEnum(both[2].a.name));
    try std.testing.expectEqual(@as(u32, 3), @intFromEnum(both[2].b.name));
}

// unification - structure/structure - records closed

test "unify - identical closed records" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const fields = [_]RecordField{
        .{ .name = @enumFromInt(1), .var_ = str, .typ = .required },
    };
    const record_data = env.mkRecordClosed(&fields);

    const a = env.types_store.freshFromContent(record_data.content);
    const b = env.types_store.freshFromContent(record_data.content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqualSlices(
        RecordField,
        env.types_store.record_fields.rangeToSlice(record_data.record.fields),
        env.types_store.record_fields.rangeToSlice(b_record.fields),
    );
}

test "unify - closed record mismatch on diff fields (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const field1 = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field2 = RecordField{ .name = @enumFromInt(2), .var_ = str, .typ = .required };

    const a_record_data = env.mkRecordClosed(&[_]RecordField{ field1, field2 });
    const a = env.types_store.freshFromContent(a_record_data.content);

    const b_record_data = env.mkRecordClosed(&[_]RecordField{field1});
    const b = env.types_store.freshFromContent(b_record_data.content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const desc_b = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc_b.content);
}

// unification - structure/structure - records open

test "unify - open record a extends b" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const field_shared = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_a_only = RecordField{ .name = @enumFromInt(2), .var_ = int, .typ = .required };

    const a_rec_data = env.mkRecordOpen(&[_]RecordField{ field_shared, field_a_only });
    const a = env.types_store.freshFromContent(a_rec_data.content);
    const b_rec_data = env.mkRecordOpen(&[_]RecordField{field_shared});
    const b = env.types_store.freshFromContent(b_rec_data.content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_record.fields.len());
    const b_record_fields = env.types_store.getRecordFieldsSlice(b_record.fields);
    try std.testing.expectEqual(field_shared, b_record_fields[0]);

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(env.scratch.fresh_vars.get(@enumFromInt(0)).*, b_record.ext);

    const b_ext_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(b_record.ext).desc);
    try std.testing.expectEqual(1, b_ext_record.fields.len());
    const b_ext_record_fields = env.types_store.getRecordFieldsSlice(b_ext_record.fields);
    try std.testing.expectEqual(field_a_only, b_ext_record_fields[0]);

    const b_ext_ext = env.types_store.resolveVar(b_ext_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);
}

test "unify - open record b extends a" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const field_shared = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_b_only = RecordField{ .name = @enumFromInt(2), .var_ = int, .typ = .required };

    const a_rec_data = env.mkRecordOpen(&[_]RecordField{field_shared});
    const a = env.types_store.freshFromContent(a_rec_data.content);
    const b_rec_data = env.mkRecordOpen(&[_]RecordField{ field_shared, field_b_only });
    const b = env.types_store.freshFromContent(b_rec_data.content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(1, b_record.fields.len());
    const b_record_fields = env.types_store.getRecordFieldsSlice(b_record.fields);
    try std.testing.expectEqual(field_shared, b_record_fields[0]);

    try std.testing.expectEqual(1, env.scratch.fresh_vars.len());
    try std.testing.expectEqual(env.scratch.fresh_vars.get(@enumFromInt(0)).*, b_record.ext);

    const b_ext_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(b_record.ext).desc);
    try std.testing.expectEqual(1, b_ext_record.fields.len());
    const b_ext_record_fields = env.types_store.getRecordFieldsSlice(b_ext_record.fields);
    try std.testing.expectEqual(field_b_only, b_ext_record_fields[0]);

    const b_ext_ext = env.types_store.resolveVar(b_ext_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext_ext);
}

test "unify - both extend open record" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));
    const bool_ = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Bool"));

    const field_shared = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_a_only = RecordField{ .name = @enumFromInt(2), .var_ = int, .typ = .required };
    const field_b_only = RecordField{ .name = @enumFromInt(3), .var_ = bool_, .typ = .required };

    const a_rec_data = env.mkRecordOpen(&[_]RecordField{ field_shared, field_a_only });
    const a = env.types_store.freshFromContent(a_rec_data.content);
    const b_rec_data = env.mkRecordOpen(&[_]RecordField{ field_shared, field_b_only });
    const b = env.types_store.freshFromContent(b_rec_data.content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const b_record = try TestEnv.getRecordOrErr(try env.getDescForRootVar(b));
    try std.testing.expectEqual(3, b_record.fields.len());
    const b_record_fields = env.types_store.getRecordFieldsSlice(b_record.fields);
    try std.testing.expectEqual(field_shared, b_record_fields[0]);

    const b_ext = env.types_store.resolveVar(b_record.ext).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, b_ext);

    try std.testing.expectEqual(3, env.scratch.fresh_vars.len());

    const only_a_var = env.scratch.fresh_vars.get(@enumFromInt(0)).*;
    const only_a_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(only_a_var).desc);
    try std.testing.expectEqual(1, only_a_record.fields.len());
    const only_a_record_fields = env.types_store.getRecordFieldsSlice(only_a_record.fields);
    try std.testing.expectEqual(field_a_only, only_a_record_fields[0]);

    const only_b_var = env.scratch.fresh_vars.get(@enumFromInt(1)).*;
    const only_b_record = try TestEnv.getRecordOrErr(env.types_store.resolveVar(only_b_var).desc);
    try std.testing.expectEqual(1, only_b_record.fields.len());
    const only_b_record_fields = env.types_store.getRecordFieldsSlice(only_b_record.fields);
    try std.testing.expectEqual(field_b_only, only_b_record_fields[0]);

    const ext_var = env.scratch.fresh_vars.get(@enumFromInt(2)).*;
    const ext_content = env.types_store.resolveVar(ext_var).desc.content;
    try std.testing.expectEqual(Content{ .flex_var = null }, ext_content);
}

test "unify - record mismatch on shared field (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const field_a = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_b = RecordField{ .name = @enumFromInt(1), .var_ = int, .typ = .required };

    const a_rec_data = env.mkRecordOpen(&[_]RecordField{field_a});
    const a = env.types_store.freshFromContent(a_rec_data.content);
    const b_rec_data = env.mkRecordOpen(&[_]RecordField{field_b});
    const b = env.types_store.freshFromContent(b_rec_data.content);

    const result = unify(&env.types_store, &env.scratch, a, b);

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

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const field_x = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_y = RecordField{ .name = @enumFromInt(2), .var_ = str, .typ = .required };

    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{ field_x, field_y }).content);
    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{field_x}).content);

    const result = unify(&env.types_store, &env.scratch, open, closed);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.types_store.getSlot(open));
    try std.testing.expectEqual(Content.err, (try env.getDescForRootVar(closed)).content);
}

test "unify - closed record extends open" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const field_x = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_y = RecordField{ .name = @enumFromInt(2), .var_ = str, .typ = .required };

    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_x}).content);
    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{ field_x, field_y }).content);

    const result = unify(&env.types_store, &env.scratch, open, closed);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.types_store.getSlot(open));
}

test "unify - open vs closed with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const field_x_str = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_x_int = RecordField{ .name = @enumFromInt(1), .var_ = int, .typ = .required };

    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_x_str}).content);
    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{field_x_int}).content);

    const result = unify(&env.types_store, &env.scratch, open, closed);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = closed }, env.types_store.getSlot(open));

    const desc = try env.getDescForRootVar(closed);
    try std.testing.expectEqual(Content.err, desc.content);
}

test "unify - closed vs open with type mismatch (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const field_x_str = RecordField{ .name = @enumFromInt(1), .var_ = str, .typ = .required };
    const field_x_int = RecordField{ .name = @enumFromInt(1), .var_ = int, .typ = .required };

    const closed = env.types_store.freshFromContent(env.mkRecordClosed(&[_]RecordField{field_x_int}).content);
    const open = env.types_store.freshFromContent(env.mkRecordOpen(&[_]RecordField{field_x_str}).content);

    const result = unify(&env.types_store, &env.scratch, closed, open);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = open }, env.types_store.getSlot(closed));

    const desc = try env.getDescForRootVar(open);
    try std.testing.expectEqual(Content.err, desc.content);
}

// unification - structure/structure - tag unions closed

test "unify - identical closed tag_unions" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));

    const tag_arg_range = env.mkTagArgs(&[_]Var{str});
    const tags = [_]Tag{
        .{ .name = @enumFromInt(1), .args = tag_arg_range },
    };
    const tag_union_data = env.mkTagUnionClosed(&tags);

    const a = env.types_store.freshFromContent(tag_union_data.content);
    const b = env.types_store.freshFromContent(tag_union_data.content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(.ok, result);
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const b_tag_union = try TestEnv.getTagUnionOrErr(try env.getDescForRootVar(b));
    const b_tags = env.types_store.tags.rangeToSlice(b_tag_union.tags);
    try std.testing.expectEqual(1, b_tags.len);

    const b_tag = b_tags[0];
    const b_tag_args = env.types_store.tag_args.rangeToSlice(b_tag.args);
    try std.testing.expectEqual(1, b_tag_args.len);
    try std.testing.expectEqual(str, b_tag_args[0]);
}

test "unify - closed tag_unions with diff args (fail)" {
    const gpa = std.testing.allocator;
    var env = TestEnv.init(gpa);
    defer env.deinit();

    const str = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Str"));
    const int = env.types_store.freshFromContent(env.mkTypeApplyNoArg("Int"));

    const a_tag_arg_range = env.mkTagArgs(&[_]Var{str});
    const a_tags = [_]Tag{
        .{ .name = @enumFromInt(1), .args = a_tag_arg_range },
    };
    const a_tag_union_data = env.mkTagUnionClosed(&a_tags);
    const a = env.types_store.freshFromContent(a_tag_union_data.content);

    const b_tag_arg_range = env.mkTagArgs(&[_]Var{int});
    const b_tags = [_]Tag{
        .{ .name = @enumFromInt(1), .args = b_tag_arg_range },
    };
    const b_tag_union_data = env.mkTagUnionClosed(&b_tags);
    const b = env.types_store.freshFromContent(b_tag_union_data.content);

    const result = unify(&env.types_store, &env.scratch, a, b);

    try std.testing.expectEqual(false, result.isOk());
    try std.testing.expectEqual(Slot{ .redirect = b }, env.types_store.getSlot(a));

    const desc = try env.getDescForRootVar(b);
    try std.testing.expectEqual(Content.err, desc.content);
}
