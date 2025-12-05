//! This module implements Hindley-Milner style type unification with extensions for:
//!
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
const builtin = @import("builtin");
const base = @import("base");
const tracy = @import("tracy");
const collections = @import("collections");
const types_mod = @import("types");
const can = @import("can");
const copy_import = @import("copy_import.zig");
const Check = @import("check").Check;

const problem_mod = @import("problem.zig");
const occurs = @import("occurs.zig");
const snapshot_mod = @import("snapshot.zig");

const ModuleEnv = can.ModuleEnv;
const AutoImportedType = can.Can.AutoImportedType;
const CIR = can.CIR;

const Region = base.Region;
const Ident = base.Ident;
const MkSafeList = collections.SafeList;

const SmallStringInterner = collections.SmallStringInterner;

const Slot = types_mod.Slot;
const ResolvedVarDesc = types_mod.ResolvedVarDesc;
const ResolvedVarDescs = types_mod.ResolvedVarDescs;

const TypeIdent = types_mod.TypeIdent;
const Var = types_mod.Var;
const Desc = types_mod.Descriptor;
const Rank = types_mod.Rank;
const Mark = types_mod.Mark;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const RecursionVar = types_mod.RecursionVar;
const Content = types_mod.Content;
const Alias = types_mod.Alias;
const NominalType = types_mod.NominalType;
const FlatType = types_mod.FlatType;
const Builtin = types_mod.Builtin;
const Tuple = types_mod.Tuple;
const Num = types_mod.Num;
const Func = types_mod.Func;
const Record = types_mod.Record;
const RecordField = types_mod.RecordField;
const TwoRecordFields = types_mod.TwoRecordFields;
const TagUnion = types_mod.TagUnion;
const Tag = types_mod.Tag;
const TwoTags = types_mod.TwoTags;
const StaticDispatchConstraint = types_mod.StaticDispatchConstraint;
const TwoStaticDispatchConstraints = types_mod.TwoStaticDispatchConstraints;

const VarSafeList = Var.SafeList;
const RecordFieldSafeMultiList = RecordField.SafeMultiList;
const RecordFieldSafeList = RecordField.SafeList;
const TwoRecordFieldsSafeMultiList = TwoRecordFields.SafeMultiList;
const TwoRecordFieldsSafeList = TwoRecordFields.SafeList;
const TagSafeList = Tag.SafeList;
const TagSafeMultiList = Tag.SafeMultiList;
const TwoTagsSafeList = TwoTags.SafeList;

const Problem = problem_mod.Problem;
const ProblemStore = problem_mod.Store;

/// The result of unification
pub const Result = union(enum) {
    const Self = @This();

    ok,
    problem: Problem.Idx,

    pub fn isOk(self: Self) bool {
        return self == .ok;
    }

    pub fn isProblem(self: Self) bool {
        switch (self) {
            .ok => return false,
            .problem => return true,
        }
    }
};

/// Unify two type variables
///
/// This function
/// * Resolves type variables & compresses paths
/// * Compares variable contents for equality
/// * Merges unified variables so 1 is "root" and the other is "redirect"
pub fn unify(
    module_env: *ModuleEnv,
    types: *types_mod.Store,
    problems: *problem_mod.Store,
    snapshots: *snapshot_mod.Store,
    type_writer: *types_mod.TypeWriter,
    unify_scratch: *Scratch,
    occurs_scratch: *occurs.Scratch,
    /// The "expected" variable
    a: Var,
    /// The "actual" variable
    b: Var,
) std.mem.Allocator.Error!Result {
    return unifyWithConf(
        module_env,
        types,
        problems,
        snapshots,
        type_writer,
        unify_scratch,
        occurs_scratch,
        a,
        b,
        Conf{ .ctx = .anon, .constraint_origin_var = null },
    );
}

/// Conf about a unification, used to improve error messages
pub const Conf = struct {
    ctx: Ctx,
    constraint_origin_var: ?Var,

    /// If the "expect" var comes fro an annotation, or if it's anonymous
    pub const Ctx = enum { anon, anno };
};

/// Unify two type variables
///
/// This function
/// * Resolves type variables & compresses paths
/// * Compares variable contents for equality
/// * Merges unified variables so 1 is "root" and the other is "redirect"
///
/// This function accepts a context and optional constraint origin var (for better error reporting)
pub fn unifyWithConf(
    module_env: *ModuleEnv,
    types: *types_mod.Store,
    problems: *problem_mod.Store,
    snapshots: *snapshot_mod.Store,
    type_writer: *types_mod.TypeWriter,
    unify_scratch: *Scratch,
    occurs_scratch: *occurs.Scratch,
    /// The "expected" variable
    a: Var,
    /// The "actual" variable
    b: Var,
    conf: Conf,
) std.mem.Allocator.Error!Result {
    const trace = tracy.trace(@src());
    defer trace.end();

    // First reset the scratch store
    unify_scratch.reset();

    // Unify
    var unifier = Unifier.init(module_env, types, unify_scratch, occurs_scratch);
    unifier.unifyGuarded(a, b) catch |err| {
        const problem: Problem = blk: {
            switch (err) {
                error.AllocatorError => {
                    return error.OutOfMemory;
                },
                error.TypeMismatch => {
                    const expected_snapshot = try snapshots.snapshotVarForError(types, type_writer, a);
                    const actual_snapshot = try snapshots.snapshotVarForError(types, type_writer, b);

                    break :blk .{ .type_mismatch = .{
                        .types = .{
                            .expected_var = a,
                            .expected_snapshot = expected_snapshot,
                            .actual_var = b,
                            .actual_snapshot = actual_snapshot,
                            .from_annotation = conf.ctx == .anno,
                            .constraint_origin_var = conf.constraint_origin_var,
                        },
                        .detail = null,
                    } };
                },
                error.NumberDoesNotFit => {
                    // For number literal errors, we need to determine which var is the literal
                    // and which is the expected type
                    const a_resolved = types.resolveVar(a);

                    // Check if 'a' is the literal (has int_poly/num_poly/unbound types) or 'b' is
                    const literal_is_a = switch (a_resolved.desc.content) {
                        .structure => |structure| switch (structure) {
                            .record_unbound => true,
                            else => false,
                        },
                        else => false,
                    };

                    const literal_var = if (literal_is_a) a else b;
                    const expected_var = if (literal_is_a) b else a;
                    const expected_snapshot = try snapshots.snapshotVarForError(types, type_writer, expected_var);

                    break :blk .{ .number_does_not_fit = .{
                        .literal_var = literal_var,
                        .expected_type = expected_snapshot,
                    } };
                },
                error.NegativeUnsignedInt => {
                    // For number literal errors, we need to determine which var is the literal
                    // and which is the expected type
                    const a_resolved = types.resolveVar(a);

                    // Check if 'a' is the literal (has int_poly/num_poly/unbound types) or 'b' is
                    const literal_is_a = switch (a_resolved.desc.content) {
                        .structure => |structure| switch (structure) {
                            .record_unbound => true,
                            else => false,
                        },
                        else => false,
                    };

                    const literal_var = if (literal_is_a) a else b;
                    const expected_var = if (literal_is_a) b else a;
                    const expected_snapshot = try snapshots.snapshotVarForError(types, type_writer, expected_var);

                    break :blk .{ .negative_unsigned_int = .{
                        .literal_var = literal_var,
                        .expected_type = expected_snapshot,
                    } };
                },
                error.UnifyErr => {
                    // Unify can error in the following ways:
                    //
                    // 1. Encountering illegal recursion (infinite or anonymous)
                    // 2. Encountering an invalid polymorphic number type
                    // 2. Encountering an invalid record extensible type
                    // 2. Encountering an invalid tag union extensible type
                    //
                    // In these cases, before throwing, we set error state in
                    // `scratch.occurs_err`. This is necessary because you cannot
                    // associated an error payload when throwing.
                    //
                    // If we threw but there is no error data, it is a bug
                    if (unify_scratch.err) |unify_err| {
                        switch (unify_err) {
                            .recursion_anonymous => |var_| {
                                // TODO: Snapshot infinite recursion
                                // const snapshot = snapshots.deepCopyVar(types, var_);
                                break :blk .{ .anonymous_recursion = .{
                                    .var_ = var_,
                                } };
                            },
                            .recursion_infinite => |var_| {
                                // TODO: Snapshot infinite recursion
                                // const snapshot = snapshots.deepCopyVar(types, var_);
                                break :blk .{ .infinite_recursion = .{
                                    .var_ = var_,
                                } };
                            },
                            .invalid_number_type => |var_| {
                                const snapshot = try snapshots.snapshotVarForError(types, type_writer, var_);
                                break :blk .{ .invalid_number_type = .{
                                    .var_ = var_,
                                    .snapshot = snapshot,
                                } };
                            },
                            .invalid_record_ext => |var_| {
                                const snapshot = try snapshots.snapshotVarForError(types, type_writer, var_);
                                break :blk .{ .invalid_record_ext = .{
                                    .var_ = var_,
                                    .snapshot = snapshot,
                                } };
                            },
                            .invalid_tag_union_ext => |var_| {
                                const snapshot = try snapshots.snapshotVarForError(types, type_writer, var_);
                                break :blk .{ .invalid_tag_union_ext = .{
                                    .var_ = var_,
                                    .snapshot = snapshot,
                                } };
                            },
                        }
                    } else {
                        const expected_snapshot = try snapshots.snapshotVarForError(types, type_writer, a);
                        const actual_snapshot = try snapshots.snapshotVarForError(types, type_writer, b);
                        break :blk .{ .bug = .{
                            .expected_var = a,
                            .expected = expected_snapshot,
                            .actual_var = b,
                            .actual = actual_snapshot,
                        } };
                    }
                },
            }
        };
        const problem_idx = try problems.appendProblem(module_env.gpa, problem);
        types.union_(a, b, .{
            .content = .err,
            .rank = Rank.generalized,
            .mark = Mark.none,
        });
        return Result{ .problem = problem_idx };
    };

    return .ok;
}

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

    module_env: *ModuleEnv,
    types_store: *types_mod.Store,
    scratch: *Scratch,
    occurs_scratch: *occurs.Scratch,
    depth: u8,
    skip_depth_check: bool,

    /// Init unifier
    pub fn init(
        module_env: *ModuleEnv,
        types_store: *types_mod.Store,
        scratch: *Scratch,
        occurs_scratch: *occurs.Scratch,
    ) Unifier {
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
    fn fresh(self: *Self, vars: *const ResolvedVarDescs, new_content: Content) std.mem.Allocator.Error!Var {
        const var_ = try self.types_store.register(.{
            .content = new_content,
            .rank = Rank.min(vars.a.desc.rank, vars.b.desc.rank),
            .mark = Mark.none,
        });
        _ = try self.scratch.fresh_vars.append(self.scratch.gpa, var_);
        return var_;
    }

    // unification

    const Error = error{
        TypeMismatch,
        UnifyErr,
        NumberDoesNotFit,
        NegativeUnsignedInt,
        AllocatorError,
    };

    const NominalDirection = enum {
        a_is_nominal,
        b_is_nominal,
    };

    const max_depth_before_occurs = 8;

    fn unifyGuarded(self: *Self, a_var: Var, b_var: Var) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (self.types_store.checkVarsEquiv(a_var, b_var)) {
            .equiv => {
                // this means that the vars point to the same exact type
                // descriptor, so nothing needs to happen
                return;
            },
            .not_equiv => |vars| {
                if (self.skip_depth_check or self.depth < max_depth_before_occurs) {
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
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (vars.a.desc.content) {
            .flex => |flex| {
                try self.unifyFlex(vars, flex, vars.b.desc.content);
            },
            .rigid => |rigid| {
                try self.unifyRigid(vars, rigid, vars.b.desc.content);
            },
            .alias => |a_alias| {
                try self.unifyAlias(vars, a_alias, vars.b.desc.content);
            },
            .structure => |a_flat_type| {
                try self.unifyStructure(vars, a_flat_type, vars.b.desc.content);
            },
            .recursion_var => |a_rec_var| {
                try self.unifyRecursionVar(vars, a_rec_var, vars.b.desc.content);
            },
            .err => self.merge(vars, .err),
        }
    }

    /// Run a full occurs check on each variable, erroring if it is infinite
    /// or anonymous recursion
    ///
    /// This function is called when unify has recursed a sufficient depth that
    /// a recursive type seems likely.
    fn checkRecursive(self: *Self, vars: *const ResolvedVarDescs) Error!void {
        const a_occurs = occurs.occurs(self.types_store, self.occurs_scratch, vars.a.var_) catch return Error.AllocatorError;
        switch (a_occurs) {
            .not_recursive => {},
            .recursive_nominal => {},
            .recursive_anonymous => {
                return self.setUnifyErrAndThrow(UnifyErrCtx{ .recursion_anonymous = vars.a.var_ });
            },
            .infinite => {
                return self.setUnifyErrAndThrow(UnifyErrCtx{ .recursion_infinite = vars.a.var_ });
            },
        }

        const b_occurs = occurs.occurs(self.types_store, self.occurs_scratch, vars.b.var_) catch return Error.AllocatorError;
        switch (b_occurs) {
            .not_recursive => {},
            .recursive_nominal => {},
            .recursive_anonymous => {
                return self.setUnifyErrAndThrow(UnifyErrCtx{ .recursion_anonymous = vars.b.var_ });
            },
            .infinite => {
                return self.setUnifyErrAndThrow(UnifyErrCtx{ .recursion_infinite = vars.b.var_ });
            },
        }
    }

    // Unify flex //

    /// Unify when `a` was a flex
    fn unifyFlex(self: *Self, vars: *const ResolvedVarDescs, a_flex: Flex, b_content: Content) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (b_content) {
            .flex => |b_flex| {
                const mb_ident = blk: {
                    if (a_flex.name) |a_ident| {
                        break :blk a_ident;
                    } else {
                        break :blk b_flex.name;
                    }
                };

                const merged_constraints = try self.unifyStaticDispatchConstraints(a_flex.constraints, b_flex.constraints);
                self.merge(vars, Content{ .flex = .{
                    .name = mb_ident,
                    .constraints = merged_constraints,
                } });
            },
            .rigid => |b_rigid| {
                if (a_flex.constraints.len() > 0) {
                    // Record that we need to check constraints later
                    _ = self.scratch.deferred_constraints.append(self.scratch.gpa, DeferredConstraintCheck{
                        .var_ = vars.b.var_, // Since the vars are merge, we arbitrary choose b
                        .constraints = a_flex.constraints,
                    }) catch return Error.AllocatorError;
                }

                self.merge(vars, .{ .rigid = b_rigid });
            },
            .alias => |b_alias| {
                if (a_flex.constraints.len() == 0) {
                    self.merge(vars, b_content);
                } else {
                    // Merge against backing var, so we don't loose static dispatch constraints
                    const backing_var = self.types_store.getAliasBackingVar(b_alias);
                    try self.unifyGuarded(vars.a.var_, backing_var);
                }
            },
            .structure => {
                if (a_flex.constraints.len() > 0) {
                    // Record that we need to check constraints later
                    _ = self.scratch.deferred_constraints.append(self.scratch.gpa, DeferredConstraintCheck{
                        .var_ = vars.b.var_, // Since the vars are merge, we arbitrary choose b
                        .constraints = a_flex.constraints,
                    }) catch return Error.AllocatorError;
                }

                self.merge(vars, b_content);
            },
            .recursion_var => {
                if (a_flex.constraints.len() > 0) {
                    // Record that we need to check constraints later
                    _ = self.scratch.deferred_constraints.append(self.scratch.gpa, DeferredConstraintCheck{
                        .var_ = vars.b.var_,
                        .constraints = a_flex.constraints,
                    }) catch return Error.AllocatorError;
                }

                self.merge(vars, b_content);
            },
            .err => self.merge(vars, .err),
        }
    }

    // Unify rigid //

    /// Unify when `a` was a rigid
    fn unifyRigid(self: *Self, vars: *const ResolvedVarDescs, a_rigid: Rigid, b_content: Content) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (b_content) {
            .flex => |b_flex| {
                if (b_flex.constraints.len() > 0) {
                    // Record that we need to check constraints later
                    _ = self.scratch.deferred_constraints.append(self.scratch.gpa, DeferredConstraintCheck{
                        .var_ = vars.b.var_, // Since the vars are merge, we arbitrary choose b
                        .constraints = b_flex.constraints,
                    }) catch return Error.AllocatorError;
                }

                self.merge(vars, .{ .rigid = a_rigid });
            },
            .rigid => return error.TypeMismatch,
            .alias => return error.TypeMismatch,
            .structure => return error.TypeMismatch,
            .recursion_var => return error.TypeMismatch,
            .err => self.merge(vars, .err),
        }
    }

    // Unify alias //

    /// Unify when `a` was a alias
    fn unifyAlias(self: *Self, vars: *const ResolvedVarDescs, a_alias: Alias, b_content: Content) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        const backing_var = self.types_store.getAliasBackingVar(a_alias);

        switch (b_content) {
            .flex => |b_flex| {
                if (b_flex.constraints.len() == 0) {
                    self.merge(vars, Content{ .alias = a_alias });
                } else {
                    // Merge against backing var, so we don't loose static dispatch constraints
                    try self.unifyGuarded(backing_var, vars.b.var_);
                }
            },
            .rigid => |_| {
                try self.unifyGuarded(backing_var, vars.b.var_);
            },
            .alias => |b_alias| {
                const b_backing_var = self.types_store.getAliasBackingVar(b_alias);
                if (TypeIdent.eql(self.module_env.getIdentStore(), a_alias.ident, b_alias.ident)) {
                    try self.unifyTwoAliases(vars, a_alias, b_alias);
                } else {
                    try self.unifyGuarded(backing_var, b_backing_var);
                }
            },
            .structure => {
                // When unifying an alias with a concrete structure, we
                // want to preserve the alias for display while ensuring the
                // types are compatible.

                // First, we unify the concrete var with the alias backing var
                // IMPORTANT: The arg order here is important! Unifying
                // updates the second var to hold the type, and the first
                // var to redirect to the second
                try self.unifyGuarded(vars.b.var_, backing_var);

                // Next, we create a fresh alias (which internally points to `backing_var`),
                // then we redirect both a & b to the new alias.
                const fresh_alias_var = self.fresh(vars, .{ .alias = a_alias }) catch return Error.AllocatorError;
                self.types_store.setVarRedirect(vars.a.var_, fresh_alias_var) catch return Error.AllocatorError;
                self.types_store.setVarRedirect(vars.b.var_, fresh_alias_var) catch return Error.AllocatorError;
            },
            .recursion_var => |_| {
                // Unify alias backing var with recursion var
                try self.unifyGuarded(backing_var, vars.b.var_);
            },
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
        const trace = tracy.trace(@src());
        defer trace.end();

        if (a_alias.vars.nonempty.count != b_alias.vars.nonempty.count) {
            return error.TypeMismatch;
        }

        // Unify each pair of arguments
        const a_args_slice = self.types_store.sliceAliasArgs(a_alias);
        const b_args_slice = self.types_store.sliceAliasArgs(b_alias);
        for (a_args_slice, b_args_slice) |a_arg, b_arg| {
            try self.unifyGuarded(a_arg, b_arg);
        }

        // Rust compiler comment:
        // Don't report real_var mismatches, because they must always be surfaced higher, from the argument types.
        const a_backing_var = self.types_store.getAliasBackingVar(a_alias);
        const b_backing_var = self.types_store.getAliasBackingVar(b_alias);
        self.unifyGuarded(a_backing_var, b_backing_var) catch {};

        // Ensure the target variable has slots for the alias arguments
        self.merge(vars, vars.b.desc.content);
    }

    // Unify structure //

    /// Unify when `a` is a structure type
    fn unifyStructure(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_flat_type: FlatType,
        b_content: Content,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (b_content) {
            .flex => |b_flex| {
                if (b_flex.constraints.len() > 0) {
                    // Record that we need to check constraints later
                    _ = self.scratch.deferred_constraints.append(self.scratch.gpa, DeferredConstraintCheck{
                        .var_ = vars.b.var_, // Since the vars are merge, we arbitrary choose b
                        .constraints = b_flex.constraints,
                    }) catch return Error.AllocatorError;
                }

                self.merge(vars, Content{ .structure = a_flat_type });
            },
            .rigid => return error.TypeMismatch,
            .alias => |b_alias| {
                // When unifying an alias with a concrete structure, we
                // want to preserve the alias for display while ensuring the
                // types are compatible.

                const backing_var = self.types_store.getAliasBackingVar(b_alias);

                // First, we unify the concrete var with the alias backing var
                // IMPORTANT: The arg order here is important! Unifying
                // updates the second var to hold the type, and the first
                // var to redirect to the second
                try self.unifyGuarded(vars.a.var_, backing_var);

                // Next, we create a fresh alias (which internally points to `backing_var`),
                // then we redirect both a & b to the new alias.
                const fresh_alias_var = self.fresh(vars, .{ .alias = b_alias }) catch return Error.AllocatorError;
                self.types_store.setVarRedirect(vars.a.var_, fresh_alias_var) catch return Error.AllocatorError;
                self.types_store.setVarRedirect(vars.b.var_, fresh_alias_var) catch return Error.AllocatorError;
            },
            .structure => |b_flat_type| {
                try self.unifyFlatType(vars, a_flat_type, b_flat_type);
            },
            .recursion_var => |b_rec_var| {
                // When unifying structure with recursion var, unify with the structure
                // the recursion var points to
                try self.unifyGuarded(vars.a.var_, b_rec_var.structure);
            },
            .err => self.merge(vars, .err),
        }
    }

    // Unify recursion var //

    /// Unify when `a` is a recursion variable
    ///
    /// Equirecursive unification: Two recursive types unify if they are structurally
    /// equal up to their recursion points. RecursionVar marks these recursion points
    /// and prevents infinite expansion during unification.
    ///
    /// The key insight: when we encounter a RecursionVar, we unify with the structure
    /// it points to. The existing cycle detection in unifyGuarded (via checkVarsEquiv)
    /// ensures we don't infinitely recurse - if we've already unified these exact vars,
    /// we return early.
    fn unifyRecursionVar(
        self: *Self,
        vars: *const ResolvedVarDescs,
        a_rec_var: RecursionVar,
        b_content: Content,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (b_content) {
            .flex => |b_flex| {
                // RecursionVar can unify with flex - defer constraints and merge
                if (b_flex.constraints.len() > 0) {
                    // Record that we need to check constraints later
                    _ = self.scratch.deferred_constraints.append(self.scratch.gpa, DeferredConstraintCheck{
                        .var_ = vars.b.var_,
                        .constraints = b_flex.constraints,
                    }) catch return Error.AllocatorError;
                }
                self.merge(vars, vars.a.desc.content);
            },
            .rigid => {
                // RecursionVar cannot unify with rigid - rigid types have no structure to recurse into
                return error.TypeMismatch;
            },
            .alias => |b_alias| {
                // Unify with the alias backing var to preserve the alias structure
                // This allows RecursionVar to work through type aliases
                const backing_var = self.types_store.getAliasBackingVar(b_alias);
                try self.unifyGuarded(vars.a.var_, backing_var);
            },
            .structure => {
                // Unify the structure the recursion var points to with b's structure
                // This is equirecursive unification: unfold one level and continue
                try self.unifyGuarded(a_rec_var.structure, vars.b.var_);
            },
            .recursion_var => |b_rec_var| {
                // Both are recursion vars - the heart of equirecursive unification
                // We unify their structures. If they form a cycle, checkVarsEquiv
                // in unifyGuarded will detect it and prevent infinite recursion.
                try self.unifyGuarded(a_rec_var.structure, b_rec_var.structure);
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
        const trace = tracy.trace(@src());
        defer trace.end();

        switch (a_flat_type) {
            .tuple => |a_tuple| {
                switch (b_flat_type) {
                    .tuple => |b_tuple| {
                        try self.unifyTuple(vars, a_tuple, b_tuple);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .nominal_type => |a_type| {
                const a_backing_var = self.types_store.getNominalBackingVar(a_type);
                const a_backing_resolved = self.types_store.resolveVar(a_backing_var);
                if (a_backing_resolved.desc.content == .err) {
                    self.merge(vars, vars.b.desc.content);
                    return;
                }

                switch (b_flat_type) {
                    .nominal_type => |b_type| {
                        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
                        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
                        if (b_backing_resolved.desc.content == .err) {
                            self.merge(vars, vars.a.desc.content);
                            return;
                        }

                        try self.unifyNominalType(vars, a_type, b_type);
                    },
                    .tag_union => |b_tag_union| {
                        // Try to unify nominal tag union (a) with anonymous tag union (b)
                        try self.unifyTagUnionWithNominal(vars, a_type, a_backing_var, a_backing_resolved, b_tag_union, .a_is_nominal);
                    },
                    .empty_tag_union => {
                        // If this nominal is opaque and we're not in the origin module, error
                        if (!a_type.canLiftInner(self.module_env.module_name_idx)) {
                            return error.TypeMismatch;
                        }

                        if (a_backing_resolved.desc.content == .structure and
                            a_backing_resolved.desc.content.structure == .empty_tag_union)
                        {
                            self.merge(vars, vars.a.desc.content);
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
            .fn_pure => |a_func| {
                switch (b_flat_type) {
                    .fn_pure => |b_func| {
                        try self.unifyFunc(vars, a_func, b_func);
                        self.merge(vars, vars.a.desc.content);
                    },
                    .fn_unbound => |b_func| {
                        // pure unifies with unbound -> pure
                        try self.unifyFunc(vars, a_func, b_func);
                        self.merge(vars, vars.a.desc.content);
                    },
                    .fn_effectful => {
                        // pure cannot unify with effectful
                        return error.TypeMismatch;
                    },
                    else => return error.TypeMismatch,
                }
            },
            .fn_effectful => |a_func| {
                switch (b_flat_type) {
                    .fn_effectful => |b_func| {
                        try self.unifyFunc(vars, a_func, b_func);
                        self.merge(vars, vars.a.desc.content);
                    },
                    .fn_unbound => |b_func| {
                        // effectful unifies with unbound -> effectful
                        try self.unifyFunc(vars, a_func, b_func);
                        self.merge(vars, vars.a.desc.content);
                    },
                    .fn_pure => {
                        // effectful cannot unify with pure
                        return error.TypeMismatch;
                    },
                    else => return error.TypeMismatch,
                }
            },
            .fn_unbound => |a_func| {
                switch (b_flat_type) {
                    .fn_pure => |b_func| {
                        // unbound unifies with pure -> pure
                        try self.unifyFunc(vars, a_func, b_func);
                        self.merge(vars, vars.b.desc.content);
                    },
                    .fn_effectful => |b_func| {
                        // unbound unifies with effectful -> effectful
                        try self.unifyFunc(vars, a_func, b_func);
                        self.merge(vars, vars.b.desc.content);
                    },
                    .fn_unbound => |b_func| {
                        // unbound unifies with unbound -> unbound
                        try self.unifyFunc(vars, a_func, b_func);
                        self.merge(vars, vars.a.desc.content);
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
                        try self.unifyTwoRecords(
                            vars,
                            a_record.fields,
                            .{ .ext = a_record.ext },
                            b_record.fields,
                            .{ .ext = b_record.ext },
                        );
                    },
                    .record_unbound => |b_fields| {
                        try self.unifyTwoRecords(
                            vars,
                            a_record.fields,
                            .{ .ext = a_record.ext },
                            b_fields,
                            .unbound,
                        );
                    },
                    else => return error.TypeMismatch,
                }
            },
            .record_unbound => |a_fields| {
                switch (b_flat_type) {
                    .empty_record => {
                        if (a_fields.len() == 0) {
                            // Both are empty, merge as empty_record
                            self.merge(vars, Content{ .structure = .empty_record });
                        } else {
                            return error.TypeMismatch;
                        }
                    },
                    .record => |b_record| {
                        try self.unifyTwoRecords(
                            vars,
                            a_fields,
                            .unbound,
                            b_record.fields,
                            .{ .ext = b_record.ext },
                        );
                    },
                    .record_unbound => |b_fields| {
                        try self.unifyTwoRecords(
                            vars,
                            a_fields,
                            .unbound,
                            b_fields,
                            .unbound,
                        );
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
                    .record_unbound => |b_fields| {
                        if (b_fields.len() == 0) {
                            // Both are empty, merge as empty_record
                            self.merge(vars, Content{ .structure = .empty_record });
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
                    .nominal_type => |b_type| {
                        // If this nominal is opaque and we're not in the origin module, error
                        if (!b_type.canLiftInner(self.module_env.module_name_idx)) {
                            return error.TypeMismatch;
                        }

                        // Try to unify anonymous tag union (a) with nominal tag union (b)
                        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
                        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
                        if (b_backing_resolved.desc.content == .err) {
                            self.merge(vars, vars.a.desc.content);
                            return;
                        }
                        try self.unifyTagUnionWithNominal(vars, b_type, b_backing_var, b_backing_resolved, a_tag_union, .b_is_nominal);
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
                    .nominal_type => |b_type| {
                        // Try to unify empty tag union (a) with nominal tag union (b)
                        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
                        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
                        if (b_backing_resolved.desc.content == .err) {
                            self.merge(vars, vars.a.desc.content);
                            return;
                        }

                        // Check if the nominal's backing is also an empty tag union
                        if (b_backing_resolved.desc.content == .structure and
                            b_backing_resolved.desc.content.structure == .empty_tag_union)
                        {
                            // Both are empty, unify with the nominal
                            self.merge(vars, vars.b.desc.content);
                        } else {
                            // Nominal has a non-empty backing, can't unify
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
        const trace = tracy.trace(@src());
        defer trace.end();

        if (a_tuple.elems.len() != b_tuple.elems.len()) {
            return error.TypeMismatch;
        }

        const a_elems = self.types_store.sliceVars(a_tuple.elems);
        const b_elems = self.types_store.sliceVars(b_tuple.elems);
        for (a_elems, b_elems) |a_elem, b_elem| {
            try self.unifyGuarded(a_elem, b_elem);
        }

        self.merge(vars, vars.b.desc.content);
    }

    // Unify nominal type //

    /// Unify when `a` was a nominal type
    fn unifyNominalType(self: *Self, vars: *const ResolvedVarDescs, a_type: NominalType, b_type: NominalType) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // Check if either nominal type has an invalid backing variable
        const a_backing_var = self.types_store.getNominalBackingVar(a_type);
        const a_backing_resolved = self.types_store.resolveVar(a_backing_var);
        if (a_backing_resolved.desc.content == .err) {
            // Invalid nominal type - treat as transparent
            self.merge(vars, vars.b.desc.content);
            return;
        }

        const b_backing_var = self.types_store.getNominalBackingVar(b_type);
        const b_backing_resolved = self.types_store.resolveVar(b_backing_var);
        if (b_backing_resolved.desc.content == .err) {
            // Invalid nominal type - treat as transparent
            self.merge(vars, vars.a.desc.content);
            return;
        }

        if (!TypeIdent.eql(self.module_env.getIdentStore(), a_type.ident, b_type.ident)) {
            return error.TypeMismatch;
        }

        if (a_type.vars.nonempty.count != b_type.vars.nonempty.count) {
            return error.TypeMismatch;
        }

        // Unify each pair of arguments using iterators
        const a_slice = self.types_store.sliceNominalArgs(a_type);
        const b_slice = self.types_store.sliceNominalArgs(b_type);
        for (a_slice, b_slice) |a_arg, b_arg| {
            try self.unifyGuarded(a_arg, b_arg);
        }

        // Note that we *do not* unify backing variable

        self.merge(vars, vars.b.desc.content);
    }

    fn unifyTagUnionWithNominal(
        self: *Self,
        vars: *const ResolvedVarDescs,
        nominal_type: NominalType,
        nominal_backing_var: Var,
        nominal_backing_resolved: ResolvedVarDesc,
        anon_tag_union: TagUnion,
        direction: NominalDirection,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // If this nominal is opaque and we're not in the origin module, error
        if (!nominal_type.canLiftInner(self.module_env.module_name_idx)) {
            return error.TypeMismatch;
        }

        // Check if the nominal's backing type is a tag union (including empty)
        const nominal_backing_content = nominal_backing_resolved.desc.content;
        if (nominal_backing_content != .structure) {
            return error.TypeMismatch;
        }

        const nominal_backing_flat = nominal_backing_content.structure;

        // Handle empty tag union case
        if (nominal_backing_flat == .empty_tag_union) {
            // The nominal's backing is an empty tag union []
            // The anon_tag_union should also be empty for unification to succeed
            if (anon_tag_union.tags.len() == 0) {
                // Both are empty - unify the extension variables
                try self.unifyGuarded(anon_tag_union.ext, nominal_backing_var);

                // Merge to the NOMINAL type (not the tag union)
                switch (direction) {
                    .a_is_nominal => self.merge(vars, vars.a.desc.content),
                    .b_is_nominal => self.merge(vars, vars.b.desc.content),
                }
                return;
            } else {
                // Anon has tags but nominal is empty
                return error.TypeMismatch;
            }
        }

        if (nominal_backing_flat != .tag_union) {
            // Nominal's backing is not a tag union (could be record, tuple, etc.)
            // Cannot unify anonymous tag union with non-tag-union nominal
            return error.TypeMismatch;
        }

        const nominal_backing_tag_union = nominal_backing_flat.tag_union;

        // Unify the two tag unions directly (without modifying the nominal's backing)
        // This checks that:
        // - All tags in the anonymous union exist in the nominal union
        // - Payload types match
        // - Extension variables are compatible
        try self.unifyTwoTagUnions(vars, anon_tag_union, nominal_backing_tag_union);

        // If we get here, unification succeeded!
        // Merge to the NOMINAL type (not the tag union)
        // This is the key: the nominal type "wins"
        switch (direction) {
            .a_is_nominal => {
                // Merge to a (which is the nominal)
                self.merge(vars, vars.a.desc.content);
            },
            .b_is_nominal => {
                // Merge to b (which is the nominal)
                self.merge(vars, vars.b.desc.content);
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
        _: *const ResolvedVarDescs,
        a_func: Func,
        b_func: Func,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        if (a_func.args.len() != b_func.args.len()) {
            return error.TypeMismatch;
        }

        const a_args = self.types_store.sliceVars(a_func.args);
        const b_args = self.types_store.sliceVars(b_func.args);
        for (a_args, b_args) |a_arg, b_arg| {
            try self.unifyGuarded(a_arg, b_arg);
        }

        try self.unifyGuarded(a_func.ret, b_func.ret);
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
        a_fields: RecordField.SafeMultiList.Range,
        a_ext: RecordExt,
        b_fields: RecordField.SafeMultiList.Range,
        b_ext: RecordExt,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // First, unwrap all fields for record, erroring if we encounter an
        // invalid record ext var
        const a_gathered_fields = try self.gatherRecordFields(a_fields, a_ext);
        const b_gathered_fields = try self.gatherRecordFields(b_fields, b_ext);

        // Then partition the fields
        const partitioned = Self.partitionFields(
            self.module_env.getIdentStore(),
            self.scratch,
            a_gathered_fields.range,
            b_gathered_fields.range,
        ) catch return Error.AllocatorError;

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

        const a_gathered_ext = blk: {
            switch (a_gathered_fields.ext) {
                .unbound => break :blk self.fresh(vars, .{ .flex = Flex.init() }) catch return Error.AllocatorError,
                .ext => |ext_var| break :blk ext_var,
            }
        };
        const b_gathered_ext = blk: {
            switch (b_gathered_fields.ext) {
                .unbound => break :blk self.fresh(vars, .{ .flex = Flex.init() }) catch return Error.AllocatorError,
                .ext => |ext_var| break :blk ext_var,
            }
        };

        // Unify fields
        switch (fields_ext) {
            .exactly_the_same => {
                // Unify exts
                try self.unifyGuarded(a_gathered_ext, b_gathered_ext);

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    self.scratch.in_both_fields.sliceRange(partitioned.in_both),
                    null,
                    null,
                    a_gathered_ext,
                );
            },
            .a_extends_b => {
                // Create a new variable of a record with only a's uniq fields
                // This copies fields from scratch into type_store
                const only_in_a_fields_range = self.types_store.appendRecordFields(
                    self.scratch.only_in_a_fields.sliceRange(partitioned.only_in_a),
                ) catch return Error.AllocatorError;
                const only_in_a_var = self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_a_fields_range,
                    .ext = a_gathered_ext,
                } } }) catch return Error.AllocatorError;

                // Unify the sub record with b's ext
                try self.unifyGuarded(only_in_a_var, b_gathered_ext);

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    self.scratch.in_both_fields.sliceRange(partitioned.in_both),
                    null,
                    null,
                    only_in_a_var,
                );
            },
            .b_extends_a => {
                // Create a new variable of a record with only b's uniq fields
                // This copies fields from scratch into type_store
                const only_in_b_fields_range = self.types_store.appendRecordFields(
                    self.scratch.only_in_b_fields.sliceRange(partitioned.only_in_b),
                ) catch return Error.AllocatorError;
                const only_in_b_var = self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_b_fields_range,
                    .ext = b_gathered_ext,
                } } }) catch return Error.AllocatorError;

                // Unify the sub record with a's ext
                try self.unifyGuarded(a_gathered_ext, only_in_b_var);

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    self.scratch.in_both_fields.sliceRange(partitioned.in_both),
                    null,
                    null,
                    only_in_b_var,
                );
            },
            .both_extend => {
                // Create a new variable of a record with only a's uniq fields
                // This copies fields from scratch into type_store
                const only_in_a_fields_range = self.types_store.appendRecordFields(
                    self.scratch.only_in_a_fields.sliceRange(partitioned.only_in_a),
                ) catch return Error.AllocatorError;
                const only_in_a_var = self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_a_fields_range,
                    .ext = a_gathered_ext,
                } } }) catch return Error.AllocatorError;

                // Create a new variable of a record with only b's uniq fields
                // This copies fields from scratch into type_store
                const only_in_b_fields_range = self.types_store.appendRecordFields(
                    self.scratch.only_in_b_fields.sliceRange(partitioned.only_in_b),
                ) catch return Error.AllocatorError;
                const only_in_b_var = self.fresh(vars, Content{ .structure = FlatType{ .record = .{
                    .fields = only_in_b_fields_range,
                    .ext = b_gathered_ext,
                } } }) catch return Error.AllocatorError;

                // Create a new ext var
                const new_ext_var = self.fresh(vars, .{ .flex = Flex.init() }) catch return Error.AllocatorError;

                // Unify the sub records with exts
                try self.unifyGuarded(a_gathered_ext, only_in_b_var);
                try self.unifyGuarded(only_in_a_var, b_gathered_ext);

                // Unify shared fields
                // This copies fields from scratch into type_store
                try self.unifySharedFields(
                    vars,
                    self.scratch.in_both_fields.sliceRange(partitioned.in_both),
                    self.scratch.only_in_a_fields.sliceRange(partitioned.only_in_a),
                    self.scratch.only_in_b_fields.sliceRange(partitioned.only_in_b),
                    new_ext_var,
                );
            },
        }
    }

    const FieldsExtension = enum { exactly_the_same, a_extends_b, b_extends_a, both_extend };

    const RecordExt = union(enum) { ext: Var, unbound };

    const GatheredFields = struct { ext: RecordExt, range: RecordFieldSafeList.Range };

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
    fn gatherRecordFields(self: *Self, record_fields: RecordField.SafeMultiList.Range, record_ext: RecordExt) Error!GatheredFields {
        // first, copy from the store's MultiList record fields array into scratch's
        // regular list, capturing the insertion range
        var range = self.scratch.copyGatherFieldsFromMultiList(
            &self.types_store.record_fields,
            record_fields,
        ) catch return Error.AllocatorError;

        // TODO: Below if we run into a record with the same field, then we
        // prefer the leftmost field. But we should probably unify the dups

        // then recursiv
        var ext = record_ext;
        var guard = types_mod.debug.IterationGuard.init("gatherRecordFields");
        while (true) {
            guard.tick();
            switch (ext) {
                .unbound => {
                    return .{ .ext = ext, .range = range };
                },
                .ext => |ext_var| {
                    switch (self.types_store.resolveVar(ext_var).desc.content) {
                        .flex => {
                            return .{ .ext = .{ .ext = ext_var }, .range = range };
                        },
                        .rigid => {
                            return .{ .ext = .{ .ext = ext_var }, .range = range };
                        },
                        .alias => |alias| {
                            ext = .{ .ext = self.types_store.getAliasBackingVar(alias) };
                        },
                        .structure => |flat_type| {
                            switch (flat_type) {
                                .record => |ext_record| {
                                    const next_fields = self.types_store.record_fields.sliceRange(ext_record.fields);
                                    const already_gathered = self.scratch.gathered_fields.sliceRange(range);

                                    for (next_fields.items(.name), next_fields.items(.var_)) |name, var_| {
                                        // Check if field name already exists (O(n) but fields are small)
                                        const already_exists = for (already_gathered) |existing| {
                                            if (existing.name == name) break true;
                                        } else false;

                                        // Only append if NOT already present (Map.union left-bias)
                                        if (!already_exists) {
                                            _ = self.scratch.gathered_fields.append(
                                                self.scratch.gpa,
                                                RecordField{ .name = name, .var_ = var_ },
                                            ) catch return Error.AllocatorError;
                                            range.count += 1;
                                        }
                                    }
                                    ext = .{ .ext = ext_record.ext };
                                },
                                .record_unbound => |fields| {
                                    const next_fields = self.types_store.record_fields.sliceRange(fields);
                                    const already_gathered = self.scratch.gathered_fields.sliceRange(range);

                                    for (next_fields.items(.name), next_fields.items(.var_)) |name, var_| {
                                        const already_exists = for (already_gathered) |existing| {
                                            if (existing.name == name) break true;
                                        } else false;

                                        if (!already_exists) {
                                            _ = self.scratch.gathered_fields.append(
                                                self.scratch.gpa,
                                                RecordField{ .name = name, .var_ = var_ },
                                            ) catch return Error.AllocatorError;
                                            range.count += 1;
                                        }
                                    }
                                    return .{ .ext = ext, .range = range };
                                },
                                .empty_record => {
                                    return .{ .ext = ext, .range = range };
                                },
                                else => try self.setUnifyErrAndThrow(.{ .invalid_record_ext = ext_var }),
                            }
                        },
                        else => try self.setUnifyErrAndThrow(.{ .invalid_record_ext = ext_var }),
                    }
                },
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
    fn partitionFields(
        ident_store: *const Ident.Store,
        scratch: *Scratch,
        a_fields_range: RecordFieldSafeList.Range,
        b_fields_range: RecordFieldSafeList.Range,
    ) std.mem.Allocator.Error!PartitionedRecordFields {
        // First sort the fields
        const a_fields = scratch.gathered_fields.sliceRange(a_fields_range);
        std.mem.sort(RecordField, a_fields, ident_store, comptime RecordField.sortByNameAsc);
        const b_fields = scratch.gathered_fields.sliceRange(b_fields_range);
        std.mem.sort(RecordField, b_fields, ident_store, comptime RecordField.sortByNameAsc);

        // Get the start of index of the new range
        const a_fields_start: u32 = @intCast(scratch.only_in_a_fields.len());
        const b_fields_start: u32 = @intCast(scratch.only_in_b_fields.len());
        const both_fields_start: u32 = @intCast(scratch.in_both_fields.len());

        // Iterate over the fields in order, grouping them
        var a_i: usize = 0;
        var b_i: usize = 0;
        while (a_i < a_fields.len and b_i < b_fields.len) {
            const a_next = a_fields[a_i];
            const b_next = b_fields[b_i];
            const ord = RecordField.orderByName(ident_store, a_next, b_next);
            switch (ord) {
                .eq => {
                    _ = try scratch.in_both_fields.append(scratch.gpa, TwoRecordFields{
                        .a = a_next,
                        .b = b_next,
                    });
                    a_i = a_i + 1;
                    b_i = b_i + 1;
                },
                .lt => {
                    _ = try scratch.only_in_a_fields.append(scratch.gpa, a_next);
                    a_i = a_i + 1;
                },
                .gt => {
                    _ = try scratch.only_in_b_fields.append(scratch.gpa, b_next);
                    b_i = b_i + 1;
                },
            }
        }

        // If b was shorter, add the extra a elems
        while (a_i < a_fields.len) {
            const a_next = a_fields[a_i];
            _ = try scratch.only_in_a_fields.append(scratch.gpa, a_next);
            a_i = a_i + 1;
        }

        // If a was shorter, add the extra b elems
        while (b_i < b_fields.len) {
            const b_next = b_fields[b_i];
            _ = try scratch.only_in_b_fields.append(scratch.gpa, b_next);
            b_i = b_i + 1;
        }

        // Return the ranges
        return .{
            .only_in_a = scratch.only_in_a_fields.rangeToEnd(a_fields_start),
            .only_in_b = scratch.only_in_b_fields.rangeToEnd(b_fields_start),
            .in_both = scratch.in_both_fields.rangeToEnd(both_fields_start),
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
        const trace = tracy.trace(@src());
        defer trace.end();

        // First, unify all field types. This may cause nested record unifications
        // which will append their own fields to the store. We must NOT interleave
        // our field appends with these nested calls.
        for (shared_fields) |shared| {
            try self.unifyGuarded(shared.a.var_, shared.b.var_);
        }

        // Now that all nested unifications are complete, append OUR fields.
        // This ensures our fields form a contiguous range.
        const range_start: u32 = self.types_store.record_fields.len();

        for (shared_fields) |shared| {
            _ = self.types_store.appendRecordFields(&[_]RecordField{.{
                .name = shared.b.name,
                .var_ = shared.b.var_,
            }}) catch return Error.AllocatorError;
        }

        // Append combined fields
        if (mb_a_extended_fields) |extended_fields| {
            _ = self.types_store.appendRecordFields(extended_fields) catch return Error.AllocatorError;
        }
        if (mb_b_extended_fields) |extended_fields| {
            _ = self.types_store.appendRecordFields(extended_fields) catch return Error.AllocatorError;
        }

        // Merge vars - now the range correctly contains only THIS record's fields
        self.merge(vars, Content{ .structure = FlatType{ .record = .{
            .fields = self.types_store.record_fields.rangeToEnd(range_start),
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
        const trace = tracy.trace(@src());
        defer trace.end();

        // First, unwrap all fields for tag unions, erroring if we encounter an
        // invalid record ext var
        const a_gathered_tags = try self.gatherTagUnionTags(a_tag_union);
        const b_gathered_tags = try self.gatherTagUnionTags(b_tag_union);

        // Then partition the tags
        const partitioned = Self.partitionTags(
            self.module_env.getIdentStore(),
            self.scratch,
            a_gathered_tags.range,
            b_gathered_tags.range,
        ) catch return Error.AllocatorError;

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
                    self.scratch.in_both_tags.sliceRange(partitioned.in_both),
                    null,
                    null,
                    a_gathered_tags.ext,
                );
            },
            .a_extends_b => {
                // Create a new variable of a tag_union with only a's uniq tags
                // This copies tags from scratch into type_store
                const only_in_a_tags_range = self.types_store.appendTags(
                    self.scratch.only_in_a_tags.sliceRange(partitioned.only_in_a),
                ) catch return Error.AllocatorError;
                const only_in_a_var = self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_a_tags_range,
                    .ext = a_gathered_tags.ext,
                } } }) catch return Error.AllocatorError;

                // Unify the sub tag_union with b's ext
                try self.unifyGuarded(only_in_a_var, b_gathered_tags.ext);

                // Unify shared tags
                // This copies tags from scratch into type_store
                try self.unifySharedTags(
                    vars,
                    self.scratch.in_both_tags.sliceRange(partitioned.in_both),
                    null,
                    null,
                    only_in_a_var,
                );
            },
            .b_extends_a => {
                // Create a new variable of a tag_union with only b's uniq tags
                // This copies tags from scratch into type_store
                const only_in_b_tags_range = self.types_store.appendTags(
                    self.scratch.only_in_b_tags.sliceRange(partitioned.only_in_b),
                ) catch return Error.AllocatorError;
                const only_in_b_var = self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_b_tags_range,
                    .ext = b_gathered_tags.ext,
                } } }) catch return Error.AllocatorError;

                // Unify the sub tag_union with a's ext
                try self.unifyGuarded(a_gathered_tags.ext, only_in_b_var);

                // Unify shared tags
                // This copies tags from scratch into type_store
                try self.unifySharedTags(
                    vars,
                    self.scratch.in_both_tags.sliceRange(partitioned.in_both),
                    null,
                    null,
                    only_in_b_var,
                );
            },
            .both_extend => {
                // Create a shared extension variable first
                // This is critical: both only_in_a_var and only_in_b_var must use this
                // shared extension to avoid creating circular type references
                const new_ext_var = self.fresh(vars, .{ .flex = Flex.init() }) catch return Error.AllocatorError;

                // Create a new variable of a tag_union with only a's uniq tags
                // Uses new_ext_var (not a_gathered_tags.ext) to prevent cycles
                const only_in_a_tags_range = self.types_store.appendTags(
                    self.scratch.only_in_a_tags.sliceRange(partitioned.only_in_a),
                ) catch return Error.AllocatorError;
                const only_in_a_var = self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_a_tags_range,
                    .ext = new_ext_var,
                } } }) catch return Error.AllocatorError;

                // Create a new variable of a tag_union with only b's uniq tags
                // Uses new_ext_var (not b_gathered_tags.ext) to prevent cycles
                const only_in_b_tags_range = self.types_store.appendTags(
                    self.scratch.only_in_b_tags.sliceRange(partitioned.only_in_b),
                ) catch return Error.AllocatorError;
                const only_in_b_var = self.fresh(vars, Content{ .structure = FlatType{ .tag_union = .{
                    .tags = only_in_b_tags_range,
                    .ext = new_ext_var,
                } } }) catch return Error.AllocatorError;

                // Unify the sub tag_unions with exts
                try self.unifyGuarded(a_gathered_tags.ext, only_in_b_var);
                try self.unifyGuarded(only_in_a_var, b_gathered_tags.ext);

                // Unify shared tags
                // Include all tags in the merged type - both shared and unique
                // The unique tags must be included because the merged type is what
                // callers see, and the extension chain via only_in_a_var/only_in_b_var
                // is for proper type equality, not for visibility
                try self.unifySharedTags(
                    vars,
                    self.scratch.in_both_tags.sliceRange(partitioned.in_both),
                    self.scratch.only_in_a_tags.sliceRange(partitioned.only_in_a),
                    self.scratch.only_in_b_tags.sliceRange(partitioned.only_in_b),
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
    fn gatherTagUnionTags(self: *Self, tag_union: TagUnion) Error!GatheredTags {
        // first, copy from the store's MultiList record fields array into scratch's
        // regular list, capturing the insertion range
        var range = self.scratch.copyGatherTagsFromMultiList(
            &self.types_store.tags,
            tag_union.tags,
        ) catch return Error.AllocatorError;

        // then loop gathering extensible tags
        var ext_var = tag_union.ext;
        var guard = types_mod.debug.IterationGuard.init("gatherTagUnionTags");
        while (true) {
            guard.tick();
            switch (self.types_store.resolveVar(ext_var).desc.content) {
                .flex => {
                    return .{ .ext = ext_var, .range = range };
                },
                .rigid => {
                    return .{ .ext = ext_var, .range = range };
                },
                .alias => |alias| {
                    ext_var = self.types_store.getAliasBackingVar(alias);
                },
                .structure => |flat_type| {
                    switch (flat_type) {
                        .tag_union => |ext_tag_union| {
                            const next_range = self.scratch.copyGatherTagsFromMultiList(
                                &self.types_store.tags,
                                ext_tag_union.tags,
                            ) catch return Error.AllocatorError;
                            range.count += next_range.count;
                            ext_var = ext_tag_union.ext;
                        },
                        .empty_tag_union => {
                            return .{ .ext = ext_var, .range = range };
                        },
                        else => try self.setUnifyErrAndThrow(.{ .invalid_tag_union_ext = ext_var }),
                    }
                },
                else => try self.setUnifyErrAndThrow(.{ .invalid_tag_union_ext = ext_var }),
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
    fn partitionTags(
        ident_store: *const Ident.Store,
        scratch: *Scratch,
        a_tags_range: TagSafeList.Range,
        b_tags_range: TagSafeList.Range,
    ) std.mem.Allocator.Error!PartitionedTags {
        // First sort the tags
        const a_tags = scratch.gathered_tags.sliceRange(a_tags_range);
        std.mem.sort(Tag, a_tags, ident_store, comptime Tag.sortByNameAsc);
        const b_tags = scratch.gathered_tags.sliceRange(b_tags_range);
        std.mem.sort(Tag, b_tags, ident_store, comptime Tag.sortByNameAsc);

        // Get the start of index of the new range
        const a_tags_start: u32 = @intCast(scratch.only_in_a_tags.len());
        const b_tags_start: u32 = @intCast(scratch.only_in_b_tags.len());
        const both_tags_start: u32 = @intCast(scratch.in_both_tags.len());

        // Iterate over the tags in order, grouping them
        var a_i: usize = 0;
        var b_i: usize = 0;
        while (a_i < a_tags.len and b_i < b_tags.len) {
            const a_next = a_tags[a_i];
            const b_next = b_tags[b_i];
            const ord = Tag.orderByName(ident_store, a_next, b_next);
            switch (ord) {
                .eq => {
                    _ = try scratch.in_both_tags.append(scratch.gpa, TwoTags{ .a = a_next, .b = b_next });
                    a_i = a_i + 1;
                    b_i = b_i + 1;
                },
                .lt => {
                    _ = try scratch.only_in_a_tags.append(scratch.gpa, a_next);
                    a_i = a_i + 1;
                },
                .gt => {
                    _ = try scratch.only_in_b_tags.append(scratch.gpa, b_next);
                    b_i = b_i + 1;
                },
            }
        }

        // If b was shorter, add the extra a elems
        while (a_i < a_tags.len) {
            const a_next = a_tags[a_i];
            _ = try scratch.only_in_a_tags.append(scratch.gpa, a_next);
            a_i = a_i + 1;
        }

        // If a was shorter, add the extra b elems
        while (b_i < b_tags.len) {
            const b_next = b_tags[b_i];
            _ = try scratch.only_in_b_tags.append(scratch.gpa, b_next);
            b_i = b_i + 1;
        }

        // Return the ranges
        return .{
            .only_in_a = scratch.only_in_a_tags.rangeToEnd(a_tags_start),
            .only_in_b = scratch.only_in_b_tags.rangeToEnd(b_tags_start),
            .in_both = scratch.in_both_tags.rangeToEnd(both_tags_start),
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
        const trace = tracy.trace(@src());
        defer trace.end();

        // IMPORTANT: First unify all shared tag arguments BEFORE recording range_start.
        // This is because unifyGuarded may trigger recursive unifications that also
        // append tags to the global tags list. We need to record range_start AFTER
        // all inner unifications complete, so we only capture this level's tags.
        for (shared_tags) |tags| {
            const tag_a_args = self.types_store.sliceVars(tags.a.args);
            const tag_b_args = self.types_store.sliceVars(tags.b.args);

            if (tag_a_args.len != tag_b_args.len) return error.TypeMismatch;

            for (tag_a_args, tag_b_args) |a_arg, b_arg| {
                try self.unifyGuarded(a_arg, b_arg);
            }
        }

        // NOW record range_start after all inner unifications are done
        const range_start: u32 = self.types_store.tags.len();

        // Append this level's shared tags
        for (shared_tags) |tags| {
            _ = self.types_store.appendTags(&[_]Tag{.{
                .name = tags.b.name,
                .args = tags.b.args,
            }}) catch return Error.AllocatorError;
        }

        // Append combined tags
        if (mb_a_extended_tags) |extended_tags| {
            _ = self.types_store.appendTags(extended_tags) catch return Error.AllocatorError;
        }
        if (mb_b_extended_tags) |extended_tags| {
            _ = self.types_store.appendTags(extended_tags) catch return Error.AllocatorError;
        }

        // Merge vars
        self.merge(vars, Content{ .structure = FlatType{ .tag_union = .{
            .tags = self.types_store.tags.rangeToEnd(range_start),
            .ext = ext,
        } } });
    }

    // constraints //

    fn unifyStaticDispatchConstraints(
        self: *Self,
        a_constraints: StaticDispatchConstraint.SafeList.Range,
        b_constraints: StaticDispatchConstraint.SafeList.Range,
    ) Error!StaticDispatchConstraint.SafeList.Range {
        const a_len = a_constraints.len();
        const b_len = b_constraints.len();

        // Early exits for empty ranges
        if (a_len == 0 and b_len == 0) {
            return .empty();
        } else if (a_len == 0 and b_len > 0) {
            return b_constraints;
        } else if (a_len > 0 and b_len == 0) {
            return a_constraints;
        }

        // Partition constraints
        const partitioned = self.partitionStaticDispatchConstraints(a_constraints, b_constraints) catch return Error.AllocatorError;

        // Unify shared constraints
        if (partitioned.in_both.len() > 0) {
            for (self.scratch.in_both_static_dispatch_constraints.sliceRange(partitioned.in_both)) |two_constraints| {
                // TODO: Catch type mismatch and throw a custom error message?
                try self.unifyStaticDispatchConstraint(two_constraints.a, two_constraints.b);
            }
        }

        const top: u32 = @intCast(self.types_store.static_dispatch_constraints.len());

        // Ensure we have enough memory for the new contiguous list
        const capacity = partitioned.in_both.len() + partitioned.only_in_a.len() + partitioned.only_in_b.len();
        self.types_store.static_dispatch_constraints.items.ensureUnusedCapacity(
            self.types_store.gpa,
            capacity,
        ) catch return Error.AllocatorError;

        for (self.scratch.in_both_static_dispatch_constraints.sliceRange(partitioned.in_both)) |two_constraints| {
            // Here, we append the constraint's b, but since a & b, it doesn't actually matter
            self.types_store.static_dispatch_constraints.items.appendAssumeCapacity(two_constraints.b);
        }
        for (self.scratch.only_in_a_static_dispatch_constraints.sliceRange(partitioned.only_in_a)) |only_a| {
            self.types_store.static_dispatch_constraints.items.appendAssumeCapacity(only_a);
        }
        for (self.scratch.only_in_b_static_dispatch_constraints.sliceRange(partitioned.only_in_b)) |only_b| {
            self.types_store.static_dispatch_constraints.items.appendAssumeCapacity(only_b);
        }

        return self.types_store.static_dispatch_constraints.rangeToEnd(top);
    }

    /// Unify two static dispatch constraints
    fn unifyStaticDispatchConstraint(
        self: *Self,
        a_constraint: StaticDispatchConstraint,
        b_constraint: StaticDispatchConstraint,
    ) Error!void {
        const trace = tracy.trace(@src());
        defer trace.end();

        // Self-referential constraints like `a.plus : a, a -> a` are valid and expected.
        // To prevent infinite recursion when unifying them, we use variable marks to detect
        // if we're already in the process of unifying these constraint function variables.
        //
        // This works together with the occurs check in occurs.zig which follows constraints
        // to detect truly infinite types.
        const a_desc = self.types_store.resolveVar(a_constraint.fn_var);
        const b_desc = self.types_store.resolveVar(b_constraint.fn_var);

        // Check if either variable is marked as "visited" (currently being unified)
        if (a_desc.desc.mark == .visited or b_desc.desc.mark == .visited) {
            // Already unifying these constraint functions - skip to prevent infinite recursion
            return;
        }

        // Mark variables as being unified
        self.types_store.setDescMark(a_desc.desc_idx, .visited);
        self.types_store.setDescMark(b_desc.desc_idx, .visited);

        // Unify the constraint function types
        const result = self.unifyGuarded(a_constraint.fn_var, b_constraint.fn_var);

        // Unmark variables
        self.types_store.setDescMark(a_desc.desc_idx, .none);
        self.types_store.setDescMark(b_desc.desc_idx, .none);

        try result;
    }

    const PartitionedStaticDispatchConstraints = struct {
        only_in_a: StaticDispatchConstraint.SafeList.Range,
        only_in_b: StaticDispatchConstraint.SafeList.Range,
        in_both: TwoStaticDispatchConstraints.SafeList.Range,
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
    fn partitionStaticDispatchConstraints(
        self: *const Self,
        a_constraints_range: StaticDispatchConstraint.SafeList.Range,
        b_constraints_range: StaticDispatchConstraint.SafeList.Range,
    ) std.mem.Allocator.Error!PartitionedStaticDispatchConstraints {
        const ident_store = self.module_env.getIdentStore();
        const scratch = self.scratch;

        // First sort the fields
        const a_constraints = self.types_store.static_dispatch_constraints.sliceRange(a_constraints_range);
        std.mem.sort(StaticDispatchConstraint, a_constraints, ident_store, comptime StaticDispatchConstraint.sortByFnNameAsc);
        const b_constraints = self.types_store.static_dispatch_constraints.sliceRange(b_constraints_range);
        std.mem.sort(StaticDispatchConstraint, b_constraints, ident_store, comptime StaticDispatchConstraint.sortByFnNameAsc);

        // Get the start of index of the new range
        const a_constraints_start: u32 = @intCast(scratch.only_in_a_static_dispatch_constraints.len());
        const b_constraints_start: u32 = @intCast(scratch.only_in_b_static_dispatch_constraints.len());
        const both_constraints_start: u32 = @intCast(scratch.in_both_static_dispatch_constraints.len());

        // Iterate over the fields in order, grouping them
        var a_i: usize = 0;
        var b_i: usize = 0;
        while (a_i < a_constraints.len and b_i < b_constraints.len) {
            const a_next = a_constraints[a_i];
            const b_next = b_constraints[b_i];
            const ord = StaticDispatchConstraint.orderByFnName(ident_store, a_next, b_next);
            switch (ord) {
                .eq => {
                    _ = try scratch.in_both_static_dispatch_constraints.append(scratch.gpa, TwoStaticDispatchConstraints{
                        .a = a_next,
                        .b = b_next,
                    });
                    a_i = a_i + 1;
                    b_i = b_i + 1;
                },
                .lt => {
                    _ = try scratch.only_in_a_static_dispatch_constraints.append(scratch.gpa, a_next);
                    a_i = a_i + 1;
                },
                .gt => {
                    _ = try scratch.only_in_b_static_dispatch_constraints.append(scratch.gpa, b_next);
                    b_i = b_i + 1;
                },
            }
        }

        // If b was shorter, add the extra a elems
        while (a_i < a_constraints.len) {
            const a_next = a_constraints[a_i];
            _ = try scratch.only_in_a_static_dispatch_constraints.append(scratch.gpa, a_next);
            a_i = a_i + 1;
        }

        // If a was shorter, add the extra b elems
        while (b_i < b_constraints.len) {
            const b_next = b_constraints[b_i];
            _ = try scratch.only_in_b_static_dispatch_constraints.append(scratch.gpa, b_next);
            b_i = b_i + 1;
        }

        // Return the ranges
        return .{
            .only_in_a = scratch.only_in_a_static_dispatch_constraints.rangeToEnd(a_constraints_start),
            .only_in_b = scratch.only_in_b_static_dispatch_constraints.rangeToEnd(b_constraints_start),
            .in_both = scratch.in_both_static_dispatch_constraints.rangeToEnd(both_constraints_start),
        };
    }

    /// Set error data in scratch & throw
    inline fn setUnifyErrAndThrow(self: *Self, err: UnifyErrCtx) Error!void {
        self.scratch.setUnifyErr(err);
        return error.UnifyErr;
    }
};

/// A fatal occurs error
pub const UnifyErrCtx = union(enum) {
    recursion_infinite: Var,
    recursion_anonymous: Var,
    invalid_number_type: Var,
    invalid_record_ext: Var,
    invalid_tag_union_ext: Var,
};

/// A list of constraint that should apply to concrete type
pub const DeferredConstraintCheck = struct {
    var_: Var,
    constraints: StaticDispatchConstraint.SafeList.Range,

    pub const SafeList = MkSafeList(@This());
};

/// Public helper functions for tests
pub fn partitionFields(
    ident_store: *const Ident.Store,
    scratch: *Scratch,
    a_fields_range: RecordFieldSafeList.Range,
    b_fields_range: RecordFieldSafeList.Range,
) std.mem.Allocator.Error!Unifier.PartitionedRecordFields {
    return try Unifier.partitionFields(ident_store, scratch, a_fields_range, b_fields_range);
}

/// Partitions tags from two tag ranges for unification.
pub fn partitionTags(
    ident_store: *const Ident.Store,
    scratch: *Scratch,
    a_tags_range: TagSafeList.Range,
    b_tags_range: TagSafeList.Range,
) std.mem.Allocator.Error!Unifier.PartitionedTags {
    return try Unifier.partitionTags(ident_store, scratch, a_tags_range, b_tags_range);
}

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
///
/// TODO: Currently, we capture vars created during unifcation in `fresh_vars`
/// and constraints to check later in `deferred_constraints`. We then copy
/// these values into other arrays in Check.  In the future, we should consider
/// passing in references to arrays by the caller of unify. Then we can write
/// directly into the output arrays and save the extra copying.
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

    // constraints
    deferred_constraints: DeferredConstraintCheck.SafeList,
    only_in_a_static_dispatch_constraints: StaticDispatchConstraint.SafeList,
    only_in_b_static_dispatch_constraints: StaticDispatchConstraint.SafeList,
    in_both_static_dispatch_constraints: TwoStaticDispatchConstraints.SafeList,

    // occurs
    occurs_scratch: occurs.Scratch,

    // err
    err: ?UnifyErrCtx,

    /// Init scratch
    pub fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!Self {
        // TODO: Set these based on the heuristics
        return .{
            .gpa = gpa,
            .fresh_vars = try VarSafeList.initCapacity(gpa, 8),
            .gathered_fields = try RecordFieldSafeList.initCapacity(gpa, 32),
            .only_in_a_fields = try RecordFieldSafeList.initCapacity(gpa, 32),
            .only_in_b_fields = try RecordFieldSafeList.initCapacity(gpa, 32),
            .in_both_fields = try TwoRecordFieldsSafeList.initCapacity(gpa, 32),
            .gathered_tags = try TagSafeList.initCapacity(gpa, 32),
            .only_in_a_tags = try TagSafeList.initCapacity(gpa, 32),
            .only_in_b_tags = try TagSafeList.initCapacity(gpa, 32),
            .in_both_tags = try TwoTagsSafeList.initCapacity(gpa, 32),
            .deferred_constraints = try DeferredConstraintCheck.SafeList.initCapacity(gpa, 32),
            .only_in_a_static_dispatch_constraints = try StaticDispatchConstraint.SafeList.initCapacity(gpa, 32),
            .only_in_b_static_dispatch_constraints = try StaticDispatchConstraint.SafeList.initCapacity(gpa, 32),
            .in_both_static_dispatch_constraints = try TwoStaticDispatchConstraints.SafeList.initCapacity(gpa, 32),
            .occurs_scratch = try occurs.Scratch.init(gpa),
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
        self.deferred_constraints.deinit(self.gpa);
        self.only_in_a_static_dispatch_constraints.deinit(self.gpa);
        self.only_in_b_static_dispatch_constraints.deinit(self.gpa);
        self.in_both_static_dispatch_constraints.deinit(self.gpa);
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
        self.deferred_constraints.items.clearRetainingCapacity();
        self.only_in_a_static_dispatch_constraints.items.clearRetainingCapacity();
        self.only_in_b_static_dispatch_constraints.items.clearRetainingCapacity();
        self.in_both_static_dispatch_constraints.items.clearRetainingCapacity();
        self.occurs_scratch.reset();
        self.err = null;
    }

    // helpers //

    /// Given a multi list of record fields and a range, copy from the multi list
    /// into scratch's gathered fields array
    fn copyGatherFieldsFromMultiList(
        self: *Self,
        multi_list: *const RecordFieldSafeMultiList,
        range: RecordFieldSafeMultiList.Range,
    ) std.mem.Allocator.Error!RecordFieldSafeList.Range {
        const start_int = self.gathered_fields.len();
        const record_fields_slice = multi_list.sliceRange(range);
        for (record_fields_slice.items(.name), record_fields_slice.items(.var_)) |name, var_| {
            _ = try self.gathered_fields.append(
                self.gpa,
                RecordField{ .name = name, .var_ = var_ },
            );
        }
        return self.gathered_fields.rangeToEnd(@intCast(start_int));
    }

    /// Given a multi list of tag and a range, copy from the multi list
    /// into scratch's gathered fields array
    fn copyGatherTagsFromMultiList(
        self: *Self,
        multi_list: *const TagSafeMultiList,
        range: TagSafeMultiList.Range,
    ) std.mem.Allocator.Error!TagSafeList.Range {
        const start_int = self.gathered_tags.len();
        const tag_slice = multi_list.sliceRange(range);
        for (tag_slice.items(.name), tag_slice.items(.args)) |ident, args| {
            _ = try self.gathered_tags.append(
                self.gpa,
                Tag{ .name = ident, .args = args },
            );
        }
        return self.gathered_tags.rangeToEnd(@intCast(start_int));
    }

    /// Exposed for tests
    pub fn appendSliceGatheredFields(self: *Self, fields: []const RecordField) std.mem.Allocator.Error!RecordFieldSafeList.Range {
        return try self.gathered_fields.appendSlice(self.gpa, fields);
    }

    /// Exposed for tests
    pub fn appendSliceGatheredTags(self: *Self, fields: []const Tag) std.mem.Allocator.Error!TagSafeList.Range {
        return try self.gathered_tags.appendSlice(self.gpa, fields);
    }

    fn setUnifyErr(self: *Self, err: UnifyErrCtx) void {
        self.err = err;
    }
};
