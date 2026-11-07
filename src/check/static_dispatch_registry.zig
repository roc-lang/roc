//! Checked static-dispatch registry and normalized dispatch-site records.
//!
//! The registry is built at checked-module publication. Post-check lowering uses
//! its exact callable-or-structural resolutions; the dispatch-site record
//! chooses the dispatcher type variable explicitly.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const TypedCIR = @import("typed_cir.zig");
const canonical = @import("canonical_names.zig");
const checked_ids = @import("checked_ids.zig");
const collections = @import("collections");
const artifact_serialize = @import("artifact_serialize.zig");
const dispatch_evidence = @import("dispatch_evidence.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
pub const NumericDefaultPhase = types.literal_defaulting.NumericDefaultPhase;
const CheckedTypeId = checked_ids.CheckedTypeId;
const DispatchScopeId = checked_ids.DispatchScopeId;

const DispatchExprTag = enum {
    e_dispatch_call,
    e_interpolation,
    e_type_dispatch_call,
    e_method_eq,
};

/// The dispatcher variable a type-dispatch owner statement declares.
pub fn typeDispatchOwnerVar(env: *const ModuleEnv, stmt_idx: CIR.Statement.Idx) Var {
    const stmt = env.store.getStatement(stmt_idx);
    const tag = std.meta.activeTag(stmt);
    if (tag == .s_type_var_alias) return ModuleEnv.varFrom(stmt.s_type_var_alias.type_var_anno);
    if (tag == .s_alias_decl) return ModuleEnv.varFrom(stmt_idx);
    base.invariant("{s}", .{"type dispatch owner statement was not a type-var alias or type alias"});
}

/// The dispatcher variable a type-dispatch call dispatches on.
pub fn typeDispatchCallDispatcherVar(env: *const ModuleEnv, owner: CIR.TypeDispatchOwner) Var {
    return switch (owner) {
        .statement => |stmt_idx| typeDispatchOwnerVar(env, stmt_idx),
        .dispatcher => |dispatcher| dispatcher,
    };
}

/// Public `ProcedureTemplateLookup` declaration.
pub const ProcedureTemplateLookup = struct {
    module_idx: u32,
    by_def: []const ProcedureTemplateLookupEntry = &.{},
    promoted: []const PromotedProcedureTemplateEntry = &.{},

    pub fn entryForDef(self: *const ProcedureTemplateLookup, def_idx: CIR.Def.Idx) ?ProcedureTemplateLookupEntry {
        const found = artifact_serialize.binarySearchByKey(ProcedureTemplateLookupEntry, CIR.Def.Idx, self.by_def, def_idx, templateEntryOrder) orelse return null;
        return found.*;
    }

    /// The template of the promoted local procedure bound by `pattern`.
    pub fn promotedTemplateForPattern(self: *const ProcedureTemplateLookup, pattern: CIR.Pattern.Idx) ?canonical.ProcedureTemplateRef {
        for (self.promoted) |entry| {
            if (entry.pattern == pattern) return entry.template;
        }
        return null;
    }
};

/// A promoted local procedure's template and the binding pattern whose
/// generalized scheme it publishes.
pub const PromotedProcedureTemplateEntry = struct {
    pattern: CIR.Pattern.Idx,
    template: canonical.ProcedureTemplateRef,
};

fn templateEntryOrder(e: ProcedureTemplateLookupEntry, key: CIR.Def.Idx) std.math.Order {
    return std.math.order(@backingInt(e.def), @backingInt(key));
}

/// Public `ProcedureTemplateKind` declaration.
///
/// The semantic lowering kind published with a checked procedure template.
/// Structural intrinsics are declarations of compiler-derived behavior, not
/// callable procedure bodies.
pub const ProcedureTemplateKind = union(enum) {
    callable,
    structural: StructuralKind,
};

/// Public `ProcedureTemplateLookupEntry` declaration.
pub const ProcedureTemplateLookupEntry = struct {
    def: CIR.Def.Idx,
    template: canonical.ProcedureTemplateRef,
    kind: ProcedureTemplateKind,

    pub fn lessThan(_: void, lhs: ProcedureTemplateLookupEntry, rhs: ProcedureTemplateLookupEntry) bool {
        return @backingInt(lhs.def) < @backingInt(rhs.def);
    }
};

/// The kind of procedure template value-binding definition `def_idx`
/// publishes, or null when it publishes none. The template table and the
/// method-binding classification (`classifyMethodBindings`) both decide it
/// here, from the module's checked source alone.
pub fn procedureTemplateKindForValueBindingDef(module: TypedCIR.Module, def_idx: CIR.Def.Idx) ?ProcedureTemplateKind {
    const expr = module.def(def_idx).expr.data;
    if (!topLevelExprIsAlreadyProcedure(module, def_idx, expr)) return null;
    const intrinsic = intrinsicForProcedureDef(module, def_idx) orelse return .callable;
    return switch (intrinsic) {
        .structural_eq => .{ .structural = .equality },
        .str_inspect,
        .parse_tag_union,
        .field_names_rename_fields,
        .field_names_shortest_name,
        .field_names_longest_name,
        .field_names_iter,
        .field_names_for_size,
        .field_name,
        => .callable,
    };
}

/// Whether a top-level definition's value is a procedure of its own.
fn topLevelExprIsAlreadyProcedure(module: TypedCIR.Module, def_idx: CIR.Def.Idx, expr: CIR.Expr) bool {
    if (expr == .e_lambda or expr == .e_closure or expr == .e_hosted_lambda) return true;
    if (expr != .e_anno_only) return false;
    // A compiler-owned intrinsic is a procedure whatever it declares, because
    // its body comes from the intrinsic wrapper rather than the declaration.
    if (intrinsicForProcedureDef(module, def_idx) != null) return true;
    // Every other annotation-only declaration is a procedure only when the
    // type it declares is a function. Annotation-only values are published
    // through the const path, which records the missing value the same way.
    return module.typeStoreConst().varResolvesToFunction(module.defType(def_idx));
}

/// What a method binding's registry entry targets, decided from the module's
/// checked source alone: the registry (`MethodRegistry.fromModule`) builds its
/// entries from it and type-root enumeration (`forEachCheckedSourceTypeRoot`)
/// reaches its callable types from it, so the two cannot disagree on which
/// source types the registry names.
pub const MethodBindingClass = union(enum) {
    /// An unsupported generated method: no registry entry.
    unsupported,
    /// An associated value that resolves to neither a callable nor an
    /// explicitly structural declaration: checked field access, not a
    /// static-dispatch resolution, so no registry entry.
    associated_value,
    /// A rejected declaration: an entry with no target.
    rejected,
    target: MethodBindingTarget,
};

/// A method binding's resolved target shape and the source types its
/// registry entry names.
pub const MethodBindingTarget = struct {
    shape: MethodBindingShape,
    /// Whether the binding reaches its target through a lookup chain
    /// (`method = f`) rather than declaring it.
    referenced: bool,
    /// The type of the target's callable value.
    callable_var: Var,
    /// The checker-recorded `T -> Str` instance of a `to_inspect` procedure
    /// (`ModuleEnv.inspectOverrideInstance`).
    inspect_instance: ?Var,
};

/// Public `MethodBindingShape` declaration.
pub const MethodBindingShape = union(enum) {
    structural: StructuralKind,
    /// The binding's own value-binding definition's callable template.
    own_procedure: CIR.Def.Idx,
    /// The callable template of the value-binding definition a lookup chain
    /// reaches.
    referenced_procedure: CIR.Def.Idx,
    /// The template of the promoted local procedure bound by this pattern.
    promoted_procedure: CIR.Pattern.Idx,
    local_proc: LocalProcedureMethodTarget,

    fn isProcedure(self: MethodBindingShape) bool {
        return switch (self) {
            .own_procedure, .referenced_procedure, .promoted_procedure => true,
            .structural, .local_proc => false,
        };
    }
};

/// The class of every method binding of `module`, aligned with
/// `module.methodDefEntries()`.
pub fn classifyMethodBindings(
    allocator: Allocator,
    module: TypedCIR.Module,
    module_index: anytype,
) Allocator.Error![]MethodBindingClass {
    const module_env = module.moduleEnvConst();
    var value_binding_defs = collections.DenseMap(CIR.Def.Idx, void).init(allocator);
    defer value_binding_defs.deinit();
    for (module_env.store.sliceDefs(module_env.value_binding_defs)) |def_idx| try value_binding_defs.put(def_idx, {});
    var promoted_patterns = collections.DenseMap(CIR.Pattern.Idx, void).init(allocator);
    defer promoted_patterns.deinit();
    for (module_env.promoted_local_procedures.items.items) |promoted| try promoted_patterns.put(promoted.pattern, {});
    const facts = SourceTemplateFacts{ .value_binding_defs = &value_binding_defs, .promoted_patterns = &promoted_patterns };

    const entries = module.methodDefEntries();
    const classes = try allocator.alloc(MethodBindingClass, entries.len);
    errdefer allocator.free(classes);
    for (entries, classes) |entry, *class| class.* = classifyMethodBinding(module, module_index, facts, entry);
    return classes;
}

/// The checked source facts that decide which definitions and patterns
/// publish procedure templates.
const SourceTemplateFacts = struct {
    value_binding_defs: *const collections.DenseMap(CIR.Def.Idx, void),
    promoted_patterns: *const collections.DenseMap(CIR.Pattern.Idx, void),

    fn templateKindForDef(self: SourceTemplateFacts, module: TypedCIR.Module, def_idx: CIR.Def.Idx) ?ProcedureTemplateKind {
        if (!self.value_binding_defs.contains(def_idx)) return null;
        return procedureTemplateKindForValueBindingDef(module, def_idx);
    }

    fn isPromoted(self: SourceTemplateFacts, pattern: CIR.Pattern.Idx) bool {
        return self.promoted_patterns.contains(pattern);
    }
};

fn classifyMethodBinding(
    module: TypedCIR.Module,
    module_index: anytype,
    facts: SourceTemplateFacts,
    entry: ModuleEnv.MethodDefs.Entry,
) MethodBindingClass {
    const binding = entry.value;
    const def_idx = binding.def_idx;
    if (unsupportedGeneratedMethodBinding(module, binding)) return .unsupported;
    if (methodBindingIsRejectedDeclaration(module, binding)) return .rejected;
    var referenced = false;
    var callable_var: Var = undefined;
    const shape: MethodBindingShape = if (generatedStructuralTargetForMethodBinding(module, binding)) |generated| blk: {
        callable_var = ModuleEnv.varFrom(binding.type_node_idx);
        break :blk .{ .structural = generated };
    } else if (facts.templateKindForDef(module, def_idx)) |kind| switch (kind) {
        .structural => |structural| blk: {
            callable_var = ModuleEnv.varFrom(binding.type_node_idx);
            break :blk .{ .structural = structural };
        },
        .callable => blk: {
            callable_var = module.defType(def_idx);
            break :blk .{ .own_procedure = def_idx };
        },
    } else if (promotedProcedureTargetForMethodBinding(module, facts, binding)) |promoted| blk: {
        callable_var = promoted.callable_var;
        break :blk promoted.shape;
    } else if (localProcedureTargetForMethodBinding(module, module_index, entry.key.owner, binding)) |local| blk: {
        callable_var = module.exprType(local.expr);
        break :blk .{ .local_proc = local };
    } else if (referencedProcedureTargetForMethodBinding(module, facts, module_index, binding)) |target| blk: {
        referenced = true;
        callable_var = target.callable_var;
        break :blk target.shape;
    } else return .associated_value;
    // Inspection calls an override from rendering workers and generic code
    // that never hold a local procedure's declaration context, so only a
    // procedure can be one.
    const inspect_instance = if (entry.key.methodIdent().eql(module.moduleEnvConst().idents.to_inspect) and shape.isProcedure())
        module.moduleEnvConst().inspectOverrideInstance(def_idx)
    else
        null;
    return .{ .target = .{
        .shape = shape,
        .referenced = referenced,
        .callable_var = callable_var,
        .inspect_instance = inspect_instance,
    } };
}

/// Public `MethodOwner` declaration.
///
/// A method owner is identified by CONTENT: the declaring module's deep
/// content identity plus the declaring statement (see `base.module_identity`
/// and `canonical.NominalTypeKey`). Type and module name text never
/// participate. Compiler-builtin owners keep their dedicated enum so builtin
/// dispatch stays exact across differently-spelled builtin idents.
pub const MethodOwner = union(enum) {
    nominal: canonical.NominalTypeKey,
    builtin: BuiltinOwner,
};

/// Public `BuiltinOwner` declaration.
pub const BuiltinOwner = enum(u8) {
    list,
    box,
    dict,
    set,
    fields,
    field,
    bool,
    str,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
    u8x16,
    i8x16,
    u16x8,
    i16x8,
    u32x4,
    i32x4,
    u64x2,
    i64x2,
    parse_tag_union_spec,
    crypto_sha256_digest,
    crypto_sha256_hasher,
    crypto_blake3_digest,
    crypto_blake3_hasher,
    iter,
    stream,
};
/// The builtin `Iter`/`Stream` nominals hold their step closure by value inside
/// a finite backing record. Later stages consult this to keep that closure a
/// lambda set (inline captures) instead of erasing it to a boxed callable.
pub fn isIteratorOwner(owner: BuiltinOwner) bool {
    return iteratorOwner(owner) != null;
}

/// The public iterator types. `Iter` steps purely and `Stream` steps
/// effectfully; their representation, producer operations, and minted chains
/// are otherwise one shared protocol keyed by this owner.
pub const IteratorOwner = enum(u8) {
    iter,
    stream,

    /// The backing record field holding the step thunk. An effectful thunk's
    /// field is spelled with `!`; every other topology name is shared.
    pub fn stepFieldName(self: IteratorOwner) []const u8 {
        return switch (self) {
            .iter => "step",
            .stream => "step!",
        };
    }
};

/// The public iterator type a builtin owner denotes, if any.
pub fn iteratorOwner(owner: BuiltinOwner) ?IteratorOwner {
    return switch (owner) {
        .iter => .iter,
        .stream => .stream,
        .list, .box, .dict, .set, .fields, .field, .bool, .str, .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2, .parse_tag_union_spec, .crypto_sha256_digest, .crypto_sha256_hasher, .crypto_blake3_digest, .crypto_blake3_hasher => null,
    };
}

/// Producer-owned identity of an internal iterator representation. This is
/// shared by Monotype and ConstStore so crossing that boundary never depends
/// on the ordinal layout of two separately maintained enums. A kind names the
/// producing operation, not the public type: a minted `Stream` map and a minted
/// `Iter` map are both `.map`, distinguished by their declaration identity.
pub const IteratorKind = enum(u8) {
    none,
    custom,
    list,
    list_rev,
    str,
    single,
    numeric_until,
    numeric_to,
    map,
    keep_if,
    drop_if,
    take_first,
    drop_first,
    concat,
    append,
    with_index,
    step_by,
    from_iter,
    forced_dynamic,

    /// See `IteratorComponentTopology`. Null for `none` (no minted kind) and
    /// `forced_dynamic` (no static topology).
    pub fn componentTopology(self: IteratorKind) ?IteratorComponentTopology {
        return switch (self) {
            .none, .forced_dynamic => null,
            .numeric_until, .numeric_to => .source_without_components,
            .custom, .list, .list_rev, .str, .single => .source_with_components,
            .map, .keep_if, .drop_if, .take_first, .drop_first, .concat, .append, .with_index, .step_by, .from_iter => .adapter,
        };
    }
};

/// Structural role of a minted iterator kind's nominal component arguments.
/// This is the one partition consumed by Monotype's mint-depth assignment,
/// graph depth finalization, and expected-producer request shaping; consumers
/// must never re-derive it independently, since a kind classified as source
/// in one consumer and adapter in another compiles clean and mints the wrong
/// representation far from the edit.
pub const IteratorComponentTopology = enum {
    /// A source whose construction inputs initialize generated step state
    /// without being stored as nominal component arguments (ranges). Its
    /// representation depth is exactly one.
    source_without_components,
    /// A source that stores its construction inputs as nominal components.
    /// Its representation depth is exactly one.
    source_with_components,
    /// An adapter over iterator components; its representation depth derives
    /// from those components.
    adapter,
};

/// Semantic identity assigned to compiler-owned iterator procedures while
/// checking still has the defining builtin declaration in hand. An id names
/// an operation, not the public type that provides it: `Iter.map` and
/// `Stream.map` carry the same `.map` stamp, and every consumer reads which
/// public type it is lowering from the solved result type. The Builtin
/// definitions carrying each stamp are declared once, per owner, by
/// `builtinNames`.
pub const IteratorProcedureId = enum(u8) {
    identity,
    next,
    custom,
    single,
    list_iter,
    list_iter_rev,
    str_iter_utf8,
    map,
    keep_if,
    drop_if,
    take_first,
    drop_first,
    concat,
    append,
    with_index,
    step_by,
    from_iter,
    /// `Range.iter` and the numeric `range_iter` implementations it
    /// dispatches to. They produce iterators but mint no representation of
    /// their own: the result is exactly the `Iter.custom` chain the numeric
    /// implementation builds.
    range_iter,
    numeric_range_iter,
    numeric_to,
    numeric_until,
    from_step,
    range_done,

    /// Whether this exact checked procedure returns an iterator value. Keep
    /// this exhaustive: adding an iterator procedure must declare its producer
    /// role here instead of letting a later pass infer it from result shape.
    pub fn producesIteratorValue(self: IteratorProcedureId) bool {
        return switch (self) {
            .next,
            .range_done,
            => false,
            .identity,
            .custom,
            .single,
            .list_iter,
            .list_iter_rev,
            .str_iter_utf8,
            .map,
            .keep_if,
            .drop_if,
            .take_first,
            .drop_first,
            .concat,
            .append,
            .with_index,
            .step_by,
            .from_iter,
            .range_iter,
            .numeric_range_iter,
            .numeric_to,
            .numeric_until,
            .from_step,
            => true,
        };
    }

    /// Whether Monotype's representation-graph protocol owns this operation's
    /// result. The delegating range producers are ordinary procedures there:
    /// their result representation is completed from their bodies, like any
    /// procedure that returns an iterator. Keep this exhaustive.
    pub fn participatesInRepresentationGraph(self: IteratorProcedureId) bool {
        return switch (self) {
            .range_iter,
            .numeric_range_iter,
            => false,
            .identity,
            .next,
            .custom,
            .single,
            .list_iter,
            .list_iter_rev,
            .str_iter_utf8,
            .map,
            .keep_if,
            .drop_if,
            .take_first,
            .drop_first,
            .concat,
            .append,
            .with_index,
            .step_by,
            .from_iter,
            .numeric_to,
            .numeric_until,
            .from_step,
            .range_done,
            => true,
        };
    }

    /// The minted iterator kind this procedure constructs, or null for
    /// procedures that pass through or consume an existing iterator
    /// (`Iter.iter`, `next`) and the internal step constructors. This is
    /// the one producer-to-kind mapping; Monotype's per-procedure request
    /// construction reads it instead of restating kinds beside each arm.
    pub fn iteratorKind(self: IteratorProcedureId) ?IteratorKind {
        return switch (self) {
            .identity, .next, .range_iter, .numeric_range_iter, .from_step, .range_done => null,
            .custom => .custom,
            .single => .single,
            .list_iter => .list,
            .list_iter_rev => .list_rev,
            .str_iter_utf8 => .str,
            .map => .map,
            .keep_if => .keep_if,
            .drop_if => .drop_if,
            .take_first => .take_first,
            .drop_first => .drop_first,
            .concat => .concat,
            .append => .append,
            .with_index => .with_index,
            .step_by => .step_by,
            .from_iter => .from_iter,
            .numeric_to => .numeric_to,
            .numeric_until => .numeric_until,
        };
    }

    /// Whether the checker must keep a call to this producer out of hoist
    /// root/cover position so its closed source expression stays available as
    /// a separate hoist root.
    ///
    /// List-backed conversions need this so an inline collection remains
    /// available for static-data hoisting, and the `Iter.iter` identity needs
    /// it so its step-closure-bearing result is never selected as a
    /// static-data root itself. Adapter inputs are already iterator
    /// expressions, while ranges and custom iterators have no eager source
    /// expression to preserve.
    pub fn preservesHoistableSourceInput(self: IteratorProcedureId) bool {
        return switch (self) {
            .list_iter, .list_iter_rev, .identity => true,
            .next,
            .custom,
            .single,
            .str_iter_utf8,
            .map,
            .keep_if,
            .drop_if,
            .take_first,
            .drop_first,
            .concat,
            .append,
            .with_index,
            .step_by,
            .from_iter,
            .range_iter,
            .numeric_range_iter,
            .numeric_to,
            .numeric_until,
            .from_step,
            .range_done,
            => false,
        };
    }

    /// The Builtin definitions stamped with this operation, by the public
    /// iterator type whose values they produce or consume. Every operation
    /// answers for both owners, so an operation added to one type states
    /// explicitly whether, and under which definitions, the other type
    /// provides it; the stamp table is generated from this alone.
    pub fn builtinNames(self: IteratorProcedureId) IteratorProcedureNames {
        return switch (self) {
            .identity => .{ .iter = &.{"Builtin.Iter.iter"}, .stream = &.{"Builtin.Stream.stream"} },
            .next => .{ .iter = &.{"Builtin.Iter.next"}, .stream = &.{"Builtin.Stream.next!"} },
            .custom => .{ .iter = &.{"Builtin.Iter.custom"}, .stream = &.{"Builtin.Stream.custom"} },
            .single => .{ .iter = &.{"Builtin.Iter.single"}, .stream = &.{} },
            .list_iter => .{ .iter = &.{"Builtin.List.iter"}, .stream = &.{} },
            .list_iter_rev => .{ .iter = &.{"Builtin.List.iter_rev"}, .stream = &.{} },
            .str_iter_utf8 => .{ .iter = &.{"Builtin.Str.iter_utf8"}, .stream = &.{} },
            .map => .{ .iter = &.{"Builtin.Iter.map"}, .stream = &.{ "Builtin.Stream.map", "Builtin.Stream.map!" } },
            .keep_if => .{ .iter = &.{"Builtin.Iter.keep_if"}, .stream = &.{"Builtin.Stream.keep_if"} },
            .drop_if => .{ .iter = &.{"Builtin.Iter.drop_if"}, .stream = &.{"Builtin.Stream.drop_if"} },
            .take_first => .{ .iter = &.{"Builtin.Iter.take_first"}, .stream = &.{"Builtin.Stream.take_first"} },
            .drop_first => .{ .iter = &.{"Builtin.Iter.drop_first"}, .stream = &.{"Builtin.Stream.drop_first"} },
            .concat => .{ .iter = &.{"Builtin.Iter.concat"}, .stream = &.{} },
            .append => .{ .iter = &.{"Builtin.Iter.append"}, .stream = &.{} },
            .with_index => .{ .iter = &.{ "iter_with_index", "Builtin.with_index" }, .stream = &.{ "stream_with_index", "Builtin.stream_with_index" } },
            .step_by => .{ .iter = &.{ "iter_step_by", "Builtin.step_by" }, .stream = &.{} },
            .from_iter => .{ .iter = &.{"Builtin.Iter.stream"}, .stream = &.{"Builtin.Stream.from_iter"} },
            .range_iter => .{ .iter = &.{"Builtin.Num.Range.iter"}, .stream = &.{} },
            .numeric_range_iter => .{ .iter = &.{ "range_iter_standard", "Builtin.range_iter_standard", "range_iter_float", "Builtin.range_iter_float" }, .stream = &.{} },
            .numeric_to => .{ .iter = &numeric_to_names, .stream = &.{} },
            .numeric_until => .{ .iter = &numeric_until_names, .stream = &.{} },
            .from_step => .{ .iter = &.{ "iter_from_step", "Builtin.from_step" }, .stream = &.{ "stream_from_step", "Builtin.stream_from_step" } },
            .range_done => .{ .iter = &.{ "range_done", "Builtin.range_done" }, .stream = &.{} },
        };
    }
};

/// Builtin definitions carrying one `IteratorProcedureId`, per public
/// iterator type. An empty list means that type does not provide the
/// operation.
pub const IteratorProcedureNames = struct {
    iter: []const []const u8,
    stream: []const []const u8,

    pub fn forOwner(self: IteratorProcedureNames, owner: IteratorOwner) []const []const u8 {
        return switch (owner) {
            .iter => self.iter,
            .stream => self.stream,
        };
    }
};

const IteratorProcedureNameEntry = struct { []const u8, IteratorProcedureId };

// Single-sourced from BuiltinLowLevel so the numeric roster cannot drift from
// the low-level registration tables. `to`/`until` need `minus_try` and
// therefore exclude IEEE floats.
const iterator_to_until_numeric_type_names = can.BuiltinLowLevel.non_float_numeric_type_names;

fn numericMethodNames(comptime numerics: anytype, comptime method: []const u8) [numerics.len][]const u8 {
    var out: [numerics.len][]const u8 = undefined;
    for (numerics, 0..) |numeric, index| out[index] = "Builtin.Num." ++ numeric ++ "." ++ method;
    return out;
}

const numeric_to_names = numericMethodNames(iterator_to_until_numeric_type_names, "to");
const numeric_until_names = numericMethodNames(iterator_to_until_numeric_type_names, "until");

const iterator_procedure_name_entries = blk: {
    @setEvalBranchQuota(10_000);
    var count: usize = 0;
    for (std.enums.values(IteratorProcedureId)) |procedure| {
        for (std.enums.values(IteratorOwner)) |owner| count += procedure.builtinNames().forOwner(owner).len;
    }
    var entries: [count]IteratorProcedureNameEntry = undefined;
    var index: usize = 0;
    for (std.enums.values(IteratorProcedureId)) |procedure| {
        for (std.enums.values(IteratorOwner)) |owner| {
            for (procedure.builtinNames().forOwner(owner)) |name| {
                entries[index] = .{ name, procedure };
                index += 1;
            }
        }
    }
    break :blk entries;
};

const iterator_procedure_by_name = std.StaticStringMap(IteratorProcedureId).initComptime(&iterator_procedure_name_entries);

/// Return the compiler-owned iterator role assigned to a Builtin definition.
pub fn iteratorProcedureForEnvDef(env: *const ModuleEnv, def_idx: CIR.Def.Idx) ?IteratorProcedureId {
    if (env.module_role != .builtin) return null;
    const def = env.store.getDef(def_idx);
    const pattern = env.store.getPattern(def.pattern);
    if (pattern != .assign) return null;
    return iterator_procedure_by_name.get(env.getIdent(pattern.assign.ident));
}

/// Every method name a hoist-preserving iterator conversion can be reached
/// through. The checker uses this as a cheap pre-filter before resolving a
/// receiver and looking up its binding, and answers from a user-defined
/// method's result type under these same names, so a producer reachable
/// through any other name would silently lose its receiver's hoistability.
/// The comptime block below keeps this in step with the two tables that
/// decide the answer.
pub const hoist_preserving_method_names = [_][]const u8{ "iter", "iter_rev", "stream" };

/// Builtin nominals whose iterator conversions delegate to a registered
/// producer (`Dict.iter` calls `List.iter` on its backing entries, and so on).
/// They carry no `IteratorProcedureId` of their own—their iterator
/// representation arrives through procedure-return propagation—but their
/// closed receiver must stay hoistable exactly like the producer they
/// delegate to.
const hoist_preserving_delegating_owners = [_][]const u8{ "Builtin.Dict", "Builtin.Set", "Builtin.Num.Range" };

/// Built from the two rosters above rather than listed separately, so the
/// delegating names cannot drift from the method names the checker
/// pre-filters on.
const hoist_preserving_delegating_producer_names = blk: {
    var entries: [hoist_preserving_delegating_owners.len * hoist_preserving_method_names.len]struct {
        []const u8,
    } = undefined;
    var index: usize = 0;
    for (hoist_preserving_delegating_owners) |owner| {
        for (hoist_preserving_method_names) |method| {
            entries[index] = .{owner ++ "." ++ method};
            index += 1;
        }
    }
    break :blk std.StaticStringMap(void).initComptime(&entries);
};

/// Producer definition names, exposed so the naming invariant that ties them
/// to `hoist_preserving_method_names` can be asserted outside this file (the
/// type checker's own sources may not compare strings).
pub const iterator_procedure_names = iterator_procedure_name_entries;

/// Whether this exact Builtin procedure needs its eager receiver preserved as
/// a separate hoist root. Iterator identity covers public producers, the
/// delegating Dict/Set conversions preserve their receiver the same way, and
/// the generated FieldNames iterator is an intrinsic with the same
/// source-lifetime requirement.
pub fn procedurePreservesHoistableSourceInputForEnvDef(env: *const ModuleEnv, def_idx: CIR.Def.Idx) bool {
    if (env.module_role != .builtin) return false;
    if (iteratorProcedureForEnvDef(env, def_idx)) |producer| {
        return producer.preservesHoistableSourceInput();
    }
    const def = env.store.getDef(def_idx);
    const pattern = env.store.getPattern(def.pattern);
    if (pattern == .assign and hoist_preserving_delegating_producer_names.has(env.getIdent(pattern.assign.ident))) {
        return true;
    }
    const expr = env.store.getExpr(def.expr);
    if (expr != .e_anno_only) return false;
    return can.BuiltinLowLevel.intrinsicAnnotation(env, expr.e_anno_only.ident) == .field_names_iter;
}

/// Return the compiler-owned iterator role assigned to a Builtin definition.
pub fn iteratorProcedureForDef(module: TypedCIR.Module, def_idx: CIR.Def.Idx) ?IteratorProcedureId {
    return iteratorProcedureForEnvDef(module.moduleEnvConst(), def_idx);
}

/// Public `MethodKey` declaration.
pub const MethodKey = struct {
    owner: MethodOwner,
    method: canonical.MethodNameId,
};

/// Producer-authored runtime category for an exact procedure method target.
pub const ProcedureRuntimeTarget = union(enum(u8)) {
    /// A normal Roc procedure specialization.
    procedure: OrdinaryProcedureTarget,
    /// One exact producer-authored low-level operation. Monotype emits this
    /// operation directly and must not request a procedure specialization.
    low_level: base.LowLevel,
    /// An annotation-only compiler intrinsic whose monomorphic implementation
    /// is emitted directly at the checked call site.
    intrinsic: can.BuiltinLowLevel.IntrinsicId,
    /// A compiler-authored operation whose runtime representation must
    /// participate in Monotype's graph protocol. The optional procedure
    /// identity selects an exact iterator construction/lowering protocol;
    /// representation-sensitive consumers such as methods on `Iter` and
    /// `Stream` require the graph without selecting such a protocol.
    /// Consumers must not infer this category from a callable's body or result
    /// shape.
    graph_participating: GraphParticipatingTarget,

    pub fn iteratorProcedure(self: ProcedureRuntimeTarget) ?IteratorProcedureId {
        return switch (self) {
            .procedure => |target| target.iterator_procedure,
            .graph_participating => |target| target.iterator_procedure,
            .low_level, .intrinsic => null,
        };
    }
};

/// An ordinary procedure target. An iterator producer outside the
/// representation-graph protocol still records its exact identity here, so
/// post-check iterator fusion admits calls to it.
pub const OrdinaryProcedureTarget = struct {
    iterator_procedure: ?IteratorProcedureId = null,
};

/// Producer-authored graph requirements for a representation-sensitive target.
pub const GraphParticipatingTarget = struct {
    iterator_procedure: ?IteratorProcedureId = null,
};

/// Exact checked procedure selected for a method registry entry.
pub const ProcedureMethodTarget = struct {
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
    runtime_target: ProcedureRuntimeTarget = .{ .procedure = .{} },
};

fn procedureRuntimeTargetForDef(
    module: TypedCIR.Module,
    def_idx: CIR.Def.Idx,
    method_owner: MethodOwner,
) ProcedureRuntimeTarget {
    if (intrinsicForProcedureDef(module, def_idx)) |intrinsic| {
        if (intrinsic.callsiteArity() != null) return .{ .intrinsic = intrinsic };
    }
    if (iteratorProcedureForDef(module, def_idx)) |iterator| return if (iterator.participatesInRepresentationGraph())
        .{ .graph_participating = .{ .iterator_procedure = iterator } }
    else
        .{ .procedure = .{ .iterator_procedure = iterator } };
    if (std.meta.activeTag(method_owner) == .builtin and isIteratorOwner(method_owner.builtin)) {
        return .{ .graph_participating = .{} };
    }
    if (module.moduleEnvConst().providedLowLevelForDef(def_idx)) |op| return .{ .low_level = op };
    return .{ .procedure = .{} };
}

/// Exact compiler-intrinsic identity for an annotation-only builtin procedure.
pub fn intrinsicForProcedureDef(module: TypedCIR.Module, def_idx: CIR.Def.Idx) ?can.BuiltinLowLevel.IntrinsicId {
    return intrinsicForEnvProcedureDef(module.moduleEnvConst(), def_idx);
}

/// The intrinsic Builtin procedure definition `def_idx` of `env` declares.
pub fn intrinsicForEnvProcedureDef(env: *const ModuleEnv, def_idx: CIR.Def.Idx) ?can.BuiltinLowLevel.IntrinsicId {
    const expr_data = env.store.getExpr(env.store.getDef(def_idx).expr);
    if (std.meta.activeTag(expr_data) != .e_anno_only) return null;
    const expr_ident = expr_data.e_anno_only.ident;
    if (!can.BuiltinLowLevel.isBuiltinModule(env)) return null;
    return can.BuiltinLowLevel.intrinsicAnnotation(env, expr_ident);
}

/// Public `LocalProcedureMethodTarget` declaration.
pub const LocalProcedureMethodTarget = struct {
    binder: CIR.Pattern.Idx,
    expr: CIR.Expr.Idx,
    /// Exact checked statement whose lexical position declares this target.
    context_anchor: CIR.Statement.Idx,
    /// Exact generalized-local evidence scope owned by this target.
    dispatch_scope: ?DispatchScopeId = null,
};

/// Public `MethodTargetKind` declaration.
pub const MethodTargetKind = union(enum(u8)) {
    procedure: ProcedureMethodTarget,
    local_proc: LocalProcedureMethodTarget,
    structural: StructuralKind,
};

/// Public `MethodTarget` declaration.
pub const MethodTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    kind: MethodTargetKind,
    callable_ty: CheckedTypeId,
    /// The exact procedure alias the method is bound to (`method = f`), when
    /// this target is the procedure the alias chain reaches. A dispatch edge
    /// instantiates the alias's scheme and resolves the alias's obligations,
    /// so the target's substitution and evidence both compose through the
    /// alias's forwarded lookup.
    alias: ?MethodAlias = null,
};

/// The exact procedure alias a method is bound to.
pub const MethodAlias = union(enum(u8)) {
    /// A top-level alias, by its checked binding pattern, which names its
    /// callable-eval template.
    top_level: CIR.Pattern.Idx,
    /// A block-local alias: its checked lookup of the aliased procedure and
    /// the generalized-local evidence scope it owns.
    local: LocalMethodAlias,
};

/// Public `LocalMethodAlias` declaration.
pub const LocalMethodAlias = struct {
    lookup: CIR.Expr.Idx,
    dispatch_scope: ?DispatchScopeId = null,
};

/// What resolving an (owner, method) pair against the checked method
/// registries found. Distinct from "no view declares this method": a
/// declaration the earlier stages rejected is still declared, but it has no
/// runtime target, so every dispatch that lands on it is a `checked_error`.
pub const CheckedMethodLookup = union(enum) {
    target: MethodTarget,
    rejected,

    /// The lowerable target, for a lookup whose method checking already
    /// accepted at that use. A lookup that can land on a rejected declaration
    /// (a compiler-generated edge) matches on `rejected` instead, so a rejected
    /// declaration reaching this is a compiler bug, not a shape to route around.
    pub fn requireTarget(self: CheckedMethodLookup, comptime context: []const u8) MethodTarget {
        return switch (self) {
            .target => |target| target,
            .rejected => base.invariant(
                "checked method lookup invariant violated: rejected declaration reached " ++ context,
                .{},
            ),
        };
    }
};

/// Public `MethodRegistryEntry` declaration.
pub const MethodRegistryEntry = struct {
    key: MethodKey,
    /// `null` when canonicalization or checking rejected this method's
    /// declaration (`methodBindingIsRejectedDeclaration`). Storing the key with
    /// no target is what keeps a rejected method distinguishable from a method
    /// no view declares at all.
    target: ?MethodTarget,
    /// For a `to_inspect` method that generic inspection uses for its owner,
    /// the checked instance of its type that inspection calls: `T -> Str`
    /// (design.md "Inspect Overrides"). `null` is the decision that inspection
    /// renders the owner's default form: the method is ineligible, is not
    /// `to_inspect`, or its declaration was rejected (`target` is `null`).
    /// Only `inspectOverride` reads it; ordinary method dispatch ignores
    /// it.
    inspect_override: ?CheckedTypeId = null,
};

/// The `to_inspect` method generic inspection calls for an owner, the checked
/// `T -> Str` instance of its type that inspection calls it at.
pub const InspectOverride = struct {
    target: MethodTarget,
    callable_ty: CheckedTypeId,
};

/// Public `MethodRegistry` declaration.
pub const MethodRegistry = struct {
    entries: []MethodRegistryEntry = &.{},

    pub fn lookup(self: *const MethodRegistry, key: MethodKey) ?CheckedMethodLookup {
        // Stack-built keys carry undefined bytes in the owner union's padding
        // and inactive-variant region; ReleaseFast fuses the comparator's
        // field reads into wide loads that touch them. Zero those bytes so
        // every load is defined (entries are zeroed at build/serialization).
        var normalized = key;
        collections.CompactWriter.zeroValuePadding(MethodKey, @ptrCast(&normalized));
        const found = artifact_serialize.binarySearchByKey(MethodRegistryEntry, MethodKey, self.entries, normalized, methodEntryOrder) orelse return null;
        const target = found.target orelse return .rejected;
        return .{ .target = target };
    }

    /// The `to_inspect` target that generic inspection calls for `key.owner`,
    /// or null when the registry recorded no override (an ineligible or
    /// rejected declaration) and inspection renders the value's default form.
    /// `key.method` names `to_inspect`.
    pub fn inspectOverride(self: *const MethodRegistry, key: MethodKey) ?InspectOverride {
        var normalized = key;
        collections.CompactWriter.zeroValuePadding(MethodKey, @ptrCast(&normalized));
        const found = artifact_serialize.binarySearchByKey(MethodRegistryEntry, MethodKey, self.entries, normalized, methodEntryOrder) orelse return null;
        const callable_ty = found.inspect_override orelse return null;
        return .{
            .target = found.target orelse
                std.debug.panic("checked static dispatch registry invariant violated: rejected declaration was recorded as an inspect override", .{}),
            .callable_ty = callable_ty,
        };
    }

    pub fn deinit(self: *MethodRegistry, allocator: Allocator) void {
        allocator.free(self.entries);
        self.* = .{};
    }

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        local_templates: *const ProcedureTemplateLookup,
        checked_types: anytype,
        module_index: anytype,
    ) Allocator.Error!MethodRegistry {
        var entries = std.ArrayList(MethodRegistryEntry).empty;
        errdefer entries.deinit(allocator);

        const module_idx = module.moduleIndex();
        if (module_idx != local_templates.module_idx) {
            base.invariant(
                "checked static dispatch registry invariant violated: template lookup module {d} does not match module {d}",
                .{ local_templates.module_idx, module_idx },
            );
        }

        const idents = module.identStoreConst();
        const classes = try classifyMethodBindings(allocator, module, module_index);
        defer allocator.free(classes);
        for (module.methodDefEntries(), classes) |entry, class| {
            const def_idx = entry.value.def_idx;
            if (class == .unsupported) continue;
            const method_owner = try methodOwnerForRegistryEntry(module, names, entry.key.ownerIdent());
            const method_key: MethodKey = .{
                .owner = method_owner,
                .method = try names.internMethodIdent(idents, entry.key.methodIdent()),
            };
            // A rejected declaration is still declared. Record the key with no
            // target so dispatch resolution can tell it apart from a method no
            // view declares, and resolve it to a checked error instead of
            // hunting for a runtime target that cannot exist.
            //
            // A rejected `to_inspect` is never an inspect override: inspection
            // renders its owner's values in the default form (design.md
            // "Inspect Overrides"). This entry is that decision; inspection
            // reads it through `inspectOverride`.
            const target = switch (class) {
                .unsupported => unreachable, // unsupported bindings continue above
                .associated_value => continue,
                .rejected => {
                    try entries.append(allocator, .{
                        .key = method_key,
                        .target = null,
                        .inspect_override = null,
                    });
                    continue;
                },
                .target => |target| target,
            };
            const target_kind: MethodTargetKind = switch (target.shape) {
                .structural => |kind| .{ .structural = kind },
                .own_procedure => |own_def| blk: {
                    const template = callableTemplateForDef(local_templates, own_def);
                    break :blk .{ .procedure = .{
                        .proc = .{ .artifact = template.artifact, .proc_base = canonical.ProcBase.of(.checked_source, @backingInt(own_def)) },
                        .template = template,
                        .runtime_target = procedureRuntimeTargetForDef(module, own_def, method_owner),
                    } };
                },
                .referenced_procedure => |target_def| blk: {
                    const template = callableTemplateForDef(local_templates, target_def);
                    break :blk .{ .procedure = .{
                        .proc = .{ .artifact = template.artifact, .proc_base = template.proc_base },
                        .template = template,
                        .runtime_target = procedureRuntimeTargetForDef(module, target_def, method_owner),
                    } };
                },
                .promoted_procedure => |pattern| blk: {
                    const template = local_templates.promotedTemplateForPattern(pattern) orelse
                        base.invariant("checked static dispatch registry invariant violated: a promoted method target has no promoted template", .{});
                    break :blk .{ .procedure = .{
                        .proc = .{ .artifact = template.artifact, .proc_base = template.proc_base },
                        .template = template,
                    } };
                },
                .local_proc => |local| .{ .local_proc = local },
            };
            const alias: ?MethodAlias = if (target.referenced and target.shape.isProcedure())
                methodAliasForBinding(module, module_index, entry.value)
            else
                null;
            const callable_ty = checked_types.methodCallableType(module, target.callable_var);

            try entries.append(allocator, .{
                .key = method_key,
                .target = .{
                    .module_idx = module_idx,
                    .def_idx = def_idx,
                    .kind = target_kind,
                    .callable_ty = callable_ty,
                    .alias = alias,
                },
                .inspect_override = if (target.inspect_instance) |instance_var|
                    inspectOverrideCallableType(module, checked_types, method_owner, instance_var)
                else
                    null,
            });
        }

        finalizeMethodRegistryEntries(entries.items);

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }
};

/// The callable template of value-binding definition `def_idx`, which the
/// method-binding classification decided publishes one.
fn callableTemplateForDef(local_templates: *const ProcedureTemplateLookup, def_idx: CIR.Def.Idx) canonical.ProcedureTemplateRef {
    const entry = local_templates.entryForDef(def_idx) orelse
        base.invariant("checked static dispatch registry invariant violated: a method target definition has no procedure template", .{});
    if (std.debug.runtime_safety and std.meta.activeTag(entry.kind) != .callable) {
        base.invariant("checked static dispatch registry invariant violated: a procedure method target's template is structural", .{});
    }
    return entry.template;
}

/// Whether this method binding's declaration was rejected before publication.
///
/// Canonicalization emits `e_runtime_error` for a body it could not
/// canonicalize, and checking rewrites a poisoned body to the same node, so
/// this is the single explicit marker for "this declaration has no runtime
/// target, and its diagnostic is already reported".
fn methodBindingIsRejectedDeclaration(
    module: TypedCIR.Module,
    binding: ModuleEnv.MethodBinding,
) bool {
    const expr_idx = methodBindingExpr(module, binding) orelse return false;
    return std.meta.activeTag(module.expr(expr_idx).data) == .e_runtime_error;
}

fn generatedStructuralTargetForMethodBinding(
    module: TypedCIR.Module,
    binding: ModuleEnv.MethodBinding,
) ?StructuralKind {
    const expr_idx = methodBindingExpr(module, binding) orelse return null;
    const expr_data = module.expr(expr_idx).data;
    if (std.meta.activeTag(expr_data) != .e_derived_method) return null;
    const kind = expr_data.e_derived_method.kind;

    return switch (kind) {
        .equality => .equality,
        .hash => .hash,
        .parser => .parser,
        .encoder => .encoder,
        .map => .map,
        .map_effectful => .map_effectful,
    };
}

fn methodBindingExpr(
    module: TypedCIR.Module,
    binding: ModuleEnv.MethodBinding,
) ?CIR.Expr.Idx {
    const raw_node = @backingInt(binding.type_node_idx);
    if (raw_node >= module.nodeCount()) {
        base.invariant(
            "checked static dispatch registry invariant violated: method binding node {d} is outside the module node store",
            .{raw_node},
        );
    }

    const node_tag = module.nodeTag(binding.type_node_idx);
    if (node_tag == .def) return module.moduleEnvConst().store.getDef(binding.def_idx).expr;
    if (node_tag != .statement_decl) return null;

    const statement: CIR.Statement.Idx = @fromBackingInt(@intCast(raw_node));
    const statement_data = module.getStatement(statement);
    if (std.meta.activeTag(statement_data) != .s_decl) return null;
    return statement_data.s_decl.expr;
}

/// The exact procedure alias a method binding is, given that its value is a
/// lookup chain reaching a procedure.
fn methodAliasForBinding(
    module: TypedCIR.Module,
    module_index: anytype,
    binding: ModuleEnv.MethodBinding,
) MethodAlias {
    if (module.nodeTag(binding.type_node_idx) == .def) {
        const pattern = module.moduleEnvConst().store.getDef(binding.def_idx).pattern;
        if (!module_index.reachesPattern(pattern)) base.invariant("checked static dispatch registry invariant violated: a method alias binding pattern was not checked", .{});
        return .{ .top_level = pattern };
    }
    const expr = methodBindingExpr(module, binding) orelse
        base.invariant("checked static dispatch registry invariant violated: a local method alias binding had no value", .{});
    if (!module_index.reachesExpr(expr)) base.invariant("checked static dispatch registry invariant violated: a local method alias lookup was not checked", .{});
    return .{ .local = .{ .lookup = expr } };
}

fn unsupportedGeneratedMethodBinding(
    module: TypedCIR.Module,
    binding: ModuleEnv.MethodBinding,
) bool {
    const expr_idx = methodBindingExpr(module, binding) orelse return false;
    const expr = module.expr(expr_idx).data;
    if (expr != .e_anno_only) return false;
    return expr.e_anno_only.kind == .unsupported_generated_method;
}

fn localProcedureTargetForMethodBinding(
    module: TypedCIR.Module,
    module_index: anytype,
    owner_statement: CIR.Statement.Idx,
    binding: ModuleEnv.MethodBinding,
) ?LocalProcedureMethodTarget {
    const raw_node = @backingInt(binding.type_node_idx);
    if (raw_node >= module.nodeCount()) {
        base.invariant(
            "checked static dispatch registry invariant violated: method binding node {d} is outside the module node store",
            .{raw_node},
        );
    }
    if (module.nodeTag(binding.type_node_idx) != .statement_decl) return null;

    const statement: CIR.Statement.Idx = @fromBackingInt(@intCast(raw_node));
    const statement_data = module.getStatement(statement);
    if (std.meta.activeTag(statement_data) != .s_decl) return null;
    const decl = statement_data.s_decl;

    if (!localProcedureExpr(module, decl.expr)) return null;

    if (!module_index.reachesExpr(decl.expr)) return null;
    if (!module_index.reachesPattern(decl.pattern)) {
        base.invariant(
            "checked static dispatch registry invariant violated: local method pattern {d} has no checked binder",
            .{@backingInt(decl.pattern)},
        );
    }
    if (!module_index.reachesStatement(owner_statement)) {
        base.invariant(
            "checked static dispatch registry invariant violated: local method owner statement {d} has no checked statement",
            .{@backingInt(owner_statement)},
        );
    }

    return .{
        .binder = decl.pattern,
        .expr = decl.expr,
        .context_anchor = owner_statement,
    };
}

fn localProcedureExpr(module: TypedCIR.Module, expr_idx: CIR.Expr.Idx) bool {
    const tag = std.meta.activeTag(module.expr(expr_idx).data);
    return tag == .e_lambda or tag == .e_closure;
}

const ReferencedProcedureTarget = struct {
    shape: MethodBindingShape,
    callable_var: Var,
};

/// Resolve a method declared in a function body whose binding checking
/// promoted to a procedure of its own. Every reference to a promoted local
/// procedure names its promoted template, and a dispatch is such a reference.
fn promotedProcedureTargetForMethodBinding(
    module: TypedCIR.Module,
    facts: SourceTemplateFacts,
    binding: ModuleEnv.MethodBinding,
) ?ReferencedProcedureTarget {
    if (module.nodeTag(binding.type_node_idx) != .statement_decl) return null;
    const decl = module.getStatement(@fromBackingInt(@intCast(@backingInt(binding.type_node_idx)))).s_decl;
    return promotedProcedureTarget(module, facts, decl.pattern, decl.expr);
}

fn promotedProcedureTarget(
    module: TypedCIR.Module,
    facts: SourceTemplateFacts,
    pattern: CIR.Pattern.Idx,
    expr: CIR.Expr.Idx,
) ?ReferencedProcedureTarget {
    if (!facts.isPromoted(pattern)) return null;
    return .{
        .shape = .{ .promoted_procedure = pattern },
        .callable_var = module.exprType(expr),
    };
}

/// Resolve a function-typed associated value bound by reference
/// (`method = top_level_fn`) to the referenced procedure. The reference chain
/// is followed through top-level defs and associated declarations until it
/// reaches a procedure-backed binding; a chain that never reaches one is an
/// associated value, not a call target, and resolves to null.
fn referencedProcedureTargetForMethodBinding(
    module: TypedCIR.Module,
    facts: SourceTemplateFacts,
    module_index: anytype,
    binding: ModuleEnv.MethodBinding,
) ?ReferencedProcedureTarget {
    const module_env = module.moduleEnvConst();
    var expr_idx = methodBindingExpr(module, binding) orelse return null;
    // Each hop follows one value binding, and a chain can visit each binding
    // at most once before repeating, so the node count bounds the walk.
    var remaining: usize = module.nodeCount();
    while (remaining > 0) : (remaining -= 1) {
        const expr_data = module.expr(expr_idx).data;
        if (std.meta.activeTag(expr_data) != .e_lookup_local) return null;
        const pattern_idx = expr_data.e_lookup_local.pattern_idx;
        if (defForBoundPattern(module_env, pattern_idx)) |target_def_idx| {
            if (facts.templateKindForDef(module, target_def_idx)) |kind| {
                return .{
                    .shape = switch (kind) {
                        .callable => .{ .referenced_procedure = target_def_idx },
                        .structural => |structural| .{ .structural = structural },
                    },
                    .callable_var = module.defType(target_def_idx),
                };
            }
            expr_idx = module_env.store.getDef(target_def_idx).expr;
            continue;
        }
        if (statementDeclForBoundPattern(module, pattern_idx)) |decl| {
            if (promotedProcedureTarget(module, facts, decl.pattern, decl.expr)) |promoted| return promoted;
            if (localProcedureExpr(module, decl.expr)) {
                if (!module_index.reachesExpr(decl.expr)) return null;
                if (!module_index.reachesPattern(decl.pattern)) return null;
                if (!module_index.reachesStatement(decl.statement)) return null;
                return .{
                    .shape = .{ .local_proc = .{
                        .binder = decl.pattern,
                        .expr = decl.expr,
                        .context_anchor = decl.statement,
                    } },
                    .callable_var = module.exprType(decl.expr),
                };
            }
            expr_idx = decl.expr;
            continue;
        }
        return null;
    }
    return null;
}

fn defForBoundPattern(module_env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?CIR.Def.Idx {
    for (module_env.store.sliceDefs(module_env.global_value_defs)) |def_idx| {
        if (module_env.store.getDef(def_idx).pattern == pattern_idx) return def_idx;
    }
    return null;
}

const BoundDecl = struct {
    statement: CIR.Statement.Idx,
    pattern: CIR.Pattern.Idx,
    expr: CIR.Expr.Idx,
};

fn statementDeclForBoundPattern(module: TypedCIR.Module, pattern_idx: CIR.Pattern.Idx) ?BoundDecl {
    var raw_node: u32 = 0;
    while (raw_node < module.nodeCount()) : (raw_node += 1) {
        if (module.nodeTag(@fromBackingInt(@intCast(raw_node))) != .statement_decl) continue;
        const statement: CIR.Statement.Idx = @fromBackingInt(@intCast(raw_node));
        const statement_data = module.getStatement(statement);
        if (std.meta.activeTag(statement_data) != .s_decl) continue;
        const decl = statement_data.s_decl;
        if (decl.pattern == pattern_idx) return .{ .statement = statement, .pattern = decl.pattern, .expr = decl.expr };
    }
    return null;
}

fn methodOwnerForRegistryEntry(
    module: TypedCIR.Module,
    names: *canonical.CanonicalNameStore,
    owner: ModuleEnv.MethodOwner,
) Allocator.Error!MethodOwner {
    const owner_env = module.moduleEnvConst();
    if (builtinOwnerForRegistryEntry(owner_env, owner.owner)) |builtin_owner| {
        return .{ .builtin = builtin_owner };
    }

    const identity_hash = owner_env.contentIdentityHash() orelse {
        base.invariant(
            "checked static dispatch registry invariant violated: module '{s}' has no content identity",
            .{owner_env.module_name},
        );
    };
    const stmt_tag = std.meta.activeTag(owner_env.store.getStatement(owner.owner));
    if (stmt_tag != .s_nominal_decl and stmt_tag != .s_alias_decl) {
        base.invariant(
            "checked static dispatch registry invariant violated: method owner statement {d} is not a type declaration",
            .{@backingInt(owner.owner)},
        );
    }
    return .{ .nominal = .{
        .module = try names.internModuleIdentity(identity_hash),
        .source_decl = @backingInt(owner.owner),
    } };
}

fn builtinOwnerForRegistryEntry(
    module_env: *const ModuleEnv,
    owner_stmt: CIR.Statement.Idx,
) ?BuiltinOwner {
    const common = module_env.idents;
    if (module_env.module_role != .builtin) return null;

    const stmt = module_env.store.getStatement(owner_stmt);
    const stmt_tag = std.meta.activeTag(stmt);
    const type_ident = if (stmt_tag == .s_nominal_decl)
        module_env.store.getTypeHeader(stmt.s_nominal_decl.header).name
    else if (stmt_tag == .s_alias_decl)
        module_env.store.getTypeHeader(stmt.s_alias_decl.header).name
    else
        return null;

    if (type_ident.eql(common.bool) or type_ident.eql(common.bool_type)) return .bool;
    if (type_ident.eql(common.str) or type_ident.eql(common.builtin_str)) return .str;
    if (type_ident.eql(common.u8) or type_ident.eql(common.u8_type)) return .u8;
    if (type_ident.eql(common.i8) or type_ident.eql(common.i8_type)) return .i8;
    if (type_ident.eql(common.u16) or type_ident.eql(common.u16_type)) return .u16;
    if (type_ident.eql(common.i16) or type_ident.eql(common.i16_type)) return .i16;
    if (type_ident.eql(common.u32) or type_ident.eql(common.u32_type)) return .u32;
    if (type_ident.eql(common.i32) or type_ident.eql(common.i32_type)) return .i32;
    if (type_ident.eql(common.u64) or type_ident.eql(common.u64_type)) return .u64;
    if (type_ident.eql(common.i64) or type_ident.eql(common.i64_type)) return .i64;
    if (type_ident.eql(common.u128) or type_ident.eql(common.u128_type)) return .u128;
    if (type_ident.eql(common.i128) or type_ident.eql(common.i128_type)) return .i128;
    if (type_ident.eql(common.f32) or type_ident.eql(common.f32_type)) return .f32;
    if (type_ident.eql(common.f64) or type_ident.eql(common.f64_type)) return .f64;
    if (type_ident.eql(common.dec) or type_ident.eql(common.dec_type)) return .dec;
    if (type_ident.eql(common.u8x16_type)) return .u8x16;
    if (type_ident.eql(common.i8x16_type)) return .i8x16;
    if (type_ident.eql(common.u16x8_type)) return .u16x8;
    if (type_ident.eql(common.i16x8_type)) return .i16x8;
    if (type_ident.eql(common.u32x4_type)) return .u32x4;
    if (type_ident.eql(common.i32x4_type)) return .i32x4;
    if (type_ident.eql(common.u64x2_type)) return .u64x2;
    if (type_ident.eql(common.i64x2_type)) return .i64x2;

    if (type_ident.eql(common.list) or type_ident.eql(common.builtin_list)) return .list;
    if (type_ident.eql(common.box) or type_ident.eql(common.builtin_box)) return .box;
    if (type_ident.eql(common.dict) or type_ident.eql(common.builtin_dict)) return .dict;
    if (type_ident.eql(common.set) or type_ident.eql(common.builtin_set)) return .set;
    if (type_ident.eql(common.iter) or type_ident.eql(common.builtin_iter)) return .iter;
    if (type_ident.eql(common.stream) or type_ident.eql(common.builtin_stream)) return .stream;
    if (type_ident.eql(common.builtin_encoding_field_names)) return .fields;
    if (type_ident.eql(common.builtin_encoding_field_name)) return .field;
    if (type_ident.eql(common.builtin_encoding_parse_tag_union_spec)) return .parse_tag_union_spec;
    if (type_ident.eql(common.builtin_crypto_sha256_digest)) return .crypto_sha256_digest;
    if (type_ident.eql(common.builtin_crypto_sha256_hasher)) return .crypto_sha256_hasher;
    if (type_ident.eql(common.builtin_crypto_blake3_digest)) return .crypto_blake3_digest;
    if (type_ident.eql(common.builtin_crypto_blake3_hasher)) return .crypto_blake3_hasher;
    return null;
}

fn methodRegistryEntryLessThan(_: void, a: MethodRegistryEntry, b: MethodRegistryEntry) bool {
    return methodKeyOrder(a.key, b.key) == .lt;
}

fn finalizeMethodRegistryEntries(entries: []MethodRegistryEntry) void {
    // Zero padding and inactive-union bytes first: at ReleaseFast the sorted
    // entries are compared with fused wide loads that touch those bytes, and
    // runtime-built entries would otherwise carry undefined memory there
    // (serialized registries are already zeroed by appendSlicePodZeroed).
    for (entries) |*entry| {
        collections.CompactWriter.zeroValuePadding(MethodRegistryEntry, @ptrCast(entry));
    }
    std.mem.sort(MethodRegistryEntry, entries, {}, methodRegistryEntryLessThan);
    assertMethodRegistryKeysUnique(entries);
}

fn assertMethodRegistryKeysUnique(entries: []const MethodRegistryEntry) void {
    if (entries.len < 2) return;
    var i: usize = 1;
    while (i < entries.len) : (i += 1) {
        if (methodKeyOrder(entries[i - 1].key, entries[i].key) != .eq) continue;
        base.invariant("checked static dispatch registry invariant violated: duplicate method registry key", .{});
    }
}

fn methodKeyOrder(a: MethodKey, b: MethodKey) std.math.Order {
    const owner_order = methodOwnerOrder(a.owner, b.owner);
    if (owner_order != .eq) return owner_order;
    return orderEnum(canonical.MethodNameId, a.method, b.method);
}

fn methodEntryOrder(e: MethodRegistryEntry, key: MethodKey) std.math.Order {
    return methodKeyOrder(e.key, key);
}

fn methodOwnerOrder(a: MethodOwner, b: MethodOwner) std.math.Order {
    return methodOwnerSortKey(a).order(methodOwnerSortKey(b));
}

/// A fully-defined scalar projection of a `MethodOwner` for ordering.
/// Comparing the union directly reads memory whose inactive-variant bytes are
/// undefined for runtime-built registries and stack keys; projecting first
/// writes every compared scalar explicitly. Nominal owners sort before
/// builtin owners, and nominal owners by module identity then declaring
/// statement.
const MethodOwnerSortKey = struct {
    tag: u32,
    first: u32,
    second: u32,

    fn order(a: @This(), b: @This()) std.math.Order {
        if (a.tag != b.tag) return orderU32(a.tag, b.tag);
        if (a.first != b.first) return orderU32(a.first, b.first);
        return orderU32(a.second, b.second);
    }
};

fn methodOwnerSortKey(owner: MethodOwner) MethodOwnerSortKey {
    return switch (owner) {
        .nominal => |nominal| .{
            .tag = 0,
            .first = @backingInt(nominal.module),
            .second = nominal.source_decl,
        },
        .builtin => |builtin_owner| .{
            .tag = 1,
            .first = @backingInt(builtin_owner),
            .second = 0,
        },
    };
}

fn orderEnum(comptime T: type, a: T, b: T) std.math.Order {
    return orderU32(@backingInt(a), @backingInt(b));
}

fn orderU32(a: u32, b: u32) std.math.Order {
    if (a == b) return .eq;
    return if (a < b) .lt else .gt;
}

/// Public `StaticDispatchResultMode` declaration.
pub const StaticDispatchResultMode = union(enum) {
    value,
    equality: struct {
        structural_allowed: bool,
        negated: bool,
        /// Checker-selected discriminant-only comparison. The named operand is
        /// the value being tested; the other operand is the payload-free tag.
        discriminant: ?struct {
            value_operand: u32,
            tag: canonical.TagLabelId,
        } = null,
    },
    /// A `to_hash : self, Hasher -> Hasher` dispatch whose receiver is an
    /// anonymous structural type. When `structural_allowed` is set, lowering
    /// decomposes the hash structurally instead of dispatching to a method.
    hash: struct {
        structural_allowed: bool,
    },
    parser_for: struct {
        structural_allowed: bool,
    },
    encoder_for: struct {
        structural_allowed: bool,
    },
    map: struct {
        structural_allowed: bool,
    },
    map_effectful: struct {
        structural_allowed: bool,
    },
};

/// Public `StaticDispatchDispatcher` declaration.
pub const StaticDispatchDispatcher = union(enum) {
    arg: u32,
    type_only,
};

/// Public `StaticDispatchOperand` declaration.
pub const StaticDispatchOperand = union(enum) {
    checked_expr: CIR.Expr.Idx,
    /// The literal segments of the checked interpolation expression, passed
    /// to `from_interpolation` as a `List(Str)`.
    generated_interpolation_segments: CIR.Expr.Idx,
    generated_numeral: ModuleEnv.NumeralLiteral,
    /// The post-escape contents of the string literal at this CIR node (an
    /// expression or a string literal pattern), passed to `from_quote` as Str
    /// (`checked_bodies.CheckedBodies.quoteText`).
    generated_quote: u32,
    /// The value a custom literal pattern matches, read through the pattern
    /// itself as its binder.
    literal_pattern_value: CIR.Pattern.Idx,
    /// A custom literal pattern's literal converted at the pattern's type
    /// (`checked_bodies.CheckedBodies.literalConversion`).
    literal_pattern_conversion: CIR.Pattern.Idx,
};

/// Public `StructuralKind` declaration.
///
/// The compiler-derived structural implementations the checker can choose for
/// a dispatch instead of a method target.
pub const StructuralKind = enum(u8) {
    equality,
    hash,
    parser,
    encoder,
    map,
    map_effectful,
};

/// How a dispatch whose dispatcher no edge can pin is discharged once the
/// dispatcher settles on its uninhabited default.
pub const UnpinnedDispatchResolution = enum {
    /// Compare or hash the vacuous shape structurally.
    structural,
    /// No value of the dispatcher's type exists, so the dispatch never runs.
    unreachable_value,
};

/// The one rule for discharging a dispatch on a dispatcher no edge can pin.
/// Equality and hashing of the vacuous shape remain structural; every other
/// dispatch, including parser, encoder, and derived map selections, is
/// statically unreachable. Checking applies it to dispatchers it can see are
/// unpinned, and post-check lowering applies it to callable-derived evidence
/// whose dispatcher finalizes as the same uninhabited default, so evidence
/// read at either stage agrees.
pub fn unpinnedDispatchResolution(structural: ?StructuralKind) UnpinnedDispatchResolution {
    const kind = structural orelse return .unreachable_value;
    return switch (kind) {
        .equality, .hash => .structural,
        .parser, .encoder, .map, .map_effectful => .unreachable_value,
    };
}

/// Canonical payload-slot identity selected by the checker for derived map.
pub const DerivedMapPlan = struct {
    tag: canonical.TagNameId,
    payload_index: u32,
};

/// A concrete compiler-derived implementation. Mapping carries the exact
/// payload slot selected during checking; later stages consume this plan.
pub const StructuralDerivation = union(enum(u8)) {
    equality,
    hash,
    parser,
    encoder,
    map: DerivedMapPlan,
    map_effectful: DerivedMapPlan,

    pub fn kind(self: StructuralDerivation) StructuralKind {
        return switch (self) {
            .equality => .equality,
            .hash => .hash,
            .parser => .parser,
            .encoder => .encoder,
            .map => .map,
            .map_effectful => .map_effectful,
        };
    }
};

/// Public `structural_method_kinds` declaration.
///
/// The one table mapping the method names that can discharge structurally to
/// their `StructuralKind`. Each `name` matches the corresponding
/// `CommonIdents` field name, so the evidence pass compares interned idents
/// via `@field` over this table while monotype lowering's component synthesis
/// classifies its view-local method names by text—both from this single
/// source.
pub const structural_method_kinds = [_]struct { method_name: [:0]const u8, common_ident: [:0]const u8, kind: StructuralKind }{
    .{ .method_name = Ident.IS_EQ_METHOD_NAME, .common_ident = "is_eq", .kind = .equality },
    .{ .method_name = Ident.TO_HASH_METHOD_NAME, .common_ident = "to_hash", .kind = .hash },
    .{ .method_name = Ident.PARSER_FOR_METHOD_NAME, .common_ident = "parser_for", .kind = .parser },
    .{ .method_name = Ident.ENCODER_FOR_METHOD_NAME, .common_ident = "encoder_for", .kind = .encoder },
    .{ .method_name = "map", .common_ident = "map", .kind = .map },
    .{ .method_name = "map!", .common_ident = "map_bang", .kind = .map_effectful },
};

/// The analysis whose evidence pool holds an evidence node, an evidence
/// range or a substitution range: one procedure template's dispatch
/// resolution, the module's resolution of what no template resolves
/// (defaulted-field expressions, chain-free plans and sites, inspect
/// overrides, generated codec calls), or an app's platform requirement roots.
pub const EvidenceOwner = enum(u32) {
    module = std.math.maxInt(u32) - 1,
    platform_requirements = std.math.maxInt(u32) - 2,
    _,

    pub fn forTemplate(template_id: canonical.CheckedProcedureTemplateId) EvidenceOwner {
        const raw = @backingInt(template_id);
        std.debug.assert(raw < @backingInt(EvidenceOwner.platform_requirements));
        return @fromBackingInt(@intCast(raw));
    }

    /// The template whose resolution owns the pool, or null for a
    /// module-level owner.
    pub fn template(self: EvidenceOwner) ?canonical.CheckedProcedureTemplateId {
        return switch (self) {
            .module, .platform_requirements => null,
            _ => @fromBackingInt(@backingInt(self)),
        };
    }
};

/// An evidence node: its index in its owner's evidence pool.
pub const EvidenceNodeId = packed struct(u64) {
    index: u32,
    owner: EvidenceOwner,
};

/// A range of one owner's evidence entries or substitution types.
pub const EvidenceSpan = struct {
    owner: EvidenceOwner = .module,
    start: u32 = 0,
    len: u32 = 0,
};

/// One parameter of a published scheme: the scheme's owning solver
/// variable and the parameter's position in the scheme's parameters.
pub const SchemeParamRef = struct {
    scheme: u32,
    index: u32,
};

/// The evidence one dispatch resolution produced: nodes, evidence entries
/// (nested ranges, site ranges and callable contracts) and the
/// substitution types nodes and sites name. Every id and range inside
/// refers to this pool.
pub const EvidencePool = struct {
    owner: EvidenceOwner,
    nodes: []const EvidenceNode = &.{},
    refs: []const CheckedEvidence = &.{},
    substitutions: []const CheckedTypeId = &.{},

    pub fn node(self: *const EvidencePool, id: EvidenceNodeId) EvidenceNode {
        std.debug.assert(id.owner == self.owner);
        return self.nodes[id.index];
    }

    pub fn entries(self: *const EvidencePool, span: EvidenceSpan) []const CheckedEvidence {
        std.debug.assert(span.len == 0 or span.owner == self.owner);
        return self.refs[span.start..][0..span.len];
    }

    pub fn types(self: *const EvidencePool, span: EvidenceSpan) []const CheckedTypeId {
        std.debug.assert(span.len == 0 or span.owner == self.owner);
        return self.substitutions[span.start..][0..span.len];
    }

    pub fn deinit(self: *EvidencePool, allocator: Allocator) void {
        allocator.free(self.nodes);
        allocator.free(self.refs);
        allocator.free(self.substitutions);
    }
};

/// Public `EvidenceChainIndex` declaration.
///
/// A dispatch obligation forwarded to the enclosing callable's evidence
/// params: `index` is the canonical evidence-param index (see
/// `dispatch_evidence.zig`), and `depth` counts enclosing generalized
/// callables outward from the reference (0 = the innermost generalized
/// callable the reference appears in).
pub const EvidenceChainIndex = struct {
    depth: u32,
    index: u32,
};

/// Reference to an enclosing evidence slot. Explicit per-use callable
/// instantiations can share the slot's target identity without sharing its
/// callable instantiation. An indexed contract supplies the exact per-use
/// evidence when the target requires checked records. Otherwise an independent
/// rank-1 relation rebuilds
/// callable-derived nested evidence from its callable. It retains the slot's
/// vector when the target schema is target-owned, or when a recorded
/// where-method use proves the signature copy shares every non-marker leaf.
pub const ConstraintEvidenceRef = struct {
    /// Requirements outside the callable signature and independent callable
    /// contracts name their exact owning scheme parameter, so dictionary ABIs
    /// need no lexical type search.
    scheme_param: ?SchemeParamRef = null,
    index: EvidenceChainIndex,
    independent_callable: bool = false,
    reuse_slot_nested_evidence: bool = false,
    /// Index of an exact per-call contract alongside the shared target slot.
    callable_contract: ?u32 = null,
};

/// Public `CheckedEvidence` declaration.
///
/// How one dispatch obligation was satisfied: with a concrete target (plus
/// nested evidence for the target's own obligations), by forwarding to the
/// enclosing callable's evidence params, or with a compiler-derived structural
/// implementation. `checked_error` marks an obligation at a site checking
/// already rejected; consuming it after checking is a compiler bug.
pub const CheckedEvidence = struct {
    /// Exact checked dispatcher type at this instantiation edge. Consumers use
    /// this for non-dictionary evidence too (notably defaulted literals whose
    /// runtime representation still needs an explicit descriptor).
    dispatcher_ty: CheckedTypeId,
    resolution: Resolution,
    /// Independent callable contracts, sharing one target slot.
    callable_contracts: EvidenceSpan = .{},

    pub const Resolution = union(enum) {
        direct: EvidenceNodeId,
        constraint: ConstraintEvidenceRef,
        structural: StructuralEvidence,
        /// The checker proved this nested-procedure obligation is the matching
        /// evidence parameter projected from the concrete callable request.
        from_callable,
        /// The local declaration binds an abstract scheme requirement. A use
        /// supplies its checked evidence; no codec is instantiated at declaration,
        /// and a declaration nothing uses has no dispatch to reach.
        from_scheme,
        checked_error,
        /// The edge left this obligation's dispatcher unsolved: no value of that
        /// type can ever reach the dispatch (e.g. the `Ok` payload of a `Try` that
        /// is always `Err` at this edge). The obligation is vacuous; consuming it
        /// lowers to an unreachable crash, never to a resolved call.
        unreachable_value,
    };
};

/// Exact checked identities for one compiler-derived evidence entry.
pub const StructuralEvidence = struct {
    derivation: StructuralDerivation,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    generated_codec_derivation: ?GeneratedCodecDerivationId = null,
};

/// Public `EvidenceNode` declaration.
///
/// A concrete method target together with evidence for the target's own
/// evidence params (in the target scheme's checker-recorded order).
pub const EvidenceTargetInstantiation = union(enum(u8)) {
    /// The target scheme has no variables and therefore no edge-specific
    /// callable instantiation.
    monomorphic,
    /// Exact callable relation produced while discharging this edge.
    callable: CheckedTypeId,
    /// Exact callable relation produced while discharging this edge, which
    /// instantiated the scheme of the exact procedure alias the target was
    /// reached through (`MethodTarget.alias`). The node's
    /// substitution substitutes that alias scope's quantified variables and
    /// its nested evidence is `through_alias`; the target's own substitution
    /// and evidence compose through the alias's forwarded lookup.
    alias: CheckedTypeId,
    /// Checking performed no instantiation of the target's scheme for this
    /// edge: publication selected it as the numeric default owner of an
    /// unpinned dispatcher. The node carries no substitution; specialization
    /// relates the target's scheme to this callable.
    derived_from_callable: CheckedTypeId,
};

/// Producer-authored source of a target's own evidence vector.
pub const EvidenceNested = union(enum(u8)) {
    /// Checking recorded the target scheme instantiation, so dispatch
    /// resolution resolved every nested obligation explicitly.
    resolved: EvidenceSpan,
    /// Target selection occurred after checking settled the dispatcher, or
    /// checking explicitly closed a concrete recursive dispatch. The edge
    /// derives the target's declared evidence params from their
    /// checker-recorded paths over its concrete callable, and from the
    /// callables of the requirement targets it selects.
    from_callable,
    /// The edge instantiated the scheme of the exact procedure alias the
    /// target was reached through (`EvidenceTargetInstantiation.alias`), and
    /// publication resolved every obligation of that alias's scheme, in its
    /// canonical order. The target's own evidence is the alias's forwarded
    /// lookup's evidence in the alias's scope under these entries.
    through_alias: EvidenceSpan,
};

/// Exact checked target and nested evidence selected for one dispatch edge.
pub const EvidenceNode = struct {
    target: MethodTarget,
    /// Exact checked dispatcher type that selected this target. Defaulted
    /// literal evidence has no concrete source type and records null.
    dispatcher_ty: ?CheckedTypeId = null,
    generated_codec_derivation: ?GeneratedCodecDerivationId = null,
    instantiation: EvidenceTargetInstantiation,
    nested: EvidenceNested = .{ .resolved = .{} },
    /// The checked type each quantified variable of the target's scheme was
    /// copied to at this edge, in the target scheme's `scheme_vars` order.
    /// Empty for a monomorphic target.
    subst: EvidenceSpan = .{},
};

/// Public `SiteEvidenceEntry` declaration.
///
/// Evidence for one instantiation site (keyed by the checked expression of the
/// use), covering the instantiated scheme's evidence params in canonical
/// order: a range into `StaticDispatchPlanTable.evidence_refs`. Sorted by key
/// for binary search (transform D).
pub const SiteEvidenceEntry = extern struct {
    /// The CIR node of the instantiation site.
    key: u32,
    start: u32,
    len: u32,
    /// Range into `StaticDispatchPlanTable.site_substitutions`: the checked
    /// type each quantified variable of the instantiated scheme was copied to
    /// at this site, in the scheme's `scheme_vars` order.
    subst_start: u32 = 0,
    subst_len: u32 = 0,
    /// For a stored nested-function use, `@backingInt` of the checked type of
    /// the instance the containing value stores; `no_site_instance` otherwise.
    instance_ty: u32 = no_site_instance,

    pub const no_site_instance = std.math.maxInt(u32);
};

/// Public `EvidencePathStep` declaration: one semantic step from a type to a
/// component, in the module's canonical names (`data` is a positional index,
/// a `canonical.RecordFieldLabelId`, or a `canonical.TagNameId` per kind).
pub const EvidencePathStep = dispatch_evidence.PathStep;

/// Public `EvidencePathNode` declaration: one step of a published evidence
/// path, linked to the node of the step before it. Paths of one published
/// param vector share their common prefixes.
pub const EvidencePathNode = dispatch_evidence.PathNode;

pub const no_evidence_path_node = dispatch_evidence.no_path_node;

/// Public `EvidencePathMemo` declaration: resolves published path nodes once
/// each per root.
pub const EvidencePathMemo = dispatch_evidence.PathMemo;

/// Public `EvidencePath` declaration: a published path, named by its last node
/// in the template table's `evidence_path_nodes`.
pub const EvidencePath = struct {
    last: u32 = no_evidence_path_node,
    len: u32 = 0,
};

/// Public `EvidenceParamRecord` declaration.
///
/// One published evidence param of a procedure template's scheme, in canonical
/// order (see `dispatch_evidence.zig`). Consumers index these by position; the
/// method name identifies the obligation, `dispatcher_ty` preserves its exact
/// checked identity, and `path` locates the dispatcher
/// within the scheme's callable so compiler-generated call edges (which have
/// no checked instantiation records) can resolve the obligation from the
/// concrete monomorphic callable. An empty path means the dispatcher has no
/// component path over the normalized callable (it is reachable only through
/// a constraint's fn type, or is an open-row remainder erased on closure).
pub const EvidenceParamRecord = struct {
    method: canonical.MethodNameId,
    /// Range of independent callable types in the template table.
    callable_contracts: artifact_serialize.Span = .{},
    dispatcher_ty: CheckedTypeId,
    /// The constraint's callable type in the owning scheme: the interface
    /// the selected target must satisfy. Relating a target to it binds the
    /// scheme's quantified variables that only this callable reaches.
    callable_ty: CheckedTypeId,
    /// Index of the dispatcher in the owning scheme's quantified-variable
    /// vector (`CheckedProcedureTemplate.scheme_vars` or
    /// `DispatchRefScope.scheme_vars`): the substitution entry this
    /// obligation's receiver is read from. Composite scheme requirements are
    /// supplied by checked evidence and have no quantified-variable slot.
    slot: ?u32,
    /// Checker-recorded derived implementation permitted when the concrete
    /// dispatcher has no registered method target.
    structural: ?StructuralKind = null,
    source: EvidenceParamSource = .scheme_callable,
    path: EvidencePath = .{},
};

/// Exact producer-authored source of an evidence parameter's dispatcher.
/// `path` is relative to the selected callable source and is never used for an
/// erased row remainder or an explicit default.
pub const EvidenceParamSource = union(enum) {
    scheme_callable,
    /// Exact captured codec relation, supplied by the checked call edge.
    scheme_requirement,
    constraint_callable: ConstraintCallableRoot,
    /// Reachable only through a nested constraint callable, with no
    /// specialization-time default to preserve. Checked use-site evidence
    /// resolves this requirement before post-check lowering, except at a
    /// closed recursive dispatch target, whose callable-derived evidence binds
    /// the receiver by relating the selected targets of its requirements.
    use_site_only,
    explicit_default: NumericDefaultPhase,
    erased_row_remainder,
};

/// Where a procedure target's nested evidence comes from. `from_target` is a
/// vector made entirely of captured scheme requirements: it is fixed by the
/// selected target instantiation and is independent of the requesting callable.
/// `requires_record` mixes sources or contains another pathless source, so only
/// checked per-use evidence (or an exact where-use slot reuse) can supply it.
pub const ProcedureEvidenceSchema = enum {
    none,
    from_callable,
    from_target,
    requires_record,
};

/// Classify a procedure template's evidence parameters into the schema above.
/// No parameters is `.none`. A scheme callable is always derivable from the
/// instantiated callable, and so is an explicit numeric default standing at the
/// callable root (an empty path). A vector consisting only of captured scheme
/// requirements belongs to the selected target. Mixed vectors and every other
/// source need the checked per-use record.
pub fn procedureEvidenceSchema(params: []const EvidenceParamRecord) ProcedureEvidenceSchema {
    if (params.len == 0) return .none;
    var scheme_requirements: usize = 0;
    for (params) |param| {
        switch (param.source) {
            .scheme_callable => {},
            .explicit_default => if (param.path.len != 0) return .requires_record,
            .scheme_requirement => if (param.path.len == 0) {
                scheme_requirements += 1;
            } else {
                return .requires_record;
            },
            .constraint_callable, .use_site_only, .erased_row_remainder => return .requires_record,
        }
    }
    if (scheme_requirements == params.len) return .from_target;
    if (scheme_requirements != 0) return .requires_record;
    return .from_callable;
}

/// Exact scheme-side constraint callable through which the dispatcher was
/// enumerated; the parameter's path is relative to this callable.
pub const ConstraintCallableRoot = struct {
    callable_ty: CheckedTypeId,
};

/// Exact CheckedModule payload for a direct call. The evidence node owns
/// the target identity, its fixed nested evidence, and (for local procedures)
/// the producer-authored lexical/capture context.
pub const DirectCall = struct {
    evidence: EvidenceNodeId,
};

/// Public checked call classification. CheckedModule construction first records
/// `direct_pending` while solving evidence, then replaces every such value with
/// one of the two durable direct categories after the exact target callable has
/// been instantiated. A serialized module can never contain
/// `direct_pending`.
pub const CheckedCallResolution = union(enum) {
    direct_pending: EvidenceNodeId,
    /// Exact target and a fully closed, structurally interned callable type.
    direct_closed: DirectCall,
    /// Exact target, but the callable still contains checked identity variables
    /// supplied by an enclosing specialization.
    direct_parametric: DirectCall,
    /// The dispatcher is one of the enclosing callable's constrained scheme
    /// vars; each specialization edge supplies the target as evidence.
    evidence_dependent: struct {
        scheme_param: ?SchemeParamRef = null,
        index: EvidenceChainIndex,
        /// The evidence slot supplies target identity but this plan owns an
        /// explicit per-use callable relation, so it instantiates the target
        /// against that relation rather than the slot's instantiation.
        independent_callable: bool = false,
        /// Keep the slot's producer-resolved nested evidence while replacing
        /// its callable instantiation. This is set only for a recorded
        /// where-method use, whose signature copy shares every non-marker leaf.
        reuse_slot_nested_evidence: bool = false,
        callable_contract: ?u32 = null,
    },
    /// The checker chose a compiler-derived structural implementation.
    structural: StructuralDerivation,
    /// Checking rejected this site; lowering must never consume the plan.
    checked_error,
    /// The dispatcher is a constrained var no specialization edge can ever
    /// supply (not an evidence param of any enclosing callable and not a
    /// defaulting literal): the dispatch is statically unreachable and lowers
    /// to an explicit crash.
    @"unreachable",
};

/// One dispatch plan as checking recorded it.
pub const RecordedDispatchPlan = struct {
    /// The CIR node of the dispatch site: a dispatching expression, a
    /// literal expression, or a custom literal pattern (whose node keys both
    /// its conversion plan and its equality plan).
    node: u32,
    method: canonical.MethodNameId,
    dispatcher: StaticDispatchDispatcher,
    dispatcher_ty: CheckedTypeId,
    /// The callable checking recorded for the call.
    callable_ty: CheckedTypeId,
    /// Range into `StaticDispatchPlanTable.operand_pool` (transform B).
    args: artifact_serialize.Span = .{},
    result_mode: StaticDispatchResultMode,

    /// The plan's operands within its table's pool.
    pub fn argsSlice(self: RecordedDispatchPlan, table: *const StaticDispatchPlanTable) []const StaticDispatchOperand {
        return table.operand_pool[self.args.start .. self.args.start + self.args.len];
    }
};

/// One dispatch plan with its total resolution: the recorded plan, the
/// callable specialized against a direct target, the resolution, and the
/// generated-codec contract a structural codec resolution selected.
pub const StaticDispatchCallPlan = struct {
    /// The CIR node of the dispatch site: a dispatching expression, a
    /// literal expression, or a custom literal pattern (whose node keys both
    /// its conversion plan and its equality plan).
    node: u32,
    method: canonical.MethodNameId,
    dispatcher: StaticDispatchDispatcher,
    dispatcher_ty: CheckedTypeId,
    /// The recorded callable, specialized against the target of a direct
    /// resolution.
    callable_ty: CheckedTypeId,
    /// Range into `StaticDispatchPlanTable.operand_pool` (transform B).
    args: artifact_serialize.Span = .{},
    result_mode: StaticDispatchResultMode,
    /// Exact generated-codec contract selected for a structural parser or
    /// encoder call. Null for every other resolution kind.
    generated_codec_derivation: ?GeneratedCodecDerivationId = null,
    resolution: CheckedCallResolution,

    pub fn fromRecorded(recorded: RecordedDispatchPlan, resolved: PlanResolution) StaticDispatchCallPlan {
        return .{
            .node = recorded.node,
            .method = recorded.method,
            .dispatcher = recorded.dispatcher,
            .dispatcher_ty = recorded.dispatcher_ty,
            .callable_ty = resolved.callable_ty,
            .args = recorded.args,
            .result_mode = recorded.result_mode,
            .generated_codec_derivation = resolved.generated_codec_derivation,
            .resolution = resolved.resolution,
        };
    }

    /// The plan's operands within its table's pool.
    pub fn argsSlice(self: StaticDispatchCallPlan, table: *const StaticDispatchPlanTable) []const StaticDispatchOperand {
        return table.operand_pool[self.args.start .. self.args.start + self.args.len];
    }
};

/// What dispatch resolution decided for one plan.
pub const PlanResolution = struct {
    resolution: CheckedCallResolution,
    callable_ty: CheckedTypeId,
    generated_codec_derivation: ?GeneratedCodecDerivationId = null,
};

/// What dispatch resolution decided for one iterator plan's two calls.
pub const IteratorPlanResolution = struct {
    iter: CheckedCallResolution,
    iter_callable_ty: CheckedTypeId,
    next: CheckedCallResolution,
    next_callable_ty: CheckedTypeId,
};

/// One iterator dispatch call with its total resolution; `callable_ty` is
/// specialized against a direct target.
pub const ResolvedIteratorDispatchCall = struct {
    method: canonical.MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    dispatcher_arg_index: u32,
    args: artifact_serialize.Span = .{},
    resolution: CheckedCallResolution,

    fn fromRecorded(call: IteratorDispatchCall, resolution: CheckedCallResolution, callable_ty: CheckedTypeId) ResolvedIteratorDispatchCall {
        return .{
            .method = call.method,
            .dispatcher_ty = call.dispatcher_ty,
            .callable_ty = callable_ty,
            .dispatcher_arg_index = call.dispatcher_arg_index,
            .args = call.args,
            .resolution = resolution,
        };
    }

    pub fn argsSlice(self: ResolvedIteratorDispatchCall, table: *const StaticDispatchPlanTable) []const IteratorDispatchOperand {
        return table.iter_operand_pool[self.args.start .. self.args.start + self.args.len];
    }
};

/// An iterator plan with its calls' total resolutions.
pub const ResolvedIteratorForPlan = struct {
    iter: ResolvedIteratorDispatchCall,
    /// `next` for a loop, `append` for an interpolation.
    next: ResolvedIteratorDispatchCall,
    item_ty: CheckedTypeId,
    iterator_ty: CheckedTypeId,
    site: IteratorForPlan.Site,

    pub fn fromRecorded(recorded: IteratorForPlan, resolved: IteratorPlanResolution) ResolvedIteratorForPlan {
        return .{
            .iter = ResolvedIteratorDispatchCall.fromRecorded(recorded.iter, resolved.iter, resolved.iter_callable_ty),
            .next = ResolvedIteratorDispatchCall.fromRecorded(recorded.next, resolved.next, resolved.next_callable_ty),
            .item_ty = recorded.item_ty,
            .iterator_ty = recorded.iterator_ty,
            .site = recorded.site,
        };
    }

    /// The plan's `for` loop site; the plan is a loop's.
    pub fn forLoopSite(self: ResolvedIteratorForPlan) IteratorForPlan.ForLoop {
        std.debug.assert(self.site == .for_loop);
        return self.site.for_loop;
    }

    /// The plan's interpolation site; the plan is an interpolation's.
    pub fn interpolationSite(self: ResolvedIteratorForPlan) IteratorForPlan.Interpolation {
        std.debug.assert(self.site == .interpolation);
        return self.site.interpolation;
    }
};

/// Public `StaticDispatchPlanId` declaration.
pub const StaticDispatchPlanId = enum(u32) { _ };

/// Kind of compiler-generated codec whose internal calls checking validated.
pub const GeneratedCodecDerivationKind = enum(u8) {
    parser,
    encoder,
};

/// Stable index of a checked generated-codec derivation contract.
pub const GeneratedCodecDerivationId = enum(u32) { _ };

/// SHA-256 of a generated codec contract's complete proof graph, encoded
/// without module-local ids: equal digests denote equivalent contracts even
/// when different checked modules recorded them.
pub const GeneratedCodecContentDigest = struct {
    bytes: [32]u8 = @splat(0),
};

/// The exact checked resolution of one compiler-generated codec call.
/// `pending` exists only while CheckedModule publication links the checker
/// snapshot to its target evidence or nested structural derivation.
pub const GeneratedCodecCallResolution = union(enum(u8)) {
    pending,
    checked_error,
    callable: EvidenceNodeId,
    structural: GeneratedCodecDerivationId,
};

/// One exact method edge inside a compiler-generated parser or encoder.
pub const GeneratedCodecCall = struct {
    method: canonical.MethodNameId,
    /// Dense producer role among distinct subject obligations for `method`.
    /// This is the post-check selection key; subject types remain validation
    /// metadata and are never rediscovered from a Monotype graph.
    method_role: u32,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    subject_ty: ?CheckedTypeId = null,
    resolution: GeneratedCodecCallResolution = .pending,
};

/// Exact checked contract for one compiler-generated codec instantiation.
pub const GeneratedCodecDerivation = struct {
    /// Producer-proven equivalence class of the complete codec proof graph.
    /// Source/frozen roles remain specific to this contract for replay.
    identity: GeneratedCodecDerivationId,
    /// Identity of the equivalence class across checked modules. Specialization
    /// identity names a contract by this digest, never by the module that
    /// recorded it.
    content: GeneratedCodecContentDigest = .{},
    kind: GeneratedCodecDerivationKind,
    source_constructor_ty: CheckedTypeId,
    source_runtime_ty: CheckedTypeId,
    source_shape_ty: CheckedTypeId,
    /// Exact structural shape walked by the generated body. This differs from
    /// `source_shape_ty` for a declaration-backed nominal codec.
    source_body_shape_ty: CheckedTypeId,
    source_encoding_ty: CheckedTypeId,
    source_state_ty: CheckedTypeId,
    source_error_ty: CheckedTypeId,
    constructor_ty: CheckedTypeId,
    runtime_ty: CheckedTypeId,
    shape_ty: CheckedTypeId,
    /// Frozen counterpart of `source_body_shape_ty`.
    body_shape_ty: CheckedTypeId,
    encoding_ty: CheckedTypeId,
    state_ty: CheckedTypeId,
    error_ty: CheckedTypeId,
    calls: artifact_serialize.Span = .{},

    pub fn callsSlice(self: GeneratedCodecDerivation, table: *const GeneratedCodecTable) []const GeneratedCodecCall {
        return table.generated_codec_calls[self.calls.start .. self.calls.start + self.calls.len];
    }
};

/// A module's generated-codec contracts with every call linked to its
/// checked evidence, and the module-level evidence pool those links name.
pub const GeneratedCodecTable = struct {
    generated_codec_derivations: []GeneratedCodecDerivation = &.{},
    generated_codec_calls: []GeneratedCodecCall = &.{},
    /// The module-level pool's nodes, entries and substitution types.
    evidence_nodes: []const EvidenceNode = &.{},
    evidence_refs: []const CheckedEvidence = &.{},
    site_substitutions: []const CheckedTypeId = &.{},

    pub fn evidenceNode(self: *const GeneratedCodecTable, id: EvidenceNodeId) EvidenceNode {
        std.debug.assert(id.owner == .module);
        return self.evidence_nodes[id.index];
    }

    /// The nested evidence of a linked callable call.
    pub fn callEvidence(self: *const GeneratedCodecTable, call: GeneratedCodecCall) []const CheckedEvidence {
        const node = switch (call.resolution) {
            .callable => |node_id| self.evidenceNode(node_id),
            .pending => base.invariant("unlinked generated codec call had no checked evidence", .{}),
            .checked_error => base.invariant("rejected generated codec call had no checked evidence", .{}),
            .structural => base.invariant("structural generated codec call had no callable evidence", .{}),
        };
        return switch (node.nested) {
            .resolved => |span| self.evidence_refs[span.start..][0..span.len],
            .from_callable, .through_alias => base.invariant("generated codec callable evidence was not resolved at its call edge", .{}),
        };
    }
};

/// Public `IteratorForPlanId` declaration.
pub const IteratorForPlanId = enum(u32) { _ };

/// Public `IteratorDispatchOperand` declaration.
pub const IteratorDispatchOperand = union(enum) {
    checked_expr: CIR.Expr.Idx,
    loop_iterator_state,
    /// The empty `List(item)` an interpolation's iterator is built from.
    interpolation_seed,
    /// The iterator an interpolation has built so far.
    interpolation_iterator,
    /// The interpolated value an interpolation appends next.
    interpolation_value,
};

/// Public `IteratorDispatchCall` declaration.
pub const IteratorDispatchCall = struct {
    method: canonical.MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    dispatcher_arg_index: u32,
    /// Range into `StaticDispatchPlanTable.iter_operand_pool` (transform B).
    args: artifact_serialize.Span = .{},

    pub fn argsSlice(self: IteratorDispatchCall, table: *const StaticDispatchPlanTable) []const IteratorDispatchOperand {
        return table.iter_operand_pool[self.args.start .. self.args.start + self.args.len];
    }
};

/// Checker-owned topology of the synthetic iterator step type. These exact
/// identities let postcheck project graph cells without recognizing compiler
/// constructs by display text or reconstructing their checked shape.
pub const IteratorStepTopology = struct {
    done_tag: canonical.TagLabelId,
    one_tag: canonical.TagLabelId,
    skip_tag: canonical.TagLabelId,
    item_field: canonical.RecordFieldLabelId,
    rest_field: canonical.RecordFieldLabelId,
    one_payload_ty: CheckedTypeId,
    skip_payload_ty: CheckedTypeId,
};

/// Checker-owned labels that define the public iterator representation.
/// Monotype consumes these exact ids when refining the public backing into a
/// private generated iterator; it never recovers field or tag roles from text.
pub const IteratorRepresentationTopology = struct {
    len_field: canonical.RecordFieldLabelId,
    step_field: canonical.RecordFieldLabelId,
    /// The length field's `Try(U64, [Unknown])`: its `Ok` and `Err` tags, and
    /// the `Unknown` tag of `Err`'s payload.
    ok_tag: canonical.TagLabelId,
    err_tag: canonical.TagLabelId,
    unknown_tag: canonical.TagLabelId,
    done_tag: canonical.TagLabelId,
    one_tag: canonical.TagLabelId,
    skip_tag: canonical.TagLabelId,
    item_field: canonical.RecordFieldLabelId,
    rest_field: canonical.RecordFieldLabelId,
};

/// Public `IteratorForPlan` declaration.
///
/// The two synthetic iterator dispatches a construct owns: a `for` loop's
/// `iter` over its iterable and `next` over its iterator state each round, or
/// an interpolation's `iter` over an empty list and `append` over its
/// iterator for each interpolated value.
pub const IteratorForPlan = struct {
    iter: IteratorDispatchCall,
    /// `next` for a loop, `append` for an interpolation.
    next: IteratorDispatchCall,
    item_ty: CheckedTypeId,
    iterator_ty: CheckedTypeId,
    site: Site,

    pub const Site = union(enum) {
        for_loop: ForLoop,
        interpolation: Interpolation,
    };

    pub const ForLoop = struct {
        iterable: CIR.Expr.Idx,
        step_ty: CheckedTypeId,
        step_topology: IteratorStepTopology,
    };

    pub const Interpolation = struct {
        expr: CIR.Expr.Idx,
        /// The empty `List(item)` the iterator is built from.
        seed_list_ty: CheckedTypeId,
    };

    /// The plan's `for` loop site; the plan is a loop's.
    pub fn forLoopSite(self: IteratorForPlan) ForLoop {
        std.debug.assert(self.site == .for_loop);
        return self.site.for_loop;
    }

    /// The plan's interpolation site; the plan is an interpolation's.
    pub fn interpolationSite(self: IteratorForPlan) Interpolation {
        std.debug.assert(self.site == .interpolation);
        return self.site.interpolation;
    }
};

/// Public `StaticDispatchPlanTable` declaration.
/// Relocatable replacement for an `AutoHashMap(idx -> id)`: a `(key, val)` pair
/// (both `@backingInt` u32s) stored in a sorted, binary-searchable POD slice
/// (transform D). Keys are unique (each source node/expr maps to one plan).
pub const PlanKV = extern struct { key: u32, val: u32 };

fn planKvLessThan(_: void, a: PlanKV, b: PlanKV) bool {
    return a.key < b.key;
}

fn planKvOrder(e: PlanKV, key: u32) std.math.Order {
    return std.math.order(e.key, key);
}

/// Binary-search a sorted `PlanKV` slice; returns the value (`@backingInt` of
/// the id) or null.
fn lookupPlanKV(sorted: []const PlanKV, key: u32) ?u32 {
    const found = artifact_serialize.binarySearchByKey(PlanKV, u32, sorted, key, planKvOrder) orelse return null;
    return found.val;
}

/// Append `ops` to `pool` and return their `(start, len)` range. Used to flatten
/// per-plan operand slices into the table's shared operand pools (transform B).
fn pushOperands(comptime T: type, pool: *std.ArrayList(T), allocator: Allocator, ops: []const T) Allocator.Error!artifact_serialize.Span {
    return artifact_serialize.appendSpan(artifact_serialize.Span, T, pool, allocator, ops);
}

/// Take `entries` as a slice sorted by key. A key recorded more than once
/// keeps the value recorded last.
fn sortedPlanKVs(allocator: Allocator, entries: *std.ArrayListUnmanaged(PlanKV)) Allocator.Error![]PlanKV {
    const items = entries.items;
    if (!std.sort.isSorted(PlanKV, items, {}, planKvLessThan)) {
        // The sort is stable, so equal keys stay in recording order.
        std.mem.sort(PlanKV, items, {}, planKvLessThan);
    }
    var len: usize = 0;
    for (items) |entry| {
        if (len > 0 and items[len - 1].key == entry.key) {
            items[len - 1] = entry;
        } else {
            items[len] = entry;
            len += 1;
        }
    }
    entries.shrinkRetainingCapacity(len);
    return entries.toOwnedSlice(allocator);
}

/// Resolved static-dispatch plans for a checked module: the per-call-site plans, the
/// sorted expr/node → plan indexes, and the shared operand pools the plans reference
/// (transform D). Reconstituted as plain slices on deserialize.
pub const StaticDispatchPlanTable = struct {
    plans: []RecordedDispatchPlan = &.{},
    /// The CIR node of a dispatching expression or of a custom literal
    /// pattern's equality -> `StaticDispatchPlanId`, sorted by key
    /// (transform D).
    by_node: []PlanKV = &.{},
    /// `CIR.Node.Idx` -> `StaticDispatchPlanId`, sorted by key.
    numeral_by_node: []PlanKV = &.{},
    /// `CIR.Node.Idx` -> `StaticDispatchPlanId`, sorted by key.
    quote_by_node: []PlanKV = &.{},
    iterator_for_plans: []IteratorForPlan = &.{},
    /// One checker-authored public iterator representation topology per
    /// `IteratorOwner`, indexed by its ordinal.
    iterator_topologies: []IteratorRepresentationTopology = &.{},
    /// `CIR.Node.Idx` -> `IteratorForPlanId`, sorted by key.
    iterator_for_by_node: []PlanKV = &.{},
    /// Shared flat pool of plan operands (transform-B side list).
    operand_pool: []const StaticDispatchOperand = &.{},
    /// Shared flat pool of iterator-plan operands.
    iter_operand_pool: []const IteratorDispatchOperand = &.{},
    /// The solver variables checking recorded for each plan, parallel to
    /// `plans`: what dispatch resolution resolves the plan from.
    plan_sources: []PlanSource = &.{},
    /// The solver variables checking recorded for each iterator plan,
    /// parallel to `iterator_for_plans`.
    iterator_plan_sources: []IteratorPlanSource = &.{},
    /// Exact generated-codec contracts emitted by checking, with their calls
    /// unlinked; the module's dispatch resolution links them
    /// (`GeneratedCodecTable`).
    recorded_codec_derivations: []GeneratedCodecDerivation = &.{},
    /// Shared flat pool backing `GeneratedCodecDerivation.calls`.
    recorded_codec_calls: []GeneratedCodecCall = &.{},

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        checked_types: anytype,
        bodies: anytype,
    ) Allocator.Error!StaticDispatchPlanTable {
        var plans = std.ArrayList(RecordedDispatchPlan).empty;
        errdefer plans.deinit(allocator);
        var plan_sources = std.ArrayList(PlanSource).empty;
        errdefer plan_sources.deinit(allocator);
        var iterator_plan_sources = std.ArrayList(IteratorPlanSource).empty;
        errdefer iterator_plan_sources.deinit(allocator);
        // Operand side-pools; per-plan operand slices are flattened into these.
        var operand_pool = std.ArrayList(StaticDispatchOperand).empty;
        errdefer operand_pool.deinit(allocator);
        var iter_operand_pool = std.ArrayList(IteratorDispatchOperand).empty;
        errdefer iter_operand_pool.deinit(allocator);
        // Each source node -> plan index is recorded as it is planned and
        // ordered by key once every plan is known.
        var by_node: std.ArrayListUnmanaged(PlanKV) = .empty;
        defer by_node.deinit(allocator);
        var numeral_by_node: std.ArrayListUnmanaged(PlanKV) = .empty;
        defer numeral_by_node.deinit(allocator);
        var quote_by_node: std.ArrayListUnmanaged(PlanKV) = .empty;
        defer quote_by_node.deinit(allocator);
        var iterator_for_plans = std.ArrayList(IteratorForPlan).empty;
        errdefer iterator_for_plans.deinit(allocator);
        var generated_codec_derivations = std.ArrayList(GeneratedCodecDerivation).empty;
        errdefer generated_codec_derivations.deinit(allocator);
        var generated_codec_calls = std.ArrayList(GeneratedCodecCall).empty;
        errdefer generated_codec_calls.deinit(allocator);
        const iterator_owners = comptime std.enums.values(IteratorOwner);
        const iterator_topologies = try allocator.alloc(IteratorRepresentationTopology, iterator_owners.len);
        errdefer allocator.free(iterator_topologies);
        for (iterator_owners) |owner| {
            iterator_topologies[@backingInt(owner)] = .{
                .len_field = try names.internRecordFieldLabel("len_if_known"),
                .step_field = try names.internRecordFieldLabel(owner.stepFieldName()),
                .ok_tag = try names.internTagLabel("Ok"),
                .err_tag = try names.internTagLabel("Err"),
                .unknown_tag = try names.internTagLabel("Unknown"),
                .done_tag = try names.internTagLabel("Done"),
                .one_tag = try names.internTagLabel("One"),
                .skip_tag = try names.internTagLabel("Skip"),
                .item_field = try names.internRecordFieldLabel("item"),
                .rest_field = try names.internRecordFieldLabel("rest"),
            };
        }
        var iterator_for_by_node: std.ArrayListUnmanaged(PlanKV) = .empty;
        defer iterator_for_by_node.deinit(allocator);

        var constraint_index = try StaticDispatchConstraintIndex.fromModule(allocator, module, bodies);
        defer constraint_index.deinit(allocator);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const tag = module.nodeTag(@fromBackingInt(@intCast(node_idx)));
            if (tag != .expr_dispatch_call and
                tag != .expr_interpolation and
                tag != .expr_type_dispatch_call and
                tag != .expr_type_dispatch_call_dispatcher and
                tag != .expr_method_eq) continue;

            const expr_idx: CIR.Expr.Idx = @fromBackingInt(@intCast(node_idx));
            if (!bodies.index().reachesExpr(expr_idx)) continue;
            const expr = module.expr(expr_idx);
            const checked_expr_data = bodies.exprData(expr_idx);
            const idents = module.identStoreConst();
            const plan_id: StaticDispatchPlanId = @fromBackingInt(@intCast(@as(u32, @intCast(plans.items.len))));
            const dispatch_expr_tag = std.meta.stringToEnum(DispatchExprTag, @tagName(std.meta.activeTag(expr.data))) orelse unreachable; // the node-tag filter above admits only nodes that decode to these expression tags
            switch (dispatch_expr_tag) {
                .e_dispatch_call => {
                    const dispatch_call = expr.data.e_dispatch_call;
                    const explicit_args = module.sliceExpr(dispatch_call.args);
                    const ar = try pushExprOperands(&operand_pool, allocator, bodies, dispatch_call.receiver, explicit_args);

                    try plans.append(allocator, .{
                        .node = node_idx,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher = .{ .arg = 0 },
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(dispatch_call.receiver)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = ar,
                        .result_mode = try staticDispatchResultModeForCheckedValueCall(allocator, module, checked_types, &constraint_index, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                    });
                    try plan_sources.append(allocator, .{
                        .dispatcher_var = module.exprType(dispatch_call.receiver),
                        .constraint_fn_var = dispatch_call.constraint_fn_var,
                    });
                },
                .e_interpolation => {
                    const interpolation = expr.data.e_interpolation;
                    if (std.meta.activeTag(checked_expr_data) != .interpolation) continue;
                    const args = [_]StaticDispatchOperand{.{ .generated_interpolation_segments = expr_idx }};
                    const from_interpolation = try names.internMethodName("from_interpolation");
                    const constraint_fn_var = interpolation.constraint_fn_var orelse
                        base.invariant("checked static dispatch registry invariant violated: checked interpolation has no constraint function variable", .{});
                    const dispatcher_var = interpolation.dispatcher_var orelse
                        base.invariant("checked static dispatch registry invariant violated: checked interpolation has no dispatcher variable", .{});
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, &args);

                    try plans.append(allocator, .{
                        .node = node_idx,
                        .method = from_interpolation,
                        .dispatcher = .type_only,
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatcher_var),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, constraint_fn_var),
                        .args = ar,
                        .result_mode = .value,
                    });
                    try plan_sources.append(allocator, .{
                        .dispatcher_var = dispatcher_var,
                        .constraint_fn_var = constraint_fn_var,
                        .literal_kind = .interpolation,
                    });

                    // The iterator the assembler receives: `iter` on an empty
                    // `List(item)`, then `append` for each value.
                    const item_var = interpolation.item_var orelse
                        base.invariant("checked static dispatch registry invariant violated: checked interpolation has no item variable", .{});
                    const iterator_var = interpolation.iterator_var orelse
                        base.invariant("checked static dispatch registry invariant violated: checked interpolation has no iterator variable", .{});
                    const seed_list_var = interpolation.seed_list_var orelse
                        base.invariant("checked static dispatch registry invariant violated: checked interpolation has no seed list variable", .{});
                    const iter_fn_var = interpolation.iter_fn_var orelse
                        base.invariant("checked static dispatch registry invariant violated: checked interpolation has no iter dispatch variable", .{});
                    const append_fn_var = interpolation.append_fn_var orelse
                        base.invariant("checked static dispatch registry invariant violated: checked interpolation has no append dispatch variable", .{});
                    const iterator_ty = try checkedTypeIdForVar(allocator, module, checked_types, iterator_var);
                    const seed_list_ty = try checkedTypeIdForVar(allocator, module, checked_types, seed_list_var);
                    const iterator_for_id: IteratorForPlanId = @fromBackingInt(@intCast(@as(u32, @intCast(iterator_for_plans.items.len))));
                    var iter_args = [_]IteratorDispatchOperand{.interpolation_seed};
                    const iter_ar = try pushOperands(IteratorDispatchOperand, &iter_operand_pool, allocator, &iter_args);
                    var append_args = [_]IteratorDispatchOperand{ .interpolation_iterator, .interpolation_value };
                    const append_ar = try pushOperands(IteratorDispatchOperand, &iter_operand_pool, allocator, &append_args);
                    try iterator_for_plans.append(allocator, .{
                        .iter = .{
                            .method = try names.internMethodName("iter"),
                            .dispatcher_ty = seed_list_ty,
                            .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, iter_fn_var),
                            .dispatcher_arg_index = 0,
                            .args = iter_ar,
                        },
                        .next = .{
                            .method = try names.internMethodName("append"),
                            .dispatcher_ty = iterator_ty,
                            .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, append_fn_var),
                            .dispatcher_arg_index = 0,
                            .args = append_ar,
                        },
                        .item_ty = try checkedTypeIdForVar(allocator, module, checked_types, item_var),
                        .iterator_ty = iterator_ty,
                        .site = .{ .interpolation = .{ .expr = expr_idx, .seed_list_ty = seed_list_ty } },
                    });
                    try iterator_plan_sources.append(allocator, .{
                        .iter_dispatcher_var = seed_list_var,
                        .next_dispatcher_var = iterator_var,
                        .iter_fn_var = iter_fn_var,
                        .next_fn_var = append_fn_var,
                    });
                    try iterator_for_by_node.append(allocator, .{ .key = @backingInt(expr_idx), .val = @backingInt(iterator_for_id) });
                },
                .e_type_dispatch_call => {
                    const dispatch_call = expr.data.e_type_dispatch_call;
                    const ar = try pushExprOperands(&operand_pool, allocator, bodies, null, module.sliceExpr(dispatch_call.args));

                    try plans.append(allocator, .{
                        .node = node_idx,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher = .type_only,
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, typeDispatchCallDispatcherVar(module.moduleEnvConst(), dispatch_call.owner)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = ar,
                        .result_mode = try staticDispatchResultModeForCheckedValueCall(allocator, module, checked_types, &constraint_index, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                    });
                    try plan_sources.append(allocator, .{
                        .dispatcher_var = typeDispatchCallDispatcherVar(module.moduleEnvConst(), dispatch_call.owner),
                        .constraint_fn_var = dispatch_call.constraint_fn_var,
                    });
                },
                .e_method_eq => {
                    const eq = expr.data.e_method_eq;
                    const ar = try pushExprOperands(&operand_pool, allocator, bodies, eq.lhs, &.{eq.rhs});

                    try plans.append(allocator, .{
                        .node = node_idx,
                        .method = try names.internMethodIdent(idents, module.commonIdents().is_eq),
                        .dispatcher = .{ .arg = 0 },
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(eq.lhs)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, eq.constraint_fn_var),
                        .args = ar,
                        .result_mode = .{ .equality = .{
                            .structural_allowed = true,
                            .negated = eq.negated,
                            .discriminant = if (zeroPayloadTagIdent(module, eq.rhs)) |tag_ident|
                                .{ .value_operand = 0, .tag = try names.internTagIdent(idents, tag_ident) }
                            else if (zeroPayloadTagIdent(module, eq.lhs)) |tag_ident|
                                .{ .value_operand = 1, .tag = try names.internTagIdent(idents, tag_ident) }
                            else
                                null,
                        } },
                    });
                    try plan_sources.append(allocator, .{
                        .dispatcher_var = module.exprType(eq.lhs),
                        .constraint_fn_var = eq.constraint_fn_var,
                    });
                },
            }
            try by_node.append(allocator, .{ .key = @backingInt(expr_idx), .val = @backingInt(plan_id) });
        }

        const module_env = module.moduleEnvConst();
        const checked_type_view = checked_types.view;
        var subject_scratch = @TypeOf(checked_type_view).EqualityScratch.init(allocator);
        defer subject_scratch.deinit();
        // Roles are keyed by the type a subject denotes, so subjects
        // spelled through different transparent aliases share a role.
        // Canonical keys retain alias identity and cannot bucket them.
        const GeneratedCodecRoleKey = struct {
            method: canonical.MethodNameId,
            has_subject: bool,
            subject_bucket: u64,
        };
        const no_role_candidate = std.math.maxInt(u32);
        const GeneratedCodecRoleCandidate = struct {
            subject_ty: ?CheckedTypeId,
            role: u32,
            next: u32 = no_role_candidate,
        };
        // A role key's candidates, chained through `role_candidate_pool` in
        // the order they were added.
        const GeneratedCodecRoleChain = struct { first: u32, last: u32 };
        var role_candidates: std.AutoHashMapUnmanaged(GeneratedCodecRoleKey, GeneratedCodecRoleChain) = .empty;
        defer role_candidates.deinit(allocator);
        var role_candidate_pool: std.ArrayListUnmanaged(GeneratedCodecRoleCandidate) = .empty;
        defer role_candidate_pool.deinit(allocator);
        var next_role_by_method = collections.DenseMap(canonical.MethodNameId, u32).init(allocator);
        defer next_role_by_method.deinit();
        for (module_env.generated_codec_derivations.items.items) |derivation| {
            const source_calls = module_env.generated_codec_calls.items.items[derivation.calls_start..][0..derivation.calls_len];
            const calls_start: u32 = @intCast(generated_codec_calls.items.len);
            role_candidates.clearRetainingCapacity();
            role_candidate_pool.clearRetainingCapacity();
            next_role_by_method.clearRetainingCapacity();
            for (source_calls) |call| {
                const method = try names.internMethodIdent(module.identStoreConst(), @bitCast(call.method_ident));
                const dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(call.dispatcher_var)));
                const callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(call.callable_var)));
                const subject_ty = if (call.subject_var == ModuleEnv.GeneratedCodecCall.no_subject_var)
                    null
                else
                    try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(call.subject_var)));
                const role_key = GeneratedCodecRoleKey{
                    .method = method,
                    .has_subject = subject_ty != null,
                    .subject_bucket = if (subject_ty) |subject| checked_type_view.aliasTransparentBucketKey(subject) else 0,
                };
                const candidates_entry = try role_candidates.getOrPut(allocator, role_key);
                if (!candidates_entry.found_existing) candidates_entry.value_ptr.* = .{ .first = no_role_candidate, .last = no_role_candidate };
                var method_role: ?u32 = null;
                var candidate_index = candidates_entry.value_ptr.first;
                while (candidate_index != no_role_candidate) : (candidate_index = role_candidate_pool.items[candidate_index].next) {
                    const candidate = role_candidate_pool.items[candidate_index];
                    if (subject_ty == null) {
                        method_role = candidate.role;
                        break;
                    }
                    if (candidate.subject_ty.? == subject_ty.? or
                        try checked_type_view.rootAliasTransparentEql(&subject_scratch, candidate.subject_ty.?, subject_ty.?))
                    {
                        method_role = candidate.role;
                        break;
                    }
                }
                if (method_role == null) {
                    const next_entry = try next_role_by_method.getOrPut(method);
                    if (!next_entry.found_existing) next_entry.value_ptr.* = 0;
                    method_role = next_entry.value_ptr.*;
                    next_entry.value_ptr.* += 1;
                    const added = std.math.cast(u32, role_candidate_pool.items.len) orelse return error.OutOfMemory;
                    try role_candidate_pool.append(allocator, .{
                        .subject_ty = subject_ty,
                        .role = method_role.?,
                    });
                    const chain = candidates_entry.value_ptr;
                    if (chain.last == no_role_candidate) {
                        chain.first = added;
                    } else {
                        role_candidate_pool.items[chain.last].next = added;
                    }
                    chain.last = added;
                }
                try generated_codec_calls.append(allocator, .{
                    .method = method,
                    .method_role = method_role.?,
                    .dispatcher_ty = dispatcher_ty,
                    .callable_ty = callable_ty,
                    .subject_ty = subject_ty,
                });
            }
            try generated_codec_derivations.append(allocator, .{
                .identity = @fromBackingInt(@intCast(@as(u32, @intCast(generated_codec_derivations.items.len)))),
                .kind = switch (@as(ModuleEnv.GeneratedCodecDerivation.Kind, @fromBackingInt(@intCast(derivation.kind)))) {
                    .parser => .parser,
                    .encoder => .encoder,
                },
                .source_constructor_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.source_constraint_fn_var))),
                .source_runtime_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.source_runtime_fn_var))),
                .source_shape_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.source_shape_var))),
                .source_body_shape_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.source_body_shape_var))),
                .source_encoding_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.source_encoding_var))),
                .source_state_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.source_state_var))),
                .source_error_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.source_error_var))),
                .constructor_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.constraint_fn_var))),
                .runtime_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.runtime_fn_var))),
                .shape_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.shape_var))),
                .body_shape_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.body_shape_var))),
                .encoding_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.encoding_var))),
                .state_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.state_var))),
                .error_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(derivation.error_var))),
                .calls = .{ .start = calls_start, .len = @intCast(source_calls.len) },
            });
        }
        for (module_env.store.literalDispatchPlans()) |numeral_plan| {
            if (numeral_plan.dispatchKind() != .numeral) continue;
            switch (numeral_plan.dispatchResolution()) {
                .builtin_direct, .checked_error => continue,
                .custom_dispatch, .specialization_dispatch => {},
                .unresolved => base.invariant("unresolved numeral dispatch plan reached checked publication", .{}),
            }
            const node: CIR.Node.Idx = @fromBackingInt(@intCast(numeral_plan.node_idx));
            if (!literalNodeConverts(bodies, numeral_plan.node_idx, .numeral)) continue;
            const literal = module_env.numeralLiteralForNode(node) orelse {
                base.invariant(
                    "checked static dispatch invariant violated: runtime from_numeral plan {d} has no exact literal",
                    .{numeral_plan.node_idx},
                );
            };
            if (!literal.isMaterialized()) {
                base.invariant(
                    "checked static dispatch invariant violated: runtime from_numeral plan {d} has an unmaterialized literal",
                    .{numeral_plan.node_idx},
                );
            }
            var args = [_]StaticDispatchOperand{.{ .generated_numeral = literal }};
            const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, &args);

            const plan_id: StaticDispatchPlanId = @fromBackingInt(@intCast(@as(u32, @intCast(plans.items.len))));
            try plans.append(allocator, .{
                .node = numeral_plan.node_idx,
                .method = try names.internMethodName(Ident.FROM_NUMERAL_METHOD_NAME),
                .dispatcher = .type_only,
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(numeral_plan.target_var))),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, numeral_plan.fnVar().?),
                .args = ar,
                .result_mode = .value,
            });
            try plan_sources.append(allocator, .{
                .dispatcher_var = @fromBackingInt(@intCast(numeral_plan.target_var)),
                .constraint_fn_var = numeral_plan.fnVar().?,
                .target_selected_by_checking = numeral_plan.dispatchResolution() != .specialization_dispatch,
                .literal_kind = .numeral,
            });
            try numeral_by_node.append(allocator, .{ .key = @backingInt(node), .val = @backingInt(plan_id) });
        }

        for (module_env.store.literalDispatchPlans()) |quote_plan| {
            if (quote_plan.dispatchKind() != .quote) continue;
            switch (quote_plan.dispatchResolution()) {
                .builtin_direct, .checked_error => continue,
                .custom_dispatch, .specialization_dispatch => {},
                .unresolved => base.invariant("unresolved quote dispatch plan reached checked publication", .{}),
            }
            const node: CIR.Node.Idx = @fromBackingInt(@intCast(quote_plan.node_idx));
            if (!literalNodeConverts(bodies, quote_plan.node_idx, .quote)) continue;
            var args = [_]StaticDispatchOperand{.{ .generated_quote = quote_plan.node_idx }};
            const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, &args);

            const plan_id: StaticDispatchPlanId = @fromBackingInt(@intCast(@as(u32, @intCast(plans.items.len))));
            try plans.append(allocator, .{
                .node = quote_plan.node_idx,
                .method = try names.internMethodName(Ident.FROM_QUOTE_METHOD_NAME),
                .dispatcher = .type_only,
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(quote_plan.target_var))),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, quote_plan.fnVar().?),
                .args = ar,
                .result_mode = .value,
            });
            try plan_sources.append(allocator, .{
                .dispatcher_var = @fromBackingInt(@intCast(quote_plan.target_var)),
                .constraint_fn_var = quote_plan.fnVar().?,
                .target_selected_by_checking = quote_plan.dispatchResolution() != .specialization_dispatch,
                .literal_kind = .quote,
            });
            try quote_by_node.append(allocator, .{ .key = @backingInt(node), .val = @backingInt(plan_id) });
        }

        // Only custom/generalized literal patterns compare through their
        // type's `is_eq`. Builtin patterns produce no equality plan.
        for (bodies.index().node_kinds, 0..) |kind, raw| {
            if (kind != .pattern) continue;
            const pattern: CIR.Pattern.Idx = @fromBackingInt(@intCast(raw));
            if (!bodies.literalPatternIsCustom(pattern)) continue;
            const source = module_env.store.literalDispatchPlanForNode(@fromBackingInt(@intCast(raw))) orelse unreachable; // literalPatternGuard reports only pattern nodes that carry a literal dispatch plan
            const context = source.patternContext(&module_env.store) orelse unreachable; // recordLiteralDispatchPlan allocates a pattern context for every pattern node's plan
            std.debug.assert(context.equality_fn_var_plus_one != 0);
            const constraint_fn: Var = @fromBackingInt(@intCast(context.equality_fn_var_plus_one - 1));
            const args = [_]StaticDispatchOperand{
                .{ .literal_pattern_value = pattern }, .{ .literal_pattern_conversion = pattern },
            };
            const plan_id: StaticDispatchPlanId = @fromBackingInt(@intCast(@as(u32, @intCast(plans.items.len))));
            try plans.append(allocator, .{
                .node = @intCast(raw),
                .method = try names.internMethodName(Ident.IS_EQ_METHOD_NAME),
                .dispatcher = .{ .arg = 0 },
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(source.target_var))),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, constraint_fn),
                .args = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, &args),
                .result_mode = .{ .equality = .{ .structural_allowed = true, .negated = false } },
            });
            try plan_sources.append(allocator, .{
                .dispatcher_var = @fromBackingInt(@intCast(source.target_var)),
                .constraint_fn_var = constraint_fn,
            });
            try by_node.append(allocator, .{ .key = @intCast(raw), .val = @backingInt(plan_id) });
        }

        for (module_env.for_loop_dispatch_plans.items.items) |for_plan| {
            const for_node_idx: CIR.Node.Idx = @fromBackingInt(@intCast(for_plan.node_idx));
            const pattern_idx: CIR.Pattern.Idx = @fromBackingInt(@intCast(for_plan.pattern_idx));
            const iterable_idx: CIR.Expr.Idx = @fromBackingInt(@intCast(for_plan.iterable_idx));

            if (!bodies.index().reachesExpr(iterable_idx)) continue;
            const for_node_tag = module.nodeTag(for_node_idx);
            const for_has_checked_node = if (for_node_tag == .expr_for)
                bodies.index().nodeKind(for_plan.node_idx) == .expr
            else if (for_node_tag == .statement_for)
                bodies.index().nodeKind(for_plan.node_idx) == .statement
            else
                false;
            if (!for_has_checked_node) continue;

            const iterable_expr = iterable_idx;
            const item_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.patternType(pattern_idx));
            const iter_callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(for_plan.iter_fn_var)));
            const next_callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(for_plan.next_fn_var)));
            const iterator_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(for_plan.iterator_var)));
            const step_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(for_plan.step_var)));
            const step_topology = IteratorStepTopology{
                .done_tag = try names.internTagIdent(module.identStoreConst(), @bitCast(for_plan.step_topology.done_tag_ident)),
                .one_tag = try names.internTagIdent(module.identStoreConst(), @bitCast(for_plan.step_topology.one_tag_ident)),
                .skip_tag = try names.internTagIdent(module.identStoreConst(), @bitCast(for_plan.step_topology.skip_tag_ident)),
                .item_field = try names.internRecordFieldIdent(module.identStoreConst(), @bitCast(for_plan.step_topology.item_field_ident)),
                .rest_field = try names.internRecordFieldIdent(module.identStoreConst(), @bitCast(for_plan.step_topology.rest_field_ident)),
                .one_payload_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(for_plan.step_topology.one_payload_var))),
                .skip_payload_ty = try checkedTypeIdForVar(allocator, module, checked_types, @fromBackingInt(@intCast(for_plan.step_topology.skip_payload_var))),
            };

            const iterator_for_id: IteratorForPlanId = @fromBackingInt(@intCast(@as(u32, @intCast(iterator_for_plans.items.len))));
            {
                var iter_args = [_]IteratorDispatchOperand{.{ .checked_expr = iterable_expr }};
                const iter_ar = try pushOperands(IteratorDispatchOperand, &iter_operand_pool, allocator, &iter_args);

                var next_args = [_]IteratorDispatchOperand{.loop_iterator_state};
                const next_ar = try pushOperands(IteratorDispatchOperand, &iter_operand_pool, allocator, &next_args);

                const iter_call = IteratorDispatchCall{
                    .method = try names.internMethodIdent(module.identStoreConst(), @bitCast(for_plan.iter_method_ident)),
                    .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(iterable_idx)),
                    .callable_ty = iter_callable_ty,
                    .dispatcher_arg_index = 0,
                    .args = iter_ar,
                };
                const next_call = IteratorDispatchCall{
                    .method = try names.internMethodIdent(module.identStoreConst(), @bitCast(for_plan.next_method_ident)),
                    .dispatcher_ty = iterator_ty,
                    .callable_ty = next_callable_ty,
                    .dispatcher_arg_index = 0,
                    .args = next_ar,
                };

                try iterator_for_plans.append(allocator, .{
                    .iter = iter_call,
                    .next = next_call,
                    .item_ty = item_ty,
                    .iterator_ty = iterator_ty,
                    .site = .{ .for_loop = .{
                        .iterable = iterable_expr,
                        .step_ty = step_ty,
                        .step_topology = step_topology,
                    } },
                });
                try iterator_plan_sources.append(allocator, .{
                    .iter_dispatcher_var = module.exprType(iterable_idx),
                    .next_dispatcher_var = @fromBackingInt(@intCast(for_plan.iterator_var)),
                    .iter_fn_var = @fromBackingInt(@intCast(for_plan.iter_fn_var)),
                    .next_fn_var = @fromBackingInt(@intCast(for_plan.next_fn_var)),
                });
            }
            try iterator_for_by_node.append(allocator, .{ .key = @backingInt(for_node_idx), .val = @backingInt(iterator_for_id) });
        }

        // Order the recorded entries into sorted, relocatable PlanKV slices
        // (transform D).
        const by_node_sorted = try sortedPlanKVs(allocator, &by_node);
        errdefer allocator.free(by_node_sorted);
        const numeral_sorted = try sortedPlanKVs(allocator, &numeral_by_node);
        errdefer allocator.free(numeral_sorted);
        const quote_sorted = try sortedPlanKVs(allocator, &quote_by_node);
        errdefer allocator.free(quote_sorted);
        const iterator_for_sorted = try sortedPlanKVs(allocator, &iterator_for_by_node);
        errdefer allocator.free(iterator_for_sorted);

        return .{
            .plan_sources = try plan_sources.toOwnedSlice(allocator),
            .iterator_plan_sources = try iterator_plan_sources.toOwnedSlice(allocator),
            .plans = try plans.toOwnedSlice(allocator),
            .by_node = by_node_sorted,
            .numeral_by_node = numeral_sorted,
            .quote_by_node = quote_sorted,
            .iterator_for_plans = try iterator_for_plans.toOwnedSlice(allocator),
            .iterator_topologies = iterator_topologies,
            .iterator_for_by_node = iterator_for_sorted,
            .operand_pool = try operand_pool.toOwnedSlice(allocator),
            .iter_operand_pool = try iter_operand_pool.toOwnedSlice(allocator),
            .recorded_codec_derivations = try generated_codec_derivations.toOwnedSlice(allocator),
            .recorded_codec_calls = try generated_codec_calls.toOwnedSlice(allocator),
        };
    }

    /// The dispatch plan of a dispatching expression, or the equality plan
    /// of a custom literal pattern, by CIR node.
    pub fn lookupByNode(self: *const StaticDispatchPlanTable, node: u32) ?StaticDispatchPlanId {
        return if (lookupPlanKV(self.by_node, node)) |v| @fromBackingInt(@intCast(v)) else null;
    }

    /// The dispatch plan of a dispatching expression.
    pub fn lookupByExpr(self: *const StaticDispatchPlanTable, expr: CIR.Expr.Idx) ?StaticDispatchPlanId {
        return self.lookupByNode(@backingInt(expr));
    }

    /// The `from_numeral` plan of the numeric literal at a CIR node.
    pub fn numeralPlanForNode(self: *const StaticDispatchPlanTable, node: u32) ?StaticDispatchPlanId {
        return if (lookupPlanKV(self.numeral_by_node, node)) |v| @fromBackingInt(@intCast(v)) else null;
    }

    /// The `from_quote` plan of the string literal at a CIR node.
    pub fn quotePlanForNode(self: *const StaticDispatchPlanTable, node: u32) ?StaticDispatchPlanId {
        return if (lookupPlanKV(self.quote_by_node, node)) |v| @fromBackingInt(@intCast(v)) else null;
    }

    /// The iterator plan of the `for` loop at a CIR node.
    pub fn iteratorForPlanForNode(self: *const StaticDispatchPlanTable, node: u32) ?IteratorForPlanId {
        return if (lookupPlanKV(self.iterator_for_by_node, node)) |v| @fromBackingInt(@intCast(v)) else null;
    }

    pub fn deinit(self: *StaticDispatchPlanTable, allocator: Allocator) void {
        allocator.free(self.by_node);
        allocator.free(self.numeral_by_node);
        allocator.free(self.quote_by_node);
        allocator.free(self.iterator_for_by_node);
        allocator.free(self.plans);
        allocator.free(self.iterator_for_plans);
        allocator.free(self.iterator_topologies);
        allocator.free(@constCast(self.operand_pool));
        allocator.free(@constCast(self.iter_operand_pool));
        allocator.free(self.plan_sources);
        allocator.free(self.iterator_plan_sources);
        allocator.free(self.recorded_codec_derivations);
        allocator.free(self.recorded_codec_calls);
        self.* = .{};
    }
};

/// Orders site evidence entries by their CIR node for binary search.
pub fn siteEvidenceOrder(e: SiteEvidenceEntry, key: u32) std.math.Order {
    return std.math.order(e.key, key);
}

/// The solver variables checking recorded for one plan: the source
/// dispatcher var and the source constraint fn var (the discharge-record
/// key), from which dispatch resolution resolves the plan.
pub const PlanSource = struct {
    dispatcher_var: Var,
    constraint_fn_var: ?Var,
    /// False for a literal conversion checking left to each specialization:
    /// any target publication resolves for it was not selected by checking.
    target_selected_by_checking: bool = true,
    /// The kind of source literal a literal-conversion plan converts.
    literal_kind: ?types.StaticDispatchConstraint.LiteralKind = null,
};

/// The solver variables checking recorded for one iterator plan.
pub const IteratorPlanSource = struct {
    iter_dispatcher_var: Var,
    next_dispatcher_var: Var,
    iter_fn_var: Var,
    next_fn_var: Var,
};

const StaticDispatchConstraintIndex = struct {
    constraints: []const types.StaticDispatchConstraint = &.{},
    by_fn_var: collections.DenseMapUnmanaged(Var, u32) = .empty,

    fn fromModule(allocator: Allocator, module: TypedCIR.Module, bodies: anytype) Allocator.Error!StaticDispatchConstraintIndex {
        const store = module.typeStoreConst();
        var live_fn_vars: collections.DenseMapUnmanaged(Var, void) = .empty;
        defer live_fn_vars.deinit(allocator);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const expr_idx: CIR.Expr.Idx = @fromBackingInt(@intCast(node_idx));
            const node_tag = module.nodeTag(@fromBackingInt(@intCast(node_idx)));
            const constraint_fn_var: ?Var = if (node_tag == .expr_dispatch_call)
                module.expr(expr_idx).data.e_dispatch_call.constraint_fn_var
            else if (node_tag == .expr_interpolation)
                module.expr(expr_idx).data.e_interpolation.constraint_fn_var
            else if (node_tag == .expr_type_dispatch_call or node_tag == .expr_type_dispatch_call_dispatcher)
                module.expr(expr_idx).data.e_type_dispatch_call.constraint_fn_var
            else if (node_tag == .expr_method_eq)
                module.expr(expr_idx).data.e_method_eq.constraint_fn_var
            else
                null;
            if (constraint_fn_var) |fn_var| {
                if (!bodies.index().reachesExpr(expr_idx)) continue;
                if (module.nodeTag(@fromBackingInt(@intCast(node_idx))) == .expr_interpolation and
                    std.meta.activeTag(bodies.exprData(expr_idx)) != .interpolation) continue;
                try live_fn_vars.put(allocator, fn_var, {});
            }
        }

        var index = StaticDispatchConstraintIndex{
            .constraints = store.static_dispatch_constraints.items.items,
        };
        errdefer index.deinit(allocator);

        try index.by_fn_var.ensureTotalCapacity(allocator, live_fn_vars.count());
        for (index.constraints, 0..) |constraint, i| {
            if (!live_fn_vars.contains(constraint.fn_var)) continue;
            const entry = try index.by_fn_var.getOrPut(allocator, constraint.fn_var);
            if (entry.found_existing) {
                const existing = index.constraints[entry.value_ptr.*];
                if (staticDispatchConstraintsEquivalent(store, existing, constraint)) continue;
                if (@import("builtin").mode == .debug) {
                    base.invariant(
                        "checked static dispatch constraint invariant violated: duplicate fn_var {d}; existing idx={d} name={s} origin={s} negated={} new idx={d} name={s} origin={s} negated={}",
                        .{
                            @backingInt(constraint.fn_var),
                            entry.value_ptr.*,
                            module.identStoreConst().getText(existing.fn_name),
                            @tagName(existing.origin),
                            existing.origin.binopNegated(),
                            i,
                            module.identStoreConst().getText(constraint.fn_name),
                            @tagName(constraint.origin),
                            constraint.origin.binopNegated(),
                        },
                    );
                }
                continue;
            }
            entry.value_ptr.* = @intCast(i);
        }

        return index;
    }

    fn lookup(self: *const StaticDispatchConstraintIndex, fn_var: Var) ?types.StaticDispatchConstraint {
        const constraint_idx = self.by_fn_var.get(fn_var) orelse return null;
        return self.constraints[constraint_idx];
    }

    fn deinit(self: *StaticDispatchConstraintIndex, allocator: Allocator) void {
        self.by_fn_var.deinit(allocator);
        self.* = .{};
    }
};

fn staticDispatchConstraintsEquivalent(store: *const types.Store, a: types.StaticDispatchConstraint, b: types.StaticDispatchConstraint) bool {
    // origin carries the binop-negation and literal-kind flags; a numeral's
    // facts are compared by content, wherever each constraint's row is.
    return a.fn_name == b.fn_name and
        a.fn_var == b.fn_var and
        std.meta.eql(a.origin, b.origin) and
        std.meta.eql(store.constraintNumeralInfo(a), store.constraintNumeralInfo(b));
}

fn staticDispatchResultModeForCheckedValueCall(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    constraint_index: *const StaticDispatchConstraintIndex,
    method_name: Ident.Idx,
    constraint_fn_var: Var,
) Allocator.Error!StaticDispatchResultMode {
    const common = module.commonIdents();
    if (method_name.eql(common.to_hash)) {
        if (sourceCallableHasHashShape(module, constraint_fn_var)) {
            return .{ .hash = .{ .structural_allowed = true } };
        }
        return .value;
    }
    if (method_name.eql(common.parser_for)) {
        return .{ .parser_for = .{
            .structural_allowed = true,
        } };
    }
    if (method_name.eql(common.encoder_for)) {
        return .{ .encoder_for = .{
            .structural_allowed = true,
        } };
    }
    if (method_name.eql(common.map)) {
        return .{ .map = .{ .structural_allowed = true } };
    }
    if (method_name.eql(common.map_bang)) {
        return .{ .map_effectful = .{ .structural_allowed = true } };
    }

    if (!method_name.eql(common.is_eq)) return .value;

    if (constraint_index.lookup(constraint_fn_var)) |constraint| {
        if (constraint.origin == .desugared_binop) {
            return .{ .equality = .{
                .structural_allowed = true,
                .negated = constraint.origin.binopNegated(),
            } };
        }
    }

    if (try sourceCallableHasEqualityShape(allocator, module, checked_types, constraint_fn_var)) {
        return .{ .equality = .{
            .structural_allowed = true,
            .negated = false,
        } };
    }

    return .value;
}

/// True when `fn_var` has the `to_hash` shape `(self, Hasher) -> Hasher`: two
/// arguments where the second (the Hasher) is threaded straight through to the
/// return type.
fn sourceCallableHasHashShape(
    module: TypedCIR.Module,
    fn_var: Var,
) bool {
    const store = module.typeStoreConst();
    const resolved = store.resolveVar(fn_var);
    const func = resolved.desc.content.unwrapFunc() orelse return false;
    const args = store.sliceVars(func.args);
    // `to_hash : self, Hasher -> Hasher` always has two arguments. Arity is the
    // only check needed here: the `to_hash` method name has already been matched
    // and this is only reached for an anonymous-structural dispatcher with no
    // method owner, so the constraint is the derived to_hash signature. (Unlike
    // the equality-shape check we cannot tie the second arg to the return—the
    // two `Hasher` occurrences are distinct vars, not a shared one like is_eq's
    // `self`, and there is no builtin-Hasher owner to match against.)
    return args.len == 2;
}

fn sourceCallableHasEqualityShape(
    allocator: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    fn_var: Var,
) Allocator.Error!bool {
    const store = module.typeStoreConst();
    const resolved = store.resolveVar(fn_var);
    const func = resolved.desc.content.unwrapFunc() orelse return false;
    const args = store.sliceVars(func.args);
    if (args.len != 2) return false;
    if (store.resolveVar(args[0]).var_ != store.resolveVar(args[1]).var_) return false;
    const ret_ty = try checkedTypeIdForVar(allocator, module, checked_types, func.ret);
    return checkedTypeIsBuiltinBool(checked_types, ret_ty);
}

fn checkedTypeIsBuiltinBool(checked_types: anytype, ty: CheckedTypeId) bool {
    const payload = checked_types.store.payload(ty);
    if (std.meta.activeTag(payload) != .nominal) return false;
    const builtin_owner = payload.nominal.builtin orelse return false;
    return builtin_owner == .bool;
}

/// The checked `T -> Str` instance inspection calls a `to_inspect` method at,
/// or null when the method is not the override generic inspection uses for
/// `owner` (design.md "Inspect Overrides"). Checking recorded the instance of
/// the method's type whose result is `Str`, if one exists.
fn inspectOverrideCallableType(
    module: TypedCIR.Module,
    checked_types: anytype,
    owner: MethodOwner,
    instance_var: Var,
) ?CheckedTypeId {
    const instance_ty = checked_types.methodCallableType(module, instance_var);
    return if (isInspectOverrideCallable(checked_types, owner, instance_ty)) instance_ty else null;
}

/// Whether a `to_inspect` instance's type is exactly `T -> Str`, where `T` is
/// `owner` applied to distinct unconstrained type variables. Aliases are
/// transparent names for the type they abbreviate.
fn isInspectOverrideCallable(checked_types: anytype, owner: MethodOwner, callable_ty: CheckedTypeId) bool {
    const store = checked_types.store;
    const callable = store.payload(checkedTypeThroughAliases(checked_types, callable_ty));
    if (std.meta.activeTag(callable) != .function) return false;
    const function = callable.function;
    if (function.kind == .effectful or function.args.len != 1) return false;

    const ret = store.payload(checkedTypeThroughAliases(checked_types, function.ret));
    if (std.meta.activeTag(ret) != .nominal) return false;
    if ((ret.nominal.builtin orelse return false) != .str) return false;

    const arg_ty = checkedTypeThroughAliases(checked_types, function.args[0]);
    const arg_owner = methodOwnerForCheckedPayload(store.payload(arg_ty)) orelse return false;
    if (methodOwnerOrder(arg_owner, owner) != .eq) return false;

    // Checked type variables carry identity, so two occurrences of one
    // variable share a root and distinct variables never do.
    const type_args = store.payload(arg_ty).nominal.args;
    for (type_args, 0..) |type_arg, index| {
        const variable = switch (store.payload(type_arg)) {
            .flex, .rigid => |variable| variable,
            .pending,
            .field_kind,
            .err,
            .alias,
            .record,
            .tuple,
            .nominal,
            .function,
            .empty_record,
            .tag_union,
            .empty_tag_union,
            => return false,
        };
        if (variable.constraints.len() != 0 or variable.numeric_default_phase != null) return false;
        for (type_args[0..index]) |earlier| {
            if (earlier == type_arg) return false;
        }
    }
    return true;
}

fn checkedTypeThroughAliases(checked_types: anytype, ty: CheckedTypeId) CheckedTypeId {
    var current = ty;
    var remaining = checked_types.store.typeCount();
    while (true) {
        const payload = checked_types.store.payload(current);
        if (std.meta.activeTag(payload) != .alias) return current;
        if (remaining == 0) {
            base.invariant("checked static dispatch invariant violated: checked type alias chain was cyclic", .{});
        }
        remaining -= 1;
        current = payload.alias.backing;
    }
}

fn methodOwnerForCheckedPayload(payload: anytype) ?MethodOwner {
    if (std.meta.activeTag(payload) != .nominal) return null;
    const nominal = payload.nominal;
    const nominal_owner: MethodOwner = .{ .nominal = .{
        .module = nominal.origin_module,
        .source_decl = nominal.source_decl,
    } };
    const builtin = nominal.builtin orelse return nominal_owner;
    if (builtin == .try_) return nominal_owner;
    return .{ .builtin = builtinOwnerForCheckedBuiltin(builtin) };
}

/// Public `builtinOwnerForCheckedBuiltin` declaration: the registry owner key
/// for a checked builtin nominal.
pub fn builtinOwnerForCheckedBuiltin(builtin: anytype) BuiltinOwner {
    return switch (builtin) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        .try_ => unreachable, // every caller routes Try to its nominal owner before asking for a builtin owner
        .u8x16 => .u8x16,
        .i8x16 => .i8x16,
        .u16x8 => .u16x8,
        .i16x8 => .i16x8,
        .u32x4 => .u32x4,
        .i32x4 => .i32x4,
        .u64x2 => .u64x2,
        .i64x2 => .i64x2,
        .list => .list,
        .box => .box,
        .dict => .dict,
        .set => .set,
        .iter => .iter,
        .stream => .stream,
        .fields => .fields,
        .field => .field,
        .parse_tag_union_spec => .parse_tag_union_spec,
        .crypto_sha256_digest => .crypto_sha256_digest,
        .crypto_sha256_hasher => .crypto_sha256_hasher,
        .crypto_blake3_digest => .crypto_blake3_digest,
        .crypto_blake3_hasher => .crypto_blake3_hasher,
    };
}

/// Public `lookupCheckedMethodTarget` declaration: exact callable-or-structural
/// registry lookup in the local registry, then the imported views.
pub fn lookupCheckedMethodTarget(
    names: *const canonical.CanonicalNameStore,
    local_method_registry: *const MethodRegistry,
    imported_views: anytype,
    owner: MethodOwner,
    method: canonical.MethodNameId,
) Allocator.Error!?CheckedMethodLookup {
    if (local_method_registry.lookup(.{ .owner = owner, .method = method })) |found| return found;

    const method_name = names.methodNameText(method);
    for (imported_views) |imported| {
        const imported_owner = methodOwnerInImportedStore(names, imported.canonical_names, owner) orelse continue;
        const imported_method = imported.canonical_names.lookupMethodName(method_name) orelse continue;
        if ((try imported.checkedTables()).method_registry.lookup(.{ .owner = imported_owner, .method = imported_method })) |found| {
            switch (found) {
                .rejected => return found,
                .target => |target| switch (target.kind) {
                    .procedure, .structural => return found,
                    .local_proc => continue,
                },
            }
        }
    }
    return null;
}

/// Rebase a method owner into an imported module's store: the module
/// component crosses by 32-byte content identity (one map probe, full-value
/// comparison); the declaring statement is the same in every store. This is
/// the single cross-module owner resolution point—no name text.
pub fn methodOwnerInImportedStore(
    source_names: *const canonical.CanonicalNameStore,
    imported_names: *const canonical.CanonicalNameStore,
    owner: MethodOwner,
) ?MethodOwner {
    return switch (owner) {
        .builtin => |builtin| .{ .builtin = builtin },
        .nominal => |nominal| .{ .nominal = .{
            .module = imported_names.lookupModuleIdentity(source_names.moduleIdentityBytes(nominal.module)) orelse return null,
            .source_decl = nominal.source_decl,
        } },
    };
}

fn checkedTypeIdForVar(
    _: Allocator,
    module: TypedCIR.Module,
    checked_types: anytype,
    var_: Var,
) Allocator.Error!CheckedTypeId {
    return checked_types.rootForSourceVar(module, var_) orelse {
        base.invariant("checked static dispatch invariant violated: dispatch type root was not published", .{});
    };
}

/// Append one checked-expression operand per source expression, `first`
/// (when present) before `rest`, directly to `pool`.
fn pushExprOperands(
    pool: *std.ArrayList(StaticDispatchOperand),
    allocator: Allocator,
    bodies: anytype,
    first: ?CIR.Expr.Idx,
    rest: []const CIR.Expr.Idx,
) Allocator.Error!artifact_serialize.Span {
    const count = rest.len + @intFromBool(first != null);
    if (count == 0) return .{ .start = 0, .len = 0 };
    const start = std.math.cast(u32, pool.items.len) orelse return error.OutOfMemory;
    const len = std.math.cast(u32, count) orelse return error.OutOfMemory;
    try pool.ensureUnusedCapacity(allocator, count);
    if (first) |expr| pool.appendAssumeCapacity(.{ .checked_expr = reachedOperand(bodies, expr) });
    for (rest) |expr| pool.appendAssumeCapacity(.{ .checked_expr = reachedOperand(bodies, expr) });
    return .{ .start = start, .len = len };
}

fn zeroPayloadTagIdent(module: TypedCIR.Module, expr_idx: CIR.Expr.Idx) ?Ident.Idx {
    const data = module.expr(expr_idx).data;
    if (data == .e_zero_argument_tag) return data.e_zero_argument_tag.name;
    if (data == .e_tag and data.e_tag.args.span.len == 0) return data.e_tag.name;
    return null;
}

fn reachedOperand(bodies: anytype, expr: CIR.Expr.Idx) CIR.Expr.Idx {
    if (!bodies.index().reachesExpr(expr)) {
        base.invariant(
            "checked static dispatch invariant violated: dispatch operand {d} is not reached by the checked bodies",
            .{@backingInt(expr)},
        );
    }
    return expr;
}

const LiteralConversionKind = enum { numeral, quote };

/// Whether the literal at `node` converts through its target's `from_numeral`
/// or `from_quote`: a reached literal expression the body reader reports as a
/// converting literal, or a reached custom literal pattern.
fn literalNodeConverts(bodies: anytype, node: u32, kind: LiteralConversionKind) bool {
    switch (bodies.index().nodeKind(node)) {
        .expr => {
            const data = bodies.exprData(@fromBackingInt(@intCast(node)));
            return switch (kind) {
                .numeral => data == .numeral,
                .quote => data == .str and bodies.quoteIsCustom(node),
            };
        },
        .pattern => return bodies.literalPatternIsCustom(@fromBackingInt(@intCast(node))),
        .statement, .unreached => return false,
    }
}

test "method registry can be empty" {
    var registry: MethodRegistry = .{};
    registry.deinit(std.testing.allocator);
}

test "method registry finalization sorts entries for binary lookup" {
    const allocator = std.testing.allocator;

    const entries = try allocator.alloc(MethodRegistryEntry, 3);
    defer allocator.free(entries);

    entries[0] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @fromBackingInt(@intCast(2)) },
        .target = testMethodTarget(@fromBackingInt(@intCast(20))),
    };
    entries[0].target.?.kind = .{ .structural = .equality };
    entries[1] = .{
        .key = .{ .owner = .{ .builtin = .list }, .method = @fromBackingInt(@intCast(1)) },
        .target = testMethodTarget(@fromBackingInt(@intCast(10))),
    };
    entries[2] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @fromBackingInt(@intCast(1)) },
        .target = testMethodTarget(@fromBackingInt(@intCast(15))),
    };

    finalizeMethodRegistryEntries(entries);

    var registry = MethodRegistry{ .entries = entries };
    const found = registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @fromBackingInt(@intCast(1)) }) orelse return error.MissingSortedMethodTarget;
    try std.testing.expectEqual(@as(CIR.Def.Idx, @fromBackingInt(@intCast(15))), found.target.def_idx);
    const structural = registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @fromBackingInt(@intCast(2)) }) orelse return error.MissingStructuralMethodTarget;
    try std.testing.expectEqual(StructuralKind.equality, structural.target.kind.structural);
    try std.testing.expect(registry.lookup(.{ .owner = .{ .builtin = .list }, .method = @fromBackingInt(@intCast(2)) }) == null);
}

test "method registry distinguishes a rejected declaration from an undeclared method" {
    const allocator = std.testing.allocator;

    const entries = try allocator.alloc(MethodRegistryEntry, 2);
    defer allocator.free(entries);

    entries[0] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @fromBackingInt(@intCast(1)) },
        .target = testMethodTarget(@fromBackingInt(@intCast(10))),
    };
    entries[1] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @fromBackingInt(@intCast(2)) },
        .target = null,
    };

    finalizeMethodRegistryEntries(entries);

    var registry = MethodRegistry{ .entries = entries };
    const declared = registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @fromBackingInt(@intCast(1)) }) orelse
        return error.MissingDeclaredMethodTarget;
    try std.testing.expectEqual(@as(CIR.Def.Idx, @fromBackingInt(@intCast(10))), declared.target.def_idx);

    const rejected = registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @fromBackingInt(@intCast(2)) }) orelse
        return error.MissingRejectedMethodEntry;
    try std.testing.expect(rejected == .rejected);

    try std.testing.expect(registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @fromBackingInt(@intCast(3)) }) == null);
}

fn testMethodTarget(def_idx: CIR.Def.Idx) MethodTarget {
    return .{
        .module_idx = 0,
        .def_idx = def_idx,
        .kind = .{
            .local_proc = .{
                .binder = undefined, // The lookup test only asserts def_idx; target kind is never read.
                .expr = undefined, // The lookup test only asserts def_idx; target kind is never read.
                .context_anchor = undefined, // The lookup test only asserts def_idx; target kind is never read.
            },
        },
        .callable_ty = undefined, // The lookup test only asserts def_idx; callable type is never read.
    };
}
