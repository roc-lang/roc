//! Checked static-dispatch registry and normalized dispatch-site records.
//!
//! The registry is built at checked-module publication. Post-check lowering uses
//! its exact callable-or-structural resolutions; the dispatch-site record
//! chooses the dispatcher type variable explicitly.

const std = @import("std");
const builtin_config = @import("builtin");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const TypedCIR = @import("typed_cir.zig");
const canonical = @import("canonical_names.zig");
const checked_ids = @import("checked_ids.zig");
const collections = @import("collections");
const artifact_serialize = @import("artifact_serialize.zig");
const dispatch_evidence = @import("dispatch_evidence.zig");
const SerializedSlice = artifact_serialize.SerializedSlice;
const CompactWriter = collections.CompactWriter;

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;
pub const NumericDefaultPhase = types.literal_defaulting.NumericDefaultPhase;
const CheckedTypeId = checked_ids.CheckedTypeId;
const CheckedExprId = checked_ids.CheckedExprId;
const CheckedStatementId = checked_ids.CheckedStatementId;
const CheckedStringLiteralId = checked_ids.CheckedStringLiteralId;
const PatternBinderId = checked_ids.PatternBinderId;
const DispatchScopeId = checked_ids.DispatchScopeId;

const DispatchExprTag = enum {
    e_dispatch_call,
    e_interpolation,
    e_type_dispatch_call,
    e_method_eq,
};

fn typeDispatchOwnerVar(module: TypedCIR.Module, stmt_idx: CIR.Statement.Idx) Var {
    const stmt = module.getStatement(stmt_idx);
    const tag = std.meta.activeTag(stmt);
    if (tag == .s_type_var_alias) return ModuleEnv.varFrom(stmt.s_type_var_alias.type_var_anno);
    if (tag == .s_alias_decl) return ModuleEnv.varFrom(stmt_idx);
    @panic("type dispatch owner statement was not a type-var alias or type alias");
}

/// Public `ProcedureTemplateLookup` declaration.
pub const ProcedureTemplateLookup = struct {
    module_idx: u32,
    by_def: []const ProcedureTemplateLookupEntry = &.{},

    pub fn entryForDef(self: *const ProcedureTemplateLookup, def_idx: CIR.Def.Idx) ?ProcedureTemplateLookupEntry {
        const found = artifact_serialize.binarySearchByKey(ProcedureTemplateLookupEntry, CIR.Def.Idx, self.by_def, def_idx, templateEntryOrder) orelse return null;
        return found.*;
    }
};

fn templateEntryOrder(e: ProcedureTemplateLookupEntry, key: CIR.Def.Idx) std.math.Order {
    return std.math.order(@intFromEnum(e.def), @intFromEnum(key));
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
        return @intFromEnum(lhs.def) < @intFromEnum(rhs.def);
    }
};

/// Public `MethodOwner` declaration.
///
/// A method owner is identified by CONTENT: the declaring module's deep
/// content identity plus the declared type name (see `base.module_identity`
/// and `canonical.NominalTypeKey`). Statement indices and module name text
/// never participate. Compiler-builtin owners keep their dedicated enum so
/// builtin dispatch stays exact across differently-spelled builtin idents.
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
    return owner == .iter or owner == .stream;
}

/// Producer-owned identity of an internal iterator representation. This is
/// shared by Monotype and ConstStore so crossing that boundary never depends
/// on the ordinal layout of two separately maintained enums.
pub const IteratorKind = enum(u8) {
    none,
    custom,
    list,
    list_rev,
    str,
    single,
    range,
    numeric_until,
    numeric_to,
    map,
    keep_if,
    drop_if,
    take_first,
    drop_first,
    concat,
    append,
    forced_dynamic,

    /// See `IteratorComponentTopology`. Null for `none` (no minted kind) and
    /// `forced_dynamic` (no static topology).
    pub fn componentTopology(self: IteratorKind) ?IteratorComponentTopology {
        return switch (self) {
            .none, .forced_dynamic => null,
            .range, .numeric_until, .numeric_to => .source_without_components,
            .custom, .list, .list_rev, .str, .single => .source_with_components,
            .map, .keep_if, .drop_if, .take_first, .drop_first, .concat, .append => .adapter,
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
/// checking still has the defining builtin declaration in hand.
pub const IteratorProcedureId = enum(u8) {
    iter_iter,
    iter_next,
    iter_custom,
    iter_single,
    list_iter,
    list_iter_rev,
    str_iter_utf8,
    iter_map,
    iter_keep_if,
    iter_drop_if,
    iter_take_first,
    iter_drop_first,
    iter_concat,
    iter_append,
    range_iter,
    numeric_range_delegate,
    numeric_to,
    numeric_until,
    iter_from_step,
    range_done,

    /// Whether this exact checked procedure returns an iterator value. Keep
    /// this exhaustive: adding an iterator procedure must declare its producer
    /// role here instead of letting a later pass infer it from result shape.
    pub fn producesIteratorValue(self: IteratorProcedureId) bool {
        return switch (self) {
            .iter_next,
            .range_done,
            => false,
            .iter_iter,
            .iter_custom,
            .iter_single,
            .list_iter,
            .list_iter_rev,
            .str_iter_utf8,
            .iter_map,
            .iter_keep_if,
            .iter_drop_if,
            .iter_take_first,
            .iter_drop_first,
            .iter_concat,
            .iter_append,
            .range_iter,
            .numeric_range_delegate,
            .numeric_to,
            .numeric_until,
            .iter_from_step,
            => true,
        };
    }

    /// The minted iterator kind this procedure constructs, or null for
    /// procedures that pass through or consume an existing iterator
    /// (`Iter.iter`, `Iter.next`) and the internal step constructors. This is
    /// the one producer-to-kind mapping; Monotype's per-procedure request
    /// construction reads it instead of restating kinds beside each arm.
    pub fn iteratorKind(self: IteratorProcedureId) ?IteratorKind {
        return switch (self) {
            .iter_iter, .iter_next, .numeric_range_delegate, .iter_from_step, .range_done => null,
            .iter_custom => .custom,
            .iter_single => .single,
            .list_iter => .list,
            .list_iter_rev => .list_rev,
            .str_iter_utf8 => .str,
            .iter_map => .map,
            .iter_keep_if => .keep_if,
            .iter_drop_if => .drop_if,
            .iter_take_first => .take_first,
            .iter_drop_first => .drop_first,
            .iter_concat => .concat,
            .iter_append => .append,
            .range_iter => .range,
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
            .list_iter, .list_iter_rev, .iter_iter => true,
            .iter_next,
            .iter_custom,
            .iter_single,
            .str_iter_utf8,
            .iter_map,
            .iter_keep_if,
            .iter_drop_if,
            .iter_take_first,
            .iter_drop_first,
            .iter_concat,
            .iter_append,
            .range_iter,
            .numeric_range_delegate,
            .numeric_to,
            .numeric_until,
            .iter_from_step,
            .range_done,
            => false,
        };
    }
};

const IteratorProcedureNameEntry = struct { []const u8, IteratorProcedureId };

const iterator_procedure_base_names = [_]IteratorProcedureNameEntry{
    .{ "Builtin.Iter.iter", .iter_iter },
    .{ "Builtin.Iter.next", .iter_next },
    .{ "Builtin.Iter.custom", .iter_custom },
    .{ "Builtin.Iter.single", .iter_single },
    .{ "Builtin.List.iter", .list_iter },
    .{ "Builtin.List.iter_rev", .list_iter_rev },
    .{ "Builtin.Str.iter_utf8", .str_iter_utf8 },
    .{ "Builtin.Iter.map", .iter_map },
    .{ "Builtin.Iter.keep_if", .iter_keep_if },
    .{ "Builtin.Iter.drop_if", .iter_drop_if },
    .{ "Builtin.Iter.take_first", .iter_take_first },
    .{ "Builtin.Iter.drop_first", .iter_drop_first },
    .{ "Builtin.Iter.concat", .iter_concat },
    .{ "Builtin.Iter.append", .iter_append },
    .{ "Builtin.Num.Range.iter", .range_iter },
    .{ "iter_from_step", .iter_from_step },
    .{ "Builtin.iter_from_step", .iter_from_step },
    .{ "range_done", .range_done },
    .{ "Builtin.range_done", .range_done },
};

// Single-sourced from BuiltinLowLevel so the numeric rosters cannot drift from
// the low-level registration tables. Range iteration covers every numeric
// type, while `to`/`until` need `minus_try` and therefore exclude IEEE floats.
const iterator_range_numeric_type_names = can.BuiltinLowLevel.numeric_type_names;

const iterator_to_until_numeric_type_names = can.BuiltinLowLevel.non_float_numeric_type_names;

const iterator_procedure_name_entries = blk: {
    var entries: [
        iterator_procedure_base_names.len +
            iterator_range_numeric_type_names.len +
            iterator_to_until_numeric_type_names.len * 2
    ]IteratorProcedureNameEntry = undefined;
    for (iterator_procedure_base_names, 0..) |entry, index| entries[index] = entry;
    var index = iterator_procedure_base_names.len;
    for (iterator_range_numeric_type_names) |numeric| {
        entries[index] = .{ "Builtin.Num." ++ numeric ++ ".range_iter", .numeric_range_delegate };
        index += 1;
    }
    for (iterator_to_until_numeric_type_names) |numeric| {
        entries[index] = .{ "Builtin.Num." ++ numeric ++ ".to", .numeric_to };
        index += 1;
        entries[index] = .{ "Builtin.Num." ++ numeric ++ ".until", .numeric_until };
        index += 1;
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
pub const hoist_preserving_method_names = [_][]const u8{ "iter", "iter_rev" };

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
pub const iterator_procedure_names = iterator_procedure_base_names;

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
    procedure,
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
            .graph_participating => |target| target.iterator_procedure,
            .procedure, .low_level, .intrinsic => null,
        };
    }
};

/// Producer-authored graph requirements for a representation-sensitive target.
pub const GraphParticipatingTarget = struct {
    iterator_procedure: ?IteratorProcedureId = null,
};

/// Exact checked procedure selected for a method registry entry.
pub const ProcedureMethodTarget = struct {
    proc: canonical.ProcedureValueRef,
    template: canonical.ProcedureTemplateRef,
    runtime_target: ProcedureRuntimeTarget = .procedure,
};

fn procedureRuntimeTargetForDef(
    module: TypedCIR.Module,
    def_idx: CIR.Def.Idx,
    method_owner: MethodOwner,
) ProcedureRuntimeTarget {
    if (intrinsicForProcedureDef(module, def_idx)) |intrinsic| {
        if (intrinsic.callsiteArity() != null) return .{ .intrinsic = intrinsic };
    }
    if (iteratorProcedureForDef(module, def_idx)) |iterator| return .{ .graph_participating = .{
        .iterator_procedure = iterator,
    } };
    if (std.meta.activeTag(method_owner) == .builtin and isIteratorOwner(method_owner.builtin)) {
        return .{ .graph_participating = .{} };
    }
    if (module.moduleEnvConst().providedLowLevelForDef(def_idx)) |op| return .{ .low_level = op };
    return .procedure;
}

/// Exact compiler-intrinsic identity for an annotation-only builtin procedure.
pub fn intrinsicForProcedureDef(module: TypedCIR.Module, def_idx: CIR.Def.Idx) ?can.BuiltinLowLevel.IntrinsicId {
    const expr_data = module.def(def_idx).expr.data;
    if (std.meta.activeTag(expr_data) != .e_anno_only) return null;
    const expr_ident = expr_data.e_anno_only.ident;
    const env = module.moduleEnvConst();
    if (!can.BuiltinLowLevel.isBuiltinModule(env)) return null;
    return can.BuiltinLowLevel.intrinsicAnnotation(env, expr_ident);
}

/// Public `LocalProcedureMethodTarget` declaration.
pub const LocalProcedureMethodTarget = struct {
    binder: PatternBinderId,
    expr: CheckedExprId,
    /// Exact checked statement whose lexical position declares this target.
    context_anchor: CheckedStatementId,
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
};

/// What resolving an (owner, method) pair against the checked method
/// registries found. Distinct from "no view declares this method": a
/// declaration the earlier stages rejected is still declared, but it has no
/// runtime target, so every dispatch that lands on it is a `checked_error`.
pub const CheckedMethodLookup = union(enum) {
    target: MethodTarget,
    rejected,

    /// The lowerable target. Post-check stages run only on programs with no
    /// diagnostics, so a rejected declaration reaching one is a compiler bug,
    /// not a shape to route around.
    pub fn requireTarget(self: CheckedMethodLookup, comptime context: []const u8) MethodTarget {
        return switch (self) {
            .target => |target| target,
            .rejected => std.debug.panic(
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
};

/// Public `MethodRegistry` declaration.
pub const MethodRegistry = struct {
    entries: []MethodRegistryEntry = &.{},

    pub const Serialized = extern struct {
        entries: SerializedSlice(MethodRegistryEntry) = .{},
        pub fn serialize(self: *Serialized, t: *const MethodRegistry, gpa: Allocator, writer: *CompactWriter) Allocator.Error!void {
            try self.entries.serialize(t.entries, gpa, writer);
        }
        pub fn deserialize(self: *const Serialized, base_addr: usize) MethodRegistry {
            return .{ .entries = self.entries.deserialize(base_addr) };
        }
    };

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

    /// Build-time-only teardown (see `StaticDispatchPlanTable.deinit`): a frozen
    /// table's `entries` alias the artifact buffer and are freed wholesale by the
    /// artifact, never here.
    pub fn deinit(self: *MethodRegistry, allocator: Allocator) void {
        allocator.free(self.entries);
        self.* = .{};
    }

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        local_templates: *const ProcedureTemplateLookup,
        available_artifacts: anytype,
        checked_types: anytype,
        checked_bodies: anytype,
    ) Allocator.Error!MethodRegistry {
        var entries = std.ArrayList(MethodRegistryEntry).empty;
        errdefer entries.deinit(allocator);

        const module_idx = module.moduleIndex();
        if (module_idx != local_templates.module_idx) {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic(
                    "checked static dispatch registry invariant violated: template lookup module {d} does not match module {d}",
                    .{ local_templates.module_idx, module_idx },
                );
            }
            unreachable;
        }

        const module_env = module.moduleEnvConst();
        const idents = module.identStoreConst();
        const module_name = try names.internModuleIdent(idents, module.qualifiedModuleIdent());

        for (module.methodDefEntries()) |entry| {
            const method_ident = module_env.lookupMethodIdentForMethodOwnerConst(entry.key.ownerIdent(), entry.key.methodIdent()) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch registry invariant violated: method def for owner {d} method {d} has no method ident",
                        .{ @intFromEnum(entry.key.owner), entry.key.method_ident_bits },
                    );
                }
                unreachable;
            };
            const def_idx = entry.value.def_idx;
            if (unsupportedGeneratedMethodBinding(module, entry.value)) continue;
            const method_owner = try methodOwnerForRegistryEntry(
                module,
                names,
                available_artifacts,
                entry.key.ownerIdent(),
            );
            const method_key: MethodKey = .{
                .owner = method_owner,
                .method = try names.internMethodIdent(idents, entry.key.methodIdent()),
            };
            // A rejected declaration is still declared. Record the key with no
            // target so dispatch resolution can tell it apart from a method no
            // view declares, and resolve it to a checked error instead of
            // hunting for a runtime target that cannot exist.
            if (methodBindingIsRejectedDeclaration(module, entry.value)) {
                try entries.append(allocator, .{ .key = method_key, .target = null });
                continue;
            }
            var referenced_callable_var: ?Var = null;
            const target_kind: MethodTargetKind = if (generatedStructuralTargetForMethodBinding(module, entry.value)) |generated|
                .{ .structural = generated }
            else if (local_templates.entryForDef(def_idx)) |template_entry| blk: {
                switch (template_entry.kind) {
                    .structural => |kind| break :blk .{ .structural = kind },
                    .callable => {
                        const template = template_entry.template;
                        const export_name = try names.internExportIdent(idents, method_ident);
                        const proc_base = try names.internProcBase(.{
                            .module_name = module_name,
                            .export_name = export_name,
                            .kind = .checked_source,
                            .ordinal = @intFromEnum(def_idx),
                            .source_def_idx = @intFromEnum(def_idx),
                        });
                        break :blk .{ .procedure = .{
                            .proc = .{ .artifact = template.artifact, .proc_base = proc_base },
                            .template = template,
                            .runtime_target = procedureRuntimeTargetForDef(module, def_idx, method_owner),
                        } };
                    },
                }
            } else if (localProcedureTargetForMethodBinding(module, checked_bodies, entry.key.owner, entry.value)) |local|
                .{ .local_proc = local }
            else if (referencedProcedureTargetForMethodBinding(
                module,
                local_templates,
                checked_bodies,
                entry.value,
                method_owner,
            )) |referenced| blk: {
                referenced_callable_var = referenced.callable_var;
                break :blk referenced.kind;
            } else
                // Associated values that resolve to neither a callable nor an
                // explicitly structural declaration are checked field access,
                // not static-dispatch resolutions.
                continue;
            const callable_var = referenced_callable_var orelse methodTargetCallableVar(module, def_idx, entry.value, target_kind);

            try entries.append(allocator, .{
                .key = method_key,
                .target = .{
                    .module_idx = module_idx,
                    .def_idx = def_idx,
                    .kind = target_kind,
                    .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, callable_var),
                },
            });
        }

        finalizeMethodRegistryEntries(entries.items);

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }
};

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

fn methodTargetCallableVar(
    module: TypedCIR.Module,
    def_idx: CIR.Def.Idx,
    binding: ModuleEnv.MethodBinding,
    target_kind: MethodTargetKind,
) Var {
    return switch (target_kind) {
        .procedure => module.defType(def_idx),
        .structural => ModuleEnv.varFrom(binding.type_node_idx),
        .local_proc => blk: {
            const raw_node = @intFromEnum(binding.type_node_idx);
            const statement: CIR.Statement.Idx = @enumFromInt(raw_node);
            const statement_data = module.getStatement(statement);
            if (std.meta.activeTag(statement_data) != .s_decl) unreachable;
            const decl = statement_data.s_decl;
            break :blk module.exprType(decl.expr);
        },
    };
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
    const raw_node = @intFromEnum(binding.type_node_idx);
    if (raw_node >= module.nodeCount()) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: method binding node {d} is outside the module node store",
                .{raw_node},
            );
        }
        unreachable;
    }

    const node_tag = module.nodeTag(binding.type_node_idx);
    if (node_tag == .def) return module.moduleEnvConst().store.getDef(binding.def_idx).expr;
    if (node_tag != .statement_decl) return null;

    const statement: CIR.Statement.Idx = @enumFromInt(raw_node);
    const statement_data = module.getStatement(statement);
    if (std.meta.activeTag(statement_data) != .s_decl) return null;
    return statement_data.s_decl.expr;
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
    checked_bodies: anytype,
    owner_statement: CIR.Statement.Idx,
    binding: ModuleEnv.MethodBinding,
) ?LocalProcedureMethodTarget {
    const raw_node = @intFromEnum(binding.type_node_idx);
    if (raw_node >= module.nodeCount()) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: method binding node {d} is outside the module node store",
                .{raw_node},
            );
        }
        unreachable;
    }
    if (module.nodeTag(binding.type_node_idx) != .statement_decl) return null;

    const statement: CIR.Statement.Idx = @enumFromInt(raw_node);
    const statement_data = module.getStatement(statement);
    if (std.meta.activeTag(statement_data) != .s_decl) return null;
    const decl = statement_data.s_decl;

    if (!localProcedureExpr(module, decl.expr)) return null;

    const expr = checked_bodies.exprIdForSource(decl.expr) orelse return null;
    const binder = checked_bodies.patternBinderForSource(decl.pattern) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: local method pattern {d} has no checked binder",
                .{@intFromEnum(decl.pattern)},
            );
        }
        unreachable;
    };

    const context_anchor = checked_bodies.statementIdForSource(owner_statement) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: local method owner statement {d} has no checked statement",
                .{@intFromEnum(owner_statement)},
            );
        }
        unreachable;
    };

    return .{
        .binder = binder,
        .expr = expr,
        .context_anchor = context_anchor,
    };
}

fn localProcedureExpr(module: TypedCIR.Module, expr_idx: CIR.Expr.Idx) bool {
    const tag = std.meta.activeTag(module.expr(expr_idx).data);
    return tag == .e_lambda or tag == .e_closure;
}

const ReferencedProcedureTarget = struct {
    kind: MethodTargetKind,
    callable_var: Var,
};

/// Resolve a function-typed associated value bound by reference
/// (`method = top_level_fn`) to the referenced procedure. The reference chain
/// is followed through top-level defs and associated declarations until it
/// reaches a procedure-backed binding; a chain that never reaches one is an
/// associated value, not a call target, and resolves to null.
fn referencedProcedureTargetForMethodBinding(
    module: TypedCIR.Module,
    local_templates: *const ProcedureTemplateLookup,
    checked_bodies: anytype,
    binding: ModuleEnv.MethodBinding,
    method_owner: MethodOwner,
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
            if (local_templates.entryForDef(target_def_idx)) |template_entry| {
                return .{
                    .kind = switch (template_entry.kind) {
                        .callable => .{ .procedure = .{
                            .proc = .{ .artifact = template_entry.template.artifact, .proc_base = template_entry.template.proc_base },
                            .template = template_entry.template,
                            .runtime_target = procedureRuntimeTargetForDef(module, target_def_idx, method_owner),
                        } },
                        .structural => |kind| .{ .structural = kind },
                    },
                    .callable_var = module.defType(target_def_idx),
                };
            }
            expr_idx = module_env.store.getDef(target_def_idx).expr;
            continue;
        }
        if (statementDeclForBoundPattern(module, pattern_idx)) |decl| {
            if (localProcedureExpr(module, decl.expr)) {
                const expr = checked_bodies.exprIdForSource(decl.expr) orelse return null;
                const binder = checked_bodies.patternBinderForSource(decl.pattern) orelse return null;
                return .{
                    .kind = .{ .local_proc = .{
                        .binder = binder,
                        .expr = expr,
                        .context_anchor = checked_bodies.statementIdForSource(decl.statement) orelse return null,
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
        if (module.nodeTag(@enumFromInt(raw_node)) != .statement_decl) continue;
        const statement: CIR.Statement.Idx = @enumFromInt(raw_node);
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
    available_artifacts: anytype,
    owner: ModuleEnv.MethodOwner,
) Allocator.Error!MethodOwner {
    const owner_env = methodOwnerEnvForRegistryEntry(module, available_artifacts, owner);
    if (builtinOwnerForRegistryEntry(owner_env, owner.owner)) |builtin_owner| {
        return .{ .builtin = builtin_owner };
    }

    const identity_hash = owner_env.contentIdentityHash() orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: module '{s}' has no content identity",
                .{owner_env.module_name},
            );
        }
        unreachable;
    };
    const stmt = owner_env.store.getStatement(owner.owner);
    const stmt_tag = std.meta.activeTag(stmt);
    const header_idx = if (stmt_tag == .s_nominal_decl)
        stmt.s_nominal_decl.header
    else if (stmt_tag == .s_alias_decl)
        stmt.s_alias_decl.header
    else {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: method owner statement {d} is not a type declaration",
                .{@intFromEnum(owner.owner)},
            );
        }
        unreachable;
    };
    const header = owner_env.store.getTypeHeader(header_idx);
    return .{ .nominal = .{
        .module = try names.internModuleIdentity(identity_hash),
        .type_name = try names.internTypeIdent(owner_env.getIdentStoreConst(), header.relative_name),
        .source_decl = @intFromEnum(owner.owner),
    } };
}

fn methodOwnerEnvForRegistryEntry(
    module: TypedCIR.Module,
    available_artifacts: anytype,
    owner: ModuleEnv.MethodOwner,
) *const ModuleEnv {
    const module_env = module.moduleEnvConst();
    const owner_hash = methodOwnerIdentityHashForRegistryEntry(module_env, owner);

    if (ownerEnvIdentityMatches(module_env, owner_hash)) return module_env;

    for (available_artifacts) |artifact| {
        const candidate = artifact.module_env;
        if (ownerEnvIdentityMatches(candidate, owner_hash)) return candidate;
    }

    if (@import("builtin").mode == .Debug) {
        std.debug.panic(
            "checked static dispatch registry invariant violated: could not find owner module '{s}' for receiver method",
            .{module.getIdent(owner.moduleIdent())},
        );
    }
    unreachable;
}

fn methodOwnerIdentityHashForRegistryEntry(
    module_env: *const ModuleEnv,
    owner: ModuleEnv.MethodOwner,
) *const base.ModuleIdentity.Hash {
    if (owner.moduleIdent().eql(module_env.qualified_module_ident)) {
        return module_env.contentIdentityHash() orelse {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic(
                    "checked static dispatch registry invariant violated: local module '{s}' has no content identity",
                    .{module_env.module_name},
                );
            }
            unreachable;
        };
    }

    const owner_identity = module_env.moduleIdentityForDisplayIdent(owner.moduleIdent()) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch registry invariant violated: receiver owner module '{s}' has no content identity in module '{s}'",
                .{ module_env.getIdent(owner.moduleIdent()), module_env.module_name },
            );
        }
        unreachable;
    };
    return module_env.moduleIdentityHash(owner_identity);
}

fn ownerEnvIdentityMatches(candidate: *const ModuleEnv, owner_hash: *const base.ModuleIdentity.Hash) bool {
    const candidate_hash = candidate.contentIdentityHash() orelse return false;
    return base.ModuleIdentity.eql(candidate_hash, owner_hash);
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
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch registry invariant violated: duplicate method registry key", .{});
        }
        unreachable;
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
/// writes every compared scalar explicitly. The order matches the previous
/// per-variant comparison (nominal < builtin; module identity then type name;
/// `source_decl == null` sorts before any value), so registries sorted by
/// earlier builds search identically.
const MethodOwnerSortKey = struct {
    tag: u32,
    first: u32,
    second: u32,
    third: u32,

    fn order(a: @This(), b: @This()) std.math.Order {
        if (a.tag != b.tag) return orderU32(a.tag, b.tag);
        if (a.first != b.first) return orderU32(a.first, b.first);
        if (a.second != b.second) return orderU32(a.second, b.second);
        return orderU32(a.third, b.third);
    }
};

fn methodOwnerSortKey(owner: MethodOwner) MethodOwnerSortKey {
    return switch (owner) {
        .nominal => |nominal| .{
            .tag = 0,
            .first = @intFromEnum(nominal.module),
            .second = @intFromEnum(nominal.type_name),
            // null sorts before any statement value.
            .third = if (nominal.source_decl) |source_decl| source_decl +| 1 else 0,
        },
        .builtin => |builtin_owner| .{
            .tag = 1,
            .first = @intFromEnum(builtin_owner),
            .second = 0,
            .third = 0,
        },
    };
}

fn orderEnum(comptime T: type, a: T, b: T) std.math.Order {
    return orderU32(@intFromEnum(a), @intFromEnum(b));
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
    checked_expr: CheckedExprId,
    /// Compiler-generated finite `Iter` for string interpolation. The checked
    /// expression owns the first segment and flat interpolation parts.
    generated_interpolation_iter: CheckedExprId,
    generated_numeral: ModuleEnv.NumeralLiteral,
    /// A string literal's post-escape contents, passed to `from_quote` as Str.
    generated_quote: CheckedStringLiteralId,
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
    .{ .method_name = "is_eq", .common_ident = "is_eq", .kind = .equality },
    .{ .method_name = "to_hash", .common_ident = "to_hash", .kind = .hash },
    .{ .method_name = "parser_for", .common_ident = "parser_for", .kind = .parser },
    .{ .method_name = "encoder_for", .common_ident = "encoder_for", .kind = .encoder },
    .{ .method_name = "map", .common_ident = "map", .kind = .map },
    .{ .method_name = "map!", .common_ident = "map_bang", .kind = .map_effectful },
};

/// Public `EvidenceNodeId` declaration. Index into
/// `StaticDispatchPlanTable.evidence_nodes`.
pub const EvidenceNodeId = enum(u32) { _ };

/// Public `EvidenceChainIndex` declaration.
///
/// A dispatch obligation forwarded to the enclosing callable's evidence
/// params: `index` is the canonical evidence-param index (see
/// `dispatch_evidence.zig`), and `depth` counts enclosing generalized
/// callables outward from the reference (0 = the innermost generalized
/// callable the reference appears in).
pub const EvidenceChainIndex = struct {
    depth: u16,
    index: u16,
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
    /// Whether this obligation is represented by a runtime dictionary slot.
    /// Literal-defaulting constraints remain in canonical evidence vectors for
    /// specialization, but do not become Boxy dictionary requirements.
    runtime_dictionary: bool,

    pub const Resolution = union(enum) {
        direct: EvidenceNodeId,
        constraint: EvidenceChainIndex,
        structural: StructuralEvidence,
        /// The checker proved this nested-procedure obligation is the matching
        /// evidence parameter projected from the concrete callable request.
        from_callable,
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
};

/// Producer-authored source of a target's own evidence vector.
pub const EvidenceNested = union(enum(u8)) {
    /// Checking recorded the target scheme instantiation, so publication
    /// resolved every nested obligation explicitly.
    resolved: artifact_serialize.Span,
    /// Target selection occurred only after checking had settled the
    /// dispatcher. The specialization edge must derive the target's declared
    /// evidence params from their checker-recorded paths over its concrete callable.
    from_callable,
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
};

/// Public `SiteEvidenceEntry` declaration.
///
/// Evidence for one instantiation site (keyed by the checked expression of the
/// use), covering the instantiated scheme's evidence params in canonical
/// order: a range into `StaticDispatchPlanTable.evidence_refs`. Sorted by key
/// for binary search (transform D).
pub const SiteEvidenceEntry = extern struct {
    /// `@intFromEnum` of the site's `CheckedExprId`.
    key: u32,
    start: u32,
    len: u32,
};

/// Public `EvidencePathStep` declaration: one semantic step from a type to a
/// component, in the artifact's canonical names (`data` is a positional index,
/// a `canonical.RecordFieldLabelId`, or a `canonical.TagNameId` per kind).
pub const EvidencePathStep = dispatch_evidence.PathStep;

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
    dispatcher_ty: CheckedTypeId,
    /// Whether this parameter becomes a runtime method dictionary. Literal
    /// defaulting evidence remains an ABI input for descriptor selection but
    /// does not carry method implementations at runtime.
    runtime_dictionary: bool,
    /// Checker-recorded derived implementation permitted when the concrete
    /// dispatcher has no registered method target.
    structural: ?StructuralKind = null,
    /// A pathless literal/defaultable dispatcher that checking explicitly left
    /// for monomorphic specialization. Compiler-generated edges materialize
    /// this default instead of projecting a callable path that does not exist.
    pathless_default_phase: ?NumericDefaultPhase = null,
    path: artifact_serialize.Span = .{},
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
/// been instantiated. A serialized artifact can never contain
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
    evidence_dependent: EvidenceChainIndex,
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

/// Public `StaticDispatchCallPlan` declaration.
pub const StaticDispatchCallPlan = struct {
    expr: CheckedExprId,
    method: canonical.MethodNameId,
    dispatcher: StaticDispatchDispatcher,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    /// Range into `StaticDispatchPlanTable.operand_pool` (transform B).
    args: artifact_serialize.Span = .{},
    result_mode: StaticDispatchResultMode,
    /// Assigned by `resolveTotalDispatchPlans` during CheckedModule construction; the default is
    /// a construction placeholder the pass overwrites for every plan.
    resolution: CheckedCallResolution = .checked_error,

    /// The plan's operands within its table's pool.
    pub fn argsSlice(self: StaticDispatchCallPlan, table: *const StaticDispatchPlanTable) []const StaticDispatchOperand {
        return table.operand_pool[self.args.start .. self.args.start + self.args.len];
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

/// One exact method edge inside a compiler-generated parser or encoder.
pub const GeneratedCodecCall = struct {
    method: canonical.MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    subject_ty: ?CheckedTypeId = null,
    generated_codec_derivation: ?GeneratedCodecDerivationId = null,
    /// Nested evidence for the instantiated target callable, in its canonical
    /// evidence-parameter order.
    nested: artifact_serialize.Span = .{},
};

/// Exact checked contract for one compiler-generated codec instantiation.
pub const GeneratedCodecDerivation = struct {
    kind: GeneratedCodecDerivationKind,
    source_constructor_ty: CheckedTypeId,
    source_runtime_ty: CheckedTypeId,
    source_shape_ty: CheckedTypeId,
    source_encoding_ty: CheckedTypeId,
    source_state_ty: CheckedTypeId,
    source_error_ty: CheckedTypeId,
    constructor_ty: CheckedTypeId,
    runtime_ty: CheckedTypeId,
    shape_ty: CheckedTypeId,
    encoding_ty: CheckedTypeId,
    state_ty: CheckedTypeId,
    error_ty: CheckedTypeId,
    calls: artifact_serialize.Span = .{},

    pub fn callsSlice(self: GeneratedCodecDerivation, table: *const StaticDispatchPlanTable) []const GeneratedCodecCall {
        return table.generated_codec_calls[self.calls.start .. self.calls.start + self.calls.len];
    }
};

/// Public `IteratorForPlanId` declaration.
pub const IteratorForPlanId = enum(u32) { _ };

/// Public `IteratorDispatchOperand` declaration.
pub const IteratorDispatchOperand = union(enum) {
    checked_expr: CheckedExprId,
    loop_iterator_state,
};

/// Public `IteratorDispatchCall` declaration.
pub const IteratorDispatchCall = struct {
    method: canonical.MethodNameId,
    dispatcher_ty: CheckedTypeId,
    callable_ty: CheckedTypeId,
    dispatcher_arg_index: u32,
    /// Range into `StaticDispatchPlanTable.iter_operand_pool` (transform B).
    args: artifact_serialize.Span = .{},
    /// Assigned by `resolveTotalDispatchPlans` during CheckedModule construction; the default is
    /// a construction placeholder the pass overwrites for every plan.
    resolution: CheckedCallResolution = .checked_error,

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
    known_tag: canonical.TagLabelId,
    unknown_tag: canonical.TagLabelId,
    done_tag: canonical.TagLabelId,
    one_tag: canonical.TagLabelId,
    skip_tag: canonical.TagLabelId,
    item_field: canonical.RecordFieldLabelId,
    rest_field: canonical.RecordFieldLabelId,
};

/// Public `IteratorForPlan` declaration.
pub const IteratorForPlan = struct {
    iter: IteratorDispatchCall,
    next: IteratorDispatchCall,
    iterable: CheckedExprId,
    item_ty: CheckedTypeId,
    iterator_ty: CheckedTypeId,
    step_ty: CheckedTypeId,
    step_topology: IteratorStepTopology,
};

/// Public `StaticDispatchPlanTable` declaration.
/// Relocatable replacement for an `AutoHashMap(idx -> id)`: a `(key, val)` pair
/// (both `@intFromEnum` u32s) stored in a sorted, binary-searchable POD slice
/// (transform D). Keys are unique (each source node/expr maps to one plan).
pub const PlanKV = extern struct { key: u32, val: u32 };

fn planKvLessThan(_: void, a: PlanKV, b: PlanKV) bool {
    return a.key < b.key;
}

fn planKvOrder(e: PlanKV, key: u32) std.math.Order {
    return std.math.order(e.key, key);
}

/// Binary-search a sorted `PlanKV` slice; returns the value (`@intFromEnum` of
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

fn sortedFromMap(allocator: Allocator, map: anytype) Allocator.Error![]PlanKV {
    const out = try allocator.alloc(PlanKV, map.count());
    errdefer allocator.free(out);
    var it = map.iterator();
    var i: usize = 0;
    while (it.next()) |entry| : (i += 1) {
        out[i] = .{ .key = @intFromEnum(entry.key_ptr.*), .val = @intFromEnum(entry.value_ptr.*) };
    }
    std.mem.sort(PlanKV, out, {}, planKvLessThan);
    return out;
}

/// Resolved static-dispatch plans for a checked module: the per-call-site plans, the
/// sorted expr/node → plan indexes, and the shared operand pools the plans reference
/// (transform D). Reconstituted as plain slices on deserialize.
pub const StaticDispatchPlanTable = struct {
    plans: []StaticDispatchCallPlan = &.{},
    /// `CIR.Expr.Idx` -> `StaticDispatchPlanId`, sorted by key (transform D).
    by_expr: []PlanKV = &.{},
    /// `CIR.Node.Idx` -> `StaticDispatchPlanId`, sorted by key.
    numeral_by_node: []PlanKV = &.{},
    /// `CIR.Node.Idx` -> `StaticDispatchPlanId`, sorted by key.
    quote_by_node: []PlanKV = &.{},
    iterator_for_plans: []IteratorForPlan = &.{},
    /// Exactly one checker-authored public iterator representation topology.
    iterator_topologies: []IteratorRepresentationTopology = &.{},
    /// `CIR.Node.Idx` -> `IteratorForPlanId`, sorted by key.
    iterator_for_by_node: []PlanKV = &.{},
    /// Build-time collection of every plan referenced by each template.
    /// CheckedModule construction consumes this to resolve evidence, then outputs the
    /// category-specific pools below for post-check consumers.
    template_refs: []StaticDispatchPlanId = &.{},
    /// Direct calls grouped by checked procedure template.
    direct_template_refs: []StaticDispatchPlanId = &.{},
    /// Evidence-dependent or representation-sensitive relations grouped by
    /// checked procedure template.
    dispatch_relation_refs: []StaticDispatchPlanId = &.{},
    /// Shared flat pool of plan operands (transform-B side list).
    operand_pool: []const StaticDispatchOperand = &.{},
    /// Shared flat pool of iterator-plan operands.
    iter_operand_pool: []const IteratorDispatchOperand = &.{},
    /// Concrete dispatch targets with nested evidence (`EvidenceNodeId`s).
    evidence_nodes: []EvidenceNode = &.{},
    /// Flat pool of evidence: node `nested` ranges and site-evidence ranges.
    evidence_refs: []CheckedEvidence = &.{},
    /// Checked-expr-keyed evidence for instantiation sites, sorted by key.
    site_evidence: []SiteEvidenceEntry = &.{},
    /// Exact generated-codec contracts emitted by checking.
    generated_codec_derivations: []GeneratedCodecDerivation = &.{},
    /// Shared flat pool backing `GeneratedCodecDerivation.calls`.
    generated_codec_calls: []GeneratedCodecCall = &.{},

    pub const Serialized = extern struct {
        plans: SerializedSlice(StaticDispatchCallPlan) = .{},
        by_expr: SerializedSlice(PlanKV) = .{},
        numeral_by_node: SerializedSlice(PlanKV) = .{},
        quote_by_node: SerializedSlice(PlanKV) = .{},
        iterator_for_plans: SerializedSlice(IteratorForPlan) = .{},
        iterator_topologies: SerializedSlice(IteratorRepresentationTopology) = .{},
        iterator_for_by_node: SerializedSlice(PlanKV) = .{},
        template_refs: SerializedSlice(StaticDispatchPlanId) = .{},
        direct_template_refs: SerializedSlice(StaticDispatchPlanId) = .{},
        dispatch_relation_refs: SerializedSlice(StaticDispatchPlanId) = .{},
        operand_pool: SerializedSlice(StaticDispatchOperand) = .{},
        iter_operand_pool: SerializedSlice(IteratorDispatchOperand) = .{},
        evidence_nodes: SerializedSlice(EvidenceNode) = .{},
        evidence_refs: SerializedSlice(CheckedEvidence) = .{},
        site_evidence: SerializedSlice(SiteEvidenceEntry) = .{},
        generated_codec_derivations: SerializedSlice(GeneratedCodecDerivation) = .{},
        generated_codec_calls: SerializedSlice(GeneratedCodecCall) = .{},

        comptime {
            // 17 side lists → 17 base-pointer fixups on deserialize, never a
            // function of how many plans/operands the table holds.
            std.debug.assert(artifact_serialize.relocatablePointerCount(Serialized) == 17);
        }

        const Serde = artifact_serialize.SliceStoreSerde(StaticDispatchPlanTable, @This());
        pub const serialize = Serde.serialize;
        pub const deserialize = Serde.deserialize;
    };

    pub fn fromModule(
        allocator: Allocator,
        module: TypedCIR.Module,
        names: *canonical.CanonicalNameStore,
        checked_types: anytype,
        checked_bodies: anytype,
        build_data: *PlanTableBuildData,
    ) Allocator.Error!StaticDispatchPlanTable {
        var plans = std.ArrayList(StaticDispatchCallPlan).empty;
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
        var by_expr: std.AutoHashMapUnmanaged(CIR.Expr.Idx, StaticDispatchPlanId) = .{};
        errdefer by_expr.deinit(allocator);
        var numeral_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, StaticDispatchPlanId) = .{};
        errdefer numeral_by_node.deinit(allocator);
        var quote_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, StaticDispatchPlanId) = .{};
        errdefer quote_by_node.deinit(allocator);
        var iterator_for_plans = std.ArrayList(IteratorForPlan).empty;
        errdefer iterator_for_plans.deinit(allocator);
        var generated_codec_derivations = std.ArrayList(GeneratedCodecDerivation).empty;
        errdefer generated_codec_derivations.deinit(allocator);
        var generated_codec_calls = std.ArrayList(GeneratedCodecCall).empty;
        errdefer generated_codec_calls.deinit(allocator);
        const iterator_topologies = try allocator.alloc(IteratorRepresentationTopology, 1);
        errdefer allocator.free(iterator_topologies);
        iterator_topologies[0] = .{
            .len_field = try names.internRecordFieldLabel("len_if_known"),
            .step_field = try names.internRecordFieldLabel("step"),
            .known_tag = try names.internTagLabel("Known"),
            .unknown_tag = try names.internTagLabel("Unknown"),
            .done_tag = try names.internTagLabel("Done"),
            .one_tag = try names.internTagLabel("One"),
            .skip_tag = try names.internTagLabel("Skip"),
            .item_field = try names.internRecordFieldLabel("item"),
            .rest_field = try names.internRecordFieldLabel("rest"),
        };
        var iterator_for_by_node: std.AutoHashMapUnmanaged(CIR.Node.Idx, IteratorForPlanId) = .{};
        errdefer iterator_for_by_node.deinit(allocator);

        var constraint_index = try StaticDispatchConstraintIndex.fromModule(allocator, module, checked_bodies);
        defer constraint_index.deinit(allocator);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const tag = module.nodeTag(@enumFromInt(node_idx));
            if (tag != .expr_dispatch_call and
                tag != .expr_interpolation and
                tag != .expr_type_dispatch_call and
                tag != .expr_method_eq) continue;

            const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
            const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse continue;
            const expr = module.expr(expr_idx);
            const checked_expr_data = checked_bodies.expr(checked_expr).data;
            const idents = module.identStoreConst();
            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            const dispatch_expr_tag = std.meta.stringToEnum(DispatchExprTag, @tagName(std.meta.activeTag(expr.data))) orelse unreachable;
            switch (dispatch_expr_tag) {
                .e_dispatch_call => {
                    const dispatch_call = expr.data.e_dispatch_call;
                    const explicit_args = module.sliceExpr(dispatch_call.args);
                    const args = try allocator.alloc(StaticDispatchOperand, explicit_args.len + 1);
                    defer allocator.free(args);
                    args[0] = .{ .checked_expr = checkedExprIdForSource(checked_bodies, dispatch_call.receiver) };
                    for (explicit_args, 0..) |arg, i| {
                        args[i + 1] = .{ .checked_expr = checkedExprIdForSource(checked_bodies, arg) };
                    }
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, args);

                    try plans.append(allocator, .{
                        .expr = checked_expr,
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
                    const checked_interpolation = checked_expr_data.interpolation;
                    const args = try allocator.alloc(StaticDispatchOperand, 2);
                    defer allocator.free(args);
                    args[0] = .{ .checked_expr = checked_interpolation.first };
                    args[1] = .{ .generated_interpolation_iter = checked_expr };
                    const from_interpolation = try names.internMethodName("from_interpolation");
                    const constraint_fn_var = interpolation.constraint_fn_var orelse unreachable;
                    const dispatcher_var = interpolation.dispatcher_var orelse unreachable;
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, args);

                    try plans.append(allocator, .{
                        .expr = checked_expr,
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
                    });
                },
                .e_type_dispatch_call => {
                    const dispatch_call = expr.data.e_type_dispatch_call;
                    const args = try staticDispatchOperandsForSlice(allocator, checked_bodies, module.sliceExpr(dispatch_call.args));
                    defer allocator.free(args);
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, args);

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, dispatch_call.method_name),
                        .dispatcher = .type_only,
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, typeDispatchOwnerVar(module, dispatch_call.type_dispatch_stmt)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, dispatch_call.constraint_fn_var),
                        .args = ar,
                        .result_mode = try staticDispatchResultModeForCheckedValueCall(allocator, module, checked_types, &constraint_index, dispatch_call.method_name, dispatch_call.constraint_fn_var),
                    });
                    try plan_sources.append(allocator, .{
                        .dispatcher_var = typeDispatchOwnerVar(module, dispatch_call.type_dispatch_stmt),
                        .constraint_fn_var = dispatch_call.constraint_fn_var,
                    });
                },
                .e_method_eq => {
                    const eq = expr.data.e_method_eq;
                    const args = try staticDispatchOperandsForSlice(allocator, checked_bodies, &.{ eq.lhs, eq.rhs });
                    defer allocator.free(args);
                    const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, args);

                    try plans.append(allocator, .{
                        .expr = checked_expr,
                        .method = try names.internMethodIdent(idents, module.commonIdents().is_eq),
                        .dispatcher = .{ .arg = 0 },
                        .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(eq.lhs)),
                        .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, eq.constraint_fn_var),
                        .args = ar,
                        .result_mode = .{ .equality = .{
                            .structural_allowed = true,
                            .negated = eq.negated,
                        } },
                    });
                    try plan_sources.append(allocator, .{
                        .dispatcher_var = module.exprType(eq.lhs),
                        .constraint_fn_var = eq.constraint_fn_var,
                    });
                },
            }
            try by_expr.put(allocator, expr_idx, plan_id);
        }

        const module_env = module.moduleEnvConst();
        for (module_env.generated_codec_derivations.items.items) |derivation| {
            const source_calls = module_env.generated_codec_calls.items.items[derivation.calls_start..][0..derivation.calls_len];
            const calls_start: u32 = @intCast(generated_codec_calls.items.len);
            for (source_calls) |call| {
                try generated_codec_calls.append(allocator, .{
                    .method = try names.internMethodIdent(module.identStoreConst(), @bitCast(call.method_ident)),
                    .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(call.dispatcher_var)),
                    .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(call.callable_var)),
                    .subject_ty = if (call.subject_var == ModuleEnv.GeneratedCodecCall.no_subject_var)
                        null
                    else
                        try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(call.subject_var)),
                });
            }
            try generated_codec_derivations.append(allocator, .{
                .kind = switch (@as(ModuleEnv.GeneratedCodecDerivation.Kind, @enumFromInt(derivation.kind))) {
                    .parser => .parser,
                    .encoder => .encoder,
                },
                .source_constructor_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.source_constraint_fn_var)),
                .source_runtime_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.source_runtime_fn_var)),
                .source_shape_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.source_shape_var)),
                .source_encoding_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.source_encoding_var)),
                .source_state_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.source_state_var)),
                .source_error_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.source_error_var)),
                .constructor_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.constraint_fn_var)),
                .runtime_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.runtime_fn_var)),
                .shape_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.shape_var)),
                .encoding_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.encoding_var)),
                .state_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.state_var)),
                .error_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(derivation.error_var)),
                .calls = .{ .start = calls_start, .len = @intCast(source_calls.len) },
            });
        }
        for (module_env.store.literalDispatchPlans()) |numeral_plan| {
            if (numeral_plan.dispatchKind() != .numeral) continue;
            switch (numeral_plan.dispatchResolution()) {
                .builtin_direct, .checked_error => continue,
                .custom_dispatch, .specialization_dispatch => {},
                .unresolved => if (@import("builtin").mode == .Debug) {
                    std.debug.panic("unresolved numeral dispatch plan reached checked publication", .{});
                } else unreachable,
            }
            const node: CIR.Node.Idx = @enumFromInt(numeral_plan.node_idx);
            const checked_expr = checked_bodies.exprIdAtRawNode(numeral_plan.node_idx) orelse
                checked_bodies.numeralConversionExprAtRawNode(numeral_plan.node_idx) orelse
                continue;
            const checked_expr_data = checked_bodies.expr(checked_expr).data;
            const checked_expr_tag = std.meta.activeTag(checked_expr_data);
            if (checked_expr_tag == .runtime_error) continue;
            if (checked_expr_tag != .numeral) {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch invariant violated: numeral dispatch plan {d} points at a non-numeric checked expression ({s})",
                        .{ numeral_plan.node_idx, @tagName(checked_expr_tag) },
                    );
                }
                unreachable;
            }
            const literal = module_env.numeralLiteralForNode(node) orelse {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch invariant violated: runtime from_numeral plan {d} has no exact literal",
                        .{numeral_plan.node_idx},
                    );
                }
                unreachable;
            };
            if (!literal.isMaterialized()) {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch invariant violated: runtime from_numeral plan {d} has an unmaterialized literal",
                        .{numeral_plan.node_idx},
                    );
                }
                unreachable;
            }
            var args = [_]StaticDispatchOperand{.{ .generated_numeral = literal }};
            const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, &args);

            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            try plans.append(allocator, .{
                .expr = checked_expr,
                .method = try names.internMethodName("from_numeral"),
                .dispatcher = .type_only,
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(numeral_plan.target_var)),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(numeral_plan.fn_var)),
                .args = ar,
                .result_mode = .value,
            });
            try plan_sources.append(allocator, .{
                .dispatcher_var = @enumFromInt(numeral_plan.target_var),
                .constraint_fn_var = @enumFromInt(numeral_plan.fn_var),
            });
            try numeral_by_node.put(allocator, node, plan_id);
        }

        for (module_env.store.literalDispatchPlans()) |quote_plan| {
            if (quote_plan.dispatchKind() != .quote) continue;
            switch (quote_plan.dispatchResolution()) {
                .builtin_direct, .checked_error => continue,
                .custom_dispatch, .specialization_dispatch => {},
                .unresolved => if (@import("builtin").mode == .Debug) {
                    std.debug.panic("unresolved quote dispatch plan reached checked publication", .{});
                } else unreachable,
            }
            const node: CIR.Node.Idx = @enumFromInt(quote_plan.node_idx);
            const checked_expr = checked_bodies.exprIdAtRawNode(quote_plan.node_idx) orelse
                checked_bodies.numeralConversionExprAtRawNode(quote_plan.node_idx) orelse
                continue;
            const checked_expr_data = checked_bodies.expr(checked_expr).data;
            const checked_expr_tag = std.meta.activeTag(checked_expr_data);
            if (checked_expr_tag == .runtime_error) continue;
            if (checked_expr_tag == .str or checked_expr_tag == .str_segment) {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch invariant violated: non-builtin quote target {d} lost its from_quote expression",
                        .{quote_plan.node_idx},
                    );
                }
                unreachable;
            }
            if (checked_expr_tag != .str_from_quote) {
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch invariant violated: quote dispatch plan {d} points at a non-string checked expression ({s})",
                        .{ quote_plan.node_idx, @tagName(checked_expr_tag) },
                    );
                }
                unreachable;
            }
            const literal = checked_expr_data.str_from_quote.literal;
            var args = [_]StaticDispatchOperand{.{ .generated_quote = literal }};
            const ar = try pushOperands(StaticDispatchOperand, &operand_pool, allocator, &args);

            const plan_id: StaticDispatchPlanId = @enumFromInt(@as(u32, @intCast(plans.items.len)));
            try plans.append(allocator, .{
                .expr = checked_expr,
                .method = try names.internMethodName("from_quote"),
                .dispatcher = .type_only,
                .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(quote_plan.target_var)),
                .callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(quote_plan.fn_var)),
                .args = ar,
                .result_mode = .value,
            });
            try plan_sources.append(allocator, .{
                .dispatcher_var = @enumFromInt(quote_plan.target_var),
                .constraint_fn_var = @enumFromInt(quote_plan.fn_var),
            });
            try quote_by_node.put(allocator, node, plan_id);
        }

        for (module_env.for_loop_dispatch_plans.items.items) |for_plan| {
            const for_node_idx: CIR.Node.Idx = @enumFromInt(for_plan.node_idx);
            const pattern_idx: CIR.Pattern.Idx = @enumFromInt(for_plan.pattern_idx);
            const iterable_idx: CIR.Expr.Idx = @enumFromInt(for_plan.iterable_idx);

            if (checked_bodies.exprIdForSource(iterable_idx) == null) continue;
            const for_node_tag = module.nodeTag(for_node_idx);
            const for_has_checked_node = if (for_node_tag == .expr_for)
                checked_bodies.exprIdForSource(@enumFromInt(for_plan.node_idx)) != null
            else if (for_node_tag == .statement_for)
                checked_bodies.statementIdForSource(@enumFromInt(for_plan.node_idx)) != null
            else
                false;
            if (!for_has_checked_node) continue;

            const iterable_expr = checkedExprIdForSource(checked_bodies, iterable_idx);
            const item_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.patternType(pattern_idx));
            const iter_callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(for_plan.iter_fn_var));
            const next_callable_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(for_plan.next_fn_var));
            const iterator_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(for_plan.iterator_var));
            const step_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(for_plan.step_var));
            const step_topology = IteratorStepTopology{
                .done_tag = try names.internTagIdent(module.identStoreConst(), @bitCast(for_plan.step_topology.done_tag_ident)),
                .one_tag = try names.internTagIdent(module.identStoreConst(), @bitCast(for_plan.step_topology.one_tag_ident)),
                .skip_tag = try names.internTagIdent(module.identStoreConst(), @bitCast(for_plan.step_topology.skip_tag_ident)),
                .item_field = try names.internRecordFieldIdent(module.identStoreConst(), @bitCast(for_plan.step_topology.item_field_ident)),
                .rest_field = try names.internRecordFieldIdent(module.identStoreConst(), @bitCast(for_plan.step_topology.rest_field_ident)),
                .one_payload_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(for_plan.step_topology.one_payload_var)),
                .skip_payload_ty = try checkedTypeIdForVar(allocator, module, checked_types, @enumFromInt(for_plan.step_topology.skip_payload_var)),
            };

            const iterator_for_id: IteratorForPlanId = @enumFromInt(@as(u32, @intCast(iterator_for_plans.items.len)));
            {
                var iter_args = [_]IteratorDispatchOperand{.{ .checked_expr = iterable_expr }};
                const iter_ar = try pushOperands(IteratorDispatchOperand, &iter_operand_pool, allocator, &iter_args);

                var next_args = [_]IteratorDispatchOperand{.loop_iterator_state};
                const next_ar = try pushOperands(IteratorDispatchOperand, &iter_operand_pool, allocator, &next_args);

                const iter_call = IteratorDispatchCall{
                    .method = try names.internMethodName("iter"),
                    .dispatcher_ty = try checkedTypeIdForVar(allocator, module, checked_types, module.exprType(iterable_idx)),
                    .callable_ty = iter_callable_ty,
                    .dispatcher_arg_index = 0,
                    .args = iter_ar,
                };
                const next_call = IteratorDispatchCall{
                    .method = try names.internMethodName("next"),
                    .dispatcher_ty = iterator_ty,
                    .callable_ty = next_callable_ty,
                    .dispatcher_arg_index = 0,
                    .args = next_ar,
                };

                try iterator_for_plans.append(allocator, .{
                    .iter = iter_call,
                    .next = next_call,
                    .iterable = iterable_expr,
                    .item_ty = item_ty,
                    .iterator_ty = iterator_ty,
                    .step_ty = step_ty,
                    .step_topology = step_topology,
                });
                try iterator_plan_sources.append(allocator, .{
                    .iter_dispatcher_var = module.exprType(iterable_idx),
                    .next_dispatcher_var = @enumFromInt(for_plan.iterator_var),
                    .iter_fn_var = @enumFromInt(for_plan.iter_fn_var),
                    .next_fn_var = @enumFromInt(for_plan.next_fn_var),
                });
            }
            try iterator_for_by_node.put(allocator, for_node_idx, iterator_for_id);
        }

        // Convert the construction-time hashmaps into sorted, relocatable
        // PlanKV slices (transform D), then release the maps.
        const by_expr_sorted = try sortedFromMap(allocator, by_expr);
        errdefer allocator.free(by_expr_sorted);
        const numeral_sorted = try sortedFromMap(allocator, numeral_by_node);
        errdefer allocator.free(numeral_sorted);
        const quote_sorted = try sortedFromMap(allocator, quote_by_node);
        errdefer allocator.free(quote_sorted);
        const iterator_for_sorted = try sortedFromMap(allocator, iterator_for_by_node);
        errdefer allocator.free(iterator_for_sorted);
        by_expr.deinit(allocator);
        numeral_by_node.deinit(allocator);
        quote_by_node.deinit(allocator);
        iterator_for_by_node.deinit(allocator);

        build_data.* = .{
            .plan_sources = try plan_sources.toOwnedSlice(allocator),
            .iterator_plan_sources = try iterator_plan_sources.toOwnedSlice(allocator),
        };

        return .{
            .plans = try plans.toOwnedSlice(allocator),
            .by_expr = by_expr_sorted,
            .numeral_by_node = numeral_sorted,
            .quote_by_node = quote_sorted,
            .iterator_for_plans = try iterator_for_plans.toOwnedSlice(allocator),
            .iterator_topologies = iterator_topologies,
            .iterator_for_by_node = iterator_for_sorted,
            .operand_pool = try operand_pool.toOwnedSlice(allocator),
            .iter_operand_pool = try iter_operand_pool.toOwnedSlice(allocator),
            .generated_codec_derivations = try generated_codec_derivations.toOwnedSlice(allocator),
            .generated_codec_calls = try generated_codec_calls.toOwnedSlice(allocator),
        };
    }

    pub fn lookupByExpr(self: *const StaticDispatchPlanTable, expr: CIR.Expr.Idx) ?StaticDispatchPlanId {
        return if (lookupPlanKV(self.by_expr, @intFromEnum(expr))) |v| @enumFromInt(v) else null;
    }

    pub fn lookupNumeralByNode(self: *const StaticDispatchPlanTable, node: CIR.Node.Idx) ?StaticDispatchPlanId {
        return if (lookupPlanKV(self.numeral_by_node, @intFromEnum(node))) |v| @enumFromInt(v) else null;
    }

    pub fn lookupQuoteByNode(self: *const StaticDispatchPlanTable, node: CIR.Node.Idx) ?StaticDispatchPlanId {
        return if (lookupPlanKV(self.quote_by_node, @intFromEnum(node))) |v| @enumFromInt(v) else null;
    }

    pub fn lookupIteratorForByNode(self: *const StaticDispatchPlanTable, node: CIR.Node.Idx) ?IteratorForPlanId {
        return if (lookupPlanKV(self.iterator_for_by_node, @intFromEnum(node))) |v| @enumFromInt(v) else null;
    }

    pub fn evidenceNode(self: *const StaticDispatchPlanTable, id: EvidenceNodeId) EvidenceNode {
        return self.evidence_nodes[@intFromEnum(id)];
    }

    /// The evidence node's nested evidence, in the target scheme's canonical
    /// evidence-param order.
    pub fn nestedEvidence(self: *const StaticDispatchPlanTable, node: EvidenceNode) []const CheckedEvidence {
        const span = switch (node.nested) {
            .resolved => |resolved| resolved,
            .from_callable => {
                if (builtin_config.mode == .Debug) {
                    std.debug.panic("callable-derived target evidence has no resolved checked-evidence span", .{});
                }
                unreachable;
            },
        };
        return self.evidence_refs[span.start .. span.start + span.len];
    }

    pub fn generatedCodecCallEvidence(self: *const StaticDispatchPlanTable, call: GeneratedCodecCall) []const CheckedEvidence {
        return self.evidence_refs[call.nested.start .. call.nested.start + call.nested.len];
    }

    /// Evidence for the scheme instantiated at `expr` (a constrained
    /// definition reference or an expression-position function construction
    /// edge), in the scheme's canonical evidence-param order; null when no
    /// checked instantiation edge was recorded for the expression.
    pub fn siteEvidence(self: *const StaticDispatchPlanTable, expr: CheckedExprId) ?[]const CheckedEvidence {
        const found = self.siteEvidenceSpan(expr) orelse return null;
        return self.evidence_refs[found.start .. found.start + found.len];
    }

    /// Exact durable range for a checker-recorded instantiation edge.
    pub fn siteEvidenceSpan(self: *const StaticDispatchPlanTable, expr: CheckedExprId) ?artifact_serialize.Span {
        const found = artifact_serialize.binarySearchByKey(SiteEvidenceEntry, u32, self.site_evidence, @intFromEnum(expr), siteEvidenceOrder) orelse return null;
        return .{ .start = found.start, .len = found.len };
    }

    /// Build-time-only teardown: frees the heap-owned slices. A frozen
    /// (deserialized) table's slices alias the artifact's single backing buffer and are
    /// NEVER freed here—the artifact's `deinitInternal` frees the buffer wholesale and
    /// does not call any sub-store `deinit` on the frozen path. (No `serialized` flag is
    /// needed because, unlike the mutation-guarded stores, this table has no post-load
    /// mutators.)
    pub fn deinit(self: *StaticDispatchPlanTable, allocator: Allocator) void {
        allocator.free(self.template_refs);
        allocator.free(self.direct_template_refs);
        allocator.free(self.dispatch_relation_refs);
        allocator.free(self.by_expr);
        allocator.free(self.numeral_by_node);
        allocator.free(self.quote_by_node);
        allocator.free(self.iterator_for_by_node);
        allocator.free(self.plans);
        allocator.free(self.iterator_for_plans);
        allocator.free(self.iterator_topologies);
        allocator.free(@constCast(self.operand_pool));
        allocator.free(@constCast(self.iter_operand_pool));
        allocator.free(self.evidence_nodes);
        allocator.free(self.evidence_refs);
        allocator.free(self.site_evidence);
        allocator.free(self.generated_codec_derivations);
        allocator.free(self.generated_codec_calls);
        self.* = .{};
    }
};

fn siteEvidenceOrder(e: SiteEvidenceEntry, key: u32) std.math.Order {
    return std.math.order(e.key, key);
}

/// Build-time-only side data recorded by `StaticDispatchPlanTable.fromModule`
/// so the total-resolution pass (`dispatch_evidence.zig`) can resolve each
/// plan from the checker type store: the source dispatcher var and the source
/// constraint fn var (the discharge-record key). Parallel to `plans`; never
/// serialized.
pub const PlanSource = struct {
    dispatcher_var: Var,
    constraint_fn_var: ?Var,
};

/// Build-time-only side data for iterator plans, parallel to
/// `iterator_for_plans`.
pub const IteratorPlanSource = struct {
    iter_dispatcher_var: Var,
    next_dispatcher_var: Var,
    iter_fn_var: Var,
    next_fn_var: Var,
};

/// Build-time-only outputs of `StaticDispatchPlanTable.fromModule` consumed by
/// the total-resolution pass.
pub const PlanTableBuildData = struct {
    plan_sources: []PlanSource = &.{},
    iterator_plan_sources: []IteratorPlanSource = &.{},

    pub fn deinit(self: *PlanTableBuildData, allocator: Allocator) void {
        allocator.free(self.plan_sources);
        allocator.free(self.iterator_plan_sources);
        self.* = .{};
    }
};

const StaticDispatchConstraintIndex = struct {
    constraints: []const types.StaticDispatchConstraint = &.{},
    by_fn_var: std.AutoHashMapUnmanaged(Var, u32) = .{},

    fn fromModule(allocator: Allocator, module: TypedCIR.Module, checked_bodies: anytype) Allocator.Error!StaticDispatchConstraintIndex {
        const store = module.typeStoreConst();
        var live_fn_vars: std.AutoHashMapUnmanaged(Var, void) = .{};
        defer live_fn_vars.deinit(allocator);

        var node_idx: u32 = 0;
        while (node_idx < module.nodeCount()) : (node_idx += 1) {
            const expr_idx: CIR.Expr.Idx = @enumFromInt(node_idx);
            const node_tag = module.nodeTag(@enumFromInt(node_idx));
            const constraint_fn_var: ?Var = if (node_tag == .expr_dispatch_call)
                module.expr(expr_idx).data.e_dispatch_call.constraint_fn_var
            else if (node_tag == .expr_interpolation)
                module.expr(expr_idx).data.e_interpolation.constraint_fn_var
            else if (node_tag == .expr_type_dispatch_call)
                module.expr(expr_idx).data.e_type_dispatch_call.constraint_fn_var
            else if (node_tag == .expr_method_eq)
                module.expr(expr_idx).data.e_method_eq.constraint_fn_var
            else
                null;
            if (constraint_fn_var) |fn_var| {
                const checked_expr = checked_bodies.exprIdForSource(expr_idx) orelse continue;
                if (module.nodeTag(@enumFromInt(node_idx)) == .expr_interpolation and
                    std.meta.activeTag(checked_bodies.expr(checked_expr).data) != .interpolation) continue;
                try live_fn_vars.put(allocator, fn_var, {});
            }
        }

        var index = StaticDispatchConstraintIndex{
            .constraints = store.static_dispatch_constraints.items.items,
        };
        errdefer index.deinit(allocator);

        try index.by_fn_var.ensureTotalCapacity(allocator, @intCast(live_fn_vars.count()));
        for (index.constraints, 0..) |constraint, i| {
            if (!live_fn_vars.contains(constraint.fn_var)) continue;
            const entry = try index.by_fn_var.getOrPut(allocator, constraint.fn_var);
            if (entry.found_existing) {
                const existing = index.constraints[entry.value_ptr.*];
                if (staticDispatchConstraintsEquivalent(existing, constraint)) continue;
                if (@import("builtin").mode == .Debug) {
                    std.debug.panic(
                        "checked static dispatch constraint invariant violated: duplicate fn_var {d}; existing idx={d} name={s} origin={s} negated={} new idx={d} name={s} origin={s} negated={}",
                        .{
                            @intFromEnum(constraint.fn_var),
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

fn staticDispatchConstraintsEquivalent(a: types.StaticDispatchConstraint, b: types.StaticDispatchConstraint) bool {
    // origin now carries the binop-negation and literal payloads, so structural
    // equality of origin subsumes the former separate field comparisons.
    return a.fn_name == b.fn_name and
        a.fn_var == b.fn_var and
        std.meta.eql(a.origin, b.origin);
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
    const raw = @intFromEnum(ty);
    if (raw >= checked_types.store.payloadCount()) {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: equality return type root was outside the checked type store", .{});
        }
        unreachable;
    }
    const payload = checked_types.store.payload(ty);
    if (std.meta.activeTag(payload) != .nominal) return false;
    const builtin_owner = payload.nominal.builtin orelse return false;
    return builtin_owner == .bool;
}

/// Public `methodOwnerForCheckedType` declaration: the method owner of a
/// published checked type, walking alias chains transparently.
pub fn methodOwnerForCheckedType(checked_types: anytype, ty: CheckedTypeId) ?MethodOwner {
    var current = ty;
    // Aliases are transparent for static dispatch: an alias's method owner is its
    // backing's owner. Walk the (finite) alias chain so an alias-over-nominal,
    // alias-over-alias, or alias-over-builtin resolves to the underlying owner
    // rather than the alias's own identity, where no methods are registered. The
    // bound on iterations is the store size, so a cyclic chain cannot loop here.
    var remaining = checked_types.store.payloads.items.len;
    while (true) {
        const raw = @intFromEnum(current);
        if (raw >= checked_types.store.payloads.items.len) {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("checked static dispatch invariant violated: dispatcher type root was outside the checked type store", .{});
            }
            unreachable;
        }
        const payload = checked_types.store.payloads.items[raw];
        if (std.meta.activeTag(payload) != .alias) return methodOwnerForCheckedPayload(payload);
        if (remaining == 0) {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("checked static dispatch invariant violated: checked type alias chain was cyclic", .{});
            }
            unreachable;
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
        .type_name = nominal.name,
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
        .try_ => unreachable,
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
    names: *canonical.CanonicalNameStore,
    local_method_registry: *const MethodRegistry,
    imported_views: anytype,
    owner: MethodOwner,
    method: canonical.MethodNameId,
) ?CheckedMethodLookup {
    if (local_method_registry.lookup(.{ .owner = owner, .method = method })) |found| return found;

    const method_name = names.methodNameText(method);
    for (imported_views) |imported| {
        const imported_owner = methodOwnerInImportedStore(names, imported.canonical_names, owner) orelse continue;
        const imported_method = imported.canonical_names.lookupMethodName(method_name) orelse continue;
        if (imported.method_registry.lookup(.{ .owner = imported_owner, .method = imported_method })) |found| {
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

/// Rebase a method owner into an imported artifact's store: the module
/// component crosses by 32-byte content identity (one map probe, full-value
/// comparison), the type-name component by declared-name interning. This is
/// the single cross-artifact owner resolution point—no module name text.
pub fn methodOwnerInImportedStore(
    source_names: *const canonical.CanonicalNameStore,
    imported_names: *const canonical.CanonicalNameStore,
    owner: MethodOwner,
) ?MethodOwner {
    return switch (owner) {
        .builtin => |builtin| .{ .builtin = builtin },
        .nominal => |nominal| .{ .nominal = .{
            .module = imported_names.lookupModuleIdentity(source_names.moduleIdentityBytes(nominal.module)) orelse return null,
            .type_name = imported_names.lookupTypeName(source_names.typeNameText(nominal.type_name)) orelse return null,
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
        if (@import("builtin").mode == .Debug) {
            std.debug.panic("checked static dispatch invariant violated: dispatch type root was not published", .{});
        }
        unreachable;
    };
}

fn staticDispatchOperandsForSlice(
    allocator: Allocator,
    checked_bodies: anytype,
    exprs: []const CIR.Expr.Idx,
) Allocator.Error![]const StaticDispatchOperand {
    if (exprs.len == 0) return &.{};
    const out = try allocator.alloc(StaticDispatchOperand, exprs.len);
    errdefer allocator.free(out);
    for (exprs, 0..) |expr, i| {
        out[i] = .{ .checked_expr = checkedExprIdForSource(checked_bodies, expr) };
    }
    return out;
}

fn checkedExprIdForSource(checked_bodies: anytype, expr: CIR.Expr.Idx) CheckedExprId {
    return checked_bodies.exprIdForSource(expr) orelse {
        if (@import("builtin").mode == .Debug) {
            std.debug.panic(
                "checked static dispatch invariant violated: dispatch expression {d} has no checked expression id",
                .{@intFromEnum(expr)},
            );
        }
        unreachable;
    };
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
        .key = .{ .owner = .{ .builtin = .box }, .method = @enumFromInt(2) },
        .target = testMethodTarget(@enumFromInt(20)),
    };
    entries[0].target.?.kind = .{ .structural = .equality };
    entries[1] = .{
        .key = .{ .owner = .{ .builtin = .list }, .method = @enumFromInt(1) },
        .target = testMethodTarget(@enumFromInt(10)),
    };
    entries[2] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @enumFromInt(1) },
        .target = testMethodTarget(@enumFromInt(15)),
    };

    finalizeMethodRegistryEntries(entries);

    var registry = MethodRegistry{ .entries = entries };
    const found = registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @enumFromInt(1) }) orelse return error.MissingSortedMethodTarget;
    try std.testing.expectEqual(@as(CIR.Def.Idx, @enumFromInt(15)), found.target.def_idx);
    const structural = registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @enumFromInt(2) }) orelse return error.MissingStructuralMethodTarget;
    try std.testing.expectEqual(StructuralKind.equality, structural.target.kind.structural);
    try std.testing.expect(registry.lookup(.{ .owner = .{ .builtin = .list }, .method = @enumFromInt(2) }) == null);
}

test "method registry distinguishes a rejected declaration from an undeclared method" {
    const allocator = std.testing.allocator;

    const entries = try allocator.alloc(MethodRegistryEntry, 2);
    defer allocator.free(entries);

    entries[0] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @enumFromInt(1) },
        .target = testMethodTarget(@enumFromInt(10)),
    };
    entries[1] = .{
        .key = .{ .owner = .{ .builtin = .box }, .method = @enumFromInt(2) },
        .target = null,
    };

    finalizeMethodRegistryEntries(entries);

    var registry = MethodRegistry{ .entries = entries };
    const declared = registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @enumFromInt(1) }) orelse
        return error.MissingDeclaredMethodTarget;
    try std.testing.expectEqual(@as(CIR.Def.Idx, @enumFromInt(10)), declared.target.def_idx);

    const rejected = registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @enumFromInt(2) }) orelse
        return error.MissingRejectedMethodEntry;
    try std.testing.expect(rejected == .rejected);

    try std.testing.expect(registry.lookup(.{ .owner = .{ .builtin = .box }, .method = @enumFromInt(3) }) == null);
}

/// Convert an intentional fixture-table position while preserving enum inference.
fn fixtureTableIndex(comptime index: u32) u32 {
    return index;
}

fn testPlan(expr_raw: u32, args_start: u32, args_len: u32) StaticDispatchCallPlan {
    return .{
        .expr = @enumFromInt(expr_raw),
        .method = @enumFromInt(1),
        .dispatcher = .{ .arg = 0 },
        .dispatcher_ty = @enumFromInt(2),
        .callable_ty = @enumFromInt(3),
        .args = .{ .start = args_start, .len = args_len },
        .result_mode = .value,
    };
}

test "StaticDispatchPlanTable: relocates with a constant number of fixups, operands resolve post-deserialize" {
    const gpa = std.testing.allocator;

    // The fixup count is fixed by the number of serialized base pointers, never
    // by how much data each pool holds. The two tables below differ in operand
    // count by three orders of magnitude yet relocate identically.
    comptime std.debug.assert(@typeInfo(StaticDispatchPlanTable.Serialized).@"struct".fields.len == 17);

    inline for (.{ @as(u32, 4), @as(u32, 4000) }) |operand_count| {
        const operands = try gpa.alloc(StaticDispatchOperand, operand_count);
        defer gpa.free(operands);
        for (operands, 0..) |*op, i| op.* = .{ .checked_expr = @enumFromInt(@as(u32, @intCast(i)) + 100) };

        var plans = [_]StaticDispatchCallPlan{
            testPlan(10, 0, 2),
            testPlan(11, 2, operand_count - 2),
        };
        var by_expr = [_]PlanKV{
            .{ .key = 10, .val = 0 },
            .{ .key = 11, .val = 1 },
        };
        var evidence_nodes = [_]EvidenceNode{.{
            .target = .{
                .module_idx = 7,
                .def_idx = @enumFromInt(8),
                .kind = .{ .local_proc = .{
                    .binder = @enumFromInt(9),
                    .expr = @enumFromInt(10),
                    .context_anchor = @enumFromInt(12),
                } },
                .callable_ty = @enumFromInt(11),
            },
            .dispatcher_ty = @enumFromInt(12),
            .instantiation = .{ .callable = @enumFromInt(13) },
            .nested = .{ .resolved = .{ .start = 0, .len = 1 } },
        }};
        var evidence_refs = [_]CheckedEvidence{
            .{ .dispatcher_ty = @enumFromInt(14), .runtime_dictionary = true, .resolution = .{ .structural = .{
                .derivation = .encoder,
                .dispatcher_ty = @enumFromInt(14),
                .callable_ty = @enumFromInt(15),
            } } },
            .{ .dispatcher_ty = @enumFromInt(12), .runtime_dictionary = false, .resolution = .{ .direct = @enumFromInt(fixtureTableIndex(0)) } },
        };
        var site_evidence = [_]SiteEvidenceEntry{.{
            .key = 16,
            .start = 1,
            .len = 1,
        }};
        var iterator_topologies = [_]IteratorRepresentationTopology{.{
            .len_field = @enumFromInt(20),
            .step_field = @enumFromInt(21),
            .known_tag = @enumFromInt(22),
            .unknown_tag = @enumFromInt(23),
            .done_tag = @enumFromInt(24),
            .one_tag = @enumFromInt(25),
            .skip_tag = @enumFromInt(26),
            .item_field = @enumFromInt(27),
            .rest_field = @enumFromInt(28),
        }};

        const table = StaticDispatchPlanTable{
            .plans = &plans,
            .by_expr = &by_expr,
            .iterator_topologies = &iterator_topologies,
            .operand_pool = operands,
            .evidence_nodes = &evidence_nodes,
            .evidence_refs = &evidence_refs,
            .site_evidence = &site_evidence,
        };

        const rt = try artifact_serialize.roundTripForTest(gpa, StaticDispatchPlanTable, &table);
        defer gpa.free(rt.buffer);

        const loaded = rt.loaded;
        try std.testing.expectEqual(@as(usize, 2), loaded.plans.len);
        try std.testing.expectEqual(@as(usize, operand_count), loaded.operand_pool.len);
        try std.testing.expectEqualSlices(IteratorRepresentationTopology, &iterator_topologies, loaded.iterator_topologies);

        const first_args = loaded.plans[0].argsSlice(&loaded);
        try std.testing.expectEqual(@as(usize, 2), first_args.len);
        try std.testing.expectEqual(@as(CheckedExprId, @enumFromInt(100)), first_args[0].checked_expr);

        const second_args = loaded.plans[1].argsSlice(&loaded);
        try std.testing.expectEqual(@as(usize, operand_count - 2), second_args.len);
        try std.testing.expectEqual(
            @as(CheckedExprId, @enumFromInt(operand_count - 1 + 100)),
            second_args[second_args.len - 1].checked_expr,
        );

        try std.testing.expectEqual(@as(?u32, 1), lookupPlanKV(loaded.by_expr, 11));

        const node = loaded.evidenceNode(@enumFromInt(fixtureTableIndex(0)));
        try std.testing.expectEqual(@as(?CheckedTypeId, @enumFromInt(12)), node.dispatcher_ty);
        try std.testing.expectEqual(@as(CheckedTypeId, @enumFromInt(13)), node.instantiation.callable);
        const nested = loaded.nestedEvidence(node);
        try std.testing.expectEqual(@as(usize, 1), nested.len);
        try std.testing.expect(nested[0].runtime_dictionary);
        try std.testing.expectEqual(StructuralKind.encoder, nested[0].resolution.structural.derivation.kind());
        try std.testing.expectEqual(@as(CheckedTypeId, @enumFromInt(14)), nested[0].resolution.structural.dispatcher_ty);
        try std.testing.expectEqual(@as(CheckedTypeId, @enumFromInt(15)), nested[0].resolution.structural.callable_ty);
        const site = loaded.siteEvidence(@enumFromInt(16)) orelse return error.TestExpectedEqual;
        try std.testing.expectEqual(@as(usize, 1), site.len);
        try std.testing.expect(!site[0].runtime_dictionary);
        try std.testing.expectEqual(@as(EvidenceNodeId, @enumFromInt(fixtureTableIndex(0))), site[0].resolution.direct);
    }
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
