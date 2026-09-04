//! Cross-module type copying for imports.
//!
//! This module provides functionality to copy types from one module's type store
//! to another module's type store when importing. This ensures each module maintains
//! its own consistent type variable namespace while still being able to use types
//! from other modules.
//!
//! Copying a type across module envs is an identity REBASE boundary: nominal and
//! alias `origin_module` values are env-local indices into the source env's
//! module identity table, so the copy reads the 32-byte content identity hash
//! from the source table and getOrInserts it into the destination table. This is
//! the single cross-env identity resolution mechanism—no name matching.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");
const types_mod = @import("types");
const literal_defaulting = types_mod.literal_defaulting;

const ModuleEnv = can.ModuleEnv;
const TypesStore = types_mod.Store;
const Var = types_mod.Var;
const Flex = types_mod.Flex;
const Rigid = types_mod.Rigid;
const StaticDispatchConstraint = types_mod.StaticDispatchConstraint;
const InterpolationPartMetadata = types_mod.InterpolationPartMetadata;
const WhereMethodMarkerContract = types_mod.WhereMethodMarkerContract;
const WhereMethodMarkerPathStep = types_mod.WhereMethodMarkerPathStep;
const WhereMethodMarkerBasis = types_mod.WhereMethodMarkerBasis;
const Content = types_mod.Content;
const FlatType = types_mod.FlatType;
const Alias = types_mod.Alias;
const Func = types_mod.Func;
const Record = types_mod.Record;
const TagUnion = types_mod.TagUnion;
const RecordField = types_mod.RecordField;
const Tag = types_mod.Tag;
const NominalType = types_mod.NominalType;

/// A mapping from source type variables to destination type variables.
/// Callers may preseed exact source substitutions; copying reuses those
/// destination roots and memoizes every newly copied root in the same map.
const VarMapping = std.AutoHashMap(Var, Var);

pub const BindingCodecMappingOrigin = struct {
    destination: Var,
    copy_step: u32,
    pair_offset: u32,
};

/// Producer-side provenance parallel to the shared variable map while one
/// imported binding's codec requirements are copied. Durable witnesses copy
/// these coordinates; this transient index is never admission authority.
pub const BindingCodecMappingOrigins = std.AutoHashMap(Var, BindingCodecMappingOrigin);

/// Explicit source declaration identity for alias substitutions performed
/// while copying a type graph between module stores.
pub const AliasSource = struct {
    origin_module: base.ModuleIdentity.Idx,
    source_decl: u32,
};

/// Finite, caller-authored origin for a marker-bearing cross-module copy.
/// `copyVarWithMarkerLineage` fills the common root and pair fields directly
/// from the traversal; callers can supply only one of ModuleEnv's explicitly
/// decoded origin variants.
/// One exact platform for-clause alias substitution that a platform
/// requirement copy may exercise. The copy traversal, not the caller,
/// decides which supplied bindings become durable rows.
pub const PlatformAliasSubstitutionInput = struct {
    platform_alias_var: Var,
    platform_identity_var: Var,
    platform_alias_source: AliasSource,
    platform_alias_statement: u32,
    app_declaration_node: u32,
    app_instantiation_step: u32,
};

pub const PlatformRequirementCopyOrigin = struct {
    requires_index: u32,
    solution_def: u32,
    substitutions: []const PlatformAliasSubstitutionInput,
};

pub const CrossModuleWhereMarkerCopyOrigin = union(enum) {
    external_cir_node: ModuleEnv.WhereMarkerNodeOrigin,
    external_numeric_suffix: ModuleEnv.WhereMarkerNodeOrigin,
    external_where_alias_receiver: ModuleEnv.WhereMarkerNodeOrigin,
    external_where_alias_parameter: ModuleEnv.WhereMarkerWhereAliasParameterOrigin,
    external_cache_seed: ModuleEnv.WhereMarkerExternalCacheSeedOrigin,
    binding_codec_receiver: ModuleEnv.WhereMarkerBindingCodecOrigin,
    binding_codec_function: ModuleEnv.WhereMarkerBindingCodecOrigin,
    selected_dispatch_method: ModuleEnv.WhereMarkerSelectedMethodOrigin,
    generated_codec_method: ModuleEnv.WhereMarkerGeneratedCodecMethodOrigin,
    inspect_method: ModuleEnv.WhereMarkerInspectMethodOrigin,
    associated_method: ModuleEnv.WhereMarkerAssociatedMethodOrigin,
    default_method: ModuleEnv.WhereMarkerDefaultMethodOrigin,
    candidate_probe_method_root: ModuleEnv.WhereMarkerCandidateProbeMethodOrigin,
    platform_requirement: PlatformRequirementCopyOrigin,

    const Encoded = struct {
        kind: ModuleEnv.WhereMarkerCopyStep.Kind,
        origin: ModuleEnv.WhereMarkerCopyOrigin,
    };

    fn encode(
        self: @This(),
        platform_substitutions_start: u32,
        platform_substitutions_len: u32,
    ) Encoded {
        return switch (self) {
            .platform_requirement => |payload| .{
                .kind = .platform_requirement,
                .origin = @unionInit(
                    ModuleEnv.WhereMarkerCopyOrigin,
                    "platform_requirement",
                    ModuleEnv.WhereMarkerPlatformRequirementOrigin{
                        .requires_index = payload.requires_index,
                        .solution_def = payload.solution_def,
                        .substitutions_start = platform_substitutions_start,
                        .substitutions_len = platform_substitutions_len,
                    },
                ),
            },
            inline else => |payload, tag| .{
                .kind = @field(ModuleEnv.WhereMarkerCopyStep.Kind, @tagName(tag)),
                .origin = @unionInit(ModuleEnv.WhereMarkerCopyOrigin, @tagName(tag), payload),
            },
        };
    }
};

const AliasSourceMapping = std.AutoHashMap(AliasSource, Var);

const PendingImportedMarkerContract = struct {
    contract: WhereMethodMarkerContract,
    source_contract_offset: u32,
};

/// Producer-time edge proof. Endpoints remain exact raw occurrences until the
/// step's complete relation is canonicalized; publication then binds them to
/// owner-relative pair offsets. Auxiliary indexes similarly name producer
/// inputs and are remapped to the step-owned canonical pool at commit.
const PendingCopyWitness = struct {
    parent_raw_source_var: u32,
    parent_raw_destination_var: u32,
    child_raw_source_var: u32,
    child_raw_destination_var: u32,
    edge_kind: ModuleEnv.WhereMarkerCopyWitness.EdgeKind,
    edge_index: u32 = 0,
    edge_name: u32 = 0,
    edge_origin_module: u32 = 0,
    edge_source_decl: u32 = 0,
    source_constraint_index: u32 = std.math.maxInt(u32),
    action: ModuleEnv.WhereMarkerCopyWitness.Action = .traverse,
    auxiliary_origin_kind: ModuleEnv.WhereMarkerCopyWitness.AuxiliaryOriginKind = .none,
    auxiliary_origin_step: u32 = 0,
    auxiliary_origin_index: u32 = 0,
    raw_source_var: u32 = std.math.maxInt(u32),
    raw_destination_var: u32 = std.math.maxInt(u32),
};

/// Reusable heap buffers backing the copy's explicit worklist. A copy drains
/// every buffer back to its entry length, so nesting a nominal-declaration
/// copy inside a graph copy is safe.
const CopyScratch = struct {
    frames: std.ArrayList(Frame) = .empty,
    values: std.ArrayList(Var) = .empty,
    pending_fields: std.ArrayList(RecordField) = .empty,
    pending_tags: std.ArrayList(Tag) = .empty,
    pending_constraints: std.ArrayList(StaticDispatchConstraint) = .empty,
    pending_marker_contracts: std.ArrayList(PendingImportedMarkerContract) = .empty,
    pending_marker_steps: std.ArrayList(WhereMethodMarkerPathStep) = .empty,
    proof_pairs: std.ArrayList(ModuleEnv.WhereMarkerCopyPair) = .empty,
    proof_occurrences: std.ArrayList(ModuleEnv.WhereMarkerCopyOccurrence) = .empty,
    proof_constraint_pairs: std.ArrayList(ModuleEnv.WhereMarkerConstraintCopyPair) = .empty,
    proof_witnesses: std.ArrayList(PendingCopyWitness) = .empty,
    exercised_platform_substitutions: std.ArrayList(u32) = .empty,
    pending_parts: std.ArrayList(InterpolationPartMetadata) = .empty,

    fn deinit(self: *CopyScratch, allocator: std.mem.Allocator) void {
        self.pending_parts.deinit(allocator);
        self.exercised_platform_substitutions.deinit(allocator);
        self.proof_witnesses.deinit(allocator);
        self.proof_constraint_pairs.deinit(allocator);
        self.proof_occurrences.deinit(allocator);
        self.proof_pairs.deinit(allocator);
        self.pending_marker_steps.deinit(allocator);
        self.pending_marker_contracts.deinit(allocator);
        self.pending_constraints.deinit(allocator);
        self.pending_tags.deinit(allocator);
        self.pending_fields.deinit(allocator);
        self.values.deinit(allocator);
        self.frames.deinit(allocator);
    }
};

/// One complete cross-module copy transaction. The type-store savepoint owns
/// every destination graph/list mutation, the ModuleEnv mark owns proof and
/// interner publication, and `mapping_keys` owns precisely the absent
/// caller-map keys this transaction inserts. The
/// boundary closes before returning to any caller that may perform semantic
/// unification.
const CrossModuleCopyTransaction = struct {
    dest_store: *TypesStore,
    dest_env: *ModuleEnv,
    var_mapping: *VarMapping,
    allocator: std.mem.Allocator,
    store_savepoint: TypesStore.Savepoint,
    env_mark: ModuleEnv.CrossModuleCopyMark,
    mapping_keys: std.ArrayList(Var) = .empty,
    binding_mapping_origins: ?*BindingCodecMappingOrigins = null,
    binding_origin_keys: std.ArrayList(Var) = .empty,
    active: bool = true,

    fn canBegin() bool {
        return active_cross_module_copy_transaction == null;
    }

    fn begin(
        self: *CrossModuleCopyTransaction,
        dest_store: *TypesStore,
        dest_env: *ModuleEnv,
        var_mapping: *VarMapping,
        allocator: std.mem.Allocator,
    ) (std.mem.Allocator.Error || error{NestedPublicCopy})!void {
        if (!canBegin()) {
            return error.NestedPublicCopy;
        }
        var store_savepoint = try dest_store.createSavepoint();
        errdefer dest_store.rollbackToSavepoint(&store_savepoint);
        var env_mark = try dest_env.beginCrossModuleCopyMark();
        errdefer dest_env.rollbackCrossModuleCopyMark(&env_mark);

        self.* = .{
            .dest_store = dest_store,
            .dest_env = dest_env,
            .var_mapping = var_mapping,
            .allocator = allocator,
            .store_savepoint = store_savepoint,
            .env_mark = env_mark,
        };
        active_cross_module_copy_transaction = self;
    }

    fn beginPublic(
        self: *CrossModuleCopyTransaction,
        dest_store: *TypesStore,
        dest_env: *ModuleEnv,
        var_mapping: *VarMapping,
        allocator: std.mem.Allocator,
    ) std.mem.Allocator.Error!void {
        self.begin(dest_store, dest_env, var_mapping, allocator) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.NestedPublicCopy => std.debug.panic(
                "public cross-module copy transactions may not nest",
                .{},
            ),
        };
    }

    fn commit(self: *CrossModuleCopyTransaction) void {
        std.debug.assert(self.active);
        std.debug.assert(active_cross_module_copy_transaction == self);
        self.dest_env.commitCrossModuleCopyMark(&self.env_mark);
        self.dest_store.commitSavepoint(&self.store_savepoint);
        active_cross_module_copy_transaction = null;
        self.binding_origin_keys.deinit(self.allocator);
        self.mapping_keys.deinit(self.allocator);
        self.active = false;
    }

    fn rollback(self: *CrossModuleCopyTransaction) void {
        if (!self.active) return;
        std.debug.assert(active_cross_module_copy_transaction == self);
        var key_index = self.mapping_keys.items.len;
        while (key_index > 0) {
            key_index -= 1;
            if (!self.var_mapping.remove(self.mapping_keys.items[key_index])) {
                std.debug.panic("cross-module copy rollback lost a journaled variable mapping", .{});
            }
        }
        if (self.binding_mapping_origins) |origins| {
            var origin_key_index = self.binding_origin_keys.items.len;
            while (origin_key_index > 0) {
                origin_key_index -= 1;
                if (!origins.remove(self.binding_origin_keys.items[origin_key_index])) {
                    std.debug.panic("binding-codec rollback lost a journaled mapping origin", .{});
                }
            }
        } else if (self.binding_origin_keys.items.len != 0) {
            std.debug.panic("binding-codec origin journal had no owner", .{});
        }
        self.dest_env.rollbackCrossModuleCopyMark(&self.env_mark);
        self.dest_store.rollbackToSavepoint(&self.store_savepoint);
        active_cross_module_copy_transaction = null;
        self.binding_origin_keys.deinit(self.allocator);
        self.mapping_keys.deinit(self.allocator);
        self.active = false;
    }

    /// Reserve one exact absent mapping insertion in this transaction's journal
    /// before mutating its one caller-owned table.
    fn putNewMapping(
        self: *CrossModuleCopyTransaction,
        mapping: *VarMapping,
        source: Var,
        destination: Var,
    ) std.mem.Allocator.Error!void {
        std.debug.assert(self.active);
        std.debug.assert(active_cross_module_copy_transaction == self);
        std.debug.assert(mapping == self.var_mapping);
        if (mapping.contains(source)) {
            std.debug.panic("cross-module copy attempted to replace an existing variable mapping", .{});
        }

        try self.mapping_keys.ensureUnusedCapacity(self.allocator, 1);
        try mapping.ensureUnusedCapacity(1);

        self.mapping_keys.appendAssumeCapacity(source);
        mapping.putAssumeCapacityNoClobber(source, destination);
    }

    fn attachBindingMappingOrigins(
        self: *CrossModuleCopyTransaction,
        origins: *BindingCodecMappingOrigins,
    ) void {
        std.debug.assert(self.active);
        if (self.binding_mapping_origins != null or self.binding_origin_keys.items.len != 0) {
            std.debug.panic("cross-module copy attached binding origins twice", .{});
        }
        self.binding_mapping_origins = origins;
    }

    fn putNewBindingMappingOrigin(
        self: *CrossModuleCopyTransaction,
        source: Var,
        origin: BindingCodecMappingOrigin,
    ) std.mem.Allocator.Error!void {
        const origins = self.binding_mapping_origins orelse
            std.debug.panic("binding-codec mapping origin had no transaction owner", .{});
        if (origins.contains(source)) {
            std.debug.panic("binding-codec copy attempted to replace a mapping origin", .{});
        }
        try self.binding_origin_keys.ensureUnusedCapacity(self.allocator, 1);
        try origins.ensureUnusedCapacity(1);
        self.binding_origin_keys.appendAssumeCapacity(source);
        origins.putAssumeCapacityNoClobber(source, origin);
    }
};

/// A public graph-copy boundary is never recursively entered. The thread-local
/// owner guard rejects accidental nesting before a second savepoint is opened;
/// nominal recursion and binding components stay inside one transaction.
threadlocal var active_cross_module_copy_transaction: ?*CrossModuleCopyTransaction = null;

/// All state threaded through a single cross-module copy operation.
const CopyContext = struct {
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    var_mapping: *VarMapping,
    alias_source_mapping: ?*const AliasSourceMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
    marker_copy_origin: ?CrossModuleWhereMarkerCopyOrigin,
    binding_mapping_origins: ?*const BindingCodecMappingOrigins = null,
    publish_support_step: bool,
    predicted_copy_step: u32,
    /// Transaction owning every caller-map insertion. Auxiliary nominal-
    /// template copying uses its private map and never enters this journal.
    transaction: *CrossModuleCopyTransaction,
    carried_where_marker: bool = false,
    /// Nominal declaration-table installation is auxiliary metadata, not part
    /// of the copied occurrence. Suppress proof pairs while its template is
    /// traversed so the relation is independent of destination table history.
    nominal_decl_aux_depth: u32 = 0,
    /// Declaration-table templates have their own memoization domain. Using
    /// the occurrence map here would make a first nominal copy preseed the
    /// later application-argument walk, while a destination with the same
    /// declaration already installed would not, making proof pairs depend on
    /// destination history.
    nominal_decl_var_mapping: VarMapping,
    scratch: CopyScratch = .{},

    fn deinit(self: *CopyContext) void {
        self.scratch.deinit(self.allocator);
        self.nominal_decl_var_mapping.deinit();
    }

    fn activeVarMapping(self: *CopyContext) *VarMapping {
        return if (self.nominal_decl_aux_depth == 0)
            self.var_mapping
        else
            &self.nominal_decl_var_mapping;
    }

    /// Publish one mapping which `request` already proved absent. Reserve the
    /// rollback journal and hash table before either append, then perform both
    /// mutations infallibly so the enclosing cross-copy transaction can remove
    /// every new caller-map key in reverse order.
    fn putNewMapping(self: *CopyContext, source: Var, destination: Var) std.mem.Allocator.Error!void {
        const mapping = self.activeVarMapping();
        if (mapping.contains(source)) {
            std.debug.panic("cross-module copy attempted to replace an existing variable mapping", .{});
        }
        if (self.nominal_decl_aux_depth != 0) {
            try mapping.put(source, destination);
            return;
        }
        try self.transaction.putNewMapping(mapping, source, destination);
    }

    fn sourceIdents(self: *const CopyContext) *const base.Ident.Store {
        return self.source_env.getIdentStoreConst();
    }

    fn copyIdent(self: *const CopyContext, source_ident: base.Ident.Idx) std.mem.Allocator.Error!base.Ident.Idx {
        const text = self.sourceIdents().getText(source_ident);
        const source_ident_value = base.Ident.for_text(text);
        const dest_idents = self.dest_env.getIdentStore();
        if (dest_idents.lookup(source_ident_value)) |existing| return existing;
        // Identifier storage belongs to the destination ModuleEnv and must use
        // its allocator so the enclosing env mark can own the insertion's
        // complete allocation lifetime independently of scratch allocation.
        return try self.dest_env.insertIdent(source_ident_value);
    }

    /// Rebase an env-local module identity index from the source env's
    /// identity table into the destination env's table via the 32-byte
    /// content identity hash.
    fn copyOriginModule(self: *const CopyContext, source_origin: base.ModuleIdentity.Idx) std.mem.Allocator.Error!base.ModuleIdentity.Idx {
        const hash = self.source_env.moduleIdentityHash(source_origin);
        if (self.dest_env.lookupModuleIdentity(hash)) |existing| return existing;
        const source_display = self.source_env.moduleIdentityDisplayIdent(source_origin);
        const display = if (source_display.isNone())
            base.Ident.Idx.NONE
        else
            try self.copyIdent(source_display);
        return try self.dest_env.internModuleIdentity(hash, display);
    }
};

/// State shared by every frame that owns a destination placeholder: the var to
/// fill once its children are copied, and the descriptor flag carried over
/// from the source var.
const Fill = struct {
    source_var: Var,
    placeholder: Var,
    empty_tag_union_is_default: bool,
};

const IdentityResult = enum { flex, rigid };

const FuncKind = enum { pure, effectful, unbound };
const FieldAxis = enum { type_var, presence_var };

const MarkerContractSortContext = struct {
    steps: []const WhereMethodMarkerPathStep,

    fn path(self: @This(), contract: WhereMethodMarkerContract) []const WhereMethodMarkerPathStep {
        return self.steps[contract.path_start..][0..contract.path_len];
    }
};

fn importedMarkerContractLessThan(
    context: MarkerContractSortContext,
    a: PendingImportedMarkerContract,
    b: PendingImportedMarkerContract,
) bool {
    const path_order = types_mod.compareWhereMethodMarkerPathSlices(
        context.path(a.contract),
        context.path(b.contract),
    );
    if (path_order != .eq) return path_order == .lt;
    return a.source_contract_offset < b.source_contract_offset;
}

fn sortImportedMarkerContracts(
    contracts: []PendingImportedMarkerContract,
    steps: []const WhereMethodMarkerPathStep,
) void {
    const context = MarkerContractSortContext{ .steps = steps };
    std.mem.sortUnstable(
        PendingImportedMarkerContract,
        contracts,
        context,
        importedMarkerContractLessThan,
    );
}

/// One suspended step of the cross-module copy. Every frame that mints a
/// placeholder has already registered it in `var_mapping`, so a child that
/// re-reaches the frame's source var resolves to the placeholder instead of
/// descending again—the same cycle termination the recursion used.
///
/// Source runs are held as slices: the copy only ever writes to the
/// destination store, so a source run stays valid across the children that
/// suspend the frame holding it.
const Frame = union(enum) {
    alias_substitution: AliasSubstitutionFrame,
    identity: IdentityFrame,
    alias: AliasFrame,
    tuple: TupleFrame,
    nominal: NominalFrame,
    nominal_decl: NominalDeclFrame,
    func: FuncFrame,
    record: RecordFrame,
    record_unbound: RecordUnboundFrame,
    tag_union: TagUnionFrame,
};

/// An alias replaced wholesale by an explicit destination root still copies
/// its children, because they are independently recorded platform identity
/// slots; their copies are discarded here.
const AliasSubstitutionFrame = struct {
    source_var: Var,
    source: Alias,
    dest_var: Var,
    backing: Var,
    args: []const Var,
    idx: u32 = 0,
    values_base: u32,
    stage: enum { backing, args } = .backing,
};

/// A flex or rigid var, copying its static-dispatch constraint list one
/// constraint at a time.
const IdentityFrame = struct {
    fill: Fill,
    result: IdentityResult,
    name: ?base.Ident.Idx,
    source_constraints: []const StaticDispatchConstraint,
    source_constraints_start: u32,
    idx: u32 = 0,
    /// Base of this frame's collected constraints in `pending_constraints`.
    cons_base: u32,
    /// Base of the current constraint's collected parts in `pending_parts`.
    parts_base: u32 = 0,
    part_idx: u32 = 0,
    marker_idx: u32 = 0,
    marker_contracts_base: u32 = 0,
    marker_steps_base: u32 = 0,
    pending: StaticDispatchConstraint = undefined,
    stage: enum {
        head,
        await_fn,
        marker_row,
        parts,
        await_part,
        await_item,
        finish_constraint,
        finish,
    } = .head,
};

const AliasFrame = struct {
    fill: Fill,
    source: Alias,
    translated_ident: base.Ident.Idx,
    backing: Var,
    args: []const Var,
    idx: u32 = 0,
    values_base: u32,
    stage: enum { backing, args } = .backing,
};

const TupleFrame = struct {
    fill: Fill,
    elems: []const Var,
    idx: u32 = 0,
    values_base: u32,
};

const NominalFrame = struct {
    fill: Fill,
    source: NominalType,
    translated_ident: base.Ident.Idx,
    translated_origin: base.ModuleIdentity.Idx,
    args: []const Var,
    idx: u32 = 0,
    values_base: u32,
    stage: enum { decl, args } = .decl,
};

/// One declaration-table entry crossing the boundary. This frame fills no
/// placeholder and leaves no value behind: it exists purely to copy the
/// entry's formals and backing template onto the reserved entry.
const NominalDeclFrame = struct {
    reserved_idx: types_mod.NominalDecl.Idx,
    formals: []const Var,
    backing: Var,
    idx: u32 = 0,
    values_base: u32,
    formals_range: Var.SafeList.Range = Var.SafeList.Range.empty(),
    stage: enum { formals, backing, finish } = .formals,
};

const FuncFrame = struct {
    fill: Fill,
    kind: FuncKind,
    args: []const Var,
    ret: Var,
    effect_deps: []const Var,
    idx: u32 = 0,
    values_base: u32,
    stage: enum { args, ret, effect_deps } = .args,
};

/// A record row. Field names are translated as each field is reached, keeping
/// destination identifier interning in the recursion's order; the copied field
/// vars land on the value stack and are zipped back onto the collected names.
const RecordFrame = struct {
    fill: Fill,
    source_fields: RecordField.SafeMultiList.Range,
    ext: Var,
    idx: u32 = 0,
    axis: FieldAxis = .type_var,
    fields_base: u32,
    values_base: u32,
    fields_range: RecordField.SafeMultiList.Range = undefined,
    stage: enum { fields, await_ext } = .fields,
};

const RecordUnboundFrame = struct {
    fill: Fill,
    source_fields: RecordField.SafeMultiList.Range,
    idx: u32 = 0,
    axis: FieldAxis = .type_var,
    fields_base: u32,
    values_base: u32,
};

const TagUnionFrame = struct {
    fill: Fill,
    source_tags: Tag.SafeMultiList.Range,
    ext: Var,
    tag_idx: u32 = 0,
    arg_idx: u32 = 0,
    /// Base of the current tag's copied payload vars in the value stack.
    values_base: u32,
    /// Base of this frame's collected tags in `pending_tags`.
    tags_base: u32,
    tags_range: Tag.SafeMultiList.Range = undefined,
    stage: enum { tag_head, tag_args, tags_done, await_ext } = .tag_head,
};

/// Copy a type from one module's type store to another module's type store.
/// Unmapped source roots receive fresh destination variables. Roots already in
/// `var_mapping` are exact substitutions and are reused without copying. When
/// `alias_source_mapping` is present, every alias carrying a matching explicit
/// declaration identity resolves directly to that destination root.
///
/// Imported identifiers are interned directly into the destination module's
/// authoritative identifier store so all copied types in that module reference
/// one consistent `Ident.Store`; imported module identities are rebased into
/// the destination module's identity table the same way.
///
/// The graph copy runs on an explicit heap worklist, so copy depth is bounded
/// only by available memory, never by the native stack.
pub fn copyVar(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *VarMapping,
    alias_source_mapping: ?*const AliasSourceMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!Var {
    return copyVarWithMarkerLineage(
        source_store,
        dest_store,
        source_var,
        var_mapping,
        alias_source_mapping,
        source_env,
        dest_env,
        allocator,
        null,
    );
}

/// Copy a type graph while publishing an exact occurrence-lineage step when
/// the traversal carries a where-method marker or static-dispatch constraint.
/// Passing null is an explicit marker- and constraint-free assertion;
/// encountering either then is a compiler invariant violation, never a
/// provenance fallback.
pub fn copyVarWithMarkerLineage(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *VarMapping,
    alias_source_mapping: ?*const AliasSourceMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
    marker_copy_origin: ?CrossModuleWhereMarkerCopyOrigin,
) std.mem.Allocator.Error!Var {
    return (try copyVarWithMarkerLineageResult(
        source_store,
        dest_store,
        source_var,
        var_mapping,
        alias_source_mapping,
        source_env,
        dest_env,
        allocator,
        marker_copy_origin,
    )).var_;
}

pub const MarkerCopyResult = struct {
    var_: Var,
    copy_step: ?u32,
};

/// One imported binding-codec requirement copied as a single ownership
/// transaction. The destination constraint is already appended at
/// `constraint_index`; its receiver and callable component steps both name
/// exact cuts through the imported binding-root support relation.
pub const ImportedConstraintMarkerCopyResult = struct {
    receiver_var: Var,
    constraint: StaticDispatchConstraint,
    constraint_index: u32,
    receiver_copy_step: u32,
    function_copy_step: u32,
};

/// Copy one detached binding-codec constraint under the exact mapping that
/// imported its owning binding. Unlike an attached descriptor constraint, the
/// receiver and callable are separate roots; publish both complete component
/// relations, then copy the marker metadata with immediate bases through the
/// callable step. All externally visible proof/constraint pools roll back to
/// their entry lengths on OOM, so no half of the two-step certificate can
/// survive independently.
pub fn copyImportedConstraintWithMarkerLineage(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_receiver_var: Var,
    source_constraint_index: u32,
    source_constraint: StaticDispatchConstraint,
    var_mapping: *VarMapping,
    binding_mapping_origins: *BindingCodecMappingOrigins,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
    binding_root_step: u32,
    requirement_ordinal: u32,
) std.mem.Allocator.Error!ImportedConstraintMarkerCopyResult {
    if (source_constraint_index >= source_store.static_dispatch_constraints.items.items.len or
        !std.meta.eql(source_store.static_dispatch_constraints.items.items[source_constraint_index], source_constraint))
    {
        std.debug.panic("binding-codec copy did not name its exact source constraint", .{});
    }
    if (source_constraint.derived_map_plan != null or source_constraint.interpolation.isPresent()) {
        std.debug.panic("binding-codec constraint carried unsupported non-marker metadata", .{});
    }
    if (literal_defaulting.constraintLiteralKind(.{
        .from_numeral = source_env.idents.from_numeral,
        .from_quote = source_env.idents.from_quote,
        .from_interpolation = source_env.idents.from_interpolation,
    }, source_constraint) != null) {
        std.debug.panic("binding-codec ingress cannot copy a literal-conversion constraint", .{});
    }
    if (source_constraint.where_method_markers.len() == 0 and
        source_constraint.where_method_marker_bases.len() != 0)
    {
        std.debug.panic("marker-free binding-codec constraint carried marker bases", .{});
    }
    if (binding_root_step >= dest_env.where_marker_copy_steps.items.items.len or
        requirement_ordinal == std.math.maxInt(u32))
    {
        std.debug.panic("binding-codec copy named an absent requirement origin", .{});
    }

    var transaction: CrossModuleCopyTransaction = undefined;
    try transaction.beginPublic(
        dest_store,
        dest_env,
        var_mapping,
        allocator,
    );
    defer transaction.rollback();
    transaction.attachBindingMappingOrigins(binding_mapping_origins);
    const steps_len = dest_env.where_marker_copy_steps.items.items.len;
    const copied_groups_len = dest_env.copied_open_literal_groups.items.items.len;
    const copied_events_len = dest_env.copied_open_literal_events.items.items.len;
    const copied_groups_start: u32 = @intCast(copied_groups_len);

    const receiver_origin: CrossModuleWhereMarkerCopyOrigin = .{ .binding_codec_receiver = .{
        .binding_root_step = binding_root_step,
        .requirement_ordinal = requirement_ordinal,
    } };
    var receiver_ctx = CopyContext{
        .source_store = source_store,
        .dest_store = dest_store,
        .var_mapping = var_mapping,
        .alias_source_mapping = null,
        .source_env = source_env,
        .dest_env = dest_env,
        .allocator = allocator,
        .marker_copy_origin = receiver_origin,
        .binding_mapping_origins = binding_mapping_origins,
        .publish_support_step = true,
        .predicted_copy_step = @intCast(steps_len),
        .transaction = &transaction,
        .nominal_decl_var_mapping = VarMapping.init(allocator),
    };
    defer receiver_ctx.deinit();
    const copied_receiver = try copyVarCtx(&receiver_ctx, source_receiver_var);
    const receiver_copy_step = copied_receiver.copy_step orelse
        std.debug.panic("binding-codec receiver support copy omitted its step", .{});
    try recordBindingCodecStepMappingOrigins(&transaction, receiver_copy_step);

    const function_origin: CrossModuleWhereMarkerCopyOrigin = .{ .binding_codec_function = .{
        .binding_root_step = binding_root_step,
        .requirement_ordinal = requirement_ordinal,
    } };
    var function_ctx = CopyContext{
        .source_store = source_store,
        .dest_store = dest_store,
        .var_mapping = var_mapping,
        .alias_source_mapping = null,
        .source_env = source_env,
        .dest_env = dest_env,
        .allocator = allocator,
        .marker_copy_origin = function_origin,
        .binding_mapping_origins = binding_mapping_origins,
        .publish_support_step = true,
        .predicted_copy_step = std.math.add(u32, receiver_copy_step, 1) catch
            std.debug.panic("binding-codec component step index overflowed u32", .{}),
        .transaction = &transaction,
        .nominal_decl_var_mapping = VarMapping.init(allocator),
    };
    defer function_ctx.deinit();
    const copied_function = try copyVarCtx(&function_ctx, source_constraint.fn_var);
    const function_copy_step = copied_function.copy_step orelse
        std.debug.panic("binding-codec function support copy omitted its step", .{});
    try recordBindingCodecStepMappingOrigins(&transaction, function_copy_step);
    const receiver_component_step = dest_env.where_marker_copy_steps.items.items[receiver_copy_step];
    const function_component_step = dest_env.where_marker_copy_steps.items.items[function_copy_step];
    const receiver_groups_end = std.math.add(
        u32,
        receiver_component_step.copied_groups_start,
        receiver_component_step.copied_groups_len,
    ) catch std.debug.panic("binding-codec receiver group range overflowed u32", .{});
    const function_groups_end = std.math.add(
        u32,
        function_component_step.copied_groups_start,
        function_component_step.copied_groups_len,
    ) catch std.debug.panic("binding-codec function group range overflowed u32", .{});
    if (receiver_component_step.copied_groups_start != copied_groups_start or
        function_component_step.copied_groups_start != receiver_groups_end or
        @as(usize, function_groups_end) != dest_env.copied_open_literal_groups.items.items.len)
    {
        std.debug.panic("binding-codec component literal groups were not gaplessly owned", .{});
    }
    var expected_component_event_start: u32 = @intCast(copied_events_len);
    for (dest_env.copied_open_literal_groups.items.items[receiver_component_step.copied_groups_start..][0..receiver_component_step.copied_groups_len]) |group| {
        const component = group.component.decodedBindingCodecReceiver() orelse
            std.debug.panic("binding-codec receiver copy published a mismatched literal group", .{});
        if (group.copy_step_index != receiver_copy_step or
            component.binding_root_step != binding_root_step or
            component.requirement_ordinal != requirement_ordinal or
            group.events_start != expected_component_event_start)
        {
            std.debug.panic("binding-codec receiver literal group lost its exact origin", .{});
        }
        expected_component_event_start = std.math.add(
            u32,
            expected_component_event_start,
            group.events_len,
        ) catch std.debug.panic("binding-codec receiver event range overflowed u32", .{});
    }
    for (dest_env.copied_open_literal_groups.items.items[function_component_step.copied_groups_start..][0..function_component_step.copied_groups_len]) |group| {
        const component = group.component.decodedBindingCodecFunction() orelse
            std.debug.panic("binding-codec function copy published a mismatched literal group", .{});
        if (group.copy_step_index != function_copy_step or
            component.binding_root_step != binding_root_step or
            component.requirement_ordinal != requirement_ordinal or
            group.events_start != expected_component_event_start)
        {
            std.debug.panic("binding-codec function literal group lost its exact origin", .{});
        }
        expected_component_event_start = std.math.add(
            u32,
            expected_component_event_start,
            group.events_len,
        ) catch std.debug.panic("binding-codec function event range overflowed u32", .{});
    }
    if (@as(usize, expected_component_event_start) != dest_env.copied_open_literal_events.items.items.len) {
        std.debug.panic("binding-codec component literal events were not gaplessly owned", .{});
    }
    const component_groups_end = dest_env.copied_open_literal_groups.items.items.len;
    const component_events_end = dest_env.copied_open_literal_events.items.items.len;

    var pending_contracts: std.ArrayList(PendingImportedMarkerContract) = .empty;
    defer pending_contracts.deinit(allocator);
    var pending_paths: std.ArrayList(WhereMethodMarkerPathStep) = .empty;
    defer pending_paths.deinit(allocator);

    const source_marker_start: u32 = @intFromEnum(source_constraint.where_method_markers.start);
    for (0..source_constraint.where_method_markers.len()) |raw_offset| {
        const source_contract_offset: u32 = @intCast(raw_offset);
        const source_marker_index = std.math.add(u32, source_marker_start, source_contract_offset) catch
            std.debug.panic("binding-codec source marker index overflowed u32", .{});
        if (source_marker_index >= source_store.where_method_marker_contracts.items.items.len) {
            std.debug.panic("binding-codec source marker range escaped its pool", .{});
        }
        var copied_marker = source_store.where_method_marker_contracts.items.items[source_marker_index];
        if (copied_marker.positionOrNull() == null or copied_marker.isWidened() == null or
            copied_marker.isReady() != true or copied_marker.hasProducer() or
            copied_marker.path_start > source_store.where_method_marker_path_steps.items.items.len or
            copied_marker.path_len > source_store.where_method_marker_path_steps.items.items.len - copied_marker.path_start)
        {
            std.debug.panic("binding-codec source marker contract was invalid", .{});
        }
        const local_path_start: u32 = @intCast(pending_paths.items.len);
        for (source_store.where_method_marker_path_steps.items.items[copied_marker.path_start..][0..copied_marker.path_len]) |source_step| {
            const kind = source_step.kindOrNull() orelse
                std.debug.panic("binding-codec marker path kind was invalid", .{});
            var destination_step = source_step;
            switch (kind) {
                .record_field, .tag_payload => destination_step.name = @bitCast(try function_ctx.copyIdent(@bitCast(source_step.name))),
                .nominal_arg => {
                    const source_decl: types_mod.SourceDecl = @bitCast(source_step.source_decl);
                    if (source_store.canonicalNominalDeclForMarkerPath(source_step) == null) {
                        std.debug.panic("binding-codec nominal marker path did not name its source declaration", .{});
                    }
                    const destination_origin = try function_ctx.copyOriginModule(@enumFromInt(source_step.origin_module));
                    const destination_decl_index = dest_store.lookupNominalDeclByKey(
                        destination_origin,
                        source_decl.statement,
                    ) orelse std.debug.panic("binding-codec nominal marker path had no destination declaration", .{});
                    const destination_decl = dest_store.getNominalDecl(destination_decl_index);
                    if (destination_decl.origin_module != destination_origin or
                        !destination_decl.source.sourceDecl().eql(source_decl) or
                        destination_decl.formals.len() != source_step.arity)
                    {
                        std.debug.panic("binding-codec nominal marker declaration did not preserve its key", .{});
                    }
                    destination_step.name = @bitCast(destination_decl.ident.ident_idx);
                    destination_step.origin_module = @intFromEnum(destination_origin);
                    if (dest_store.canonicalNominalDeclForMarkerPath(destination_step) == null) {
                        std.debug.panic("binding-codec nominal marker path was not canonical in the destination", .{});
                    }
                },
                .fn_arg, .fn_ret, .tuple_elem => {},
            }
            try pending_paths.append(allocator, destination_step);
        }
        copied_marker.path_start = local_path_start;
        try pending_contracts.append(allocator, .{
            .contract = copied_marker,
            .source_contract_offset = source_contract_offset,
        });
    }

    sortImportedMarkerContracts(pending_contracts.items, pending_paths.items);
    var unique_contract_count: usize = 0;
    var unique_path_count: usize = 0;
    for (pending_contracts.items, 0..) |pending, pending_index| {
        const is_duplicate = pending_index != 0 and
            types_mod.compareWhereMethodMarkerPathSlices(
                pending_paths.items[pending_contracts.items[pending_index - 1].contract.path_start..][0..pending_contracts.items[pending_index - 1].contract.path_len],
                pending_paths.items[pending.contract.path_start..][0..pending.contract.path_len],
            ) == .eq;
        if (!is_duplicate) {
            unique_contract_count += 1;
            unique_path_count += pending.contract.path_len;
        }
    }

    const destination_fn_name = try dest_env.insertIdent(base.Ident.for_text(
        source_env.getIdentStoreConst().getText(source_constraint.fn_name),
    ));
    if (dest_store.static_dispatch_constraints.items.items.len >= std.math.maxInt(u32) or
        dest_env.selected_receiver_anchors.items.items.len >= std.math.maxInt(u32) or
        dest_store.constraint_evidence_handles.items.items.len >= std.math.maxInt(u32))
    {
        std.debug.panic("binding-codec copied-source indexes overflowed u32", .{});
    }
    try dest_store.where_method_marker_path_steps.items.ensureUnusedCapacity(allocator, unique_path_count);
    try dest_store.where_method_marker_contracts.items.ensureUnusedCapacity(allocator, unique_contract_count);
    try dest_store.where_method_marker_bases.items.ensureUnusedCapacity(allocator, pending_contracts.items.len);
    try dest_store.static_dispatch_constraints.items.ensureUnusedCapacity(allocator, 1);
    try dest_env.where_marker_constraint_copy_pairs.items.ensureUnusedCapacity(allocator, 1);
    try dest_env.where_marker_copy_witnesses.items.ensureUnusedCapacity(allocator, 1);
    try dest_env.selected_receiver_anchors.items.ensureUnusedCapacity(allocator, 1);
    try dest_env.dispatch_settlement_sources.items.ensureUnusedCapacity(allocator, 1);
    try dest_store.constraint_evidence_handles.items.ensureUnusedCapacity(allocator, 1);

    const destination_contract_start: u32 = @intCast(dest_store.where_method_marker_contracts.items.items.len);
    const destination_basis_start: u32 = @intCast(dest_store.where_method_marker_bases.items.items.len);
    var destination_marker_offset: u32 = undefined;
    for (pending_contracts.items, 0..) |pending, pending_index| {
        const is_duplicate = pending_index != 0 and
            types_mod.compareWhereMethodMarkerPathSlices(
                pending_paths.items[pending_contracts.items[pending_index - 1].contract.path_start..][0..pending_contracts.items[pending_index - 1].contract.path_len],
                pending_paths.items[pending.contract.path_start..][0..pending.contract.path_len],
            ) == .eq;
        if (!is_duplicate) {
            destination_marker_offset = @intCast(
                dest_store.where_method_marker_contracts.items.items.len - @as(usize, destination_contract_start),
            );
            const destination_path_start: u32 = @intCast(dest_store.where_method_marker_path_steps.items.items.len);
            dest_store.where_method_marker_path_steps.items.appendSliceAssumeCapacity(
                pending_paths.items[pending.contract.path_start..][0..pending.contract.path_len],
            );
            var destination_contract = pending.contract;
            destination_contract.path_start = destination_path_start;
            dest_store.where_method_marker_contracts.items.appendAssumeCapacity(destination_contract);
        } else {
            const previous = &dest_store.where_method_marker_contracts.items.items[
                @as(usize, destination_contract_start) + destination_marker_offset
            ];
            if (!types_mod.whereMethodMarkerNominalNamesMatch(
                dest_store.where_method_marker_path_steps.items.items[previous.path_start..][0..previous.path_len],
                pending_paths.items[pending.contract.path_start..][0..pending.contract.path_len],
            ) or previous.position != pending.contract.position) {
                std.debug.panic("binding-codec rebased paths carried incompatible metadata", .{});
            }
            previous.widened = @intFromBool(previous.isWidened().? or pending.contract.isWidened().?);
        }
        dest_store.where_method_marker_bases.items.appendAssumeCapacity(.{
            .marker_offset = destination_marker_offset,
            .copy_step = function_copy_step,
            .source_constraint_index = source_constraint_index,
            .source_contract_offset = pending.source_contract_offset,
        });
    }
    const receiver_step = dest_env.where_marker_copy_steps.items.items[receiver_copy_step];
    const function_step = dest_env.where_marker_copy_steps.items.items[function_copy_step];
    if (receiver_step.decodedKind() != .binding_codec_receiver or
        receiver_step.origin.binding_codec_receiver.binding_root_step != binding_root_step or
        receiver_step.origin.binding_codec_receiver.requirement_ordinal != requirement_ordinal or
        function_step.decodedKind() != .binding_codec_function or
        function_step.origin.binding_codec_function.binding_root_step != binding_root_step or
        function_step.origin.binding_codec_function.requirement_ordinal != requirement_ordinal)
    {
        std.debug.panic("binding-codec component steps lost their common exact origin", .{});
    }
    for (dest_store.where_method_marker_bases.items.items[destination_basis_start..][0..pending_contracts.items.len]) |basis| {
        if (basis.copy_step != function_copy_step) {
            std.debug.panic("binding-codec constraint basis did not use its function component step", .{});
        }
    }

    var destination_constraint = source_constraint;
    destination_constraint.constraint_evidence = .none;
    destination_constraint.fn_name = destination_fn_name;
    destination_constraint.fn_var = copied_function.var_;
    destination_constraint.provenance = .{};
    destination_constraint.where_method_markers = .{
        .start = @enumFromInt(destination_contract_start),
        .count = @intCast(unique_contract_count),
    };
    destination_constraint.where_method_marker_bases = .{
        .start = @enumFromInt(destination_basis_start),
        .count = @intCast(pending_contracts.items.len),
    };
    if (literal_defaulting.constraintLiteralKind(.{
        .from_numeral = dest_env.idents.from_numeral,
        .from_quote = dest_env.idents.from_quote,
        .from_interpolation = dest_env.idents.from_interpolation,
    }, destination_constraint) != null) {
        std.debug.panic("binding-codec destination ingress became a literal-conversion constraint", .{});
    }
    const destination_constraint_index: u32 = @intCast(dest_store.static_dispatch_constraints.items.items.len);
    dest_store.static_dispatch_constraints.items.appendAssumeCapacity(destination_constraint);
    const ingress = commitBindingCodecRequirementIngress(
        source_store,
        dest_store,
        dest_env,
        source_receiver_var,
        copied_receiver.var_,
        receiver_copy_step,
        function_copy_step,
        binding_root_step,
        requirement_ordinal,
        source_constraint_index,
        destination_constraint_index,
    );

    const anchor_index: u32 = @intCast(dest_env.selected_receiver_anchors.items.items.len);
    const handle_start: u32 = @intCast(dest_store.constraint_evidence_handles.items.items.len);
    const source = ModuleEnv.DispatchSettlementSource.copiedConstraint(
        anchor_index,
        ModuleEnv.CopiedConstraintComponentRef.bindingCodecReceiver(
            receiver_copy_step,
            ingress.receiver_occurrence_offset,
            requirement_ordinal,
            binding_root_step,
        ),
        ModuleEnv.CopiedConstraintComponentRef.bindingCodecFunction(
            function_copy_step,
            ingress.function_occurrence_offset,
            ingress.constraint_pair_offset,
            requirement_ordinal,
            binding_root_step,
        ),
    );
    if (!source.hasCanonicalTags()) {
        std.debug.panic("binding-codec copy built a noncanonical settlement source", .{});
    }
    dest_env.selected_receiver_anchors.items.appendAssumeCapacity(.{
        .constraint_index = destination_constraint_index,
        .receiver_var = @intFromEnum(copied_receiver.var_),
        .kind = @intFromEnum(ModuleEnv.SelectedMethodDecision.ReceiverAnchorKind.copied_constraint),
        .node = ModuleEnv.SelectedMethodDecision.none,
        .slot = ModuleEnv.SelectedMethodDecision.none,
        .copy_step = receiver_copy_step,
        .receiver_occurrence_offset = ingress.receiver_occurrence_offset,
        .constraint_pair_offset = ingress.constraint_pair_offset,
    });
    dest_store.constraint_evidence_handles.items.appendAssumeCapacity(.{
        .kind = @intFromEnum(types_mod.ConstraintEvidenceHandle.Kind.selected_receiver_anchor),
        .index = anchor_index,
    });
    dest_env.dispatch_settlement_sources.items.appendAssumeCapacity(source);
    destination_constraint.constraint_evidence = .{ .start = handle_start, .len = 1 };
    dest_store.static_dispatch_constraints.items.items[destination_constraint_index]
        .constraint_evidence = destination_constraint.constraint_evidence;
    if (dest_env.copied_open_literal_groups.items.items.len != component_groups_end or
        dest_env.copied_open_literal_events.items.items.len != component_events_end)
    {
        std.debug.panic("detached binding-codec ingress published literal inventory", .{});
    }

    const result: ImportedConstraintMarkerCopyResult = .{
        .receiver_var = copied_receiver.var_,
        .constraint = destination_constraint,
        .constraint_index = destination_constraint_index,
        .receiver_copy_step = receiver_copy_step,
        .function_copy_step = function_copy_step,
    };
    transaction.commit();
    return result;
}

pub fn copyVarWithMarkerLineageResult(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *VarMapping,
    alias_source_mapping: ?*const AliasSourceMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
    marker_copy_origin: ?CrossModuleWhereMarkerCopyOrigin,
) std.mem.Allocator.Error!MarkerCopyResult {
    return copyVarWithMarkerLineageMode(
        source_store,
        dest_store,
        source_var,
        var_mapping,
        alias_source_mapping,
        source_env,
        dest_env,
        allocator,
        marker_copy_origin,
        false,
    );
}

/// Eager support-copy boundary. The exact graph relation is published even
/// when it carries no marker because a later binding-codec proof can name it.
pub fn copyVarWithMarkerLineageSupport(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *VarMapping,
    alias_source_mapping: ?*const AliasSourceMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
    marker_copy_origin: CrossModuleWhereMarkerCopyOrigin,
) std.mem.Allocator.Error!MarkerCopyResult {
    return copyVarWithMarkerLineageMode(
        source_store,
        dest_store,
        source_var,
        var_mapping,
        alias_source_mapping,
        source_env,
        dest_env,
        allocator,
        marker_copy_origin,
        true,
    );
}

fn copyVarWithMarkerLineageMode(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    source_var: Var,
    var_mapping: *VarMapping,
    alias_source_mapping: ?*const AliasSourceMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
    marker_copy_origin: ?CrossModuleWhereMarkerCopyOrigin,
    publish_support_step: bool,
) std.mem.Allocator.Error!MarkerCopyResult {
    if (publish_support_step and marker_copy_origin == null) {
        std.debug.panic("support marker-copy step had no finite origin", .{});
    }
    var transaction: CrossModuleCopyTransaction = undefined;
    try transaction.beginPublic(
        dest_store,
        dest_env,
        var_mapping,
        allocator,
    );
    defer transaction.rollback();
    var ctx = CopyContext{
        .source_store = source_store,
        .dest_store = dest_store,
        .var_mapping = var_mapping,
        .alias_source_mapping = alias_source_mapping,
        .source_env = source_env,
        .dest_env = dest_env,
        .allocator = allocator,
        .marker_copy_origin = marker_copy_origin,
        .publish_support_step = publish_support_step,
        .predicted_copy_step = @intCast(dest_env.where_marker_copy_steps.items.items.len),
        .transaction = &transaction,
        .nominal_decl_var_mapping = VarMapping.init(allocator),
    };
    defer ctx.deinit();
    const result = try copyVarCtx(&ctx, source_var);
    transaction.commit();
    return result;
}

fn copyVarCtx(ctx: *CopyContext, source_var: Var) std.mem.Allocator.Error!MarkerCopyResult {
    const frames_base = ctx.scratch.frames.items.len;
    const values_base = ctx.scratch.values.items.len;
    const source_root = ctx.source_store.resolveVar(source_var).var_;
    const prior_destination = ctx.activeVarMapping().get(source_root);
    const root_alias_preseed_index = platformAliasPreseedForVar(ctx, source_root);
    if (!try request(ctx, source_var)) {
        try drive(ctx, frames_base);
    }
    std.debug.assert(ctx.scratch.values.items.len == values_base + 1);
    const destination_root = ctx.scratch.values.pop().?;
    try recordRootActionWitness(
        ctx,
        source_var,
        destination_root,
        prior_destination,
        root_alias_preseed_index,
    );
    const copied_constraints = ctx.scratch.proof_constraint_pairs.items.len != 0;
    if (copied_constraints and ctx.marker_copy_origin == null) {
        std.debug.panic("constraint-bearing cross-module copy had no finite lineage origin", .{});
    }
    const copy_step = if (ctx.carried_where_marker or ctx.publish_support_step or copied_constraints)
        try publishMarkerCopyLineage(ctx, source_var, destination_root)
    else
        null;
    return .{ .var_ = destination_root, .copy_step = copy_step };
}

fn whereMarkerCopyPairLessThan(
    _: void,
    a: ModuleEnv.WhereMarkerCopyPair,
    b: ModuleEnv.WhereMarkerCopyPair,
) bool {
    if (a.source_var != b.source_var) return a.source_var < b.source_var;
    return a.destination_var < b.destination_var;
}

fn whereMarkerCopyOccurrenceLessThan(
    _: void,
    a: ModuleEnv.WhereMarkerCopyOccurrence,
    b: ModuleEnv.WhereMarkerCopyOccurrence,
) bool {
    return ModuleEnv.WhereMarkerCopyOccurrence.canonicalLessThan(a, b);
}

fn rawMarkerCopyOccurrencesAreFunctional(
    occurrences: []const ModuleEnv.WhereMarkerCopyOccurrence,
) bool {
    for (occurrences, 0..) |occurrence, occurrence_index| {
        for (occurrences[0..occurrence_index]) |prior| {
            if (prior.raw_source_var == occurrence.raw_source_var and
                prior.raw_destination_var != occurrence.raw_destination_var)
            {
                return false;
            }
        }
    }
    return true;
}

fn whereMarkerConstraintCopyPairLessThan(
    _: void,
    a: ModuleEnv.WhereMarkerConstraintCopyPair,
    b: ModuleEnv.WhereMarkerConstraintCopyPair,
) bool {
    return ModuleEnv.WhereMarkerConstraintCopyPair.canonicalLessThan(a, b);
}

fn whereMarkerCopyWitnessLessThan(
    _: void,
    a: ModuleEnv.WhereMarkerCopyWitness,
    b: ModuleEnv.WhereMarkerCopyWitness,
) bool {
    return ModuleEnv.WhereMarkerCopyWitness.canonicalLessThan(a, b);
}

fn exactMarkerCopyPairOffset(
    pairs: []const ModuleEnv.WhereMarkerCopyPair,
    source_var: u32,
    destination_var: u32,
) u32 {
    var found: ?u32 = null;
    for (pairs, 0..) |pair, offset| {
        if (pair.source_var != source_var or pair.destination_var != destination_var) continue;
        if (found != null) std.debug.panic("marker-copy relation repeated one exact pair", .{});
        found = @intCast(offset);
    }
    return found orelse std.debug.panic("marker-copy witness named a pair absent from its relation", .{});
}

fn exactMarkerCopyOccurrenceOffset(
    occurrences: []const ModuleEnv.WhereMarkerCopyOccurrence,
    raw_source_var: u32,
    raw_destination_var: u32,
) u32 {
    var found: ?u32 = null;
    for (occurrences, 0..) |occurrence, offset| {
        if (occurrence.raw_source_var != raw_source_var or
            occurrence.raw_destination_var != raw_destination_var) continue;
        if (found != null) {
            std.debug.panic("marker-copy proof repeated one exact raw occurrence", .{});
        }
        found = @intCast(offset);
    }
    return found orelse
        std.debug.panic("marker-copy witness named a raw occurrence absent from its relation", .{});
}

fn exactConstraintCopyPairOffset(
    pairs: []const ModuleEnv.WhereMarkerConstraintCopyPair,
    source_constraint_index: u32,
) u32 {
    var found: ?u32 = null;
    for (pairs, 0..) |pair, offset| {
        if (pair.source_constraint_index != source_constraint_index) continue;
        if (found != null) std.debug.panic("constraint-copy relation repeated one source occurrence", .{});
        found = @intCast(offset);
    }
    return found orelse std.debug.panic("copy witness named an absent constraint-copy occurrence", .{});
}

/// One ordinary cross-module copied constraint while its source and
/// destination stores are simultaneously available. These rows are sorted by
/// their exact raw receiver occurrence before the copied-open-literal
/// inventory is frozen.
const CopiedConstraintSourceDraft = struct {
    source_constraint_index: u32,
    destination_constraint_index: u32,
    receiver_occurrence_offset: u32,
    function_occurrence_offset: u32,
    constraint_pair_offset: u32,
};

const CopiedOpenLiteralEventDraft = struct {
    destination_constraint_index: u32,
    constraint_offset: u32,
    literal_kind: ModuleEnv.CopiedOpenLiteralEvent.LiteralKind,
};

const CopiedOpenLiteralGroupDraft = struct {
    receiver_occurrence_offset: u32,
    source_constraints_start: u32,
    source_constraints_len: u32,
    destination_constraints_start: u32,
    destination_constraints_len: u32,
    events_start: u32,
    events_len: u32,
};

fn copiedOpenLiteralKind(
    kind: StaticDispatchConstraint.LiteralKind,
) ModuleEnv.CopiedOpenLiteralEvent.LiteralKind {
    return switch (kind) {
        .numeral => .numeral,
        .quote => .quote,
        .interpolation => .interpolation,
    };
}

fn copiedConstraintSourceDraftLessThan(
    _: void,
    left: CopiedConstraintSourceDraft,
    right: CopiedConstraintSourceDraft,
) bool {
    if (left.receiver_occurrence_offset != right.receiver_occurrence_offset) {
        return left.receiver_occurrence_offset < right.receiver_occurrence_offset;
    }
    if (left.source_constraint_index != right.source_constraint_index) {
        return left.source_constraint_index < right.source_constraint_index;
    }
    return left.destination_constraint_index < right.destination_constraint_index;
}

/// Extend the latest binding-codec function support step with the detached
/// requirement occurrence that its graph traversal cannot discover through a
/// descriptor range. All required capacity is reserved by the caller before
/// this infallible suffix commit. The exact requirement row is a typed virtual
/// ingress at the root, not an additional BFS root.
const BindingCodecRequirementIngress = struct {
    receiver_occurrence_offset: u32,
    function_occurrence_offset: u32,
    constraint_pair_offset: u32,
};

fn exactBindingCodecRootOccurrence(
    source_store: *const TypesStore,
    dest_store: *const TypesStore,
    dest_env: *const ModuleEnv,
    step: ModuleEnv.WhereMarkerCopyStep,
    source_var: Var,
    destination_var: Var,
) u32 {
    const source_root = source_store.resolveVar(source_var).var_;
    const destination_root = dest_store.resolveVar(destination_var).var_;
    if (step.source_root_var != @intFromEnum(source_root) or
        step.destination_root_var != @intFromEnum(destination_root))
    {
        std.debug.panic("binding-codec component step did not bind its exact roots", .{});
    }

    const pair_start: usize = step.pairs_start;
    const pair_len: usize = step.pairs_len;
    const pairs = dest_env.where_marker_copy_pairs.items.items;
    if (pair_start > pairs.len or pair_len > pairs.len - pair_start) {
        std.debug.panic("binding-codec component pair range escaped its pool", .{});
    }
    const root_pair_offset = exactMarkerCopyPairOffset(
        pairs[pair_start..][0..pair_len],
        @intFromEnum(source_root),
        @intFromEnum(destination_root),
    );

    const occurrence_start: usize = step.occurrences_start;
    const occurrence_len: usize = step.occurrences_len;
    const occurrences = dest_env.where_marker_copy_occurrences.items.items;
    if (occurrence_start > occurrences.len or occurrence_len > occurrences.len - occurrence_start) {
        std.debug.panic("binding-codec component occurrence range escaped its pool", .{});
    }
    const root_occurrence_offset = exactMarkerCopyOccurrenceOffset(
        occurrences[occurrence_start..][0..occurrence_len],
        @intFromEnum(source_var),
        @intFromEnum(destination_var),
    );
    if (step.root_occurrence_offset != root_occurrence_offset or
        occurrences[occurrence_start + root_occurrence_offset].canonical_pair_offset != root_pair_offset)
    {
        std.debug.panic("binding-codec component root occurrence was invalid", .{});
    }
    return root_occurrence_offset;
}

fn commitBindingCodecRequirementIngress(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    dest_env: *ModuleEnv,
    source_receiver_var: Var,
    destination_receiver_var: Var,
    receiver_copy_step: u32,
    function_copy_step: u32,
    binding_root_step: u32,
    requirement_ordinal: u32,
    source_constraint_index: u32,
    destination_constraint_index: u32,
) BindingCodecRequirementIngress {
    const steps = dest_env.where_marker_copy_steps.items.items;
    if (receiver_copy_step >= steps.len or function_copy_step >= steps.len or
        binding_root_step >= steps.len or requirement_ordinal == std.math.maxInt(u32))
    {
        std.debug.panic("binding-codec requirement ingress named an absent origin coordinate", .{});
    }
    const expected_function_step = std.math.add(u32, receiver_copy_step, 1) catch
        std.debug.panic("binding-codec component step index overflowed u32", .{});
    if (function_copy_step != expected_function_step) {
        std.debug.panic("binding-codec component steps were not the returned adjacent pair", .{});
    }
    const receiver_step = steps[receiver_copy_step];
    const step = &dest_env.where_marker_copy_steps.items.items[function_copy_step];
    const receiver_group_end = std.math.add(
        u32,
        receiver_step.copied_groups_start,
        receiver_step.copied_groups_len,
    ) catch std.debug.panic("binding-codec receiver group range overflowed u32", .{});
    const function_group_end = std.math.add(
        u32,
        step.copied_groups_start,
        step.copied_groups_len,
    ) catch std.debug.panic("binding-codec function group range overflowed u32", .{});
    if (receiver_step.decodedKind() != .binding_codec_receiver or
        receiver_step.origin.binding_codec_receiver.binding_root_step != binding_root_step or
        receiver_step.origin.binding_codec_receiver.requirement_ordinal != requirement_ordinal or
        step.decodedKind() != .binding_codec_function or
        step.origin.binding_codec_function.binding_root_step != binding_root_step or
        step.origin.binding_codec_function.requirement_ordinal != requirement_ordinal or
        step.copied_groups_start != receiver_group_end or
        @as(usize, function_group_end) != dest_env.copied_open_literal_groups.items.items.len)
    {
        std.debug.panic("binding-codec requirement ingress named crossed component origins", .{});
    }
    if (source_constraint_index >= source_store.static_dispatch_constraints.items.items.len or
        destination_constraint_index >= dest_store.static_dispatch_constraints.items.items.len)
    {
        std.debug.panic("binding-codec requirement ingress named an absent constraint", .{});
    }
    const receiver_occurrence_offset = exactBindingCodecRootOccurrence(
        source_store,
        dest_store,
        dest_env,
        receiver_step,
        source_receiver_var,
        destination_receiver_var,
    );
    const source_function_var = source_store.static_dispatch_constraints.items.items[
        source_constraint_index
    ].fn_var;
    const destination_function_var = dest_store.static_dispatch_constraints.items.items[
        destination_constraint_index
    ].fn_var;
    const function_occurrence_offset = exactBindingCodecRootOccurrence(
        source_store,
        dest_store,
        dest_env,
        step.*,
        source_function_var,
        destination_function_var,
    );
    if (dest_store.static_dispatch_constraints.items.items[destination_constraint_index]
        .constraint_evidence.start != 0 or
        dest_store.static_dispatch_constraints.items.items[destination_constraint_index]
            .constraint_evidence.len != 0)
    {
        std.debug.panic("binding-codec destination constraint already carried evidence", .{});
    }

    const constraint_pool = &dest_env.where_marker_constraint_copy_pairs.items;
    const old_constraint_len: usize = step.constraint_pairs_len;
    const constraint_start: usize = step.constraint_pairs_start;
    if (constraint_start > constraint_pool.items.len or
        old_constraint_len != constraint_pool.items.len - constraint_start or
        step.constraint_pairs_len == std.math.maxInt(u32))
    {
        std.debug.panic("binding-codec function constraint proof was not the latest suffix", .{});
    }
    var constraint_insert_offset: usize = 0;
    while (constraint_insert_offset < old_constraint_len and
        constraint_pool.items[constraint_start + constraint_insert_offset].source_constraint_index <
            source_constraint_index)
    {
        constraint_insert_offset += 1;
    }
    if (constraint_insert_offset < old_constraint_len and
        constraint_pool.items[constraint_start + constraint_insert_offset].source_constraint_index ==
            source_constraint_index)
    {
        std.debug.panic("binding-codec detached constraint duplicated an attached copy occurrence", .{});
    }
    const constraint_insert_offset_u32: u32 = @intCast(constraint_insert_offset);

    const witness_pool = &dest_env.where_marker_copy_witnesses.items;
    const witness_start: usize = step.witnesses_start;
    const old_witness_len: usize = step.witnesses_len;
    if (witness_start > witness_pool.items.len or
        old_witness_len != witness_pool.items.len - witness_start or
        step.witnesses_len == std.math.maxInt(u32))
    {
        std.debug.panic("binding-codec function witness proof was not the latest suffix", .{});
    }
    for (witness_pool.items[witness_start..]) |witness| {
        if (witness.constraint_pair_offset != std.math.maxInt(u32) and
            witness.constraint_pair_offset >= step.constraint_pairs_len)
        {
            std.debug.panic("binding-codec witness constraint offset escaped its owner step", .{});
        }
    }

    const ingress = ModuleEnv.WhereMarkerCopyWitness{
        .parent_occurrence_offset = function_occurrence_offset,
        .child_occurrence_offset = function_occurrence_offset,
        .edge_kind = @intFromEnum(ModuleEnv.WhereMarkerCopyWitness.EdgeKind.scheme_requirement_function),
        .edge_index = requirement_ordinal,
        .edge_name = 0,
        .edge_origin_module = 0,
        .edge_source_decl = 0,
        .constraint_pair_offset = constraint_insert_offset_u32,
        .action = @intFromEnum(ModuleEnv.WhereMarkerCopyWitness.Action.requirement_component_ingress),
        .auxiliary_origin_kind = @intFromEnum(
            ModuleEnv.WhereMarkerCopyWitness.AuxiliaryOriginKind.binding_scheme_codec_requirement,
        ),
        .auxiliary_origin_index = requirement_ordinal,
        .raw_source_var = std.math.maxInt(u32),
        .raw_destination_var = std.math.maxInt(u32),
    };
    var witness_insert_offset: usize = 0;
    while (witness_insert_offset < old_witness_len) {
        var remapped_witness = witness_pool.items[witness_start + witness_insert_offset];
        if (remapped_witness.constraint_pair_offset != std.math.maxInt(u32) and
            remapped_witness.constraint_pair_offset >= constraint_insert_offset_u32)
        {
            remapped_witness.constraint_pair_offset += 1;
        }
        if (!whereMarkerCopyWitnessLessThan({}, remapped_witness, ingress)) break;
        witness_insert_offset += 1;
    }
    if (witness_insert_offset < old_witness_len) {
        var remapped_witness = witness_pool.items[witness_start + witness_insert_offset];
        if (remapped_witness.constraint_pair_offset != std.math.maxInt(u32) and
            remapped_witness.constraint_pair_offset >= constraint_insert_offset_u32)
        {
            remapped_witness.constraint_pair_offset += 1;
        }
        if (std.meta.eql(remapped_witness, ingress)) {
            std.debug.panic("binding-codec requirement ingress witness was duplicated", .{});
        }
    }

    const anchors = dest_env.selected_receiver_anchors.items.items;
    const sources = dest_env.dispatch_settlement_sources.items.items;
    for (anchors, 0..) |anchor, anchor_index| {
        if (anchor.decodedKind() != .copied_constraint or
            anchor.copy_step != function_copy_step or
            anchor.constraint_pair_offset < constraint_insert_offset_u32)
        {
            continue;
        }
        if (anchor.constraint_pair_offset >= step.constraint_pairs_len) {
            std.debug.panic("binding-codec copied anchor pair escaped its function step", .{});
        }
        var matching_source_count: usize = 0;
        for (sources) |source| {
            const copied = source.decodedCopiedConstraint() orelse continue;
            if (copied.anchor_index != @as(u32, @intCast(anchor_index))) continue;
            matching_source_count += 1;
            if (!source.hasCanonicalTags() or
                copied.receiver_component_ref.decodedKind() != .root_graph or
                copied.function_component_ref.decodedKind() != .root_graph or
                copied.receiver_component_ref.copy_step_index != function_copy_step or
                copied.receiver_component_ref.occurrence_offset != anchor.receiver_occurrence_offset or
                copied.function_component_ref.copy_step_index != function_copy_step or
                copied.function_component_ref.constraint_pair_offset != anchor.constraint_pair_offset)
            {
                std.debug.panic("binding-codec pair insertion found a crossed copied source", .{});
            }
        }
        if (matching_source_count != 1) {
            std.debug.panic("binding-codec copied anchor lacked one exact source producer", .{});
        }
    }
    for (sources) |source| {
        const copied = source.decodedCopiedConstraint() orelse continue;
        if (copied.function_component_ref.copy_step_index != function_copy_step or
            copied.function_component_ref.constraint_pair_offset < constraint_insert_offset_u32)
        {
            continue;
        }
        if (copied.anchor_index >= anchors.len or
            anchors[copied.anchor_index].copy_step != function_copy_step or
            anchors[copied.anchor_index].constraint_pair_offset !=
                copied.function_component_ref.constraint_pair_offset)
        {
            std.debug.panic("binding-codec copied source lacked its exact anchor inverse", .{});
        }
    }

    constraint_pool.appendAssumeCapacity(undefined);
    std.mem.copyBackwards(
        ModuleEnv.WhereMarkerConstraintCopyPair,
        constraint_pool.items[constraint_start + constraint_insert_offset + 1 ..],
        constraint_pool.items[constraint_start + constraint_insert_offset .. constraint_pool.items.len - 1],
    );
    constraint_pool.items[constraint_start + constraint_insert_offset] = .{
        .source_constraint_index = source_constraint_index,
        .destination_constraint_index = destination_constraint_index,
    };
    for (witness_pool.items[witness_start..]) |*witness| {
        if (witness.constraint_pair_offset != std.math.maxInt(u32) and
            witness.constraint_pair_offset >= constraint_insert_offset_u32)
        {
            witness.constraint_pair_offset += 1;
        }
    }
    for (dest_env.selected_receiver_anchors.items.items) |*anchor| {
        if (anchor.decodedKind() == .copied_constraint and
            anchor.copy_step == function_copy_step and
            anchor.constraint_pair_offset >= constraint_insert_offset_u32)
        {
            anchor.constraint_pair_offset += 1;
        }
    }
    for (dest_env.dispatch_settlement_sources.items.items) |*source| {
        const copied = source.decodedCopiedConstraint() orelse continue;
        if (copied.function_component_ref.copy_step_index == function_copy_step and
            copied.function_component_ref.constraint_pair_offset >= constraint_insert_offset_u32)
        {
            source.payload.copied_constraint.function_component_ref.constraint_pair_offset += 1;
        }
    }
    witness_pool.appendAssumeCapacity(undefined);
    std.mem.copyBackwards(
        ModuleEnv.WhereMarkerCopyWitness,
        witness_pool.items[witness_start + witness_insert_offset + 1 ..],
        witness_pool.items[witness_start + witness_insert_offset .. witness_pool.items.len - 1],
    );
    witness_pool.items[witness_start + witness_insert_offset] = ingress;

    step.constraint_pairs_len = std.math.add(u32, step.constraint_pairs_len, 1) catch
        std.debug.panic("binding-codec constraint proof length overflowed u32", .{});
    step.witnesses_len = std.math.add(u32, step.witnesses_len, 1) catch
        std.debug.panic("binding-codec witness proof length overflowed u32", .{});
    return .{
        .receiver_occurrence_offset = receiver_occurrence_offset,
        .function_occurrence_offset = function_occurrence_offset,
        .constraint_pair_offset = constraint_insert_offset_u32,
    };
}

fn recordProofPair(ctx: *CopyContext, source: Var, destination: Var) std.mem.Allocator.Error!void {
    if (ctx.nominal_decl_aux_depth != 0) return;
    const source_root = ctx.source_store.resolveVar(source).var_;
    const destination_root = ctx.dest_store.resolveVar(destination).var_;
    try ctx.scratch.proof_pairs.append(ctx.allocator, .{
        .source_var = @intFromEnum(source_root),
        .destination_var = @intFromEnum(destination_root),
        .discovery_depth = std.math.maxInt(u32),
        .predecessor_pair_offset = std.math.maxInt(u32),
        .predecessor_edge_ordinal = std.math.maxInt(u32),
    });
    try ctx.scratch.proof_occurrences.append(ctx.allocator, .{
        .raw_source_var = @intFromEnum(source),
        .raw_destination_var = @intFromEnum(destination),
        .canonical_pair_offset = std.math.maxInt(u32),
    });
}

const CopyEdgeLocator = struct {
    kind: ModuleEnv.WhereMarkerCopyWitness.EdgeKind,
    index: u32 = 0,
    name: u32 = 0,
    origin_module: u32 = 0,
    source_decl: u32 = 0,
    source_constraint_index: u32 = std.math.maxInt(u32),
};

fn exactProofPairAlreadyRecorded(
    ctx: *const CopyContext,
    source: Var,
    destination: Var,
) bool {
    const source_root = ctx.source_store.resolveVar(source).var_;
    const destination_root = ctx.dest_store.resolveVar(destination).var_;
    for (ctx.scratch.proof_pairs.items) |pair| {
        if (pair.source_var == @intFromEnum(source_root) and
            pair.destination_var == @intFromEnum(destination_root)) return true;
    }
    return false;
}

fn platformVarSubstitutionIndex(ctx: *const CopyContext, source: Var) ?u32 {
    const origin = ctx.marker_copy_origin orelse return null;
    const platform = switch (origin) {
        .platform_requirement => |payload| payload,
        else => return null,
    };
    const source_root = ctx.source_store.resolveVar(source).var_;
    var found: ?u32 = null;
    for (platform.substitutions, 0..) |binding, index| {
        const alias_root = ctx.source_store.resolveVar(binding.platform_alias_var).var_;
        const identity_root = ctx.source_store.resolveVar(binding.platform_identity_var).var_;
        if (source_root != alias_root and source_root != identity_root) continue;
        if (found != null and found.? != index) {
            std.debug.panic("one platform source root named two for-clause alias bindings", .{});
        }
        found = @intCast(index);
    }
    return found;
}

fn platformAliasSubstitutionIndex(ctx: *const CopyContext, source: AliasSource) ?u32 {
    const origin = ctx.marker_copy_origin orelse return null;
    const platform = switch (origin) {
        .platform_requirement => |payload| payload,
        else => return null,
    };
    var found: ?u32 = null;
    for (platform.substitutions, 0..) |binding, index| {
        if (!std.meta.eql(source, binding.platform_alias_source)) continue;
        if (found != null and found.? != index) {
            std.debug.panic("one platform alias source named two for-clause alias bindings", .{});
        }
        found = @intCast(index);
    }
    return found;
}

/// Return the exact platform substitution whose AliasSource replaces this
/// root, when the root copy takes the alias-substitution path rather than a
/// caller-preseeded `var_mapping` path. This is captured before `request`
/// memoizes the destination so the root cut cannot be confused with an
/// ordinary cycle/share revisit.
fn platformAliasPreseedForVar(ctx: *const CopyContext, source: Var) ?u32 {
    const content = ctx.source_store.resolveVar(source).desc.content;
    if (content != .alias) return null;
    const source_decl = content.alias.source_decl.toOptional() orelse return null;
    const alias_source = AliasSource{
        .origin_module = content.alias.origin_module,
        .source_decl = source_decl,
    };
    const mapping = ctx.alias_source_mapping orelse return null;
    if (mapping.get(alias_source) == null) return null;
    return platformAliasSubstitutionIndex(ctx, alias_source) orelse
        std.debug.panic("root alias substitution had no exact platform binding row", .{});
}

pub fn initBindingCodecMappingOrigins(
    allocator: std.mem.Allocator,
    dest_env: *const ModuleEnv,
    var_mapping: *const VarMapping,
    binding_root_step: u32,
) std.mem.Allocator.Error!BindingCodecMappingOrigins {
    if (binding_root_step >= dest_env.where_marker_copy_steps.items.items.len) {
        std.debug.panic("binding-codec mapping origins named an absent root step", .{});
    }
    const step = dest_env.where_marker_copy_steps.items.items[binding_root_step];
    if (step.decodedKind() == .binding_codec_receiver or
        step.decodedKind() == .binding_codec_function or
        step.decodedKind() == .reserved or
        step.decodedKind() == .discarded)
    {
        std.debug.panic("binding-codec mapping origins named an invalid root step", .{});
    }
    var origins = BindingCodecMappingOrigins.init(allocator);
    errdefer origins.deinit();
    try origins.ensureTotalCapacity(step.pairs_len);
    for (dest_env.where_marker_copy_pairs.items.items[step.pairs_start..][0..step.pairs_len], 0..) |pair, pair_offset| {
        const source: Var = @enumFromInt(pair.source_var);
        const destination: Var = @enumFromInt(pair.destination_var);
        if (var_mapping.get(source) != destination) {
            std.debug.panic("binding-root pair did not match its producer variable mapping", .{});
        }
        if (origins.get(source)) |prior| {
            if (prior.destination != destination) {
                std.debug.panic("binding-root copy mapped one source to two destinations", .{});
            }
            continue;
        }
        origins.putAssumeCapacityNoClobber(source, .{
            .destination = destination,
            .copy_step = binding_root_step,
            .pair_offset = @intCast(pair_offset),
        });
    }
    return origins;
}

fn recordBindingCodecStepMappingOrigins(
    transaction: *CrossModuleCopyTransaction,
    copy_step: u32,
) std.mem.Allocator.Error!void {
    const origins = transaction.binding_mapping_origins orelse
        std.debug.panic("binding-codec component publication omitted its mapping origins", .{});
    if (copy_step >= transaction.dest_env.where_marker_copy_steps.items.items.len) {
        std.debug.panic("binding-codec mapping origin named an absent component step", .{});
    }
    const step = transaction.dest_env.where_marker_copy_steps.items.items[copy_step];
    switch (step.decodedKind() orelse
        std.debug.panic("binding-codec mapping origin step had an invalid kind", .{})) {
        .binding_codec_receiver, .binding_codec_function => {},
        else => std.debug.panic("binding-codec mapping origin named a noncomponent step", .{}),
    }
    for (transaction.dest_env.where_marker_copy_pairs.items.items[step.pairs_start..][0..step.pairs_len], 0..) |pair, pair_offset| {
        const source: Var = @enumFromInt(pair.source_var);
        const destination: Var = @enumFromInt(pair.destination_var);
        if (transaction.var_mapping.get(source) != destination) {
            std.debug.panic("binding-codec component pair lost its variable mapping", .{});
        }
        if (origins.get(source)) |prior| {
            if (prior.destination != destination) {
                std.debug.panic("binding-codec components mapped one source to two destinations", .{});
            }
            continue;
        }
        try transaction.putNewBindingMappingOrigin(source, .{
            .destination = destination,
            .copy_step = copy_step,
            .pair_offset = @intCast(pair_offset),
        });
    }
}

fn bindingMappingOrigin(
    ctx: *const CopyContext,
    source: Var,
    destination: Var,
) ?BindingCodecMappingOrigin {
    const origin = ctx.marker_copy_origin orelse return null;
    _ = switch (origin) {
        .binding_codec_receiver, .binding_codec_function => {},
        else => return null,
    };
    const source_root = ctx.source_store.resolveVar(source).var_;
    const destination_root = ctx.dest_store.resolveVar(destination).var_;
    const origins = ctx.binding_mapping_origins orelse
        std.debug.panic("binding-codec copy omitted its producer mapping origins", .{});
    const mapping_origin = origins.get(source_root) orelse return null;
    if (mapping_origin.destination != destination_root) {
        std.debug.panic("binding-codec mapping origin disagreed with its variable mapping", .{});
    }
    if (mapping_origin.copy_step >= ctx.predicted_copy_step or
        mapping_origin.copy_step >= ctx.dest_env.where_marker_copy_steps.items.items.len)
    {
        std.debug.panic("binding-codec mapping origin did not precede its consumer step", .{});
    }
    const BindingCurrent = struct {
        kind: ModuleEnv.WhereMarkerCopyStep.Kind,
        binding: ModuleEnv.WhereMarkerBindingCodecOrigin,
    };
    const current: BindingCurrent = switch (origin) {
        .binding_codec_receiver => |binding| .{
            .kind = ModuleEnv.WhereMarkerCopyStep.Kind.binding_codec_receiver,
            .binding = binding,
        },
        .binding_codec_function => |binding| .{
            .kind = ModuleEnv.WhereMarkerCopyStep.Kind.binding_codec_function,
            .binding = binding,
        },
        else => unreachable,
    };
    const source_step = ctx.dest_env.where_marker_copy_steps.items.items[mapping_origin.copy_step];
    const source_allowed = if (mapping_origin.copy_step == current.binding.binding_root_step)
        true
    else switch (source_step.decodedKind() orelse
        std.debug.panic("binding-codec mapping origin step had an invalid kind", .{})) {
        .binding_codec_receiver => blk: {
            const source_binding = source_step.origin.binding_codec_receiver;
            break :blk source_binding.binding_root_step == current.binding.binding_root_step and
                (source_binding.requirement_ordinal < current.binding.requirement_ordinal or
                    (source_binding.requirement_ordinal == current.binding.requirement_ordinal and
                        current.kind == .binding_codec_function));
        },
        .binding_codec_function => blk: {
            const source_binding = source_step.origin.binding_codec_function;
            break :blk source_binding.binding_root_step == current.binding.binding_root_step and
                source_binding.requirement_ordinal < current.binding.requirement_ordinal;
        },
        else => false,
    };
    if (!source_allowed or mapping_origin.pair_offset >= source_step.pairs_len) {
        std.debug.panic("binding-codec mapping origin escaped its typed prior-component relation", .{});
    }
    const source_pair = ctx.dest_env.where_marker_copy_pairs.items.items[
        source_step.pairs_start + mapping_origin.pair_offset
    ];
    if (source_pair.source_var != @intFromEnum(source_root) or
        source_pair.destination_var != @intFromEnum(destination_root))
    {
        std.debug.panic("binding-codec mapping origin did not name its exact pair", .{});
    }
    return mapping_origin;
}

/// Record the finite authority for a root whose action is not otherwise named
/// by an outgoing semantic edge: either a structural leaf or a root already
/// mapped on entry. The root remains the sole depth-zero pair; this
/// self-addressed row is an authenticated action, not a traversable graph
/// edge.
fn recordRootActionWitness(
    ctx: *CopyContext,
    source: Var,
    destination: Var,
    prior_destination: ?Var,
    alias_preseed_index: ?u32,
) std.mem.Allocator.Error!void {
    const source_root = ctx.source_store.resolveVar(source).var_;
    const destination_root = ctx.dest_store.resolveVar(destination).var_;
    if (prior_destination == null and alias_preseed_index == null) {
        // A structural leaf has no outgoing witness that could otherwise
        // authenticate the root pair. Publish the finite traverse action as a
        // self-addressed root certificate; it is not a BFS predecessor.
        if (ctx.scratch.proof_witnesses.items.len != 0) return;
        try ctx.scratch.proof_witnesses.append(ctx.allocator, .{
            .parent_raw_source_var = @intFromEnum(source),
            .parent_raw_destination_var = @intFromEnum(destination),
            .child_raw_source_var = @intFromEnum(source),
            .child_raw_destination_var = @intFromEnum(destination),
            .edge_kind = .root_copy_action,
            .action = .traverse,
        });
        return;
    }

    if (prior_destination) |prior| {
        if (ctx.dest_store.resolveVar(prior).var_ != destination_root) {
            std.debug.panic("root preseed changed destination during copy", .{});
        }
    }

    var action: ModuleEnv.WhereMarkerCopyWitness.Action = undefined;
    var auxiliary_kind: ModuleEnv.WhereMarkerCopyWitness.AuxiliaryOriginKind = undefined;
    var auxiliary_step: u32 = 0;
    var auxiliary_index: u32 = undefined;
    if (alias_preseed_index orelse platformVarSubstitutionIndex(ctx, source_root)) |binding_index| {
        action = .platform_preseed_cut;
        auxiliary_kind = .platform_substitution;
        auxiliary_index = binding_index;
    } else if (bindingMappingOrigin(ctx, source_root, destination_root)) |mapping_origin| {
        action = .binding_codec_reuse_cut;
        auxiliary_kind = .binding_copy_pair;
        auxiliary_step = mapping_origin.copy_step;
        auxiliary_index = mapping_origin.pair_offset;
    } else {
        std.debug.panic("cross-module root used an unclassified preseed cut", .{});
    }

    try ctx.scratch.proof_witnesses.append(ctx.allocator, .{
        .parent_raw_source_var = @intFromEnum(source),
        .parent_raw_destination_var = @intFromEnum(destination),
        .child_raw_source_var = @intFromEnum(source),
        .child_raw_destination_var = @intFromEnum(destination),
        .edge_kind = .root_copy_action,
        .action = action,
        .auxiliary_origin_kind = auxiliary_kind,
        .auxiliary_origin_step = auxiliary_step,
        .auxiliary_origin_index = auxiliary_index,
    });
}

/// Request one semantic child and record the exact copy-time edge/cut before
/// suspending its parent frame. The destination mapping is installed by
/// `request` before it can return, even when the child itself needs a frame.
fn requestChild(
    ctx: *CopyContext,
    parent_source: Var,
    parent_destination: Var,
    child_source: Var,
    locator: CopyEdgeLocator,
) std.mem.Allocator.Error!bool {
    if (ctx.nominal_decl_aux_depth != 0) return request(ctx, child_source);

    const child_source_root = ctx.source_store.resolveVar(child_source).var_;
    const prior_destination = ctx.activeVarMapping().get(child_source_root);
    const already_in_step = if (prior_destination) |destination|
        exactProofPairAlreadyRecorded(ctx, child_source_root, destination)
    else
        false;
    const alias_preseed_index: ?u32 = blk: {
        const content = ctx.source_store.resolveVar(child_source_root).desc.content;
        if (content != .alias) break :blk null;
        const source_decl = content.alias.source_decl.toOptional() orelse break :blk null;
        const source = AliasSource{
            .origin_module = content.alias.origin_module,
            .source_decl = source_decl,
        };
        if (ctx.alias_source_mapping == null or ctx.alias_source_mapping.?.get(source) == null) {
            break :blk null;
        }
        break :blk platformAliasSubstitutionIndex(ctx, source) orelse
            std.debug.panic("alias-source preseed had no exact platform substitution row", .{});
    };
    const completed = try request(ctx, child_source);
    const child_destination = ctx.activeVarMapping().get(child_source_root) orelse
        std.debug.panic("copy request did not install its exact destination mapping", .{});

    var action: ModuleEnv.WhereMarkerCopyWitness.Action = .traverse;
    var auxiliary_kind: ModuleEnv.WhereMarkerCopyWitness.AuxiliaryOriginKind = .none;
    var auxiliary_step: u32 = 0;
    var auxiliary_index: u32 = 0;
    if (!already_in_step and (prior_destination != null or alias_preseed_index != null)) {
        if (alias_preseed_index orelse platformVarSubstitutionIndex(ctx, child_source_root)) |binding_index| {
            action = .platform_preseed_cut;
            auxiliary_kind = .platform_substitution;
            auxiliary_index = binding_index;
        } else if (bindingMappingOrigin(ctx, child_source_root, child_destination)) |mapping_origin| {
            action = .binding_codec_reuse_cut;
            auxiliary_kind = .binding_copy_pair;
            auxiliary_step = mapping_origin.copy_step;
            auxiliary_index = mapping_origin.pair_offset;
        } else {
            std.debug.panic("cross-module copy encountered an unclassified preseed cut", .{});
        }
    }

    try ctx.scratch.proof_witnesses.append(ctx.allocator, .{
        .parent_raw_source_var = @intFromEnum(parent_source),
        .parent_raw_destination_var = @intFromEnum(parent_destination),
        .child_raw_source_var = @intFromEnum(child_source),
        .child_raw_destination_var = @intFromEnum(child_destination),
        .edge_kind = locator.kind,
        .edge_index = locator.index,
        .edge_name = locator.name,
        .edge_origin_module = locator.origin_module,
        .edge_source_decl = locator.source_decl,
        .source_constraint_index = locator.source_constraint_index,
        .action = action,
        .auxiliary_origin_kind = auxiliary_kind,
        .auxiliary_origin_step = auxiliary_step,
        .auxiliary_origin_index = auxiliary_index,
    });
    return completed;
}

fn recordExercisedPlatformVarSubstitution(ctx: *CopyContext, source: Var) std.mem.Allocator.Error!void {
    const origin = ctx.marker_copy_origin orelse return;
    const platform = switch (origin) {
        .platform_requirement => |payload| payload,
        else => return,
    };
    const source_root = ctx.source_store.resolveVar(source).var_;
    var found: ?u32 = null;
    for (platform.substitutions, 0..) |binding, index| {
        const alias_root = ctx.source_store.resolveVar(binding.platform_alias_var).var_;
        const identity_root = ctx.source_store.resolveVar(binding.platform_identity_var).var_;
        if (source_root != alias_root and source_root != identity_root) continue;
        if (found != null and found.? != index) {
            std.debug.panic("one platform source root named two for-clause alias bindings", .{});
        }
        found = @intCast(index);
    }
    if (found) |index| try ctx.scratch.exercised_platform_substitutions.append(ctx.allocator, index);
}

fn recordExercisedPlatformAliasSubstitution(
    ctx: *CopyContext,
    source: AliasSource,
) std.mem.Allocator.Error!void {
    const origin = ctx.marker_copy_origin orelse return;
    const platform = switch (origin) {
        .platform_requirement => |payload| payload,
        else => return,
    };
    var found: ?u32 = null;
    for (platform.substitutions, 0..) |binding, index| {
        if (!std.meta.eql(source, binding.platform_alias_source)) continue;
        if (found != null and found.? != index) {
            std.debug.panic("one platform alias source named two for-clause alias bindings", .{});
        }
        found = @intCast(index);
    }
    const index = found orelse
        std.debug.panic("platform alias substitution had no exact for-clause binding", .{});
    try ctx.scratch.exercised_platform_substitutions.append(ctx.allocator, index);
}

fn publishMarkerCopyLineage(
    ctx: *CopyContext,
    source_var: Var,
    destination_var: Var,
) std.mem.Allocator.Error!u32 {
    const origin_input = ctx.marker_copy_origin orelse
        std.debug.panic("marker-bearing cross-module copy had no explicit origin", .{});
    if (ctx.dest_env.where_marker_copy_steps.items.items.len != ctx.predicted_copy_step) {
        std.debug.panic("nested marker copy changed the predicted lineage step index", .{});
    }

    const pairs = ctx.scratch.proof_pairs.items;
    std.mem.sortUnstable(ModuleEnv.WhereMarkerCopyPair, pairs, {}, whereMarkerCopyPairLessThan);
    var write: usize = 0;
    for (pairs) |pair| {
        if (write != 0 and
            pairs[write - 1].source_var == pair.source_var and
            pairs[write - 1].destination_var == pair.destination_var)
        {
            continue;
        }
        pairs[write] = pair;
        write += 1;
    }
    ctx.scratch.proof_pairs.items.len = write;
    if (write == 0 or write > std.math.maxInt(u32)) {
        std.debug.panic("marker copy produced an invalid complete pair count", .{});
    }
    const source_root = ctx.source_store.resolveVar(source_var).var_;
    const destination_root = ctx.dest_store.resolveVar(destination_var).var_;
    for (ctx.scratch.proof_pairs.items) |*pair| {
        pair.discovery_depth = if (pair.source_var == @intFromEnum(source_root) and
            pair.destination_var == @intFromEnum(destination_root))
            0
        else
            std.math.maxInt(u32);
        pair.predecessor_pair_offset = std.math.maxInt(u32);
        pair.predecessor_edge_ordinal = std.math.maxInt(u32);
    }
    const root_pair_found = for (ctx.scratch.proof_pairs.items) |pair| {
        if (pair.source_var == @intFromEnum(source_root) and
            pair.destination_var == @intFromEnum(destination_root)) break true;
    } else false;
    if (!root_pair_found) {
        std.debug.panic("marker copy proof omitted its exact root pair", .{});
    }

    const occurrences = ctx.scratch.proof_occurrences.items;
    for (occurrences) |*occurrence| {
        const raw_source: Var = @enumFromInt(occurrence.raw_source_var);
        const raw_destination: Var = @enumFromInt(occurrence.raw_destination_var);
        occurrence.canonical_pair_offset = exactMarkerCopyPairOffset(
            ctx.scratch.proof_pairs.items,
            @intFromEnum(ctx.source_store.resolveVar(raw_source).var_),
            @intFromEnum(ctx.dest_store.resolveVar(raw_destination).var_),
        );
    }
    std.mem.sortUnstable(
        ModuleEnv.WhereMarkerCopyOccurrence,
        occurrences,
        {},
        whereMarkerCopyOccurrenceLessThan,
    );
    if (!rawMarkerCopyOccurrencesAreFunctional(occurrences)) {
        std.debug.panic("one raw marker-copy source occurrence named two destinations", .{});
    }
    var occurrence_write: usize = 0;
    for (occurrences) |occurrence| {
        if (occurrence_write != 0 and
            occurrences[occurrence_write - 1].raw_source_var == occurrence.raw_source_var and
            occurrences[occurrence_write - 1].raw_destination_var == occurrence.raw_destination_var)
        {
            if (occurrences[occurrence_write - 1].canonical_pair_offset != occurrence.canonical_pair_offset) {
                std.debug.panic("one raw marker-copy occurrence resolved to two canonical pairs", .{});
            }
            continue;
        }
        occurrences[occurrence_write] = occurrence;
        occurrence_write += 1;
    }
    ctx.scratch.proof_occurrences.items.len = occurrence_write;
    if (occurrence_write == 0 or occurrence_write > std.math.maxInt(u32)) {
        std.debug.panic("marker copy produced an invalid raw occurrence count", .{});
    }
    const root_occurrence_offset = exactMarkerCopyOccurrenceOffset(
        ctx.scratch.proof_occurrences.items,
        @intFromEnum(source_var),
        @intFromEnum(destination_var),
    );

    const constraint_pairs = ctx.scratch.proof_constraint_pairs.items;
    std.mem.sortUnstable(
        ModuleEnv.WhereMarkerConstraintCopyPair,
        constraint_pairs,
        {},
        whereMarkerConstraintCopyPairLessThan,
    );
    var constraint_write: usize = 0;
    for (constraint_pairs) |pair| {
        if (constraint_write != 0 and
            constraint_pairs[constraint_write - 1].source_constraint_index == pair.source_constraint_index)
        {
            if (constraint_pairs[constraint_write - 1].destination_constraint_index !=
                pair.destination_constraint_index)
            {
                std.debug.panic("one source constraint copied to two destination occurrences", .{});
            }
            continue;
        }
        constraint_pairs[constraint_write] = pair;
        constraint_write += 1;
    }
    ctx.scratch.proof_constraint_pairs.items.len = constraint_write;

    const exercised = ctx.scratch.exercised_platform_substitutions.items;
    std.mem.sortUnstable(u32, exercised, {}, std.sort.asc(u32));
    var exercised_write: usize = 0;
    for (exercised) |index| {
        if (exercised_write != 0 and exercised[exercised_write - 1] == index) continue;
        exercised[exercised_write] = index;
        exercised_write += 1;
    }
    ctx.scratch.exercised_platform_substitutions.items.len = exercised_write;
    const substitutions_start: u32 = @intCast(ctx.dest_env.where_marker_platform_substitutions.items.items.len);
    const substitutions_len: u32 = switch (origin_input) {
        .platform_requirement => @intCast(exercised_write),
        else => blk: {
            if (exercised_write != 0) {
                std.debug.panic("non-platform marker copy recorded platform substitutions", .{});
            }
            break :blk 0;
        },
    };
    const origin = origin_input.encode(substitutions_start, substitutions_len);
    const copied_group_component = switch (origin_input) {
        .binding_codec_receiver => |binding| ModuleEnv.CopiedOpenLiteralComponent.bindingCodecReceiver(
            binding.binding_root_step,
            binding.requirement_ordinal,
        ),
        .binding_codec_function => |binding| ModuleEnv.CopiedOpenLiteralComponent.bindingCodecFunction(
            binding.binding_root_step,
            binding.requirement_ordinal,
        ),
        else => ModuleEnv.CopiedOpenLiteralComponent.rootGraph(),
    };

    var canonical_witnesses: std.ArrayList(ModuleEnv.WhereMarkerCopyWitness) = .empty;
    defer canonical_witnesses.deinit(ctx.allocator);
    try canonical_witnesses.ensureTotalCapacity(ctx.allocator, ctx.scratch.proof_witnesses.items.len);
    for (ctx.scratch.proof_witnesses.items) |pending| {
        var auxiliary_index = pending.auxiliary_origin_index;
        if (pending.auxiliary_origin_kind == .platform_substitution) {
            var found: ?u32 = null;
            for (ctx.scratch.exercised_platform_substitutions.items, 0..) |input_index, output_offset| {
                if (input_index != pending.auxiliary_origin_index) continue;
                found = @intCast(output_offset);
                break;
            }
            auxiliary_index = found orelse
                std.debug.panic("platform cut witness named an unretained exercised substitution", .{});
        }
        const constraint_pair_offset = if (pending.source_constraint_index == std.math.maxInt(u32))
            std.math.maxInt(u32)
        else
            exactConstraintCopyPairOffset(
                ctx.scratch.proof_constraint_pairs.items,
                pending.source_constraint_index,
            );
        canonical_witnesses.appendAssumeCapacity(.{
            .parent_occurrence_offset = exactMarkerCopyOccurrenceOffset(
                ctx.scratch.proof_occurrences.items,
                pending.parent_raw_source_var,
                pending.parent_raw_destination_var,
            ),
            .child_occurrence_offset = exactMarkerCopyOccurrenceOffset(
                ctx.scratch.proof_occurrences.items,
                pending.child_raw_source_var,
                pending.child_raw_destination_var,
            ),
            .edge_kind = @intFromEnum(pending.edge_kind),
            .edge_index = pending.edge_index,
            .edge_name = pending.edge_name,
            .edge_origin_module = pending.edge_origin_module,
            .edge_source_decl = pending.edge_source_decl,
            .constraint_pair_offset = constraint_pair_offset,
            .action = @intFromEnum(pending.action),
            .auxiliary_origin_kind = @intFromEnum(pending.auxiliary_origin_kind),
            .auxiliary_origin_step = pending.auxiliary_origin_step,
            .auxiliary_origin_index = auxiliary_index,
            .raw_source_var = pending.raw_source_var,
            .raw_destination_var = pending.raw_destination_var,
        });
    }
    std.mem.sortUnstable(
        ModuleEnv.WhereMarkerCopyWitness,
        canonical_witnesses.items,
        {},
        whereMarkerCopyWitnessLessThan,
    );
    for (canonical_witnesses.items, 0..) |witness, witness_index| {
        if (witness_index != 0 and std.meta.eql(canonical_witnesses.items[witness_index - 1], witness)) {
            std.debug.panic("marker-copy proof repeated one semantic edge witness", .{});
        }
    }
    for (ctx.scratch.proof_constraint_pairs.items, 0..) |_, constraint_pair_offset| {
        var matching_witness_count: usize = 0;
        for (canonical_witnesses.items) |witness| {
            if (witness.decodedEdgeKind() == .static_dispatch_function and
                witness.constraint_pair_offset == constraint_pair_offset)
            {
                matching_witness_count += 1;
            }
        }
        if (matching_witness_count != 1) {
            std.debug.panic("constraint-copy pair lacked one exact static-dispatch witness", .{});
        }
    }

    // A cross-module constraint namespace is disjoint from the source even
    // when its next numeric index happens to be equal. Every exact static-
    // dispatch witness therefore authors one fresh destination-local source,
    // anchor, and primary evidence handle. Keep the exact source/destination
    // pair while both stores are present; this is also the only sound input to
    // the copied-open-literal inventory below.
    var copied_source_drafts: std.ArrayList(CopiedConstraintSourceDraft) = .empty;
    defer copied_source_drafts.deinit(ctx.allocator);
    try copied_source_drafts.ensureTotalCapacity(ctx.allocator, canonical_witnesses.items.len);
    for (canonical_witnesses.items, 0..) |witness, witness_index| {
        if (witness.decodedEdgeKind() != .static_dispatch_function) continue;
        if (witness.parent_occurrence_offset >= ctx.scratch.proof_occurrences.items.len or
            witness.child_occurrence_offset >= ctx.scratch.proof_occurrences.items.len or
            witness.constraint_pair_offset >= ctx.scratch.proof_constraint_pairs.items.len)
        {
            std.debug.panic("copied-constraint source escaped its exact witness relation", .{});
        }
        const constraint_pair = ctx.scratch.proof_constraint_pairs.items[witness.constraint_pair_offset];
        if (constraint_pair.source_constraint_index >= ctx.source_store.static_dispatch_constraints.items.items.len or
            constraint_pair.destination_constraint_index >= ctx.dest_store.static_dispatch_constraints.items.items.len)
        {
            std.debug.panic("copied-constraint source named an absent constraint occurrence", .{});
        }
        const receiver_occurrence = ctx.scratch.proof_occurrences.items[witness.parent_occurrence_offset];
        const function_occurrence = ctx.scratch.proof_occurrences.items[witness.child_occurrence_offset];
        if (receiver_occurrence.raw_source_var >= ctx.source_store.len() or
            receiver_occurrence.raw_destination_var >= ctx.dest_store.len() or
            function_occurrence.raw_source_var >= ctx.source_store.len() or
            function_occurrence.raw_destination_var >= ctx.dest_store.len())
        {
            std.debug.panic("copied-constraint source named an absent copy occurrence", .{});
        }
        const source_constraint = ctx.source_store.static_dispatch_constraints.items.items[
            constraint_pair.source_constraint_index
        ];
        const destination_constraint = ctx.dest_store.static_dispatch_constraints.items.items[
            constraint_pair.destination_constraint_index
        ];
        if (@intFromEnum(source_constraint.fn_var) != function_occurrence.raw_source_var or
            @intFromEnum(destination_constraint.fn_var) != function_occurrence.raw_destination_var)
        {
            std.debug.panic("copied-constraint function reference did not match its exact constraint pair", .{});
        }
        if (destination_constraint.constraint_evidence.start != 0 or
            destination_constraint.constraint_evidence.len != 0)
        {
            std.debug.panic("copied constraint retained foreign or duplicate receiver provenance", .{});
        }
        for (canonical_witnesses.items[0..witness_index]) |prior_witness| {
            if (prior_witness.decodedEdgeKind() != .static_dispatch_function) continue;
            const prior_pair = ctx.scratch.proof_constraint_pairs.items[prior_witness.constraint_pair_offset];
            if (prior_pair.destination_constraint_index == constraint_pair.destination_constraint_index) {
                std.debug.panic("two static-dispatch witnesses named one new destination constraint", .{});
            }
        }
        copied_source_drafts.appendAssumeCapacity(.{
            .source_constraint_index = constraint_pair.source_constraint_index,
            .destination_constraint_index = constraint_pair.destination_constraint_index,
            .receiver_occurrence_offset = witness.parent_occurrence_offset,
            .function_occurrence_offset = witness.child_occurrence_offset,
            .constraint_pair_offset = witness.constraint_pair_offset,
        });
    }
    const copied_constraint_count = copied_source_drafts.items.len;
    if (copied_constraint_count > std.math.maxInt(u32)) {
        std.debug.panic("copied-constraint source count overflowed u32", .{});
    }
    if (copied_constraint_count != 0) {
        if (ctx.dest_env.selected_receiver_anchors.items.items.len >= std.math.maxInt(u32) or
            copied_constraint_count > std.math.maxInt(u32) - ctx.dest_env.selected_receiver_anchors.items.items.len)
        {
            std.debug.panic("copied-constraint publication indexes overflowed u32", .{});
        }
        const anchor_start: u32 = @intCast(ctx.dest_env.selected_receiver_anchors.items.items.len);
        var source_offset: u32 = 0;
        for (canonical_witnesses.items) |witness| {
            if (witness.decodedEdgeKind() != .static_dispatch_function) continue;
            const source = ModuleEnv.DispatchSettlementSource.copiedConstraint(
                std.math.add(u32, anchor_start, source_offset) catch
                    std.debug.panic("copied-constraint anchor index overflowed u32", .{}),
                ModuleEnv.CopiedConstraintComponentRef.rootGraphReceiver(
                    ctx.predicted_copy_step,
                    witness.parent_occurrence_offset,
                ),
                ModuleEnv.CopiedConstraintComponentRef.rootGraphFunction(
                    ctx.predicted_copy_step,
                    witness.child_occurrence_offset,
                    witness.constraint_pair_offset,
                ),
            );
            if (!source.hasCanonicalTags()) {
                std.debug.panic("cross-module copy built a noncanonical settlement source", .{});
            }
            source_offset += 1;
        }
    }

    // Freeze one receiver-level group for every genuinely fresh cross-module
    // receiver which copied at least one literal-conversion constraint. A
    // binding-codec graph component carries its exact component identity;
    // only the detached outer ingress is categorically nonliteral.
    var grouped_source_drafts: std.ArrayList(CopiedConstraintSourceDraft) = .empty;
    defer grouped_source_drafts.deinit(ctx.allocator);
    try grouped_source_drafts.ensureTotalCapacity(ctx.allocator, copied_source_drafts.items.len);
    grouped_source_drafts.appendSliceAssumeCapacity(copied_source_drafts.items);
    std.mem.sortUnstable(
        CopiedConstraintSourceDraft,
        grouped_source_drafts.items,
        {},
        copiedConstraintSourceDraftLessThan,
    );

    var copied_group_drafts: std.ArrayList(CopiedOpenLiteralGroupDraft) = .empty;
    defer copied_group_drafts.deinit(ctx.allocator);
    var copied_event_drafts: std.ArrayList(CopiedOpenLiteralEventDraft) = .empty;
    defer copied_event_drafts.deinit(ctx.allocator);
    try copied_group_drafts.ensureTotalCapacity(ctx.allocator, grouped_source_drafts.items.len);
    try copied_event_drafts.ensureTotalCapacity(ctx.allocator, grouped_source_drafts.items.len);

    var group_source_start: usize = 0;
    while (group_source_start < grouped_source_drafts.items.len) {
        const first = grouped_source_drafts.items[group_source_start];
        var group_source_end = group_source_start + 1;
        while (group_source_end < grouped_source_drafts.items.len and
            grouped_source_drafts.items[group_source_end].receiver_occurrence_offset ==
                first.receiver_occurrence_offset)
        {
            group_source_end += 1;
        }
        const group_sources = grouped_source_drafts.items[group_source_start..group_source_end];
        if (first.receiver_occurrence_offset >= ctx.scratch.proof_occurrences.items.len) {
            std.debug.panic("copied literal receiver escaped its exact occurrence relation", .{});
        }
        const receiver_occurrence = ctx.scratch.proof_occurrences.items[
            first.receiver_occurrence_offset
        ];
        // Source and destination variable numbers inhabit disjoint
        // namespaces, so numeric inequality cannot establish freshness.
        // The transaction's destination slot baseline is the exact owner
        // boundary for every placeholder minted by this public copy.
        if (receiver_occurrence.raw_destination_var < ctx.transaction.store_savepoint.baseline_slots) {
            std.debug.panic("copied constraint source used a non-fresh receiver occurrence", .{});
        }

        const source_constraints_start = first.source_constraint_index;
        const destination_constraints_start = first.destination_constraint_index;
        for (group_sources, 0..) |draft, raw_offset| {
            const offset: u32 = @intCast(raw_offset);
            const expected_source = std.math.add(u32, source_constraints_start, offset) catch
                std.debug.panic("copied literal source range overflowed u32", .{});
            const expected_destination = std.math.add(u32, destination_constraints_start, offset) catch
                std.debug.panic("copied literal destination range overflowed u32", .{});
            if (draft.source_constraint_index != expected_source or
                draft.destination_constraint_index != expected_destination or
                draft.constraint_pair_offset >= ctx.scratch.proof_constraint_pairs.items.len)
            {
                std.debug.panic("one copied receiver did not own complete contiguous constraint ranges", .{});
            }
            const exact_pair = ctx.scratch.proof_constraint_pairs.items[draft.constraint_pair_offset];
            if (exact_pair.source_constraint_index != draft.source_constraint_index or
                exact_pair.destination_constraint_index != draft.destination_constraint_index)
            {
                std.debug.panic("copied literal draft did not name its exact constraint pair", .{});
            }
        }

        const event_start: u32 = @intCast(copied_event_drafts.items.len);
        for (group_sources, 0..) |draft, raw_offset| {
            const source_constraint = ctx.source_store.static_dispatch_constraints.items.items[
                draft.source_constraint_index
            ];
            const literal_kind = literal_defaulting.constraintLiteralKind(.{
                .from_numeral = ctx.source_env.idents.from_numeral,
                .from_quote = ctx.source_env.idents.from_quote,
                .from_interpolation = ctx.source_env.idents.from_interpolation,
            }, source_constraint) orelse continue;
            copied_event_drafts.appendAssumeCapacity(.{
                .destination_constraint_index = draft.destination_constraint_index,
                .constraint_offset = @intCast(raw_offset),
                .literal_kind = copiedOpenLiteralKind(literal_kind),
            });
        }
        const events_len: u32 = @intCast(copied_event_drafts.items.len - event_start);
        if (events_len != 0) {
            copied_group_drafts.appendAssumeCapacity(.{
                .receiver_occurrence_offset = first.receiver_occurrence_offset,
                .source_constraints_start = source_constraints_start,
                .source_constraints_len = @intCast(group_sources.len),
                .destination_constraints_start = destination_constraints_start,
                .destination_constraints_len = @intCast(group_sources.len),
                .events_start = event_start,
                .events_len = events_len,
            });
        }
        group_source_start = group_source_end;
    }

    const evidence_handle_count = std.math.add(
        usize,
        copied_constraint_count,
        copied_event_drafts.items.len,
    ) catch return error.OutOfMemory;
    const copied_group_start = std.math.cast(
        u32,
        ctx.dest_env.copied_open_literal_groups.items.items.len,
    ) orelse return error.OutOfMemory;
    const copied_event_start = std.math.cast(
        u32,
        ctx.dest_env.copied_open_literal_events.items.items.len,
    ) orelse return error.OutOfMemory;
    _ = std.math.add(u32, copied_group_start, @intCast(copied_group_drafts.items.len)) catch
        return error.OutOfMemory;
    _ = std.math.add(u32, copied_event_start, @intCast(copied_event_drafts.items.len)) catch
        return error.OutOfMemory;
    if (ctx.dest_store.constraint_evidence_handles.items.items.len > std.math.maxInt(u32) or
        evidence_handle_count > std.math.maxInt(u32) - ctx.dest_store.constraint_evidence_handles.items.items.len)
    {
        return error.OutOfMemory;
    }

    // Reserve every durable pool before the first append. From this point
    // publication is infallible; any preceding scratch or reserve OOM is
    // wholly owned by the enclosing cross-module transaction.
    try ctx.dest_env.where_marker_copy_pairs.items.ensureUnusedCapacity(ctx.allocator, write);
    try ctx.dest_env.where_marker_copy_occurrences.items.ensureUnusedCapacity(
        ctx.allocator,
        occurrence_write,
    );
    try ctx.dest_env.where_marker_constraint_copy_pairs.items.ensureUnusedCapacity(
        ctx.allocator,
        constraint_write,
    );
    try ctx.dest_env.where_marker_copy_witnesses.items.ensureUnusedCapacity(
        ctx.allocator,
        canonical_witnesses.items.len,
    );
    try ctx.dest_env.where_marker_platform_substitutions.items.ensureUnusedCapacity(
        ctx.allocator,
        exercised_write,
    );
    try ctx.dest_env.copied_open_literal_groups.items.ensureUnusedCapacity(
        ctx.allocator,
        copied_group_drafts.items.len,
    );
    try ctx.dest_env.copied_open_literal_events.items.ensureUnusedCapacity(
        ctx.allocator,
        copied_event_drafts.items.len,
    );
    try ctx.dest_env.where_marker_copy_steps.items.ensureUnusedCapacity(ctx.allocator, 1);
    try ctx.dest_env.selected_receiver_anchors.items.ensureUnusedCapacity(
        ctx.allocator,
        copied_constraint_count,
    );
    try ctx.dest_env.dispatch_settlement_sources.items.ensureUnusedCapacity(
        ctx.allocator,
        copied_constraint_count,
    );
    try ctx.dest_store.constraint_evidence_handles.items.ensureUnusedCapacity(
        ctx.allocator,
        evidence_handle_count,
    );
    const pair_start: u32 = @intCast(ctx.dest_env.where_marker_copy_pairs.items.items.len);
    const occurrence_start: u32 = @intCast(
        ctx.dest_env.where_marker_copy_occurrences.items.items.len,
    );
    const constraint_pair_start: u32 = @intCast(
        ctx.dest_env.where_marker_constraint_copy_pairs.items.items.len,
    );
    const witness_start: u32 = @intCast(ctx.dest_env.where_marker_copy_witnesses.items.items.len);
    ctx.dest_env.where_marker_copy_pairs.items.appendSliceAssumeCapacity(
        ctx.scratch.proof_pairs.items,
    );
    ctx.dest_env.where_marker_copy_occurrences.items.appendSliceAssumeCapacity(
        ctx.scratch.proof_occurrences.items,
    );
    ctx.dest_env.where_marker_constraint_copy_pairs.items.appendSliceAssumeCapacity(
        ctx.scratch.proof_constraint_pairs.items,
    );
    ctx.dest_env.where_marker_copy_witnesses.items.appendSliceAssumeCapacity(
        canonical_witnesses.items,
    );
    switch (origin_input) {
        .platform_requirement => |platform| {
            for (ctx.scratch.exercised_platform_substitutions.items) |binding_index| {
                if (binding_index >= platform.substitutions.len) {
                    std.debug.panic("platform substitution index escaped its producer input", .{});
                }
                const binding = platform.substitutions[binding_index];
                ctx.dest_env.where_marker_platform_substitutions.items.appendAssumeCapacity(.{
                    .platform_alias_statement = binding.platform_alias_statement,
                    .app_declaration_node = binding.app_declaration_node,
                    .app_instantiation_step = binding.app_instantiation_step,
                });
            }
        },
        else => {},
    }
    for (copied_group_drafts.items) |group_draft| {
        const group_index: u32 = @intCast(ctx.dest_env.copied_open_literal_groups.items.items.len);
        const events_start = std.math.add(u32, copied_event_start, group_draft.events_start) catch
            unreachable;
        ctx.dest_env.copied_open_literal_groups.items.appendAssumeCapacity(.{
            .copy_step_index = ctx.predicted_copy_step,
            .receiver_occurrence_offset = group_draft.receiver_occurrence_offset,
            .source_constraints_start = group_draft.source_constraints_start,
            .source_constraints_len = group_draft.source_constraints_len,
            .destination_constraints_start = group_draft.destination_constraints_start,
            .destination_constraints_len = group_draft.destination_constraints_len,
            .component = copied_group_component,
            .events_start = events_start,
            .events_len = group_draft.events_len,
        });
        for (copied_event_drafts.items[group_draft.events_start..][0..group_draft.events_len]) |event_draft| {
            ctx.dest_env.copied_open_literal_events.items.appendAssumeCapacity(.{
                .group_index = group_index,
                .constraint_offset = event_draft.constraint_offset,
                .literal_kind = @intFromEnum(event_draft.literal_kind),
            });
        }
    }
    ctx.dest_env.where_marker_copy_steps.items.appendAssumeCapacity(.{
        .kind = @intFromEnum(origin.kind),
        .copy_policy = @intFromEnum(ModuleEnv.WhereMarkerCopyStep.CopyPolicy.cross_module_import),
        .source_root_var = @intFromEnum(source_root),
        .destination_root_var = @intFromEnum(destination_root),
        .pairs_start = pair_start,
        .pairs_len = @intCast(write),
        .occurrences_start = occurrence_start,
        .occurrences_len = @intCast(occurrence_write),
        .root_occurrence_offset = root_occurrence_offset,
        .constraint_pairs_start = constraint_pair_start,
        .constraint_pairs_len = @intCast(constraint_write),
        .witnesses_start = witness_start,
        .witnesses_len = @intCast(canonical_witnesses.items.len),
        .copied_groups_start = copied_group_start,
        .copied_groups_len = @intCast(copied_group_drafts.items.len),
        .origin = origin.origin,
    });
    for (canonical_witnesses.items) |witness| {
        if (witness.decodedEdgeKind() != .static_dispatch_function) continue;
        const constraint_pair = ctx.scratch.proof_constraint_pairs.items[witness.constraint_pair_offset];
        const receiver_occurrence = ctx.scratch.proof_occurrences.items[witness.parent_occurrence_offset];
        const anchor_index: u32 = @intCast(ctx.dest_env.selected_receiver_anchors.items.items.len);
        const handle_start: u32 = @intCast(ctx.dest_store.constraint_evidence_handles.items.items.len);
        ctx.dest_env.selected_receiver_anchors.items.appendAssumeCapacity(.{
            .constraint_index = constraint_pair.destination_constraint_index,
            .receiver_var = receiver_occurrence.raw_destination_var,
            .kind = @intFromEnum(ModuleEnv.SelectedMethodDecision.ReceiverAnchorKind.copied_constraint),
            .node = ModuleEnv.SelectedMethodDecision.none,
            .slot = ModuleEnv.SelectedMethodDecision.none,
            .copy_step = ctx.predicted_copy_step,
            .receiver_occurrence_offset = witness.parent_occurrence_offset,
            .constraint_pair_offset = witness.constraint_pair_offset,
        });
        ctx.dest_store.constraint_evidence_handles.items.appendAssumeCapacity(.{
            .kind = @intFromEnum(types_mod.ConstraintEvidenceHandle.Kind.selected_receiver_anchor),
            .index = anchor_index,
        });
        var copied_event_index: ?u32 = null;
        for (copied_event_drafts.items, 0..) |event_draft, event_offset| {
            if (event_draft.destination_constraint_index != constraint_pair.destination_constraint_index) {
                continue;
            }
            if (copied_event_index != null) {
                std.debug.panic("one copied destination constraint authored two literal events", .{});
            }
            copied_event_index = std.math.add(u32, copied_event_start, @intCast(event_offset)) catch
                unreachable;
        }
        if (copied_event_index) |event_index| {
            ctx.dest_store.constraint_evidence_handles.items.appendAssumeCapacity(.{
                .kind = @intFromEnum(types_mod.ConstraintEvidenceHandle.Kind.copied_literal_event),
                .index = event_index,
            });
        }
        ctx.dest_env.dispatch_settlement_sources.items.appendAssumeCapacity(
            ModuleEnv.DispatchSettlementSource.copiedConstraint(
                anchor_index,
                ModuleEnv.CopiedConstraintComponentRef.rootGraphReceiver(
                    ctx.predicted_copy_step,
                    witness.parent_occurrence_offset,
                ),
                ModuleEnv.CopiedConstraintComponentRef.rootGraphFunction(
                    ctx.predicted_copy_step,
                    witness.child_occurrence_offset,
                    witness.constraint_pair_offset,
                ),
            ),
        );
        ctx.dest_store.static_dispatch_constraints.items.items[constraint_pair.destination_constraint_index]
            .constraint_evidence = .{
            .start = handle_start,
            .len = if (copied_event_index == null) 1 else 2,
        };
    }
    return ctx.predicted_copy_step;
}

/// Run every frame above `frames_base` to completion.
fn drive(ctx: *CopyContext, frames_base: usize) std.mem.Allocator.Error!void {
    const machine = &ctx.scratch;
    while (machine.frames.items.len > frames_base) {
        const top = &machine.frames.items[machine.frames.items.len - 1];
        // A step either suspends after requesting exactly one child (having
        // already written its own resume state), or finishes without
        // requesting anything—so popping on finish always removes the frame
        // the step ran for.
        const finished = switch (top.*) {
            .alias_substitution => |*frame| try stepAliasSubstitution(ctx, frame),
            .identity => |*frame| try stepIdentity(ctx, frame),
            .alias => |*frame| try stepAlias(ctx, frame),
            .tuple => |*frame| try stepTuple(ctx, frame),
            .nominal => |*frame| try stepNominal(ctx, frame),
            .nominal_decl => |*frame| try stepNominalDecl(ctx, frame),
            .func => |*frame| try stepFunc(ctx, frame),
            .record => |*frame| try stepRecord(ctx, frame),
            .record_unbound => |*frame| try stepRecordUnbound(ctx, frame),
            .tag_union => |*frame| try stepTagUnion(ctx, frame),
        };
        if (finished) {
            machine.frames.items.len -= 1;
        }
    }
}

/// Copy the head of one source var: reuse an existing mapping, and otherwise
/// mint + register the placeholder and either fill it immediately (contents
/// with no children) or push the frame that will fill it. Returns true when
/// the result var is already on the value stack; false when a frame was
/// pushed.
fn request(ctx: *CopyContext, source_var: Var) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    const resolved = ctx.source_store.resolveVar(source_var);

    const var_mapping = ctx.activeVarMapping();
    if (var_mapping.get(resolved.var_)) |dest_var| {
        if (ctx.nominal_decl_aux_depth == 0) {
            try recordExercisedPlatformVarSubstitution(ctx, resolved.var_);
        }
        try recordProofPair(ctx, source_var, dest_var);
        try machine.values.append(ctx.allocator, dest_var);
        return true;
    }

    if (resolved.desc.content == .alias) {
        const source_alias = resolved.desc.content.alias;
        if (source_alias.source_decl.toOptional()) |source_decl| {
            const alias_source = AliasSource{
                .origin_module = source_alias.origin_module,
                .source_decl = source_decl,
            };
            if (if (ctx.nominal_decl_aux_depth == 0)
                if (ctx.alias_source_mapping) |mapping| mapping.get(alias_source) else null
            else
                null) |dest_var|
            {
                try recordExercisedPlatformAliasSubstitution(ctx, alias_source);
                // Memoize before visiting children so recursive source graphs
                // terminate. The replacement drops the source alias payload,
                // but its children must still be copied/memoized because they
                // are independently recorded platform identity slots.
                try ctx.putNewMapping(resolved.var_, dest_var);
                try recordProofPair(ctx, source_var, dest_var);
                try machine.frames.append(ctx.allocator, .{ .alias_substitution = .{
                    .source_var = source_var,
                    .source = source_alias,
                    .dest_var = dest_var,
                    .backing = ctx.source_store.getAliasBackingVar(source_alias),
                    .args = ctx.source_store.sliceAliasArgs(source_alias),
                    .values_base = @intCast(machine.values.items.len),
                } });
                return false;
            }
        }
    }

    const placeholder_var = try ctx.dest_store.fresh();
    try ctx.putNewMapping(resolved.var_, placeholder_var);
    try recordProofPair(ctx, source_var, placeholder_var);

    const fill = Fill{
        .source_var = source_var,
        .placeholder = placeholder_var,
        .empty_tag_union_is_default = resolved.desc.flags.empty_tag_union_is_default,
    };

    // NOTE: a copied var whose content is a flex carrying a literal-conversion
    // constraint is an open literal in the destination module. Registering it on
    // the checker's open-literal worklist is the CALLER's job (see `Check.copyVar`,
    // which post-processes the destination store's allocation range)—this
    // module only copies type data between stores.
    return try pushContent(ctx, fill, resolved.desc.content);
}

/// Push the frame that copies `content`'s children, or fill the placeholder
/// outright when it has none. Returns true when the placeholder is already on
/// the value stack.
fn pushContent(ctx: *CopyContext, fill: Fill, content: Content) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    switch (content) {
        .err => {
            try finishFrame(ctx, fill, Content.err);
            return true;
        },
        .flex => |flex| {
            const translated_name = if (flex.name) |name_ident|
                try ctx.copyIdent(name_ident)
            else
                null;
            return try pushIdentity(ctx, fill, .flex, translated_name, flex.constraints);
        },
        .rigid => |rigid| {
            const translated_name = try ctx.copyIdent(rigid.name);
            return try pushIdentity(ctx, fill, .rigid, translated_name, rigid.constraints);
        },
        .alias => |alias| {
            const translated_ident = try ctx.copyIdent(alias.ident.ident_idx);
            try machine.frames.append(ctx.allocator, .{ .alias = .{
                .fill = fill,
                .source = alias,
                .translated_ident = translated_ident,
                .backing = ctx.source_store.getAliasBackingVar(alias),
                .args = ctx.source_store.sliceAliasArgs(alias),
                .values_base = @intCast(machine.values.items.len),
            } });
            return false;
        },
        .field_presence => |field_presence| {
            const copied_presence = switch (field_presence) {
                .required, .optional => field_presence,
                .defaulted => |id| types_mod.FieldPresence{ .defaulted = .{
                    .origin_module = try ctx.copyOriginModule(id.origin_module),
                    .expr_node = id.expr_node,
                } },
            };
            try finishFrame(ctx, fill, .{ .field_presence = copied_presence });
            return true;
        },
        .structure => |flat_type| switch (flat_type) {
            .empty_record => {
                try finishFrame(ctx, fill, Content{ .structure = FlatType.empty_record });
                return true;
            },
            .empty_tag_union => {
                try finishFrame(ctx, fill, Content{ .structure = FlatType.empty_tag_union });
                return true;
            },
            .tuple => |tuple| {
                try machine.frames.append(ctx.allocator, .{ .tuple = .{
                    .fill = fill,
                    .elems = ctx.source_store.sliceVars(tuple.elems),
                    .values_base = @intCast(machine.values.items.len),
                } });
                return false;
            },
            .nominal_type => |nominal| {
                const translated_ident = try ctx.copyIdent(nominal.ident.ident_idx);
                const translated_origin = try ctx.copyOriginModule(nominal.origin_module);
                try machine.frames.append(ctx.allocator, .{ .nominal = .{
                    .fill = fill,
                    .source = nominal,
                    .translated_ident = translated_ident,
                    .translated_origin = translated_origin,
                    .args = ctx.source_store.sliceNominalArgs(nominal),
                    .values_base = @intCast(machine.values.items.len),
                } });
                return false;
            },
            .fn_pure => |func| return try pushFunc(ctx, fill, .pure, func),
            .fn_effectful => |func| return try pushFunc(ctx, fill, .effectful, func),
            .fn_unbound => |func| return try pushFunc(ctx, fill, .unbound, func),
            .record => |record| {
                try machine.frames.append(ctx.allocator, .{ .record = .{
                    .fill = fill,
                    .source_fields = record.fields,
                    .ext = record.ext,
                    .fields_base = @intCast(machine.pending_fields.items.len),
                    .values_base = @intCast(machine.values.items.len),
                } });
                return false;
            },
            .record_unbound => |fields| {
                try machine.frames.append(ctx.allocator, .{ .record_unbound = .{
                    .fill = fill,
                    .source_fields = fields,
                    .fields_base = @intCast(machine.pending_fields.items.len),
                    .values_base = @intCast(machine.values.items.len),
                } });
                return false;
            },
            .tag_union => |tag_union| {
                try machine.frames.append(ctx.allocator, .{ .tag_union = .{
                    .fill = fill,
                    .source_tags = tag_union.tags,
                    .ext = tag_union.ext,
                    .values_base = @intCast(machine.values.items.len),
                    .tags_base = @intCast(machine.pending_tags.items.len),
                } });
                return false;
            },
        },
    }
}

fn pushIdentity(
    ctx: *CopyContext,
    fill: Fill,
    result: IdentityResult,
    name: ?base.Ident.Idx,
    constraints: StaticDispatchConstraint.SafeList.Range,
) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    if (constraints.len() == 0) {
        const empty = StaticDispatchConstraint.SafeList.Range.empty();
        const content: Content = switch (result) {
            .flex => Content{ .flex = Flex{ .name = name, .constraints = empty } },
            .rigid => Content{ .rigid = Rigid{ .name = name.?, .constraints = empty } },
        };
        try finishFrame(ctx, fill, content);
        return true;
    }
    try machine.frames.append(ctx.allocator, .{ .identity = .{
        .fill = fill,
        .result = result,
        .name = name,
        .source_constraints = ctx.source_store.sliceStaticDispatchConstraints(constraints),
        .source_constraints_start = @intFromEnum(constraints.start),
        .cons_base = @intCast(machine.pending_constraints.items.len),
    } });
    return false;
}

fn pushFunc(
    ctx: *CopyContext,
    fill: Fill,
    kind: FuncKind,
    func: Func,
) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    try machine.frames.append(ctx.allocator, .{ .func = .{
        .fill = fill,
        .kind = kind,
        .args = ctx.source_store.sliceVars(func.args),
        .ret = func.ret,
        .effect_deps = ctx.source_store.sliceVars(func.effect_deps),
        .values_base = @intCast(machine.values.items.len),
    } });
    return false;
}

fn finishFrame(ctx: *CopyContext, fill: Fill, content: Content) std.mem.Allocator.Error!void {
    try ctx.dest_store.dangerousSetVarDesc(fill.placeholder, .{
        .content = content,
        .rank = types_mod.Rank.generalized,
        .flags = .{ .empty_tag_union_is_default = fill.empty_tag_union_is_default },
    });
    try ctx.scratch.values.append(ctx.allocator, fill.placeholder);
}

fn stepAliasSubstitution(ctx: *CopyContext, frame: *AliasSubstitutionFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .backing => {
                frame.stage = .args;
                if (!try requestChild(ctx, frame.source_var, frame.dest_var, frame.backing, .{
                    .kind = .alias_backing,
                    .name = @bitCast(frame.source.ident.ident_idx),
                    .origin_module = @intFromEnum(frame.source.origin_module),
                    .source_decl = @bitCast(frame.source.source_decl),
                })) return false;
            },
            .args => {
                if (frame.idx < frame.args.len) {
                    const arg_index = frame.idx;
                    const arg_var = frame.args[frame.idx];
                    frame.idx += 1;
                    if (!try requestChild(ctx, frame.source_var, frame.dest_var, arg_var, .{
                        .kind = .alias_argument,
                        .index = arg_index,
                        .name = @bitCast(frame.source.ident.ident_idx),
                        .origin_module = @intFromEnum(frame.source.origin_module),
                        .source_decl = @bitCast(frame.source.source_decl),
                    })) return false;
                    continue;
                }
                machine.values.items.len = frame.values_base;
                try machine.values.append(ctx.allocator, frame.dest_var);
                return true;
            },
        }
    }
}

fn stepIdentity(ctx: *CopyContext, frame: *IdentityFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .head => {
                if (frame.idx == frame.source_constraints.len) {
                    frame.stage = .finish;
                    continue;
                }
                const source_constraint = frame.source_constraints[frame.idx];
                frame.pending = source_constraint;
                frame.pending.constraint_evidence = .none;
                frame.pending.fn_name = try ctx.copyIdent(source_constraint.fn_name);
                // A root import authenticates only the provider occurrence it
                // copied. Provider-side inherited chains are deliberately not
                // transplanted into this store; one fresh local basis is
                // emitted for each copied provider marker below.
                frame.pending.where_method_marker_bases = .empty();
                frame.stage = .await_fn;
                if (!try requestChild(
                    ctx,
                    frame.fill.source_var,
                    frame.fill.placeholder,
                    source_constraint.fn_var,
                    .{
                        .kind = .static_dispatch_function,
                        .source_constraint_index = std.math.add(
                            u32,
                            frame.source_constraints_start,
                            frame.idx,
                        ) catch std.debug.panic("source constraint index overflowed u32", .{}),
                    },
                )) return false;
            },
            .await_fn => {
                frame.pending.fn_var = machine.values.pop().?;
                frame.marker_idx = 0;
                frame.marker_contracts_base = @intCast(machine.pending_marker_contracts.items.len);
                frame.marker_steps_base = @intCast(machine.pending_marker_steps.items.len);
                frame.stage = .marker_row;
            },
            .marker_row => {
                const source_constraint = frame.source_constraints[frame.idx];
                const marker_range = source_constraint.where_method_markers;
                if (ctx.nominal_decl_aux_depth != 0 and
                    (marker_range.len() != 0 or source_constraint.where_method_marker_bases.len() != 0))
                {
                    std.debug.panic("nominal declaration-table template unexpectedly carried where-marker metadata", .{});
                }
                if (frame.marker_idx == marker_range.len()) {
                    const pending_steps = machine.pending_marker_steps.items[frame.marker_steps_base..];
                    const pending_contracts = machine.pending_marker_contracts.items[frame.marker_contracts_base..];
                    // Re-interning identifiers and module origins is not
                    // order-preserving. Sort in the destination namespace;
                    // equal paths coalesce to one destination marker but keep
                    // one basis per exact provider marker occurrence.
                    sortImportedMarkerContracts(pending_contracts, pending_steps);

                    if (pending_contracts.len > 0 and ctx.marker_copy_origin == null) {
                        std.debug.panic("marker-bearing cross-module copy used a marker-free boundary", .{});
                    }
                    var unique_contract_count: usize = 0;
                    var unique_path_count: usize = 0;
                    for (pending_contracts, 0..) |pending, pending_index| {
                        const is_duplicate = pending_index != 0 and
                            types_mod.compareWhereMethodMarkerPathSlices(
                                pending_steps[pending_contracts[pending_index - 1].contract.path_start..][0..pending_contracts[pending_index - 1].contract.path_len],
                                pending_steps[pending.contract.path_start..][0..pending.contract.path_len],
                            ) == .eq;
                        if (!is_duplicate) {
                            unique_contract_count += 1;
                            unique_path_count += @intCast(pending.contract.path_len);
                        }
                    }

                    const dest_contract_base: u32 = @intCast(ctx.dest_store.where_method_marker_contracts.items.items.len);
                    const dest_basis_base: u32 = @intCast(ctx.dest_store.where_method_marker_bases.items.items.len);
                    try ctx.dest_store.where_method_marker_path_steps.items.ensureUnusedCapacity(ctx.allocator, unique_path_count);
                    try ctx.dest_store.where_method_marker_contracts.items.ensureUnusedCapacity(ctx.allocator, unique_contract_count);
                    try ctx.dest_store.where_method_marker_bases.items.ensureUnusedCapacity(ctx.allocator, pending_contracts.len);
                    var destination_marker_offset: u32 = undefined;
                    for (pending_contracts, 0..) |pending, pending_index| {
                        const contract = pending.contract;
                        const is_duplicate = pending_index != 0 and
                            types_mod.compareWhereMethodMarkerPathSlices(
                                pending_steps[pending_contracts[pending_index - 1].contract.path_start..][0..pending_contracts[pending_index - 1].contract.path_len],
                                pending_steps[contract.path_start..][0..contract.path_len],
                            ) == .eq;
                        if (is_duplicate) {
                            const previous = &ctx.dest_store.where_method_marker_contracts.items.items[
                                @as(usize, @intCast(dest_contract_base)) + destination_marker_offset
                            ];
                            if (!types_mod.whereMethodMarkerNominalNamesMatch(
                                ctx.dest_store.where_method_marker_path_steps.items.items[previous.path_start..][0..previous.path_len],
                                pending_steps[contract.path_start..][0..contract.path_len],
                            ) or previous.position != contract.position) {
                                std.debug.panic("rebased where-method marker paths carried incompatible metadata", .{});
                            }
                            previous.widened = @intFromBool(previous.isWidened().? or contract.isWidened().?);
                        } else {
                            destination_marker_offset = @intCast(
                                ctx.dest_store.where_method_marker_contracts.items.items.len - @as(usize, @intCast(dest_contract_base)),
                            );
                            const dest_path_start: u32 = @intCast(ctx.dest_store.where_method_marker_path_steps.items.items.len);
                            ctx.dest_store.where_method_marker_path_steps.items.appendSliceAssumeCapacity(
                                pending_steps[contract.path_start..][0..contract.path_len],
                            );
                            var destination_contract = contract;
                            destination_contract.path_start = dest_path_start;
                            ctx.dest_store.where_method_marker_contracts.items.appendAssumeCapacity(destination_contract);
                        }
                        ctx.dest_store.where_method_marker_bases.items.appendAssumeCapacity(.{
                            .marker_offset = destination_marker_offset,
                            .copy_step = ctx.predicted_copy_step,
                            .source_constraint_index = std.math.add(
                                u32,
                                frame.source_constraints_start,
                                frame.idx,
                            ) catch std.debug.panic("source constraint index overflowed u32", .{}),
                            .source_contract_offset = pending.source_contract_offset,
                        });
                    }
                    frame.pending.where_method_markers = .{
                        .start = @enumFromInt(dest_contract_base),
                        .count = @intCast(unique_contract_count),
                    };
                    frame.pending.where_method_marker_bases = .{
                        .start = @enumFromInt(dest_basis_base),
                        .count = @intCast(pending_contracts.len),
                    };
                    ctx.carried_where_marker = ctx.carried_where_marker or pending_contracts.len > 0;
                    machine.pending_marker_steps.items.len = frame.marker_steps_base;
                    machine.pending_marker_contracts.items.len = frame.marker_contracts_base;
                    if (!frame.source_constraints[frame.idx].interpolation.isPresent()) {
                        frame.stage = .finish_constraint;
                        continue;
                    }
                    frame.parts_base = @intCast(machine.pending_parts.items.len);
                    frame.part_idx = 0;
                    frame.stage = .parts;
                    continue;
                }

                const source_index = @intFromEnum(marker_range.start) + frame.marker_idx;
                const source_marker = ctx.source_store.where_method_marker_contracts.items.items[source_index];
                if (source_marker.positionOrNull() == null or source_marker.isWidened() == null or
                    source_marker.isReady() != true or source_marker.hasProducer() or
                    source_marker.path_start > ctx.source_store.where_method_marker_path_steps.items.items.len or
                    source_marker.path_len > ctx.source_store.where_method_marker_path_steps.items.items.len - source_marker.path_start)
                {
                    std.debug.panic("imported where-method marker contract was invalid", .{});
                }
                const local_path_start: u32 = @intCast(machine.pending_marker_steps.items.len - frame.marker_steps_base);
                const source_steps = ctx.source_store.where_method_marker_path_steps.items.items[source_marker.path_start .. source_marker.path_start + source_marker.path_len];
                try machine.pending_marker_steps.ensureUnusedCapacity(ctx.allocator, source_steps.len);
                for (source_steps) |source_step| {
                    const kind = source_step.kindOrNull() orelse
                        std.debug.panic("imported where-method marker path kind was invalid", .{});
                    var dest_step = source_step;
                    switch (kind) {
                        .record_field, .tag_payload => dest_step.name = @bitCast(try ctx.copyIdent(@bitCast(source_step.name))),
                        .nominal_arg => {
                            const source_decl: types_mod.SourceDecl = @bitCast(source_step.source_decl);
                            if (ctx.source_store.canonicalNominalDeclForMarkerPath(source_step) == null) {
                                std.debug.panic("imported nominal marker path did not name its canonical declaration", .{});
                            }
                            const source_origin: base.ModuleIdentity.Idx = @enumFromInt(source_step.origin_module);

                            const dest_origin = try ctx.copyOriginModule(source_origin);
                            const dest_decl_idx = ctx.dest_store.lookupNominalDeclByKey(
                                dest_origin,
                                source_decl.statement,
                            ) orelse std.debug.panic("copied nominal marker path had no destination declaration entry", .{});
                            const dest_entry = ctx.dest_store.getNominalDecl(dest_decl_idx);
                            if (dest_entry.origin_module != dest_origin or
                                !dest_entry.source.sourceDecl().eql(source_decl) or
                                dest_entry.formals.len() != source_step.arity)
                            {
                                std.debug.panic("copied nominal marker declaration did not preserve its exact key and arity", .{});
                            }
                            dest_step.name = @bitCast(dest_entry.ident.ident_idx);
                            dest_step.origin_module = @intFromEnum(dest_origin);
                            if (ctx.dest_store.canonicalNominalDeclForMarkerPath(dest_step) == null) {
                                std.debug.panic("copied nominal marker path did not name its destination declaration", .{});
                            }
                        },
                        .fn_arg, .fn_ret, .tuple_elem => {},
                    }
                    machine.pending_marker_steps.appendAssumeCapacity(dest_step);
                }
                try machine.pending_marker_contracts.append(ctx.allocator, .{
                    .contract = .{
                        .producer_owner_node = std.math.maxInt(u32),
                        .producer_where_node = std.math.maxInt(u32),
                        .producer_method_name = std.math.maxInt(u32),
                        .position = source_marker.position,
                        .widened = source_marker.widened,
                        .ready = 1,
                        .path_start = local_path_start,
                        .path_len = source_marker.path_len,
                    },
                    .source_contract_offset = frame.marker_idx,
                });
                frame.marker_idx += 1;
                frame.stage = .marker_row;
            },
            .parts => {
                const source_parts = ctx.source_store.sliceInterpolationParts(
                    frame.source_constraints[frame.idx].interpolation.interpolated_parts,
                );
                if (frame.part_idx < source_parts.len) {
                    frame.stage = .await_part;
                    if (!try requestChild(
                        ctx,
                        frame.fill.source_var,
                        frame.fill.placeholder,
                        source_parts[frame.part_idx].var_,
                        .{
                            .kind = .interpolation_part,
                            .index = frame.part_idx,
                            .source_constraint_index = std.math.add(
                                u32,
                                frame.source_constraints_start,
                                frame.idx,
                            ) catch std.debug.panic("source constraint index overflowed u32", .{}),
                        },
                    )) return false;
                    continue;
                }
                frame.stage = .await_item;
                if (!try requestChild(
                    ctx,
                    frame.fill.source_var,
                    frame.fill.placeholder,
                    frame.source_constraints[frame.idx].interpolation.item_var,
                    .{
                        .kind = .interpolation_item,
                        .source_constraint_index = std.math.add(
                            u32,
                            frame.source_constraints_start,
                            frame.idx,
                        ) catch std.debug.panic("source constraint index overflowed u32", .{}),
                    },
                )) return false;
            },
            .await_part => {
                const source_parts = ctx.source_store.sliceInterpolationParts(
                    frame.source_constraints[frame.idx].interpolation.interpolated_parts,
                );
                try machine.pending_parts.append(ctx.allocator, .{
                    .var_ = machine.values.pop().?,
                    .region = source_parts[frame.part_idx].region,
                });
                frame.part_idx += 1;
                frame.stage = .parts;
            },
            .await_item => {
                const dest_item_var = machine.values.pop().?;
                const dest_parts_range = try ctx.dest_store.appendInterpolationParts(
                    machine.pending_parts.items[frame.parts_base..],
                );
                machine.pending_parts.items.len = frame.parts_base;
                frame.pending.interpolation = .{
                    .expr_region = frame.source_constraints[frame.idx].interpolation.expr_region,
                    .item_var = dest_item_var,
                    .interpolated_parts = dest_parts_range,
                };
                frame.stage = .finish_constraint;
            },
            .finish_constraint => {
                if (frame.source_constraints[frame.idx].derived_map_plan) |plan| {
                    frame.pending.derived_map_plan = .{
                        .tag_name = try ctx.copyIdent(plan.tag_name),
                        .payload_index = plan.payload_index,
                    };
                }
                // The introducing expression is module-scoped: its index refers to the
                // SOURCE module's CIR and is meaningless here. Clear it on the boundary
                // crossing so a consumer never dereferences a foreign expression index
                // against the destination module.
                frame.pending.provenance = .{};
                try machine.pending_constraints.append(ctx.allocator, frame.pending);
                frame.idx += 1;
                frame.stage = .head;
            },
            .finish => {
                const dest_range = try ctx.dest_store.appendStaticDispatchConstraints(
                    machine.pending_constraints.items[frame.cons_base..],
                );
                machine.pending_constraints.items.len = frame.cons_base;
                if (ctx.nominal_decl_aux_depth == 0) {
                    if (dest_range.len() != frame.source_constraints.len) {
                        std.debug.panic("constraint copy changed the exact occurrence count", .{});
                    }
                    try machine.proof_constraint_pairs.ensureUnusedCapacity(
                        ctx.allocator,
                        frame.source_constraints.len,
                    );
                    for (0..frame.source_constraints.len) |raw_offset| {
                        const offset: u32 = @intCast(raw_offset);
                        machine.proof_constraint_pairs.appendAssumeCapacity(.{
                            .source_constraint_index = std.math.add(
                                u32,
                                frame.source_constraints_start,
                                offset,
                            ) catch std.debug.panic("source constraint index overflowed u32", .{}),
                            .destination_constraint_index = std.math.add(
                                u32,
                                @intFromEnum(dest_range.start),
                                offset,
                            ) catch std.debug.panic("destination constraint index overflowed u32", .{}),
                        });
                    }
                }
                const content: Content = switch (frame.result) {
                    .flex => Content{ .flex = Flex{ .name = frame.name, .constraints = dest_range } },
                    .rigid => Content{ .rigid = Rigid{ .name = frame.name.?, .constraints = dest_range } },
                };
                try finishFrame(ctx, frame.fill, content);
                return true;
            },
        }
    }
}

fn stepAlias(ctx: *CopyContext, frame: *AliasFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .backing => {
                frame.stage = .args;
                if (!try requestChild(ctx, frame.fill.source_var, frame.fill.placeholder, frame.backing, .{
                    .kind = .alias_backing,
                    .name = @bitCast(frame.source.ident.ident_idx),
                    .origin_module = @intFromEnum(frame.source.origin_module),
                    .source_decl = @bitCast(frame.source.source_decl),
                })) return false;
            },
            .args => {
                if (frame.idx < frame.args.len) {
                    const arg_index = frame.idx;
                    const arg_var = frame.args[frame.idx];
                    frame.idx += 1;
                    if (!try requestChild(ctx, frame.fill.source_var, frame.fill.placeholder, arg_var, .{
                        .kind = .alias_argument,
                        .index = arg_index,
                        .name = @bitCast(frame.source.ident.ident_idx),
                        .origin_module = @intFromEnum(frame.source.origin_module),
                        .source_decl = @bitCast(frame.source.source_decl),
                    })) return false;
                    continue;
                }
                // The backing copy leads the run, matching the alias layout's
                // backing-then-args ordering.
                const dest_vars_span = try ctx.dest_store.appendVars(machine.values.items[frame.values_base..]);
                machine.values.items.len = frame.values_base;
                const translated_origin = try ctx.copyOriginModule(frame.source.origin_module);
                try finishFrame(ctx, frame.fill, Content{ .alias = Alias{
                    .ident = types_mod.TypeIdent{ .ident_idx = frame.translated_ident },
                    .vars = .{ .nonempty = dest_vars_span },
                    .origin_module = translated_origin,
                    .source_decl = frame.source.source_decl,
                } });
                return true;
            },
        }
    }
}

fn stepTuple(ctx: *CopyContext, frame: *TupleFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        if (frame.idx < frame.elems.len) {
            const elem_index = frame.idx;
            const elem_var = frame.elems[frame.idx];
            frame.idx += 1;
            if (!try requestChild(ctx, frame.fill.source_var, frame.fill.placeholder, elem_var, .{
                .kind = .tuple_element,
                .index = elem_index,
            })) return false;
            continue;
        }
        const dest_range = try ctx.dest_store.appendVars(machine.values.items[frame.values_base..]);
        machine.values.items.len = frame.values_base;
        try finishFrame(ctx, frame.fill, Content{ .structure = FlatType{ .tuple = types_mod.Tuple{ .elems = dest_range } } });
        return true;
    }
}

fn stepNominal(ctx: *CopyContext, frame: *NominalFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .decl => {
                frame.stage = .args;
                if (!try ensureNominalDeclCopied(ctx, frame.source, frame.translated_origin)) return false;
            },
            .args => {
                if (frame.idx < frame.args.len) {
                    const arg_index = frame.idx;
                    const arg_var = frame.args[frame.idx];
                    frame.idx += 1;
                    if (!try requestChild(ctx, frame.fill.source_var, frame.fill.placeholder, arg_var, .{
                        .kind = .nominal_argument,
                        .index = arg_index,
                        .name = @bitCast(frame.source.ident.ident_idx),
                        .origin_module = @intFromEnum(frame.source.origin_module),
                        .source_decl = @bitCast(frame.source.sourceDecl()),
                    })) return false;
                    continue;
                }
                const dest_args_range = try ctx.dest_store.appendVars(machine.values.items[frame.values_base..]);
                machine.values.items.len = frame.values_base;
                try finishFrame(ctx, frame.fill, Content{ .structure = FlatType{ .nominal_type = NominalType{
                    .ident = types_mod.TypeIdent{ .ident_idx = frame.translated_ident },
                    .args = dest_args_range,
                    .origin_module = frame.translated_origin,
                    .source = frame.source.source,
                } } });
                return true;
            },
        }
    }
}

fn stepNominalDecl(ctx: *CopyContext, frame: *NominalDeclFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .formals => {
                if (frame.idx < frame.formals.len) {
                    const formal_var = frame.formals[frame.idx];
                    frame.idx += 1;
                    if (!try request(ctx, formal_var)) return false;
                    continue;
                }
                frame.formals_range = try ctx.dest_store.appendVars(machine.values.items[frame.values_base..]);
                machine.values.items.len = frame.values_base;
                frame.stage = .backing;
            },
            .backing => {
                frame.stage = .finish;
                if (!try request(ctx, frame.backing)) return false;
            },
            .finish => {
                const dest_backing = machine.values.pop().?;
                var dest_entry = ctx.dest_store.getNominalDecl(frame.reserved_idx);
                dest_entry.formals = frame.formals_range;
                dest_entry.backing = dest_backing;
                try ctx.dest_store.setNominalDecl(frame.reserved_idx, dest_entry);
                std.debug.assert(ctx.nominal_decl_aux_depth != 0);
                ctx.nominal_decl_aux_depth -= 1;
                if (ctx.nominal_decl_aux_depth == 0) {
                    ctx.nominal_decl_var_mapping.clearRetainingCapacity();
                }
                return true;
            },
        }
    }
}

fn stepFunc(ctx: *CopyContext, frame: *FuncFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .args => {
                if (frame.idx < frame.args.len) {
                    const arg_index = frame.idx;
                    const arg_var = frame.args[frame.idx];
                    frame.idx += 1;
                    if (!try requestChild(ctx, frame.fill.source_var, frame.fill.placeholder, arg_var, .{
                        .kind = .function_argument,
                        .index = arg_index,
                    })) return false;
                    continue;
                }
                frame.idx = 0;
                frame.stage = .ret;
            },
            .ret => {
                frame.stage = .effect_deps;
                if (!try requestChild(ctx, frame.fill.source_var, frame.fill.placeholder, frame.ret, .{
                    .kind = .function_return,
                })) return false;
            },
            .effect_deps => {
                if (frame.idx < frame.effect_deps.len) {
                    const dependency_index = frame.idx;
                    const effect_dep = frame.effect_deps[frame.idx];
                    frame.idx += 1;
                    if (!try requestChild(ctx, frame.fill.source_var, frame.fill.placeholder, effect_dep, .{
                        .kind = .function_effect_dependency,
                        .index = dependency_index,
                    })) return false;
                    continue;
                }
                // The value run holds the copied args, then the copied return
                // type, then the copied effect dependencies.
                const values = machine.values.items;
                const args_end = frame.values_base + frame.args.len;
                const dest_args_range = try ctx.dest_store.appendVars(values[frame.values_base..args_end]);
                const dest_ret = values[args_end];
                const dest_effect_deps_range = try ctx.dest_store.appendVars(values[args_end + 1 ..]);
                machine.values.items.len = frame.values_base;
                const dest_func = Func{
                    .args = dest_args_range,
                    .ret = dest_ret,
                    .effect_deps = dest_effect_deps_range,
                };
                const content: Content = switch (frame.kind) {
                    .pure => Content{ .structure = FlatType{ .fn_pure = dest_func } },
                    .effectful => Content{ .structure = FlatType{ .fn_effectful = dest_func } },
                    .unbound => Content{ .structure = FlatType{ .fn_unbound = dest_func } },
                };
                try finishFrame(ctx, frame.fill, content);
                return true;
            },
        }
    }
}

/// Zip the copied field types back onto the names collected for this row and
/// append the finished run to the destination store.
fn finishRecordFields(
    ctx: *CopyContext,
    source_fields: RecordField.SafeMultiList.Range,
    fields_base: u32,
    values_base: u32,
) std.mem.Allocator.Error!RecordField.SafeMultiList.Range {
    const machine = &ctx.scratch;
    const fields = machine.pending_fields.items[fields_base..];
    var value_idx: usize = values_base;
    for (fields, 0..) |*field, i| {
        const source_field = ctx.source_store.record_fields.get(@enumFromInt(@intFromEnum(source_fields.start) + i));
        const dest_type = machine.values.items[value_idx];
        value_idx += 1;
        field.presence = if (source_field.presence.presenceVar()) |_| blk: {
            const dest_presence = machine.values.items[value_idx];
            value_idx += 1;
            break :blk .unknown(dest_presence, dest_type);
        } else .required(dest_type);
    }
    const range = try ctx.dest_store.appendRecordFields(fields);
    machine.pending_fields.items.len = fields_base;
    machine.values.items.len = values_base;
    return range;
}

/// Translate one field's name, record it against its slot in this row's run,
/// and request the field's type.
fn requestRecordField(
    ctx: *CopyContext,
    parent: Fill,
    source_fields: RecordField.SafeMultiList.Range,
    idx: *u32,
    axis: *FieldAxis,
    unbound: bool,
) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    // Indexing through the run's start only happens when the record has
    // fields; start may be undefined when count is 0.
    const field = ctx.source_store.record_fields.get(@enumFromInt(@intFromEnum(source_fields.start) + idx.*));
    return switch (axis.*) {
        .type_var => blk: {
            const translated_name = try ctx.copyIdent(field.name);
            try machine.pending_fields.append(ctx.allocator, .{ .name = translated_name, .presence = undefined });
            if (field.presence.presenceVar() != null) {
                axis.* = .presence_var;
            } else {
                idx.* += 1;
            }
            break :blk try requestChild(ctx, parent.source_var, parent.placeholder, field.presence.typeVar(), .{
                .kind = if (unbound) .record_unbound_field_type else .record_field_type,
                .name = @bitCast(field.name),
            });
        },
        .presence_var => blk: {
            idx.* += 1;
            axis.* = .type_var;
            break :blk try requestChild(ctx, parent.source_var, parent.placeholder, field.presence.presenceVar().?, .{
                .kind = if (unbound) .record_unbound_field_presence else .record_field_presence,
                .name = @bitCast(field.name),
            });
        },
    };
}

fn stepRecord(ctx: *CopyContext, frame: *RecordFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .fields => {
                if (frame.idx < frame.source_fields.count) {
                    if (!try requestRecordField(
                        ctx,
                        frame.fill,
                        frame.source_fields,
                        &frame.idx,
                        &frame.axis,
                        false,
                    )) return false;
                    continue;
                }
                frame.fields_range = try finishRecordFields(ctx, frame.source_fields, frame.fields_base, frame.values_base);
                frame.stage = .await_ext;
                if (!try requestChild(ctx, frame.fill.source_var, frame.fill.placeholder, frame.ext, .{
                    .kind = .record_extension,
                })) return false;
            },
            .await_ext => {
                const dest_ext = machine.values.pop().?;
                try finishFrame(ctx, frame.fill, Content{ .structure = FlatType{ .record = Record{
                    .fields = frame.fields_range,
                    .ext = dest_ext,
                } } });
                return true;
            },
        }
    }
}

fn stepRecordUnbound(ctx: *CopyContext, frame: *RecordUnboundFrame) std.mem.Allocator.Error!bool {
    while (true) {
        if (frame.idx < frame.source_fields.count) {
            if (!try requestRecordField(
                ctx,
                frame.fill,
                frame.source_fields,
                &frame.idx,
                &frame.axis,
                true,
            )) return false;
            continue;
        }
        const fields_range = try finishRecordFields(ctx, frame.source_fields, frame.fields_base, frame.values_base);
        try finishFrame(ctx, frame.fill, Content{ .structure = FlatType{ .record_unbound = fields_range } });
        return true;
    }
}

fn stepTagUnion(ctx: *CopyContext, frame: *TagUnionFrame) std.mem.Allocator.Error!bool {
    const machine = &ctx.scratch;
    while (true) {
        switch (frame.stage) {
            .tag_head => {
                if (frame.tag_idx == frame.source_tags.count) {
                    frame.stage = .tags_done;
                    continue;
                }
                frame.arg_idx = 0;
                frame.stage = .tag_args;
            },
            .tag_args => {
                // Indexing through the run's start only happens when the tag
                // union has tags; start may be undefined when count is 0.
                const tag = ctx.source_store.tags.get(@enumFromInt(@intFromEnum(frame.source_tags.start) + frame.tag_idx));
                const args_slice = ctx.source_store.sliceVars(tag.args);
                if (frame.arg_idx < args_slice.len) {
                    const payload_index = frame.arg_idx;
                    const arg_var = args_slice[frame.arg_idx];
                    frame.arg_idx += 1;
                    if (!try requestChild(ctx, frame.fill.source_var, frame.fill.placeholder, arg_var, .{
                        .kind = .tag_payload,
                        .index = payload_index,
                        .name = @bitCast(tag.name),
                    })) return false;
                    continue;
                }
                const dest_args_range = try ctx.dest_store.appendVars(machine.values.items[frame.values_base..]);
                machine.values.items.len = frame.values_base;
                const translated_name = try ctx.copyIdent(tag.name);
                try machine.pending_tags.append(ctx.allocator, .{
                    .name = translated_name,
                    .args = dest_args_range,
                });
                frame.tag_idx += 1;
                frame.stage = .tag_head;
            },
            .tags_done => {
                frame.tags_range = try ctx.dest_store.appendTags(machine.pending_tags.items[frame.tags_base..]);
                machine.pending_tags.items.len = frame.tags_base;
                frame.stage = .await_ext;
                if (!try requestChild(ctx, frame.fill.source_var, frame.fill.placeholder, frame.ext, .{
                    .kind = .tag_extension,
                })) return false;
            },
            .await_ext => {
                const dest_ext = machine.values.pop().?;
                try finishFrame(ctx, frame.fill, Content{ .structure = FlatType{ .tag_union = TagUnion{
                    .tags = frame.tags_range,
                    .ext = dest_ext,
                } } });
                return true;
            },
        }
    }
}

/// Ensure the destination store's nominal declaration table has an entry for
/// the declaration behind `source_nominal`, copying it from the source store's
/// table on first encounter. This runs once per (destination module,
/// declaration): every later application of the same declaration finds the
/// key already present and returns immediately, so declaration data crosses a
/// module boundary at most once regardless of how many applications do.
///
/// The entry is reserved (key registered) before its formals and backing are
/// copied so that self-referential backing templates terminate: copying the
/// template's own recursive application re-enters this function and finds the
/// key already present.
///
/// Returns true when nothing had to be copied; false when a frame was pushed
/// to copy the entry.
fn ensureNominalDeclCopied(
    ctx: *CopyContext,
    source_nominal: NominalType,
    translated_origin: base.ModuleIdentity.Idx,
) std.mem.Allocator.Error!bool {
    const source_decl = source_nominal.sourceDecl();
    // A nominal without a source declaration has no key and no declaration
    // table entry (only possible for hand-constructed types in tests).
    if (!source_decl.present) return true;

    if (ctx.dest_store.lookupNominalDeclByKey(translated_origin, source_decl.statement) != null) return true;

    const source_decl_idx = ctx.source_store.lookupNominalDecl(source_nominal) orelse {
        // Invariant: every nominal application in a store can resolve its
        // declaration in that store, so a keyed application without a source
        // table entry is a compiler bug.
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "copy_import invariant violated: nominal '{s}' has a source declaration but no declaration table entry in its source store",
                .{ctx.sourceIdents().getText(source_nominal.ident.ident_idx)},
            );
        }
        unreachable;
    };

    try pushNominalDeclEntry(ctx, ctx.source_store.getNominalDecl(source_decl_idx), translated_origin);
    return false;
}

/// Ensure the destination store has a declaration-table entry for the nominal
/// declaration at `statement` in the source module env, keyed under the source
/// module's own identity rebased into the destination env. No-op when the
/// source store has no entry for that statement (e.g. an alias declaration) or
/// when the destination already has one.
///
/// Newly created destination vars are recorded in `var_mapping`; the caller
/// owns follow-up bookkeeping for them (regions, worklists), exactly as with
/// `copyVar`.
pub fn ensureNominalDeclForStatement(
    source_store: *const TypesStore,
    dest_store: *TypesStore,
    statement: u32,
    var_mapping: *VarMapping,
    source_env: *const ModuleEnv,
    dest_env: *ModuleEnv,
    allocator: std.mem.Allocator,
) std.mem.Allocator.Error!void {
    const source_origin = source_env.selfModuleIdentity();
    const source_decl_idx = source_store.lookupNominalDeclByKey(source_origin, statement) orelse return;
    var transaction: CrossModuleCopyTransaction = undefined;
    try transaction.beginPublic(
        dest_store,
        dest_env,
        var_mapping,
        allocator,
    );
    defer transaction.rollback();
    var ctx = CopyContext{
        .source_store = source_store,
        .dest_store = dest_store,
        .var_mapping = var_mapping,
        .alias_source_mapping = null,
        .source_env = source_env,
        .dest_env = dest_env,
        .allocator = allocator,
        // Builtin nominal declaration templates are copied only to populate
        // the local keyed declaration table. They are not a semantic import
        // occurrence and therefore may not carry W6 marker authority.
        .marker_copy_origin = null,
        .publish_support_step = false,
        .predicted_copy_step = @intCast(dest_env.where_marker_copy_steps.items.items.len),
        .transaction = &transaction,
        .nominal_decl_var_mapping = VarMapping.init(allocator),
    };
    defer ctx.deinit();

    const translated_origin = try ctx.copyOriginModule(source_origin);
    if (dest_store.lookupNominalDeclByKey(translated_origin, statement) != null) {
        transaction.commit();
        return;
    }

    const frames_base = ctx.scratch.frames.items.len;
    try pushNominalDeclEntry(&ctx, source_store.getNominalDecl(source_decl_idx), translated_origin);
    try drive(&ctx, frames_base);
    if (ctx.carried_where_marker) {
        std.debug.panic("builtin nominal declaration template unexpectedly carried where-marker authority", .{});
    }
    transaction.commit();
}

/// Reserve one declaration-table entry (formals + backing template) in the
/// destination store and push the frame that copies its graph. The key is
/// reserved before the graph copy so that self-referential backing templates
/// terminate: copying the template's own recursive application re-enters
/// `ensureNominalDeclCopied` and finds the key already present. Nothing reads
/// the reserved entry's formals/backing while the copy is in flight—lookups
/// only test key presence.
fn pushNominalDeclEntry(
    ctx: *CopyContext,
    source_entry: types_mod.NominalDecl,
    translated_origin: base.ModuleIdentity.Idx,
) std.mem.Allocator.Error!void {
    if (ctx.nominal_decl_aux_depth == 0) {
        ctx.nominal_decl_var_mapping.clearRetainingCapacity();
    }
    const translated_ident = try ctx.copyIdent(source_entry.ident.ident_idx);
    const reserved_idx = try ctx.dest_store.registerNominalDecl(.{
        .ident = types_mod.TypeIdent{ .ident_idx = translated_ident },
        .origin_module = translated_origin,
        .source = source_entry.source,
        .formals = Var.SafeList.Range.empty(),
        // Never read while the copy is in flight (see above); both fields are
        // filled in below once the graph copy completes.
        .backing = undefined,
        .flags = source_entry.flags,
    });

    try ctx.scratch.frames.append(ctx.allocator, .{ .nominal_decl = .{
        .reserved_idx = reserved_idx,
        .formals = ctx.source_store.sliceVars(source_entry.formals),
        .backing = source_entry.backing,
        .values_base = @intCast(ctx.scratch.values.items.len),
    } });
    ctx.nominal_decl_aux_depth = std.math.add(u32, ctx.nominal_decl_aux_depth, 1) catch
        std.debug.panic("nominal declaration copy nesting overflowed u32", .{});
}

/// Typed snapshot of every destination surface owned by a public
/// cross-module-copy transaction. The type store uses its deterministic
/// serialized form; the ModuleEnv fields remain individually typed so adding
/// another owned pool requires an explicit field here instead of disappearing
/// into a type-erased list table.
/// Read-only exact-state snapshot used by public-copy transaction tests. It
/// exposes no mutation path and compares the transaction's complete declared
/// ownership surface.
const CrossCopyDestinationSnapshot = struct {
    const MappingEntry = struct { source: Var, destination: Var };
    const SlotUndo = @typeInfo(@FieldType(@FieldType(TypesStore, "slot_trail"), "items")).pointer.child;
    const DescUndo = @typeInfo(@FieldType(@FieldType(TypesStore, "desc_trail"), "items")).pointer.child;
    const RootMetaUndo = @typeInfo(@FieldType(@FieldType(TypesStore, "root_meta_trail"), "items")).pointer.child;
    const UnionRankUndo = @typeInfo(@FieldType(@FieldType(TypesStore, "union_rank_trail"), "items")).pointer.child;
    const NominalDeclUndo = @typeInfo(@FieldType(@FieldType(TypesStore, "nominal_decl_trail"), "items")).pointer.child;

    arena: std.heap.ArenaAllocator,
    store_bytes: []const u8,
    store_savepoint_active: bool,
    store_savepoint_depth: u32,
    store_savepoint_baseline_slots: u32,
    store_savepoint_baseline_descs: u32,
    store_savepoint_baseline_nominal_decls: u32,
    slot_trail: []const SlotUndo,
    desc_trail: []const DescUndo,
    root_meta_trail: []const RootMetaUndo,
    union_rank_trail: []const UnionRankUndo,
    nominal_decl_trail: []const NominalDeclUndo,
    ident_entry_count: u32,
    ident_bytes: []const u8,
    ident_index: []const base.SmallStringInterner.Idx,
    ident_savepoint_depth: u16,
    identity_count: u32,
    identity_bytes: []const u8,
    identity_ranges: []const base.SerialStringInterner.Range,
    identity_index: []const u32,
    identity_savepoint_depth: u16,
    identity_displays: []const base.Ident.Idx,
    copy_steps: []const ModuleEnv.WhereMarkerCopyStep,
    copy_pairs: []const ModuleEnv.WhereMarkerCopyPair,
    copy_occurrences: []const ModuleEnv.WhereMarkerCopyOccurrence,
    constraint_pairs: []const ModuleEnv.WhereMarkerConstraintCopyPair,
    copy_witnesses: []const ModuleEnv.WhereMarkerCopyWitness,
    copied_literal_groups: []const ModuleEnv.CopiedOpenLiteralGroup,
    copied_literal_events: []const ModuleEnv.CopiedOpenLiteralEvent,
    platform_substitutions: []const ModuleEnv.WhereMarkerPlatformSubstitution,
    selected_anchors: []const ModuleEnv.SelectedReceiverAnchor,
    settlement_sources: []const ModuleEnv.DispatchSettlementSource,
    mapping_entries: []const MappingEntry,

    fn serializeStore(allocator: std.mem.Allocator, store: *const TypesStore) ![]u8 {
        var writer = collections.CompactWriter.init();
        defer writer.deinit(allocator);
        const header = try writer.appendAlloc(allocator, TypesStore.Serialized);
        try header.serialize(store, allocator, &writer);
        const bytes = try allocator.alloc(u8, @intCast(writer.total_bytes));
        errdefer allocator.free(bytes);
        _ = try writer.writeToBuffer(bytes);
        return bytes;
    }

    pub fn capture(
        parent_allocator: std.mem.Allocator,
        store: *const TypesStore,
        env: *const ModuleEnv,
        mapping: *const VarMapping,
    ) !@This() {
        var arena = std.heap.ArenaAllocator.init(parent_allocator);
        errdefer arena.deinit();
        const allocator = arena.allocator();

        const mapping_entries = try allocator.alloc(MappingEntry, mapping.count());
        var mapping_iterator = mapping.iterator();
        var mapping_index: usize = 0;
        while (mapping_iterator.next()) |entry| : (mapping_index += 1) {
            mapping_entries[mapping_index] = .{
                .source = entry.key_ptr.*,
                .destination = entry.value_ptr.*,
            };
        }
        std.debug.assert(mapping_index == mapping_entries.len);

        const store_bytes = try serializeStore(allocator, store);
        const slot_trail = try allocator.dupe(SlotUndo, store.slot_trail.items);
        const desc_trail = try allocator.dupe(DescUndo, store.desc_trail.items);
        const root_meta_trail = try allocator.dupe(RootMetaUndo, store.root_meta_trail.items);
        const union_rank_trail = try allocator.dupe(UnionRankUndo, store.union_rank_trail.items);
        const nominal_decl_trail = try allocator.dupe(NominalDeclUndo, store.nominal_decl_trail.items);
        const ident_bytes = try allocator.dupe(u8, env.common.idents.interner.bytes.items.items);
        const ident_index = try allocator.dupe(
            base.SmallStringInterner.Idx,
            env.common.idents.interner.index.items.items,
        );
        const identity_bytes = try allocator.dupe(u8, env.module_identities.bytes.items.items);
        const identity_ranges = try allocator.dupe(
            base.SerialStringInterner.Range,
            env.module_identities.ranges.items.items,
        );
        const identity_index = try allocator.dupe(u32, env.module_identities.index.items.items);
        const identity_displays = try allocator.dupe(
            base.Ident.Idx,
            env.module_identity_displays.items.items,
        );
        const copy_steps = try allocator.dupe(
            ModuleEnv.WhereMarkerCopyStep,
            env.where_marker_copy_steps.items.items,
        );
        const copy_pairs = try allocator.dupe(
            ModuleEnv.WhereMarkerCopyPair,
            env.where_marker_copy_pairs.items.items,
        );
        const copy_occurrences = try allocator.dupe(
            ModuleEnv.WhereMarkerCopyOccurrence,
            env.where_marker_copy_occurrences.items.items,
        );
        const constraint_pairs = try allocator.dupe(
            ModuleEnv.WhereMarkerConstraintCopyPair,
            env.where_marker_constraint_copy_pairs.items.items,
        );
        const copy_witnesses = try allocator.dupe(
            ModuleEnv.WhereMarkerCopyWitness,
            env.where_marker_copy_witnesses.items.items,
        );
        const copied_literal_groups = try allocator.dupe(
            ModuleEnv.CopiedOpenLiteralGroup,
            env.copied_open_literal_groups.items.items,
        );
        const copied_literal_events = try allocator.dupe(
            ModuleEnv.CopiedOpenLiteralEvent,
            env.copied_open_literal_events.items.items,
        );
        const platform_substitutions = try allocator.dupe(
            ModuleEnv.WhereMarkerPlatformSubstitution,
            env.where_marker_platform_substitutions.items.items,
        );
        const selected_anchors = try allocator.dupe(
            ModuleEnv.SelectedReceiverAnchor,
            env.selected_receiver_anchors.items.items,
        );
        const settlement_sources = try allocator.dupe(
            ModuleEnv.DispatchSettlementSource,
            env.dispatch_settlement_sources.items.items,
        );

        return .{
            .arena = arena,
            .store_bytes = store_bytes,
            .store_savepoint_active = store.savepoint_active,
            .store_savepoint_depth = store.savepoint_depth,
            .store_savepoint_baseline_slots = store.savepoint_baseline_slots,
            .store_savepoint_baseline_descs = store.savepoint_baseline_descs,
            .store_savepoint_baseline_nominal_decls = store.savepoint_baseline_nominal_decls,
            .slot_trail = slot_trail,
            .desc_trail = desc_trail,
            .root_meta_trail = root_meta_trail,
            .union_rank_trail = union_rank_trail,
            .nominal_decl_trail = nominal_decl_trail,
            .ident_entry_count = env.common.idents.interner.entry_count,
            .ident_bytes = ident_bytes,
            .ident_index = ident_index,
            .ident_savepoint_depth = env.common.idents.interner.savepoint_depth,
            .identity_count = env.module_identities.count(),
            .identity_bytes = identity_bytes,
            .identity_ranges = identity_ranges,
            .identity_index = identity_index,
            .identity_savepoint_depth = env.module_identities.savepoint_depth,
            .identity_displays = identity_displays,
            .copy_steps = copy_steps,
            .copy_pairs = copy_pairs,
            .copy_occurrences = copy_occurrences,
            .constraint_pairs = constraint_pairs,
            .copy_witnesses = copy_witnesses,
            .copied_literal_groups = copied_literal_groups,
            .copied_literal_events = copied_literal_events,
            .platform_substitutions = platform_substitutions,
            .selected_anchors = selected_anchors,
            .settlement_sources = settlement_sources,
            .mapping_entries = mapping_entries,
        };
    }

    fn portableExternUnionWordsEqual(comptime T: type, left: T, right: T) bool {
        const word_count = @sizeOf(T) / @sizeOf(u32);
        comptime {
            std.debug.assert(@sizeOf(T) == word_count * @sizeOf(u32));
            std.debug.assert(@alignOf(T) <= @alignOf(u32));
        }
        const left_words: [word_count]u32 = @bitCast(left);
        const right_words: [word_count]u32 = @bitCast(right);
        return std.mem.eql(u32, &left_words, &right_words);
    }

    fn canonicalValueEqual(comptime T: type, left: T, right: T) bool {
        if (T == ModuleEnv.WhereMarkerCopyStep) {
            inline for (.{
                "kind",
                "copy_policy",
                "source_root_var",
                "destination_root_var",
                "pairs_start",
                "pairs_len",
                "occurrences_start",
                "occurrences_len",
                "root_occurrence_offset",
                "constraint_pairs_start",
                "constraint_pairs_len",
                "witnesses_start",
                "witnesses_len",
                "copied_groups_start",
                "copied_groups_len",
            }) |field| {
                if (@field(left, field) != @field(right, field)) return false;
            }
            // `WhereMarkerCopyOrigin` is a portable serialized extern union:
            // each arm is a canonical sequence of u32 words and all inactive
            // words are required to be zero. Comparing those declared ABI
            // words avoids both untagged-union reflection and struct padding.
            return portableExternUnionWordsEqual(
                ModuleEnv.WhereMarkerCopyOrigin,
                left.origin,
                right.origin,
            );
        }
        if (T == ModuleEnv.DispatchSettlementSource) {
            return left.kind == right.kind and portableExternUnionWordsEqual(
                ModuleEnv.DispatchSettlementSource.Payload,
                left.payload,
                right.payload,
            );
        }
        return std.meta.eql(left, right);
    }

    fn expectCanonicalSliceEqual(comptime T: type, expected: []const T, actual: []const T) !void {
        try std.testing.expectEqual(expected.len, actual.len);
        for (expected, actual) |expected_item, actual_item| {
            try std.testing.expect(canonicalValueEqual(T, expected_item, actual_item));
        }
    }

    pub fn expectRuntimeOwnerRestored(
        self: *const @This(),
        store: *const TypesStore,
        env: *const ModuleEnv,
    ) !void {
        try std.testing.expectEqual(self.store_savepoint_active, store.savepoint_active);
        try std.testing.expectEqual(self.store_savepoint_depth, store.savepoint_depth);
        try std.testing.expectEqual(self.store_savepoint_baseline_slots, store.savepoint_baseline_slots);
        try std.testing.expectEqual(self.store_savepoint_baseline_descs, store.savepoint_baseline_descs);
        try std.testing.expectEqual(
            self.store_savepoint_baseline_nominal_decls,
            store.savepoint_baseline_nominal_decls,
        );
        try expectCanonicalSliceEqual(SlotUndo, self.slot_trail, store.slot_trail.items);
        try expectCanonicalSliceEqual(DescUndo, self.desc_trail, store.desc_trail.items);
        try expectCanonicalSliceEqual(RootMetaUndo, self.root_meta_trail, store.root_meta_trail.items);
        try expectCanonicalSliceEqual(UnionRankUndo, self.union_rank_trail, store.union_rank_trail.items);
        try expectCanonicalSliceEqual(NominalDeclUndo, self.nominal_decl_trail, store.nominal_decl_trail.items);
        try std.testing.expectEqual(self.ident_entry_count, env.common.idents.interner.entry_count);
        try std.testing.expectEqualSlices(u8, self.ident_bytes, env.common.idents.interner.bytes.items.items);
        try expectCanonicalSliceEqual(base.SmallStringInterner.Idx, self.ident_index, env.common.idents.interner.index.items.items);
        try std.testing.expectEqual(self.ident_savepoint_depth, env.common.idents.interner.savepoint_depth);
        try std.testing.expectEqual(self.identity_count, env.module_identities.count());
        try std.testing.expectEqualSlices(u8, self.identity_bytes, env.module_identities.bytes.items.items);
        try expectCanonicalSliceEqual(base.SerialStringInterner.Range, self.identity_ranges, env.module_identities.ranges.items.items);
        try std.testing.expectEqualSlices(u32, self.identity_index, env.module_identities.index.items.items);
        try std.testing.expectEqual(self.identity_savepoint_depth, env.module_identities.savepoint_depth);
        try expectCanonicalSliceEqual(base.Ident.Idx, self.identity_displays, env.module_identity_displays.items.items);
    }

    pub fn expectRetainedProofPrefixesEqual(
        self: *const @This(),
        env: *const ModuleEnv,
        mapping: *const VarMapping,
    ) !void {
        try std.testing.expect(env.where_marker_copy_steps.items.items.len >= self.copy_steps.len);
        try std.testing.expect(env.where_marker_copy_pairs.items.items.len >= self.copy_pairs.len);
        try std.testing.expect(env.where_marker_copy_occurrences.items.items.len >= self.copy_occurrences.len);
        try std.testing.expect(env.where_marker_constraint_copy_pairs.items.items.len >= self.constraint_pairs.len);
        try std.testing.expect(env.where_marker_copy_witnesses.items.items.len >= self.copy_witnesses.len);
        try std.testing.expect(env.copied_open_literal_groups.items.items.len >= self.copied_literal_groups.len);
        try std.testing.expect(env.copied_open_literal_events.items.items.len >= self.copied_literal_events.len);
        try std.testing.expect(env.where_marker_platform_substitutions.items.items.len >= self.platform_substitutions.len);
        try std.testing.expect(env.selected_receiver_anchors.items.items.len >= self.selected_anchors.len);
        try std.testing.expect(env.dispatch_settlement_sources.items.items.len >= self.settlement_sources.len);
        try expectCanonicalSliceEqual(ModuleEnv.WhereMarkerCopyStep, self.copy_steps, env.where_marker_copy_steps.items.items[0..self.copy_steps.len]);
        try expectCanonicalSliceEqual(ModuleEnv.WhereMarkerCopyPair, self.copy_pairs, env.where_marker_copy_pairs.items.items[0..self.copy_pairs.len]);
        try expectCanonicalSliceEqual(ModuleEnv.WhereMarkerCopyOccurrence, self.copy_occurrences, env.where_marker_copy_occurrences.items.items[0..self.copy_occurrences.len]);
        try expectCanonicalSliceEqual(ModuleEnv.WhereMarkerConstraintCopyPair, self.constraint_pairs, env.where_marker_constraint_copy_pairs.items.items[0..self.constraint_pairs.len]);
        try expectCanonicalSliceEqual(ModuleEnv.WhereMarkerCopyWitness, self.copy_witnesses, env.where_marker_copy_witnesses.items.items[0..self.copy_witnesses.len]);
        try expectCanonicalSliceEqual(ModuleEnv.CopiedOpenLiteralGroup, self.copied_literal_groups, env.copied_open_literal_groups.items.items[0..self.copied_literal_groups.len]);
        try expectCanonicalSliceEqual(ModuleEnv.CopiedOpenLiteralEvent, self.copied_literal_events, env.copied_open_literal_events.items.items[0..self.copied_literal_events.len]);
        try expectCanonicalSliceEqual(ModuleEnv.WhereMarkerPlatformSubstitution, self.platform_substitutions, env.where_marker_platform_substitutions.items.items[0..self.platform_substitutions.len]);
        try expectCanonicalSliceEqual(ModuleEnv.SelectedReceiverAnchor, self.selected_anchors, env.selected_receiver_anchors.items.items[0..self.selected_anchors.len]);
        try expectCanonicalSliceEqual(ModuleEnv.DispatchSettlementSource, self.settlement_sources, env.dispatch_settlement_sources.items.items[0..self.settlement_sources.len]);
        try std.testing.expect(mapping.count() >= self.mapping_entries.len);
        for (self.mapping_entries) |entry| {
            try std.testing.expectEqual(entry.destination, mapping.get(entry.source).?);
        }
    }

    pub fn expectEqual(
        self: *const @This(),
        store: *const TypesStore,
        env: *const ModuleEnv,
        mapping: *const VarMapping,
    ) !void {
        const actual_store = try serializeStore(std.testing.allocator, store);
        defer std.testing.allocator.free(actual_store);
        try std.testing.expectEqualSlices(u8, self.store_bytes, actual_store);
        try self.expectRuntimeOwnerRestored(store, env);
        try std.testing.expectEqual(self.copy_steps.len, env.where_marker_copy_steps.items.items.len);
        try std.testing.expectEqual(self.copy_pairs.len, env.where_marker_copy_pairs.items.items.len);
        try std.testing.expectEqual(self.copy_occurrences.len, env.where_marker_copy_occurrences.items.items.len);
        try std.testing.expectEqual(self.constraint_pairs.len, env.where_marker_constraint_copy_pairs.items.items.len);
        try std.testing.expectEqual(self.copy_witnesses.len, env.where_marker_copy_witnesses.items.items.len);
        try std.testing.expectEqual(self.copied_literal_groups.len, env.copied_open_literal_groups.items.items.len);
        try std.testing.expectEqual(self.copied_literal_events.len, env.copied_open_literal_events.items.items.len);
        try std.testing.expectEqual(self.platform_substitutions.len, env.where_marker_platform_substitutions.items.items.len);
        try std.testing.expectEqual(self.selected_anchors.len, env.selected_receiver_anchors.items.items.len);
        try std.testing.expectEqual(self.settlement_sources.len, env.dispatch_settlement_sources.items.items.len);
        try std.testing.expectEqual(self.mapping_entries.len, mapping.count());
        try self.expectRetainedProofPrefixesEqual(env, mapping);
    }

    pub fn deinit(self: *@This()) void {
        self.arena.deinit();
        self.* = undefined;
    }
};

/// Cross-file observability for Check-owned transaction tests. The namespace
/// is empty in production builds, so neither the snapshot nor the owner-state
/// query becomes a compiler API.
pub const testing = if (builtin.is_test) struct {
    pub const DestinationSnapshot = CrossCopyDestinationSnapshot;

    pub fn expectPublicCopyClosed(store: *const TypesStore, env: *const ModuleEnv) !void {
        try expectPublicCrossCopyClosed(store, env);
    }
} else struct {};

const CrossCopyOomSource = struct {
    env: ModuleEnv,
    store: TypesStore,
    generic_root: Var,
    nominal_statement: u32,
    preseed_source: Var,
    binding_root: Var,
    binding_receiver: Var,
    binding_constraint_index: u32,

    fn init() !@This() {
        const allocator = std.testing.allocator;
        var env = try ModuleEnv.init(allocator, "CrossCopyOomSource");
        errdefer env.deinit();
        try env.setContentIdentity([_]u8{0x41} ** 32);
        var store = try TypesStore.initCapacity(allocator, 32, 32);
        errdefer store.deinit();

        const nominal_statement: u32 = 5;
        const declaration_name = try env.insertIdent(base.Ident.for_text("CrossCopyOomNominal"));
        const formal_name = try env.insertIdent(base.Ident.for_text("cross_copy_oom_formal"));
        const formal = try store.freshFromContent(.{ .rigid = Rigid.init(formal_name) });
        const formals = try store.appendVars(&.{formal});
        const source_decl = try types_mod.NominalType.Source.initChecked(
            try types_mod.SourceDecl.fromStatementChecked(nominal_statement),
            false,
            false,
        );
        _ = try store.registerNominalDecl(.{
            .ident = .{ .ident_idx = declaration_name },
            .origin_module = env.selfModuleIdentity(),
            .source = source_decl,
            .formals = formals,
            .backing = formal,
            .flags = .{ .valid = true },
        });

        const preseed_source = try store.freshFromContent(.{ .structure = .empty_record });
        const nominal = try store.freshFromContent(try store.mkNominalWithSourceDecl(
            .{ .ident_idx = declaration_name },
            &.{preseed_source},
            env.selfModuleIdentity(),
            nominal_statement,
            false,
        ));
        const callable = try store.freshFromContent(try store.mkFuncPure(&.{nominal}, nominal));
        const marker_path = try store.appendWhereMethodMarkerPathSteps(&.{
            .{ .kind = @intFromEnum(WhereMethodMarkerPathStep.Kind.fn_ret), .index = 0, .arity = 0, .name = 0, .origin_module = 0, .source_decl = 0 },
            .{
                .kind = @intFromEnum(WhereMethodMarkerPathStep.Kind.nominal_arg),
                .index = 0,
                .arity = 1,
                .name = @bitCast(declaration_name),
                .origin_module = @intFromEnum(env.selfModuleIdentity()),
                .source_decl = @bitCast(source_decl.sourceDecl()),
            },
        });
        const none = std.math.maxInt(u32);
        const markers = try store.appendWhereMethodMarkerContracts(&.{.{
            .producer_owner_node = none,
            .producer_where_node = none,
            .producer_method_name = none,
            .position = @intFromEnum(WhereMethodMarkerContract.Position.nested),
            .widened = 1,
            .ready = 1,
            .path_start = @intFromEnum(marker_path.start),
            .path_len = 2,
        }});
        const method_name = try env.insertIdent(base.Ident.for_text("cross_copy_oom_method"));
        const generic_constraints = try store.appendStaticDispatchConstraints(&.{
            .{
                .fn_name = method_name,
                .fn_var = callable,
                .origin = .{ .where_clause = .{} },
                .where_method_markers = markers,
            },
            .{
                .fn_name = env.idents.from_quote,
                .fn_var = callable,
                .origin = .{ .from_literal = .quote },
            },
        });
        const generic_root = try store.freshFromContent(.{
            .flex = Flex.init().withConstraints(generic_constraints),
        });

        // The binding root first copies `generic_root`; both later components
        // reference it and must take an authenticated binding-root reuse cut.
        // Each component also owns one distinct fresh literal receiver, so the
        // two tagged copied-literal group arms are exercised independently.
        const binding_root_items = try store.appendVars(&.{generic_root});
        const binding_root = try store.freshFromContent(.{ .structure = .{ .tuple = .{
            .elems = binding_root_items,
        } } });
        const receiver_literal_constraints = try store.appendStaticDispatchConstraints(&.{.{
            .fn_name = env.idents.from_numeral,
            .fn_var = callable,
            .origin = .{ .from_literal = .{
                .numeral = types_mod.NumeralInfo.testOnlyInt(23, false, base.Region.zero()),
            } },
        }});
        const receiver_literal = try store.freshFromContent(.{
            .flex = Flex.init().withConstraints(receiver_literal_constraints),
        });
        const receiver_items = try store.appendVars(&.{ receiver_literal, generic_root });
        const binding_receiver = try store.freshFromContent(.{ .structure = .{ .tuple = .{
            .elems = receiver_items,
        } } });
        const function_literal_constraints = try store.appendStaticDispatchConstraints(&.{.{
            .fn_name = env.idents.from_quote,
            .fn_var = callable,
            .origin = .{ .from_literal = .quote },
        }});
        const function_literal = try store.freshFromContent(.{
            .flex = Flex.init().withConstraints(function_literal_constraints),
        });
        const binding_callable = try store.freshFromContent(try store.mkFuncPure(
            &.{ function_literal, generic_root, nominal },
            nominal,
        ));
        const binding_constraint_range = try store.appendStaticDispatchConstraints(&.{.{
            .fn_name = method_name,
            .fn_var = binding_callable,
            .origin = .{ .where_clause = .{} },
            .where_method_markers = markers,
        }});

        return .{
            .env = env,
            .store = store,
            .generic_root = generic_root,
            .nominal_statement = nominal_statement,
            .preseed_source = preseed_source,
            .binding_root = binding_root,
            .binding_receiver = binding_receiver,
            .binding_constraint_index = @intFromEnum(binding_constraint_range.start),
        };
    }

    fn deinit(self: *@This()) void {
        self.store.deinit();
        self.env.deinit();
        self.* = undefined;
    }
};

test "cross-module copy transaction rejects nesting before opening owned state" {
    const allocator = std.testing.allocator;
    var dest_env = try ModuleEnv.init(allocator, "");
    defer dest_env.deinit();
    var dest_store = try TypesStore.initCapacity(allocator, 8, 8);
    defer dest_store.deinit();
    var mapping = VarMapping.init(allocator);
    defer mapping.deinit();

    var outer: CrossModuleCopyTransaction = undefined;
    try outer.begin(&dest_store, &dest_env, &mapping, allocator);
    defer outer.rollback();
    try std.testing.expectEqual(&outer, active_cross_module_copy_transaction.?);
    try std.testing.expect(!CrossModuleCopyTransaction.canBegin());
    const store_depth = dest_store.savepoint_depth;
    const ident_depth = dest_env.common.idents.interner.savepoint_depth;
    const identity_depth = dest_env.module_identities.savepoint_depth;
    const mapping_count = mapping.count();

    var nested: CrossModuleCopyTransaction = undefined;
    try std.testing.expectError(
        error.NestedPublicCopy,
        nested.begin(&dest_store, &dest_env, &mapping, allocator),
    );
    try std.testing.expectEqual(&outer, active_cross_module_copy_transaction.?);
    try std.testing.expectEqual(store_depth, dest_store.savepoint_depth);
    try std.testing.expectEqual(ident_depth, dest_env.common.idents.interner.savepoint_depth);
    try std.testing.expectEqual(identity_depth, dest_env.module_identities.savepoint_depth);
    try std.testing.expectEqual(mapping_count, mapping.count());

    outer.rollback();
    try std.testing.expect(active_cross_module_copy_transaction == null);
    try std.testing.expectEqual(@as(u32, 0), dest_store.savepoint_depth);
    try std.testing.expectEqual(@as(u16, 0), dest_env.common.idents.interner.savepoint_depth);
    try std.testing.expectEqual(@as(u16, 0), dest_env.module_identities.savepoint_depth);
}

test "cross-module copy transaction begin is an exhaustive OOM no-op" {
    const allocator = std.testing.allocator;
    var saw_oom = false;
    var reached_success = false;
    for (0..16) |fail_index| {
        var dest_env = try ModuleEnv.init(allocator, "");
        defer dest_env.deinit();
        var dest_store = try TypesStore.initCapacity(allocator, 8, 8);
        defer dest_store.deinit();
        var mapping = VarMapping.init(allocator);
        defer mapping.deinit();
        const ident_count = dest_env.common.idents.interner.entry_count;
        const identity_count = dest_env.module_identities.count();
        const identity_displays_len = dest_env.module_identity_displays.items.items.len;

        var failing = std.testing.FailingAllocator.init(allocator, .{
            .fail_index = fail_index,
            .resize_fail_index = 0,
        });
        const saved_env_gpa = dest_env.gpa;
        const saved_store_gpa = dest_store.gpa;
        dest_env.gpa = failing.allocator();
        dest_store.gpa = failing.allocator();
        var transaction: CrossModuleCopyTransaction = undefined;
        const result = transaction.begin(
            &dest_store,
            &dest_env,
            &mapping,
            failing.allocator(),
        );
        dest_env.gpa = saved_env_gpa;
        dest_store.gpa = saved_store_gpa;

        if (result) {
            var transaction_open = true;
            errdefer if (transaction_open) transaction.rollback();
            try std.testing.expect(!failing.has_induced_failure);
            transaction.rollback();
            transaction_open = false;
            reached_success = true;
        } else |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            saw_oom = true;
        }
        try std.testing.expect(active_cross_module_copy_transaction == null);
        try std.testing.expectEqual(@as(u32, 0), dest_store.savepoint_depth);
        try std.testing.expectEqual(@as(u16, 0), dest_env.common.idents.interner.savepoint_depth);
        try std.testing.expectEqual(@as(u16, 0), dest_env.module_identities.savepoint_depth);
        try std.testing.expectEqual(ident_count, dest_env.common.idents.interner.entry_count);
        try std.testing.expectEqual(identity_count, dest_env.module_identities.count());
        try std.testing.expectEqual(identity_displays_len, dest_env.module_identity_displays.items.items.len);
        try std.testing.expectEqual(@as(usize, 0), mapping.count());
        if (reached_success) break;
    }
    try std.testing.expect(saw_oom);
    try std.testing.expect(reached_success);
}

fn expectPublicCrossCopyClosed(store: *const TypesStore, env: *const ModuleEnv) !void {
    try std.testing.expect(active_cross_module_copy_transaction == null);
    try std.testing.expect(!store.savepoint_active);
    try std.testing.expectEqual(@as(u32, 0), store.savepoint_depth);
    try std.testing.expectEqual(@as(u16, 0), env.common.idents.interner.savepoint_depth);
    try std.testing.expectEqual(@as(u16, 0), env.module_identities.savepoint_depth);
}

test "public cross-module generic copy is an exhaustive OOM transaction" {
    const allocator = std.testing.allocator;
    var source = try CrossCopyOomSource.init();
    defer source.deinit();

    var saw_oom = false;
    var reached_success = false;
    for (0..256) |fail_index| {
        var dest_env = try ModuleEnv.init(allocator, "CrossCopyGenericDestination");
        defer dest_env.deinit();
        try dest_env.setContentIdentity([_]u8{0x51} ** 32);
        var dest_store = try TypesStore.initCapacity(allocator, 8, 8);
        defer dest_store.deinit();
        const preseed_destination = try dest_store.freshFromContent(.{ .structure = .empty_record });
        var mapping = VarMapping.init(allocator);
        defer mapping.deinit();
        // Preserve an unrelated caller substitution across every failure. A
        // preseed cut inside the copied graph requires its own finite semantic
        // origin and is deliberately not fabricated by this OOM fixture.
        try mapping.put(source.binding_root, preseed_destination);

        var baseline = try CrossCopyDestinationSnapshot.capture(
            allocator,
            &dest_store,
            &dest_env,
            &mapping,
        );
        defer baseline.deinit();

        var failing = std.testing.FailingAllocator.init(allocator, .{
            .fail_index = fail_index,
            .resize_fail_index = 0,
        });
        const failing_allocator = failing.allocator();
        const saved_store_gpa = dest_store.gpa;
        const saved_env_gpa = dest_env.gpa;
        const saved_mapping_allocator = mapping.allocator;
        dest_store.gpa = failing_allocator;
        dest_env.gpa = failing_allocator;
        mapping.allocator = failing_allocator;
        const result = copyVarWithMarkerLineageResult(
            &source.store,
            &dest_store,
            source.generic_root,
            &mapping,
            null,
            &source.env,
            &dest_env,
            failing_allocator,
            .{ .external_cir_node = .{ .node = @intFromEnum(source.generic_root) } },
        );
        mapping.allocator = saved_mapping_allocator;
        dest_env.gpa = saved_env_gpa;
        dest_store.gpa = saved_store_gpa;

        if (result) |copied| {
            try std.testing.expect(!failing.has_induced_failure);
            try std.testing.expect(copied.copy_step != null);
            try std.testing.expect(mapping.count() > baseline.mapping_entries.len);
            try std.testing.expectEqual(preseed_destination, mapping.get(source.binding_root).?);
            try std.testing.expect(dest_env.where_marker_copy_steps.items.items.len > baseline.copy_steps.len);
            try std.testing.expect(dest_env.where_marker_copy_pairs.items.items.len > baseline.copy_pairs.len);
            try std.testing.expect(dest_env.where_marker_copy_occurrences.items.items.len > baseline.copy_occurrences.len);
            try std.testing.expect(dest_env.where_marker_constraint_copy_pairs.items.items.len > baseline.constraint_pairs.len);
            try std.testing.expect(dest_env.where_marker_copy_witnesses.items.items.len > baseline.copy_witnesses.len);
            try std.testing.expect(dest_env.copied_open_literal_groups.items.items.len > baseline.copied_literal_groups.len);
            try std.testing.expect(dest_env.copied_open_literal_events.items.items.len > baseline.copied_literal_events.len);
            try std.testing.expect(dest_env.selected_receiver_anchors.items.items.len > baseline.selected_anchors.len);
            try std.testing.expect(dest_env.dispatch_settlement_sources.items.items.len > baseline.settlement_sources.len);
            try expectPublicCrossCopyClosed(&dest_store, &dest_env);
            reached_success = true;
            break;
        } else |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            saw_oom = true;
            try baseline.expectEqual(&dest_store, &dest_env, &mapping);
            try expectPublicCrossCopyClosed(&dest_store, &dest_env);
        }
    }
    try std.testing.expect(saw_oom);
    try std.testing.expect(reached_success);
}

test "public cross-module nominal declaration copy is an exhaustive OOM transaction" {
    const allocator = std.testing.allocator;
    var source = try CrossCopyOomSource.init();
    defer source.deinit();
    const source_hash = source.env.contentIdentityHash() orelse return error.TestUnexpectedResult;

    var saw_oom = false;
    var reached_success = false;
    for (0..256) |fail_index| {
        var dest_env = try ModuleEnv.init(allocator, "CrossCopyNominalDestination");
        defer dest_env.deinit();
        try dest_env.setContentIdentity([_]u8{0x52} ** 32);
        var dest_store = try TypesStore.initCapacity(allocator, 8, 8);
        defer dest_store.deinit();
        const retained_destination = try dest_store.freshFromContent(.{ .structure = .empty_tag_union });
        var mapping = VarMapping.init(allocator);
        defer mapping.deinit();
        try mapping.put(source.generic_root, retained_destination);

        var baseline = try CrossCopyDestinationSnapshot.capture(
            allocator,
            &dest_store,
            &dest_env,
            &mapping,
        );
        defer baseline.deinit();

        var failing = std.testing.FailingAllocator.init(allocator, .{
            .fail_index = fail_index,
            .resize_fail_index = 0,
        });
        const failing_allocator = failing.allocator();
        const saved_store_gpa = dest_store.gpa;
        const saved_env_gpa = dest_env.gpa;
        const saved_mapping_allocator = mapping.allocator;
        dest_store.gpa = failing_allocator;
        dest_env.gpa = failing_allocator;
        mapping.allocator = failing_allocator;
        const result = ensureNominalDeclForStatement(
            &source.store,
            &dest_store,
            source.nominal_statement,
            &mapping,
            &source.env,
            &dest_env,
            failing_allocator,
        );
        mapping.allocator = saved_mapping_allocator;
        dest_env.gpa = saved_env_gpa;
        dest_store.gpa = saved_store_gpa;

        if (result) {
            try std.testing.expect(!failing.has_induced_failure);
            const translated_origin = dest_env.lookupModuleIdentity(source_hash) orelse
                return error.TestUnexpectedResult;
            try std.testing.expect(
                dest_store.lookupNominalDeclByKey(translated_origin, source.nominal_statement) != null,
            );
            try std.testing.expectEqual(baseline.mapping_entries.len, mapping.count());
            try std.testing.expectEqual(retained_destination, mapping.get(source.generic_root).?);
            try expectPublicCrossCopyClosed(&dest_store, &dest_env);
            reached_success = true;
            break;
        } else |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            saw_oom = true;
            try baseline.expectEqual(&dest_store, &dest_env, &mapping);
            try expectPublicCrossCopyClosed(&dest_store, &dest_env);
        }
    }
    try std.testing.expect(saw_oom);
    try std.testing.expect(reached_success);
}

test "public cross-module binding copy is an exhaustive OOM transaction" {
    const allocator = std.testing.allocator;
    const requirement_ordinal: u32 = 7;
    var source = try CrossCopyOomSource.init();
    defer source.deinit();
    const source_constraint = source.store.getStaticDispatchConstraintAt(
        source.binding_constraint_index,
    );

    var saw_oom = false;
    var reached_success = false;
    for (0..256) |fail_index| {
        var dest_env = try ModuleEnv.init(allocator, "CrossCopyBindingDestination");
        defer dest_env.deinit();
        try dest_env.setContentIdentity([_]u8{0x53} ** 32);
        var dest_store = try TypesStore.initCapacity(allocator, 8, 8);
        defer dest_store.deinit();
        var mapping = VarMapping.init(allocator);
        defer mapping.deinit();

        const copied_binding = try copyVarWithMarkerLineageSupport(
            &source.store,
            &dest_store,
            source.binding_root,
            &mapping,
            null,
            &source.env,
            &dest_env,
            allocator,
            .{ .external_cir_node = .{ .node = @intFromEnum(source.binding_root) } },
        );
        const binding_root_step = copied_binding.copy_step orelse
            return error.TestUnexpectedResult;
        const binding_reuse_destination = mapping.get(source.generic_root) orelse
            return error.TestUnexpectedResult;
        var binding_mapping_origins = try initBindingCodecMappingOrigins(
            allocator,
            &dest_env,
            &mapping,
            binding_root_step,
        );
        defer binding_mapping_origins.deinit();
        const binding_origins_count = binding_mapping_origins.count();
        var baseline_binding_origins = try binding_mapping_origins.cloneWithAllocator(allocator);
        defer baseline_binding_origins.deinit();
        var baseline = try CrossCopyDestinationSnapshot.capture(
            allocator,
            &dest_store,
            &dest_env,
            &mapping,
        );
        defer baseline.deinit();

        var failing = std.testing.FailingAllocator.init(allocator, .{
            .fail_index = fail_index,
            .resize_fail_index = 0,
        });
        const failing_allocator = failing.allocator();
        const saved_store_gpa = dest_store.gpa;
        const saved_env_gpa = dest_env.gpa;
        const saved_mapping_allocator = mapping.allocator;
        const saved_origins_allocator = binding_mapping_origins.allocator;
        dest_store.gpa = failing_allocator;
        dest_env.gpa = failing_allocator;
        mapping.allocator = failing_allocator;
        binding_mapping_origins.allocator = failing_allocator;
        const result = copyImportedConstraintWithMarkerLineage(
            &source.store,
            &dest_store,
            source.binding_receiver,
            source.binding_constraint_index,
            source_constraint,
            &mapping,
            &binding_mapping_origins,
            &source.env,
            &dest_env,
            failing_allocator,
            binding_root_step,
            requirement_ordinal,
        );
        binding_mapping_origins.allocator = saved_origins_allocator;
        mapping.allocator = saved_mapping_allocator;
        dest_env.gpa = saved_env_gpa;
        dest_store.gpa = saved_store_gpa;

        if (result) |copied| {
            try std.testing.expect(!failing.has_induced_failure);
            try std.testing.expectEqual(baseline.copy_steps.len + 2, dest_env.where_marker_copy_steps.items.items.len);
            try std.testing.expectEqual(baseline.copied_literal_groups.len + 2, dest_env.copied_open_literal_groups.items.items.len);
            try std.testing.expectEqual(baseline.copied_literal_events.len + 2, dest_env.copied_open_literal_events.items.items.len);
            try std.testing.expectEqual(baseline.settlement_sources.len + 3, dest_env.dispatch_settlement_sources.items.items.len);
            try std.testing.expectEqual(baseline.selected_anchors.len + 3, dest_env.selected_receiver_anchors.items.items.len);
            try std.testing.expect(copied.receiver_copy_step != copied.function_copy_step);
            const receiver_step = dest_env.where_marker_copy_steps.items.items[copied.receiver_copy_step];
            const function_step = dest_env.where_marker_copy_steps.items.items[copied.function_copy_step];
            try std.testing.expectEqual(@as(u32, @intCast(baseline.copied_literal_groups.len)), receiver_step.copied_groups_start);
            try std.testing.expectEqual(@as(u32, 1), receiver_step.copied_groups_len);
            try std.testing.expectEqual(@as(u32, @intCast(baseline.copied_literal_groups.len + 1)), function_step.copied_groups_start);
            try std.testing.expectEqual(@as(u32, 1), function_step.copied_groups_len);
            const receiver_group = dest_env.copied_open_literal_groups.items.items[
                receiver_step.copied_groups_start
            ];
            const receiver_component = receiver_group.component.decodedBindingCodecReceiver() orelse
                return error.TestUnexpectedResult;
            try std.testing.expectEqual(binding_root_step, receiver_component.binding_root_step);
            try std.testing.expectEqual(requirement_ordinal, receiver_component.requirement_ordinal);
            try std.testing.expectEqual(@as(u32, 1), receiver_group.events_len);
            const function_group = dest_env.copied_open_literal_groups.items.items[
                function_step.copied_groups_start
            ];
            const function_component = function_group.component.decodedBindingCodecFunction() orelse
                return error.TestUnexpectedResult;
            try std.testing.expectEqual(binding_root_step, function_component.binding_root_step);
            try std.testing.expectEqual(requirement_ordinal, function_component.requirement_ordinal);
            try std.testing.expectEqual(@as(u32, 1), function_group.events_len);
            try std.testing.expectEqual(binding_reuse_destination, mapping.get(source.generic_root).?);
            try std.testing.expect(binding_mapping_origins.count() > binding_origins_count);

            for ([_]ModuleEnv.WhereMarkerCopyStep{ receiver_step, function_step }) |component_step| {
                var matching_reuse_cuts: usize = 0;
                const occurrences = dest_env.where_marker_copy_occurrences.items.items[component_step.occurrences_start..][0..component_step.occurrences_len];
                for (dest_env.where_marker_copy_witnesses.items.items[component_step.witnesses_start..][0..component_step.witnesses_len]) |witness| {
                    if (witness.decodedAction() != .binding_codec_reuse_cut or
                        witness.decodedAuxiliaryOriginKind() != .binding_copy_pair or
                        witness.child_occurrence_offset >= occurrences.len)
                    {
                        continue;
                    }
                    if (occurrences[witness.child_occurrence_offset].raw_source_var ==
                        @intFromEnum(source.generic_root))
                    {
                        matching_reuse_cuts += 1;
                    }
                }
                try std.testing.expectEqual(@as(usize, 1), matching_reuse_cuts);
            }

            const groups_after_fresh_components = dest_env.copied_open_literal_groups.items.items.len;
            const events_after_fresh_components = dest_env.copied_open_literal_events.items.items.len;
            const repeated = try copyImportedConstraintWithMarkerLineage(
                &source.store,
                &dest_store,
                source.binding_receiver,
                source.binding_constraint_index,
                source_constraint,
                &mapping,
                &binding_mapping_origins,
                &source.env,
                &dest_env,
                allocator,
                binding_root_step,
                requirement_ordinal + 1,
            );
            try std.testing.expectEqual(
                groups_after_fresh_components,
                dest_env.copied_open_literal_groups.items.items.len,
            );
            try std.testing.expectEqual(
                events_after_fresh_components,
                dest_env.copied_open_literal_events.items.items.len,
            );
            const repeated_steps = [_]struct {
                current: u32,
                expected_origin: u32,
            }{
                .{
                    .current = repeated.receiver_copy_step,
                    .expected_origin = copied.receiver_copy_step,
                },
                .{
                    .current = repeated.function_copy_step,
                    .expected_origin = copied.function_copy_step,
                },
            };
            for (repeated_steps) |expected| {
                const repeated_step = dest_env.where_marker_copy_steps.items.items[expected.current];
                try std.testing.expectEqual(@as(u32, 0), repeated_step.copied_groups_len);
                var matching_root_cuts: usize = 0;
                for (dest_env.where_marker_copy_witnesses.items.items[repeated_step.witnesses_start..][0..repeated_step.witnesses_len]) |witness| {
                    if (witness.decodedEdgeKind() != .root_copy_action or
                        witness.decodedAction() != .binding_codec_reuse_cut)
                    {
                        continue;
                    }
                    try std.testing.expectEqual(
                        ModuleEnv.WhereMarkerCopyWitness.AuxiliaryOriginKind.binding_copy_pair,
                        witness.decodedAuxiliaryOriginKind().?,
                    );
                    try std.testing.expectEqual(expected.expected_origin, witness.auxiliary_origin_step);
                    matching_root_cuts += 1;
                }
                try std.testing.expectEqual(@as(usize, 1), matching_root_cuts);
            }
            try expectPublicCrossCopyClosed(&dest_store, &dest_env);
            reached_success = true;
            break;
        } else |err| {
            try std.testing.expectEqual(error.OutOfMemory, err);
            try std.testing.expect(failing.has_induced_failure);
            saw_oom = true;
            try baseline.expectEqual(&dest_store, &dest_env, &mapping);
            try std.testing.expectEqual(binding_origins_count, binding_mapping_origins.count());
            var origin_iter = baseline_binding_origins.iterator();
            while (origin_iter.next()) |entry| {
                try std.testing.expect(std.meta.eql(
                    entry.value_ptr.*,
                    binding_mapping_origins.get(entry.key_ptr.*) orelse
                        return error.TestUnexpectedResult,
                ));
            }
            try expectPublicCrossCopyClosed(&dest_store, &dest_env);
        }
    }
    try std.testing.expect(saw_oom);
    try std.testing.expect(reached_success);
}

test "copy_import publishes one exact multi-literal group across disjoint equal-number namespaces" {
    const allocator = std.testing.allocator;
    var source_env = try ModuleEnv.init(allocator, "CopiedLiteralSource");
    defer source_env.deinit();
    try source_env.setContentIdentity([_]u8{0x61} ** 32);
    var dest_env = try ModuleEnv.init(allocator, "CopiedLiteralDestination");
    defer dest_env.deinit();
    try dest_env.setContentIdentity([_]u8{0x62} ** 32);

    var source_store = try TypesStore.initCapacity(allocator, 16, 16);
    defer source_store.deinit();
    var dest_store = try TypesStore.initCapacity(allocator, 16, 16);
    defer dest_store.deinit();

    const numeral_fn = try source_store.freshFromContent(.{ .structure = .empty_record });
    const ordinary_fn = try source_store.freshFromContent(.{ .structure = .empty_record });
    const quote_fn = try source_store.freshFromContent(.{ .structure = .empty_record });
    const interpolation_fn = try source_store.freshFromContent(.{ .structure = .empty_record });
    const ordinary_name = try source_env.insertIdent(base.Ident.for_text("copied_literal_ordinary"));
    const source_constraints = try source_store.appendStaticDispatchConstraints(&.{
        .{
            .fn_name = source_env.idents.from_numeral,
            .fn_var = numeral_fn,
            .origin = .{ .from_literal = .{
                .numeral = types_mod.NumeralInfo.testOnlyInt(17, false, base.Region.zero()),
            } },
        },
        .{
            .fn_name = ordinary_name,
            .fn_var = ordinary_fn,
            .origin = .{ .where_clause = .{} },
        },
        .{
            .fn_name = source_env.idents.from_quote,
            .fn_var = quote_fn,
            .origin = .{ .from_literal = .quote },
        },
        .{
            .fn_name = source_env.idents.from_interpolation,
            .fn_var = interpolation_fn,
            .origin = .{ .from_literal = .interpolation },
        },
    });
    const source_root = try source_store.freshFromContent(.{
        .flex = Flex.init().withConstraints(source_constraints),
    });

    var mapping = VarMapping.init(allocator);
    defer mapping.deinit();
    const copied = try copyVarWithMarkerLineageResult(
        &source_store,
        &dest_store,
        source_root,
        &mapping,
        null,
        &source_env,
        &dest_env,
        allocator,
        .{ .external_cir_node = .{ .node = @intFromEnum(source_root) } },
    );
    const step_index = copied.copy_step orelse return error.TestUnexpectedResult;
    const step = dest_env.where_marker_copy_steps.items.items[step_index];
    try std.testing.expectEqual(@as(u32, 0), step.copied_groups_start);
    try std.testing.expectEqual(@as(u32, 1), step.copied_groups_len);
    try std.testing.expectEqual(@as(usize, 1), dest_env.copied_open_literal_groups.items.items.len);
    try std.testing.expectEqual(@as(usize, 3), dest_env.copied_open_literal_events.items.items.len);

    const copied_content = dest_store.resolveVar(copied.var_).desc.content;
    try std.testing.expect(copied_content == .flex);
    const destination_constraints = copied_content.flex.constraints;
    const group = dest_env.copied_open_literal_groups.items.items[step.copied_groups_start];
    try std.testing.expect(group.hasCanonicalTags());
    try std.testing.expectEqual(step_index, group.copy_step_index);
    try std.testing.expectEqual(step.root_occurrence_offset, group.receiver_occurrence_offset);
    try std.testing.expectEqual(ModuleEnv.CopiedOpenLiteralComponent.Kind.root_graph, group.component.decodedKind().?);
    try std.testing.expectEqual(@as(u32, @intFromEnum(source_constraints.start)), group.source_constraints_start);
    try std.testing.expectEqual(@as(u32, @intFromEnum(destination_constraints.start)), group.destination_constraints_start);
    try std.testing.expectEqual(@as(u32, 4), group.source_constraints_len);
    try std.testing.expectEqual(@as(u32, 4), group.destination_constraints_len);
    try std.testing.expectEqual(group.source_constraints_start, group.destination_constraints_start);
    try std.testing.expectEqual(@as(u32, 0), group.events_start);
    try std.testing.expectEqual(@as(u32, 3), group.events_len);

    const constraint_pairs = dest_env.where_marker_constraint_copy_pairs.items.items[step.constraint_pairs_start..][0..step.constraint_pairs_len];
    try std.testing.expectEqual(@as(usize, 4), constraint_pairs.len);
    for (constraint_pairs) |pair| {
        // Equal integers are still distinct source/destination occurrences at
        // a cross-module boundary and therefore all remain in the certificate.
        try std.testing.expectEqual(pair.source_constraint_index, pair.destination_constraint_index);
    }

    const events = dest_env.copied_open_literal_events.items.items[group.events_start..][0..group.events_len];
    const expected_offsets = [_]u32{ 0, 2, 3 };
    const expected_kinds = [_]ModuleEnv.CopiedOpenLiteralEvent.LiteralKind{
        .numeral,
        .quote,
        .interpolation,
    };
    for (events, expected_offsets, expected_kinds, 0..) |event, expected_offset, expected_kind, event_offset| {
        try std.testing.expect(event.hasCanonicalTags());
        try std.testing.expectEqual(@as(u32, 0), event.group_index);
        try std.testing.expectEqual(expected_offset, event.constraint_offset);
        try std.testing.expectEqual(expected_kind, event.decodedLiteralKind().?);
        const destination_index = group.destination_constraints_start + event.constraint_offset;
        const destination_constraint = dest_store.static_dispatch_constraints.items.items[destination_index];
        const evidence = dest_store.sliceConstraintEvidenceHandles(destination_constraint.constraint_evidence);
        try std.testing.expectEqual(@as(usize, 2), evidence.len);
        try std.testing.expectEqual(
            types_mod.ConstraintEvidenceHandle.Kind.selected_receiver_anchor,
            evidence[0].decodedKind().?,
        );
        try std.testing.expectEqual(
            types_mod.ConstraintEvidenceHandle.Kind.copied_literal_event,
            evidence[1].decodedKind().?,
        );
        try std.testing.expectEqual(@as(u32, @intCast(event_offset)), evidence[1].index);
    }
    const ordinary_constraint = dest_store.static_dispatch_constraints.items.items[
        group.destination_constraints_start + 1
    ];
    const ordinary_evidence = dest_store.sliceConstraintEvidenceHandles(
        ordinary_constraint.constraint_evidence,
    );
    try std.testing.expectEqual(@as(usize, 1), ordinary_evidence.len);
    try std.testing.expectEqual(
        types_mod.ConstraintEvidenceHandle.Kind.selected_receiver_anchor,
        ordinary_evidence[0].decodedKind().?,
    );
}

// Depth pin for the cross-module graph copy. Types crossing a module
// boundary are whatever the instantiator built, whose depth is bounded only by
// heap, so this copy must be too. A 40,000-node spine is past what a per-node
// native frame can hold on any ordinary 8 MiB stack: the recursive copy this
// replaced segfaulted on exactly this chain.
test "copy_import copies a spine deeper than any native-stack budget" {
    const allocator = std.testing.allocator;
    const depth: u32 = 40000;

    var source_env = try ModuleEnv.init(allocator, "");
    defer source_env.deinit();
    var dest_env = try ModuleEnv.init(allocator, "");
    defer dest_env.deinit();

    var source_store = try TypesStore.initCapacity(allocator, depth + 8, 8);
    defer source_store.deinit();
    var dest_store = try TypesStore.initCapacity(allocator, depth + 8, 8);
    defer dest_store.deinit();

    var current = try source_store.freshFromContent(.{ .structure = .empty_record });
    for (0..depth) |_| {
        const elems = try source_store.appendVars(&.{current});
        current = try source_store.freshFromContent(.{ .structure = .{ .tuple = .{ .elems = elems } } });
    }

    var mapping = VarMapping.init(allocator);
    defer mapping.deinit();

    const copied = try copyVar(&source_store, &dest_store, current, &mapping, null, &source_env, &dest_env, allocator);
    try std.testing.expect(dest_store.resolveVar(copied).desc.content == .structure);
}

test "copy_import destination-sorts equal rebased where-method marker paths adjacently" {
    const none = std.math.maxInt(u32);
    const path_kind = @intFromEnum(WhereMethodMarkerPathStep.Kind.record_field);
    const position = @intFromEnum(WhereMethodMarkerContract.Position.nested);
    const steps = [_]WhereMethodMarkerPathStep{
        .{ .kind = path_kind, .index = 0, .arity = 0, .name = 30, .origin_module = 0, .source_decl = 0 },
        .{ .kind = path_kind, .index = 0, .arity = 0, .name = 10, .origin_module = 0, .source_decl = 0 },
        .{ .kind = path_kind, .index = 0, .arity = 0, .name = 10, .origin_module = 0, .source_decl = 0 },
    };
    var contracts = [_]PendingImportedMarkerContract{
        .{ .contract = .{ .producer_owner_node = none, .producer_where_node = none, .producer_method_name = none, .position = position, .widened = 0, .ready = 1, .path_start = 0, .path_len = 1 }, .source_contract_offset = 0 },
        .{ .contract = .{ .producer_owner_node = none, .producer_where_node = none, .producer_method_name = none, .position = position, .widened = 0, .ready = 1, .path_start = 1, .path_len = 1 }, .source_contract_offset = 1 },
        .{ .contract = .{ .producer_owner_node = none, .producer_where_node = none, .producer_method_name = none, .position = position, .widened = 1, .ready = 1, .path_start = 2, .path_len = 1 }, .source_contract_offset = 2 },
    };

    sortImportedMarkerContracts(&contracts, &steps);
    try std.testing.expectEqual(@as(u32, 10), steps[contracts[0].contract.path_start].name);
    try std.testing.expectEqual(@as(u32, 10), steps[contracts[1].contract.path_start].name);
    try std.testing.expectEqual(@as(u32, 30), steps[contracts[2].contract.path_start].name);
    try std.testing.expectEqual(@as(u32, 1), contracts[0].source_contract_offset);
    try std.testing.expectEqual(@as(u32, 2), contracts[1].source_contract_offset);
}

test "copy_import copyVar sorts where-method paths after identifier re-interning" {
    const allocator = std.testing.allocator;
    var source_env = try ModuleEnv.init(allocator, "");
    defer source_env.deinit();
    var dest_env = try ModuleEnv.init(allocator, "");
    defer dest_env.deinit();
    var source_store = try TypesStore.initCapacity(allocator, 8, 8);
    defer source_store.deinit();
    var dest_store = try TypesStore.initCapacity(allocator, 8, 8);
    defer dest_store.deinit();

    // Source IDs order z before a; destination IDs deliberately order a
    // before z. A copied range that retained source ordering would therefore
    // violate the destination namespace's canonical path order.
    const source_z = try source_env.insertIdent(base.Ident.for_text("z_rebased_field"));
    const source_a = try source_env.insertIdent(base.Ident.for_text("a_rebased_field"));
    try std.testing.expect(@as(u32, @bitCast(source_z)) < @as(u32, @bitCast(source_a)));
    const dest_a = try dest_env.insertIdent(base.Ident.for_text("a_rebased_field"));
    const dest_z = try dest_env.insertIdent(base.Ident.for_text("z_rebased_field"));
    try std.testing.expect(@as(u32, @bitCast(dest_a)) < @as(u32, @bitCast(dest_z)));

    const path_kind = @intFromEnum(WhereMethodMarkerPathStep.Kind.record_field);
    const paths = try source_store.appendWhereMethodMarkerPathSteps(&.{
        .{ .kind = path_kind, .index = 0, .arity = 0, .name = @bitCast(source_z), .origin_module = 0, .source_decl = 0 },
        .{ .kind = path_kind, .index = 0, .arity = 0, .name = @bitCast(source_a), .origin_module = 0, .source_decl = 0 },
    });
    const none = std.math.maxInt(u32);
    const position = @intFromEnum(WhereMethodMarkerContract.Position.nested);
    const markers = try source_store.appendWhereMethodMarkerContracts(&.{
        .{ .producer_owner_node = none, .producer_where_node = none, .producer_method_name = none, .position = position, .widened = 0, .ready = 1, .path_start = @intFromEnum(paths.start), .path_len = 1 },
        .{ .producer_owner_node = none, .producer_where_node = none, .producer_method_name = none, .position = position, .widened = 1, .ready = 1, .path_start = @intFromEnum(paths.start) + 1, .path_len = 1 },
    });
    const source_callable = try source_store.freshFromContent(.{ .structure = .empty_record });
    const source_method = try source_env.insertIdent(base.Ident.for_text("rebased_method"));
    const constraints = try source_store.appendStaticDispatchConstraints(&.{.{
        .fn_name = source_method,
        .fn_var = source_callable,
        .origin = .{ .where_clause = .{} },
        .where_method_markers = markers,
    }});
    const source_root = try source_store.freshFromContent(.{ .flex = Flex.init().withConstraints(constraints) });

    var mapping = VarMapping.init(allocator);
    defer mapping.deinit();
    const copied = try copyVarWithMarkerLineageResult(
        &source_store,
        &dest_store,
        source_root,
        &mapping,
        null,
        &source_env,
        &dest_env,
        allocator,
        .{ .external_cir_node = .{ .node = 0 } },
    );
    const copied_root = copied.var_;
    const copy_step = copied.copy_step orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u32, 0), copy_step);
    try std.testing.expectEqual(@as(usize, 1), dest_env.where_marker_copy_steps.items.items.len);
    const published_step = dest_env.where_marker_copy_steps.items.items[copy_step];
    try std.testing.expectEqual(@intFromEnum(source_root), published_step.source_root_var);
    try std.testing.expectEqual(@intFromEnum(copied_root), published_step.destination_root_var);
    const root_occurrence = dest_env.where_marker_copy_occurrences.items.items[
        published_step.occurrences_start + published_step.root_occurrence_offset
    ];
    try std.testing.expectEqual(@intFromEnum(source_root), root_occurrence.raw_source_var);
    try std.testing.expectEqual(@intFromEnum(copied_root), root_occurrence.raw_destination_var);
    const copied_content = dest_store.resolveVar(copied_root).desc.content;
    try std.testing.expect(copied_content == .flex);
    const copied_constraints = dest_store.sliceStaticDispatchConstraints(copied_content.flex.constraints);
    try std.testing.expectEqual(@as(usize, 1), copied_constraints.len);
    const copied_constraint_pairs = dest_env.where_marker_constraint_copy_pairs.items.items[published_step.constraint_pairs_start..][0..published_step.constraint_pairs_len];
    try std.testing.expectEqual(@as(usize, 1), copied_constraint_pairs.len);
    try std.testing.expectEqual(
        @as(u32, @intFromEnum(constraints.start)),
        copied_constraint_pairs[0].source_constraint_index,
    );
    try std.testing.expectEqual(
        @as(u32, @intFromEnum(copied_content.flex.constraints.start)),
        copied_constraint_pairs[0].destination_constraint_index,
    );
    // Equal numeric indexes across the source and destination stores are not
    // local identity: their namespaces are disjoint and the copied constraint
    // therefore owns one complete source/anchor/handle triple.
    try std.testing.expectEqual(
        copied_constraint_pairs[0].source_constraint_index,
        copied_constraint_pairs[0].destination_constraint_index,
    );
    try std.testing.expectEqual(@as(usize, 1), dest_env.dispatch_settlement_sources.items.items.len);
    try std.testing.expectEqual(@as(usize, 1), dest_env.selected_receiver_anchors.items.items.len);
    const copied_source = dest_env.dispatch_settlement_sources.items.items[0]
        .decodedCopiedConstraint() orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(u32, 0), copied_source.anchor_index);
    try std.testing.expectEqual(
        ModuleEnv.CopiedConstraintComponentRef.Kind.root_graph,
        copied_source.receiver_component_ref.decodedKind().?,
    );
    try std.testing.expectEqual(
        ModuleEnv.CopiedConstraintComponentRef.Kind.root_graph,
        copied_source.function_component_ref.decodedKind().?,
    );
    try std.testing.expectEqual(copy_step, copied_source.receiver_component_ref.copy_step_index);
    try std.testing.expectEqual(copy_step, copied_source.function_component_ref.copy_step_index);
    try std.testing.expectEqual(@as(u32, 0), copied_source.function_component_ref.constraint_pair_offset);
    const copied_anchor = dest_env.selected_receiver_anchors.items.items[copied_source.anchor_index];
    try std.testing.expectEqual(copy_step, copied_anchor.copy_step);
    try std.testing.expectEqual(
        copied_source.receiver_component_ref.occurrence_offset,
        copied_anchor.receiver_occurrence_offset,
    );
    try std.testing.expectEqual(
        copied_source.function_component_ref.constraint_pair_offset,
        copied_anchor.constraint_pair_offset,
    );
    const evidence = dest_store.sliceConstraintEvidenceHandles(copied_constraints[0].constraint_evidence);
    try std.testing.expectEqual(@as(usize, 1), evidence.len);
    try std.testing.expectEqual(
        types_mod.ConstraintEvidenceHandle.Kind.selected_receiver_anchor,
        evidence[0].decodedKind().?,
    );
    try std.testing.expectEqual(copied_source.anchor_index, evidence[0].index);
    const copied_markers = dest_store.sliceWhereMethodMarkerContracts(copied_constraints[0].where_method_markers);
    const copied_bases = dest_store.sliceWhereMethodMarkerBases(copied_constraints[0].where_method_marker_bases);
    try std.testing.expectEqual(@as(usize, 2), copied_markers.len);
    try std.testing.expectEqual(@as(usize, 2), copied_bases.len);
    const expected_source_contract_offsets = [_]u32{ 1, 0 };
    for (copied_bases, 0..) |basis, offset| {
        try std.testing.expectEqual(copy_step, basis.copy_step);
        try std.testing.expectEqual(@as(u32, @intFromEnum(constraints.start)), basis.source_constraint_index);
        // Destination canonical order is a,z, but these coordinates must keep
        // naming the original source rows z=0,a=1.
        try std.testing.expectEqual(expected_source_contract_offsets[offset], basis.source_contract_offset);
    }
    const copied_paths = dest_store.where_method_marker_path_steps.items.items;
    try std.testing.expectEqual(@as(u32, @bitCast(dest_a)), copied_paths[copied_markers[0].path_start].name);
    try std.testing.expectEqual(true, copied_markers[0].isWidened().?);
    try std.testing.expectEqual(@as(u32, @bitCast(dest_z)), copied_paths[copied_markers[1].path_start].name);
    try std.testing.expectEqual(false, copied_markers[1].isWidened().?);
    try std.testing.expectEqual(
        std.math.Order.lt,
        dest_store.compareWhereMethodMarkerContracts(copied_markers[0], copied_markers[1]),
    );
}

test "copy_import nominal marker paths use the destination declaration spelling" {
    const allocator = std.testing.allocator;
    var source_env = try ModuleEnv.init(allocator, "");
    defer source_env.deinit();
    var dest_env = try ModuleEnv.init(allocator, "");
    defer dest_env.deinit();
    try source_env.setContentIdentity([_]u8{0x31} ** 32);
    try dest_env.setContentIdentity([_]u8{0x52} ** 32);

    var source_store = try TypesStore.initCapacity(allocator, 16, 8);
    defer source_store.deinit();
    var dest_store = try TypesStore.initCapacity(allocator, 16, 8);
    defer dest_store.deinit();

    const declaration_name = try source_env.insertIdent(base.Ident.for_text("DeclaredNominal"));
    const application_name = try source_env.insertIdent(base.Ident.for_text("AlternateApplication"));
    const formal = try source_store.fresh();
    const formals = try source_store.appendVars(&.{formal});
    const source_decl = try types_mod.NominalType.Source.initChecked(
        try types_mod.SourceDecl.fromStatementChecked(5),
        false,
        false,
    );
    _ = try source_store.registerNominalDecl(.{
        .ident = .{ .ident_idx = declaration_name },
        .origin_module = source_env.selfModuleIdentity(),
        .source = source_decl,
        .formals = formals,
        .backing = formal,
        .flags = .{ .valid = true },
    });

    const row = try source_store.freshFromContent(.{ .structure = .empty_tag_union });
    const nominal = try source_store.freshFromContent(try source_store.mkNominalWithSourceDecl(
        .{ .ident_idx = application_name },
        &.{row},
        source_env.selfModuleIdentity(),
        5,
        false,
    ));
    const callable = try source_store.freshFromContent(try source_store.mkFuncPure(&.{}, nominal));
    const paths = try source_store.appendWhereMethodMarkerPathSteps(&.{
        .{ .kind = @intFromEnum(WhereMethodMarkerPathStep.Kind.fn_ret), .index = 0, .arity = 0, .name = 0, .origin_module = 0, .source_decl = 0 },
        .{
            .kind = @intFromEnum(WhereMethodMarkerPathStep.Kind.nominal_arg),
            .index = 0,
            .arity = 1,
            .name = @bitCast(declaration_name),
            .origin_module = @intFromEnum(source_env.selfModuleIdentity()),
            .source_decl = @bitCast(source_decl.sourceDecl()),
        },
    });
    const none = std.math.maxInt(u32);
    const markers = try source_store.appendWhereMethodMarkerContracts(&.{.{
        .producer_owner_node = none,
        .producer_where_node = none,
        .producer_method_name = none,
        .position = @intFromEnum(WhereMethodMarkerContract.Position.nested),
        .widened = 1,
        .ready = 1,
        .path_start = @intFromEnum(paths.start),
        .path_len = 2,
    }});
    const method_name = try source_env.insertIdent(base.Ident.for_text("method"));
    const constraints = try source_store.appendStaticDispatchConstraints(&.{.{
        .fn_name = method_name,
        .fn_var = callable,
        .origin = .{ .where_clause = .{} },
        .where_method_markers = markers,
    }});
    const source_root = try source_store.freshFromContent(.{ .flex = Flex.init().withConstraints(constraints) });

    var mapping = VarMapping.init(allocator);
    defer mapping.deinit();
    const copied = try copyVarWithMarkerLineageResult(
        &source_store,
        &dest_store,
        source_root,
        &mapping,
        null,
        &source_env,
        &dest_env,
        allocator,
        .{ .external_cir_node = .{ .node = 0 } },
    );
    const copied_root = copied.var_;
    try std.testing.expect(copied.copy_step != null);
    const copied_constraint = dest_store.sliceStaticDispatchConstraints(
        dest_store.resolveVar(copied_root).desc.content.flex.constraints,
    )[0];
    const copied_marker = dest_store.sliceWhereMethodMarkerContracts(copied_constraint.where_method_markers)[0];
    const copied_path = dest_store.where_method_marker_path_steps.items.items[copied_marker.path_start..][0..copied_marker.path_len];
    const copied_nominal_step = copied_path[1];
    const copied_decl = dest_store.canonicalNominalDeclForMarkerPath(copied_nominal_step) orelse
        return error.TestUnexpectedResult;
    try std.testing.expectEqualStrings("DeclaredNominal", dest_env.getIdent(copied_decl.ident.ident_idx));
    try std.testing.expectEqual(@as(u32, @bitCast(copied_decl.ident.ident_idx)), copied_nominal_step.name);

    const copied_function = dest_store.resolveVar(copied_constraint.fn_var).desc.content.unwrapFunc().?;
    const copied_application = dest_store.resolveVar(copied_function.ret).desc.content.structure.nominal_type;
    try std.testing.expectEqualStrings("AlternateApplication", dest_env.getIdent(copied_application.ident.ident_idx));
    try std.testing.expect(copied_nominal_step.name != @as(u32, @bitCast(copied_application.ident.ident_idx)));
}
