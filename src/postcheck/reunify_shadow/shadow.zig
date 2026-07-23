//! Debug-only, state-isolated shadow of directed scheme instantiation
//! (reunify.md Slice 5). It runs strictly after Monotype lowering has sealed
//! its output, over the closed subset of concrete non-template roots and
//! instantiation edges with fully concrete bindings, and only compares
//! deterministic ids. It owns its own skeleton store (its own name interner and
//! its own bucket-enabled Monotype store) and writes nothing to any output
//! store, so deleting it changes only Debug time and memory.
//!
//! Two comparisons run:
//!
//!   1. Concrete-root: for each checked type that lowering translated to a
//!      Monotype id, the erased skeleton of the checked type is compared with
//!      the erased skeleton of the Monotype id. Both walks erase representation
//!      and aliases through the same rules, so a match proves the two erasures
//!      converge; a Monotype id carrying representation content is outside the
//!      closed subset and is skipped.
//!
//!   2. Scheme instantiation: for each closed instantiation edge, the edge's
//!      scheme root is instantiated under the edge's actual bindings and
//!      compared with the direct translation of the edge's instantiated root.
//!      A match is the reunify.md section 7.2 substitution invariant checked
//!      through the section 9 machinery.
//!
//! An outcome is only ever a counter (`shadow_match`, `shadow_mismatch`,
//! `shadow_skipped_<reason>`); a mismatch records bounded detail and never
//! panics. The counters are dumped to the `ROC_REUNIFY_CENSUS` file when the
//! shadow is enabled by `ROC_REUNIFY_SHADOW`.

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const check = @import("check");

const MonoType = @import("../monotype/type.zig");
const monotype_ast = @import("../monotype/ast.zig");
const census = @import("../monotype/census.zig");
const logic = @import("logical_identity.zig");

const names = check.CheckedNames;
const checked = check.CheckedModule;

const LogicalStore = logic.LogicalStore;
const SkipReason = logic.SkipReason;

/// All work here compiles out unless this is a Debug build on a 64-bit non-wasm
/// target: the shadow reads an env var through libc, which the wasm builds do
/// not support, and it is measurement-only, deliberately outside release.
pub const enabled = builtin.mode == .Debug and
    !builtin.target.cpu.arch.isWasm() and
    builtin.target.ptrBitWidth() >= 64 and
    builtin.os.tag != .freestanding;

/// One checked module's frozen types plus the name store that resolves them.
pub const ShadowModule = struct {
    key_bytes: [32]u8,
    view: checked.CheckedTypeStoreView,
    source_names: *const names.NameStore,
};

/// One (checked type, Monotype id) pair that lowering produced, indexed to its
/// owning module in `concrete_modules`.
pub const ConcreteRoot = struct {
    module_index: usize,
    checked_ty: checked.CheckedTypeId,
    mono_ty: MonoType.TypeId,
};

/// One checked module whose instantiation edges the scheme comparison reads.
pub const SchemeSource = struct {
    module_bytes: [32]u8,
    store: *const checked.CheckedTypeStore,
    source_names: *const names.NameStore,
};

/// Everything the shadow reads, all immutable and owned elsewhere. The shadow
/// reads it after sealing and never writes any of it.
pub const Inputs = struct {
    program_store: *const MonoType.Store,
    program_names: *const names.NameStore,
    concrete_modules: []const ShadowModule,
    concrete_roots: []const ConcreteRoot,
    scheme_sources: []const SchemeSource,
    /// The sealed specialization records (reunify.md 11, Slice 6): one record per
    /// reserved, lowering, or ready specialization. The shadow reads these read-only
    /// to census the logical identity of each record and to locate and classify the
    /// records whose solved type digest differs from the requested type digest.
    program_specs: []const monotype_ast.SpecRecord,
};

/// Whether the shadow should run: compiled in, and turned on by the env var.
pub fn shouldRun() bool {
    if (comptime !enabled) return false;
    const raw = std.c.getenv("ROC_REUNIFY_SHADOW") orelse return false;
    return std.mem.len(raw) > 0;
}

const max_mismatch_details = 32;

/// A concrete-root mismatch carries no scheme owner; this marks that.
const owner_kind_none: u8 = 0xFF;

const MismatchDetail = struct {
    scope: []const u8,
    module_bytes: [32]u8,
    checked_ty: u32,
    shadow_digest: [32]u8,
    other_digest: [32]u8,
    /// Bounded S-expressions of the two diverging skeletons (owned).
    shadow_desc: []u8,
    other_desc: []u8,
    /// Scheme-edge context (`owner_kind_none` for a concrete-root mismatch).
    owner_kind: u8,
    use_node: u32,
    binders: u32,

    fn deinit(self: MismatchDetail, allocator: Allocator) void {
        allocator.free(self.shadow_desc);
        allocator.free(self.other_desc);
    }
};

/// One dispatch-target site that carries no resolved evidence reference (reunify.md
/// 9.7, Slice 6): the edge identity, so the gap can be located exactly. Bounded to
/// `max_mismatch_details`.
const EvidenceGapDetail = struct {
    module_bytes: [32]u8,
    use_node: u32,
    slot_data: u32,
    scheme_owner_node: u32,
};

/// One specialization record whose solved type digest differs from its requested
/// type digest AND whose two sides reduce to DIFFERENT logical skeletons (reunify.md
/// 11.4, Slice 6). The digest difference is not explained by a representation-only
/// change, so it is either a corrected checked output or a rejected compiler
/// bug; the bounded detail locates the record for classification. `logical_divergent`
/// is a red flag: the request and solved logical skeletons are not equal.
const SpecDiffDetail = struct {
    request_digest: [32]u8,
    solved_digest: [32]u8,
    request_logical_digest: [32]u8,
    solved_logical_digest: [32]u8,
    request_shape: []u8,
    solved_shape: []u8,

    fn deinit(self: SpecDiffDetail, allocator: Allocator) void {
        allocator.free(self.request_shape);
        allocator.free(self.solved_shape);
    }
};

/// The shadow's outcome counters. Single-threaded (the shadow runs on the
/// lowering thread after sealing), so plain integers suffice.
const ShadowCensus = struct {
    concrete_match: u64 = 0,
    concrete_mismatch: u64 = 0,
    scheme_match: u64 = 0,
    /// Scheme edges that matched only up to a renaming of independent enclosing
    /// binders (reunify.md 7.3): a genuine match, kept separate for visibility.
    scheme_match_alpha: u64 = 0,
    scheme_mismatch: u64 = 0,
    /// Imported-scheme edges (reunify.md 7.1): the scheme root and binders come
    /// from the defining module, the actuals from the consuming module.
    scheme_match_imported: u64 = 0,
    scheme_mismatch_imported: u64 = 0,

    skipped_recursive_cycle: u64 = 0,
    skipped_representation_bearing: u64 = 0,
    skipped_pending_or_err: u64 = 0,
    skipped_numeric_default_unresolved: u64 = 0,
    skipped_zero_sized_or_erased: u64 = 0,
    skipped_alias_without_backing: u64 = 0,
    skipped_malformed_builtin_arity: u64 = 0,
    skipped_open_row: u64 = 0,
    skipped_binder_not_found: u64 = 0,

    scheme_skipped_imported: u64 = 0,
    scheme_skipped_no_snapshot: u64 = 0,
    scheme_skipped_unresolved_scheme: u64 = 0,
    scheme_skipped_arity_mismatch: u64 = 0,
    scheme_skipped_unreached_actual: u64 = 0,

    // Reasons a scheme edge's own translation (actual, callee root, or
    // instantiated root) left the comparable subset. Under caller-environment
    // translation (reunify.md 7.3) a free actual is no longer skipped — it
    // skolemizes — so what remains here is genuine structure the shadow does not
    // yet translate: recursive cycles (Slice 6 recursive-group interner), open
    // rows, or representation-bearing Monotype content in a checked payload.
    scheme_walk_recursive_cycle: u64 = 0,
    scheme_walk_open_row: u64 = 0,
    scheme_walk_representation_bearing: u64 = 0,
    scheme_walk_pending_or_err: u64 = 0,
    scheme_walk_numeric_default_unresolved: u64 = 0,
    scheme_walk_zero_sized_or_erased: u64 = 0,
    scheme_walk_alias_without_backing: u64 = 0,
    scheme_walk_malformed_builtin_arity: u64 = 0,
    scheme_walk_binder_not_found: u64 = 0,

    // Evidence-carry totality (reunify.md 9.7, Slice 6): per slot kind, how many
    // recorded sites carry a resolved evidence-vector reference and how many do not.
    // A dispatch-target site whose nested evidence checking resolved always carries
    // one, so an absent one is a flagged gap (bounded detail in `evidence_gaps`); a
    // value or shared use with an evidence-free scheme legitimately carries none.
    evidence_dispatch_target_present: u64 = 0,
    evidence_dispatch_target_absent: u64 = 0,
    evidence_value_use_present: u64 = 0,
    evidence_value_use_absent: u64 = 0,
    evidence_shared_value_use_present: u64 = 0,
    evidence_shared_value_use_absent: u64 = 0,
    evidence_nested_function_use_present: u64 = 0,
    evidence_nested_function_use_absent: u64 = 0,

    // Specialization registry census (reunify.md 11, Slice 6). The shadow reserves
    // one provisional record per sealed production record, so record-count parity is
    // `spec_records_total` by construction. Per record it reduces the requested type to
    // a logical skeleton (`spec_logical_computed` when reducible, `spec_logical_skipped`
    // when representation-bearing or recursive). Where the solved type digest differs
    // from the requested digest (reunify.md 11.4), it reduces both sides: equal logical
    // skeletons mean a representation-interface relation (legitimate); unequal skeletons
    // mean a corrected checked output or a rejected bug (a red flag, bounded detail
    // in `spec_diffs`); a side that will not reduce is `spec_diff_skipped`.
    spec_records_total: u64 = 0,
    spec_logical_computed: u64 = 0,
    spec_logical_skipped: u64 = 0,
    spec_request_equals_solved: u64 = 0,
    spec_request_differs_solved: u64 = 0,
    spec_diff_representation_only: u64 = 0,
    spec_diff_logical_divergent: u64 = 0,
    spec_diff_skipped: u64 = 0,

    fn bumpSkip(self: *ShadowCensus, reason: SkipReason) void {
        switch (reason) {
            .recursive_cycle => self.skipped_recursive_cycle += 1,
            .representation_bearing => self.skipped_representation_bearing += 1,
            .pending_or_err => self.skipped_pending_or_err += 1,
            .numeric_default_unresolved => self.skipped_numeric_default_unresolved += 1,
            .zero_sized_or_erased => self.skipped_zero_sized_or_erased += 1,
            .alias_without_backing => self.skipped_alias_without_backing += 1,
            .malformed_builtin_arity => self.skipped_malformed_builtin_arity += 1,
            .open_row => self.skipped_open_row += 1,
            .binder_not_found => self.skipped_binder_not_found += 1,
        }
    }
};

/// Record why one scheme edge's translation fell outside the comparable subset,
/// separate from the concrete-root skip counters so the two populations stay
/// distinct in the census.
fn bumpSchemeSkip(counts: *ShadowCensus, reason: SkipReason) void {
    switch (reason) {
        .recursive_cycle => counts.scheme_walk_recursive_cycle += 1,
        .open_row => counts.scheme_walk_open_row += 1,
        .representation_bearing => counts.scheme_walk_representation_bearing += 1,
        .pending_or_err => counts.scheme_walk_pending_or_err += 1,
        .numeric_default_unresolved => counts.scheme_walk_numeric_default_unresolved += 1,
        .zero_sized_or_erased => counts.scheme_walk_zero_sized_or_erased += 1,
        .alias_without_backing => counts.scheme_walk_alias_without_backing += 1,
        .malformed_builtin_arity => counts.scheme_walk_malformed_builtin_arity += 1,
        .binder_not_found => counts.scheme_walk_binder_not_found += 1,
    }
}

/// Build the isolated skeleton store, run both comparisons over the closed
/// subset, and dump the counters. Any resource failure ends the shadow run
/// with no effect on the sealed output.
pub fn run(allocator: Allocator, inputs: Inputs) void {
    if (comptime !enabled) return;
    runInner(allocator, inputs) catch return;
}

fn runInner(allocator: Allocator, inputs: Inputs) Allocator.Error!void {
    var logical = LogicalStore.init(allocator);
    defer logical.deinit();

    var counts: ShadowCensus = .{};

    var details = std.ArrayList(MismatchDetail).empty;
    defer {
        for (details.items) |detail| detail.deinit(allocator);
        details.deinit(allocator);
    }

    var evidence_gaps = std.ArrayList(EvidenceGapDetail).empty;
    defer evidence_gaps.deinit(allocator);

    var spec_diffs = std.ArrayList(SpecDiffDetail).empty;
    defer {
        for (spec_diffs.items) |diff| diff.deinit(allocator);
        spec_diffs.deinit(allocator);
    }

    try runConcreteRoots(allocator, &logical, inputs, &counts, &details);
    try runSchemeEdges(allocator, &logical, inputs, &counts, &details);
    try runSiteEvidenceCensus(allocator, inputs, &counts, &evidence_gaps);
    try runSpecRegistryCensus(allocator, &logical, inputs, &counts, &spec_diffs);

    dump(allocator, &counts, details.items, evidence_gaps.items, spec_diffs.items);
}

/// Census the sealed specialization registry (reunify.md 11, Slice 6). Read-only: the
/// shadow reduces each record's requested type to a logical skeleton and, where the
/// solved type digest differs from the requested digest, reduces both sides to
/// classify the difference per reunify.md 11.4 as a representation-interface relation
/// (equal logical skeletons) or a corrected-output/rejected-bug divergence
/// (unequal logical skeletons, bounded detail). Record-count parity holds by
/// construction: the shadow reserves one provisional record per production record.
fn runSpecRegistryCensus(
    allocator: Allocator,
    logical: *LogicalStore,
    inputs: Inputs,
    counts: *ShadowCensus,
    spec_diffs: *std.ArrayList(SpecDiffDetail),
) Allocator.Error!void {
    counts.spec_records_total = inputs.program_specs.len;

    for (inputs.program_specs) |record| {
        // The record's requested type reduces to a logical skeleton unless it is
        // representation-bearing or recursive (outside the reducible subset).
        const request_logical = logicalOf(logical, inputs, record.request_fn_ty) catch |err| switch (err) {
            error.Skip => {
                counts.spec_logical_skipped += 1;
                continue;
            },
            else => |other| return other,
        };
        counts.spec_logical_computed += 1;

        if (std.meta.eql(record.request_fn_ty_digest.bytes, record.solved_fn_ty_digest.bytes)) {
            counts.spec_request_equals_solved += 1;
            continue;
        }
        counts.spec_request_differs_solved += 1;

        const solved_logical = logicalOf(logical, inputs, record.solved_fn_ty) catch |err| switch (err) {
            error.Skip => {
                counts.spec_diff_skipped += 1;
                continue;
            },
            else => |other| return other,
        };

        if (request_logical == solved_logical) {
            // reunify.md 11.4: logically equal, differing only in representation — an
            // explicit representation-interface relation, not a logical refinement.
            counts.spec_diff_representation_only += 1;
        } else {
            // reunify.md 11.4: the solved skeleton is not logically equal to the
            // requested one — a corrected checked output or a rejected bug.
            counts.spec_diff_logical_divergent += 1;
            if (spec_diffs.items.len < max_mismatch_details) {
                const request_shape = try logical.describe(allocator, request_logical);
                errdefer allocator.free(request_shape);
                const solved_shape = try logical.describe(allocator, solved_logical);
                errdefer allocator.free(solved_shape);
                try spec_diffs.append(allocator, .{
                    .request_digest = record.request_fn_ty_digest.bytes,
                    .solved_digest = record.solved_fn_ty_digest.bytes,
                    .request_logical_digest = logical.digestBytes(request_logical),
                    .solved_logical_digest = logical.digestBytes(solved_logical),
                    .request_shape = request_shape,
                    .solved_shape = solved_shape,
                });
            }
        }
    }
}

/// The logical skeleton of a program Monotype id, resolving the skip reason locally
/// (the specialization census does not distinguish the reason). Read-only.
fn logicalOf(logical: *LogicalStore, inputs: Inputs, mono_ty: MonoType.TypeId) logic.WalkError!logic.LogicalTypeIdentity {
    var reason: SkipReason = undefined;
    return logical.monoLogicalIdentity(inputs.program_store, inputs.program_names, mono_ty, &reason);
}

/// Census the evidence-carry totality of every recorded instantiation site
/// (reunify.md 9.7, Slice 6). Per slot kind, count sites whose `evidenceRange` is
/// present versus absent; a dispatch-target site with no reference is a flagged gap
/// with bounded detail. Read-only over the sealed checked store.
fn runSiteEvidenceCensus(
    allocator: Allocator,
    inputs: Inputs,
    counts: *ShadowCensus,
    gaps: *std.ArrayList(EvidenceGapDetail),
) Allocator.Error!void {
    for (inputs.scheme_sources) |source| {
        for (source.store.instantiationSites()) |site| {
            const present = site.evidenceRange() != null;
            switch (@as(checked.InstantiationSiteSlotKind, @enumFromInt(site.slot_kind))) {
                .dispatch_target => {
                    if (present) {
                        counts.evidence_dispatch_target_present += 1;
                    } else {
                        counts.evidence_dispatch_target_absent += 1;
                        if (gaps.items.len < max_mismatch_details) {
                            try gaps.append(allocator, .{
                                .module_bytes = source.module_bytes,
                                .use_node = site.use_node,
                                .slot_data = site.slot_data,
                                .scheme_owner_node = site.scheme_owner_node,
                            });
                        }
                    }
                },
                .value_use => if (present) {
                    counts.evidence_value_use_present += 1;
                } else {
                    counts.evidence_value_use_absent += 1;
                },
                .shared_value_use => if (present) {
                    counts.evidence_shared_value_use_present += 1;
                } else {
                    counts.evidence_shared_value_use_absent += 1;
                },
                .nested_function_use => if (present) {
                    counts.evidence_nested_function_use_present += 1;
                } else {
                    counts.evidence_nested_function_use_absent += 1;
                },
            }
        }
    }
}

fn runConcreteRoots(
    allocator: Allocator,
    logical: *LogicalStore,
    inputs: Inputs,
    counts: *ShadowCensus,
    details: *std.ArrayList(MismatchDetail),
) Allocator.Error!void {
    for (inputs.concrete_roots) |root| {
        const module = inputs.concrete_modules[root.module_index];

        var mono_reason: SkipReason = undefined;
        const mono_id = logical.monoLogicalIdentity(
            inputs.program_store,
            inputs.program_names,
            root.mono_ty,
            &mono_reason,
        ) catch |err| switch (err) {
            error.Skip => {
                counts.bumpSkip(mono_reason);
                continue;
            },
            else => |other| return other,
        };

        var checked_reason: SkipReason = undefined;
        const checked_id = logical.checkedLogicalIdentity(
            module.view,
            module.source_names,
            root.checked_ty,
            &checked_reason,
        ) catch |err| switch (err) {
            error.Skip => {
                counts.bumpSkip(checked_reason);
                continue;
            },
            else => |other| return other,
        };

        if (checked_id == mono_id) {
            counts.concrete_match += 1;
        } else {
            counts.concrete_mismatch += 1;
            try recordMismatch(allocator, logical, details, .{
                .scope = "concrete",
                .module_bytes = module.key_bytes,
                .checked_ty = root.checked_ty,
                .shadow_id = checked_id,
                .other_id = mono_id,
            });
        }
    }
}

/// One side's checked store plus the name store and module identity that
/// resolve it. Every site's scheme root, binders, actuals, and instantiated root
/// now live in the CONSUMING store: a local site's scheme is recorded there
/// directly, and an imported site's defining scheme is projected there when the
/// consumer's checked store is built (reunify.md 7.1, Slice 6), so no
/// defining-module view is needed.
const TranslationContext = struct {
    view: checked.CheckedTypeStoreView,
    source_names: *const names.NameStore,
    module_bytes: [32]u8,
};

/// A `ResolvedScheme.owner_kind` value for an imported scheme, whose defining
/// `CheckedTypeScheme.owner_kind` is not carried into the consuming checked store.
/// Distinct from every real `CheckedSchemeOwnerKind` and from `owner_kind_none`.
const owner_kind_imported: u8 = 0xFE;

/// One site's scheme resolved entirely within the consuming store: the identity
/// that keys instantiation memoization and skolemization, the scheme root, and
/// its ordered binders. `ident.module_bytes` is the consuming module for a local
/// scheme and the DEFINING module for an imported one, so an imported scheme's
/// memo key and captured-binder skolemization stay attributed to its defining
/// module even though its structure is projected into the consumer.
const ResolvedScheme = struct {
    ident: logic.SchemeIdent,
    root: checked.CheckedTypeId,
    binders: []const checked.CheckedTypeId,
    owner_node: u32,
    owner_kind: u8,
    imported: bool,
};

fn runSchemeEdges(
    allocator: Allocator,
    logical: *LogicalStore,
    inputs: Inputs,
    counts: *ShadowCensus,
    details: *std.ArrayList(MismatchDetail),
) Allocator.Error!void {
    for (inputs.scheme_sources) |source| {
        const consuming = TranslationContext{
            .view = source.store.view(),
            .source_names = source.source_names,
            .module_bytes = source.module_bytes,
        };
        for (source.store.instantiationSites()) |site| {
            const scheme_id = site.schemeId() orelse {
                counts.scheme_skipped_unresolved_scheme += 1;
                continue;
            };

            // Resolve the scheme root and binders — both from the CONSUMING store
            // (reunify.md 7.1, Slice 6). A local site reads its recorded scheme; an
            // imported site reads the defining scheme projected into this store's
            // imported-scheme table, keyed by (defining module, source owner node).
            const resolved: ResolvedScheme = if (site.importedDefiningModule()) |defining_hash| blk: {
                const entry = consuming.view.importedSchemeByOwner(defining_hash, site.scheme_owner_node) orelse {
                    counts.scheme_skipped_imported += 1;
                    continue;
                };
                break :blk .{
                    .ident = .{ .module_bytes = defining_hash, .scheme = @intFromEnum(scheme_id) },
                    .root = entry.localRoot(),
                    .binders = entry.binders(consuming.view),
                    .owner_node = site.scheme_owner_node,
                    .owner_kind = owner_kind_imported,
                    .imported = true,
                };
            } else blk: {
                const scheme = consuming.view.schemeById(scheme_id) orelse {
                    counts.scheme_skipped_unresolved_scheme += 1;
                    continue;
                };
                if (scheme.snapshotRoot() == null) {
                    counts.scheme_skipped_no_snapshot += 1;
                    continue;
                }
                break :blk .{
                    .ident = .{ .module_bytes = consuming.module_bytes, .scheme = @intFromEnum(scheme_id) },
                    .root = scheme.root,
                    .binders = scheme.generalizedVars(consuming.view),
                    .owner_node = scheme.owner_node,
                    .owner_kind = @intFromEnum(scheme.owner_kind),
                    .imported = false,
                };
            };

            const actuals = site.actuals(source.store);
            if (actuals.len != resolved.binders.len) {
                counts.scheme_skipped_arity_mismatch += 1;
                continue;
            }

            try compareSchemeEdge(allocator, logical, consuming, resolved, actuals, site, counts, details);
        }
    }
}

fn compareSchemeEdge(
    allocator: Allocator,
    logical: *LogicalStore,
    consuming: TranslationContext,
    resolved: ResolvedScheme,
    actuals: []const checked.CheckedTypeId,
    site: checked.CheckedInstantiationSite,
    counts: *ShadowCensus,
    details: *std.ArrayList(MismatchDetail),
) Allocator.Error!void {
    var binding = std.ArrayList(logic.BoundType).empty;
    defer binding.deinit(allocator);

    for (actuals) |actual| {
        if (@intFromEnum(actual) == checked.checked_instantiation_actual_unreached) {
            counts.scheme_skipped_unreached_actual += 1;
            return;
        }
        // Translate the actual under the CONSUMING module's binding environment
        // (reunify.md 7.3, 9.1): an actual that is a free binder of an enclosing
        // scheme becomes an abstract skolem keyed by its checked id, so the
        // enclosing binder appears identically here and in the instantiated root
        // below. A concrete actual translates to its ground skeleton unchanged.
        var reason: SkipReason = undefined;
        const actual_logical = logical.checkedLogicalIdentityUnder(
            consuming.view,
            consuming.source_names,
            actual,
            .skolemize,
            consuming.module_bytes,
            &reason,
        ) catch |err| switch (err) {
            error.Skip => {
                bumpSchemeSkip(counts, reason);
                return;
            },
            else => |other| return other,
        };
        try binding.append(allocator, logic.BoundType.closed(actual_logical));
    }

    // Instantiate the scheme root. Its structure lives in the CONSUMING store for
    // both local and imported (projected) schemes (reunify.md 7.1, Slice 6); the
    // scheme identity (`resolved.ident`) keys the memo and skolemizes captured
    // enclosing binders under the scheme's owning module, and the binding carries
    // the consuming-side actuals.
    var inst_reason: SkipReason = undefined;
    const instantiated = logical.instantiateScheme(
        resolved.ident,
        consuming.view,
        consuming.source_names,
        resolved.owner_node,
        resolved.root,
        resolved.binders,
        binding.items,
        &.{},
        .skolemize,
        &inst_reason,
    ) catch |err| switch (err) {
        error.Skip => {
            bumpSchemeSkip(counts, inst_reason);
            return;
        },
        else => |other| return other,
    };

    // The instantiated root is a CONSUMING-module checked id.
    var direct_reason: SkipReason = undefined;
    const direct = logical.checkedLogicalIdentityUnder(
        consuming.view,
        consuming.source_names,
        site.instantiated_root,
        .skolemize,
        consuming.module_bytes,
        &direct_reason,
    ) catch |err| switch (err) {
        error.Skip => {
            bumpSchemeSkip(counts, direct_reason);
            return;
        },
        else => |other| return other,
    };

    if (instantiated == direct) {
        if (resolved.imported) counts.scheme_match_imported += 1 else counts.scheme_match += 1;
    } else if (try logical.alphaEqual(instantiated, direct)) {
        // Equal up to a renaming of enclosing binders that appear as independent
        // fresh copies on the two sides (a binders=0 nested scheme's root versus
        // the site's instantiated root); a real transposition is not accepted
        // (reunify.md 7.3, 7.6). Counted apart from an exact-id match so these
        // stay visible rather than folded into the mismatch total.
        counts.scheme_match_alpha += 1;
    } else {
        if (resolved.imported) counts.scheme_mismatch_imported += 1 else counts.scheme_mismatch += 1;
        try recordMismatch(allocator, logical, details, .{
            .scope = if (resolved.imported) "scheme_imported" else "scheme",
            .module_bytes = consuming.module_bytes,
            .checked_ty = site.instantiated_root,
            .shadow_id = instantiated,
            .other_id = direct,
            .owner_kind = resolved.owner_kind,
            .use_node = site.use_node,
            .binders = @intCast(resolved.binders.len),
        });
    }
}

/// The fields one mismatch records. `shadow_id` is the shadow's own computation
/// (instantiation or checked translation); `other_id` is the value it is
/// compared against (production erasure or the direct instantiated-root walk).
const MismatchContext = struct {
    scope: []const u8,
    module_bytes: [32]u8,
    checked_ty: checked.CheckedTypeId,
    shadow_id: logic.LogicalTypeIdentity,
    other_id: logic.LogicalTypeIdentity,
    owner_kind: u8 = owner_kind_none,
    use_node: u32 = 0,
    binders: u32 = 0,
};

fn recordMismatch(
    allocator: Allocator,
    logical: *LogicalStore,
    details: *std.ArrayList(MismatchDetail),
    ctx: MismatchContext,
) Allocator.Error!void {
    if (details.items.len >= max_mismatch_details) return;
    const shadow_desc = try logical.describe(allocator, ctx.shadow_id);
    errdefer allocator.free(shadow_desc);
    const other_desc = try logical.describe(allocator, ctx.other_id);
    errdefer allocator.free(other_desc);
    try details.append(allocator, .{
        .scope = ctx.scope,
        .module_bytes = ctx.module_bytes,
        .checked_ty = @intFromEnum(ctx.checked_ty),
        .shadow_digest = logical.digestBytes(ctx.shadow_id),
        .other_digest = logical.digestBytes(ctx.other_id),
        .shadow_desc = shadow_desc,
        .other_desc = other_desc,
        .owner_kind = ctx.owner_kind,
        .use_node = ctx.use_node,
        .binders = ctx.binders,
    });
}

/// Append the counter dump and any bounded mismatch/evidence-gap/spec-diff detail to
/// the census file.
fn dump(
    allocator: Allocator,
    counts: *ShadowCensus,
    details: []const MismatchDetail,
    evidence_gaps: []const EvidenceGapDetail,
    spec_diffs: []const SpecDiffDetail,
) void {
    if (comptime !enabled) return;
    const raw_path = std.c.getenv("ROC_REUNIFY_CENSUS") orelse return;
    if (std.mem.len(raw_path) == 0) return;

    var out = std.ArrayList(u8).empty;
    defer out.deinit(allocator);

    inline for (@typeInfo(ShadowCensus).@"struct".fields) |field| {
        const value = @field(counts, field.name);
        const line = std.fmt.allocPrint(allocator, "shadow_{s} {d}\n", .{ field.name, value }) catch return;
        defer allocator.free(line);
        out.appendSlice(allocator, line) catch return;
    }

    for (evidence_gaps) |gap| {
        const module_hex = std.fmt.bytesToHex(gap.module_bytes[0..8].*, .lower);
        const line = std.fmt.allocPrint(
            allocator,
            "shadow_evidence_gap_detail scope=dispatch_target module={s} use_node={d} slot_data={d} scheme_owner_node={d}\n",
            .{ &module_hex, gap.use_node, gap.slot_data, gap.scheme_owner_node },
        ) catch return;
        defer allocator.free(line);
        out.appendSlice(allocator, line) catch return;
    }

    for (spec_diffs) |diff| {
        const request_hex = std.fmt.bytesToHex(diff.request_digest[0..8].*, .lower);
        const solved_hex = std.fmt.bytesToHex(diff.solved_digest[0..8].*, .lower);
        const request_logical_hex = std.fmt.bytesToHex(diff.request_logical_digest[0..8].*, .lower);
        const solved_logical_hex = std.fmt.bytesToHex(diff.solved_logical_digest[0..8].*, .lower);
        const line = std.fmt.allocPrint(
            allocator,
            "shadow_spec_diff_detail request={s} solved={s} request_logical={s} solved_logical={s}\n  request_shape={s}\n  solved_shape={s}\n",
            .{ &request_hex, &solved_hex, &request_logical_hex, &solved_logical_hex, diff.request_shape, diff.solved_shape },
        ) catch return;
        defer allocator.free(line);
        out.appendSlice(allocator, line) catch return;
    }

    for (details) |detail| {
        const module_hex = std.fmt.bytesToHex(detail.module_bytes[0..8].*, .lower);
        const shadow_hex = std.fmt.bytesToHex(detail.shadow_digest[0..8].*, .lower);
        const production_hex = std.fmt.bytesToHex(detail.other_digest[0..8].*, .lower);
        const line = std.fmt.allocPrint(
            allocator,
            "shadow_mismatch_detail scope={s} module={s} checked_ty={d} owner_kind={d} use_node={d} binders={d} shadow={s} other={s}\n  shadow_shape={s}\n  other_shape={s}\n",
            .{ detail.scope, &module_hex, detail.checked_ty, detail.owner_kind, detail.use_node, detail.binders, &shadow_hex, &production_hex, detail.shadow_desc, detail.other_desc },
        ) catch return;
        defer allocator.free(line);
        out.appendSlice(allocator, line) catch return;
    }

    if (out.items.len == 0) return;
    census.appendToFile(raw_path, out.items);
}

test "shadow does not run without the enabling env var" {
    if (comptime !enabled) return;
    // The tests run without ROC_REUNIFY_SHADOW set, so the gate is off and the
    // shadow performs no work: production is untouched by default.
    try std.testing.expect(!shouldRun());
}

test "declarations are referenced" {
    std.testing.refAllDecls(@This());
}
