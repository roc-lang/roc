//! Debug-only Slice 0 measurement census for the reunify migration
//! (reunify.md sections 5.4, 7.2, 13 "Slice 0").
//!
//! Slice 0 declares the current semantics and measures assumptions before any
//! later slice builds on them. Two measurements live here:
//!
//!   * repeated scheme-use record equivalence (reunify.md 7.2): a re-checked
//!     CIR edge may record its instantiation more than once; before the
//!     exactly-one-equivalent-record invariant becomes authoritative, this
//!     census reports whether each such duplicate resolves to equivalent
//!     content.
//!   * checked-boundary `.err`-reachability (reunify.md 5.4/7.5): a module that
//!     will lower must never publish a checked type payload that reaches `.err`;
//!     this census counts any lowerable corpus module that violates it.
//!
//! Everything here compiles to nothing outside Debug: the counters, the
//! recording entry points, and the dump are all gated on `builtin.mode`, and
//! the enabled flag is only ever raised by the compile driver when the
//! `ROC_REUNIFY_CHECK_CENSUS` env var names a dump-file path. Release builds see
//! `active()` fold to a comptime `false`, so no call site changes behavior.

const std = @import("std");
const builtin = @import("builtin");

/// All measurement is compiled out unless this is a Debug build on a 64-bit
/// non-wasm target: the counters are 64-bit atomics and self-enabling reads
/// an env var through libc, neither of which the wasm builds support.
pub const enabled = builtin.mode == .Debug and
    !builtin.target.cpu.arch.isWasm() and
    builtin.target.ptrBitWidth() >= 64 and
    builtin.os.tag != .freestanding;

/// Number of divergent/violation identifications kept per category. The census
/// measures counts exactly; it retains only the first few identities so a
/// finding can be located in the corpus without an unbounded buffer.
const max_identifications = 8;

/// Enough bytes to hold a module name for later location in the corpus.
const module_name_capacity = 128;

const Identification = struct {
    module: [module_name_capacity]u8 = [_]u8{0} ** module_name_capacity,
    module_len: usize = 0,
    node_idx: u32 = 0,
    has_node: bool = false,

    fn moduleText(self: *const Identification) []const u8 {
        return self.module[0..self.module_len];
    }
};

// A lightweight spinlock guarding the bounded identification buffers and the
// dump. Recording runs from checking without an `Io` in hand, so an
// `Io`-parameterized mutex does not fit; contention is near zero (only rare
// duplicate/violation records and a per-module dump), so a spin is enough.
var buffer_lock = std.atomic.Value(bool).init(false);
var active_flag = std.atomic.Value(bool).init(false);
var env_checked = std.atomic.Value(bool).init(false);

// Some checking drivers construct no PackageEnv or Coordinator (the snapshot
// tool's file snapshots call `canonicalizeAndTypeCheckModule` directly), so
// the census reads its env var itself the first time `active()` is asked,
// rather than depending on every driver's constructor to call `enable`.
fn checkEnvOnce() void {
    if (comptime !enabled) return;
    if (env_checked.load(.acquire)) return;
    if (std.c.getenv("ROC_REUNIFY_CHECK_CENSUS")) |raw| {
        var raw_len: usize = 0;
        while (raw[raw_len] != 0) raw_len += 1;
        const path = raw[0..raw_len];
        if (path.len > 0) enable(path);
    }
    env_checked.store(true, .release);
}

fn lockBuffer() void {
    while (buffer_lock.cmpxchgWeak(false, true, .acquire, .monotonic) != null) {
        std.atomic.spinLoopHint();
    }
}

fn unlockBuffer() void {
    buffer_lock.store(false, .release);
}

var scheme_use_duplicate_edges = std.atomic.Value(u64).init(0);
var scheme_use_duplicates_equivalent = std.atomic.Value(u64).init(0);
var scheme_use_duplicates_divergent = std.atomic.Value(u64).init(0);
var err_reachable_in_lowerable_module = std.atomic.Value(u64).init(0);
var scheme_snapshot_matches_final = std.atomic.Value(u64).init(0);
var scheme_snapshot_diverged_from_final = std.atomic.Value(u64).init(0);

// Slice 2b dense use-site measurement (reunify.md 7.2). Recorded sites by slot
// kind, unreached binders, sites whose scheme owns no local snapshot, publication
// duplicate equivalence, shared-edge dense-vs-marker resolution, and the artifact
// byte cost checkpoint.
var site_recorded_value_use = std.atomic.Value(u64).init(0);
var site_recorded_nested = std.atomic.Value(u64).init(0);
var site_recorded_dispatch = std.atomic.Value(u64).init(0);
var site_recorded_shared = std.atomic.Value(u64).init(0);
var site_binder_unreached = std.atomic.Value(u64).init(0);
var site_without_snapshot_value_use = std.atomic.Value(u64).init(0);
var site_without_snapshot_nested = std.atomic.Value(u64).init(0);
var site_without_snapshot_dispatch = std.atomic.Value(u64).init(0);
var site_duplicate_equivalent = std.atomic.Value(u64).init(0);
var site_duplicate_divergent = std.atomic.Value(u64).init(0);
var shared_edges_dense = std.atomic.Value(u64).init(0);
var shared_edges_marker = std.atomic.Value(u64).init(0);
var published_sites_total = std.atomic.Value(u64).init(0);
var published_site_actuals_total = std.atomic.Value(u64).init(0);
var census_env_bytes = std.atomic.Value(u64).init(0);
var census_artifact_bytes = std.atomic.Value(u64).init(0);
var census_module_count = std.atomic.Value(u64).init(0);

// Slice 2 (this sub-slice) captured-binder closures (reunify.md 7.1) and
// imported-scheme instantiation projections (reunify.md 7.1/7.2).
var schemes_with_captures = std.atomic.Value(u64).init(0);
var captured_refs_attributed = std.atomic.Value(u64).init(0);
var captured_refs_unattributed = std.atomic.Value(u64).init(0);
var site_imported_value_use = std.atomic.Value(u64).init(0);
var site_imported_dispatch = std.atomic.Value(u64).init(0);
// Publication-side imported-scheme resolution (reunify.md 7.1, Slice 2). Where the
// check-time `site_imported_*` counters above count every recorded imported-scheme
// instantiation (per instantiation, including duplicates), these count the
// published (edge-deduped) imported sites split by whether the consuming side could
// name the DEFINING module's `CheckedTypeSchemeId` through its serialized owner
// index: `resolved` when it could, `without_defining_scheme` when it could not
// (defining artifact not among the loaded import views, or its owner unindexed).
var site_imported_defining_scheme_resolved = std.atomic.Value(u64).init(0);
var site_imported_without_defining_scheme = std.atomic.Value(u64).init(0);
var annotation_scheme_owner_aliased = std.atomic.Value(u64).init(0);
var annotation_scheme_owner_diverged = std.atomic.Value(u64).init(0);

// Slice 2 (this sub-slice) residual-variable dispositions (reunify.md 7.4 phase
// one). Every reachable plain-unconstrained residual variable in a published scheme
// body gets exactly one recorded disposition; a variable-shaped residual that does
// not fit the plain-unconstrained classification (a dispatcher-constrained flex or a
// stray rigid) is counted but left undisposed.
var disposition_contextual = std.atomic.Value(u64).init(0);
var disposition_uninhabited = std.atomic.Value(u64).init(0);
var residual_undisposed = std.atomic.Value(u64).init(0);
var disposition_adopted_from_dispatch = std.atomic.Value(u64).init(0);

// Slice 2 boundary verifier (reunify.md 7.5). The verifier hard-asserts the
// structural invariants; these count the two checks left measured because a later
// slice must first close their gap: whether a local resolved site's dense actual
// count equals its scheme's binder count, and whether an imported site resolved a
// defining scheme id.
var site_actuals_len_matches_binders = std.atomic.Value(u64).init(0);
var site_actuals_len_diverges_from_binders = std.atomic.Value(u64).init(0);

// Slice 2 boundary verifier, scheme binder completeness (reunify.md 7.1/7.5).
// Every published scheme is classified by what its root reaches: no checked
// variable at all (`ground` — a genuinely monomorphic source, whose empty binder
// vector is the right answer), only variables the scheme's binding names
// (`bound`: its own ordered binders plus the enclosing-scheme binders it
// captures), variables outside that binding which nonetheless carry an explicit
// final disposition or checked default (`disposed`), or at least one variable
// that neither names nor settles (`unaccounted`). The last is the class 7.1
// forbids — the shape a missed boundary capture leaves behind — and must reach
// zero before the invariant can be asserted rather than measured.
var scheme_root_ground = std.atomic.Value(u64).init(0);
var scheme_root_bound = std.atomic.Value(u64).init(0);
var scheme_root_disposed = std.atomic.Value(u64).init(0);
var scheme_root_unaccounted = std.atomic.Value(u64).init(0);
// The forbidden class split by which owner kind published it, so a gap names the
// checking-side boundary that must start capturing.
var scheme_unaccounted_top_level = std.atomic.Value(u64).init(0);
var scheme_unaccounted_nested = std.atomic.Value(u64).init(0);
var scheme_unaccounted_required = std.atomic.Value(u64).init(0);
var scheme_unaccounted_synthetic = std.atomic.Value(u64).init(0);

// The same coverage question asked of every resolved dispatch plan's published
// callable (reunify.md 6.5/7.4/7.5). A plan callable is written by publication
// rather than by solving, so it is the one published type a specialization pass
// can mint a variable into that no scheme binds and no disposition settles.
// `plan_callable_constrained` is the dispatcher-constrained flex that
// `publishResidualDispositions` already declares undisposed and counts as
// `residual_undisposed`; `plan_callable_unaccounted` is everything else and must
// stay zero, since a variable of that shape reachable from a published callable
// is one publication minted after the dispositions were recorded.
var plan_callable_ground = std.atomic.Value(u64).init(0);
var plan_callable_bound = std.atomic.Value(u64).init(0);
var plan_callable_disposed = std.atomic.Value(u64).init(0);
var plan_callable_constrained = std.atomic.Value(u64).init(0);
var plan_callable_unaccounted = std.atomic.Value(u64).init(0);

// Slice 2 validation matcher (reunify.md 7.6). For each local site whose scheme is
// verified-pristine (snapshot_root != none) and whose actuals carry no unreached
// sentinel, the matcher applies the published substitution to the scheme structure
// and compares against the published instantiated root. Match/mismatch/skip
// distribution; a mismatch is a publication bug (bounded detail retained).
var matcher_sites_total = std.atomic.Value(u64).init(0);
var matcher_match = std.atomic.Value(u64).init(0);
var matcher_mismatch = std.atomic.Value(u64).init(0);
var matcher_skipped_imported = std.atomic.Value(u64).init(0);
var matcher_skipped_no_snapshot = std.atomic.Value(u64).init(0);
var matcher_skipped_unreached = std.atomic.Value(u64).init(0);
var matcher_skipped_no_scheme = std.atomic.Value(u64).init(0);
var matcher_skipped_walk = std.atomic.Value(u64).init(0);

var matcher_mismatch_details: [max_identifications][detail_capacity]u8 = undefined;
var matcher_mismatch_detail_lens: [max_identifications]usize = [_]usize{0} ** max_identifications;
var matcher_mismatch_detail_count: usize = 0;

// Slice 6 evidence carry (reunify.md 9.7). When wiring each dense site's
// evidence-vector reference, classify the outcome. A dispatch-target site's
// reference is its resolved evidence node's nested vector; the two absent reasons
// distinguish a target whose dispatch did not discharge a scheme-use pair record
// (its resolution is a chain forward, structural derivation, checked error, or
// vacuous edge — no procedure evidence node exists) from a record that resolved with
// no evidence node. A value or shared use's reference is its site-evidence range;
// an absent one means the used scheme carries no evidence params.
var evidence_dispatch_wired = std.atomic.Value(u64).init(0);
var evidence_dispatch_no_pair_record = std.atomic.Value(u64).init(0);
var evidence_dispatch_no_evidence_node = std.atomic.Value(u64).init(0);
var evidence_value_wired = std.atomic.Value(u64).init(0);
var evidence_value_no_site_entry = std.atomic.Value(u64).init(0);
var evidence_shared_wired = std.atomic.Value(u64).init(0);
var evidence_shared_no_site_entry = std.atomic.Value(u64).init(0);

var divergent_ids: [max_identifications]Identification = [_]Identification{.{}} ** max_identifications;
var divergent_id_count: usize = 0;
var err_ids: [max_identifications]Identification = [_]Identification{.{}} ** max_identifications;
var err_id_count: usize = 0;
var snapshot_divergent_ids: [max_identifications]Identification = [_]Identification{.{}} ** max_identifications;
var snapshot_divergent_id_count: usize = 0;

/// Free-form detail lines for divergent scheme-use pairs, kept bounded so the
/// dump can show exactly which recorded pairs disagreed.
const detail_capacity = 1024;
var divergent_details: [max_identifications][detail_capacity]u8 = undefined;
var divergent_detail_lens: [max_identifications]usize = [_]usize{0} ** max_identifications;
var divergent_detail_count: usize = 0;

/// Retain one bounded detail line describing a divergent scheme-use pair.
pub fn recordSchemeUseDivergentDetail(text: []const u8) void {
    if (comptime !enabled) return;
    lockBuffer();
    defer unlockBuffer();
    if (divergent_detail_count >= max_identifications) return;
    const copied = @min(text.len, detail_capacity);
    @memcpy(divergent_details[divergent_detail_count][0..copied], text[0..copied]);
    divergent_detail_lens[divergent_detail_count] = copied;
    divergent_detail_count += 1;
}

/// Room to hold the dump-file path named by `ROC_REUNIFY_CHECK_CENSUS`.
const path_capacity = 4096;
var stored_path: [path_capacity]u8 = undefined;
var stored_path_len: usize = 0;

/// The compile driver calls this once, when `ROC_REUNIFY_CHECK_CENSUS` names a
/// dump-file path, before any module is checked: it stores the path (owned, for
/// the process's lifetime) and raises the measurement flag. Any driver can then
/// dump without threading the path through its own state. A no-op outside Debug,
/// and an empty path leaves the census disabled.
pub fn enable(path: []const u8) void {
    if (comptime !enabled) return;
    if (path.len == 0) return;
    lockBuffer();
    defer unlockBuffer();
    const copied = @min(path.len, stored_path.len);
    @memcpy(stored_path[0..copied], path[0..copied]);
    stored_path_len = copied;
    active_flag.store(true, .monotonic);
}

/// Whether census measurement should run. Folds to a comptime `false` outside
/// Debug so gated call sites cost nothing in release.
pub fn active() bool {
    if (comptime !enabled) return false;
    checkEnvOnce();
    return active_flag.load(.monotonic);
}

fn makeIdentification(module_name: []const u8, node_idx: u32, has_node: bool) Identification {
    var id = Identification{ .node_idx = node_idx, .has_node = has_node };
    const copied = @min(module_name.len, id.module.len);
    @memcpy(id.module[0..copied], module_name[0..copied]);
    id.module_len = copied;
    return id;
}

/// Record one legitimate duplicate scheme-use record for the same CIR edge
/// (reunify.md 7.2). `equivalent` is whether the two records resolve to
/// structurally equal content. Divergent identities are retained (bounded) for
/// later location.
pub fn recordSchemeUseDuplicate(equivalent: bool, module_name: []const u8, node_idx: u32) void {
    if (comptime !enabled) return;
    _ = scheme_use_duplicate_edges.fetchAdd(1, .monotonic);
    if (equivalent) {
        _ = scheme_use_duplicates_equivalent.fetchAdd(1, .monotonic);
        return;
    }
    _ = scheme_use_duplicates_divergent.fetchAdd(1, .monotonic);
    lockBuffer();
    defer unlockBuffer();
    if (divergent_id_count < max_identifications) {
        divergent_ids[divergent_id_count] = makeIdentification(module_name, node_idx, true);
        divergent_id_count += 1;
    }
}

/// Record one published scheme whose captured pristine snapshot was compared
/// against its final published root (reunify.md 7.1/7.5, Slice 2). `matches` is
/// whether the snapshot and final roots have equal structural digests: they
/// diverge exactly when an escaped free variable was unified after the
/// generalization boundary. Measures risk-1 — how often publication's mutable
/// root differs from the pristine scheme.
pub fn recordSchemeSnapshot(matches: bool, module_name: []const u8, node_idx: u32) void {
    if (comptime !enabled) return;
    if (matches) {
        _ = scheme_snapshot_matches_final.fetchAdd(1, .monotonic);
        return;
    }
    _ = scheme_snapshot_diverged_from_final.fetchAdd(1, .monotonic);
    lockBuffer();
    defer unlockBuffer();
    if (snapshot_divergent_id_count < max_identifications) {
        snapshot_divergent_ids[snapshot_divergent_id_count] = makeIdentification(module_name, node_idx, true);
        snapshot_divergent_id_count += 1;
    }
}

/// Record one dense ordinary instantiation site (reunify.md 7.2, Slice 2b) whose
/// owning scheme had a local snapshot: bump the per-slot recorded counter and add
/// this site's unreached-binder count. `slot_kind` is a `SchemeUseRecord.Slot`.
pub fn recordSchemeUseSite(slot_kind: u32, unreached: u32) void {
    if (comptime !enabled) return;
    switch (slot_kind) {
        0 => _ = site_recorded_value_use.fetchAdd(1, .monotonic),
        1 => _ = site_recorded_nested.fetchAdd(1, .monotonic),
        2 => _ = site_recorded_dispatch.fetchAdd(1, .monotonic),
        else => {},
    }
    if (unreached > 0) _ = site_binder_unreached.fetchAdd(unreached, .monotonic);
}

/// Record one dense shared in-group instantiation site (reunify.md 7.2, Slice 2b),
/// recorded before its scheme generalized.
pub fn recordSchemeUseSiteShared() void {
    if (comptime !enabled) return;
    _ = site_recorded_shared.fetchAdd(1, .monotonic);
}

/// Record one ordinary instantiation site whose scheme owns no local snapshot —
/// an imported, external, required, or synthetic scheme a later sub-slice handles
/// (reunify.md 7.2, Slice 2b). Measured, not failed. `slot_kind` is a
/// `SchemeUseRecord.Slot`.
pub fn recordSchemeUseSiteWithoutSnapshot(slot_kind: u32) void {
    if (comptime !enabled) return;
    switch (slot_kind) {
        0 => _ = site_without_snapshot_value_use.fetchAdd(1, .monotonic),
        1 => _ = site_without_snapshot_nested.fetchAdd(1, .monotonic),
        2 => _ = site_without_snapshot_dispatch.fetchAdd(1, .monotonic),
        else => {},
    }
}

/// Record whether a shared in-group edge resolved to a dense identity actual
/// vector at publication (`dense` true) or remained a bare marker because its
/// owning definition had no local snapshot (reunify.md 7.2, Slice 2b).
pub fn recordSharedEdgeResolution(dense: bool) void {
    if (comptime !enabled) return;
    if (dense) {
        _ = shared_edges_dense.fetchAdd(1, .monotonic);
    } else {
        _ = shared_edges_marker.fetchAdd(1, .monotonic);
    }
}

/// Record one duplicate dense-site write for the same edge at publication
/// (reunify.md 7.2, Slice 2b). `equivalent` is whether the two records' positional
/// actuals resolve to equal content. With deterministic projection these must be
/// equivalent; a divergence is a checking/publication bug, retained (bounded) for
/// location through the shared divergent-id buffer.
pub fn recordSchemeUseSiteDuplicate(equivalent: bool, module_name: []const u8, node_idx: u32) void {
    if (comptime !enabled) return;
    if (equivalent) {
        _ = site_duplicate_equivalent.fetchAdd(1, .monotonic);
        return;
    }
    _ = site_duplicate_divergent.fetchAdd(1, .monotonic);
    lockBuffer();
    defer unlockBuffer();
    if (divergent_id_count < max_identifications) {
        divergent_ids[divergent_id_count] = makeIdentification(module_name, node_idx, true);
        divergent_id_count += 1;
    }
}

/// Add one module's serialized env and artifact byte totals to the cost
/// checkpoint accumulators (reunify.md 7.2/15.3, Slice 2b). Called per module when
/// the census is active, so the final dump block holds corpus totals.
pub fn addArtifactBytes(env_bytes: u64, artifact_bytes: u64) void {
    if (comptime !enabled) return;
    _ = census_env_bytes.fetchAdd(env_bytes, .monotonic);
    _ = census_artifact_bytes.fetchAdd(artifact_bytes, .monotonic);
    _ = census_module_count.fetchAdd(1, .monotonic);
}

/// Add one published module's dense instantiation-site table sizes to the corpus
/// totals (reunify.md 7.2/15.3, Slice 2b) — the new tables' element counts, from
/// which the byte cost is the count times the POD element size.
pub fn addPublishedSites(sites: u64, actuals: u64) void {
    if (comptime !enabled) return;
    _ = published_sites_total.fetchAdd(sites, .monotonic);
    _ = published_site_actuals_total.fetchAdd(actuals, .monotonic);
}

/// Record one lowerable module whose published checked types reach an `.err`
/// payload (reunify.md 5.4/7.5). The identity is retained (bounded) for later
/// location.
pub fn recordErrReachableInLowerableModule(module_name: []const u8) void {
    if (comptime !enabled) return;
    _ = err_reachable_in_lowerable_module.fetchAdd(1, .monotonic);
    lockBuffer();
    defer unlockBuffer();
    if (err_id_count < max_identifications) {
        err_ids[err_id_count] = makeIdentification(module_name, 0, false);
        err_id_count += 1;
    }
}

/// Record one published nested scheme's captured-binder closure (reunify.md 7.1,
/// Slice 2). `attributed` is how many captured references resolved to an
/// enclosing scheme's published binder; `unattributed` is how many could not be
/// (a free reference that turned out not to be an enclosing generalized binder).
/// A scheme is counted as having captures only when `attributed > 0`.
pub fn recordSchemeCaptures(attributed: u32, unattributed: u32) void {
    if (comptime !enabled) return;
    if (attributed > 0) {
        _ = schemes_with_captures.fetchAdd(1, .monotonic);
        _ = captured_refs_attributed.fetchAdd(attributed, .monotonic);
    }
    if (unattributed > 0) _ = captured_refs_unattributed.fetchAdd(unattributed, .monotonic);
}

/// Record one instantiation site of a scheme defined in ANOTHER module that this
/// sub-slice's imported-binder projection resolved (reunify.md 7.1/7.2, Slice 2):
/// its dense actuals were recorded against the defining module's binder order.
/// `slot_kind` is a `SchemeUseRecord.Slot`.
pub fn recordImportedSchemeSite(slot_kind: u32) void {
    if (comptime !enabled) return;
    switch (slot_kind) {
        2 => _ = site_imported_dispatch.fetchAdd(1, .monotonic),
        else => _ = site_imported_value_use.fetchAdd(1, .monotonic),
    }
}

/// Record whether one published imported-scheme site resolved to the DEFINING
/// module's `CheckedTypeSchemeId` at the consuming side (reunify.md 7.1, Slice 2).
/// Resolution runs at publication of the consuming module against the defining
/// module's serialized owner index (the defining artifact is a loaded import view);
/// `resolved` is whether that lookup succeeded.
pub fn recordImportedSiteResolution(resolved: bool) void {
    if (comptime !enabled) return;
    if (resolved) {
        _ = site_imported_defining_scheme_resolved.fetchAdd(1, .monotonic);
    } else {
        _ = site_imported_without_defining_scheme.fetchAdd(1, .monotonic);
    }
}

/// Record whether one annotated definition's annotation pre-declaration names the
/// same scheme as the definition itself (reunify.md 7.1): `aliased` when the exact
/// witness held and the annotation node's use sites publish under the definition's
/// owner node, `diverged` when the two snapshots disagree on binder count or
/// canonical digest and both owners stay distinct.
pub fn recordAnnotationSchemeOwner(aliased: bool) void {
    if (comptime !enabled) return;
    if (aliased) {
        _ = annotation_scheme_owner_aliased.fetchAdd(1, .monotonic);
    } else {
        _ = annotation_scheme_owner_diverged.fetchAdd(1, .monotonic);
    }
}

/// Which residual-variable disposition outcome a census record counts (reunify.md
/// 7.4): `contextual` (adopts an enclosing use edge's concrete type),
/// `uninhabited` (defaults to the uninhabited leaf, matching today's
/// empty-tag-union materialization), `undisposed` (variable-shaped but not a
/// plain unconstrained residual, so left unclassified), or
/// `adopted_from_dispatch` (an `uninhabited` disposition a resolved dispatch
/// target's own signature later replaced with `contextual`).
pub const DispositionCensus = enum { contextual, uninhabited, undisposed, adopted_from_dispatch };

/// Record one residual-variable disposition decision (reunify.md 7.4, Slice 2 phase
/// one) into the matching census counter.
pub fn recordResidualDisposition(kind: DispositionCensus) void {
    if (comptime !enabled) return;
    switch (kind) {
        .contextual => _ = disposition_contextual.fetchAdd(1, .monotonic),
        .uninhabited => _ = disposition_uninhabited.fetchAdd(1, .monotonic),
        .undisposed => _ = residual_undisposed.fetchAdd(1, .monotonic),
        .adopted_from_dispatch => _ = disposition_adopted_from_dispatch.fetchAdd(1, .monotonic),
    }
}

/// Record whether one local resolved instantiation site's dense actual count equals
/// its scheme's binder count (reunify.md 7.5, Slice 2). Measured rather than
/// hard-asserted because the site actuals and the scheme binders are recorded by
/// two independent checker passes; the corpus run reports whether they agree.
pub fn recordSiteActualsLenMatchesBinders(matches: bool) void {
    if (comptime !enabled) return;
    if (matches) {
        _ = site_actuals_len_matches_binders.fetchAdd(1, .monotonic);
    } else {
        _ = site_actuals_len_diverges_from_binders.fetchAdd(1, .monotonic);
    }
}

/// How one published scheme's root relates to its binding (reunify.md 7.1/7.5),
/// as the boundary verifier classifies it: `ground` reaches no checked variable,
/// `bound` reaches only variables the scheme's own or captured binders name,
/// `disposed` reaches variables outside that binding which carry an explicit
/// final disposition or checked default, and `unaccounted` reaches at least one
/// variable with neither — the one class the invariant forbids.
pub const SchemeBinderCoverage = enum {
    ground,
    bound,
    disposed,
    unaccounted,
};

/// Which owner kind published a scheme, mirroring `CheckedSchemeOwnerKind` so the
/// census does not depend on the checked-artifact module.
pub const SchemeOwnerCensus = enum { top_level_def, nested_def, required_type, synthetic };

/// Record one published scheme's binder-coverage classification (reunify.md
/// 7.1/7.5). Measured rather than hard-asserted while the forbidden class is
/// still being driven to zero; that class is additionally split by owner kind.
pub fn recordSchemeBinderCoverage(coverage: SchemeBinderCoverage, owner: SchemeOwnerCensus) void {
    if (comptime !enabled) return;
    switch (coverage) {
        .ground => _ = scheme_root_ground.fetchAdd(1, .monotonic),
        .bound => _ = scheme_root_bound.fetchAdd(1, .monotonic),
        .disposed => _ = scheme_root_disposed.fetchAdd(1, .monotonic),
        .unaccounted => {
            _ = scheme_root_unaccounted.fetchAdd(1, .monotonic);
            switch (owner) {
                .top_level_def => _ = scheme_unaccounted_top_level.fetchAdd(1, .monotonic),
                .nested_def => _ = scheme_unaccounted_nested.fetchAdd(1, .monotonic),
                .required_type => _ = scheme_unaccounted_required.fetchAdd(1, .monotonic),
                .synthetic => _ = scheme_unaccounted_synthetic.fetchAdd(1, .monotonic),
            }
        },
    }
}

/// How one resolved dispatch plan's published callable is accounted for
/// (reunify.md 6.5/7.4/7.5), as the boundary verifier classifies it. The first
/// three mirror `SchemeBinderCoverage`; `constrained` is the dispatcher-
/// constrained flex `publishResidualDispositions` declares undisposed, held
/// apart so it does not mask the class this measurement targets; `unaccounted`
/// is a variable of any other shape that no binder names and no disposition or
/// default settles. The worst class a callable reaches is the one recorded.
pub const DispatchPlanCallableCoverage = enum {
    ground,
    bound,
    disposed,
    constrained,
    unaccounted,
};

/// Record one resolved dispatch plan's published callable against the same
/// coverage rule the scheme roots answer (reunify.md 6.5/7.4/7.5). Publication
/// writes these callables — projecting an imported target and specializing it
/// against the site — so they are the one published type family whose residuals
/// no solving step produced, and `unaccounted` names exactly that: a variable
/// publication left in a callable after the dispositions were recorded, so
/// nothing published says what value the position takes.
pub fn recordDispatchPlanCallableCoverage(coverage: DispatchPlanCallableCoverage) void {
    if (comptime !enabled) return;
    switch (coverage) {
        .ground => _ = plan_callable_ground.fetchAdd(1, .monotonic),
        .bound => _ = plan_callable_bound.fetchAdd(1, .monotonic),
        .disposed => _ = plan_callable_disposed.fetchAdd(1, .monotonic),
        .constrained => _ = plan_callable_constrained.fetchAdd(1, .monotonic),
        .unaccounted => _ = plan_callable_unaccounted.fetchAdd(1, .monotonic),
    }
}

/// Which validation-matcher (reunify.md 7.6) outcome a census record counts.
pub const MatcherOutcome = enum {
    match,
    mismatch,
    skipped_imported,
    skipped_no_snapshot,
    skipped_unreached,
    skipped_no_scheme,
    skipped_walk,
};

/// Record one validation-matcher outcome (reunify.md 7.6, Slice 2). Sites the
/// matcher actually walked (match/mismatch/skipped_walk) also bump the walked-total.
pub fn recordMatcherOutcome(outcome: MatcherOutcome) void {
    if (comptime !enabled) return;
    switch (outcome) {
        .match => {
            _ = matcher_sites_total.fetchAdd(1, .monotonic);
            _ = matcher_match.fetchAdd(1, .monotonic);
        },
        .mismatch => {
            _ = matcher_sites_total.fetchAdd(1, .monotonic);
            _ = matcher_mismatch.fetchAdd(1, .monotonic);
        },
        .skipped_walk => {
            _ = matcher_sites_total.fetchAdd(1, .monotonic);
            _ = matcher_skipped_walk.fetchAdd(1, .monotonic);
        },
        .skipped_imported => _ = matcher_skipped_imported.fetchAdd(1, .monotonic),
        .skipped_no_snapshot => _ = matcher_skipped_no_snapshot.fetchAdd(1, .monotonic),
        .skipped_unreached => _ = matcher_skipped_unreached.fetchAdd(1, .monotonic),
        .skipped_no_scheme => _ = matcher_skipped_no_scheme.fetchAdd(1, .monotonic),
    }
}

/// Which evidence-carry (reunify.md 9.7, Slice 6) outcome a census record counts,
/// classifying how each dense site's evidence-vector reference was resolved.
pub const EvidenceCarryOutcome = enum {
    dispatch_wired,
    dispatch_no_pair_record,
    dispatch_no_evidence_node,
    value_wired,
    value_no_site_entry,
    shared_wired,
    shared_no_site_entry,
};

/// Record one evidence-carry classification (reunify.md 9.7, Slice 6).
pub fn recordEvidenceCarry(outcome: EvidenceCarryOutcome) void {
    if (comptime !enabled) return;
    switch (outcome) {
        .dispatch_wired => _ = evidence_dispatch_wired.fetchAdd(1, .monotonic),
        .dispatch_no_pair_record => _ = evidence_dispatch_no_pair_record.fetchAdd(1, .monotonic),
        .dispatch_no_evidence_node => _ = evidence_dispatch_no_evidence_node.fetchAdd(1, .monotonic),
        .value_wired => _ = evidence_value_wired.fetchAdd(1, .monotonic),
        .value_no_site_entry => _ = evidence_value_no_site_entry.fetchAdd(1, .monotonic),
        .shared_wired => _ = evidence_shared_wired.fetchAdd(1, .monotonic),
        .shared_no_site_entry => _ = evidence_shared_no_site_entry.fetchAdd(1, .monotonic),
    }
}

/// Retain one bounded detail line describing a validation-matcher mismatch
/// (reunify.md 7.6, Slice 2), so a nonzero mismatch count can be located in the
/// corpus and investigated.
pub fn recordMatcherMismatchDetail(text: []const u8) void {
    if (comptime !enabled) return;
    lockBuffer();
    defer unlockBuffer();
    if (matcher_mismatch_detail_count >= max_identifications) return;
    const copied = @min(text.len, detail_capacity);
    @memcpy(matcher_mismatch_details[matcher_mismatch_detail_count][0..copied], text[0..copied]);
    matcher_mismatch_detail_lens[matcher_mismatch_detail_count] = copied;
    matcher_mismatch_detail_count += 1;
}

const Sink = struct {
    data: []u8,
    len: usize = 0,

    fn print(self: *Sink, comptime fmt: []const u8, args: anytype) void {
        const written = std.fmt.bufPrint(self.data[self.len..], fmt, args) catch return;
        self.len += written.len;
    }

    fn slice(self: *const Sink) []const u8 {
        return self.data[0..self.len];
    }
};

/// Append one `name=value` census snapshot block to the enabled dump path.
/// Called at an end-of-checking point by the compile driver. Fresh reads of the
/// atomic counters make each block a complete snapshot, so the last block
/// appended holds the run's final totals. A no-op when the census is disabled.
/// Every failure is silent: the census must never perturb a compile. The write
/// goes directly through libc with `O_APPEND` — Debug-only measurement
/// plumbing, deliberately outside the compiler's file-system abstraction, and
/// atomic enough that concurrent corpus processes interleave whole blocks.
pub fn dumpAppend() void {
    if (comptime !enabled) return;
    if (!active()) return;

    lockBuffer();
    defer unlockBuffer();

    if (stored_path_len == 0) return;
    if (stored_path_len >= stored_path.len) return;
    stored_path[stored_path_len] = 0;
    const path_z: [*:0]const u8 = @ptrCast(stored_path[0..stored_path_len :0]);

    var buffer: [8192]u8 = undefined;
    var sink = Sink{ .data = &buffer };
    sink.print("# reunify check census (Slice 0)\n", .{});
    sink.print("scheme_use_duplicate_edges={d}\n", .{scheme_use_duplicate_edges.load(.monotonic)});
    sink.print("scheme_use_duplicates_equivalent={d}\n", .{scheme_use_duplicates_equivalent.load(.monotonic)});
    sink.print("scheme_use_duplicates_divergent={d}\n", .{scheme_use_duplicates_divergent.load(.monotonic)});
    sink.print("err_reachable_in_lowerable_module={d}\n", .{err_reachable_in_lowerable_module.load(.monotonic)});
    sink.print("scheme_snapshot_matches_final={d}\n", .{scheme_snapshot_matches_final.load(.monotonic)});
    sink.print("scheme_snapshot_diverged_from_final={d}\n", .{scheme_snapshot_diverged_from_final.load(.monotonic)});
    sink.print("site_recorded_value_use={d}\n", .{site_recorded_value_use.load(.monotonic)});
    sink.print("site_recorded_nested={d}\n", .{site_recorded_nested.load(.monotonic)});
    sink.print("site_recorded_dispatch={d}\n", .{site_recorded_dispatch.load(.monotonic)});
    sink.print("site_recorded_shared={d}\n", .{site_recorded_shared.load(.monotonic)});
    sink.print("site_binder_unreached={d}\n", .{site_binder_unreached.load(.monotonic)});
    sink.print("site_without_snapshot_value_use={d}\n", .{site_without_snapshot_value_use.load(.monotonic)});
    sink.print("site_without_snapshot_nested={d}\n", .{site_without_snapshot_nested.load(.monotonic)});
    sink.print("site_without_snapshot_dispatch={d}\n", .{site_without_snapshot_dispatch.load(.monotonic)});
    sink.print("site_duplicate_equivalent={d}\n", .{site_duplicate_equivalent.load(.monotonic)});
    sink.print("site_duplicate_divergent={d}\n", .{site_duplicate_divergent.load(.monotonic)});
    sink.print("shared_edges_dense={d}\n", .{shared_edges_dense.load(.monotonic)});
    sink.print("shared_edges_marker={d}\n", .{shared_edges_marker.load(.monotonic)});
    sink.print("published_sites_total={d}\n", .{published_sites_total.load(.monotonic)});
    sink.print("published_site_actuals_total={d}\n", .{published_site_actuals_total.load(.monotonic)});
    sink.print("census_module_count={d}\n", .{census_module_count.load(.monotonic)});
    sink.print("census_env_bytes={d}\n", .{census_env_bytes.load(.monotonic)});
    sink.print("census_artifact_bytes={d}\n", .{census_artifact_bytes.load(.monotonic)});
    sink.print("schemes_with_captures={d}\n", .{schemes_with_captures.load(.monotonic)});
    sink.print("captured_refs_attributed={d}\n", .{captured_refs_attributed.load(.monotonic)});
    sink.print("captured_refs_unattributed={d}\n", .{captured_refs_unattributed.load(.monotonic)});
    sink.print("site_imported_value_use={d}\n", .{site_imported_value_use.load(.monotonic)});
    sink.print("site_imported_dispatch={d}\n", .{site_imported_dispatch.load(.monotonic)});
    sink.print("site_imported_defining_scheme_resolved={d}\n", .{site_imported_defining_scheme_resolved.load(.monotonic)});
    sink.print("site_imported_without_defining_scheme={d}\n", .{site_imported_without_defining_scheme.load(.monotonic)});
    sink.print("annotation_scheme_owner_aliased={d}\n", .{annotation_scheme_owner_aliased.load(.monotonic)});
    sink.print("annotation_scheme_owner_diverged={d}\n", .{annotation_scheme_owner_diverged.load(.monotonic)});
    sink.print("disposition_contextual={d}\n", .{disposition_contextual.load(.monotonic)});
    sink.print("disposition_uninhabited={d}\n", .{disposition_uninhabited.load(.monotonic)});
    sink.print("residual_undisposed={d}\n", .{residual_undisposed.load(.monotonic)});
    sink.print("disposition_adopted_from_dispatch={d}\n", .{disposition_adopted_from_dispatch.load(.monotonic)});
    sink.print("site_actuals_len_matches_binders={d}\n", .{site_actuals_len_matches_binders.load(.monotonic)});
    sink.print("site_actuals_len_diverges_from_binders={d}\n", .{site_actuals_len_diverges_from_binders.load(.monotonic)});
    sink.print("scheme_root_ground={d}\n", .{scheme_root_ground.load(.monotonic)});
    sink.print("scheme_root_bound={d}\n", .{scheme_root_bound.load(.monotonic)});
    sink.print("scheme_root_disposed={d}\n", .{scheme_root_disposed.load(.monotonic)});
    sink.print("scheme_root_unaccounted={d}\n", .{scheme_root_unaccounted.load(.monotonic)});
    sink.print("scheme_unaccounted_top_level={d}\n", .{scheme_unaccounted_top_level.load(.monotonic)});
    sink.print("scheme_unaccounted_nested={d}\n", .{scheme_unaccounted_nested.load(.monotonic)});
    sink.print("scheme_unaccounted_required={d}\n", .{scheme_unaccounted_required.load(.monotonic)});
    sink.print("scheme_unaccounted_synthetic={d}\n", .{scheme_unaccounted_synthetic.load(.monotonic)});
    sink.print("plan_callable_ground={d}\n", .{plan_callable_ground.load(.monotonic)});
    sink.print("plan_callable_bound={d}\n", .{plan_callable_bound.load(.monotonic)});
    sink.print("plan_callable_disposed={d}\n", .{plan_callable_disposed.load(.monotonic)});
    sink.print("plan_callable_constrained={d}\n", .{plan_callable_constrained.load(.monotonic)});
    sink.print("plan_callable_unaccounted={d}\n", .{plan_callable_unaccounted.load(.monotonic)});
    sink.print("matcher_sites_total={d}\n", .{matcher_sites_total.load(.monotonic)});
    sink.print("matcher_match={d}\n", .{matcher_match.load(.monotonic)});
    sink.print("matcher_mismatch={d}\n", .{matcher_mismatch.load(.monotonic)});
    sink.print("matcher_skipped_imported={d}\n", .{matcher_skipped_imported.load(.monotonic)});
    sink.print("matcher_skipped_no_snapshot={d}\n", .{matcher_skipped_no_snapshot.load(.monotonic)});
    sink.print("matcher_skipped_unreached={d}\n", .{matcher_skipped_unreached.load(.monotonic)});
    sink.print("matcher_skipped_no_scheme={d}\n", .{matcher_skipped_no_scheme.load(.monotonic)});
    sink.print("matcher_skipped_walk={d}\n", .{matcher_skipped_walk.load(.monotonic)});
    sink.print("evidence_dispatch_wired={d}\n", .{evidence_dispatch_wired.load(.monotonic)});
    sink.print("evidence_dispatch_no_pair_record={d}\n", .{evidence_dispatch_no_pair_record.load(.monotonic)});
    sink.print("evidence_dispatch_no_evidence_node={d}\n", .{evidence_dispatch_no_evidence_node.load(.monotonic)});
    sink.print("evidence_value_wired={d}\n", .{evidence_value_wired.load(.monotonic)});
    sink.print("evidence_value_no_site_entry={d}\n", .{evidence_value_no_site_entry.load(.monotonic)});
    sink.print("evidence_shared_wired={d}\n", .{evidence_shared_wired.load(.monotonic)});
    sink.print("evidence_shared_no_site_entry={d}\n", .{evidence_shared_no_site_entry.load(.monotonic)});
    for (divergent_ids[0..divergent_id_count], 0..) |id, i| {
        sink.print("divergent_scheme_use_{d}={s}:node{d}\n", .{ i, id.moduleText(), id.node_idx });
    }
    for (snapshot_divergent_ids[0..snapshot_divergent_id_count], 0..) |id, i| {
        sink.print("snapshot_diverged_{d}={s}:node{d}\n", .{ i, id.moduleText(), id.node_idx });
    }
    for (err_ids[0..err_id_count], 0..) |id, i| {
        sink.print("err_reachable_{d}={s}\n", .{ i, id.moduleText() });
    }
    for (0..divergent_detail_count) |i| {
        sink.print("divergent_detail_{d}={s}\n", .{ i, divergent_details[i][0..divergent_detail_lens[i]] });
    }
    for (0..matcher_mismatch_detail_count) |i| {
        sink.print("matcher_mismatch_detail_{d}={s}\n", .{ i, matcher_mismatch_details[i][0..matcher_mismatch_detail_lens[i]] });
    }

    const fd = std.c.open(path_z, .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true }, @as(std.c.mode_t, 0o644));
    if (fd < 0) return;
    defer _ = std.c.close(fd);
    var remaining = sink.slice();
    while (remaining.len > 0) {
        const written = std.c.write(fd, remaining.ptr, remaining.len);
        if (written <= 0) return;
        remaining = remaining[@intCast(written)..];
    }
}

test "recording is a no-op outside Debug and bounded inside it" {
    // The recording entry points must never fail; exercise them so the module's
    // decls type-check under the check-stage test step.
    recordSchemeUseDuplicate(true, "Main", 3);
    recordSchemeUseDuplicate(false, "Main", 7);
    recordErrReachableInLowerableModule("Main");
    recordSchemeSnapshot(true, "Main", 3);
    recordSchemeSnapshot(false, "Main", 9);
    try std.testing.expect(builtin.mode != .Debug or scheme_use_duplicate_edges.load(.monotonic) >= 2);
    try std.testing.expect(builtin.mode != .Debug or scheme_snapshot_matches_final.load(.monotonic) >= 1);
}
