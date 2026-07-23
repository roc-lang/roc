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
    /// Every checked module view available to lowering, keyed by `key_bytes`, so
    /// an imported-scheme site can resolve its defining module's frozen types and
    /// names (reunify.md 7.1, Slice 6). Includes the root; the consuming module is
    /// found here too.
    modules_by_hash: []const ShadowModule = &.{},
};

/// The defining module of an imported scheme, or null when that module was not
/// among the loaded views.
fn moduleByHash(inputs: Inputs, hash: [32]u8) ?ShadowModule {
    for (inputs.modules_by_hash) |module| {
        if (std.mem.eql(u8, &module.key_bytes, &hash)) return module;
    }
    return null;
}

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

    try runConcreteRoots(allocator, &logical, inputs, &counts, &details);
    try runSchemeEdges(allocator, &logical, inputs, &counts, &details);

    dump(allocator, &counts, details.items);
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
/// resolve it. The consuming context translates a site's actuals and
/// instantiated root; the scheme context resolves the scheme root and binders.
/// They differ exactly when the site instantiates an imported scheme
/// (reunify.md 7.1).
const TranslationContext = struct {
    view: checked.CheckedTypeStoreView,
    source_names: *const names.NameStore,
    module_bytes: [32]u8,
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

            // Resolve the scheme root and binders. A local site reads them from
            // the consuming module; an imported site reads them from the defining
            // module's own frozen types under the defining scheme id (reunify.md
            // 7.1, Slice 6).
            var scheme_ctx = consuming;
            if (site.importedDefiningModule()) |defining_hash| {
                const defining = moduleByHash(inputs, defining_hash) orelse {
                    counts.scheme_skipped_imported += 1;
                    continue;
                };
                scheme_ctx = .{
                    .view = defining.view,
                    .source_names = defining.source_names,
                    .module_bytes = defining.key_bytes,
                };
            }

            const scheme = scheme_ctx.view.schemeById(scheme_id) orelse {
                counts.scheme_skipped_unresolved_scheme += 1;
                continue;
            };
            if (scheme.snapshotRoot() == null) {
                counts.scheme_skipped_no_snapshot += 1;
                continue;
            }

            const binders = scheme.generalizedVars(scheme_ctx.view);
            // The actuals are the CONSUMING module's checked ids at this edge,
            // whether or not the scheme is imported.
            const actuals = site.actuals(source.store);
            if (actuals.len != binders.len) {
                counts.scheme_skipped_arity_mismatch += 1;
                continue;
            }

            try compareSchemeEdge(allocator, logical, consuming, scheme_ctx, scheme, binders, actuals, site, counts, details);
        }
    }
}

fn compareSchemeEdge(
    allocator: Allocator,
    logical: *LogicalStore,
    consuming: TranslationContext,
    scheme_ctx: TranslationContext,
    scheme: checked.CheckedTypeScheme,
    binders: []const checked.CheckedTypeId,
    actuals: []const checked.CheckedTypeId,
    site: checked.CheckedInstantiationSite,
    counts: *ShadowCensus,
    details: *std.ArrayList(MismatchDetail),
) Allocator.Error!void {
    const imported = !std.mem.eql(u8, &consuming.module_bytes, &scheme_ctx.module_bytes);
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

    // Instantiate the scheme root under its own (possibly imported) module's
    // frozen types and names; the binding carries the consuming-side actuals.
    var inst_reason: SkipReason = undefined;
    const instantiated = logical.instantiateScheme(
        .{ .module_bytes = scheme_ctx.module_bytes, .scheme = @intFromEnum(scheme.id) },
        scheme_ctx.view,
        scheme_ctx.source_names,
        scheme.owner_node,
        scheme.root,
        binders,
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
        if (imported) counts.scheme_match_imported += 1 else counts.scheme_match += 1;
    } else if (try logical.alphaEqual(instantiated, direct)) {
        // Equal up to a renaming of enclosing binders that appear as independent
        // fresh copies on the two sides (a binders=0 nested scheme's root versus
        // the site's instantiated root); a real transposition is not accepted
        // (reunify.md 7.3, 7.6). Counted apart from an exact-id match so these
        // stay visible rather than folded into the mismatch total.
        counts.scheme_match_alpha += 1;
    } else {
        if (imported) counts.scheme_mismatch_imported += 1 else counts.scheme_mismatch += 1;
        try recordMismatch(allocator, logical, details, .{
            .scope = if (imported) "scheme_imported" else "scheme",
            .module_bytes = consuming.module_bytes,
            .checked_ty = site.instantiated_root,
            .shadow_id = instantiated,
            .other_id = direct,
            .owner_kind = @intFromEnum(scheme.owner_kind),
            .use_node = site.use_node,
            .binders = @intCast(binders.len),
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

/// Append the counter dump and any bounded mismatch detail to the census file.
fn dump(allocator: Allocator, counts: *ShadowCensus, details: []const MismatchDetail) void {
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
