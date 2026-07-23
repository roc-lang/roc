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
};

/// Whether the shadow should run: compiled in, and turned on by the env var.
pub fn shouldRun() bool {
    if (comptime !enabled) return false;
    const raw = std.c.getenv("ROC_REUNIFY_SHADOW") orelse return false;
    return std.mem.len(raw) > 0;
}

const max_mismatch_details = 32;

const MismatchDetail = struct {
    scope: []const u8,
    module_bytes: [32]u8,
    checked_ty: u32,
    shadow_digest: [32]u8,
    other_digest: [32]u8,
};

/// The shadow's outcome counters. Single-threaded (the shadow runs on the
/// lowering thread after sealing), so plain integers suffice.
const ShadowCensus = struct {
    concrete_match: u64 = 0,
    concrete_mismatch: u64 = 0,
    scheme_match: u64 = 0,
    scheme_mismatch: u64 = 0,

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
    scheme_skipped_captured_closure: u64 = 0,
    scheme_skipped_open_actual: u64 = 0,

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
    defer details.deinit(allocator);

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
            try recordMismatch(allocator, logical, details, "concrete", module.key_bytes, root.checked_ty, checked_id, mono_id);
        }
    }
}

fn runSchemeEdges(
    allocator: Allocator,
    logical: *LogicalStore,
    inputs: Inputs,
    counts: *ShadowCensus,
    details: *std.ArrayList(MismatchDetail),
) Allocator.Error!void {
    for (inputs.scheme_sources) |source| {
        const view = source.store.view();
        for (source.store.instantiationSites()) |site| {
            // Only local edges: an imported scheme lives in another module's
            // types, translated with that module's names — outside this slice.
            if (site.importedDefiningModule() != null) {
                counts.scheme_skipped_imported += 1;
                continue;
            }
            const scheme_id = site.schemeId() orelse {
                counts.scheme_skipped_unresolved_scheme += 1;
                continue;
            };
            const scheme = view.schemeById(scheme_id) orelse {
                counts.scheme_skipped_unresolved_scheme += 1;
                continue;
            };
            if (scheme.snapshotRoot() == null) {
                counts.scheme_skipped_no_snapshot += 1;
                continue;
            }
            if (scheme.captured_len != 0) {
                counts.scheme_skipped_captured_closure += 1;
                continue;
            }

            const binders = scheme.generalizedVars(view);
            const actuals = site.actuals(source.store);
            if (actuals.len != binders.len) {
                counts.scheme_skipped_arity_mismatch += 1;
                continue;
            }

            try compareSchemeEdge(allocator, logical, source, view, scheme, binders, actuals, site, counts, details);
        }
    }
}

fn compareSchemeEdge(
    allocator: Allocator,
    logical: *LogicalStore,
    source: SchemeSource,
    view: checked.CheckedTypeStoreView,
    scheme: checked.CheckedTypeScheme,
    binders: []const checked.CheckedTypeId,
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
        // A fully concrete binding has no free variable in any actual. An
        // actual that is a free binder of an enclosing scheme is
        // resolved under the caller environment in Slice 6, not here, so such
        // an edge is outside the Slice 5 closed subset.
        if (!try logic.LogicalStore.isConcreteBinding(allocator, view, actual)) {
            counts.scheme_skipped_open_actual += 1;
            return;
        }
        var reason: SkipReason = undefined;
        const actual_logical = logical.checkedLogicalIdentity(view, source.source_names, actual, &reason) catch |err| switch (err) {
            error.Skip => {
                counts.bumpSkip(reason);
                return;
            },
            else => |other| return other,
        };
        try binding.append(allocator, logic.BoundType.closed(actual_logical));
    }

    var inst_reason: SkipReason = undefined;
    const instantiated = logical.instantiateScheme(
        .{ .module_bytes = source.module_bytes, .scheme = @intFromEnum(scheme.id) },
        view,
        source.source_names,
        scheme.owner_node,
        scheme.root,
        binders,
        binding.items,
        &.{},
        &inst_reason,
    ) catch |err| switch (err) {
        error.Skip => {
            counts.bumpSkip(inst_reason);
            return;
        },
        else => |other| return other,
    };

    var direct_reason: SkipReason = undefined;
    const direct = logical.checkedLogicalIdentity(view, source.source_names, site.instantiated_root, &direct_reason) catch |err| switch (err) {
        error.Skip => {
            counts.bumpSkip(direct_reason);
            return;
        },
        else => |other| return other,
    };

    if (instantiated == direct) {
        counts.scheme_match += 1;
    } else {
        counts.scheme_mismatch += 1;
        try recordMismatch(allocator, logical, details, "scheme", source.module_bytes, site.instantiated_root, direct, instantiated);
    }
}

fn recordMismatch(
    allocator: Allocator,
    logical: *LogicalStore,
    details: *std.ArrayList(MismatchDetail),
    scope: []const u8,
    module_bytes: [32]u8,
    checked_ty: checked.CheckedTypeId,
    shadow_id: logic.LogicalTypeIdentity,
    other_id: logic.LogicalTypeIdentity,
) Allocator.Error!void {
    if (details.items.len >= max_mismatch_details) return;
    try details.append(allocator, .{
        .scope = scope,
        .module_bytes = module_bytes,
        .checked_ty = @intFromEnum(checked_ty),
        .shadow_digest = logical.digestBytes(shadow_id),
        .other_digest = logical.digestBytes(other_id),
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
            "shadow_mismatch_detail scope={s} module={s} checked_ty={d} shadow={s} production={s}\n",
            .{ detail.scope, &module_hex, detail.checked_ty, &shadow_hex, &production_hex },
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
