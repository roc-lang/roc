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
/// recorded before its scheme generalized. `slot_kind` is a `SchemeUseRecord.Slot`.
pub fn recordSchemeUseSiteShared(slot_kind: u32) void {
    if (comptime !enabled) return;
    _ = slot_kind;
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
