//! Debug-only counters for what the Monotype instantiation graph decides.
//!
//! Slice 0 measures the current post-check meaning before any of it moves.
//! Each counter answers one classification question about the graph's work:
//! symmetric and directional row merges, import extension widening, checked
//! defaulting, deferred procedure requests, iterator relations, request
//! refinement, backed aliases owned by a builtin, and the Lambda Solved
//! score ties. Every counter is inert outside Debug builds, so release
//! lowering keeps its exact behavior.

const std = @import("std");
const builtin = @import("builtin");

/// All counting and the dump are compiled out unless this is a Debug build
/// on a 64-bit non-wasm target: the counters are 64-bit atomics and the dump
/// reads an env var, neither of which the wasm builds support.
pub const enabled = builtin.mode == .Debug and
    !builtin.target.cpu.arch.isWasm() and
    builtin.target.ptrBitWidth() >= 64 and
    builtin.os.tag != .freestanding;

const Counter = std.atomic.Value(u64);

/// One atomic u64 per classification question. Each field name is the text
/// the dump writes on its line, so a corpus run reads the names directly.
pub const Census = struct {
    two_sided_tag_row_merge: Counter = Counter.init(0),
    two_sided_record_row_merge: Counter = Counter.init(0),
    one_sided_tag_row_merge: Counter = Counter.init(0),
    one_sided_record_row_merge: Counter = Counter.init(0),
    import_tag_ext_kept_open: Counter = Counter.init(0),
    import_record_ext_kept_open: Counter = Counter.init(0),
    import_ext_widened: Counter = Counter.init(0),
    plain_variable_to_empty_tag_union: Counter = Counter.init(0),
    empty_tag_union_yield: Counter = Counter.init(0),
    nominal_backing_root_join: Counter = Counter.init(0),
    iter_public_minted: Counter = Counter.init(0),
    iter_forced_dynamic: Counter = Counter.init(0),
    iter_minted_join: Counter = Counter.init(0),
    numeric_default_applied: Counter = Counter.init(0),
    row_default_applied: Counter = Counter.init(0),
    expected_return_constraint_bound: Counter = Counter.init(0),
    deferred_request_sealed_shape_changed: Counter = Counter.init(0),
    deferred_request_recursive: Counter = Counter.init(0),
    deferred_request_nonrecursive: Counter = Counter.init(0),
    generated_opaque_evidence_gate: Counter = Counter.init(0),
    request_refined: Counter = Counter.init(0),
    request_refined_digest_changed: Counter = Counter.init(0),
    solved_digest_differs_from_request: Counter = Counter.init(0),
    builtin_owned_alias_created: Counter = Counter.init(0),
    lambda_alias_unwrap_builtin_owned: Counter = Counter.init(0),
    lambda_generated_backing_equal_score: Counter = Counter.init(0),
    // reunify.md 7.1, Slice 2: how a procedure binding's source scheme root was
    // resolved. `by_id` is the dense scheme id carried on the binding; `by_content_digest`
    // is the content-key lookup used only when the binding stored no id.
    scheme_lookup_by_id: Counter = Counter.init(0),
    scheme_lookup_by_content_digest: Counter = Counter.init(0),
    // reunify.md 8.1, Slice 3: how often a type digest walk exhausts its fixed
    // visiting stack and digests the content shape instead of recursing. The
    // count measures whether any corpus type is deep enough to reach the cap;
    // the shape digest uses the content variant, never an allocation id.
    digest_stack_depth_exceeded: Counter = Counter.init(0),
    // reunify.md 8.1, Slice 3: interner outcomes. `intern_hit` reuses an existing
    // id after an exact-equality bucket match; `intern_miss` adds a fresh id.
    intern_hit: Counter = Counter.init(0),
    intern_miss: Counter = Counter.init(0),
    // reunify.md section 9, Slice 7 Stage A: the directed stored-form translation
    // probe. For each concrete checked root that lowering translated to a
    // Monotype id, the directed translation's stored digest is compared with the
    // graph's stored digest. `match` counts equal stored digests; `mismatch`
    // counts unequal, split by whether the graph type carries iterator/generated
    // representation content (`mismatch_representation`, expected until Stage B
    // supplies interface outputs) or does not (`mismatch_logical`, which must be
    // zero — an unequal representation-free stored form is a translation bug).
    // The skip counters record roots outside the translatable subset.
    direct_stored_match: Counter = Counter.init(0),
    direct_stored_mismatch: Counter = Counter.init(0),
    direct_stored_mismatch_representation: Counter = Counter.init(0),
    direct_stored_mismatch_logical: Counter = Counter.init(0),
    direct_stored_skip_recursive: Counter = Counter.init(0),
    direct_stored_skip_open_row: Counter = Counter.init(0),
    direct_stored_skip_other: Counter = Counter.init(0),
    // reunify.md section 10, Slice 7 Stage B: the representation closure engine
    // driven from the graph as an inert shadow. Wherever the graph applies a
    // representation decision, the same relation is mirrored into engine slots,
    // sealed at the graph's seal point, and the engine's sealed representation
    // descriptor (tier/kind/depth/owner) is compared against the graph-sealed
    // node's representation content. `match` counts equal descriptors; `mismatch`
    // counts unequal — an engine rule gap the flip must not carry. The per-rule
    // counters split both by the section 10.3 rule that placed the slot in its
    // class. Every mismatch is measured, never a panic.
    representation_mirror_match: Counter = Counter.init(0),
    representation_mirror_mismatch: Counter = Counter.init(0),
    representation_mirror_match_public_minted: Counter = Counter.init(0),
    representation_mirror_mismatch_public_minted: Counter = Counter.init(0),
    representation_mirror_match_forced_dynamic: Counter = Counter.init(0),
    representation_mirror_mismatch_forced_dynamic: Counter = Counter.init(0),
    representation_mirror_match_minted_join: Counter = Counter.init(0),
    representation_mirror_mismatch_minted_join: Counter = Counter.init(0),
    representation_mirror_match_evidence: Counter = Counter.init(0),
    representation_mirror_mismatch_evidence: Counter = Counter.init(0),
    // A mirrored `relate` refused its operands as logically unequal. Because the
    // graph only relates logically-equal nodes at these sites, a rejection is a
    // mirror-side token or slot-shape imprecision, recorded rather than asserted.
    representation_mirror_relate_rejected: Counter = Counter.init(0),
    // The sanctioned nominal-backing relation (two equal-identity nominals whose
    // backings the graph relates) mirrored into the engine as a component
    // equality of the two nominal wrappers. Counts the applied relations.
    representation_mirror_nominal_backing_related: Counter = Counter.init(0),
    // The generic try-the-backing-on-head-mismatch path, reunify.md section
    // 10.5: dying bookkeeping the flip deletes, not a section 10.3 edge. It is
    // counted when it fires rather than mirrored into the engine.
    nominal_generic_mismatch_path_fired: Counter = Counter.init(0),
    // reunify.md section 11.1, Slice 7 Stage B: the interface reservation trial.
    // For each specialization the graph lowers, argument and result
    // representation slots are reserved in the mirror before body lowering;
    // `gained_info` counts positions whose representation tier moved up during
    // body discovery, and `gained_info_nonrecursive` restricts that to
    // specializations that made no recursive self-request (the section 11
    // openness measurement on live data, at slot granularity).
    interface_slots_reserved: Counter = Counter.init(0),
    interface_slots_gained_info: Counter = Counter.init(0),
    gained_info_nonrecursive: Counter = Counter.init(0),
    // reunify.md section 11.1/11.5, Slice 7 Stage C: the parallel FinalSpecId
    // computed on the production spec builder at `markReady`. `computed` counts a
    // record whose request type reduced to a FinalSpecId; `skipped` counts one
    // whose request left the representation-reducible subset (recursive, open
    // row, zero sized). The collision counters key by FinalSpecId: two records
    // sharing one are the same specialization and must reduce to structurally
    // equal solved skeletons — `equivalent` counts a matching repeat, `divergent`
    // a mismatch (a red flag the flip must not carry; expected zero),
    // `solved_skipped` a repeat whose solved witness could not be compared. This
    // is the production port of the shadow's `spec_seal_*`/`spec_collisions_*`
    // sealing census, driven on live records instead of a read-only post-pass.
    final_spec_id_computed: Counter = Counter.init(0),
    final_spec_id_skipped: Counter = Counter.init(0),
    final_spec_id_collisions_equivalent: Counter = Counter.init(0),
    final_spec_id_collisions_divergent: Counter = Counter.init(0),
    final_spec_id_collisions_solved_skipped: Counter = Counter.init(0),
};

/// The single process-wide census. A corpus run accumulates into it and the
/// pipeline dumps it once lowering finishes.
pub var global: Census = .{};

/// Add one to the named counter. Inert outside Debug builds. `name` is a
/// field of `Census`, checked at compile time.
pub inline fn bump(comptime name: []const u8) void {
    if (!enabled) return;
    _ = @field(global, name).fetchAdd(1, .monotonic);
}

/// Render every counter as a `name value` line. Inert outside Debug builds.
/// The caller owns and frees the returned bytes.
pub fn dumpText(allocator: std.mem.Allocator) std.mem.Allocator.Error![]u8 {
    var out: std.ArrayList(u8) = .empty;
    errdefer out.deinit(allocator);
    if (enabled) {
        inline for (@typeInfo(Census).@"struct".fields) |field| {
            const value = @field(global, field.name).load(.monotonic);
            const line = try std.fmt.allocPrint(allocator, "{s} {d}\n", .{ field.name, value });
            defer allocator.free(line);
            try out.appendSlice(allocator, line);
        }
    }
    return out.toOwnedSlice(allocator);
}

/// When `ROC_REUNIFY_CENSUS` names a file, append the counter dump to it.
/// The census owns this write directly through libc — it is Debug-only
/// measurement plumbing, deliberately outside the compiler's file-system
/// abstraction, and every failure is silent so lowering is never affected.
pub fn appendDumpToEnvPath(allocator: std.mem.Allocator) void {
    if (comptime !enabled) return;
    const raw_path = std.c.getenv("ROC_REUNIFY_CENSUS") orelse return;
    const path = raw_path[0..std.mem.len(raw_path)];
    if (path.len == 0) return;
    const text = dumpText(allocator) catch return;
    defer allocator.free(text);
    if (text.len == 0) return;
    appendToFile(raw_path, text);
}

/// Append bytes to the named file through libc with `O_APPEND`, so multiple
/// processes measuring one corpus interleave whole writes rather than
/// clobbering each other's offsets.
pub fn appendToFile(path: [*:0]const u8, bytes: []const u8) void {
    if (comptime !enabled) return;
    const fd = std.c.open(path, .{ .ACCMODE = .WRONLY, .CREAT = true, .APPEND = true }, @as(std.c.mode_t, 0o644));
    if (fd < 0) return;
    defer _ = std.c.close(fd);
    var remaining = bytes;
    while (remaining.len > 0) {
        const written = std.c.write(fd, remaining.ptr, remaining.len);
        if (written <= 0) return;
        remaining = remaining[@intCast(written)..];
    }
}
