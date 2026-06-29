const std = @import("std");
const builtin = @import("builtin");
const build_options = @import("build_options");

pub const enable_runtime_metrics = builtin.is_test or build_options.metrics;

pub const RuntimeMetrics = struct {
    active_graph_records_rebuilt: u64,
    append_child: u64,
    active_intervals_synced: u64,
    allocs_this_event: u64,
    bind_event: u64,
    closure_releases: u64,
    closure_retains: u64,
    create_element: u64,
    deallocs_this_event: u64,
    derived_calls_into_roc: u64,
    each_key_compares: u64,
    each_key_hashes: u64,
    each_key_reuse_compares: u64,
    each_key_duplicate_compares: u64,
    each_item_compares: u64,
    each_syncs: u64,
    each_sync_keys: u64,
    each_sync_existing_rows: u64,
    events_processed: u64,
    host_alloc_bytes_this_event: u64,
    host_allocs_this_event: u64,
    host_dealloc_bytes_this_event: u64,
    host_deallocs_this_event: u64,
    host_retained_alloc_delta: i64,
    host_retained_bytes_delta: i64,
    move_before: u64,
    nodes_recomputed: u64,
    patches_emitted: u64,
    propagation_prunes: u64,
    recompute_batches: u64,
    remove_node: u64,
    render_indexes_refreshed: u64,
    retained_alloc_delta: i64,
    reset_dom: u64,
    rows_created: u64,
    rows_removed: u64,
    rows_reused: u64,
    scopes_created: u64,
    scopes_disposed: u64,
    set_checked: u64,
    set_disabled: u64,
    set_metadata: u64,
    set_text: u64,
    set_value: u64,
    signal_record_table_rebuilt: u64,
    stream_nodes_scanned: u64,
    stream_nodes_scanned_apply: u64,
    stream_nodes_scanned_children: u64,
    stream_nodes_scanned_dirty_scope: u64,
    stream_nodes_scanned_events: u64,
    stream_nodes_scanned_mounts: u64,
    stream_nodes_scanned_remove_target: u64,
    stream_nodes_scanned_render_scope: u64,
    stream_nodes_scanned_splice: u64,

    pub const Field = std.meta.FieldEnum(@This());

    pub inline fn bump(self: *RuntimeMetrics, comptime field: Field, n: u64) void {
        if (comptime !enable_runtime_metrics) return;
        @field(self, @tagName(field)) += n;
    }
};

pub fn zeroRuntimeMetrics() RuntimeMetrics {
    return std.mem.zeroes(RuntimeMetrics);
}

pub fn addRuntimeMetrics(left: RuntimeMetrics, right: RuntimeMetrics) RuntimeMetrics {
    var result: RuntimeMetrics = undefined;
    inline for (std.meta.fields(RuntimeMetrics)) |field| {
        @field(result, field.name) = @field(left, field.name) + @field(right, field.name);
    }
    return result;
}

/// Zero-size stand-in for `RuntimeMetrics`: every `bump` is a comptime no-op.
pub const NoMetrics = struct {
    pub const Field = RuntimeMetrics.Field;

    pub inline fn bump(_: *NoMetrics, comptime _: Field, _: u64) void {}
};

/// Dispatch counters, accumulated per host event and folded into the finalized
/// runtime metrics. Engine-owned so both hosts share one dispatch path.
pub const DispatchMetrics = struct {
    events_processed: u64 = 0,
    recompute_batches: u64 = 0,
};

test "zeroRuntimeMetrics clears every runtime metric field" {
    const metrics = zeroRuntimeMetrics();

    inline for (std.meta.fields(RuntimeMetrics)) |field| {
        switch (field.type) {
            u64 => try std.testing.expectEqual(@as(u64, 0), @field(metrics, field.name)),
            i64 => try std.testing.expectEqual(@as(i64, 0), @field(metrics, field.name)),
            else => @compileError("runtime metrics must stay scalar counters"),
        }
    }
}

test "RuntimeMetrics bump updates requested counter when metrics are enabled" {
    var metrics = zeroRuntimeMetrics();
    metrics.bump(.nodes_recomputed, 3);

    if (comptime enable_runtime_metrics) {
        try std.testing.expectEqual(@as(u64, 3), metrics.nodes_recomputed);
    } else {
        try std.testing.expectEqual(@as(u64, 0), metrics.nodes_recomputed);
    }
}

test "addRuntimeMetrics folds unsigned and signed counters" {
    var left = zeroRuntimeMetrics();
    var right = zeroRuntimeMetrics();
    left.nodes_recomputed = 3;
    right.nodes_recomputed = 4;
    left.host_retained_alloc_delta = -2;
    right.host_retained_alloc_delta = 5;

    const total = addRuntimeMetrics(left, right);

    try std.testing.expectEqual(@as(u64, 7), total.nodes_recomputed);
    try std.testing.expectEqual(@as(i64, 3), total.host_retained_alloc_delta);
}

test "NoMetrics is a zero-size no-op metrics sink" {
    try std.testing.expectEqual(@as(usize, 0), @sizeOf(NoMetrics));
    var metrics: NoMetrics = .{};
    metrics.bump(.closure_retains, 2);
    metrics.bump(.nodes_recomputed, 1);
}
