const std = @import("std");

const engine = @import("../engine.zig");
const render = @import("../render_commands.zig");
const spec_parser = @import("../spec/spec_parser.zig");

pub const Stats = struct {
    init_roc_ns: u64 = 0,
    init_apply_ns: u64 = 0,
    dispatch_roc_ns: u64 = 0,
    dispatch_apply_ns: u64 = 0,
    actions: u64 = 0,
    allocs: u64 = 0,
    deallocs: u64 = 0,
    retained_alloc_delta: i64 = 0,
    commands: render.Counts = .{},
    metrics: engine.RuntimeMetrics = engine.zeroRuntimeMetrics(),
};

pub fn nowNs() u64 {
    const ns = std.Io.Clock.awake.now(std.Io.Threaded.global_single_threaded.io()).nanoseconds;
    return @intCast(@max(ns, 0));
}

pub fn commandIsAction(cmd: spec_parser.SpecCommand) bool {
    return switch (cmd.cmd_type) {
        .click, .pointer_down, .pointer_up, .pointer_enter, .pointer_leave, .key_down, .submit, .fill, .check, .uncheck, .resolve_task, .reject_task, .tick_interval => true,
        else => false,
    };
}

fn writeStdout(bytes: []const u8) void {
    std.Io.File.stdout().writeStreamingAll(std.Io.Threaded.global_single_threaded.io(), bytes) catch {};
}

fn printStdout(comptime fmt: []const u8, args: anytype) void {
    var buf: [2048]u8 = undefined;
    const out = std.fmt.bufPrint(&buf, fmt, args) catch return;
    writeStdout(out);
}

pub fn printHeader() void {
    writeStdout("case,sample,iterations,actions,init_roc_ns,init_apply_ns,dispatch_roc_ns,dispatch_apply_ns,total_ns,allocs,deallocs,retained_alloc_delta,commands,reset_dom,create_element,append_child,remove_node,move_before,set_text,set_value,set_checked,set_disabled,set_metadata,bind_event,active_graph_records_rebuilt,stream_nodes_scanned,stream_nodes_scanned_apply,stream_nodes_scanned_children,stream_nodes_scanned_dirty_scope,stream_nodes_scanned_events,stream_nodes_scanned_mounts,stream_nodes_scanned_remove_target,stream_nodes_scanned_render_scope,stream_nodes_scanned_splice,signal_record_table_rebuilt,active_intervals_synced,render_indexes_refreshed,each_key_compares,each_key_hashes,each_key_reuse_compares,each_key_duplicate_compares,each_item_compares,each_syncs,each_sync_keys,each_sync_existing_rows,allocs_this_event,deallocs_this_event,host_allocs_this_event,host_deallocs_this_event,host_alloc_bytes_this_event,host_dealloc_bytes_this_event,events_processed,nodes_recomputed,propagation_prunes,derived_calls_into_roc,recompute_batches,patches_emitted,scopes_created,scopes_disposed,rows_reused,rows_created,rows_removed,closure_retains,closure_releases,metrics_retained_alloc_delta,host_retained_alloc_delta,host_retained_bytes_delta\n");
}

pub fn printRow(case_name: []const u8, sample: usize, iterations: usize, stats: Stats) void {
    const total_ns = stats.init_roc_ns + stats.init_apply_ns + stats.dispatch_roc_ns + stats.dispatch_apply_ns;
    printStdout(
        "{s},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},",
        .{
            case_name,
            sample,
            iterations,
            stats.actions,
            stats.init_roc_ns,
            stats.init_apply_ns,
            stats.dispatch_roc_ns,
            stats.dispatch_apply_ns,
            total_ns,
            stats.allocs,
            stats.deallocs,
            stats.retained_alloc_delta,
        },
    );
    printStdout(
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},",
        .{
            stats.commands.total,
            stats.commands.reset_dom,
            stats.commands.create_element,
            stats.commands.append_child,
            stats.commands.remove_node,
            stats.commands.move_before,
            stats.commands.set_text,
            stats.commands.set_value,
            stats.commands.set_checked,
            stats.commands.set_disabled,
            stats.commands.set_metadata,
            stats.commands.bind_event,
        },
    );
    printStdout(
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},",
        .{
            stats.metrics.active_graph_records_rebuilt,
            stats.metrics.stream_nodes_scanned,
            stats.metrics.stream_nodes_scanned_apply,
            stats.metrics.stream_nodes_scanned_children,
            stats.metrics.stream_nodes_scanned_dirty_scope,
            stats.metrics.stream_nodes_scanned_events,
            stats.metrics.stream_nodes_scanned_mounts,
            stats.metrics.stream_nodes_scanned_remove_target,
            stats.metrics.stream_nodes_scanned_render_scope,
            stats.metrics.stream_nodes_scanned_splice,
            stats.metrics.signal_record_table_rebuilt,
            stats.metrics.active_intervals_synced,
            stats.metrics.render_indexes_refreshed,
            stats.metrics.each_key_compares,
            stats.metrics.each_key_hashes,
            stats.metrics.each_key_reuse_compares,
            stats.metrics.each_key_duplicate_compares,
            stats.metrics.each_item_compares,
            stats.metrics.each_syncs,
            stats.metrics.each_sync_keys,
            stats.metrics.each_sync_existing_rows,
        },
    );
    printStdout(
        "{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d},{d}\n",
        .{
            stats.metrics.allocs_this_event,
            stats.metrics.deallocs_this_event,
            stats.metrics.host_allocs_this_event,
            stats.metrics.host_deallocs_this_event,
            stats.metrics.host_alloc_bytes_this_event,
            stats.metrics.host_dealloc_bytes_this_event,
            stats.metrics.events_processed,
            stats.metrics.nodes_recomputed,
            stats.metrics.propagation_prunes,
            stats.metrics.derived_calls_into_roc,
            stats.metrics.recompute_batches,
            stats.metrics.patches_emitted,
            stats.metrics.scopes_created,
            stats.metrics.scopes_disposed,
            stats.metrics.rows_reused,
            stats.metrics.rows_created,
            stats.metrics.rows_removed,
            stats.metrics.closure_retains,
            stats.metrics.closure_releases,
            stats.metrics.retained_alloc_delta,
            stats.metrics.host_retained_alloc_delta,
            stats.metrics.host_retained_bytes_delta,
        },
    );
}

test "commandIsAction recognizes only mutating commands" {
    try std.testing.expect(commandIsAction(.{
        .cmd_type = .click,
        .locator = .{ .kind = .none },
        .expected_text = null,
        .expected_count = null,
        .expected_bool = null,
        .line_num = 1,
    }));
    try std.testing.expect(!commandIsAction(.{
        .cmd_type = .expect_text,
        .locator = .{ .kind = .none },
        .expected_text = null,
        .expected_count = null,
        .expected_bool = null,
        .line_num = 2,
    }));
}
