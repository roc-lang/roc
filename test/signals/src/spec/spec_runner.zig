const std = @import("std");

const signals = @import("signals");
const engine = signals.engine;
const render = signals.render;
const spec_parser = @import("spec_parser.zig");

const EventPayloadAccessor = render.EventPayloadAccessor;
const EventPayloadKind = render.EventPayloadKind;
const RuntimeMetrics = engine.RuntimeMetrics;
const SpecCommand = spec_parser.SpecCommand;
const SpecCommandType = spec_parser.SpecCommandType;

pub fn Runner(comptime Ctx: type) type {
    return struct {
        const Host = Ctx.Host;
        const RocHost = Ctx.RocHost;

        pub fn run(host: *Host, roc_host: *RocHost, commands: []const SpecCommand, verbose: bool) c_int {
            var metrics_mark: ?RuntimeMetrics = null;

            for (commands) |cmd| {
                switch (cmd.cmd_type) {
                    .mark_metrics => {
                        metrics_mark = Ctx.lastRuntimeMetrics(host);
                    },

                    .click => {
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        if (elem.disabled) {
                            writeLocatorFailure(cmd.line_num, "target is disabled");
                            return 1;
                        }
                        const event_id = elem.bound_click_event orelse {
                            writeLocatorFailure(cmd.line_num, "target has no click binding");
                            return 1;
                        };
                        Ctx.dispatchRocEvent(host, roc_host, event_id, .unit, Ctx.hostValueUnit(host, roc_host));
                    },

                    .pointer_down, .pointer_up, .pointer_enter, .pointer_leave => {
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        if (elem.disabled) {
                            writeLocatorFailure(cmd.line_num, "target is disabled");
                            return 1;
                        }
                        const event_id = pointerEventIdForCommand(elem, cmd.cmd_type) orelse {
                            writeLocatorFailure(cmd.line_num, "target has no pointer binding");
                            return 1;
                        };
                        Ctx.dispatchRocEvent(host, roc_host, event_id, .unit, Ctx.hostValueUnit(host, roc_host));
                    },

                    .key_down => {
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        if (elem.disabled) {
                            writeLocatorFailure(cmd.line_num, "target is disabled");
                            return 1;
                        }
                        const event = Ctx.namedEvent(elem, "keydown") orelse {
                            writeLocatorFailure(cmd.line_num, "target has no keydown binding");
                            return 1;
                        };
                        if (event.payload_kind != .bytes or event.payload_accessor != .record_key_shift) {
                            writeLocatorFailure(cmd.line_num, "keydown binding does not request the key/shift payload descriptor");
                            return 1;
                        }
                        const key = cmd.expected_text orelse {
                            writeLocatorFailure(cmd.line_num, "key_down command is missing key text");
                            return 1;
                        };
                        const shift_key = cmd.expected_bool orelse {
                            writeLocatorFailure(cmd.line_num, "key_down command is missing shift flag");
                            return 1;
                        };
                        const payload_bytes = encodeKeyShiftPayload(Ctx.allocator(host), key, shift_key);
                        defer Ctx.allocator(host).free(payload_bytes);
                        Ctx.dispatchRocEvent(host, roc_host, event.event_id, .bytes, Ctx.hostValueU8List(host, roc_host, payload_bytes));
                    },

                    .submit => {
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        if (elem.disabled) {
                            writeLocatorFailure(cmd.line_num, "target is disabled");
                            return 1;
                        }
                        const event = Ctx.namedEvent(elem, "submit") orelse {
                            writeLocatorFailure(cmd.line_num, "target has no submit binding");
                            return 1;
                        };
                        if (event.payload_kind != .unit or event.payload_accessor != .none) {
                            writeLocatorFailure(cmd.line_num, "submit binding does not use a unit payload descriptor");
                            return 1;
                        }
                        if ((event.options & render.listener_option_prevent_default) == 0) {
                            writeLocatorFailure(cmd.line_num, "submit binding is missing the static prevent-default listener policy");
                            return 1;
                        }
                        Ctx.dispatchRocEvent(host, roc_host, event.event_id, .unit, Ctx.hostValueUnit(host, roc_host));
                    },

                    .fill => {
                        const value = cmd.expected_text orelse "";
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        if (elem.disabled) {
                            writeLocatorFailure(cmd.line_num, "target is disabled");
                            return 1;
                        }
                        if (elem.bound_input_event) |event_id| {
                            Ctx.dispatchRocEvent(host, roc_host, event_id, .str, Ctx.hostValueStr(host, roc_host, value));
                        } else {
                            _ = Ctx.setElementValueIfChanged(host, elem, value);
                        }
                    },

                    .check, .uncheck => {
                        const checked = cmd.cmd_type == .check;
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        if (elem.disabled) {
                            writeLocatorFailure(cmd.line_num, "target is disabled");
                            return 1;
                        }
                        if (elem.bound_check_event) |event_id| {
                            Ctx.dispatchRocEvent(host, roc_host, event_id, .bool, Ctx.hostValueBool(host, roc_host, checked));
                        } else {
                            _ = Ctx.setElementCheckedIfChanged(elem, checked);
                        }
                    },

                    .resolve_task, .reject_task => {
                        const task_name = cmd.task_name orelse {
                            writeLocatorFailure(cmd.line_num, "task command had no task name");
                            return 1;
                        };
                        const payload = cmd.expected_text orelse "";
                        _ = Ctx.resolvePendingTask(host, roc_host, task_name, payload, cmd.cmd_type == .reject_task);
                        Ctx.finishHostMetrics(host);
                    },

                    .tick_interval => {
                        const period_ms = cmd.interval_ms orelse {
                            writeLocatorFailure(cmd.line_num, "interval command had no period");
                            return 1;
                        };
                        _ = Ctx.tickIntervalSource(host, roc_host, period_ms);
                        Ctx.finishHostMetrics(host);
                    },

                    .expect_visible => {
                        _ = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one visible element");
                            return 1;
                        };
                    },

                    .expect_absent => {
                        const match_count = Ctx.countElementsByLocator(host, cmd.locator);
                        if (match_count != 0) {
                            writeAbsentFailure(cmd.line_num, match_count);
                            return 1;
                        }
                    },

                    .expect_text => {
                        const expected = cmd.expected_text orelse "";
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        const actual = elem.text orelse "";
                        if (!std.mem.eql(u8, actual, expected)) {
                            writeStringMismatch(cmd.line_num, "text", expected, actual);
                            return 1;
                        }
                    },

                    .expect_value => {
                        const expected = cmd.expected_text orelse "";
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        const actual = elem.value orelse "";
                        if (!std.mem.eql(u8, actual, expected)) {
                            writeStringMismatch(cmd.line_num, "value", expected, actual);
                            return 1;
                        }
                    },

                    .expect_attr => {
                        const attr_name = cmd.expected_attr orelse {
                            writeLocatorFailure(cmd.line_num, "attr assertion had no attr name");
                            return 1;
                        };
                        const expected = cmd.expected_text orelse "";
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        const actual = Ctx.elementTextAttr(elem, attr_name) orelse "";
                        if (!std.mem.eql(u8, actual, expected)) {
                            writeStringMismatch(cmd.line_num, attr_name, expected, actual);
                            return 1;
                        }
                    },

                    .expect_checked => {
                        const expected = cmd.expected_bool orelse false;
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        if (elem.checked != expected) {
                            writeBoolMismatch(cmd.line_num, "checked", expected, elem.checked);
                            return 1;
                        }
                    },

                    .expect_disabled => {
                        const expected = cmd.expected_bool orelse false;
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        if (elem.disabled != expected) {
                            writeBoolMismatch(cmd.line_num, "disabled", expected, elem.disabled);
                            return 1;
                        }
                    },

                    .expect_updates => {
                        const expected = cmd.expected_count orelse 0;
                        const elem = Ctx.findElementByLocator(host, cmd.locator, cmd.line_num) orelse {
                            writeLocatorFailure(cmd.line_num, "locator did not resolve to one element");
                            return 1;
                        };
                        const actual = elem.text_update_count + elem.value_update_count + elem.checked_update_count + elem.disabled_update_count;
                        if (actual != expected) {
                            var buf: [512]u8 = undefined;
                            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected updates: {d}\n  Got updates:      {d}\n", .{ cmd.line_num, expected, actual }) catch "TEST FAILED\n";
                            Ctx.writeStderr(msg);
                            return 1;
                        }
                    },

                    .expect_cleanup => {
                        const name = cmd.task_name orelse "";
                        const expected = cmd.expected_count orelse 0;
                        const actual = Ctx.cleanupEventCount(host, name);
                        if (actual != expected) {
                            var buf: [512]u8 = undefined;
                            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected cleanup \"{s}\": {d}\n  Got cleanup count:       {d}\n", .{ cmd.line_num, name, expected, actual }) catch "TEST FAILED\n";
                            Ctx.writeStderr(msg);
                            return 1;
                        }
                    },

                    .expect_pending_task => {
                        const name = cmd.task_name orelse "";
                        const expected = cmd.expected_count orelse 0;
                        const actual = Ctx.pendingTaskCountByName(host, name);
                        if (actual != expected) {
                            var buf: [512]u8 = undefined;
                            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected pending task \"{s}\": {d}\n  Got pending task count:       {d}\n", .{ cmd.line_num, name, expected, actual }) catch "TEST FAILED\n";
                            Ctx.writeStderr(msg);
                            return 1;
                        }
                    },

                    .expect_interval => {
                        const period_ms = cmd.interval_ms orelse 0;
                        const expected = cmd.expected_count orelse 0;
                        const actual = Ctx.activeIntervalRecordCountByPeriod(host, period_ms);
                        if (actual != expected) {
                            var buf: [512]u8 = undefined;
                            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected active interval {d}ms: {d}\n  Got active interval count:   {d}\n", .{ cmd.line_num, period_ms, expected, actual }) catch "TEST FAILED\n";
                            Ctx.writeStderr(msg);
                            return 1;
                        }
                    },

                    .expect_metric_delta => {
                        const metric_name = cmd.expected_text orelse "";
                        const expected = cmd.expected_metric_delta orelse 0;
                        const marked = metrics_mark orelse {
                            writeMetricFailure(cmd.line_num, "mark_metrics must run before expect_metric_delta");
                            return 1;
                        };
                        const start = runtimeMetricValue(marked, metric_name) orelse {
                            writeUnknownMetric(cmd.line_num, metric_name);
                            return 1;
                        };
                        const current = runtimeMetricValue(Ctx.lastRuntimeMetrics(host), metric_name) orelse {
                            writeUnknownMetric(cmd.line_num, metric_name);
                            return 1;
                        };
                        const actual = current - start;
                        if (actual != expected) {
                            writeMetricDeltaMismatch(cmd.line_num, metric_name, expected, actual);
                            return 1;
                        }
                    },

                    .expect_metric_delta_at_most => {
                        const metric_name = cmd.expected_text orelse "";
                        const expected = cmd.expected_metric_delta orelse 0;
                        const marked = metrics_mark orelse {
                            writeMetricFailure(cmd.line_num, "mark_metrics must run before expect_metric_delta_at_most");
                            return 1;
                        };
                        const start = runtimeMetricValue(marked, metric_name) orelse {
                            writeUnknownMetric(cmd.line_num, metric_name);
                            return 1;
                        };
                        const current = runtimeMetricValue(Ctx.lastRuntimeMetrics(host), metric_name) orelse {
                            writeUnknownMetric(cmd.line_num, metric_name);
                            return 1;
                        };
                        const actual = current - start;
                        if (actual > expected) {
                            writeMetricDeltaExceeded(cmd.line_num, metric_name, expected, actual);
                            return 1;
                        }
                    },
                }
            }

            if (verbose) {
                Ctx.writeStderr("[PASS] All tests passed\n");
            }

            return 0;
        }

        fn u64MetricAsI64(value: u64) i64 {
            return std.math.cast(i64, value) orelse Ctx.fail("runtime metric exceeded signed assertion range");
        }

        fn runtimeMetricValue(metrics: RuntimeMetrics, name: []const u8) ?i64 {
            if (std.mem.eql(u8, name, "active_graph_records_rebuilt")) return u64MetricAsI64(metrics.active_graph_records_rebuilt);
            if (std.mem.eql(u8, name, "active_intervals_synced")) return u64MetricAsI64(metrics.active_intervals_synced);
            if (std.mem.eql(u8, name, "reset_dom")) return u64MetricAsI64(metrics.reset_dom);
            if (std.mem.eql(u8, name, "create_element")) return u64MetricAsI64(metrics.create_element);
            if (std.mem.eql(u8, name, "append_child")) return u64MetricAsI64(metrics.append_child);
            if (std.mem.eql(u8, name, "remove_node")) return u64MetricAsI64(metrics.remove_node);
            if (std.mem.eql(u8, name, "move_before")) return u64MetricAsI64(metrics.move_before);
            if (std.mem.eql(u8, name, "set_text")) return u64MetricAsI64(metrics.set_text);
            if (std.mem.eql(u8, name, "set_value")) return u64MetricAsI64(metrics.set_value);
            if (std.mem.eql(u8, name, "set_checked")) return u64MetricAsI64(metrics.set_checked);
            if (std.mem.eql(u8, name, "set_disabled")) return u64MetricAsI64(metrics.set_disabled);
            if (std.mem.eql(u8, name, "set_metadata")) return u64MetricAsI64(metrics.set_metadata);
            if (std.mem.eql(u8, name, "bind_event")) return u64MetricAsI64(metrics.bind_event);
            if (std.mem.eql(u8, name, "allocs_this_event")) return u64MetricAsI64(metrics.allocs_this_event);
            if (std.mem.eql(u8, name, "deallocs_this_event")) return u64MetricAsI64(metrics.deallocs_this_event);
            if (std.mem.eql(u8, name, "host_allocs_this_event")) return u64MetricAsI64(metrics.host_allocs_this_event);
            if (std.mem.eql(u8, name, "host_deallocs_this_event")) return u64MetricAsI64(metrics.host_deallocs_this_event);
            if (std.mem.eql(u8, name, "host_alloc_bytes_this_event")) return u64MetricAsI64(metrics.host_alloc_bytes_this_event);
            if (std.mem.eql(u8, name, "host_dealloc_bytes_this_event")) return u64MetricAsI64(metrics.host_dealloc_bytes_this_event);
            if (std.mem.eql(u8, name, "events_processed")) return u64MetricAsI64(metrics.events_processed);
            if (std.mem.eql(u8, name, "nodes_recomputed")) return u64MetricAsI64(metrics.nodes_recomputed);
            if (std.mem.eql(u8, name, "propagation_prunes")) return u64MetricAsI64(metrics.propagation_prunes);
            if (std.mem.eql(u8, name, "derived_calls_into_roc")) return u64MetricAsI64(metrics.derived_calls_into_roc);
            if (std.mem.eql(u8, name, "each_key_compares")) return u64MetricAsI64(metrics.each_key_compares);
            if (std.mem.eql(u8, name, "each_key_hashes")) return u64MetricAsI64(metrics.each_key_hashes);
            if (std.mem.eql(u8, name, "each_key_reuse_compares")) return u64MetricAsI64(metrics.each_key_reuse_compares);
            if (std.mem.eql(u8, name, "each_key_duplicate_compares")) return u64MetricAsI64(metrics.each_key_duplicate_compares);
            if (std.mem.eql(u8, name, "each_item_compares")) return u64MetricAsI64(metrics.each_item_compares);
            if (std.mem.eql(u8, name, "each_syncs")) return u64MetricAsI64(metrics.each_syncs);
            if (std.mem.eql(u8, name, "each_sync_keys")) return u64MetricAsI64(metrics.each_sync_keys);
            if (std.mem.eql(u8, name, "each_sync_existing_rows")) return u64MetricAsI64(metrics.each_sync_existing_rows);
            if (std.mem.eql(u8, name, "recompute_batches")) return u64MetricAsI64(metrics.recompute_batches);
            if (std.mem.eql(u8, name, "patches_emitted")) return u64MetricAsI64(metrics.patches_emitted);
            if (std.mem.eql(u8, name, "scopes_created")) return u64MetricAsI64(metrics.scopes_created);
            if (std.mem.eql(u8, name, "scopes_disposed")) return u64MetricAsI64(metrics.scopes_disposed);
            if (std.mem.eql(u8, name, "rows_reused")) return u64MetricAsI64(metrics.rows_reused);
            if (std.mem.eql(u8, name, "rows_created")) return u64MetricAsI64(metrics.rows_created);
            if (std.mem.eql(u8, name, "rows_removed")) return u64MetricAsI64(metrics.rows_removed);
            if (std.mem.eql(u8, name, "closure_retains")) return u64MetricAsI64(metrics.closure_retains);
            if (std.mem.eql(u8, name, "closure_releases")) return u64MetricAsI64(metrics.closure_releases);
            if (std.mem.eql(u8, name, "render_indexes_refreshed")) return u64MetricAsI64(metrics.render_indexes_refreshed);
            if (std.mem.eql(u8, name, "signal_record_table_rebuilt")) return u64MetricAsI64(metrics.signal_record_table_rebuilt);
            if (std.mem.eql(u8, name, "stream_nodes_scanned")) return u64MetricAsI64(metrics.stream_nodes_scanned);
            if (std.mem.eql(u8, name, "stream_nodes_scanned_apply")) return u64MetricAsI64(metrics.stream_nodes_scanned_apply);
            if (std.mem.eql(u8, name, "stream_nodes_scanned_children")) return u64MetricAsI64(metrics.stream_nodes_scanned_children);
            if (std.mem.eql(u8, name, "stream_nodes_scanned_dirty_scope")) return u64MetricAsI64(metrics.stream_nodes_scanned_dirty_scope);
            if (std.mem.eql(u8, name, "stream_nodes_scanned_events")) return u64MetricAsI64(metrics.stream_nodes_scanned_events);
            if (std.mem.eql(u8, name, "stream_nodes_scanned_mounts")) return u64MetricAsI64(metrics.stream_nodes_scanned_mounts);
            if (std.mem.eql(u8, name, "stream_nodes_scanned_remove_target")) return u64MetricAsI64(metrics.stream_nodes_scanned_remove_target);
            if (std.mem.eql(u8, name, "stream_nodes_scanned_render_scope")) return u64MetricAsI64(metrics.stream_nodes_scanned_render_scope);
            if (std.mem.eql(u8, name, "stream_nodes_scanned_splice")) return u64MetricAsI64(metrics.stream_nodes_scanned_splice);
            if (std.mem.eql(u8, name, "retained_alloc_delta")) return metrics.retained_alloc_delta;
            if (std.mem.eql(u8, name, "host_retained_alloc_delta")) return metrics.host_retained_alloc_delta;
            if (std.mem.eql(u8, name, "host_retained_bytes_delta")) return metrics.host_retained_bytes_delta;
            return null;
        }

        fn encodeKeyShiftPayload(allocator: std.mem.Allocator, key: []const u8, shift_key: bool) []u8 {
            const bytes = allocator.alloc(u8, @sizeOf(u32) + key.len + 1) catch std.process.exit(1);
            std.mem.writeInt(u32, bytes[0..@sizeOf(u32)], @intCast(key.len), .little);
            @memcpy(bytes[@sizeOf(u32)..][0..key.len], key);
            bytes[@sizeOf(u32) + key.len] = if (shift_key) 1 else 0;
            return bytes;
        }

        fn pointerEventIdForCommand(elem: anytype, cmd_type: SpecCommandType) ?u64 {
            return switch (cmd_type) {
                .pointer_down => elem.bound_pointer_down_event,
                .pointer_up => elem.bound_pointer_up_event,
                .pointer_enter => elem.bound_pointer_enter_event,
                .pointer_leave => elem.bound_pointer_leave_event,
                else => null,
            };
        }

        fn writeLocatorFailure(line_num: usize, message: []const u8) void {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: {s}\n", .{ line_num, message }) catch "TEST FAILED\n";
            Ctx.writeStderr(msg);
        }

        fn writeAbsentFailure(line_num: usize, match_count: usize) void {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: expected no matching elements, found {d}\n", .{ line_num, match_count }) catch "TEST FAILED\n";
            Ctx.writeStderr(msg);
        }

        fn writeStringMismatch(line_num: usize, field: []const u8, expected: []const u8, actual: []const u8) void {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected {s}: \"{s}\"\n  Got {s}:      \"{s}\"\n", .{ line_num, field, expected, field, actual }) catch "TEST FAILED\n";
            Ctx.writeStderr(msg);
        }

        fn writeBoolMismatch(line_num: usize, field: []const u8, expected: bool, actual: bool) void {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected {s}: {}\n  Got {s}:      {}\n", .{ line_num, field, expected, field, actual }) catch "TEST FAILED\n";
            Ctx.writeStderr(msg);
        }

        fn writeMetricFailure(line_num: usize, message: []const u8) void {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: {s}\n", .{ line_num, message }) catch "TEST FAILED\n";
            Ctx.writeStderr(msg);
        }

        fn writeUnknownMetric(line_num: usize, metric_name: []const u8) void {
            var buf: [256]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}: unknown metric \"{s}\"\n", .{ line_num, metric_name }) catch "TEST FAILED\n";
            Ctx.writeStderr(msg);
        }

        fn writeMetricDeltaMismatch(line_num: usize, metric_name: []const u8, expected: i64, actual: i64) void {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected {s} delta: {d}\n  Got {s} delta:      {d}\n", .{ line_num, metric_name, expected, metric_name, actual }) catch "TEST FAILED\n";
            Ctx.writeStderr(msg);
        }

        fn writeMetricDeltaExceeded(line_num: usize, metric_name: []const u8, expected: i64, actual: i64) void {
            var buf: [512]u8 = undefined;
            const msg = std.fmt.bufPrint(&buf, "TEST FAILED at line {d}:\n  Expected {s} delta at most: {d}\n  Got {s} delta:             {d}\n", .{ line_num, metric_name, expected, metric_name, actual }) catch "TEST FAILED\n";
            Ctx.writeStderr(msg);
        }
    };
}

test "spec runner resolves runtime metric names" {
    const TestCtx = struct {
        pub const Host = void;
        pub const RocHost = void;

        pub fn fail(_: []const u8) noreturn {
            unreachable;
        }

        pub fn writeStderr(_: []const u8) void {}
    };
    const TestRunner = Runner(TestCtx);
    var metrics = engine.zeroRuntimeMetrics();
    metrics.rows_reused = 7;
    metrics.retained_alloc_delta = -2;

    try std.testing.expectEqual(@as(?i64, 7), TestRunner.runtimeMetricValue(metrics, "rows_reused"));
    try std.testing.expectEqual(@as(?i64, -2), TestRunner.runtimeMetricValue(metrics, "retained_alloc_delta"));
    try std.testing.expectEqual(@as(?i64, null), TestRunner.runtimeMetricValue(metrics, "missing_metric"));
}
