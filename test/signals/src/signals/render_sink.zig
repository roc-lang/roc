//! Host-facing render sink interface for the shared Signals engine.
//!
//! This is intentionally a thin generic adapter. Slice 4d starts by routing the
//! native simulated DOM through this sink without changing behavior; later
//! slices move render decisions into the engine while each host keeps its own
//! concrete sink implementation.

const std = @import("std");
const render = @import("render_commands.zig");

pub const TextField = render.TextField;
pub const BoolField = render.BoolField;
pub const EventKind = render.EventKind;
pub const EventPayloadKind = render.EventPayloadKind;
pub const EventPayloadAccessor = render.EventPayloadAccessor;
pub const Counts = render.Counts;

pub fn DomSink(comptime Host: type) type {
    return struct {
        host: *Host,

        pub fn reset(self: @This()) void {
            self.host.sinkReset();
        }

        pub fn appendNode(self: @This(), elem_id: u64, parent_elem_id: u64, tag: []const u8) void {
            self.host.sinkAppendNode(elem_id, parent_elem_id, tag);
        }

        pub fn ensureNode(self: @This(), elem_id: u64, tag: []const u8) void {
            self.host.sinkEnsureNode(elem_id, tag);
        }

        pub fn removeNode(self: @This(), elem_id: u64) void {
            self.host.sinkRemoveNode(elem_id);
        }

        pub fn replaceChildren(self: @This(), parent_elem_id: u64, next_child_ids: []const u64) void {
            self.host.sinkReplaceChildren(parent_elem_id, next_child_ids);
        }

        pub fn replaceChildrenForMoves(self: @This(), parent_elem_id: u64, next_child_ids: []const u64) void {
            self.host.sinkReplaceChildrenForMoves(parent_elem_id, next_child_ids);
        }

        pub fn applyTextField(self: @This(), elem_id: u64, field: TextField, value: []const u8) void {
            self.host.sinkApplyTextField(elem_id, field, value);
        }

        pub fn applyTextAttr(self: @This(), elem_id: u64, name: []const u8, value: []const u8) void {
            self.host.sinkApplyTextAttr(elem_id, name, value);
        }

        pub fn applyBoolField(self: @This(), elem_id: u64, field: BoolField, value: bool) void {
            self.host.sinkApplyBoolField(elem_id, field, value);
        }

        pub fn clearTextField(self: @This(), elem_id: u64, field: TextField) void {
            self.host.sinkClearTextField(elem_id, field);
        }

        pub fn clearTextAttr(self: @This(), elem_id: u64, name: []const u8) void {
            self.host.sinkClearTextAttr(elem_id, name);
        }

        pub fn clearBoolField(self: @This(), elem_id: u64, field: BoolField) void {
            self.host.sinkClearBoolField(elem_id, field);
        }

        pub fn bindEventKind(self: @This(), elem_id: u64, kind: EventKind, event_id: u64, payload_accessor: EventPayloadAccessor) void {
            self.host.sinkBindEventKind(elem_id, kind, event_id, payload_accessor);
        }

        pub fn clearEvent(self: @This(), elem_id: u64, kind: EventKind) void {
            self.host.sinkClearEvent(elem_id, kind);
        }

        pub fn bindEventName(self: @This(), elem_id: u64, name: []const u8, event_id: u64, options: u32, payload_kind: EventPayloadKind, payload_accessor: EventPayloadAccessor) void {
            self.host.sinkBindEventName(elem_id, name, event_id, options, payload_kind, payload_accessor);
        }

        pub fn clearEventName(self: @This(), elem_id: u64, name: []const u8) void {
            self.host.sinkClearEventName(elem_id, name);
        }

        pub fn startInterval(self: @This(), token: u64, period_ms: u64) void {
            self.host.sinkStartInterval(token, period_ms);
        }

        pub fn cancelInterval(self: @This(), token: u64) void {
            self.host.sinkCancelInterval(token);
        }

        pub fn startTask(self: @This(), request_id: u64, task_name: []const u8, request: []const u8) void {
            self.host.sinkStartTask(request_id, task_name, request);
        }

        pub fn cancelTask(self: @This(), request_id: u64) void {
            self.host.sinkCancelTask(request_id);
        }

        pub fn debugAssertNode(self: @This(), elem_id: u64, active: bool, tag: ?[]const u8, parent_id: ?u64, children: []const u64, click_event: ?u64, input_event: ?u64, check_event: ?u64, pointer_down_event: ?u64, pointer_up_event: ?u64, pointer_enter_event: ?u64, pointer_leave_event: ?u64) void {
            self.host.sinkDebugAssertNode(elem_id, active, tag, parent_id, children, click_event, input_event, check_event, pointer_down_event, pointer_up_event, pointer_enter_event, pointer_leave_event);
        }
    };
}

test "DomSink forwards every render seam method to the host" {
    const TestHost = struct {
        seen: u32 = 0,
        last_event_accessor: EventPayloadAccessor = .none,
        last_task_name: []const u8 = "",
        last_task_request: []const u8 = "",
        last_children_len: usize = 0,
        last_debug_children_len: usize = 0,

        fn mark(self: *@This(), bit: u5) void {
            self.seen |= @as(u32, 1) << bit;
        }

        pub fn sinkReset(self: *@This()) void {
            self.mark(0);
        }

        pub fn sinkAppendNode(self: *@This(), _: u64, _: u64, _: []const u8) void {
            self.mark(1);
        }

        pub fn sinkEnsureNode(self: *@This(), _: u64, _: []const u8) void {
            self.mark(2);
        }

        pub fn sinkRemoveNode(self: *@This(), _: u64) void {
            self.mark(3);
        }

        pub fn sinkReplaceChildren(self: *@This(), _: u64, children: []const u64) void {
            self.mark(4);
            self.last_children_len = children.len;
        }

        pub fn sinkReplaceChildrenForMoves(self: *@This(), _: u64, _: []const u64) void {
            self.mark(5);
        }

        pub fn sinkApplyTextField(self: *@This(), _: u64, _: TextField, _: []const u8) void {
            self.mark(6);
        }

        pub fn sinkApplyTextAttr(self: *@This(), _: u64, _: []const u8, _: []const u8) void {
            self.mark(17);
        }

        pub fn sinkApplyBoolField(self: *@This(), _: u64, _: BoolField, _: bool) void {
            self.mark(7);
        }

        pub fn sinkClearTextField(self: *@This(), _: u64, _: TextField) void {
            self.mark(8);
        }

        pub fn sinkClearTextAttr(self: *@This(), _: u64, _: []const u8) void {
            self.mark(18);
        }

        pub fn sinkClearBoolField(self: *@This(), _: u64, _: BoolField) void {
            self.mark(9);
        }

        pub fn sinkBindEventKind(self: *@This(), _: u64, _: EventKind, _: u64, payload_accessor: EventPayloadAccessor) void {
            self.mark(10);
            self.last_event_accessor = payload_accessor;
        }

        pub fn sinkClearEvent(self: *@This(), _: u64, _: EventKind) void {
            self.mark(11);
        }

        pub fn sinkBindEventName(self: *@This(), _: u64, _: []const u8, _: u64, _: u32, _: EventPayloadKind, payload_accessor: EventPayloadAccessor) void {
            self.mark(19);
            self.last_event_accessor = payload_accessor;
        }

        pub fn sinkClearEventName(self: *@This(), _: u64, _: []const u8) void {
            self.mark(20);
        }

        pub fn sinkStartInterval(self: *@This(), _: u64, _: u64) void {
            self.mark(12);
        }

        pub fn sinkCancelInterval(self: *@This(), _: u64) void {
            self.mark(13);
        }

        pub fn sinkStartTask(self: *@This(), _: u64, task_name: []const u8, request: []const u8) void {
            self.mark(14);
            self.last_task_name = task_name;
            self.last_task_request = request;
        }

        pub fn sinkCancelTask(self: *@This(), _: u64) void {
            self.mark(15);
        }

        pub fn sinkDebugAssertNode(self: *@This(), _: u64, _: bool, _: ?[]const u8, _: ?u64, children: []const u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64, _: ?u64) void {
            self.mark(16);
            self.last_debug_children_len = children.len;
        }
    };

    var host: TestHost = .{};
    const sink: DomSink(TestHost) = .{ .host = &host };
    const children = [_]u64{ 3, 4 };

    sink.reset();
    sink.appendNode(1, 0, "div");
    sink.ensureNode(1, "div");
    sink.removeNode(9);
    sink.replaceChildren(1, &children);
    sink.replaceChildrenForMoves(1, &children);
    sink.applyTextField(1, .text, "hello");
    sink.applyTextAttr(1, "data-state", "ready");
    sink.applyBoolField(1, .disabled, true);
    sink.clearTextField(1, .label);
    sink.clearTextAttr(1, "data-state");
    sink.clearBoolField(1, .checked);
    sink.bindEventKind(1, .input, 7, .target_value);
    sink.clearEvent(1, .input);
    sink.bindEventName(1, "keydown", 8, 0, .bytes, .record_key_shift);
    sink.clearEventName(1, "keydown");
    sink.startInterval(8, 1000);
    sink.cancelInterval(8);
    sink.startTask(9, "lookup", "roc");
    sink.cancelTask(9);
    sink.debugAssertNode(1, true, "div", 0, &children, 7, null, null, null, null, null, null);

    try std.testing.expectEqual((@as(u32, 1) << 21) - 1, host.seen);
    try std.testing.expectEqual(@as(usize, 2), host.last_children_len);
    try std.testing.expectEqual(@as(usize, 2), host.last_debug_children_len);
    try std.testing.expectEqual(EventPayloadAccessor.record_key_shift, host.last_event_accessor);
    try std.testing.expectEqualStrings("lookup", host.last_task_name);
    try std.testing.expectEqualStrings("roc", host.last_task_request);
}
