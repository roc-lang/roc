//! Host-facing render sink interface for the shared Signals engine.
//!
//! This is intentionally a thin generic adapter. Slice 4d starts by routing the
//! native simulated DOM through this sink without changing behavior; later
//! slices move render decisions into the engine while each host keeps its own
//! concrete sink implementation.

const render = @import("render_commands.zig");

pub const TextField = render.TextField;
pub const BoolField = render.BoolField;
pub const EventKind = render.EventKind;
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

        pub fn ensureNode(self: @This(), elem_id: u64, tag: []const u8, counts: *Counts) void {
            self.host.sinkEnsureNode(elem_id, tag, counts);
        }

        pub fn removeNode(self: @This(), elem_id: u64, counts: *Counts) void {
            self.host.sinkRemoveNode(elem_id, counts);
        }

        pub fn replaceChildren(self: @This(), parent_elem_id: u64, next_child_ids: []const u64, counts: *Counts) void {
            self.host.sinkReplaceChildren(parent_elem_id, next_child_ids, counts);
        }

        pub fn replaceChildrenForMoves(self: @This(), parent_elem_id: u64, next_child_ids: []const u64, counts: *Counts) void {
            self.host.sinkReplaceChildrenForMoves(parent_elem_id, next_child_ids, counts);
        }

        pub fn applyTextField(self: @This(), elem_id: u64, field: TextField, value: []const u8) void {
            self.host.sinkApplyTextField(elem_id, field, value);
        }

        pub fn applyBoolField(self: @This(), elem_id: u64, field: BoolField, value: bool) void {
            self.host.sinkApplyBoolField(elem_id, field, value);
        }

        pub fn clearTextField(self: @This(), elem_id: u64, field: TextField) void {
            self.host.sinkClearTextField(elem_id, field);
        }

        pub fn clearBoolField(self: @This(), elem_id: u64, field: BoolField) void {
            self.host.sinkClearBoolField(elem_id, field);
        }

        pub fn bindEvent(self: @This(), desc: anytype, event_id: u64) void {
            self.host.sinkBindEvent(desc, event_id);
        }
    };
}
