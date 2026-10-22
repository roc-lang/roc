//! Typed continuation stacks for a labeled-switch kernel.
//!
//! A kernel pushes a label together with that label's payload, later pops the
//! label to dispatch on it, and takes the payload back from the stack that
//! matches the label. Each label has its own payload stack, so pushing and
//! popping never copies a payload larger than the one in hand. A label whose
//! payload type is `void` has no payload stack.
const std = @import("std");
const Allocator = std.mem.Allocator;

/// Continuation stacks keyed by `Label`, with `payloadOf(label)` naming the
/// payload type each label carries.
pub fn WorkStacks(comptime Label: type, comptime payloadOf: fn (comptime Label) type) type {
    const labels = std.enums.values(Label);
    const Payloads = comptime payloads: {
        var names: [labels.len][:0]const u8 = undefined;
        var types: [labels.len]type = undefined;
        var attrs: [labels.len]std.builtin.Type.Struct.FieldAttributes = undefined;
        var count: usize = 0;
        for (labels) |label| {
            const Payload = payloadOf(label);
            if (Payload == void) continue;
            const Stack = std.ArrayList(Payload);
            names[count] = @tagName(label);
            types[count] = Stack;
            attrs[count] = .{ .default_value_ptr = @ptrCast(&@as(Stack, .empty)) };
            count += 1;
        }
        const final_names = names;
        const final_types = types;
        const final_attrs = attrs;
        break :payloads @Struct(.auto, null, final_names[0..count], final_types[0..count], final_attrs[0..count]);
    };

    return struct {
        const Self = @This();

        labels: std.ArrayList(Label) = .empty,
        payloads: Payloads = .{},

        fn stack(self: *Self, comptime label: Label) *std.ArrayList(payloadOf(label)) {
            return &@field(self.payloads, @tagName(label));
        }

        /// Push `label` with its payload. On allocation failure neither is pushed.
        pub fn push(self: *Self, comptime label: Label, allocator: Allocator, item: payloadOf(label)) Allocator.Error!void {
            if (payloadOf(label) != void) {
                try self.stack(label).append(allocator, item);
                errdefer _ = self.stack(label).pop();
                try self.labels.append(allocator, label);
            } else {
                try self.labels.append(allocator, label);
            }
        }

        /// Pop the most recently pushed label, or null when no work remains.
        pub fn popLabel(self: *Self) ?Label {
            return self.labels.pop();
        }

        /// Take the payload of a label returned by `popLabel`.
        pub fn take(self: *Self, comptime label: Label) payloadOf(label) {
            if (payloadOf(label) == void) return;
            return self.stack(label).pop() orelse unreachable; // push appends a label and its payload together, and the caller popped this label
        }

        /// Drop the payload of a label returned by `popLabel`.
        pub fn discard(self: *Self, label: Label) void {
            inline for (comptime std.enums.values(Label)) |l| {
                if (label == l) {
                    _ = self.take(l);
                    return;
                }
            }
            unreachable; // the loop covers every Label value
        }

        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.labels.deinit(allocator);
            inline for (@typeInfo(Payloads).@"struct".field_names) |name| {
                @field(self.payloads, name).deinit(allocator);
            }
        }
    };
}

test "WorkStacks pushes, pops, takes and discards per-label payloads" {
    const Label = enum { dispatch, leaf, pair };
    const Pair = struct { a: u32, b: u32 };
    const payloadOf = struct {
        fn f(comptime label: Label) type {
            return switch (label) {
                .dispatch => void,
                .leaf => u8,
                .pair => Pair,
            };
        }
    }.f;
    const allocator = std.testing.allocator;

    var stacks: WorkStacks(Label, payloadOf) = .{};
    defer stacks.deinit(allocator);

    try stacks.push(.leaf, allocator, 7);
    try stacks.push(.pair, allocator, .{ .a = 1, .b = 2 });
    try stacks.push(.leaf, allocator, 9);

    try std.testing.expectEqual(Label.leaf, stacks.popLabel().?);
    try std.testing.expectEqual(@as(u8, 9), stacks.take(.leaf));
    try std.testing.expectEqual(Label.pair, stacks.popLabel().?);
    stacks.discard(.pair);
    try std.testing.expectEqual(Label.leaf, stacks.popLabel().?);
    try std.testing.expectEqual(@as(u8, 7), stacks.take(.leaf));
    try std.testing.expectEqual(@as(?Label, null), stacks.popLabel());
}
