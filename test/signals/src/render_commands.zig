const std = @import("std");

pub const Op = enum(u32) {
    reset_dom = 1,
    create_element = 2,
    create_text = 3,
    append_child = 4,
    remove_node = 5,
    move_before = 6,
    set_text = 7,
    set_value = 8,
    set_checked = 9,
    set_disabled = 10,
    set_role = 11,
    set_label = 12,
    set_test_id = 13,
    bind_click = 14,
    bind_input = 15,
    bind_check = 16,
};

pub const Record = extern struct {
    op: u32,
    a: u32 = 0,
    b: u32 = 0,
    c: u32 = 0,
    d: u32 = 0,
    e: u32 = 0,

    pub const word_count = @divExact(@sizeOf(Record), @sizeOf(u32));

    pub fn init(op: Op, a: u32, b: u32, c: u32, d: u32, e: u32) Record {
        return .{
            .op = @intFromEnum(op),
            .a = a,
            .b = b,
            .c = c,
            .d = d,
            .e = e,
        };
    }
};

pub const Buffer = struct {
    records: std.ArrayListUnmanaged(Record) = .empty,

    pub fn deinit(self: *Buffer, allocator: std.mem.Allocator) void {
        self.records.deinit(allocator);
        self.* = .{};
    }

    pub fn clearRetainingCapacity(self: *Buffer) void {
        self.records.clearRetainingCapacity();
    }

    pub fn len(self: *const Buffer) usize {
        return self.records.items.len;
    }

    pub fn ptrAddress(self: *const Buffer) usize {
        if (self.records.items.len == 0) return 0;
        return @intFromPtr(self.records.items.ptr);
    }

    pub fn append(self: *Buffer, allocator: std.mem.Allocator, op: Op, a: u32, b: u32, c: u32, d: u32, e: u32) !void {
        try self.records.append(allocator, Record.init(op, a, b, c, d, e));
    }
};

pub const TextField = enum(u64) {
    text = 1,
    role = 2,
    label = 3,
    test_id = 4,
    value = 5,

    pub fn setOp(self: TextField) Op {
        return switch (self) {
            .text => .set_text,
            .role => .set_role,
            .label => .set_label,
            .test_id => .set_test_id,
            .value => .set_value,
        };
    }
};

pub const BoolField = enum(u64) {
    checked = 1,
    disabled = 2,

    pub fn setOp(self: BoolField) Op {
        return switch (self) {
            .checked => .set_checked,
            .disabled => .set_disabled,
        };
    }
};

pub const EventKind = enum(u64) {
    click = 1,
    input = 2,
    check = 3,

    pub fn bindOp(self: EventKind) Op {
        return switch (self) {
            .click => .bind_click,
            .input => .bind_input,
            .check => .bind_check,
        };
    }
};

pub const Counts = struct {
    total: u64 = 0,
    reset_dom: u64 = 0,
    create_element: u64 = 0,
    append_child: u64 = 0,
    remove_node: u64 = 0,
    move_before: u64 = 0,
    set_text: u64 = 0,
    set_value: u64 = 0,
    set_checked: u64 = 0,
    set_disabled: u64 = 0,
    set_metadata: u64 = 0,
    bind_event: u64 = 0,

    pub fn addOp(self: *Counts, op: Op) void {
        self.total += 1;
        switch (op) {
            .reset_dom => self.reset_dom += 1,
            .create_element, .create_text => self.create_element += 1,
            .append_child => self.append_child += 1,
            .remove_node => self.remove_node += 1,
            .move_before => self.move_before += 1,
            .set_text => self.set_text += 1,
            .set_value => self.set_value += 1,
            .set_checked => self.set_checked += 1,
            .set_disabled => self.set_disabled += 1,
            .set_role, .set_label, .set_test_id => self.set_metadata += 1,
            .bind_click, .bind_input, .bind_check => self.bind_event += 1,
        }
    }

    pub fn addHostReset(self: *Counts) void {
        self.addOp(.reset_dom);
    }

    pub fn addCreateElement(self: *Counts) void {
        self.addOp(.create_element);
    }

    pub fn addAppendChild(self: *Counts) void {
        self.addOp(.append_child);
    }

    pub fn addRemoveNode(self: *Counts) void {
        self.addOp(.remove_node);
    }

    pub fn addMoveBefore(self: *Counts) void {
        self.addOp(.move_before);
    }

    pub fn addTextField(self: *Counts, field: TextField) void {
        self.addOp(field.setOp());
    }

    pub fn addBoolField(self: *Counts, field: BoolField) void {
        self.addOp(field.setOp());
    }

    pub fn addEventBinding(self: *Counts) void {
        self.addOp(.bind_click);
    }

    pub fn addEventBindingKind(self: *Counts, kind: EventKind) void {
        self.addOp(kind.bindOp());
    }

    pub fn addAll(self: *Counts, other: Counts) void {
        self.total += other.total;
        self.reset_dom += other.reset_dom;
        self.create_element += other.create_element;
        self.append_child += other.append_child;
        self.remove_node += other.remove_node;
        self.move_before += other.move_before;
        self.set_text += other.set_text;
        self.set_value += other.set_value;
        self.set_checked += other.set_checked;
        self.set_disabled += other.set_disabled;
        self.set_metadata += other.set_metadata;
        self.bind_event += other.bind_event;
    }
};

pub const Metrics = struct {
    patches_emitted: u64 = 0,
    reset_dom: u64 = 0,
    create_element: u64 = 0,
    append_child: u64 = 0,
    remove_node: u64 = 0,
    move_before: u64 = 0,
    set_text: u64 = 0,
    set_value: u64 = 0,
    set_checked: u64 = 0,
    set_disabled: u64 = 0,
    set_metadata: u64 = 0,
    bind_event: u64 = 0,

    pub fn addCommandCounts(self: *Metrics, counts: Counts) void {
        self.patches_emitted += counts.total;
        self.reset_dom += counts.reset_dom;
        self.create_element += counts.create_element;
        self.append_child += counts.append_child;
        self.remove_node += counts.remove_node;
        self.move_before += counts.move_before;
        self.set_text += counts.set_text;
        self.set_value += counts.set_value;
        self.set_checked += counts.set_checked;
        self.set_disabled += counts.set_disabled;
        self.set_metadata += counts.set_metadata;
        self.bind_event += counts.bind_event;
    }
};

test "render command counts group detailed host-independent ops" {
    var counts: Counts = .{};
    counts.addOp(.reset_dom);
    counts.addOp(.create_element);
    counts.addOp(.create_text);
    counts.addOp(.append_child);
    counts.addOp(.remove_node);
    counts.addOp(.move_before);
    counts.addTextField(.text);
    counts.addTextField(.value);
    counts.addTextField(.role);
    counts.addTextField(.label);
    counts.addTextField(.test_id);
    counts.addBoolField(.checked);
    counts.addBoolField(.disabled);
    counts.addEventBindingKind(.click);
    counts.addEventBindingKind(.input);
    counts.addEventBindingKind(.check);

    try std.testing.expectEqual(@as(u64, 16), counts.total);
    try std.testing.expectEqual(@as(u64, 1), counts.reset_dom);
    try std.testing.expectEqual(@as(u64, 2), counts.create_element);
    try std.testing.expectEqual(@as(u64, 1), counts.append_child);
    try std.testing.expectEqual(@as(u64, 1), counts.remove_node);
    try std.testing.expectEqual(@as(u64, 1), counts.move_before);
    try std.testing.expectEqual(@as(u64, 1), counts.set_text);
    try std.testing.expectEqual(@as(u64, 1), counts.set_value);
    try std.testing.expectEqual(@as(u64, 1), counts.set_checked);
    try std.testing.expectEqual(@as(u64, 1), counts.set_disabled);
    try std.testing.expectEqual(@as(u64, 3), counts.set_metadata);
    try std.testing.expectEqual(@as(u64, 3), counts.bind_event);
}

test "render command buffer stores fixed-width records" {
    var buffer: Buffer = .{};
    defer buffer.deinit(std.testing.allocator);

    try std.testing.expectEqual(@as(usize, 0), buffer.len());
    try std.testing.expectEqual(@as(usize, 0), buffer.ptrAddress());

    try buffer.append(std.testing.allocator, .create_element, 7, 2, 0, 0, 0);
    try buffer.append(std.testing.allocator, .set_text, 7, 1024, 12, 0, 0);

    try std.testing.expectEqual(@as(usize, 2), buffer.len());
    try std.testing.expect(buffer.ptrAddress() != 0);
    try std.testing.expectEqual(@as(usize, 6), Record.word_count);
    try std.testing.expectEqual(@intFromEnum(Op.create_element), buffer.records.items[0].op);
    try std.testing.expectEqual(@as(u32, 7), buffer.records.items[0].a);
    try std.testing.expectEqual(@as(u32, 2), buffer.records.items[0].b);
    try std.testing.expectEqual(@intFromEnum(Op.set_text), buffer.records.items[1].op);
    try std.testing.expectEqual(@as(u32, 1024), buffer.records.items[1].b);
    try std.testing.expectEqual(@as(u32, 12), buffer.records.items[1].c);

    buffer.clearRetainingCapacity();
    try std.testing.expectEqual(@as(usize, 0), buffer.len());
    try std.testing.expectEqual(@as(usize, 0), buffer.ptrAddress());
}

test "render metrics accumulate command counts" {
    var metrics: Metrics = .{};
    var counts: Counts = .{};
    counts.addOp(.append_child);
    counts.addOp(.move_before);
    counts.addOp(.set_label);
    counts.addOp(.bind_input);

    metrics.addCommandCounts(counts);

    try std.testing.expectEqual(@as(u64, 4), metrics.patches_emitted);
    try std.testing.expectEqual(@as(u64, 1), metrics.append_child);
    try std.testing.expectEqual(@as(u64, 1), metrics.move_before);
    try std.testing.expectEqual(@as(u64, 1), metrics.set_metadata);
    try std.testing.expectEqual(@as(u64, 1), metrics.bind_event);
}
