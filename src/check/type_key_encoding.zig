//! Versioned checked-type key wire encoding. All producers share tags, integer
//! encoding, child references, and node-buffer finalization here.
const std = @import("std");
const TypeDigestHasher = @import("base").TypeDigestHasher;
const CanonicalTypeKey = @import("canonical_names.zig").CanonicalTypeKey;
const Allocator = std.mem.Allocator;

/// The version is in the message as well as the enclosing artifact format.
pub const domain = [_]u8{ 'R', 'T', 'K', 2 };

/// Zero and one remain reserved for boolean markers. Discriminators are stable
/// wire values; changes to this enum require a new domain and cache versions.
pub const Tag = enum(u8) {
    child_key = 2,
    empty_tag_union,
    alias,
    canonical_type_scheme,
    cycle,
    empty_record,
    err,
    err_var,
    field_default,
    fn_effectful,
    fn_pure,
    identity_var_anchor,
    identity_var_ref,
    named,
    nominal,
    padding,
    presence_defaulted,
    presence_optional,
    presence_optional_field,
    presence_required,
    presence_variable,
    record,
    record_unbound,
    tag_union,
    tuple,
    flex,
    rigid,
    defaulted_empty_tag_union,
    desugared_binop,
    desugared_unaryop,
    method_call,
    where_clause,
    from_literal,
    checked_synthetic_function,
    checked_identity_instance,
    /// Literal-default query: one boolean selects Str (true) or Dec (false).
    literal_default,
};

/// Render nested nodes in contiguous byte ranges. A composed node is hashed in
/// one update when it closes; the top-level stream flushes complete SHA blocks.
pub const Encoder = struct {
    allocator: Allocator,
    hasher: TypeDigestHasher,
    bytes: std.ArrayList(u8) = .empty,
    starts: std.ArrayList(usize) = .empty,

    /// Start a domain-separated top-level stream.
    pub fn init(allocator: Allocator) Encoder {
        var hasher = TypeDigestHasher.init();
        hasher.update(&domain);
        return .{ .allocator = allocator, .hasher = hasher };
    }

    /// Release rendering storage.
    pub fn deinit(self: *Encoder) void {
        self.bytes.deinit(self.allocator);
        self.starts.deinit(self.allocator);
    }

    /// Discard all earlier bytes and unfinished nodes, retaining capacity.
    pub fn reset(self: *Encoder) void {
        self.bytes.clearRetainingCapacity();
        self.starts.clearRetainingCapacity();
        self.hasher = TypeDigestHasher.init();
        self.hasher.update(&domain);
    }

    /// Begin a context-independent child node.
    pub fn beginNode(self: *Encoder) Allocator.Error!void {
        try self.starts.append(self.allocator, self.bytes.items.len);
        try self.bytes.appendSlice(self.allocator, &domain);
    }

    /// Hash exactly the immediate node encoding, then discard its byte range.
    pub fn endNode(self: *Encoder) CanonicalTypeKey {
        const start = self.starts.pop().?;
        var hasher = TypeDigestHasher.init();
        hasher.update(self.bytes.items[start..]);
        const key = CanonicalTypeKey{ .bytes = hasher.finalResult() };
        self.bytes.items.len = start;
        return key;
    }

    /// Append raw fixed-shape bytes. Variable-length data uses `writeBytes`.
    pub fn update(self: *Encoder, bytes: []const u8) Allocator.Error!void {
        try self.bytes.appendSlice(self.allocator, bytes);
        if (self.starts.items.len == 0) self.flushBlocks();
    }

    fn flushBlocks(self: *Encoder) void {
        // Account for the short domain already in the hasher's partial block.
        // The first flush completes it; subsequent updates contain whole blocks.
        const prefix: usize = @intCast(self.hasher.total_len % 64);
        const available = prefix + self.bytes.items.len;
        if (available < 64) return;
        const count = available / 64 * 64 - prefix;
        self.hasher.update(self.bytes.items[0..count]);
        const remaining = self.bytes.items.len - count;
        std.mem.copyForwards(u8, self.bytes.items[0..remaining], self.bytes.items[count..]);
        self.bytes.items.len = remaining;
    }

    /// Finish a complete top-level stream. No allocation occurs at finalization.
    pub fn finalResult(self: *Encoder) [32]u8 {
        std.debug.assert(self.starts.items.len == 0);
        self.hasher.update(self.bytes.items);
        self.bytes.clearRetainingCapacity();
        return self.hasher.finalResult();
    }

    /// Emit a single node or field discriminator.
    pub fn writeTag(self: *Encoder, tag: Tag) Allocator.Error!void {
        try self.update(&.{@intFromEnum(tag)});
    }

    /// Emit a boolean marker, disjoint from tag values.
    pub fn writeBool(self: *Encoder, value: bool) Allocator.Error!void {
        try self.update(&.{@intFromBool(value)});
    }

    /// Counts, lengths, slots and source discriminators use unsigned LEB128.
    pub fn writeU32(self: *Encoder, value: u32) Allocator.Error!void {
        var bytes: [5]u8 = undefined;
        var remaining = value;
        var count: usize = 0;
        while (true) {
            const low: u8 = @truncate(remaining & 0x7f);
            remaining >>= 7;
            bytes[count] = low | @as(u8, if (remaining != 0) 0x80 else 0);
            count += 1;
            if (remaining == 0) break;
        }
        try self.update(bytes[0..count]);
    }

    /// Identifiers and other variable-length data retain an explicit length.
    pub fn writeBytes(self: *Encoder, bytes: []const u8) Allocator.Error!void {
        try self.writeU32(@intCast(bytes.len));
        try self.update(bytes);
    }

    /// Shared function-node header for source, checked and synthetic functions.
    pub fn writeFunctionHeader(self: *Encoder, effectful: bool, arg_count: u32) Allocator.Error!void {
        try self.writeTag(if (effectful) .fn_effectful else .fn_pure);
        try self.writeU32(arg_count);
    }
};

/// A child reference is one discriminator followed by exactly 32 digest bytes.
pub fn writeChildKeyReference(encoder: *Encoder, key: CanonicalTypeKey) Allocator.Error!void {
    try encoder.writeTag(.child_key);
    try encoder.update(&key.bytes);
}

test "checked key rendering batches streams and hashes nested nodes independently" {
    const allocator = std.testing.allocator;
    var encoder = Encoder.init(allocator);
    defer encoder.deinit();
    const payload = [_]u8{0xa5} ** 257;
    var expected = TypeDigestHasher.init();
    expected.update(&domain);
    expected.update(&payload);
    for (payload) |byte| try encoder.update(&.{byte});
    try std.testing.expectEqualSlices(u8, &expected.finalResult(), &encoder.finalResult());
    encoder.reset();
    try encoder.writeTag(.tuple);
    try encoder.writeU32(1);
    try encoder.beginNode();
    try encoder.writeTag(.empty_record);
    const child = encoder.endNode();
    try writeChildKeyReference(&encoder, child);
    var expected_child = TypeDigestHasher.init();
    expected_child.update(&domain);
    expected_child.update(&.{@intFromEnum(Tag.empty_record)});
    try std.testing.expectEqualSlices(u8, &expected_child.finalResult(), &child.bytes);
    var expected_parent = TypeDigestHasher.init();
    expected_parent.update(&domain);
    expected_parent.update(&.{ @intFromEnum(Tag.tuple), 1, @intFromEnum(Tag.child_key) });
    expected_parent.update(&child.bytes);
    try std.testing.expectEqualSlices(u8, &expected_parent.finalResult(), &encoder.finalResult());
}

test "checked key varints preserve boundaries and full u32 values" {
    const allocator = std.testing.allocator;
    var encoder = Encoder.init(allocator);
    defer encoder.deinit();
    try encoder.beginNode();
    const values = [_]u32{ 0, 1, 127, 128, 16383, 16384, 0xffffffff };
    for (values) |value| try encoder.writeU32(value);
    var decoder = TestDecoder{ .bytes = encoder.bytes.items[domain.len..] };
    for (values) |value| try std.testing.expectEqual(value, try decoder.integer());
    try std.testing.expectEqual(decoder.bytes.len, decoder.offset);
}

test "checked node encoding decodes record union function alias nominal and literal shapes" {
    const allocator = std.testing.allocator;
    const child = CanonicalTypeKey{ .bytes = [_]u8{0xb7} ** 32 };
    const shapes = [_]Tag{ .record, .tag_union, .fn_pure, .alias, .nominal, .literal_default };
    for (shapes) |shape| {
        var encoder = Encoder.init(allocator);
        defer encoder.deinit();
        try encoder.beginNode();
        try encoder.writeTag(shape);
        switch (shape) {
            .literal_default => try encoder.writeBool(true),
            .record => {
                try encoder.writeU32(1);
                try encoder.writeBytes("field");
                try encoder.writeBool(false);
                try writeChildKeyReference(&encoder, child);
                try writeChildKeyReference(&encoder, child);
            },
            .tag_union => {
                try encoder.writeU32(1);
                try encoder.writeBytes("Some");
                try encoder.writeU32(1);
                try writeChildKeyReference(&encoder, child);
                try writeChildKeyReference(&encoder, child);
            },
            .fn_pure => {
                try encoder.writeU32(1);
                try writeChildKeyReference(&encoder, child);
                try writeChildKeyReference(&encoder, child);
            },
            .alias => {
                try encoder.writeBytes(&([_]u8{0xa1} ** 32));
                try encoder.writeBool(false);
                try encoder.writeBytes("Alias");
                try writeChildKeyReference(&encoder, child);
                try encoder.writeU32(1);
                try writeChildKeyReference(&encoder, child);
            },
            .nominal => {
                try encoder.writeBytes(&([_]u8{0xa1} ** 32));
                try encoder.writeBool(true);
                try encoder.writeU32(128);
                try encoder.writeBool(true);
                try encoder.writeU32(2);
                try writeChildKeyReference(&encoder, child);
                try writeChildKeyReference(&encoder, child);
                try encoder.writeU32(0);
                try encoder.writeU32(0);
            },
            .child_key,
            .empty_tag_union,
            .canonical_type_scheme,
            .cycle,
            .empty_record,
            .err,
            .err_var,
            .field_default,
            .fn_effectful,
            .identity_var_anchor,
            .identity_var_ref,
            .named,
            .padding,
            .presence_defaulted,
            .presence_optional,
            .presence_optional_field,
            .presence_required,
            .presence_variable,
            .record_unbound,
            .tuple,
            .flex,
            .rigid,
            .defaulted_empty_tag_union,
            .desugared_binop,
            .desugared_unaryop,
            .method_call,
            .where_clause,
            .from_literal,
            .checked_synthetic_function,
            .checked_identity_instance,
            => unreachable,
        }
        var decoder = TestDecoder{ .bytes = encoder.bytes.items[domain.len..] };
        try std.testing.expectEqual(@intFromEnum(shape), try decoder.byte());
        switch (shape) {
            .literal_default => try std.testing.expectEqual(@as(u8, 1), try decoder.byte()),
            .record => {
                try std.testing.expectEqual(@as(u32, 1), try decoder.integer());
                try decoder.string();
                try std.testing.expectEqual(@as(u8, 0), try decoder.byte());
            },
            .tag_union => {
                try std.testing.expectEqual(@as(u32, 1), try decoder.integer());
                try decoder.string();
                try std.testing.expectEqual(@as(u32, 1), try decoder.integer());
            },
            .fn_pure => try std.testing.expectEqual(@as(u32, 1), try decoder.integer()),
            .alias => {
                try decoder.string();
                try std.testing.expectEqual(@as(u8, 0), try decoder.byte());
                try decoder.string();
                try decoder.child();
                try std.testing.expectEqual(@as(u32, 1), try decoder.integer());
            },
            .nominal => {
                try decoder.string();
                try std.testing.expectEqual(@as(u8, 1), try decoder.byte());
                try std.testing.expectEqual(@as(u32, 128), try decoder.integer());
                try std.testing.expectEqual(@as(u8, 1), try decoder.byte());
                try std.testing.expectEqual(@as(u32, 2), try decoder.integer());
            },
            .child_key,
            .empty_tag_union,
            .canonical_type_scheme,
            .cycle,
            .empty_record,
            .err,
            .err_var,
            .field_default,
            .fn_effectful,
            .identity_var_anchor,
            .identity_var_ref,
            .named,
            .padding,
            .presence_defaulted,
            .presence_optional,
            .presence_optional_field,
            .presence_required,
            .presence_variable,
            .record_unbound,
            .tuple,
            .flex,
            .rigid,
            .defaulted_empty_tag_union,
            .desugared_binop,
            .desugared_unaryop,
            .method_call,
            .where_clause,
            .from_literal,
            .checked_synthetic_function,
            .checked_identity_instance,
            => unreachable,
        }
        if (shape != .literal_default) {
            try decoder.child();
            if (shape != .alias) try decoder.child();
        }
        if (shape == .nominal) {
            try std.testing.expectEqual(@as(u32, 0), try decoder.integer());
            try std.testing.expectEqual(@as(u32, 0), try decoder.integer());
        }
        try std.testing.expectEqual(decoder.bytes.len, decoder.offset);
    }
}

const TestDecoder = struct {
    bytes: []const u8,
    offset: usize = 0,

    fn byte(self: *TestDecoder) error{TruncatedEncoding}!u8 {
        if (self.offset == self.bytes.len) return error.TruncatedEncoding;
        const value = self.bytes[self.offset];
        self.offset += 1;
        return value;
    }

    fn integer(self: *TestDecoder) error{ TruncatedEncoding, InvalidInteger }!u32 {
        var result: u32 = 0;
        var shift: u5 = 0;
        while (true) {
            const value = try self.byte();
            if (shift == 28 and value > 15) return error.InvalidInteger;
            result |= @as(u32, value & 0x7f) << shift;
            if (value & 0x80 == 0) return result;
            shift += 7;
        }
    }

    fn string(self: *TestDecoder) error{ TruncatedEncoding, InvalidInteger }!void {
        const len = try self.integer();
        if (len > self.bytes.len - self.offset) return error.TruncatedEncoding;
        self.offset += len;
    }

    fn child(self: *TestDecoder) !void {
        try std.testing.expectEqual(@intFromEnum(Tag.child_key), try self.byte());
        if (self.bytes.len - self.offset < 32) return error.TruncatedEncoding;
        self.offset += 32;
    }
};
