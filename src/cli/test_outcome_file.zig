//! One checked module's cached `roc test` outcomes (design.md "Test outcome
//! files").
//!
//! A file records what running one module's test roots observed under one
//! execution mode. Its identity names everything those observations are a
//! function of: the module's checked artifact key (its own source inputs,
//! the content identity of its whole import closure, its platform/app
//! relation and its imports' keys), the compiler, the native target, the
//! specialization strategy and the optimization mode. Inside, each entry is
//! keyed by the `order` of one root within the module and holds only what
//! execution produced: whether the root passed, the region and message of a
//! `?` that failed it, its transcript, and the inline expects it reached.
//! Everything else a report shows is read from the current checked module.

const std = @import("std");
const base = @import("base");

const Allocator = std.mem.Allocator;
const Sha256 = base.Sha256;

const magic = "roc-test-outcomes-v2";

/// Everything a module's outcome file is a function of besides each root's
/// order. Paths, rendering settings and worker counts are not inputs.
pub const Inputs = struct {
    /// The module's checked artifact key (`CheckedModuleArtifactKey`).
    artifact_key: [32]u8,
    compiler: []const u8,
    target: []const u8,
    strategy: []const u8,
    /// An outcome records what one execution mode did with the tests,
    /// including whether they exhausted its resources.
    opt: []const u8,

    pub fn key(self: Inputs) [32]u8 {
        var hasher = Sha256.init(.{});
        hashString(&hasher, magic);
        hasher.update(&self.artifact_key);
        hashString(&hasher, self.compiler);
        hashString(&hasher, self.target);
        hashString(&hasher, self.strategy);
        hashString(&hasher, self.opt);
        return hasher.finalResult();
    }
};

fn hashString(hasher: *Sha256, bytes: []const u8) void {
    var len: [8]u8 = undefined;
    std.mem.writeInt(u64, &len, bytes.len, .little);
    hasher.update(&len);
    hasher.update(bytes);
}

/// One event of a root's transcript: its stream and kind as the CLI numbers
/// them, and its payload.
pub const Event = struct {
    stream: u8,
    kind: u8,
    payload: []const u8,
};

/// One inline expect a root reached: the content identity of the module
/// holding it, its region there, and how many times it passed and failed.
pub const InlineExpect = struct {
    module: [32]u8,
    region_start: u32,
    region_end: u32,
    passed: u64,
    failed: u64,
};

/// Where a failed root's detail comes from: nowhere (a crash explains
/// itself in the transcript), the expect's own source, or a message
/// execution produced (the `Err` a `?` failed with).
pub const Detail = union(enum) {
    none,
    source,
    message: []const u8,
};

/// A source region by byte offsets.
pub const Region = struct { start: u32, end: u32 };

/// What running one root observed.
pub const Outcome = struct {
    order: u32,
    passed: bool,
    /// The region the report points at when it is not the root's own: the
    /// `?` expression whose `Err` failed the expect.
    region: ?Region,
    detail: Detail,
    events: []const Event,
    inline_expects: []const InlineExpect,
};

/// The outcomes a file records, by increasing order; slices borrow the
/// file's bytes.
pub const Outcomes = []const Outcome;

const Reader = struct {
    bytes: []const u8,
    offset: usize = 0,

    fn u8_(self: *Reader) ?u8 {
        if (self.offset >= self.bytes.len) return null;
        defer self.offset += 1;
        return self.bytes[self.offset];
    }

    fn u32_(self: *Reader) ?u32 {
        if (self.bytes.len - self.offset < 4) return null;
        defer self.offset += 4;
        return std.mem.readInt(u32, self.bytes[self.offset..][0..4], .little);
    }

    fn u64_(self: *Reader) ?u64 {
        if (self.bytes.len - self.offset < 8) return null;
        defer self.offset += 8;
        return std.mem.readInt(u64, self.bytes[self.offset..][0..8], .little);
    }

    fn slice(self: *Reader, len: usize) ?[]const u8 {
        if (self.bytes.len - self.offset < len) return null;
        defer self.offset += len;
        return self.bytes[self.offset..][0..len];
    }

    fn string(self: *Reader) ?[]const u8 {
        return self.slice(self.u32_() orelse return null);
    }
};

/// The outcomes `bytes` records. A file this version did not write, or one
/// that does not decode completely as outcomes in strictly increasing
/// order, records none: it is a miss for every root, and the next
/// publication replaces it.
pub fn parse(allocator: Allocator, bytes: []const u8) Allocator.Error!Outcomes {
    return parseAll(allocator, bytes) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.Malformed => return &.{},
    };
}

const ParseError = Allocator.Error || error{Malformed};

fn parseAll(allocator: Allocator, bytes: []const u8) ParseError!Outcomes {
    var reader = Reader{ .bytes = bytes };
    const head = reader.slice(magic.len) orelse return error.Malformed;
    if (!std.mem.eql(u8, head, magic)) return error.Malformed;
    const count = reader.u32_() orelse return error.Malformed;
    // Every entry holds at least its order and its flags.
    if (count > (bytes.len - reader.offset) / 6) return error.Malformed;
    var outcomes: std.ArrayList(Outcome) = .empty;
    errdefer outcomes.deinit(allocator);
    errdefer freeOutcomeList(allocator, outcomes.items);
    try outcomes.ensureTotalCapacity(allocator, count);
    for (0..count) |index| {
        const order = reader.u32_() orelse return error.Malformed;
        if (index != 0 and outcomes.items[index - 1].order >= order) return error.Malformed;
        const passed = switch (reader.u8_() orelse return error.Malformed) {
            0 => false,
            1 => true,
            else => return error.Malformed,
        };
        const region: ?Region = switch (reader.u8_() orelse return error.Malformed) {
            0 => null,
            1 => .{ .start = reader.u32_() orelse return error.Malformed, .end = reader.u32_() orelse return error.Malformed },
            else => return error.Malformed,
        };
        const detail: Detail = switch (reader.u8_() orelse return error.Malformed) {
            0 => .none,
            1 => .source,
            2 => .{ .message = reader.string() orelse return error.Malformed },
            else => return error.Malformed,
        };
        if (passed and detail != .none) return error.Malformed;
        const event_count = reader.u32_() orelse return error.Malformed;
        if (event_count > (bytes.len - reader.offset) / 6) return error.Malformed;
        const events = try allocator.alloc(Event, event_count);
        errdefer allocator.free(events);
        for (events) |*event| {
            event.* = .{
                .stream = reader.u8_() orelse return error.Malformed,
                .kind = reader.u8_() orelse return error.Malformed,
                .payload = reader.string() orelse return error.Malformed,
            };
        }
        const inline_count = reader.u32_() orelse return error.Malformed;
        if (inline_count > (bytes.len - reader.offset) / 56) return error.Malformed;
        const inline_expects = try allocator.alloc(InlineExpect, inline_count);
        errdefer allocator.free(inline_expects);
        for (inline_expects, 0..) |*inline_expect, inline_index| {
            inline_expect.* = .{
                .module = (reader.slice(32) orelse return error.Malformed)[0..32].*,
                .region_start = reader.u32_() orelse return error.Malformed,
                .region_end = reader.u32_() orelse return error.Malformed,
                .passed = reader.u64_() orelse return error.Malformed,
                .failed = reader.u64_() orelse return error.Malformed,
            };
            if (inline_expect.passed == 0 and inline_expect.failed == 0) return error.Malformed;
            if (inline_index != 0 and !inlineExpectLessThan(inline_expects[inline_index - 1], inline_expect.*)) return error.Malformed;
        }
        outcomes.appendAssumeCapacity(.{
            .order = order,
            .passed = passed,
            .region = region,
            .detail = detail,
            .events = events,
            .inline_expects = inline_expects,
        });
    }
    if (reader.offset != bytes.len) return error.Malformed;
    return try outcomes.toOwnedSlice(allocator);
}

/// Inline expects are recorded by module, then by region, so a file's
/// contents are a function of what ran and not of the order it ran in.
pub fn inlineExpectLessThan(a: InlineExpect, b: InlineExpect) bool {
    const modules = std.mem.order(u8, &a.module, &b.module);
    if (modules != .eq) return modules == .lt;
    if (a.region_start != b.region_start) return a.region_start < b.region_start;
    return a.region_end < b.region_end;
}

fn freeOutcomeList(allocator: Allocator, outcomes: []const Outcome) void {
    for (outcomes) |outcome| {
        allocator.free(outcome.events);
        allocator.free(outcome.inline_expects);
    }
}

/// Free what `parse` returned.
pub fn free(allocator: Allocator, outcomes: Outcomes) void {
    freeOutcomeList(allocator, outcomes);
    if (outcomes.len != 0) allocator.free(outcomes);
}

/// A copy of `outcome` whose event and inline-expect lists `allocator`
/// owns, and whose payloads and message still borrow the original's.
pub fn dupeBorrowed(allocator: Allocator, outcome: Outcome) Allocator.Error!Outcome {
    const events = try allocator.dupe(Event, outcome.events);
    errdefer allocator.free(events);
    var copy = outcome;
    copy.events = events;
    copy.inline_expects = try allocator.dupe(InlineExpect, outcome.inline_expects);
    return copy;
}

/// Free the lists of an outcome `dupeBorrowed` made, or one whose lists
/// `allocator` owns the same way.
pub fn freeBorrowed(allocator: Allocator, outcome: Outcome) void {
    allocator.free(outcome.events);
    allocator.free(outcome.inline_expects);
}

/// Builds the bytes of one file. Outcomes are appended in strictly
/// increasing order.
pub const Builder = struct {
    bytes: std.ArrayList(u8) = .empty,
    count: u32 = 0,
    last_order: ?u32 = null,

    pub fn init(allocator: Allocator) Allocator.Error!Builder {
        var builder = Builder{};
        try builder.bytes.appendSlice(allocator, magic);
        try builder.bytes.appendSlice(allocator, &[_]u8{ 0, 0, 0, 0 });
        return builder;
    }

    pub fn deinit(self: *Builder, allocator: Allocator) void {
        self.bytes.deinit(allocator);
    }

    pub fn append(self: *Builder, allocator: Allocator, outcome: Outcome) Allocator.Error!void {
        if (self.last_order) |last| std.debug.assert(last < outcome.order);
        self.last_order = outcome.order;
        std.debug.assert(!outcome.passed or outcome.detail == .none);
        try appendU32(&self.bytes, allocator, outcome.order);
        try self.bytes.append(allocator, @intFromBool(outcome.passed));
        if (outcome.region) |region| {
            try self.bytes.append(allocator, 1);
            try appendU32(&self.bytes, allocator, region.start);
            try appendU32(&self.bytes, allocator, region.end);
        } else {
            try self.bytes.append(allocator, 0);
        }
        switch (outcome.detail) {
            .none => try self.bytes.append(allocator, 0),
            .source => try self.bytes.append(allocator, 1),
            .message => |message| {
                try self.bytes.append(allocator, 2);
                try appendString(&self.bytes, allocator, message);
            },
        }
        try appendU32(&self.bytes, allocator, @intCast(outcome.events.len));
        for (outcome.events) |event| {
            try self.bytes.append(allocator, event.stream);
            try self.bytes.append(allocator, event.kind);
            try appendString(&self.bytes, allocator, event.payload);
        }
        try appendU32(&self.bytes, allocator, @intCast(outcome.inline_expects.len));
        for (outcome.inline_expects, 0..) |inline_expect, index| {
            std.debug.assert(inline_expect.passed != 0 or inline_expect.failed != 0);
            std.debug.assert(index == 0 or inlineExpectLessThan(outcome.inline_expects[index - 1], inline_expect));
            try self.bytes.appendSlice(allocator, &inline_expect.module);
            try appendU32(&self.bytes, allocator, inline_expect.region_start);
            try appendU32(&self.bytes, allocator, inline_expect.region_end);
            try appendU64(&self.bytes, allocator, inline_expect.passed);
            try appendU64(&self.bytes, allocator, inline_expect.failed);
        }
        self.count += 1;
        std.mem.writeInt(u32, self.bytes.items[magic.len..][0..4], self.count, .little);
    }

    /// The file's bytes so far.
    pub fn written(self: *const Builder) []const u8 {
        return self.bytes.items;
    }
};

fn appendU32(bytes: *std.ArrayList(u8), allocator: Allocator, value: u32) Allocator.Error!void {
    var buffer: [4]u8 = undefined;
    std.mem.writeInt(u32, &buffer, value, .little);
    try bytes.appendSlice(allocator, &buffer);
}

fn appendU64(bytes: *std.ArrayList(u8), allocator: Allocator, value: u64) Allocator.Error!void {
    var buffer: [8]u8 = undefined;
    std.mem.writeInt(u64, &buffer, value, .little);
    try bytes.appendSlice(allocator, &buffer);
}

fn appendString(bytes: *std.ArrayList(u8), allocator: Allocator, string: []const u8) Allocator.Error!void {
    try appendU32(bytes, allocator, @intCast(string.len));
    try bytes.appendSlice(allocator, string);
}

/// Whether two outcomes record the same observations.
pub fn eql(a: Outcome, b: Outcome) bool {
    if (a.order != b.order or a.passed != b.passed) return false;
    if ((a.region == null) != (b.region == null)) return false;
    if (a.region) |region| if (region.start != b.region.?.start or region.end != b.region.?.end) return false;
    switch (a.detail) {
        .none => if (b.detail != .none) return false,
        .source => if (b.detail != .source) return false,
        .message => |message| switch (b.detail) {
            .message => |other| if (!std.mem.eql(u8, message, other)) return false,
            .none, .source => return false,
        },
    }
    if (a.events.len != b.events.len or a.inline_expects.len != b.inline_expects.len) return false;
    for (a.events, b.events) |x, y| if (x.stream != y.stream or x.kind != y.kind or !std.mem.eql(u8, x.payload, y.payload)) return false;
    for (a.inline_expects, b.inline_expects) |x, y| if (!std.meta.eql(x, y)) return false;
    return true;
}

test "the file key names every input it is a function of" {
    const inputs = Inputs{
        .artifact_key = @splat(0x5a),
        .compiler = "compiler-a",
        .target = "x64glibc",
        .strategy = "boxy",
        .opt = "dev",
    };
    var changed: [5]Inputs = @splat(inputs);
    changed[0].artifact_key = @splat(0x5b);
    changed[1].compiler = "compiler-b";
    changed[2].target = "arm64glibc";
    changed[3].strategy = "lss";
    changed[4].opt = "speed";
    for (changed) |other| try std.testing.expect(!std.mem.eql(u8, &inputs.key(), &other.key()));
    try std.testing.expectEqualSlices(u8, &inputs.key(), &inputs.key());
}

test "a file records its outcomes and anything else records none" {
    const allocator = std.testing.allocator;
    const events = [_]Event{ .{ .stream = 0, .kind = 0, .payload = "dbg: 1" }, .{ .stream = 1, .kind = 1, .payload = "" } };
    const inline_expects = [_]InlineExpect{
        .{ .module = @splat(1), .region_start = 10, .region_end = 20, .passed = 3, .failed = 0 },
        .{ .module = @splat(1), .region_start = 30, .region_end = 40, .passed = 0, .failed = 2 },
    };
    const outcomes = [_]Outcome{
        .{ .order = 0, .passed = true, .region = null, .detail = .none, .events = &.{}, .inline_expects = &.{} },
        .{ .order = 2, .passed = false, .region = null, .detail = .source, .events = &events, .inline_expects = &inline_expects },
        .{ .order = 7, .passed = false, .region = .{ .start = 5, .end = 9 }, .detail = .{ .message = "Err(Oops)" }, .events = &.{}, .inline_expects = &.{} },
    };
    var builder = try Builder.init(allocator);
    defer builder.deinit(allocator);
    for (outcomes) |outcome| try builder.append(allocator, outcome);
    const bytes = builder.written();
    {
        const parsed = try parse(allocator, bytes);
        defer free(allocator, parsed);
        try std.testing.expectEqual(outcomes.len, parsed.len);
        for (outcomes, parsed) |expected, actual| try std.testing.expect(eql(expected, actual));
    }
    {
        var empty = try Builder.init(allocator);
        defer empty.deinit(allocator);
        const parsed = try parse(allocator, empty.written());
        defer free(allocator, parsed);
        try std.testing.expectEqual(@as(usize, 0), parsed.len);
    }
    for (0..bytes.len) |len| {
        const parsed = try parse(allocator, bytes[0..len]);
        defer free(allocator, parsed);
        try std.testing.expectEqual(@as(usize, 0), parsed.len);
    }
    const damaged = try allocator.dupe(u8, bytes);
    defer allocator.free(damaged);
    damaged[0] ^= 0x40;
    {
        const parsed = try parse(allocator, damaged);
        defer free(allocator, parsed);
        try std.testing.expectEqual(@as(usize, 0), parsed.len);
    }
    damaged[0] ^= 0x40;
    // The second entry's order, lowered below the first's.
    const second_order = magic.len + 4 + 4 + 1 + 1 + 1 + 4 + 4;
    std.mem.writeInt(u32, damaged[second_order..][0..4], 0, .little);
    {
        const parsed = try parse(allocator, damaged);
        defer free(allocator, parsed);
        try std.testing.expectEqual(@as(usize, 0), parsed.len);
    }
}
