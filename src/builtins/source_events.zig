//! Compiler-owned source tracing, independent of platform host operations.
//! Immutable pointer-free records preserve producer semantics across execution
//! images; an optional thread-local observer gives those records meaning.

const std = @import("std");

pub const abi_version: u32 = 1;
pub const registration_abi_version: u32 = 1;
pub const symbol_name = "roc_source_event";
pub const Identity = [32]u8;

pub const Kind = enum(u32) {
    branch_taken = 1,
    exhaustiveness_failed = 2,
    failure = 3,
    call_enter = 4,
    call_exit = 5,
};

pub const Header = extern struct {
    version: u32 = abi_version,
    kind: u32,
    byte_size: u32 = @sizeOf(Descriptor),
    reserved: u32 = 0,
};

pub const Region = extern struct {
    start: u32 = 0,
    end: u32 = 0,
};

/// `has_location` preserves producer absence; consumers must not invent a source.
pub const Source = extern struct {
    checked_module: Identity = @splat(0),
    source_identity: Identity = @splat(0),
    region: Region = .{},
    line: u32 = 0,
    column: u32 = 0,
    has_location: u32 = 0,
};

pub const LiteralKind = enum(u32) { numeral = 1, quote = 2 };
pub const Literal = extern struct {
    checked_module: Identity = @splat(0),
    checked_expr: u32 = 0,
    kind: u32 = 0,
};

pub const ProducerKind = enum(u32) { checked = 1, literal = 2 };
pub const RootRole = enum(u32) { value = 1, failure_message = 2 };

/// A semantic producer certificate, never a mutable evaluator slot ordinal.
pub const Root = extern struct {
    module: Identity = @splat(0),
    producer_kind: u32 = 0,
    checked_expr: u32 = 0,
    literal: Literal = .{},
    procedure_identity: Identity = @splat(0),
    layout_digest: Identity = @splat(0),
    role: u32 = 0,
    failed_field: u32 = 0,
    message_field: u32 = 0,
    failed_offset: u32 = 0,
    message_offset: u32 = 0,
};

pub const Failure = extern struct {
    checked_error: u32 = 0,
    has_literal_rejection: u32 = 0,
    literal_rejection: Literal = .{},
    has_guard_root: u32 = 0,
    guard_root: Root = .{},
};

pub const SiteKind = enum(u32) { match = 1, destructure = 2, if_ = 3 };
pub const Site = extern struct {
    checked_module: Identity = @splat(0),
    procedure_identity: Identity = @splat(0),
    checked_site: u32 = 0,
    has_checked_site: u32 = 0,
    kind: u32 = 0,
    region: Region = .{},
    /// Offset from the header to `branch_count` consecutive Region records.
    branches_offset: u32 = 0,
    branch_count: u32 = 0,
};

/// ABI v1 has a fixed prefix and an optional Region tail for site events.
/// All integer fields use the execution target's native endian; persistent
/// encoders must encode fields explicitly, not hash padding or process pointers.
/// Call events carry the original call-site Source and procedure identity.
pub const Descriptor = extern struct {
    header: Header,
    source: Source = .{},
    procedure_identity: Identity = @splat(0),
    site: Site = .{},
    branch_index: u32 = 0,
    failure: Failure = .{},
};

pub const Invalid = error{
    UnsupportedVersion,
    InvalidKind,
    InvalidSize,
    InvalidReserved,
    InvalidFlag,
    InvalidRegion,
    InvalidLocation,
    InvalidSite,
    InvalidBranch,
    InvalidLiteral,
    InvalidRoot,
};

/// Stable registration ABI v1 tags; these are compiler contract violations,
/// never classifications of a Roc program's own failure.
pub const InvalidTag = enum(u32) {
    UnsupportedVersion = 1,
    InvalidKind = 2,
    InvalidSize = 3,
    InvalidReserved = 4,
    InvalidFlag = 5,
    InvalidRegion = 6,
    InvalidLocation = 7,
    InvalidSite = 8,
    InvalidBranch = 9,
    InvalidLiteral = 10,
    InvalidRoot = 11,
};

pub fn invalidTag(err: Invalid) InvalidTag {
    return switch (err) {
        error.UnsupportedVersion => .UnsupportedVersion,
        error.InvalidKind => .InvalidKind,
        error.InvalidSize => .InvalidSize,
        error.InvalidReserved => .InvalidReserved,
        error.InvalidFlag => .InvalidFlag,
        error.InvalidRegion => .InvalidRegion,
        error.InvalidLocation => .InvalidLocation,
        error.InvalidSite => .InvalidSite,
        error.InvalidBranch => .InvalidBranch,
        error.InvalidLiteral => .InvalidLiteral,
        error.InvalidRoot => .InvalidRoot,
    };
}

pub const Event = struct {
    kind: Kind,
    descriptor: *const Descriptor,
    branches: []align(1) const Region,
};

/// Registration ABI v1. Callbacks borrow validated descriptor storage only for
/// the duration of the call. `validate` exposes the typed Event view to Zig.
/// Invalid metadata is a producer-contract violation, not a missing event.
pub const Observer = extern struct {
    context: ?*anyopaque,
    on_event: *const fn (?*anyopaque, *const Header) callconv(.c) void,
    on_invalid: *const fn (?*anyopaque, u32) callconv(.c) void,
};

threadlocal var current_observer: ?Observer = null;
// An exact capability count, not a platform/TLS readiness probe. Raw-startup
// programs with no observer must return before native TLS is accessed at all.
var active_observers: std.atomic.Value(usize) = .init(0);

/// Live registration state, not persistent metadata. A Saved value must be
/// restored on its originating thread, in nesting order, without modification.
/// Pointer widths follow the target C ABI; inactive records have null pointers.
pub const Saved = extern struct {
    active: u32,
    reserved: u32 = 0,
    context: ?*anyopaque,
    on_event: ?*const fn (?*anyopaque, *const Header) callconv(.c) void,
    on_invalid: ?*const fn (?*anyopaque, u32) callconv(.c) void,
};

/// Registration belongs to this thread, independently of its RocOps host.
/// Active tracing requires an initialized native thread/TLS runtime on every
/// thread that may execute events. Globally disabled tracing has no such need.
pub fn enter(observer: ?Observer) Saved {
    const saved: Saved = if (current_observer) |previous| .{
        .active = 1,
        .context = previous.context,
        .on_event = previous.on_event,
        .on_invalid = previous.on_invalid,
    } else .{
        .active = 0,
        .context = null,
        .on_event = null,
        .on_invalid = null,
    };
    replaceObserver(observer);
    return saved;
}

pub fn leave(saved: Saved) void {
    if (saved.reserved != 0) @trap();
    replaceObserver(switch (saved.active) {
        0 => null,
        1 => .{
            .context = saved.context,
            .on_event = saved.on_event orelse @trap(),
            .on_invalid = saved.on_invalid orelse @trap(),
        },
        else => @trap(),
    });
}

fn replaceObserver(observer: ?Observer) void {
    const was_active = current_observer != null;
    current_observer = observer;
    // Publish a completed TLS transition before changing the global count.
    // Both operations complete before registration returns to its caller.
    if (!was_active and observer != null) {
        _ = active_observers.fetchAdd(1, .release);
    } else if (was_active and observer == null) {
        _ = active_observers.fetchSub(1, .release);
    }
}

/// Optional tracing-service exports, not required platform host symbols.
/// The supplied Observer is copied; its context must outlive the registration.
pub fn roc_source_event_observer_enter(observer: ?*const Observer, saved: *Saved) callconv(.c) void {
    saved.* = enter(if (observer) |value| value.* else null);
}

pub fn roc_source_event_observer_leave(saved: *const Saved) callconv(.c) void {
    leave(saved.*);
}

/// Generated code has this single C ABI, with or without an observer.
/// Globally unregistered calls inspect neither the pointer nor native TLS.
/// Registered callers guarantee a readable Header and `byte_size` readable bytes.
/// Validation detects malformed records, not arbitrary invalid pointers.
pub fn roc_source_event(header: *const Header) callconv(.c) void {
    if (active_observers.load(.acquire) == 0) return;
    // Keep all TLS address/resolver work behind the capability gate, including
    // in optimizing backends that can otherwise hoist TLS address computation.
    @call(.never_inline, observe, .{header});
}

fn observe(header: *const Header) void {
    const observer = current_observer orelse return;
    _ = validate(header) catch |err| {
        observer.on_invalid(observer.context, @intFromEnum(invalidTag(err)));
        return;
    };
    observer.on_event(observer.context, header);
}

// Only the standalone compiler-runtime object owns the named export.
// In-process consumers resolve `symbol_name` to `address()`; importing this
// module into a platform archive must not add a second symbol definition.
comptime {
    if (@sizeOf(Header) != 16 or @sizeOf(Region) != 8 or
        @sizeOf(Source) != 84 or @sizeOf(Literal) != 40 or
        @sizeOf(Root) != 164 or @sizeOf(Failure) != 216 or
        @sizeOf(Site) != 92 or @sizeOf(Descriptor) != 444 or
        @alignOf(Descriptor) != 4)
        @compileError("source event descriptor ABI v1 layout changed");
    if (@import("root") == @This()) {
        @export(&roc_source_event, .{ .name = symbol_name });
        @export(&roc_source_event_observer_enter, .{ .name = "roc_source_event_observer_enter" });
        @export(&roc_source_event_observer_leave, .{ .name = "roc_source_event_observer_leave" });
    }
}

pub fn address() usize {
    return @intFromPtr(&roc_source_event);
}

fn flag(value: u32) Invalid!void {
    if (value > 1) return error.InvalidFlag;
}

fn region(value: Region) Invalid!void {
    if (value.start > value.end) return error.InvalidRegion;
}

fn tag(comptime T: type, value: u32) ?T {
    inline for (@typeInfo(T).@"enum".fields) |field| {
        if (value == field.value) return @enumFromInt(value);
    }
    return null;
}

fn literal(value: Literal) Invalid!void {
    _ = tag(LiteralKind, value.kind) orelse return error.InvalidLiteral;
}

fn root(value: Root) Invalid!void {
    const producer = tag(ProducerKind, value.producer_kind) orelse return error.InvalidRoot;
    _ = tag(RootRole, value.role) orelse return error.InvalidRoot;
    if (producer == .literal) try literal(value.literal);
}

pub fn validate(header: *const Header) Invalid!Event {
    if (header.version != abi_version) return error.UnsupportedVersion;
    const kind = tag(Kind, header.kind) orelse return error.InvalidKind;
    if (header.reserved != 0) return error.InvalidReserved;
    if (header.byte_size < @sizeOf(Descriptor)) return error.InvalidSize;
    const descriptor: *const Descriptor = @ptrCast(header);
    try flag(descriptor.source.has_location);
    try region(descriptor.source.region);
    if (descriptor.source.has_location == 1 and
        (descriptor.source.line == 0 or descriptor.source.column == 0))
        return error.InvalidLocation;

    var branches: []align(1) const Region = &.{};
    switch (kind) {
        .branch_taken, .exhaustiveness_failed => {
            const site = descriptor.site;
            try flag(site.has_checked_site);
            _ = tag(SiteKind, site.kind) orelse return error.InvalidSite;
            try region(site.region);
            const tail_size = @as(u64, site.branch_count) * @sizeOf(Region);
            if (site.branch_count == 0) {
                if (site.branches_offset != 0 or header.byte_size != @sizeOf(Descriptor))
                    return error.InvalidSize;
            } else {
                if (site.branches_offset != @sizeOf(Descriptor) or
                    @as(u64, site.branches_offset) + tail_size != header.byte_size)
                    return error.InvalidSize;
                const bytes: [*]const u8 = @ptrCast(header);
                const records: [*]align(1) const Region = @ptrCast(bytes + site.branches_offset);
                branches = records[0..site.branch_count];
                for (branches) |branch| try region(branch);
            }
            if (kind == .branch_taken and descriptor.branch_index >= site.branch_count)
                return error.InvalidBranch;
        },
        .failure => {
            if (header.byte_size != @sizeOf(Descriptor)) return error.InvalidSize;
            try flag(descriptor.failure.checked_error);
            try flag(descriptor.failure.has_literal_rejection);
            try flag(descriptor.failure.has_guard_root);
            if (descriptor.failure.has_literal_rejection == 1)
                try literal(descriptor.failure.literal_rejection);
            if (descriptor.failure.has_guard_root == 1)
                try root(descriptor.failure.guard_root);
        },
        .call_enter, .call_exit => {
            if (header.byte_size != @sizeOf(Descriptor)) return error.InvalidSize;
        },
    }
    return .{ .kind = kind, .descriptor = descriptor, .branches = branches };
}

const Recorder = struct {
    count: usize = 0,
    invalid_count: usize = 0,
    last_kind: ?Kind = null,
    last_invalid: ?InvalidTag = null,

    fn observer(self: *Recorder) Observer {
        return .{ .context = self, .on_event = record, .on_invalid = invalid };
    }

    fn record(context: ?*anyopaque, header: *const Header) callconv(.c) void {
        const self: *Recorder = @ptrCast(@alignCast(context.?));
        const event = validate(header) catch unreachable;
        self.count += 1;
        self.last_kind = event.kind;
    }

    fn invalid(context: ?*anyopaque, raw_tag: u32) callconv(.c) void {
        const self: *Recorder = @ptrCast(@alignCast(context.?));
        self.invalid_count += 1;
        self.last_invalid = @enumFromInt(raw_tag);
    }
};

test "source events disabled execution does not inspect or record descriptors" {
    const saved = enter(null);
    defer leave(saved);
    try std.testing.expectEqual(@as(usize, 0), active_observers.load(.acquire));
    const malformed: Header = .{ .version = 99, .kind = 0, .byte_size = 0 };
    roc_source_event(&malformed);
    roc_source_event(@ptrFromInt(@alignOf(Header)));
}

test "source events registered execution and nested restoration" {
    var outer: Recorder = .{};
    var inner: Recorder = .{};
    const descriptor: Descriptor = .{ .header = .{ .kind = @intFromEnum(Kind.call_enter) } };
    const saved = enter(outer.observer());
    defer leave(saved);
    roc_source_event(&descriptor.header);
    const nested = enter(inner.observer());
    roc_source_event(&descriptor.header);
    const disabled = enter(null);
    roc_source_event(&descriptor.header);
    leave(disabled);
    roc_source_event(&descriptor.header);
    leave(nested);
    roc_source_event(&descriptor.header);
    try std.testing.expectEqual(@as(usize, 2), outer.count);
    try std.testing.expectEqual(@as(usize, 2), inner.count);
    try std.testing.expectEqual(Kind.call_enter, outer.last_kind.?);
}

test "source events capability count follows active thread transitions exactly" {
    var recorder: Recorder = .{};
    try std.testing.expectEqual(@as(usize, 0), active_observers.load(.acquire));
    {
        const outer = enter(recorder.observer());
        defer leave(outer);
        try std.testing.expectEqual(@as(usize, 1), active_observers.load(.acquire));
        {
            const nested = enter(recorder.observer());
            defer leave(nested);
            try std.testing.expectEqual(@as(usize, 1), active_observers.load(.acquire));
            {
                const disabled = enter(null);
                defer leave(disabled);
                try std.testing.expectEqual(@as(usize, 0), active_observers.load(.acquire));
                roc_source_event(@ptrFromInt(@alignOf(Header)));
            }
            try std.testing.expectEqual(@as(usize, 1), active_observers.load(.acquire));
        }
        try std.testing.expectEqual(@as(usize, 1), active_observers.load(.acquire));
    }
    try std.testing.expectEqual(@as(usize, 0), active_observers.load(.acquire));
}

test "source events registration is owned by the executing thread" {
    var owner: Recorder = .{};
    const saved = enter(owner.observer());
    defer leave(saved);
    const Worker = struct {
        const Counts = struct { during: usize = 0, after: usize = 0 };
        fn run(counts: *Counts) void {
            const descriptor: Descriptor = .{ .header = .{ .kind = @intFromEnum(Kind.call_exit) } };
            roc_source_event(&descriptor.header);
            var local: Recorder = .{};
            const nested = enter(local.observer());
            counts.during = active_observers.load(.acquire);
            roc_source_event(&descriptor.header);
            std.debug.assert(local.count == 1);
            leave(nested);
            counts.after = active_observers.load(.acquire);
        }
    };
    var counts: Worker.Counts = .{};
    const thread = try std.Thread.spawn(.{}, Worker.run, .{&counts});
    thread.join();
    try std.testing.expectEqual(@as(usize, 2), counts.during);
    try std.testing.expectEqual(@as(usize, 1), counts.after);
    try std.testing.expectEqual(@as(usize, 0), owner.count);
}

test "source events invalid descriptors report violations without delivering events" {
    var recorder: Recorder = .{};
    const saved = enter(recorder.observer());
    defer leave(saved);
    var descriptor: Descriptor = .{ .header = .{ .kind = @intFromEnum(Kind.failure) } };
    descriptor.header.version = 2;
    roc_source_event(&descriptor.header);
    try std.testing.expectEqual(InvalidTag.UnsupportedVersion, recorder.last_invalid.?);
    descriptor.header.version = abi_version;
    descriptor.header.byte_size = @sizeOf(Header);
    roc_source_event(&descriptor.header);
    try std.testing.expectEqual(InvalidTag.InvalidSize, recorder.last_invalid.?);
    descriptor.header.byte_size = @sizeOf(Descriptor);
    descriptor.failure.has_guard_root = 1;
    roc_source_event(&descriptor.header);
    try std.testing.expectEqual(InvalidTag.InvalidRoot, recorder.last_invalid.?);
    try std.testing.expectEqual(@as(usize, 0), recorder.count);
    try std.testing.expectEqual(@as(usize, 3), recorder.invalid_count);
}

test "source events semantic site tail and branch bounds" {
    const WithBranches = extern struct {
        descriptor: Descriptor,
        branches: [2]Region,
    };
    var record: WithBranches = .{
        .descriptor = .{
            .header = .{ .kind = @intFromEnum(Kind.branch_taken), .byte_size = @sizeOf(WithBranches) },
            .source = .{ .has_location = 1, .line = 7, .column = 9, .region = .{ .start = 12, .end = 24 } },
            .site = .{
                .has_checked_site = 1,
                .checked_site = 42,
                .kind = @intFromEnum(SiteKind.match),
                .branches_offset = @sizeOf(Descriptor),
                .branch_count = 2,
            },
            .branch_index = 1,
        },
        .branches = .{ .{ .start = 1, .end = 2 }, .{ .start = 3, .end = 4 } },
    };
    const event = try validate(&record.descriptor.header);
    try std.testing.expectEqual(@as(u32, 42), event.descriptor.site.checked_site);
    try std.testing.expectEqual(@as(u32, 3), event.branches[1].start);
    record.descriptor.branch_index = 2;
    try std.testing.expectError(error.InvalidBranch, validate(&record.descriptor.header));
    record.descriptor.header.kind = @intFromEnum(Kind.exhaustiveness_failed);
    _ = try validate(&record.descriptor.header);
    record.descriptor.site.branch_count = std.math.maxInt(u32);
    try std.testing.expectError(error.InvalidSize, validate(&record.descriptor.header));
}

test "source events fixed width ABI has no target pointer fields" {
    try std.testing.expectEqual(@as(usize, 16), @sizeOf(Header));
    try std.testing.expectEqual(@as(usize, 8), @sizeOf(Region));
    try std.testing.expectEqual(@as(usize, 84), @sizeOf(Source));
    try std.testing.expectEqual(@as(usize, 40), @sizeOf(Literal));
    try std.testing.expectEqual(@as(usize, 164), @sizeOf(Root));
    try std.testing.expectEqual(@as(usize, 216), @sizeOf(Failure));
    try std.testing.expectEqual(@as(usize, 92), @sizeOf(Site));
    try std.testing.expectEqual(@as(usize, 444), @sizeOf(Descriptor));
    try std.testing.expectEqual(@as(usize, 0), @offsetOf(Descriptor, "header"));
    try std.testing.expectEqual(@as(usize, 3 * @sizeOf(usize)), @sizeOf(Observer));
    try std.testing.expectEqual(@as(usize, 8 + 3 * @sizeOf(usize)), @sizeOf(Saved));
    try std.testing.expectEqual(@as(usize, 8), @offsetOf(Saved, "context"));
}

test "source events observer context may be null under the C registration ABI" {
    const Sink = struct {
        threadlocal var calls: usize = 0;
        fn record(context: ?*anyopaque, _: *const Header) callconv(.c) void {
            if (context != null) @trap();
            calls += 1;
        }
        fn invalid(_: ?*anyopaque, _: u32) callconv(.c) void {
            @trap();
        }
    };
    Sink.calls = 0;
    const observer: Observer = .{ .context = null, .on_event = Sink.record, .on_invalid = Sink.invalid };
    var saved: Saved = undefined;
    roc_source_event_observer_enter(&observer, &saved);
    defer roc_source_event_observer_leave(&saved);
    const descriptor: Descriptor = .{ .header = .{ .kind = @intFromEnum(Kind.call_exit) } };
    roc_source_event(&descriptor.header);
    try std.testing.expectEqual(@as(usize, 1), Sink.calls);
}

test "source events malformed version tags flags ranges and offsets" {
    var descriptor: Descriptor = .{ .header = .{ .kind = @intFromEnum(Kind.failure) } };
    descriptor.header.kind = 0;
    try std.testing.expectError(error.InvalidKind, validate(&descriptor.header));
    descriptor.header.kind = @intFromEnum(Kind.failure);
    descriptor.header.reserved = 1;
    try std.testing.expectError(error.InvalidReserved, validate(&descriptor.header));
    descriptor.header.reserved = 0;
    descriptor.source.has_location = 2;
    try std.testing.expectError(error.InvalidFlag, validate(&descriptor.header));
    descriptor.source.has_location = 1;
    try std.testing.expectError(error.InvalidLocation, validate(&descriptor.header));
    descriptor.source.line = 1;
    descriptor.source.column = 1;
    descriptor.source.region = .{ .start = 2, .end = 1 };
    try std.testing.expectError(error.InvalidRegion, validate(&descriptor.header));
    descriptor.source.region = .{};
    descriptor.failure.checked_error = 2;
    try std.testing.expectError(error.InvalidFlag, validate(&descriptor.header));
    descriptor.failure.checked_error = 0;
    descriptor.failure.has_literal_rejection = 2;
    try std.testing.expectError(error.InvalidFlag, validate(&descriptor.header));
    descriptor.failure.has_literal_rejection = 1;
    try std.testing.expectError(error.InvalidLiteral, validate(&descriptor.header));
    descriptor.failure.literal_rejection.kind = @intFromEnum(LiteralKind.quote);
    descriptor.failure.has_guard_root = 2;
    try std.testing.expectError(error.InvalidFlag, validate(&descriptor.header));
    descriptor.failure.has_guard_root = 1;
    descriptor.failure.guard_root.producer_kind = @intFromEnum(ProducerKind.checked);
    try std.testing.expectError(error.InvalidRoot, validate(&descriptor.header));
    descriptor.failure.guard_root.role = @intFromEnum(RootRole.value);
    descriptor.failure.guard_root.producer_kind = @intFromEnum(ProducerKind.literal);
    try std.testing.expectError(error.InvalidLiteral, validate(&descriptor.header));

    descriptor.header.kind = @intFromEnum(Kind.exhaustiveness_failed);
    try std.testing.expectError(error.InvalidSite, validate(&descriptor.header));
    descriptor.site.kind = @intFromEnum(SiteKind.if_);
    descriptor.site.has_checked_site = 2;
    try std.testing.expectError(error.InvalidFlag, validate(&descriptor.header));
    descriptor.site.has_checked_site = 0;
    descriptor.site.region = .{ .start = 2, .end = 1 };
    try std.testing.expectError(error.InvalidRegion, validate(&descriptor.header));
    descriptor.site.region = .{};
    descriptor.site.branches_offset = @sizeOf(Descriptor);
    try std.testing.expectError(error.InvalidSize, validate(&descriptor.header));
    descriptor.site.branch_count = 1;
    descriptor.site.branches_offset = @sizeOf(Descriptor) - 4;
    try std.testing.expectError(error.InvalidSize, validate(&descriptor.header));
    descriptor.site.branches_offset = std.math.maxInt(u32);
    try std.testing.expectError(error.InvalidSize, validate(&descriptor.header));
    descriptor.site.branch_count = 0;
    descriptor.site.branches_offset = 0;
    _ = try validate(&descriptor.header);
    descriptor.header.kind = @intFromEnum(Kind.branch_taken);
    try std.testing.expectError(error.InvalidBranch, validate(&descriptor.header));
}

test "source events preserve failure literal guard and original source identities" {
    const descriptor: Descriptor = .{
        .header = .{ .kind = @intFromEnum(Kind.failure) },
        .source = .{
            .checked_module = @splat(1),
            .source_identity = @splat(2),
            .region = .{ .start = 9, .end = 27 },
            .line = 3,
            .column = 7,
            .has_location = 1,
        },
        .procedure_identity = @splat(3),
        .failure = .{
            .checked_error = 1,
            .has_literal_rejection = 1,
            .literal_rejection = .{
                .checked_module = @splat(4),
                .checked_expr = 19,
                .kind = @intFromEnum(LiteralKind.numeral),
            },
            .has_guard_root = 1,
            .guard_root = .{
                .module = @splat(5),
                .producer_kind = @intFromEnum(ProducerKind.literal),
                .literal = .{
                    .checked_module = @splat(6),
                    .checked_expr = 29,
                    .kind = @intFromEnum(LiteralKind.quote),
                },
                .procedure_identity = @splat(7),
                .layout_digest = @splat(8),
                .role = @intFromEnum(RootRole.failure_message),
                .failed_field = 11,
                .message_field = 13,
                .failed_offset = 17,
                .message_offset = 23,
            },
        },
    };
    const event = try validate(&descriptor.header);
    try std.testing.expectEqual(&descriptor, event.descriptor);
    try std.testing.expectEqualDeep(descriptor.source, event.descriptor.source);
    try std.testing.expectEqualDeep(descriptor.failure, event.descriptor.failure);
}

test "source events reject invalid tail regions and trailing bytes" {
    const WithBranch = extern struct { descriptor: Descriptor, branch: Region };
    var record: WithBranch = .{
        .descriptor = .{
            .header = .{ .kind = @intFromEnum(Kind.branch_taken), .byte_size = @sizeOf(WithBranch) },
            .site = .{
                .kind = @intFromEnum(SiteKind.destructure),
                .branches_offset = @sizeOf(Descriptor),
                .branch_count = 1,
            },
        },
        .branch = .{ .start = 9, .end = 8 },
    };
    try std.testing.expectError(error.InvalidRegion, validate(&record.descriptor.header));
    record.branch.end = 10;
    _ = try validate(&record.descriptor.header);
    record.descriptor.header.byte_size += 1;
    try std.testing.expectError(error.InvalidSize, validate(&record.descriptor.header));
    record.descriptor.header.kind = @intFromEnum(Kind.call_exit);
    try std.testing.expectError(error.InvalidSize, validate(&record.descriptor.header));
}
