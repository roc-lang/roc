//! Host-agnostic HostValue adapter shared by both Signals hosts.
//!
//! `host_value_registry.zig` owns the handle table; this module owns the thin
//! layer above it that both hosts duplicated: the registry-ops adapter (how a
//! retained box/tag is refcounted through the active Roc host) and the boxed
//! value constructors. The pieces that genuinely differ per host — which
//! allocator/registry/`roc_host` to use, and the native-only test-kind
//! bookkeeping — are supplied by the caller through an explicit `roc_host` and a
//! small constructor context. See `DESIGN.md` (one engine, two thin hosts).

const abi = @import("roc_platform_abi.zig");

pub const HostValue = u64;
pub const HostValueTypeTag = abi.__AnonStruct19;

/// Carrier-type category recorded for a freshly boxed value. The native host
/// uses it for debug type-tag assertions; the browser host ignores it.
pub const ValueKind = enum { unit, str, bool, i64 };

/// Signals represents signal tokens and binder tokens as boxed `U64` payloads
/// in Roc. On wasm32 their payload alignment is 8 while pointer width is 4, so
/// releasing them must not use the pointer-aligned `decrefBox` convenience
/// helper.
pub fn releaseU64Box(box: anytype, roc_host: *abi.RocHost) void {
    abi.decrefBoxWith(@ptrCast(box), @alignOf(u64), null, roc_host);
}

pub fn normalizeHostValueTypeTag(tag: anytype) HostValueTypeTag {
    return .{
        .id = tag.id,
        .split = tag.split,
    };
}

pub fn retainHostValueTypeTag(tag: HostValueTypeTag) HostValueTypeTag {
    abi.incref__AnonStruct19(tag, 1);
    return tag;
}

pub fn releaseHostValueTypeTag(tag: HostValueTypeTag, roc_host: *abi.RocHost) void {
    abi.decref__AnonStruct19(tag, roc_host);
}

pub fn hostValueTypeTagId(tag: HostValueTypeTag) u64 {
    return tag.id;
}

pub fn hostValueTypeTagSplit(tag: HostValueTypeTag) abi.RocErasedCallable {
    return tag.split;
}

pub fn hostValueTypeTagSplitFn(tag: HostValueTypeTag) ?abi.RocErasedCallableFn {
    const split = hostValueTypeTagSplit(tag) orelse return null;
    return abi.rocErasedCallablePayloadPtr(split).callable_fn_ptr;
}

/// Registry-ops adapter passed to `host_value_registry.Registry` calls. It
/// refcounts boxes and tags through one `roc_host`.
pub fn RegistryOps(comptime TypeTag: type) type {
    return struct {
        roc_host: *abi.RocHost,

        pub fn retainTag(_: @This(), tag: TypeTag) void {
            abi.incref__AnonStruct19(tag, 1);
        }

        pub fn releaseTag(self: @This(), tag: TypeTag) void {
            releaseHostValueTypeTag(tag, self.roc_host);
        }

        pub fn tagId(_: @This(), tag: TypeTag) u64 {
            return hostValueTypeTagId(tag);
        }

        pub fn tagsMatch(_: @This(), actual_tag: TypeTag, expected_tag: TypeTag) bool {
            const actual_id = hostValueTypeTagId(actual_tag);
            if (actual_id != 0 and actual_id == hostValueTypeTagId(expected_tag)) return true;
            const actual_split = hostValueTypeTagSplit(actual_tag) orelse return false;
            const expected_split = hostValueTypeTagSplit(expected_tag) orelse return false;
            return actual_split == expected_split;
        }

        pub fn splitBox(self: @This(), box: abi.RocBox, tag: TypeTag) @import("erased_calls.zig").RocBoxPair {
            return @import("erased_calls.zig").callErasedRocBoxToRocBoxPair(self.roc_host, hostValueTypeTagSplit(tag), box);
        }
    };
}

// Boxed-value constructors. `ctx` must expose:
//   - `store(box: abi.RocBox) HostValue` — the host's registry store, including
//     any native test-kind slot bookkeeping (and registry-error handling).
//   - `recordKind(value: HostValue, kind: ValueKind) void` — native test-kind
//     instrumentation; a no-op on the browser host.
// `roc_host` is threaded explicitly so the box allocation matches the active
// host (mirroring how the erased-call adapters take `roc_host`).

pub fn makeUnit(ctx: anytype, roc_host: *abi.RocHost) HostValue {
    const payload = abi.allocateBox(0, @alignOf(u8), false, roc_host);
    const value = ctx.store(@ptrCast(payload));
    ctx.recordKind(value, .unit);
    return value;
}

pub fn makeStr(ctx: anytype, roc_host: *abi.RocHost, bytes: []const u8) HostValue {
    const payload: *abi.RocStr = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(abi.RocStr), @alignOf(abi.RocStr), true, roc_host)));
    payload.* = abi.RocStr.fromSlice(bytes, roc_host);
    const value = ctx.store(@ptrCast(payload));
    ctx.recordKind(value, .str);
    return value;
}

pub fn makeBool(ctx: anytype, roc_host: *abi.RocHost, b: bool) HostValue {
    const payload: *bool = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(bool), @alignOf(bool), false, roc_host)));
    payload.* = b;
    const value = ctx.store(@ptrCast(payload));
    ctx.recordKind(value, .bool);
    return value;
}

pub fn makeI64(ctx: anytype, roc_host: *abi.RocHost, n: i64) HostValue {
    const payload: *i64 = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(i64), @alignOf(i64), false, roc_host)));
    payload.* = n;
    const value = ctx.store(@ptrCast(payload));
    ctx.recordKind(value, .i64);
    return value;
}
