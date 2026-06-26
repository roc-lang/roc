//! Host-agnostic HostValue adapter shared by both Signals hosts.
//!
//! `host_value_registry.zig` owns the handle table; this module owns the thin
//! layer above it that both hosts share: capability retain/release/equality,
//! capability-driven splitting, and the boxed value constructors. The pieces
//! that genuinely differ per host — which allocator/registry/`roc_host` to use,
//! and native-only test-kind bookkeeping — are supplied by the caller.

const abi = @import("roc_platform_abi.zig");

pub const HostValue = u64;
pub const HostValueCapabilityHandle = abi.HostValueCapabilityHandle;

/// Carrier-type category recorded for a freshly boxed value. The native host
/// uses it for debug type assertions; the browser host ignores it.
pub const ValueKind = enum { unit, str, bool, i64 };

/// Signals represents signal tokens and binder tokens as boxed `U64` payloads
/// in Roc. On wasm32 their payload alignment is 8 while pointer width is 4, so
/// releasing them must not use the pointer-aligned `decrefBox` convenience
/// helper.
pub fn releaseU64Box(box: anytype, roc_host: *abi.RocHost) void {
    abi.decrefBoxWith(@ptrCast(box), @alignOf(u64), null, roc_host);
}

pub fn retainHostValueCapability(capability: HostValueCapabilityHandle) HostValueCapabilityHandle {
    abi.increfHostValueCapabilityHandle(capability, 1);
    return capability;
}

pub fn releaseHostValueCapability(capability: HostValueCapabilityHandle, roc_host: *abi.RocHost) void {
    abi.decrefHostValueCapabilityHandle(capability, roc_host);
}

pub fn hostValueCapabilityId(capability: HostValueCapabilityHandle) usize {
    return @intFromPtr(capability.split);
}

pub fn hostValueCapabilityEq(capability: HostValueCapabilityHandle) abi.RocErasedCallable {
    return capability.eq;
}

pub fn hostValueCapabilitySplit(capability: HostValueCapabilityHandle) abi.RocErasedCallable {
    return capability.split;
}

pub fn hostValueCapabilityDrop(capability: HostValueCapabilityHandle) abi.RocErasedCallable {
    return capability.drop;
}

pub fn hostValueCapabilityEqFn(capability: HostValueCapabilityHandle) ?abi.RocErasedCallableFn {
    const eq = hostValueCapabilityEq(capability) orelse return null;
    return abi.rocErasedCallablePayloadPtr(eq).callable_fn_ptr;
}

pub fn hostValueCapabilitySplitFn(capability: HostValueCapabilityHandle) ?abi.RocErasedCallableFn {
    const split = hostValueCapabilitySplit(capability) orelse return null;
    return abi.rocErasedCallablePayloadPtr(split).callable_fn_ptr;
}

pub fn hostValueCapabilityDropFn(capability: HostValueCapabilityHandle) ?abi.RocErasedCallableFn {
    const drop = hostValueCapabilityDrop(capability) orelse return null;
    return abi.rocErasedCallablePayloadPtr(drop).callable_fn_ptr;
}

pub fn hostValueCapabilitiesMatch(actual: HostValueCapabilityHandle, expected: HostValueCapabilityHandle) bool {
    return actual.split == expected.split and actual.eq == expected.eq and actual.drop == expected.drop;
}

/// Registry-ops adapter passed to `host_value_registry.Registry` calls. It
/// refcounts boxes and capabilities through one active `roc_host`.
pub fn RegistryOps() type {
    return struct {
        roc_host: *abi.RocHost,

        pub fn retainCapability(_: @This(), capability: HostValueCapabilityHandle) void {
            abi.increfHostValueCapabilityHandle(capability, 1);
        }

        pub fn releaseCapability(self: @This(), capability: HostValueCapabilityHandle) void {
            releaseHostValueCapability(capability, self.roc_host);
        }

        pub fn capabilitiesMatch(_: @This(), actual: HostValueCapabilityHandle, expected: HostValueCapabilityHandle) bool {
            return hostValueCapabilitiesMatch(actual, expected);
        }

        pub fn capabilityMatchesSplit(_: @This(), actual: HostValueCapabilityHandle, expected_split: abi.RocErasedCallable) bool {
            return hostValueCapabilitySplit(actual) == expected_split;
        }

        pub fn splitBoxWithCapability(self: @This(), box: abi.RocBox, capability: HostValueCapabilityHandle) @import("erased_calls.zig").RocBoxPair {
            return @import("erased_calls.zig").callErasedRocBoxToRocBoxPair(self.roc_host, hostValueCapabilitySplit(capability), box);
        }

        pub fn splitBoxWithSplit(self: @This(), box: abi.RocBox, split: abi.RocErasedCallable) @import("erased_calls.zig").RocBoxPair {
            return @import("erased_calls.zig").callErasedRocBoxToRocBoxPair(self.roc_host, split, box);
        }
    };
}

// Boxed-value constructors. `ctx` must expose:
//   - `store(box: abi.RocBox) HostValue` — store a value that will receive a
//     capability before typed access.
//   - `storeWithCapability(box: abi.RocBox, cap: HostValueCapabilityHandle)
//     HostValue` — store a value with a retained capability.
//   - `recordKind(value: HostValue, kind: ValueKind) void` — native test-kind
//     instrumentation; a no-op on the browser host.
// `roc_host` is threaded explicitly so the box allocation matches the active
// host.

pub fn makeUnit(ctx: anytype, roc_host: *abi.RocHost) HostValue {
    const payload = abi.allocateBox(0, @alignOf(u8), false, roc_host);
    const value = ctx.store(@ptrCast(payload));
    ctx.recordKind(value, .unit);
    return value;
}

pub fn makeUnitWithCapability(ctx: anytype, roc_host: *abi.RocHost, cap: HostValueCapabilityHandle) HostValue {
    const payload = abi.allocateBox(0, @alignOf(u8), false, roc_host);
    const value = ctx.storeWithCapability(@ptrCast(payload), cap);
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

pub fn makeStrWithCapability(ctx: anytype, roc_host: *abi.RocHost, bytes: []const u8, cap: HostValueCapabilityHandle) HostValue {
    const payload: *abi.RocStr = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(abi.RocStr), @alignOf(abi.RocStr), true, roc_host)));
    payload.* = abi.RocStr.fromSlice(bytes, roc_host);
    const value = ctx.storeWithCapability(@ptrCast(payload), cap);
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

pub fn makeBoolWithCapability(ctx: anytype, roc_host: *abi.RocHost, b: bool, cap: HostValueCapabilityHandle) HostValue {
    const payload: *bool = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(bool), @alignOf(bool), false, roc_host)));
    payload.* = b;
    const value = ctx.storeWithCapability(@ptrCast(payload), cap);
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

pub fn makeI64WithCapability(ctx: anytype, roc_host: *abi.RocHost, n: i64, cap: HostValueCapabilityHandle) HostValue {
    const payload: *i64 = @ptrCast(@alignCast(abi.allocateBox(@sizeOf(i64), @alignOf(i64), false, roc_host)));
    payload.* = n;
    const value = ctx.storeWithCapability(@ptrCast(payload), cap);
    ctx.recordKind(value, .i64);
    return value;
}
