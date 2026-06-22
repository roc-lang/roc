//! Host-agnostic HostValue adapter shared by both Signals hosts.
//!
//! `host_value_registry.zig` owns the handle table; this module owns the thin
//! layer above it that both hosts duplicated: the registry-ops adapter (how a
//! retained box/tag is refcounted through the active Roc host) and the boxed
//! value constructors. The pieces that genuinely differ per host — which
//! allocator/registry/`roc_host` to use, and the native-only test-kind
//! bookkeeping — are supplied by the caller through an explicit `roc_host` and a
//! small duck-typed `ctx`. See `BROWSER_RUNTIME_DESIGN.md` O9 / `NEXT_STEPS.md`
//! G-B0.

const std = @import("std");
const abi = @import("roc_platform_abi.zig");

pub const HostValue = u64;

/// Carrier-type category recorded for a freshly boxed value. The native host
/// uses it for debug type-tag assertions; the browser host ignores it.
pub const ValueKind = enum { unit, str, bool, i64 };

/// Registry-ops adapter passed to `host_value_registry.Registry` calls. It
/// refcounts boxes and tags through one `roc_host`. The two hosts use different
/// `TypeTag` representations (`*anyopaque` vs `*u64`), so the tag parameters are
/// duck-typed; `tagId` reads the leading `u64` id, which is valid for both.
pub const RegistryOps = struct {
    roc_host: *abi.RocHost,

    pub fn retainBox(_: RegistryOps, box: abi.RocBox) void {
        abi.increfBox(box, 1);
    }

    pub fn releaseBox(self: RegistryOps, box: abi.RocBox) void {
        abi.decrefBox(box, self.roc_host);
    }

    pub fn retainTag(_: RegistryOps, tag: anytype) void {
        abi.increfBox(@ptrCast(tag), 1);
    }

    pub fn releaseTag(self: RegistryOps, tag: anytype) void {
        abi.decrefBox(@ptrCast(tag), self.roc_host);
    }

    pub fn tagId(_: RegistryOps, tag: anytype) u64 {
        const payload: *const u64 = @ptrCast(@alignCast(tag));
        return payload.*;
    }
};

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
