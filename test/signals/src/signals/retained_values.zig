const abi = @import("roc_platform_abi.zig");
const erased_calls = @import("erased_calls.zig");
const hv = @import("host_values.zig");

pub const HostValue = u64;
pub const HostValueCapability = hv.HostValueCapabilityHandle;
pub const HostTextRead = abi.HostValueTextReadHandle;
pub const HostBoolRead = abi.HostValueBoolReadHandle;
pub const HostEventReducer = abi.HostValueEventReducerHandle;
pub const HostTaskRequestRead = abi.HostValueTaskRequestReadHandle;
pub const HostEachOps = abi.__AnonStruct58;
pub const HostSignalToken = *u64;

/// A retained Roc value plus the capability that owns its equality/drop
/// operations. Holds exactly one refcount on the capability while live.
pub const HostValueCell = struct {
    value: HostValue,
    cap: HostValueCapability,

    pub fn initRetained(value: HostValue, cap: HostValueCapability, metrics: anytype) HostValueCell {
        _ = retainHostValueCapability(cap, metrics);
        return .{ .value = value, .cap = cap };
    }

    /// Clone the cell, retaining the capability and cloning the boxed value through
    /// `ctx.cloneHostValue` (which the host implements with its registry).
    pub fn cloneRetained(self: HostValueCell, ctx: anytype, metrics: anytype) HostValueCell {
        const value = ctx.cloneHostValue(self.value);
        _ = retainHostValueCapability(self.cap, metrics);
        return .{ .value = value, .cap = self.cap };
    }

    pub fn deinit(self: *HostValueCell, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
        const caps = [_]HostValueCapability{self.cap};
        ctx.pushHostValueCapabilities(&caps);
        defer ctx.popHostValueCapabilities();
        erased_calls.callErasedHostValueToUnit(roc_host, hv.hostValueCapabilityDrop(self.cap), self.value);
        releaseHostValueCapability(self.cap, roc_host, metrics);
        self.* = undefined;
    }

    pub fn valueEquals(self: *const HostValueCell, ctx: anytype, roc_host: *abi.RocHost, value: HostValue) bool {
        const caps = [_]HostValueCapability{self.cap};
        ctx.pushHostValueCapabilities(&caps);
        defer ctx.popHostValueCapabilities();
        return erased_calls.callErasedHostValueHostValueToBool(roc_host, hv.hostValueCapabilityEq(self.cap), self.value, value);
    }

    pub fn dropIncoming(self: *const HostValueCell, ctx: anytype, roc_host: *abi.RocHost, value: HostValue) void {
        const caps = [_]HostValueCapability{self.cap};
        ctx.pushHostValueCapabilities(&caps);
        defer ctx.popHostValueCapabilities();
        erased_calls.callErasedHostValueToUnit(roc_host, hv.hostValueCapabilityDrop(self.cap), value);
    }

    pub fn replaceValue(self: *HostValueCell, ctx: anytype, roc_host: *abi.RocHost, value: HostValue) void {
        const caps = [_]HostValueCapability{self.cap};
        ctx.pushHostValueCapabilities(&caps);
        defer ctx.popHostValueCapabilities();
        erased_calls.callErasedHostValueToUnit(roc_host, hv.hostValueCapabilityDrop(self.cap), self.value);
        self.value = value;
    }

    pub fn replaceRetained(self: *HostValueCell, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype, value: HostValue, cap: HostValueCapability) void {
        const old_cap = self.cap;
        _ = retainHostValueCapability(cap, metrics);
        const caps = [_]HostValueCapability{old_cap};
        ctx.pushHostValueCapabilities(&caps);
        defer ctx.popHostValueCapabilities();
        erased_calls.callErasedHostValueToUnit(roc_host, hv.hostValueCapabilityDrop(old_cap), self.value);
        releaseHostValueCapability(old_cap, roc_host, metrics);
        self.* = .{ .value = value, .cap = cap };
    }
};

/// Retain one refcount on a Roc thunk the host is about to store.
pub fn retainHostCallable(callable: abi.RocErasedCallable, metrics: anytype) abi.RocErasedCallable {
    abi.increfErasedCallable(callable, 1);
    metrics.bump(.closure_retains, 1);
    return callable;
}

pub fn retainHostSignalToken(token: HostSignalToken) HostSignalToken {
    abi.increfBox(@ptrCast(token), 1);
    return token;
}

pub fn releaseHostSignalToken(token: HostSignalToken, roc_host: *abi.RocHost) void {
    hv.releaseU64Box(token, roc_host);
}

pub fn retainHostValueCapability(capability: HostValueCapability, metrics: anytype) HostValueCapability {
    metrics.bump(.closure_retains, 3);
    return hv.retainHostValueCapability(capability);
}

pub fn releaseHostValueCapability(capability: HostValueCapability, roc_host: *abi.RocHost, metrics: anytype) void {
    metrics.bump(.closure_releases, 3);
    hv.releaseHostValueCapability(capability, roc_host);
}

pub fn assertHostValueCapabilitiesMatch(actual: HostValueCapability, expected: HostValueCapability, message: []const u8) void {
    if (!hv.hostValueCapabilitiesMatch(actual, expected)) @panic(message);
}

pub fn retainHostTextRead(read: HostTextRead, metrics: anytype) HostTextRead {
    _ = retainHostValueCapability(read.capability, metrics);
    abi.increfErasedCallable(read.read, 1);
    metrics.bump(.closure_retains, 1);
    return read;
}

pub fn releaseHostTextRead(read: HostTextRead, roc_host: *abi.RocHost, metrics: anytype) void {
    releaseHostValueCapability(read.capability, roc_host, metrics);
    abi.decrefErasedCallable(read.read, roc_host);
    metrics.bump(.closure_releases, 1);
}

pub fn retainHostBoolRead(read: HostBoolRead, metrics: anytype) HostBoolRead {
    _ = retainHostValueCapability(read.capability, metrics);
    abi.increfErasedCallable(read.read, 1);
    metrics.bump(.closure_retains, 1);
    return read;
}

pub fn releaseHostBoolRead(read: HostBoolRead, roc_host: *abi.RocHost, metrics: anytype) void {
    releaseHostValueCapability(read.capability, roc_host, metrics);
    abi.decrefErasedCallable(read.read, roc_host);
    metrics.bump(.closure_releases, 1);
}

pub fn retainHostEventReducer(reducer: HostEventReducer, metrics: anytype) HostEventReducer {
    _ = retainHostValueCapability(reducer.capability, metrics);
    abi.increfErasedCallable(reducer.transform, 1);
    metrics.bump(.closure_retains, 1);
    return reducer;
}

pub fn releaseHostEventReducer(reducer: HostEventReducer, roc_host: *abi.RocHost, metrics: anytype) void {
    releaseHostValueCapability(reducer.capability, roc_host, metrics);
    abi.decrefErasedCallable(reducer.transform, roc_host);
    metrics.bump(.closure_releases, 1);
}

pub fn retainHostEachOps(ops: HostEachOps, metrics: anytype) HostEachOps {
    _ = retainHostValueCapability(ops.items_capability, metrics);
    _ = retainHostValueCapability(ops.item_capability, metrics);
    _ = retainHostValueCapability(ops.key_capability, metrics);
    abi.increfErasedCallable(ops.items_to_values, 1);
    abi.increfErasedCallable(ops.key_text, 1);
    abi.increfErasedCallable(ops.key_of, 1);
    abi.increfErasedCallable(ops.row, 1);
    metrics.bump(.closure_retains, 4);
    return ops;
}

pub fn releaseHostEachOps(ops: HostEachOps, roc_host: *abi.RocHost, metrics: anytype) void {
    releaseHostValueCapability(ops.items_capability, roc_host, metrics);
    releaseHostValueCapability(ops.item_capability, roc_host, metrics);
    releaseHostValueCapability(ops.key_capability, roc_host, metrics);
    abi.decrefErasedCallable(ops.items_to_values, roc_host);
    abi.decrefErasedCallable(ops.key_text, roc_host);
    abi.decrefErasedCallable(ops.key_of, roc_host);
    abi.decrefErasedCallable(ops.row, roc_host);
    metrics.bump(.closure_releases, 4);
}
