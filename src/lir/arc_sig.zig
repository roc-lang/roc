//! Per-proc reference-count ownership signatures used by ARC insertion.
//!
//! A signature records, for one proc, which refcounted argument positions are
//! borrowed (the caller keeps ownership and the callee emits no RC statements
//! for them) versus owned (the caller transfers exactly one ownership unit per
//! position), plus the ownership mode of the return position. Signatures are
//! ARC-stage-local: they are solved before RC statement emission, consumed by
//! emission and by the debug borrow certifier, and dropped when ARC insertion
//! finishes. They never appear in LirImage or any later stage.

const std = @import("std");
const core = @import("lir_core");

const LIR = core.LIR;

/// Ownership mode of one refcounted position.
pub const Mode = enum(u1) {
    borrowed,
    owned,
};

/// Solved ownership signature of one proc.
///
/// Argument positions are indexed by position in the proc's `args` span.
/// Positions at or beyond 64 are always owned. Non-refcounted positions are
/// reported as owned; their mode is never consulted.
pub const RcSig = struct {
    /// Bit i set means argument position i is borrowed.
    borrowed_params: u64 = 0,
    ret_mode: Mode = .owned,
    /// For a borrowed return, bit i set means the result may borrow from
    /// argument position i. Unused when `ret_mode` is owned.
    ret_lenders: u64 = 0,
    /// The returned value's outermost allocation has count 1 on return:
    /// every `ret` in the proc returns a born-unique value that survives to
    /// the return with no other holder, so the return is the value's single
    /// consuming use. Pinned signatures never claim a unique return.
    ret_unique: bool = false,
    /// Bit i set means argument position i is treated as born-unique inside
    /// the proc body: the call site proved its dying argument unique, so
    /// runtime uniqueness checks that consume the parameter go check-free.
    /// Only mode-specialized variants carry these bits; solved base
    /// signatures and pinned signatures are always zero.
    unique_params: u64 = 0,

    pub const all_owned: RcSig = .{};

    pub fn paramMode(self: RcSig, index: usize) Mode {
        if (index >= 64) return .owned;
        const bit = @as(u64, 1) << @as(u6, @intCast(index));
        return if ((self.borrowed_params & bit) != 0) .borrowed else .owned;
    }

    pub fn withBorrowedParam(self: RcSig, index: usize) RcSig {
        if (index >= 64) return self;
        var updated = self;
        updated.borrowed_params |= @as(u64, 1) << @as(u6, @intCast(index));
        return updated;
    }
};

/// Signature lookup for every proc in one LIR store. Procs without a solved
/// entry are all-owned, which is always a sound signature.
pub const SigTable = struct {
    sigs: []const RcSig = &.{},

    pub const all_owned: SigTable = .{};

    pub fn get(self: SigTable, proc: LIR.LirProcSpecId) RcSig {
        const idx = @intFromEnum(proc);
        if (idx >= self.sigs.len) return RcSig.all_owned;
        return self.sigs[idx];
    }
};

test "all-owned signature reports owned for every position" {
    const sig = RcSig.all_owned;
    try std.testing.expectEqual(Mode.owned, sig.paramMode(0));
    try std.testing.expectEqual(Mode.owned, sig.paramMode(63));
    try std.testing.expectEqual(Mode.owned, sig.paramMode(200));
    try std.testing.expectEqual(Mode.owned, sig.ret_mode);
    try std.testing.expectEqual(false, sig.ret_unique);
    try std.testing.expectEqual(@as(u64, 0), sig.unique_params);
}

test "borrowed param bits round-trip" {
    const sig = RcSig.all_owned.withBorrowedParam(0).withBorrowedParam(3);
    try std.testing.expectEqual(Mode.borrowed, sig.paramMode(0));
    try std.testing.expectEqual(Mode.owned, sig.paramMode(1));
    try std.testing.expectEqual(Mode.borrowed, sig.paramMode(3));
    try std.testing.expectEqual(Mode.owned, sig.paramMode(64));
}

test "empty signature table answers all-owned" {
    const table = SigTable.all_owned;
    const sig = table.get(@enumFromInt(7));
    try std.testing.expectEqual(@as(u64, 0), sig.borrowed_params);
    try std.testing.expectEqual(Mode.owned, sig.ret_mode);
    try std.testing.expectEqual(false, sig.ret_unique);
    try std.testing.expectEqual(@as(u64, 0), sig.unique_params);
}
