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

/// Parameter positions represented in inferred signatures and mode demands.
/// Later positions follow the exact all-owned schedule.
pub const ParamMask = u16;
/// Number of procedure argument positions represented by `ParamMask`.
pub const tracked_param_count = @bitSizeOf(ParamMask);

/// Returns the represented bit for one procedure argument position.
pub fn paramBit(index: usize) ?ParamMask {
    if (index >= tracked_param_count) return null;
    return @as(ParamMask, 1) << @as(u4, @intCast(index));
}

/// Ownership mode of one refcounted position.
pub const Mode = enum(u1) {
    borrowed,
    owned,
};

/// One normally returned top-level tag outcome of a direct procedure.
///
/// `restituted_params` names owned argument positions whose exact entry
/// ownership units are present again after this discriminant. Rows are
/// ARC-stage-local calling-convention facts; no runtime representation is
/// added to the source result.
pub const Outcome = struct {
    discriminant: u16,
    restituted_params: ParamMask,
};

/// Span of sorted, complete `Outcome` rows in `SigTable.outcomes`.
pub const OutcomeSpan = extern struct {
    start: u32 = 0,
    len: u32 = 0,

    pub const empty: OutcomeSpan = .{};

    pub fn isEmpty(self: OutcomeSpan) bool {
        return self.len == 0;
    }
};

/// Solved ownership signature of one proc.
///
/// Argument positions are indexed by position in the proc's `args` span.
/// Positions at or beyond `tracked_param_count` are always owned.
/// Non-refcounted positions are reported as owned; their mode is never
/// consulted.
pub const RcSig = struct {
    /// Bit i set means argument position i is borrowed.
    borrowed_params: ParamMask = 0,
    ret_mode: Mode = .owned,
    /// For a borrowed return, bit i set means the result may borrow from
    /// argument position i. Unused when `ret_mode` is owned.
    ret_lenders: ParamMask = 0,
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
    unique_params: ParamMask = 0,
    /// Complete normally-returned discriminants and their exact argument
    /// restitution masks. Empty means the proc has no conditional ownership
    /// convention.
    outcomes: OutcomeSpan = .empty,

    pub const all_owned: RcSig = .{};

    pub fn paramMode(self: RcSig, index: usize) Mode {
        const bit = paramBit(index) orelse return .owned;
        return if ((self.borrowed_params & bit) != 0) .borrowed else .owned;
    }

    pub fn withBorrowedParam(self: RcSig, index: usize) RcSig {
        const bit = paramBit(index) orelse return self;
        var updated = self;
        updated.borrowed_params |= bit;
        return updated;
    }
};

/// Signature lookup for every proc in one LIR store. Procs without a solved
/// entry are all-owned, which is always a sound signature.
pub const SigTable = struct {
    sigs: []const RcSig = &.{},
    outcomes: []const Outcome = &.{},

    pub const all_owned: SigTable = .{};

    pub fn get(self: SigTable, proc: LIR.LirProcSpecId) RcSig {
        const idx = @intFromEnum(proc);
        if (idx >= self.sigs.len) return RcSig.all_owned;
        return self.sigs[idx];
    }

    pub fn outcomesOf(self: SigTable, sig: RcSig) []const Outcome {
        const start: usize = @intCast(sig.outcomes.start);
        const len: usize = @intCast(sig.outcomes.len);
        if (start > self.outcomes.len or len > self.outcomes.len - start) {
            if (@import("builtin").mode == .Debug) {
                std.debug.panic("ARC signature outcome span exceeded its table", .{});
            }
            unreachable;
        }
        return self.outcomes[start..][0..len];
    }

    pub fn outcomesForProc(self: SigTable, proc: LIR.LirProcSpecId) []const Outcome {
        return self.outcomesOf(self.get(proc));
    }
};

test "all-owned signature reports owned for every position" {
    const sig = RcSig.all_owned;
    try std.testing.expectEqual(Mode.owned, sig.paramMode(0));
    try std.testing.expectEqual(Mode.owned, sig.paramMode(15));
    try std.testing.expectEqual(Mode.owned, sig.paramMode(16));
    try std.testing.expectEqual(Mode.owned, sig.paramMode(200));
    try std.testing.expectEqual(Mode.owned, sig.ret_mode);
    try std.testing.expectEqual(false, sig.ret_unique);
    try std.testing.expectEqual(@as(ParamMask, 0), sig.unique_params);
    try std.testing.expect(sig.outcomes.isEmpty());
}

test "borrowed param bits round-trip" {
    const sig = RcSig.all_owned.withBorrowedParam(0).withBorrowedParam(3).withBorrowedParam(15).withBorrowedParam(16);
    try std.testing.expectEqual(Mode.borrowed, sig.paramMode(0));
    try std.testing.expectEqual(Mode.owned, sig.paramMode(1));
    try std.testing.expectEqual(Mode.borrowed, sig.paramMode(3));
    try std.testing.expectEqual(Mode.borrowed, sig.paramMode(15));
    try std.testing.expectEqual(Mode.owned, sig.paramMode(16));
}

test "empty signature table answers all-owned" {
    const table = SigTable.all_owned;
    const sig = table.get(@enumFromInt(7));
    try std.testing.expectEqual(@as(ParamMask, 0), sig.borrowed_params);
    try std.testing.expectEqual(Mode.owned, sig.ret_mode);
    try std.testing.expectEqual(false, sig.ret_unique);
    try std.testing.expectEqual(@as(ParamMask, 0), sig.unique_params);
    try std.testing.expect(sig.outcomes.isEmpty());
}

test "outcome spans expose exact restitution rows" {
    const sigs = [_]RcSig{.{ .outcomes = .{ .start = 1, .len = 2 } }};
    const outcomes = [_]Outcome{
        .{ .discriminant = 99, .restituted_params = 0 },
        .{ .discriminant = 0, .restituted_params = 1 },
        .{ .discriminant = 1, .restituted_params = 0 },
    };
    const table = SigTable{ .sigs = &sigs, .outcomes = &outcomes };
    const rows = table.outcomesOf(sigs[0]);
    try std.testing.expectEqual(@as(usize, 2), rows.len);
    try std.testing.expectEqual(@as(u16, 0), rows[0].discriminant);
    try std.testing.expectEqual(@as(ParamMask, 1), rows[0].restituted_params);
    try std.testing.expectEqual(@as(u16, 1), rows[1].discriminant);
}
