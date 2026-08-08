//! Stable ids for checked artifact payload stores.

const std = @import("std");

/// Public `CheckedBodyId` declaration.
pub const CheckedBodyId = enum(u32) { _ };
/// Public `CheckedExprId` declaration.
pub const CheckedExprId = enum(u32) { _ };
/// Public `CheckedPatternId` declaration.
pub const CheckedPatternId = enum(u32) { _ };
/// Public `CheckedStatementId` declaration.
pub const CheckedStatementId = enum(u32) { _ };
/// Public `CheckedExhaustivenessSiteId` declaration.
pub const CheckedExhaustivenessSiteId = enum(u32) { _ };
/// Public `CheckedStringLiteralId` declaration.
pub const CheckedStringLiteralId = enum(u32) { _ };
/// Public `CheckedTypeId` declaration.
pub const CheckedTypeId = enum(u32) { _ };
/// Public `CheckedTypeSchemeId` declaration.
pub const CheckedTypeSchemeId = enum(u32) { _ };
/// Public `PatternBinderId` declaration.
pub const PatternBinderId = enum(u32) { _ };
/// Stable identity of a generalized-local dispatch scope within a checked
/// module artifact.
pub const DispatchScopeId = enum(u32) { _ };
/// Serial id into a `CheckedTypeStore`'s `var_names` interner (the text of a
/// stored type-variable name).
pub const CheckedVarNameId = enum(u32) { _ };

/// The one inline optional-id encoding for checked-artifact data: an
/// `enum(u32)` whose `0` value means "absent" and whose other values are the
/// wrapped id biased by `+1`. Being a fixed-tag enum it is extern-compatible
/// and exactly 4 bytes, so it can sit inline in serialized POD rows (unlike a
/// native `?Id`, whose layout is not fixed, or `SerializedOptional`, which
/// stores its payload out of line behind a relocation fixup). All bias
/// arithmetic lives here so individual stores cannot drift into bespoke
/// `_plus_one` / sentinel encodings.
pub fn OptionalId(comptime Id: type) type {
    comptime std.debug.assert(@typeInfo(Id).@"enum".tag_type == u32);
    return enum(u32) {
        none = 0,
        _,

        const Self = @This();

        /// Wrap a present id. `maxInt(u32)` is unrepresentable (it would bias
        /// to the `none` sentinel), which no checked-artifact id space reaches.
        pub fn some(id: Id) Self {
            const raw = @intFromEnum(id);
            std.debug.assert(raw != std.math.maxInt(u32));
            return @enumFromInt(raw + 1);
        }

        /// The wrapped id, or null for `none`.
        pub fn get(self: Self) ?Id {
            if (self == .none) return null;
            return @enumFromInt(@intFromEnum(self) - 1);
        }
    };
}

test "OptionalId round-trips none and some" {
    const Opt = OptionalId(CheckedVarNameId);
    const absent: Opt = .none;
    try std.testing.expectEqual(@as(?CheckedVarNameId, null), absent.get());
    // The lowest id exercises the +1-bias boundary (raw 0 is reserved for none).
    const id: CheckedVarNameId = @enumFromInt(std.math.minInt(u32));
    try std.testing.expectEqual(id, Opt.some(id).get().?);
    const high: CheckedVarNameId = @enumFromInt(std.math.maxInt(u32) - 1);
    try std.testing.expectEqual(high, Opt.some(high).get().?);
    comptime std.debug.assert(@sizeOf(Opt) == 4);
}

/// One explicit identity for a closure capture. Checked artifacts and active
/// Monotype instantiation use the checked identities below; final Monotype
/// publication replaces every materialized capturable local's provisional
/// identity with a program-global lift identity. Operand↔slot joins therefore
/// remain exact key lookups without conflating distinct specializations of one
/// checked binder.
///
/// The `u32` is split into two disjoint ranges by the high bit:
///
///  - **canonical** (high bit clear, `[0, 2^31)`): the identity of a captured
///    checked binding. The index is exactly the `PatternBinderId` of the
///    binder, so a canonical id is a pure function of (module name, source
///    bytes) and is cache-safe to serialize in checked artifacts. Because the
///    mapping is the identity function, the originating binder is always
///    recoverable via `binder()`.
///  - **generated** (high bit set, `[2^31, 2^32)`): the identity of a
///    compiler-synthesized capturable local that has no checked binder—
///    allocated deterministically by the pass that synthesizes it. The
///    generated range is split again by the next bit into two disjoint
///    sub-ranges so ids minted by different synthesizing passes can never
///    collide inside a single function's capture set:
///      - **check** (`0x8000_0000` | index): compile-time evaluation during
///        checking. The index is a per-`ConstStore`-closure counter and must be
///        stable, because it round-trips through serialized `ConstStore`
///        captures.
///      - **lift** (`0xC000_0000` | index): closure lifting / spec_constr after
///        checking. The index is a per-Lifted-program counter. These ids never
///        enter checked artifacts (post-check IRs are not cached).
pub const CaptureId = enum(u32) {
    _,

    /// High bit distinguishing generated ids from canonical ids.
    const generated_bit: u32 = 0x8000_0000;
    /// Within the generated range, distinguishes lift-time from check-time ids.
    const lift_bit: u32 = 0x4000_0000;
    /// Largest index representable in a canonical id.
    pub const max_canonical_index: u32 = generated_bit - 1;
    /// Largest index representable in a generated sub-range.
    pub const max_generated_index: u32 = lift_bit - 1;

    /// The canonical capture id for a captured binder.
    pub fn fromBinder(id: PatternBinderId) CaptureId {
        return canonical(@intFromEnum(id));
    }

    /// The canonical capture id for a raw binder index.
    pub fn canonical(index: u32) CaptureId {
        std.debug.assert(index <= max_canonical_index);
        return @enumFromInt(index);
    }

    /// The generated capture id minted by compile-time evaluation for a
    /// per-`ConstStore`-closure counter value.
    pub fn generatedCheck(index: u32) CaptureId {
        std.debug.assert(index <= max_generated_index);
        return @enumFromInt(index | generated_bit);
    }

    /// The generated capture id minted by final Monotype publication, closure
    /// lifting, or spec_constr for a per-Lifted-program counter value.
    pub fn generatedLift(index: u32) CaptureId {
        std.debug.assert(index <= max_generated_index);
        return @enumFromInt(index | generated_bit | lift_bit);
    }

    /// Whether this id names a captured checked binder.
    pub fn isCanonical(self: CaptureId) bool {
        return (@intFromEnum(self) & generated_bit) == 0;
    }

    /// Whether this id names a compiler-synthesized capturable local.
    pub fn isGenerated(self: CaptureId) bool {
        return !self.isCanonical();
    }

    /// Whether this id was minted by Monotype publication, closure lifting,
    /// or a later post-check transform.
    pub fn isLiftGenerated(self: CaptureId) bool {
        const raw = @intFromEnum(self);
        return (raw & generated_bit) != 0 and (raw & lift_bit) != 0;
    }

    /// Whether this id belongs to the lift-time generated sub-range.
    pub fn isGeneratedLift(self: CaptureId) bool {
        return (@intFromEnum(self) & (generated_bit | lift_bit)) == (generated_bit | lift_bit);
    }

    /// The `PatternBinderId` this canonical id was derived from. Asserts the id
    /// is canonical.
    pub fn binder(self: CaptureId) PatternBinderId {
        std.debug.assert(self.isCanonical());
        return @enumFromInt(@intFromEnum(self));
    }

    /// The opaque low-31-bit index of a generated id, unique within its
    /// generated sub-range. Asserts the id is generated.
    pub fn generatedIndex(self: CaptureId) u32 {
        std.debug.assert(self.isGenerated());
        return @intFromEnum(self) & ~generated_bit;
    }

    /// Direct-column index for this namespaced identity.
    pub fn denseIndex(self: CaptureId) usize {
        const raw = @intFromEnum(self);
        const namespace: usize = if ((raw & generated_bit) == 0)
            0
        else if ((raw & lift_bit) == 0)
            1
        else
            2;
        const index = if (namespace == 0) raw else raw & (lift_bit - 1);
        return @as(usize, index) * 3 + namespace;
    }

    /// Construct a namespaced identity from its direct-column index.
    pub fn fromDenseIndex(dense_index: usize) CaptureId {
        const index: u32 = @intCast(dense_index / 3);
        return switch (dense_index % 3) {
            0 => canonical(index),
            1 => generatedCheck(index),
            2 => generatedLift(index),
            else => unreachable,
        };
    }
};
