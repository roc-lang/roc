//! Stable ids for checked artifact payload stores.

const std = @import("std");
const can = @import("can");
const collections = @import("collections");

/// Public `CheckedExhaustivenessSiteId` declaration.
pub const CheckedExhaustivenessSiteId = enum(u32) { _ };
/// A checked type of one module: either a variable of the module's sealed
/// solver store or an entry of its synthesized type store. Readers treat it
/// as opaque and read it through the module's type view.
pub const CheckedTypeId = enum(u32) {
    _,

    /// The id's position in a dense map: its bits rotated so the module's
    /// source and synthesized ids interleave into one dense range.
    pub fn denseIndex(self: CheckedTypeId) usize {
        return std.math.rotl(u32, @backingInt(self), 1);
    }
};
/// Public `CheckedTypeSchemeId` declaration.
pub const CheckedTypeSchemeId = enum(u32) { _ };

/// Identity of one compile-time root within its checked module.
pub const ComptimeRootId = enum(u32) { _ };
/// Identity of one callable-eval template within its checked module: a
/// function-valued top-level binding that is no procedure of its own, whose
/// callable its compile-time root evaluates.
pub const CallableEvalTemplateId = enum(u32) { _ };
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
/// arithmetic is shared with transient columns so stores cannot drift into bespoke
/// `_plus_one` / sentinel encodings.
pub const OptionalId = collections.OptionalId;

test "OptionalId round-trips none and some" {
    const Opt = OptionalId(CheckedVarNameId);
    const absent: Opt = .none;
    try std.testing.expectEqual(@as(?CheckedVarNameId, null), absent.get());
    // The lowest id exercises the +1-bias boundary (raw 0 is reserved for none).
    const id: CheckedVarNameId = @fromBackingInt(@intCast(std.math.minInt(u32)));
    try std.testing.expectEqual(id, Opt.some(id).get().?);
    const high: CheckedVarNameId = @fromBackingInt(@intCast(std.math.maxInt(u32) - 1));
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
///    checked binding. The index is exactly the CIR node of the binder's
///    pattern, so a canonical id is a pure function of (module name, source
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
    pub fn fromBinder(binder_pattern: can.CIR.Pattern.Idx) CaptureId {
        return canonical(@backingInt(binder_pattern));
    }

    /// The canonical capture id for a raw binder index.
    pub fn canonical(index: u32) CaptureId {
        std.debug.assert(index <= max_canonical_index);
        return @fromBackingInt(@intCast(index));
    }

    /// The generated capture id minted by compile-time evaluation for a
    /// per-`ConstStore`-closure counter value.
    pub fn generatedCheck(index: u32) CaptureId {
        std.debug.assert(index <= max_generated_index);
        return @fromBackingInt(@intCast(index | generated_bit));
    }

    /// The generated capture id minted by final Monotype publication, closure
    /// lifting, or spec_constr for a per-Lifted-program counter value.
    pub fn generatedLift(index: u32) CaptureId {
        std.debug.assert(index <= max_generated_index);
        return @fromBackingInt(@intCast(index | generated_bit | lift_bit));
    }

    /// Whether this id names a captured checked binder.
    pub fn isCanonical(self: CaptureId) bool {
        return (@backingInt(self) & generated_bit) == 0;
    }

    /// Whether this id was minted by Monotype publication, closure lifting,
    /// or a later post-check transform.
    pub fn isLiftGenerated(self: CaptureId) bool {
        const raw = @backingInt(self);
        return (raw & generated_bit) != 0 and (raw & lift_bit) != 0;
    }

    /// Whether this id belongs to the lift-time generated sub-range.
    pub fn isGeneratedLift(self: CaptureId) bool {
        return (@backingInt(self) & (generated_bit | lift_bit)) == (generated_bit | lift_bit);
    }

    /// The binder pattern this canonical id was derived from. Asserts the id
    /// is canonical.
    pub fn binder(self: CaptureId) can.CIR.Pattern.Idx {
        std.debug.assert(self.isCanonical());
        return @fromBackingInt(@intCast(@backingInt(self)));
    }

    /// Direct-column index for this namespaced identity.
    pub fn denseIndex(self: CaptureId) usize {
        const raw = @backingInt(self);
        const namespace: usize = if ((raw & generated_bit) == 0)
            0
        else if ((raw & lift_bit) == 0)
            1
        else
            2;
        const index = if (namespace == 0) raw else raw & (lift_bit - 1);
        return @as(usize, index) * 3 + namespace;
    }
};
