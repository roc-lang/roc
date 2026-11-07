//! Stable ids for checked module payload stores.

const std = @import("std");
const can = @import("can");

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
/// A scheme of one checked module: a source scheme is named by its root's id,
/// a source variable, so it is a function of the module's sealed data; a
/// synthetic scheme by its row of the module's synthetic schemes, in the
/// synthesized id space (`CheckedTypeStore.internScheme`).
pub const CheckedTypeSchemeId = enum(u32) { _ };

/// Identity of one compile-time root within its checked module, a function
/// of the module's checked data alone: a REPL expression's root named by its
/// expression, a definition's by its definition, a selected hoisted root by
/// its index in the module's selected hoisted roots, an expect's by its
/// statement, a literal conversion root by its literal, or a literal argument
/// root by the edge that fixes its type and the callee requirement it
/// converts. The top three bits say which; the literal encodings live with
/// the literal identities (`checked_module.literalConversionRootId`,
/// `checked_module.literalArgumentRootId`). No kind encodes as zero, so a
/// root's identity is never a template table row
/// (`CheckedProcedureTemplateId`).
pub const ComptimeRootKey = enum(u128) {
    _,

    pub const Kind = enum(u3) {
        repl_expr = 1,
        def = 2,
        hoisted = 3,
        expect = 4,
        literal_conversion = 5,
        literal_argument = 6,
    };

    pub const kind_shift = 125;

    /// The root of a source kind (`repl_expr`, `def`, `hoisted`, `expect`)
    /// named by `source`.
    pub fn ofSource(source_kind: Kind, source_index: u32) ComptimeRootKey {
        std.debug.assert(!kindIsLiteral(source_kind));
        return @fromBackingInt((@as(u128, @backingInt(source_kind)) << kind_shift) | source_index);
    }

    pub fn kind(self: ComptimeRootKey) Kind {
        return switch (@as(u3, @truncate(@backingInt(self) >> kind_shift))) {
            1 => .repl_expr,
            2 => .def,
            3 => .hoisted,
            4 => .expect,
            5 => .literal_conversion,
            6 => .literal_argument,
            0, 7 => unreachable, // no root identity encodes these kinds
        };
    }

    fn kindIsLiteral(root_kind: Kind) bool {
        return switch (root_kind) {
            .repl_expr, .def, .hoisted, .expect => false,
            .literal_conversion, .literal_argument => true,
        };
    }

    /// Whether the root is a literal root rather than a row of the module's
    /// root table.
    pub fn isLiteral(self: ComptimeRootKey) bool {
        return kindIsLiteral(self.kind());
    }

    /// The source a row root's identity names: its REPL expression,
    /// definition, selected hoisted root index or expect statement.
    pub fn source(self: ComptimeRootKey) u32 {
        std.debug.assert(!self.isLiteral());
        return @truncate(@backingInt(self));
    }

    /// Feed the id to a digest: 32 set bits and then its 128-bit identity,
    /// little-endian, as `canonical_names.digestId` feeds every identity
    /// that is not a template table row.
    pub fn digestInto(self: ComptimeRootKey, hasher: anytype) void {
        var marker: [4]u8 = undefined;
        std.mem.writeInt(u32, &marker, std.math.maxInt(u32), .little);
        hasher.update(&marker);
        var identity: [16]u8 = undefined;
        std.mem.writeInt(u128, &identity, @backingInt(self), .little);
        hasher.update(&identity);
    }
};
/// Stable identity of a generalized-local dispatch scope within a checked
/// module.
pub const DispatchScopeId = enum(u32) { _ };
/// Serial id into a `CheckedTypeStore`'s `var_names` interner (the text of a
/// stored type-variable name).
pub const CheckedVarNameId = enum(u32) { _ };

/// The one inline optional-id encoding for checked-module data: an
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
        /// to the `none` sentinel), which no checked-module id space reaches.
        pub fn some(id: Id) Self {
            const raw = @backingInt(id);
            std.debug.assert(raw != std.math.maxInt(u32));
            return @fromBackingInt(@intCast(raw + 1));
        }

        /// The wrapped id, or null for `none`.
        pub fn get(self: Self) ?Id {
            if (self == .none) return null;
            return @fromBackingInt(@intCast(@backingInt(self) - 1));
        }
    };
}

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

/// One explicit identity for a closure capture. Checked modules and active
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
///    bytes) and is cache-safe to serialize in checked modules. Because the
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
///        enter checked modules (post-check IRs are not cached).
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
