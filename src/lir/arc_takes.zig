//! Shared claim-bit encoding for field takes from dying tag unions.
//!
//! A struct container's take claims are keyed directly by field index. A tag
//! union's stored units live behind a variant, so its claims are keyed by
//! (variant, field) pairs packed into the same 64-bit mask: variants in
//! declaration order, and within each variant its refcounted payload fields
//! in stored order. A variant whose payload is not a struct exposes the
//! payload itself as its single field, addressed as field 0.
//!
//! The dismantle analysis and the debug certifier must agree bit-for-bit on
//! this encoding while sharing no side tables, so both derive it here, from
//! the union layout alone. A union whose refcounted fields do not fit the
//! mask simply has no claim encoding, and its containers keep their whole
//! release.

const std = @import("std");
const layout_mod = @import("layout");

/// One (variant, field) slot in a union's claim mask.
pub const UnionFieldBit = struct {
    bit: u6,
    variant: u16,
    /// Field index as `assign_ref .field` addresses it. For a non-struct
    /// payload this is 0 and the field is the payload itself.
    field_idx: u16,
    field_layout: layout_mod.Idx,
};

/// Claim-mask geometry for one tag union layout. Construct via
/// `unionClaimEncoding`; a returned encoding is always complete.
pub const UnionClaimEncoding = struct {
    layouts: *const layout_mod.Store,
    info: layout_mod.TagUnionInfo,

    /// The claim bit for `field_idx` of `variant`, or null when that pair
    /// holds no refcounted stored unit.
    pub fn bitFor(self: UnionClaimEncoding, variant: u16, field_idx: u16) ?u6 {
        var it = self.iterate();
        while (it.next()) |field| {
            if (field.variant == variant and field.field_idx == field_idx) return field.bit;
        }
        return null;
    }

    /// All claim bits belonging to `variant`: the mask a fully dismantled
    /// container of that runtime variant must have claimed.
    pub fn variantMask(self: UnionClaimEncoding, variant: u16) u64 {
        var mask: u64 = 0;
        var it = self.iterate();
        while (it.next()) |field| {
            if (field.variant == variant) mask |= @as(u64, 1) << field.bit;
        }
        return mask;
    }

    /// The variant every set bit of `claims` belongs to, or null when the
    /// claims are empty, span more than one variant, or hold bits outside
    /// the encoding.
    pub fn variantOfClaims(self: UnionClaimEncoding, claims: u64) ?u16 {
        if (claims == 0) return null;
        var found: ?u16 = null;
        var covered: u64 = 0;
        var it = self.iterate();
        while (it.next()) |field| {
            const bit = @as(u64, 1) << field.bit;
            if (claims & bit == 0) continue;
            if (found) |existing| {
                if (existing != field.variant) return null;
            } else {
                found = field.variant;
            }
            covered |= bit;
        }
        if (covered != claims) return null;
        return found;
    }

    /// Every claim-eligible field of every variant, in bit order.
    pub fn iterate(self: UnionClaimEncoding) FieldIterator {
        return .{ .layouts = self.layouts, .info = self.info };
    }
};

/// Iterates every refcounted (variant, field) pair of a union in the stable
/// encoding order, assigning consecutive bits.
pub const FieldIterator = struct {
    layouts: *const layout_mod.Store,
    info: layout_mod.TagUnionInfo,
    variant: u16 = 0,
    /// Position within the current variant's payload struct; `null` before
    /// the variant's shape has been resolved.
    struct_index: ?u32 = null,
    next_bit: u7 = 0,
    /// Set when the union cannot be encoded; `unionClaimEncoding` rejects
    /// such layouts, so a constructed encoding never yields with this set.
    invalid: bool = false,

    pub fn next(self: *FieldIterator) ?UnionFieldBit {
        while (self.variant < self.info.variants.len) {
            const payload_idx = self.info.variants.get(self.variant).payload_layout;
            const payload_layout = self.layouts.getLayout(payload_idx);
            if (payload_layout.tag == .struct_) {
                const struct_info = self.layouts.getStructInfo(payload_layout);
                var index = self.struct_index orelse 0;
                while (index < struct_info.fields.len) {
                    const field = struct_info.fields.get(@intCast(index));
                    index += 1;
                    if (!self.layouts.layoutContainsRefcounted(self.layouts.getLayout(field.layout))) continue;
                    if (field.index >= 64 or self.next_bit >= 64) {
                        self.invalid = true;
                        return null;
                    }
                    self.struct_index = index;
                    const bit: u6 = @intCast(self.next_bit);
                    self.next_bit += 1;
                    return .{ .bit = bit, .variant = self.variant, .field_idx = field.index, .field_layout = field.layout };
                }
            } else if (self.layouts.layoutContainsRefcounted(payload_layout)) {
                if (self.struct_index == null) {
                    if (self.next_bit >= 64) {
                        self.invalid = true;
                        return null;
                    }
                    self.struct_index = 1;
                    const bit: u6 = @intCast(self.next_bit);
                    self.next_bit += 1;
                    return .{ .bit = bit, .variant = self.variant, .field_idx = 0, .field_layout = payload_idx };
                }
            }
            self.variant += 1;
            self.struct_index = null;
        }
        return null;
    }
};

/// The claim encoding for a tag-union layout, or null when the layout is not
/// a tag union or its refcounted fields exceed the mask.
pub fn unionClaimEncoding(layouts: *const layout_mod.Store, union_layout: layout_mod.Layout) ?UnionClaimEncoding {
    if (union_layout.tag != .tag_union) return null;
    const info = layouts.getTagUnionInfo(union_layout);
    const encoding = UnionClaimEncoding{ .layouts = layouts, .info = info };
    var it = encoding.iterate();
    while (it.next()) |_| {}
    if (it.invalid) return null;
    return encoding;
}
