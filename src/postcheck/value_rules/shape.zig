//! The shapes value rules distinguish.
//!
//! Inspection, structural equality, and structural hashing are value
//! rules: they depend on a type's shape, not on which lowering strategy
//! produced it. Each consumer (Monotype lowering, Boxy lowering, the Boxy
//! runtime) reads its own type representation into one of these shapes, and
//! the shared walkers decide what each shape means.

/// Every shape a consumer's type representation reads as.
pub const ShapeKind = enum {
    /// A builtin scalar or `Str`.
    primitive,
    /// A Bool its consumer stores as a bare discriminant.
    bool,
    empty_record,
    record,
    tuple,
    /// A tag union whose variants the type closes; a union with no variants
    /// has no values.
    tag_union,
    /// A field-presence slot: an unset optional field or its value.
    presence_slot,
    list,
    box,
    /// A nominal type: its own method replaces the default, otherwise its
    /// backing decides, unless it hides its structure.
    nominal,
    /// A name for another type that behaves exactly as that type.
    transparent,
    function,
    /// A value only its runtime descriptor describes.
    dynamic,
};
