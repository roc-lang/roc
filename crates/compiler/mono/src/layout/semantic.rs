//! Semantic representations of memory layouts for the purposes of specialization.

/// A semantic representation of a memory layout.
/// Semantic representations describe the shape of a type a [Layout][super::Layout] is generated
/// for. Semantic representations disambiguate types that have the same runtime memory layout, but
/// different shapes.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SemanticRepr<'a>(Inner<'a>);

impl<'a> std::fmt::Debug for SemanticRepr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Inner<'a> {
    None,
    Record(SemaRecord<'a>),
    Tuple(SemaTuple),
}

impl<'a> SemanticRepr<'a> {
    pub const NONE: Self = Self(Inner::None);

    pub(super) const fn record(fields: &'a [&'a str]) -> Self {
        Self(Inner::Record(SemaRecord { fields }))
    }

    pub(super) fn tuple(size: usize) -> Self {
        Self(Inner::Tuple(SemaTuple { size }))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct SemaRecord<'a> {
    fields: &'a [&'a str],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct SemaTuple {
    size: usize,
}
