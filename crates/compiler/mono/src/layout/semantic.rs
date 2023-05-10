//! Semantic representations of memory layouts for the purposes of specialization.

/// A semantic representation of a memory layout.
/// Semantic representations describe the shape of a type a [Layout][super::Layout] is generated
/// for. Semantic representations disambiguate types that have the same runtime memory layout, but
/// different shapes.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum SemanticRepr<'a> {
    None,
    Record(SemaRecord<'a>),
}

impl<'a> SemanticRepr<'a> {
    pub(super) const EMPTY_RECORD: Self = Self::Record(SemaRecord { fields: &[] });

    pub(super) fn record(fields: &'a [&'a str]) -> Self {
        Self::Record(SemaRecord { fields })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct SemaRecord<'a> {
    pub fields: &'a [&'a str],
}
