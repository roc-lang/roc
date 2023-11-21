//! Semantic representations of memory layouts for the purposes of specialization.

use roc_module::symbol::Symbol;

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

impl<'a> SemanticRepr<'a> {
    pub fn fmt_consistent(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt_consistent(f)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Inner<'a> {
    None,
    Record(SemaRecord<'a>),
    Tuple(SemaTuple),
    TagUnion(SemaTagUnion<'a>),
    Lambdas(SemaLambdas<'a>),
}

impl<'a> Inner<'a> {
    fn fmt_consistent(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // don't print the interned string name of a symbol to get consistent output
        if let Self::Lambdas(sema_lambdas) = self {
            f.debug_struct("SemaLambdas")
                .field("lambda_count", &sema_lambdas.lambdas.len())
                .finish()
        } else {
            std::fmt::Debug::fmt(&self, f)
        }
    }
}

impl<'a> SemanticRepr<'a> {
    pub(super) const NONE: Self = Self(Inner::None);
    pub(super) const EMPTY_RECORD: Self = Self::record(&[]);

    pub(super) const fn record(fields: &'a [&'a str]) -> Self {
        Self(Inner::Record(SemaRecord { fields }))
    }

    pub(super) fn tuple(size: usize) -> Self {
        Self(Inner::Tuple(SemaTuple { size }))
    }

    pub(super) fn tag_union(tags: &'a [&'a str]) -> Self {
        Self(Inner::TagUnion(SemaTagUnion { tags }))
    }

    pub(super) fn lambdas(lambdas: &'a [Symbol]) -> Self {
        Self(Inner::Lambdas(SemaLambdas { lambdas }))
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct SemaTagUnion<'a> {
    tags: &'a [&'a str],
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct SemaLambdas<'a> {
    lambdas: &'a [Symbol],
}
