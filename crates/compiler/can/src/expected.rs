use crate::pattern::Pattern;
use roc_region::all::{Loc, Region};
use roc_types::types::{AnnotationSource, PReason, Reason};

#[derive(Debug, Clone, PartialEq)]
pub enum Expected<T> {
    NoExpectation(T),
    FromAnnotation(Loc<Pattern>, usize, AnnotationSource, T),
    ForReason(Reason, T, Region),
}

/// Like Expected, but for Patterns.
#[derive(Debug, Clone, PartialEq)]
pub enum PExpected<T> {
    NoExpectation(T),
    ForReason(PReason, T, Region),
}

impl<T> PExpected<T> {
    pub fn get_type(self) -> T {
        match self {
            PExpected::NoExpectation(val) => val,
            PExpected::ForReason(_, val, _) => val,
        }
    }

    pub fn get_type_ref(&self) -> &T {
        match self {
            PExpected::NoExpectation(val) => val,
            PExpected::ForReason(_, val, _) => val,
        }
    }

    pub fn get_type_mut_ref(&mut self) -> &mut T {
        match self {
            PExpected::NoExpectation(val) => val,
            PExpected::ForReason(_, val, _) => val,
        }
    }

    #[inline(always)]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> PExpected<U> {
        match self {
            PExpected::NoExpectation(val) => PExpected::NoExpectation(f(val)),
            PExpected::ForReason(reason, val, region) => {
                PExpected::ForReason(reason, f(val), region)
            }
        }
    }

    pub fn replace<U>(self, new: U) -> PExpected<U> {
        match self {
            PExpected::NoExpectation(_val) => PExpected::NoExpectation(new),
            PExpected::ForReason(reason, _val, region) => PExpected::ForReason(reason, new, region),
        }
    }

    pub fn replace_ref<U>(&self, new: U) -> PExpected<U> {
        match self {
            PExpected::NoExpectation(_val) => PExpected::NoExpectation(new),
            PExpected::ForReason(reason, _val, region) => {
                PExpected::ForReason(reason.clone(), new, *region)
            }
        }
    }
}

impl<T> Expected<T> {
    pub fn get_type(self) -> T {
        match self {
            Expected::NoExpectation(val) => val,
            Expected::ForReason(_, val, _) => val,
            Expected::FromAnnotation(_, _, _, val) => val,
        }
    }

    pub fn get_type_ref(&self) -> &T {
        match self {
            Expected::NoExpectation(val) => val,
            Expected::ForReason(_, val, _) => val,
            Expected::FromAnnotation(_, _, _, val) => val,
        }
    }

    pub fn get_type_mut_ref(&mut self) -> &mut T {
        match self {
            Expected::NoExpectation(val) => val,
            Expected::ForReason(_, val, _) => val,
            Expected::FromAnnotation(_, _, _, val) => val,
        }
    }

    pub fn get_annotation_region(&self) -> Option<Region> {
        match self {
            Expected::FromAnnotation(_, _, AnnotationSource::TypedBody { region }, _) => {
                Some(*region)
            }
            _ => None,
        }
    }

    #[inline(always)]
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Expected<U> {
        match self {
            Expected::NoExpectation(val) => Expected::NoExpectation(f(val)),
            Expected::ForReason(reason, val, region) => Expected::ForReason(reason, f(val), region),
            Expected::FromAnnotation(pattern, size, source, val) => {
                Expected::FromAnnotation(pattern, size, source, f(val))
            }
        }
    }

    pub fn replace<U>(self, new: U) -> Expected<U> {
        match self {
            Expected::NoExpectation(_val) => Expected::NoExpectation(new),
            Expected::ForReason(reason, _val, region) => Expected::ForReason(reason, new, region),
            Expected::FromAnnotation(pattern, size, source, _val) => {
                Expected::FromAnnotation(pattern, size, source, new)
            }
        }
    }

    pub fn replace_ref<U>(&self, new: U) -> Expected<U> {
        match self {
            Expected::NoExpectation(_val) => Expected::NoExpectation(new),
            Expected::ForReason(reason, _val, region) => {
                Expected::ForReason(reason.clone(), new, *region)
            }
            Expected::FromAnnotation(pattern, size, source, _val) => {
                Expected::FromAnnotation(pattern.clone(), *size, *source, new)
            }
        }
    }
}
