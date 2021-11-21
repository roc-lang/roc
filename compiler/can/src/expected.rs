use crate::pattern::Pattern;
use roc_region::all::{Located, Region};
use roc_types::types::{AnnotationSource, PReason, Reason};

#[derive(Debug, Clone, PartialEq)]
pub enum Expected<T, Annot> {
    NoExpectation(T),
    FromAnnotation(Located<Pattern>, usize, AnnotationSource<Annot>, T),
    ForReason(Reason, T, Region),
}

/// Like Expected, but for Patterns.
#[derive(Debug, Clone, PartialEq, Eq)]
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

    pub fn replace<U>(self, new: U) -> PExpected<U> {
        match self {
            PExpected::NoExpectation(_val) => PExpected::NoExpectation(new),
            PExpected::ForReason(reason, _val, region) => PExpected::ForReason(reason, new, region),
        }
    }
}

impl<T, Annot> Expected<T, Annot> {
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
            Expected::FromAnnotation(_, _, ann_source, _) => Some(ann_source.region()),
            _ => None,
        }
    }

    pub fn replace<U>(self, new: U) -> Expected<U, Annot> {
        match self {
            Expected::NoExpectation(_val) => Expected::NoExpectation(new),
            Expected::ForReason(reason, _val, region) => Expected::ForReason(reason, new, region),
            Expected::FromAnnotation(pattern, size, source, _val) => {
                Expected::FromAnnotation(pattern, size, source, new)
            }
        }
    }

    pub fn replace_ref<U>(&self, new: U) -> Expected<U, Annot>
    where
        Annot: Clone,
    {
        match self {
            Expected::NoExpectation(_val) => Expected::NoExpectation(new),
            Expected::ForReason(reason, _val, region) => {
                Expected::ForReason(reason.clone(), new, *region)
            }
            Expected::FromAnnotation(pattern, size, source, _val) => {
                Expected::FromAnnotation(pattern.clone(), *size, source.clone(), new)
            }
        }
    }

    pub fn replace_annotation_with<Annot2, F>(self, create_new_annotation: F) -> Expected<T, Annot2>
    where
        F: FnOnce(Annot) -> Annot2,
    {
        match self {
            Expected::NoExpectation(val) => Expected::NoExpectation(val),
            Expected::ForReason(reason, val, region) => Expected::ForReason(reason, val, region),
            Expected::FromAnnotation(pattern, size, ann_source, val) => Expected::FromAnnotation(
                pattern,
                size,
                ann_source.replace_with(create_new_annotation),
                val,
            ),
        }
    }
}
