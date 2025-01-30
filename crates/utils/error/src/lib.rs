//! Provides utility functions used all over the code base.
use std::{collections::HashMap, slice::SliceIndex};

#[derive(Debug)]
pub enum UtilError {
    IndexOfFailed {
        elt_str: String,
        collection_str: String,
    },
    KeyNotFound {
        key_str: String,
    },
    OutOfBounds {
        index: usize,
        collection_name: String,
        len: usize,
    },
}

impl std::fmt::Display for UtilError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IndexOfFailed {
                elt_str,
                collection_str,
            } => {
                write!(
                    f,
                    "IndexOfFailed: Element {} was not found in collection {}.",
                    elt_str, collection_str
                )
            }
            Self::KeyNotFound { key_str } => {
                write!(f, "KeyNotFound: key {} was not found in HashMap.", key_str)
            }
            Self::OutOfBounds {
                index,
                collection_name,
                len,
            } => {
                write!(
                    f,
                    "OutOfBounds: index {} was out of bounds for {} with length {}.",
                    index, collection_name, len
                )
            }
        }
    }
}

impl std::error::Error for UtilError {}

pub type UtilResult<T, E = UtilError> = std::result::Result<T, E>;

// replace HashMap method that returns Option with one that returns Result and proper Error
pub fn map_get<'a, K: ::std::fmt::Debug + std::hash::Hash + std::cmp::Eq, V>(
    hash_map: &'a HashMap<K, V>,
    key: &K,
) -> UtilResult<&'a V> {
    let value = hash_map.get(key).ok_or_else(|| UtilError::KeyNotFound {
        key_str: format!("{key:?}"),
    })?;

    Ok(value)
}

pub fn index_of<T: ::std::fmt::Debug + std::cmp::Eq>(elt: T, slice: &[T]) -> UtilResult<usize> {
    let index = slice
        .iter()
        .position(|slice_elt| *slice_elt == elt)
        .ok_or_else(|| {
            let elt_str = format!("{elt:?}");
            let collection_str = format!("{slice:?}");

            UtilError::IndexOfFailed {
                elt_str,
                collection_str,
            }
        })?;

    Ok(index)
}

// replaces slice method that return Option with one that return Result and proper Error
pub fn slice_get<T>(index: usize, slice: &[T]) -> UtilResult<&<usize as SliceIndex<[T]>>::Output> {
    let elt_ref = slice.get(index).ok_or_else(|| UtilError::OutOfBounds {
        index,
        collection_name: "Slice".to_owned(),
        len: slice.len(),
    })?;

    Ok(elt_ref)
}

pub fn slice_get_mut<T>(
    index: usize,
    slice: &mut [T],
) -> UtilResult<&mut <usize as SliceIndex<[T]>>::Output> {
    let slice_len = slice.len();

    let elt_ref = slice.get_mut(index).ok_or_else(|| UtilError::OutOfBounds {
        index,
        collection_name: "Slice".to_owned(),
        len: slice_len,
    })?;

    Ok(elt_ref)
}

// returns the index of the first occurrence of element and index of the last occurrence
pub fn first_last_index_of<T: ::std::fmt::Debug + std::cmp::Eq>(
    elt: T,
    slice: &[T],
) -> UtilResult<(usize, usize)> {
    let mut first_index_opt = None;
    let mut last_index_opt = None;

    for (index, list_elt) in slice.iter().enumerate() {
        if *list_elt == elt {
            if first_index_opt.is_none() {
                first_index_opt = Some(index);
                last_index_opt = Some(index);
            } else {
                last_index_opt = Some(index)
            }
        } else if last_index_opt.is_some() {
            break;
        }
    }

    if let (Some(first_index), Some(last_index)) = (first_index_opt, last_index_opt) {
        Ok((first_index, last_index))
    } else {
        let elt_str = format!("{elt:?}");
        let collection_str = format!("{slice:?}");

        Err(UtilError::IndexOfFailed {
            elt_str,
            collection_str,
        })
    }
}
