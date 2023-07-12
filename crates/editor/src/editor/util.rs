use super::ed_error::{EdResult, KeyNotFoundSnafu};
use crate::editor::ed_error::IndexOfFailedSnafu;
use snafu::OptionExt;
use std::collections::HashMap;

// replace HashMap method that returns Option with one that returns Result and proper Error
pub fn map_get<'a, K: ::std::fmt::Debug + std::hash::Hash + std::cmp::Eq, V>(
    hash_map: &'a HashMap<K, V>,
    key: &K,
) -> EdResult<&'a V> {
    let value = hash_map.get(key).context(KeyNotFoundSnafu {
        key_str: format!("{key:?}"),
    })?;

    Ok(value)
}

pub fn index_of<T: ::std::fmt::Debug + std::cmp::Eq>(elt: T, slice: &[T]) -> EdResult<usize> {
    let index = slice
        .iter()
        .position(|slice_elt| *slice_elt == elt)
        .with_context(|| {
            let elt_str = format!("{elt:?}");
            let collection_str = format!("{slice:?}");

            IndexOfFailedSnafu {
                elt_str,
                collection_str,
            }
        })?;

    Ok(index)
}

// returns the index of the first occurrence of element and index of the last occurrence
pub fn first_last_index_of<T: ::std::fmt::Debug + std::cmp::Eq>(
    elt: T,
    slice: &[T],
) -> EdResult<(usize, usize)> {
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

        IndexOfFailedSnafu {
            elt_str,
            collection_str,
        }
        .fail()
    }
}
