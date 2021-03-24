use crate::editor::ed_error::IndexOfFailed;
use super::ed_error::{EdResult, KeyNotFound};
use snafu::OptionExt;
use std::collections::HashMap;

// replace HashMap method that returns Option with one that returns Result and proper Error
pub fn map_get<'a, K: ::std::fmt::Debug + std::hash::Hash + std::cmp::Eq, V>(
    hash_map: &'a HashMap<K, V>,
    key: &K,
) -> EdResult<&'a V> {
    let value = hash_map.get(key).context(KeyNotFound {
        key_str: format!("{:?}", key),
    })?;

    Ok(value)
}

pub fn index_of<T: ::std::fmt::Debug + std::cmp::Eq>(elt:T, slice: &[T]) -> EdResult<usize> {
    let index = slice.iter().position(|&slice_elt| slice_elt == elt).with_context(
        || {
            let elt_str = format!("{:?}", elt);
            let collection_str = format!("{:?}", slice);

            IndexOfFailed {
                elt_str,
                collection_str,
            }
        }
    )?;

    Ok(index)
}
