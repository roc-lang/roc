use super::ed_error::{EdResult, OutOfBounds, KeyNotFound};
use snafu::OptionExt;
use std::slice::SliceIndex;
use std::collections::HashMap;

// replace vec methods that return Option with ones that return Result and proper Error
pub fn slice_get<T>(index: usize, slice: &[T]) -> EdResult<&<usize as SliceIndex<[T]>>::Output> {
    let elt_ref = slice.get(index).context(OutOfBounds {
        index,
        collection_name: "Slice",
        len: slice.len(),
    })?;

    Ok(elt_ref)
}

pub fn map_get<K: ::std::fmt::Debug + std::hash::Hash + std::cmp::Eq, V>
    (hash_map: &HashMap<K, V>, key: &K) -> EdResult<&V> {
    
    let value =
        hash_map.get(key).context(
            KeyNotFound {
                key_str: format!("{:?}", key),
            }
        )?;
    
    Ok(value)
}
