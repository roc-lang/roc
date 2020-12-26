
use crate::error::OutOfBounds;
use crate::error::{EdResult};
use std::slice::SliceIndex;
use snafu::{OptionExt};

// replace vec methods that return Option with ones that return Result and proper Error

pub fn get_res<T>(index: usize, vec: &Vec<T>) -> EdResult<&<usize as SliceIndex<[T]>>::Output> {

    let elt_ref = vec.get(index).context(OutOfBounds {
        index,
        vec_len: vec.len()
    })?;

    Ok(elt_ref)
}