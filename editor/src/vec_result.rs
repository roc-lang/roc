
use crate::error::{OutOfBounds};
use std::slice::SliceIndex;

// replace vec methods that return Option with Result and proper Error

pub fn get_res<T>(indx: usize, vec: &Vec<T>) -> Result<&<usize as SliceIndex<[T]>>::Output, OutOfBounds> {
    match vec.get(indx) {
        Some(elt) =>
            Ok(elt),
        None =>
            Err(OutOfBounds {})
    }
}