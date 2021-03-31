use super::ui_error::{OutOfBounds, UIResult};
use snafu::OptionExt;
use std::slice::SliceIndex;

pub fn is_newline(char_ref: &char) -> bool {
    let newline_codes = vec!['\u{d}', '\n'];

    newline_codes.contains(char_ref)
}

// replace slice method that return Option with one that return Result and proper Error
pub fn slice_get<T>(index: usize, slice: &[T]) -> UIResult<&<usize as SliceIndex<[T]>>::Output> {
    let elt_ref = slice.get(index).context(OutOfBounds {
        index,
        collection_name: "Slice",
        len: slice.len(),
    })?;

    Ok(elt_ref)
}

pub fn slice_get_mut<T>(
    index: usize,
    slice: &mut [T],
) -> UIResult<&mut <usize as SliceIndex<[T]>>::Output> {
    let slice_len = slice.len();

    let elt_ref = slice.get_mut(index).context(OutOfBounds {
        index,
        collection_name: "Slice",
        len: slice_len,
    })?;

    Ok(elt_ref)
}
