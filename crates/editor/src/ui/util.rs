use super::ui_error::{FileOpenFailedSnafu, FileWriteFailedSnafu, OutOfBoundsSnafu, UIResult};
use snafu::{OptionExt, ResultExt};
use std::{fs::File, io::BufReader, path::Path, slice::SliceIndex};

pub fn is_newline(char_ref: &char) -> bool {
    let newline_codes = vec!['\u{d}', '\n'];

    newline_codes.contains(char_ref)
}

// replace slice method that return Option with one that return Result and proper Error
pub fn slice_get<T>(index: usize, slice: &[T]) -> UIResult<&<usize as SliceIndex<[T]>>::Output> {
    let elt_ref = slice.get(index).context(OutOfBoundsSnafu {
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

    let elt_ref = slice.get_mut(index).context(OutOfBoundsSnafu {
        index,
        collection_name: "Slice",
        len: slice_len,
    })?;

    Ok(elt_ref)
}

pub fn reader_from_path(path: &Path) -> UIResult<BufReader<File>> {
    match File::open(path) {
        Ok(file) => Ok(BufReader::new(file)),
        Err(e) => FileOpenFailedSnafu {
            path_str: path_to_string(path),
            err_msg: e.to_string(),
        }
        .fail()?,
    }
}

pub fn path_to_string(path: &Path) -> String {
    let mut path_str = String::new();
    path_str.push_str(&path.to_string_lossy());

    path_str
}

pub fn write_to_file(path: &Path, content: &str) -> UIResult<()> {
    std::fs::write(path, content).with_context(|_| FileWriteFailedSnafu {
        path_str: path_to_string(path),
    })
}
