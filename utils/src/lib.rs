use snafu::OptionExt;
use std::path::{PathBuf, Path};
use std::{collections::HashMap, slice::SliceIndex};
use std::fs;
use util_error::{IndexOfFailed, KeyNotFound, OutOfBounds, UtilResult};

pub mod util_error;

// replace HashMap method that returns Option with one that returns Result and proper Error
pub fn map_get<'a, K: ::std::fmt::Debug + std::hash::Hash + std::cmp::Eq, V>(
    hash_map: &'a HashMap<K, V>,
    key: &K,
) -> UtilResult<&'a V> {
    let value = hash_map.get(key).context(KeyNotFound {
        key_str: format!("{:?}", key),
    })?;

    Ok(value)
}

pub fn index_of<T: ::std::fmt::Debug + std::cmp::Eq>(elt: T, slice: &[T]) -> UtilResult<usize> {
    let index = slice
        .iter()
        .position(|slice_elt| *slice_elt == elt)
        .with_context(|| {
            let elt_str = format!("{:?}", elt);
            let collection_str = format!("{:?}", slice);

            IndexOfFailed {
                elt_str,
                collection_str,
            }
        })?;

    Ok(index)
}

// replaces slice method that return Option with one that return Result and proper Error
pub fn slice_get<T>(index: usize, slice: &[T]) -> UtilResult<&<usize as SliceIndex<[T]>>::Output> {
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
) -> UtilResult<&mut <usize as SliceIndex<[T]>>::Output> {
    let slice_len = slice.len();

    let elt_ref = slice.get_mut(index).context(OutOfBounds {
        index,
        collection_name: "Slice",
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
        let elt_str = format!("{:?}", elt);
        let collection_str = format!("{:?}", slice);

        IndexOfFailed {
            elt_str,
            collection_str,
        }
        .fail()
    }
}

pub fn zig_cache_find(dir: &str, filename: &str) -> String {
    #[cfg(unix)]
    return find(dir, filename);
    #[cfg(not(unix))]
    path_to_str(&zig_cache_find_no_unix(dir, filename)).to_string()
}

#[cfg(unix)]
fn find(dir: &str, filename: &str) -> String {
    run_command(
        &std::env::current_dir().unwrap(),
        "find",
        [dir, "-name", filename]
    ).split('\n').next().unwrap().to_string()
}

#[cfg(not(unix))]
fn zig_cache_find_no_unix(dir: &str, filename: &str) -> PathBuf {
    let dir_contents = fs::read_dir(dir).unwrap();

    for dir_entry in dir_contents {
        let files_in_sub_dir =  fs::read_dir(dir_entry.unwrap().path()).unwrap();
        
        for dir_entry in files_in_sub_dir {
            let file = dir_entry.unwrap();

            if file.file_name() == filename {
                return file.path()
            }
        }
    }

    panic!("Could not find {} in {}", filename, dir)
}

pub fn path_to_str(path: &Path) -> &str {
    path.as_os_str().to_str().unwrap()
}