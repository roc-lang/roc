//! Provides utility functions used all over the code base.
use snafu::OptionExt;
use std::{
    collections::HashMap,
    env::{self, VarError},
    path::PathBuf,
    process::Command,
    slice::SliceIndex,
};
use util_error::{IndexOfFailedSnafu, KeyNotFoundSnafu, OutOfBoundsSnafu, UtilResult};

pub mod util_error;

// replace HashMap method that returns Option with one that returns Result and proper Error
pub fn map_get<'a, K: ::std::fmt::Debug + std::hash::Hash + std::cmp::Eq, V>(
    hash_map: &'a HashMap<K, V>,
    key: &K,
) -> UtilResult<&'a V> {
    let value = hash_map.get(key).context(KeyNotFoundSnafu {
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

            IndexOfFailedSnafu {
                elt_str,
                collection_str,
            }
        })?;

    Ok(index)
}

// replaces slice method that return Option with one that return Result and proper Error
pub fn slice_get<T>(index: usize, slice: &[T]) -> UtilResult<&<usize as SliceIndex<[T]>>::Output> {
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
) -> UtilResult<&mut <usize as SliceIndex<[T]>>::Output> {
    let slice_len = slice.len();

    let elt_ref = slice.get_mut(index).context(OutOfBoundsSnafu {
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

        IndexOfFailedSnafu {
            elt_str,
            collection_str,
        }
        .fail()
    }
}

// get the path of the lib folder
// runtime dependencies like zig files, Windows dylib builds, are put in the lib folder
pub fn get_lib_path() -> Option<PathBuf> {
    let exe_relative_str_path_opt = std::env::current_exe().ok();

    if let Some(exe_relative_str_path) = exe_relative_str_path_opt {
        #[cfg(windows)]
        let exe_relative_str_path = strip_windows_prefix(&exe_relative_str_path);

        let mut curr_parent_opt = exe_relative_str_path.parent();

        // this differs for regular build and nix releases, so we check in multiple spots.
        for _ in 0..3 {
            if let Some(curr_parent) = curr_parent_opt {
                let lib_path = curr_parent.join("lib");

                if std::path::Path::exists(&lib_path) {
                    return Some(lib_path);
                } else {
                    curr_parent_opt = curr_parent.parent();
                }
            } else {
                break;
            }
        }
    }

    None
}

#[cfg(windows)]
use std::path::Path;

/// On windows, the path is prefixed with `\\?\`, the "verbatim" prefix.
/// Such a path does not works as an argument to `zig` and other command line tools,
/// and there seems to be no good way to strip it. So we resort to some string manipulation.
#[cfg(windows)]
pub fn strip_windows_prefix(path_buf: &Path) -> PathBuf {
    let path_str = path_buf.display().to_string();

    Path::new(path_str.trim_start_matches(r"\\?\")).to_path_buf()
}

/// get the Path of the root of the repository
pub fn root_dir() -> PathBuf {
    let mut path = env::current_exe().ok().unwrap();

    // Get rid of the filename in target/debug/deps/cli_run-99c65e4e9a1fbd06
    path.pop();

    // If we're in deps/ get rid of deps/ in target/debug/deps/
    if path.ends_with("deps") {
        path.pop();
    }

    // Get rid of target/debug/ so we're back at the project root
    path.pop();
    path.pop();

    // running cargo with --target will put us in the target dir
    if path.ends_with("target") {
        path.pop();
    }

    path
}

/// Gives a friendly error if cargo is not installed.
/// Also makes it easy to track where we use cargo in the codebase.
pub fn cargo() -> Command {
    let command_str = "cargo";

    if check_command_available(command_str) {
        Command::new(command_str)
    } else {
        panic!("I could not find the cargo command.\nVisit https://rustup.rs/ to install cargo.",)
    }
}

/// Gives a friendly error if rustup is not installed.
/// Also makes it easy to track where we use rustup in the codebase.
pub fn rustup() -> Command {
    // on windows, we need the version of cargo installed by rustup. The meaning of `cargo` is
    // different within a process that runs rust. So we need to explicitly refer to where
    // rustup put the binary
    let command_str = "rustup";

    if check_command_available(command_str) {
        Command::new(command_str)
    } else {
        panic!("I could not find the rustup command.\nVisit https://rustup.rs/ to install rustup.",)
    }
}

/// Gives a friendly error if clang is not installed.
/// Also makes it easy to track where we use clang in the codebase.
pub fn clang() -> Command {
    let command_str = "clang";

    if check_command_available(command_str) {
        Command::new(command_str)
    } else {
        panic!("I could not find the clang command.\nPlease install clang.",)
        //TODO detect OS and provide detailed install instructions
    }
}

/// Gives a friendly error if zig is not installed.
/// Also makes it easy to track where we use zig in the codebase.
pub fn zig() -> Command {
    let command_str = match std::env::var("ROC_ZIG") {
        Ok(path) => path,
        Err(_) => "zig".into(),
    };

    if check_command_available(&command_str) {
        Command::new(command_str)
    } else {
        panic!("I could not find the zig command.\nPlease install zig, see instructions at https://ziglang.org/learn/getting-started/.",)
    }
}

fn check_command_available(command_name: &str) -> bool {
    if cfg!(target_family = "unix") {
        let unparsed_path = match std::env::var("PATH") {
            Ok(var) => var,
            Err(VarError::NotPresent) => return false,
            Err(VarError::NotUnicode(_)) => {
                panic!("found PATH, but it included invalid unicode data!")
            }
        };

        std::env::split_paths(&unparsed_path).any(|dir| dir.join(command_name).exists())
    } else if cfg!(target = "windows") {
        let mut cmd = Command::new("Get-Command");

        cmd.args([command_name]);

        let cmd_str = format!("{:?}", cmd);

        let cmd_out = cmd.output().unwrap_or_else(|err| {
            panic!(
                "Failed to execute `{}` to check if {} is available:\n    {}",
                cmd_str, command_name, err
            )
        });

        cmd_out.status.success()
    } else {
        // We're in uncharted waters, best not to panic if
        // things may end up working out down the line.
        true
    }
}
