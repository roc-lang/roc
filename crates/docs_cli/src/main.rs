//! Provides a binary that is only used for static build servers.
use clap::{Arg, Command};
use roc_docs::generate_docs_html;
use std::fs::{self, FileType};
use std::io;
use std::path::{Path, PathBuf};

pub const DIRECTORY_OR_FILES: &str = "DIRECTORY_OR_FILES";

fn main() -> io::Result<()> {
    let matches = Command::new("roc-docs")
        .about("Build HTML documentation files from the given .roc files")
        .arg(
            Arg::new(DIRECTORY_OR_FILES)
                .multiple_values(true)
                .required(true)
                .help("The directory or files to build documentation for")
                .allow_invalid_utf8(true),
        )
        .get_matches();

    let mut roc_files = Vec::new();

    // Populate roc_files
    for os_str in matches.values_of_os(DIRECTORY_OR_FILES).unwrap() {
        let metadata = fs::metadata(os_str)?;
        roc_files_recursive(os_str, metadata.file_type(), &mut roc_files)?;
    }

    generate_docs_html(roc_files);

    Ok(())
}

fn roc_files_recursive<P: AsRef<Path>>(
    path: P,
    file_type: FileType,
    roc_files: &mut Vec<PathBuf>,
) -> io::Result<()> {
    if file_type.is_dir() {
        for entry_res in fs::read_dir(path)? {
            let entry = entry_res?;

            roc_files_recursive(entry.path(), entry.file_type()?, roc_files)?;
        }
    } else {
        roc_files.push(path.as_ref().to_path_buf());
    }

    Ok(())
}

// These functions don't end up in the final Roc binary but Windows linker needs a definition inside the crate.
// On Windows, there seems to be less dead-code-elimination than on Linux or MacOS, or maybe it's done later.
#[cfg(windows)]
#[allow(unused_imports)]
use windows_roc_platform_functions::*;

#[cfg(windows)]
mod windows_roc_platform_functions {
    use core::ffi::c_void;

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
        libc::malloc(size)
    }

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_realloc(
        c_ptr: *mut c_void,
        new_size: usize,
        _old_size: usize,
        _alignment: u32,
    ) -> *mut c_void {
        libc::realloc(c_ptr, new_size)
    }

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
        libc::free(c_ptr)
    }
}
