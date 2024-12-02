//! Provides a binary that is only used for static build servers.
use bumpalo::Bump;
use clap::{value_parser, Arg, Command};
use roc_docs_io::Problem;
use std::io;
use std::path::PathBuf;

pub const ROC_FILE: &str = "ROC_FILE";
const DEFAULT_ROC_FILENAME: &str = "main.roc";

fn main() -> io::Result<()> {
    let matches = Command::new("roc-docs")
        .about("Generate documentation for a Roc package")
        .arg(
            Arg::new(ROC_FILE)
                .help("The package's main .roc file")
                .num_args(0..)
                .value_parser(value_parser!(PathBuf))
                .default_value(DEFAULT_ROC_FILENAME),
        )
        .get_matches();

    let arena = Bump::new();

    // Populate roc_files
    match roc_docs::generate_docs_html(
        &arena,
        // TODO get this from the platform's source file rather than hardcoding it!
        // github.com/roc-lang/roc/issues/5712
        "Documentation",
        matches.get_one::<PathBuf>(ROC_FILE).unwrap().to_owned(),
        &PathBuf::from("./generated-docs"),
        None,
    ) {
        Ok(()) => Ok(()),
        Err(problem) => match problem {
            Problem::FailedToLoadModule => {
                let todo = todo!();
            }
            Problem::FailedToDeleteDir(_, _) => {
                let todo = todo!();
            }
            Problem::FailedToCreateDir(_, _) => {
                let todo = todo!();
            }
            Problem::FailedToWrite(_, _) => {
                let todo = todo!();
            }
        },
    }
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
