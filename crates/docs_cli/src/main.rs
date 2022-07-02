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
