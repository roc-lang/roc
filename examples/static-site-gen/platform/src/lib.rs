use core::ffi::c_void;
use libc;
use roc_std::{RocList, RocResult, RocStr};
use std::env;
use std::ffi::CStr;
use std::fs;
use std::os::raw::c_char;
use std::path::{Path, PathBuf};

extern "C" {
    #[link_name = "roc__transformFileContentForHost_1_exposed"]
    fn roc_transformFileContentForHost(content: RocList<u8>) -> RocResult<RocList<u8>, RocStr>;
}

#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    libc::malloc(size)
}

#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    libc::realloc(c_ptr, new_size)
}

#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    libc::free(c_ptr)
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} path/to/input/dir path/to/output/dir", args[0]);
        return 1;
    }

    match run(&args[1], &args[2]) {
        Err(e) => {
            eprintln!("{}", e);
            1
        }
        Ok(()) => 0,
    }
}

#[no_mangle]
pub unsafe extern "C" fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
    match tag_id {
        0 => {
            let slice = CStr::from_ptr(c_ptr as *const c_char);
            let string = slice.to_str().unwrap();
            eprintln!("Roc hit a panic: {}", string);
            std::process::exit(1);
        }
        _ => todo!(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn roc_memcpy(
    dest: *mut c_void,
    src: *const c_void,
    bytes: usize,
) -> *mut c_void {
    libc::memcpy(dest, src, bytes)
}

#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}

fn run(input_dirname: &str, output_dirname: &str) -> Result<(), String> {
    let input_dir = PathBuf::from(input_dirname)
        .canonicalize()
        .map_err(|e| format!("{}: {}", input_dirname, e))?;

    let output_dir = {
        let dir = PathBuf::from(output_dirname);
        if !dir.exists() {
            fs::create_dir(&dir).unwrap();
        }
        dir.canonicalize()
            .map_err(|e| format!("{}: {}", output_dirname, e))?
    };

    let mut input_files: Vec<PathBuf> = vec![];
    find_files(&input_dir, &mut input_files)
        .map_err(|e| format!("Error finding input files: {}", e))?;

    println!("Processing {} input files...", input_files.len());

    let mut had_errors = false;

    // TODO: process the files asynchronously
    for input_file in input_files {
        match process_file(&input_dir, &output_dir, &input_file) {
            Ok(()) => {}
            Err(e) => {
                eprintln!("{}", e);
                had_errors = true;
            }
        }
    }

    if had_errors {
        Err("Could not process all files".into())
    } else {
        Ok(())
    }
}

fn process_file(input_dir: &Path, output_dir: &Path, input_file: &Path) -> Result<(), String> {
    let rust_content = match fs::read(input_file) {
        Ok(bytes) => bytes,
        Err(e) => {
            return Err(format!(
                "Error reading {}: {}",
                input_file.to_str().unwrap_or("an input file"),
                e
            ));
        }
    };
    let roc_content = RocList::from_iter(rust_content);
    let roc_result = unsafe { roc_transformFileContentForHost(roc_content) };
    match Result::from(roc_result) {
        Ok(roc_output_bytes) => {
            let input_relpath = input_file
                .strip_prefix(input_dir)
                .map_err(|e| e.to_string())?
                .to_path_buf();

            let mut output_relpath = input_relpath.clone();
            output_relpath.set_extension("html");

            let output_file = output_dir.join(&output_relpath);
            let rust_output_bytes = Vec::from_iter(roc_output_bytes.into_iter());
            fs::write(&output_file, &rust_output_bytes).map_err(|e| format!("{}", e))?;

            println!(
                "{} -> {}",
                input_relpath.display(),
                output_relpath.display()
            );
            Ok(())
        }
        Err(roc_error_str) => Err(format!(
            "Error transforming {}: {}",
            input_file.to_str().unwrap_or("an input file"),
            roc_error_str.as_str()
        )),
    }
}

fn find_files(dir: &Path, file_paths: &mut Vec<PathBuf>) -> std::io::Result<()> {
    for entry in fs::read_dir(dir)? {
        let pathbuf = entry?.path();
        if pathbuf.is_dir() {
            find_files(&pathbuf, file_paths)?;
        } else {
            file_paths.push(pathbuf);
        }
    }
    Ok(())
}
