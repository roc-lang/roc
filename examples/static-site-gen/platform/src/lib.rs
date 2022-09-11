use core::ffi::c_void;
use libc;
use pulldown_cmark::{html, Parser};
use roc_std::RocStr;
use std::env;
use std::ffi::CStr;
use std::fs;
use std::os::raw::c_char;
use std::path::{Path, PathBuf};

extern "C" {
    #[link_name = "roc__transformFileContentForHost_1_exposed"]
    fn roc_transformFileContentForHost(relPath: &RocStr, content: &RocStr) -> RocStr;
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

    if !input_dir.exists() {
        return Err(format!("{} does not exist. The first argument should be the directory where your Markdown files are.", input_dir.display()));
    }

    if !output_dir.exists() {
        return Err(format!("Could not create {}", output_dir.display()));
    }

    let mut input_files: Vec<PathBuf> = vec![];
    find_files(&input_dir, &mut input_files)
        .map_err(|e| format!("Error finding input files: {}", e))?;

    println!("Processing {} input files...", input_files.len());

    // TODO: process the files asynchronously
    let num_files = input_files.len();
    let mut num_errors = 0;
    let mut num_successes = 0;
    for input_file in input_files {
        match process_file(&input_dir, &output_dir, &input_file) {
            Ok(()) => {
                num_successes += 1;
            }
            Err(e) => {
                eprintln!("{}", e);
                num_errors += 1;
            }
        }
    }

    println!(
        "Processed {} files with {} successes and {} errors",
        num_files, num_successes, num_errors
    );

    if num_errors > 0 {
        Err("Could not process all files".into())
    } else {
        Ok(())
    }
}

fn process_file(input_dir: &Path, output_dir: &Path, input_file: &Path) -> Result<(), String> {
    match input_file.extension() {
        Some(s) if s.eq("md".into()) => {}
        _ => return Err("Only .md files are supported".into()),
    };

    let input_relpath = input_file
        .strip_prefix(input_dir)
        .map_err(|e| e.to_string())?
        .to_path_buf();

    let mut output_relpath = input_relpath.clone();
    output_relpath.set_extension("html");

    let content_md = fs::read_to_string(input_file).map_err(|e| {
        format!(
            "Error reading {}: {}",
            input_file.to_str().unwrap_or("an input file"),
            e
        )
    })?;

    let mut content_html = String::new();
    let parser = Parser::new(&content_md);
    html::push_html(&mut content_html, parser);

    let roc_relpath = RocStr::from(output_relpath.to_str().unwrap());
    let roc_content_html = RocStr::from(content_html.as_str());
    let roc_output_str =
        unsafe { roc_transformFileContentForHost(&roc_relpath, &roc_content_html) };

    let output_file = output_dir.join(&output_relpath);
    let rust_output_str: &str = &roc_output_str;

    println!("{} -> {}", input_file.display(), output_file.display());

    fs::write(output_file, rust_output_str).map_err(|e| format!("{}", e))
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
