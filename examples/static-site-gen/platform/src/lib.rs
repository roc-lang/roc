use core::ffi::c_void;
use libc;
use pulldown_cmark::{html, Options, Parser};
use roc_std::{RocBox, RocStr};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

use syntect::easy::HighlightLines;
use syntect::highlighting::{ThemeSet};
use syntect::html::{ClassStyle, ClassedHTMLGenerator};
use syntect::parsing::SyntaxSet;
use syntect::util::LinesWithEndings;

extern "C" {
    #[link_name = "roc__transformFileContentForHost_1_exposed"]
    fn roc_transformFileContentForHost(relPath: RocBox<RocStr>, content: RocBox<RocStr>) -> RocStr;
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

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_getppid() -> libc::pid_t {
    libc::getppid()
}

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_mmap(
    addr: *mut libc::c_void,
    len: libc::size_t,
    prot: libc::c_int,
    flags: libc::c_int,
    fd: libc::c_int,
    offset: libc::off_t,
) -> *mut libc::c_void {
    libc::mmap(addr, len, prot, flags, fd, offset)
}

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_shm_open(
    name: *const libc::c_char,
    oflag: libc::c_int,
    mode: libc::mode_t,
) -> libc::c_int {
    libc::shm_open(name, oflag, mode as libc::c_uint)
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
pub unsafe extern "C" fn roc_panic(msg: *mut RocStr, tag_id: u32) {
    match tag_id {
        0 => {
            eprintln!("Roc standard library hit a panic: {}", &*msg);
        }
        1 => {
            eprintln!("Application hit a panic: {}", &*msg);
        }
        _ => unreachable!(),
    }
    std::process::exit(1);
}

#[no_mangle]
pub unsafe extern "C" fn roc_dbg(loc: *mut RocStr, msg: *mut RocStr, src: *mut RocStr) {
    eprintln!("[{}] {} = {}", &*loc, &*src, &*msg);
}

#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}

fn run(input_dirname: &str, output_dirname: &str) -> Result<(), String> {
    let input_dir = strip_windows_prefix(
        PathBuf::from(input_dirname)
            .canonicalize()
            .map_err(|e| format!("{}: {}", input_dirname, e))?,
    );

    let output_dir = {
        let dir = PathBuf::from(output_dirname);
        if !dir.exists() {
            fs::create_dir_all(&dir).unwrap();
        }
        strip_windows_prefix(
            dir.canonicalize()
                .map_err(|e| format!("{}: {}", output_dirname, e))?,
        )
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
        match input_file.extension() {
            Some(s) if s.eq("md".into()) => {
                match process_file(&input_dir, &output_dir, &input_file) {
                    Ok(()) => {
                        num_successes += 1;
                    }
                    Err(e) => {
                        eprintln!(
                            "Failed to process file:\n\n  ({:?})with error:\n\n  {}",
                            &input_file, e
                        );
                        num_errors += 1;
                    }
                }
            }
            _ => {}
        };
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
    let mut options = Options::all();

    // In the tutorial, this messes up string literals in <samp> blocks.
    // Those could be done as markdown code blocks, but the repl ones need
    // a special class, and there's no way to add that class using markdown alone.
    //
    // We could make this option user-configurable if people actually want it!
    options.remove(Options::ENABLE_SMART_PUNCTUATION);

    let parser = Parser::new_ext(&content_md, options);

    // We'll build a new vector of events since we can only consume the parser once
    let mut parser_with_highlighting = Vec::new();
    // As we go along, we'll want to highlight code in bundles, not lines
    let mut code_to_highlight = String::new();
    // And track a little bit of state
    let mut in_code_block = false;
    let mut is_roc_code = false;
    let syntax_set: syntect::parsing::SyntaxSet = SyntaxSet::load_defaults_newlines();
    let theme_set: syntect::highlighting::ThemeSet = ThemeSet::load_defaults();

    for event in parser {
        match event {
            pulldown_cmark::Event::Code(code_str) => {
                if code_str.starts_with("roc!") {
                    let stripped = code_str
                        .strip_prefix("roc!")
                        .expect("expected leading 'roc!'");

                    let highlighted_html =
                        roc_highlight::highlight_roc_code_inline(stripped.to_string().as_str());

                    parser_with_highlighting.push(pulldown_cmark::Event::Html(
                        pulldown_cmark::CowStr::from(highlighted_html),
                    ));
                } else {
                    let inline_code =
                        pulldown_cmark::CowStr::from(format!("<code>{}</code>", code_str));
                    parser_with_highlighting.push(pulldown_cmark::Event::Html(inline_code));
                }
            }
            pulldown_cmark::Event::Start(pulldown_cmark::Tag::CodeBlock(cbk)) => {
                in_code_block = true;
                is_roc_code = is_roc_code_block(&cbk);
            }
            pulldown_cmark::Event::End(pulldown_cmark::Tag::CodeBlock(
                pulldown_cmark::CodeBlockKind::Fenced(extention_str),
            )) => {
                if in_code_block {
                    match &code_to_highlight.split(':').collect::<Vec<_>>()[..] {
                        ["file", replacement_file_name, "snippet", snippet_name] => {
                            code_to_highlight = read_replacement_snippet(
                                replacement_file_name.trim(),
                                snippet_name.trim(),
                                input_file,
                            );
                        }
                        ["file", replacement_file_name] => {
                            code_to_highlight =
                                read_replacement_file(replacement_file_name.trim(), input_file);
                        }
                        _ => {}
                    }

                    // Format the whole multi-line code block as HTML all at once
                    let highlighted_html: String;
                    if is_roc_code {
                        highlighted_html = roc_highlight::highlight_roc_code(&code_to_highlight)
                    } else if let Some(syntax) = syntax_set.find_syntax_by_token(&extention_str) {
                        let mut h =
                            HighlightLines::new(syntax, &theme_set.themes["base16-ocean.dark"]);

                        let mut html_generator = ClassedHTMLGenerator::new_with_class_style(
                            syntax,
                            &syntax_set,
                            ClassStyle::Spaced,
                        );
                        for line in LinesWithEndings::from(&code_to_highlight) {
                            html_generator.parse_html_for_line_which_includes_newline(line);
                        }
                        highlighted_html =
                            format!("<pre><samp>{}</pre></samp>", html_generator.finalize())
                    } else {
                        highlighted_html = format!("<pre><samp>{}</pre></samp>", &code_to_highlight)
                    }

                    // And put it into the vector
                    parser_with_highlighting.push(pulldown_cmark::Event::Html(
                        pulldown_cmark::CowStr::from(highlighted_html),
                    ));
                    code_to_highlight = String::new();
                    in_code_block = false;
                }
            }
            pulldown_cmark::Event::Text(t) => {
                if in_code_block {
                    // If we're in a code block, build up the string of text
                    code_to_highlight.push_str(&t);
                } else {
                    parser_with_highlighting.push(pulldown_cmark::Event::Text(t))
                }
            }
            e => {
                parser_with_highlighting.push(e);
            }
        }
    }

    html::push_html(&mut content_html, parser_with_highlighting.into_iter());

    let roc_relpath = RocStr::from(output_relpath.to_str().unwrap());
    let roc_content_html = RocStr::from(content_html.as_str());
    let roc_output_str = unsafe {
        roc_transformFileContentForHost(RocBox::new(roc_relpath), RocBox::new(roc_content_html))
    };

    let output_file = output_dir.join(&output_relpath);
    let rust_output_str: &str = &roc_output_str;

    println!("{} -> {}", input_file.display(), output_file.display());

    // Create parent directory if it doesn't exist
    let parent_dir = output_file.parent().unwrap();
    if !parent_dir.exists() {
        fs::create_dir_all(&parent_dir).unwrap();
    }

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

/// On windows, the path is prefixed with `\\?\`, the "verbatim" prefix.
/// Such a path does not works as an argument to `zig` and other command line tools,
/// and there seems to be no good way to strip it. So we resort to some string manipulation.
pub fn strip_windows_prefix(path_buf: PathBuf) -> std::path::PathBuf {
    #[cfg(not(windows))]
    {
        path_buf
    }

    #[cfg(windows)]
    {
        let path_str = path_buf.display().to_string();

        std::path::Path::new(path_str.trim_start_matches(r"\\?\")).to_path_buf()
    }
}

fn is_roc_code_block(cbk: &pulldown_cmark::CodeBlockKind) -> bool {
    match cbk {
        pulldown_cmark::CodeBlockKind::Indented => false,
        pulldown_cmark::CodeBlockKind::Fenced(cow_str) => cow_str.contains("roc")
    }
}

fn read_replacement_file(replacement_file_name: &str, input_file: &Path) -> String {
    if replacement_file_name.contains("../") {
        panic!(
            "ERROR File \"{}\" must be located within the input diretory.",
            replacement_file_name
        );
    }

    let input_dir = input_file.parent().unwrap();
    let replacement_file_path = input_dir.join(replacement_file_name);

    match fs::read(&replacement_file_path) {
        Ok(content) => String::from_utf8(content).unwrap(),
        Err(err) => {
            panic!(
                "ERROR File \"{}\" is unreadable:\n\t{}",
                replacement_file_path.to_str().unwrap(),
                err
            );
        }
    }
}

fn remove_snippet_comments(input: &str) -> String {
    let line_ending = if input.contains("\r\n") { "\r\n" } else { "\n" };

    input
        .lines()
        .filter(|line| {
            !line.contains("### start snippet") && !line.contains("### end snippet")
        })
        .collect::<Vec<&str>>()
        .join(line_ending)
}

fn read_replacement_snippet(
    replacement_file_name: &str,
    snippet_name: &str,
    input_file: &Path,
) -> String {
    let start_marker = format!("### start snippet {}", snippet_name);
    let end_marker = format!("### end snippet {}", snippet_name);

    let replacement_file_content = read_replacement_file(replacement_file_name.trim(), input_file);

    let start_position = replacement_file_content
        .find(&start_marker)
        .expect(format!("ERROR Failed to find snippet start \"{}\". ", &start_marker).as_str());

    let end_position = replacement_file_content
        .find(&end_marker)
        .expect(format!("ERROR Failed to find snippet end \"{}\". ", &end_marker).as_str());

    if start_position >= end_position {
        let start_position_str = start_position.to_string();
        let end_position_str = end_position.to_string();

        panic!(
            "ERROR Detected start position ({start_position_str}) of snippet \"{snippet_name}\" was greater than or equal to detected end position ({end_position_str})." 
        );
    } else {
        // We want to remove other snippet comments inside this one if they exist.
        remove_snippet_comments(
            &replacement_file_content[start_position + start_marker.len()..end_position]
        )
    }
}
