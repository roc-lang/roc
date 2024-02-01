use std::ffi::OsStr;
use std::io::Write;
use std::path::{Path, PathBuf};

use bumpalo::Bump;
use roc_error_macros::{internal_error, user_error};
use roc_fmt::def::fmt_defs;
use roc_fmt::module::fmt_module;
use roc_fmt::spaces::RemoveSpaces;
use roc_fmt::{Ast, Buf};
use roc_parse::{
    module::{self, parse_module_defs},
    parser::SyntaxError,
    state::State,
};

#[derive(Copy, Clone, Debug)]
pub enum FormatMode {
    WriteToFile,
    WriteToStdout,
    CheckOnly,
}

fn flatten_directories(files: std::vec::Vec<PathBuf>) -> std::vec::Vec<PathBuf> {
    let mut to_flatten = files;
    let mut files = vec![];

    while let Some(path) = to_flatten.pop() {
        if path.is_dir() {
            match path.read_dir() {
                Ok(directory) => {
                    for item in directory {
                        match item {
                            Ok(file) => {
                                let file_path = file.path();
                                if file_path.is_dir() {
                                    to_flatten.push(file_path);
                                } else if is_roc_file(&file_path) {
                                    files.push(file_path);
                                }
                            }

                            Err(error) => internal_error!(
                                "There was an error while trying to read a file from a directory: {:?}",
                                error
                            ),
                        }
                    }
                }

                Err(error) => internal_error!(
                    "There was an error while trying to read the contents of a directory: {:?}",
                    error
                ),
            }
        } else if is_roc_file(&path) {
            files.push(path);
        }
    }

    files
}

fn is_roc_file(path: &Path) -> bool {
    matches!(path.extension().and_then(OsStr::to_str), Some("roc"))
}

pub fn format_files(files: std::vec::Vec<PathBuf>, mode: FormatMode) -> Result<(), String> {
    let arena = Bump::new();

    for file in flatten_directories(files) {
        let src = std::fs::read_to_string(&file).unwrap();

        match format_src(&arena, &src) {
            Ok(buf) => {
                match mode {
                    FormatMode::CheckOnly => {
                        // If we notice that this file needs to be formatted, return early
                        if buf.as_str() != src {
                            return Err("One or more files need to be reformatted.".to_string());
                        }
                    }
                    FormatMode::WriteToFile => {
                        // If all the checks above passed, actually write out the new file.
                        std::fs::write(&file, buf.as_str()).unwrap();
                    }
                    FormatMode::WriteToStdout => {
                        std::io::stdout().lock().write_all(buf.as_bytes()).unwrap()
                    }
                }
            }
            Err(err) => match err {
                FormatProblem::ParsingFailed {
                    formatted_src,
                    parse_err,
                } => {
                    let fail_file = file.with_extension("roc-format-failed");

                    std::fs::write(&fail_file, formatted_src.as_str()).unwrap();

                    internal_error!(
                        "Formatting bug; formatted code isn't valid\n\n\
                        I wrote the incorrect result to this file for debugging purposes:\n{}\n\n\
                        Parse error was: {:?}\n\n",
                        fail_file.display(),
                        parse_err
                    );
                }
                FormatProblem::ReformattingChangedAst {
                    formatted_src,
                    ast_before,
                    ast_after,
                } => {
                    let mut fail_file = file.clone();
                    fail_file.set_extension("roc-format-failed");
                    std::fs::write(&fail_file, formatted_src.as_str()).unwrap();

                    let mut before_file = file.clone();
                    before_file.set_extension("roc-format-failed-ast-before");
                    std::fs::write(&before_file, ast_before).unwrap();

                    let mut after_file = file.clone();
                    after_file.set_extension("roc-format-failed-ast-after");
                    std::fs::write(&after_file, ast_after).unwrap();

                    internal_error!(
                        "Formatting bug; formatting didn't reparse as the same tree\n\n\
                        I wrote the incorrect result to this file for debugging purposes:\n{}\n\n\
                        I wrote the tree before and after formatting to these files for debugging purposes:\n{}\n{}\n\n",
                        fail_file.display(),
                        before_file.display(),
                        after_file.display()
                    );
                }
                FormatProblem::ReformattingUnstable {
                    formatted_src,
                    reformatted_src,
                } => {
                    let mut unstable_1_file = file.clone();
                    unstable_1_file.set_extension("roc-format-unstable-1");
                    std::fs::write(&unstable_1_file, formatted_src).unwrap();

                    let mut unstable_2_file = file.clone();
                    unstable_2_file.set_extension("roc-format-unstable-2");
                    std::fs::write(&unstable_2_file, reformatted_src).unwrap();

                    internal_error!(
                        "Formatting bug; formatting is not stable. Reformatting the formatted file changed it again.\n\n\
                        I wrote the result of formatting to this file for debugging purposes:\n{}\n\n\
                        I wrote the result of double-formatting here:\n{}\n\n",
                        unstable_1_file.display(),
                        unstable_2_file.display()
                    );
                }
            },
        }
    }

    Ok(())
}

#[derive(Debug)]
pub enum FormatProblem {
    ParsingFailed {
        formatted_src: String,
        parse_err: String,
    },
    ReformattingChangedAst {
        formatted_src: String,
        ast_before: String,
        ast_after: String,
    },
    ReformattingUnstable {
        formatted_src: String,
        reformatted_src: String,
    },
}

pub fn format_src(arena: &Bump, src: &str) -> Result<String, FormatProblem> {
    let ast = arena.alloc(parse_all(arena, src).unwrap_or_else(|e| {
        user_error!("Unexpected parse failure when parsing this formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", src, e)
    }));
    let mut buf = Buf::new_in(arena);
    fmt_all(&mut buf, ast);

    let reparsed_ast = match arena.alloc(parse_all(arena, buf.as_str())) {
        Ok(ast) => ast,
        Err(e) => {
            return Err(FormatProblem::ParsingFailed {
                formatted_src: buf.as_str().to_string(),
                parse_err: format!("{:?}", e),
            });
        }
    };

    let ast_normalized = ast.remove_spaces(arena);
    let reparsed_ast_normalized = reparsed_ast.remove_spaces(arena);

    // HACK!
    // We compare the debug format strings of the ASTs, because I'm finding in practice that _somewhere_ deep inside the ast,
    // the PartialEq implementation is returning `false` even when the Debug-formatted impl is exactly the same.
    // I don't have the patience to debug this right now, so let's leave it for another day...
    // TODO: fix PartialEq impl on ast types
    if format!("{ast_normalized:?}") != format!("{reparsed_ast_normalized:?}") {
        return Err(FormatProblem::ReformattingChangedAst {
            formatted_src: buf.as_str().to_string(),
            ast_before: format!("{ast_normalized:#?}\n"),
            ast_after: format!("{reparsed_ast_normalized:#?}\n"),
        });
    }

    // Now verify that the resultant formatting is _stable_ - i.e. that it doesn't change again if re-formatted
    let mut reformatted_buf = Buf::new_in(arena);

    fmt_all(&mut reformatted_buf, reparsed_ast);

    if buf.as_str() != reformatted_buf.as_str() {
        return Err(FormatProblem::ReformattingUnstable {
            formatted_src: buf.as_str().to_string(),
            reformatted_src: reformatted_buf.as_str().to_string(),
        });
    }

    Ok(buf.as_str().to_string())
}

fn parse_all<'a>(arena: &'a Bump, src: &'a str) -> Result<Ast<'a>, SyntaxError<'a>> {
    let (module, state) = module::parse_header(arena, State::new(src.as_bytes()))
        .map_err(|e| SyntaxError::Header(e.problem))?;

    let (module, defs) = module.upgrade_imports(arena);

    let defs = parse_module_defs(arena, state, defs)?;

    Ok(Ast { module, defs })
}

fn fmt_all<'a>(buf: &mut Buf<'a>, ast: &'a Ast) {
    fmt_module(buf, &ast.module);

    buf.newline();

    fmt_defs(buf, &ast.defs, 0);

    buf.fmt_end_of_file();
}
