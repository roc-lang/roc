use std::ffi::OsStr;
use std::io::Write;
use std::ops::Range;
use std::path::{Path, PathBuf};

use bumpalo::{collections::String as BumpString, Bump};
use roc_can::abilities::{IAbilitiesStore, Resolved};
use roc_can::expr::{DeclarationTag, Declarations, Expr};
use roc_error_macros::{internal_error, user_error};
use roc_fmt::def::fmt_defs;
use roc_fmt::header::fmt_header;
use roc_fmt::Buf;
use roc_fmt::MigrationFlags;
use roc_load::{ExecutionMode, FunctionKind, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_module::symbol::{Interns, ModuleId};
use roc_packaging::cache::{self, RocCacheDir};
use roc_parse::ast::{FullAst, SpacesBefore};
use roc_parse::header::parse_module_defs;
use roc_parse::normalize::Normalize;
use roc_parse::{header, parser::SyntaxError, state::State};
use roc_problem::can::RuntimeError;
use roc_region::all::{LineColumn, LineInfo};
use roc_reporting::report::{RenderTarget, DEFAULT_PALETTE};
use roc_target::Target;
use roc_types::subs::{Subs, Variable};

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

pub fn format_files(
    files: std::vec::Vec<PathBuf>,
    mode: FormatMode,
    flags: MigrationFlags,
) -> Result<(), String> {
    let arena = Bump::new();
    let mut files_to_reformat = Vec::new(); // to track which files failed `roc format --check`

    for file in flatten_directories(files) {
        let src = std::fs::read_to_string(&file).unwrap();

        match format_src(&arena, &src, flags) {
            Ok(buf) => {
                match mode {
                    FormatMode::CheckOnly => {
                        // If a file fails `format --check`, add it to the file
                        // list for reporting afterwards.
                        if buf.as_str() != src {
                            files_to_reformat.push(file.display().to_string());
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
    // After processing all files, check if any files failed `format --check`
    if !files_to_reformat.is_empty() {
        let file_list = files_to_reformat.join(", ");
        return Err(format!(
            "The following file(s) failed `roc format --check`:\n\t{}\nYou can fix this with `roc format filename.roc`.",
            file_list
        ));
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

pub fn format_src(arena: &Bump, src: &str, flags: MigrationFlags) -> Result<String, FormatProblem> {
    let ast = arena.alloc(parse_all(arena, src).unwrap_or_else(|e| {
        user_error!("Unexpected parse failure when parsing this formatting:\n\n{src}\n\nParse error was:\n\n{:#?}\n\n", e)
    }));
    let mut buf = Buf::new_in(arena, flags);
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

    let ast_normalized = ast.normalize(arena);
    let reparsed_ast_normalized = reparsed_ast.normalize(arena);

    // HACK!
    // We compare the debug format strings of the ASTs, because I'm finding in practice that _somewhere_ deep inside the ast,
    // the PartialEq implementation is returning `false` even when the Debug-formatted impl is exactly the same.
    // I don't have the patience to debug this right now, so let's leave it for another day...
    // TODO: fix PartialEq impl on ast types
    if !flags.at_least_one_active()
        && format!("{ast_normalized:?}") != format!("{reparsed_ast_normalized:?}")
    {
        return Err(FormatProblem::ReformattingChangedAst {
            formatted_src: buf.as_str().to_string(),
            ast_before: format!("{ast_normalized:#?}\n"),
            ast_after: format!("{reparsed_ast_normalized:#?}\n"),
        });
    }

    // Now verify that the resultant formatting is _stable_ - i.e. that it doesn't change again if re-formatted
    let mut reformatted_buf = Buf::new_in(arena, flags);

    fmt_all(&mut reformatted_buf, reparsed_ast);

    if buf.as_str() != reformatted_buf.as_str() {
        return Err(FormatProblem::ReformattingUnstable {
            formatted_src: buf.as_str().to_string(),
            reformatted_src: reformatted_buf.as_str().to_string(),
        });
    }

    Ok(buf.as_str().to_string())
}

fn parse_all<'a>(arena: &'a Bump, src: &'a str) -> Result<FullAst<'a>, SyntaxError<'a>> {
    let (header, state) = header::parse_header(arena, State::new(src.as_bytes()))
        .map_err(|e| SyntaxError::Header(e.problem))?;

    let (h, defs) = header.item.upgrade_header_imports(arena);

    let defs = parse_module_defs(arena, state, defs)?;

    Ok(FullAst {
        header: SpacesBefore {
            before: header.before,
            item: h,
        },
        defs,
    })
}

fn fmt_all<'a>(buf: &mut Buf<'a>, ast: &'a FullAst) {
    fmt_header(buf, &ast.header);

    fmt_defs(buf, &ast.defs, 0);

    buf.fmt_end_of_file();
}

#[derive(Debug)]
pub enum AnnotationProblem<'a> {
    Loading(LoadingProblem<'a>),
    Type(TypeProblem),
}

#[derive(Debug)]
pub struct TypeProblem {
    pub name: String,
    pub position: LineColumn,
}

pub fn annotate_file(arena: &Bump, file: PathBuf) -> Result<(), AnnotationProblem> {
    let load_config = LoadConfig {
        target: Target::default(),
        function_kind: FunctionKind::from_env(),
        render: RenderTarget::ColorTerminal,
        palette: DEFAULT_PALETTE,
        threading: Threading::AllAvailable,
        exec_mode: ExecutionMode::Check,
    };

    let mut loaded = roc_load::load_and_typecheck(
        arena,
        file.clone(),
        None,
        RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
        load_config,
    )
    .map_err(AnnotationProblem::Loading)?;

    let buf = annotate_module(arena, &mut loaded)?;

    std::fs::write(&file, buf.as_str())
        .unwrap_or_else(|e| internal_error!("failed to write annotated file to {file:?}: {e}"));

    Ok(())
}

fn annotate_module<'a>(
    arena: &'a Bump,
    loaded: &mut LoadedModule,
) -> Result<BumpString<'a>, AnnotationProblem<'a>> {
    let (decls, subs, abilities) =
        if let Some(decls) = loaded.declarations_by_id.get(&loaded.module_id) {
            let subs = loaded.solved.inner_mut();
            let abilities = &loaded.abilities_store;

            (decls, subs, abilities)
        } else if let Some(checked) = loaded.typechecked.get_mut(&loaded.module_id) {
            let decls = &checked.decls;
            let subs = checked.solved_subs.inner_mut();
            let abilities = &checked.abilities_store;

            (decls, subs, abilities)
        } else {
            internal_error!("Could not find file's module");
        };

    let src = &loaded
        .sources
        .get(&loaded.module_id)
        .unwrap_or_else(|| internal_error!("Could not find the file's source"))
        .1;

    let mut edits = annotation_edits(
        decls,
        subs,
        abilities,
        src,
        loaded.module_id,
        &loaded.interns,
    )
    .map_err(AnnotationProblem::Type)?;
    edits.sort_by_key(|(offset, _)| *offset);

    let mut buffer = BumpString::new_in(arena);
    let mut file_progress = 0;

    for (position, edit) in edits {
        buffer.push_str(&src[file_progress..position]);
        buffer.push_str(&edit);

        file_progress = position;
    }
    buffer.push_str(&src[file_progress..]);

    Ok(buffer)
}

pub fn annotation_edits(
    decls: &Declarations,
    subs: &Subs,
    abilities: &IAbilitiesStore<Resolved>,
    src: &str,
    module_id: ModuleId,
    interns: &Interns,
) -> Result<Vec<(usize, String)>, TypeProblem> {
    let mut edits = Vec::with_capacity(decls.len());

    for (index, tag) in decls.iter_bottom_up() {
        let var = decls.variables[index];
        let symbol = decls.symbols[index];
        let expr = &decls.expressions[index].value;

        if decls.annotations[index].is_some()
            | matches!(
                *expr,
                Expr::RuntimeError(RuntimeError::ExposedButNotDefined(..)) | Expr::ImportParams(..)
            )
            | abilities.is_specialization_name(symbol.value)
            | matches!(tag, DeclarationTag::MutualRecursion { .. })
        {
            continue;
        }

        let byte_range = match tag {
            DeclarationTag::Destructure(i) => decls.destructs[i.index()].loc_pattern.byte_range(),
            _ => symbol.byte_range(),
        };

        let edit = annotation_edit(src, subs, interns, module_id, var, byte_range)?;

        edits.push(edit);
    }
    Ok(edits)
}

pub fn annotation_edit(
    src: &str,
    subs: &Subs,
    interns: &Interns,
    module_id: ModuleId,
    var: Variable,
    symbol_range: Range<usize>,
) -> Result<(usize, String), TypeProblem> {
    let symbol_str = &src[symbol_range.clone()];
    if subs.var_contains_error(var) {
        let line_info = LineInfo::new(src);
        let position = line_info.convert_offset(symbol_range.start as u32);
        return Err(TypeProblem {
            name: symbol_str.to_owned(),
            position,
        });
    }

    let signature = roc_types::pretty_print::name_and_print_var(
        var,
        &mut subs.clone(),
        module_id,
        interns,
        roc_types::pretty_print::DebugPrint::NOTHING,
    );

    let line_start = src[..symbol_range.start]
        .rfind('\n')
        .map_or(symbol_range.start, |pos| pos + 1);
    let indent = src[line_start..]
        .split_once(|c: char| !c.is_ascii_whitespace())
        .map_or("", |pair| pair.0);

    let edit = format!("{indent}{symbol_str} : {signature}\n");

    Ok((line_start, edit))
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use std::fs::{read_to_string, File};
    use std::io::Write;
    use tempfile::{tempdir, TempDir};

    const FORMATTED_ROC: &str = r#"app [main] { pf: platform "platform/main.roc" }

import pf.Stdout
import pf.Stdin

main =
    Stdout.line! "What's your name?"
    name = Stdin.line!
    Stdout.line! "Hi ${name}!""#;

    const UNFORMATTED_ROC: &str = r#"app [main] { pf: platform "platform/main.roc" }

main =
        Stdout.line! "What's your name?"
        name = Stdin.line!
        Stdout.line! "Hi ${name}!"
"#;

    fn setup_test_file(dir: &Path, file_name: &str, contents: &str) -> PathBuf {
        let file_path = dir.join(file_name);
        let mut file = File::create(&file_path).unwrap();
        writeln!(file, "{}", contents).unwrap();
        file.flush().unwrap();
        file_path
    }

    fn cleanup_temp_dir(dir: TempDir) {
        let result = dir.close();
        assert!(result.is_ok(), "Failed to delete temp directory");
    }

    #[test]
    fn test_single_file_needs_reformatting() {
        let dir = tempdir().unwrap();
        let file_path = setup_test_file(dir.path(), "test1.roc", UNFORMATTED_ROC);
        let flags = MigrationFlags {
            snakify: false,
            parens_and_commas: false,
        };

        let result = format_files(vec![file_path.clone()], FormatMode::CheckOnly, flags);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            format!(
                "The following file(s) failed `roc format --check`:\n\t{}\nYou can fix this with `roc format filename.roc`.",
                &file_path.as_path().to_str().unwrap()
            )
        );

        cleanup_temp_dir(dir);
    }

    #[test]
    fn test_multiple_files_needs_reformatting() {
        let dir = tempdir().unwrap();
        let file1 = setup_test_file(dir.path(), "test1.roc", UNFORMATTED_ROC);
        let file2 = setup_test_file(dir.path(), "test2.roc", UNFORMATTED_ROC);
        let flags = MigrationFlags {
            snakify: false,
            parens_and_commas: false,
        };

        let result = format_files(vec![file1, file2], FormatMode::CheckOnly, flags);
        assert!(result.is_err());
        let error_message = result.unwrap_err();
        assert!(error_message.contains("test1.roc") && error_message.contains("test2.roc"));

        cleanup_temp_dir(dir);
    }

    #[test]
    fn test_no_files_need_reformatting() {
        let dir = tempdir().unwrap();
        let file_path = setup_test_file(dir.path(), "formatted.roc", FORMATTED_ROC);
        let flags = MigrationFlags {
            snakify: false,
            parens_and_commas: false,
        };

        let result = format_files(vec![file_path], FormatMode::CheckOnly, flags);
        assert!(result.is_ok());

        cleanup_temp_dir(dir);
    }

    #[test]
    fn test_some_files_need_reformatting() {
        let dir = tempdir().unwrap();
        let file_formatted = setup_test_file(dir.path(), "formatted.roc", FORMATTED_ROC);
        let file1_unformated = setup_test_file(dir.path(), "test1.roc", UNFORMATTED_ROC);
        let file2_unformated = setup_test_file(dir.path(), "test2.roc", UNFORMATTED_ROC);
        let flags = MigrationFlags {
            snakify: false,
            parens_and_commas: false,
        };

        let result = format_files(
            vec![file_formatted, file1_unformated, file2_unformated],
            FormatMode::CheckOnly,
            flags,
        );
        assert!(result.is_err());
        let error_message = result.unwrap_err();
        assert!(error_message.contains("test1.roc") && error_message.contains("test2.roc"));
        assert!(!error_message.contains("formatted.roc"));

        cleanup_temp_dir(dir);
    }

    const HEADER: &str = indoc! {r#"
        interface Test
            exposes []
            imports []

    "#};

    fn annotate_string(before: String) -> String {
        let dir = tempdir().unwrap();
        let file_path = setup_test_file(dir.path(), "before.roc", &before);

        let arena = Bump::new();
        let result = annotate_file(&arena, file_path.clone());
        result.unwrap();

        let annotated = read_to_string(file_path).unwrap();

        cleanup_temp_dir(dir);
        annotated
    }

    #[test]
    fn test_annotate_simple() {
        let before = HEADER.to_string()
            + indoc! {r#"
            main =
                "Hello, World!""#};

        let after = HEADER.to_string()
            + indoc! {r#"
            main : Str
            main =
                "Hello, World!"
            "#};

        let annotated = annotate_string(before);

        assert_eq!(annotated, after);
    }

    #[test]
    fn test_annotate_empty() {
        let before = HEADER.to_string();
        let after = HEADER.to_string() + "\n";
        let annotated = annotate_string(before);

        assert_eq!(annotated, after);
    }

    #[test]
    fn test_annotate_destructure() {
        let before = HEADER.to_string()
            + indoc! {r#"
            {a, b} = {a: "zero", b: (1, 2)}

            main = a"#};

        let after = HEADER.to_string()
            + indoc! {r#"
            {a, b} : { a : Str, b : ( Num *, Num * )* }
            {a, b} = {a: "zero", b: (1, 2)}

            main : Str
            main = a
            "#};

        let annotated = annotate_string(before);

        assert_eq!(annotated, after);
    }
}
