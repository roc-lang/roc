use std::{
    error::Error,
    ffi::OsStr,
    fs, io,
    path::{Path, PathBuf},
    process::Command,
};

use lazy_static::lazy_static;
use libtest_mimic::{run, Arguments, Failed, Trial};
use mono::MonoOptions;
use regex::Regex;
use roc_collections::VecMap;
use roc_solve::FunctionKind;
use test_solve_helpers::{
    infer_queries, Elaboration, InferOptions, InferredProgram, InferredQuery, MUTLILINE_MARKER,
};

mod mono;

fn main() -> Result<(), Box<dyn Error>> {
    let args = Arguments::from_args();

    let test_files = collect_uitest_files()?;
    let tests = test_files
        .into_iter()
        .map(into_test)
        .collect::<Result<_, _>>()?;

    run(&args, tests).exit()
}

lazy_static! {
    static ref UITEST_PATH: PathBuf = PathBuf::from(std::env!("ROC_WORKSPACE_DIR"))
        .join("crates")
        .join("compiler")
        .join("uitest")
        .join("tests");

    /// # +set <setting>
    static ref RE_SETTING: Regex =
        Regex::new(r"# \+set (?P<setting>.*?)=(?P<value>.*)").unwrap();

    /// # +opt can:<opt>
    static ref RE_OPT_CAN: Regex =
        Regex::new(r"# \+opt can:(?P<opt>.*)").unwrap();

    /// # +opt infer:<opt>
    static ref RE_OPT_INFER: Regex =
        Regex::new(r"# \+opt infer:(?P<opt>.*)").unwrap();

    /// # +opt mono:<opt>
    static ref RE_OPT_MONO: Regex =
        Regex::new(r"# \+opt mono:(?P<opt>.*)").unwrap();

    /// # +emit:<opt>
    static ref RE_EMIT: Regex =
        Regex::new(r"# \+emit:(?P<opt>.*)").unwrap();

    /// ## module <name>
    static ref RE_MODULE: Regex =
        Regex::new(r#"## module (?P<name>.*)"#).unwrap();
}

fn collect_uitest_files() -> io::Result<Vec<PathBuf>> {
    let mut tests = Vec::with_capacity(200);

    let mut dirs_to_visit = vec![UITEST_PATH.clone()];
    while let Some(dir) = dirs_to_visit.pop() {
        for entry in fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            let entry_type = entry.file_type()?;
            if entry_type.is_dir() {
                dirs_to_visit.push(path);
                continue;
            }

            if path.extension() == Some(OsStr::new("txt")) {
                // see issue 6947
                if cfg!(debug_assertions)
                    && path.to_string_lossy().contains(
                        "match_on_result_with_uninhabited_error_destructuring_in_lambda_syntax",
                    )
                {
                    continue;
                }
                tests.push(path);
            }
        }
    }

    Ok(tests)
}

fn into_test(path: PathBuf) -> io::Result<Trial> {
    let name = path
        .strip_prefix(UITEST_PATH.as_path().parent().unwrap())
        .expect("collected path does not have uitest prefix")
        .display()
        .to_string();

    let trial = Trial::test(name, move || run_test(path));
    Ok(trial)
}

fn run_test(path: PathBuf) -> Result<(), Failed> {
    let data = std::fs::read_to_string(&path)?;
    let TestCase {
        compiler_settings,
        can_options,
        infer_options,
        emit_options,
        mono_options,
        program,
    } = TestCase::parse(&data)?;

    let inferred_program = infer_queries(
        program.test_module,
        program
            .other_modules
            .iter()
            .map(|(md, src)| (&**md, &**src)),
        infer_options,
        can_options.allow_errors,
        compiler_settings.function_kind,
    )?;

    {
        let mut fd = fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .open(&path)?;

        assemble_query_output(
            &mut fd,
            program,
            inferred_program,
            compiler_settings,
            can_options,
            mono_options,
            emit_options,
        )?;
    }

    check_for_changes(&path)?;

    Ok(())
}

const EMIT_HEADER: &str = "# -emit:";

struct Modules<'a> {
    before_any: &'a str,
    other_modules: VecMap<&'a str, &'a str>,
    test_module: &'a str,
}

struct TestCase<'a> {
    compiler_settings: CompilerSettings,
    can_options: CanOptions,
    infer_options: InferOptions,
    mono_options: MonoOptions,
    emit_options: EmitOptions,
    program: Modules<'a>,
}

struct CompilerSettings {
    function_kind: FunctionKind,
}

impl Default for CompilerSettings {
    fn default() -> Self {
        Self {
            function_kind: FunctionKind::LambdaSet,
        }
    }
}

#[derive(Default)]
struct CanOptions {
    allow_errors: bool,
}

#[derive(Default)]
struct EmitOptions {
    can_decls: bool,
    mono: bool,
}

impl<'a> TestCase<'a> {
    fn parse(mut data: &'a str) -> Result<Self, Failed> {
        // Drop anything following `# -emit:` header lines; that's the output.
        if let Some(drop_at) = data.find(EMIT_HEADER) {
            data = data[..drop_at].trim_end();
        }

        let compiler_settings = Self::parse_compiler_settings(data)?;
        let can_options = Self::parse_can_options(data)?;
        let infer_options = Self::parse_infer_options(data)?;
        let mono_options = Self::parse_mono_options(data)?;
        let emit_options = Self::parse_emit_options(data)?;

        let program = Self::parse_modules(data);

        Ok(TestCase {
            compiler_settings,
            can_options,
            infer_options,
            mono_options,
            emit_options,
            program,
        })
    }

    fn parse_modules(data: &'a str) -> Modules<'a> {
        let mut module_starts = RE_MODULE.captures_iter(data).peekable();

        let first_module_start = match module_starts.peek() {
            None => {
                // This is just a single module with no name; it is the test module.
                return Modules {
                    before_any: Default::default(),
                    other_modules: Default::default(),
                    test_module: data,
                };
            }
            Some(p) => p.get(0).unwrap().start(),
        };

        let before_any = data[..first_module_start].trim();

        let mut test_module = None;
        let mut other_modules = VecMap::new();

        while let Some(module_start) = module_starts.next() {
            let module_name = module_start.name("name").unwrap().as_str();
            let module_start = module_start.get(0).unwrap().end();
            let module = match module_starts.peek() {
                None => &data[module_start..],
                Some(next_module_start) => {
                    let module_end = next_module_start.get(0).unwrap().start();
                    &data[module_start..module_end]
                }
            }
            .trim();

            if module_name == "Test" {
                test_module = Some(module);
            } else {
                other_modules.insert(module_name, module);
            }
        }

        let test_module = test_module.expect("no Test module found");
        Modules {
            before_any,
            other_modules,
            test_module,
        }
    }

    fn parse_compiler_settings(data: &str) -> Result<CompilerSettings, Failed> {
        let mut settings = CompilerSettings::default();

        let found_settings = RE_SETTING.captures_iter(data);
        for set in found_settings {
            let setting = set.name("setting").unwrap().as_str();
            let value = set.name("value").unwrap().as_str();
            match setting.trim() {
                "function_kind" => match value.trim() {
                    "lambda_set" => settings.function_kind = FunctionKind::LambdaSet,
                    "erased" => settings.function_kind = FunctionKind::Erased,
                    other => return Err(format!("unknown function kind: {other:?}").into()),
                },
                other => return Err(format!("unknown compiler setting: {other:?}").into()),
            }
        }

        Ok(settings)
    }

    fn parse_can_options(data: &str) -> Result<CanOptions, Failed> {
        let mut can_opts = CanOptions::default();

        let found_can_opts = RE_OPT_CAN.captures_iter(data);
        for can_opt in found_can_opts {
            let opt = can_opt.name("opt").unwrap().as_str();
            match opt.trim() {
                "allow_errors" => can_opts.allow_errors = true,
                other => return Err(format!("unknown can option: {other:?}").into()),
            }
        }

        Ok(can_opts)
    }

    fn parse_infer_options(data: &str) -> Result<InferOptions, Failed> {
        let mut infer_opts = InferOptions {
            no_promote: true,
            ..Default::default()
        };

        let found_infer_opts = RE_OPT_INFER.captures_iter(data);
        for infer_opt in found_infer_opts {
            let opt = infer_opt.name("opt").unwrap().as_str();
            match opt.trim() {
                "allow_errors" => infer_opts.allow_errors = true,
                "print_only_under_alias" => infer_opts.print_only_under_alias = true,
                "print_ranks" => infer_opts.print_ranks = true,
                "print_variables" => infer_opts.print_variables = true,
                other => return Err(format!("unknown infer option: {other:?}").into()),
            }
        }

        Ok(infer_opts)
    }

    fn parse_mono_options(data: &str) -> Result<MonoOptions, Failed> {
        let mut mono_opts = MonoOptions::default();

        let found_infer_opts = RE_OPT_MONO.captures_iter(data);
        for infer_opt in found_infer_opts {
            let opt = infer_opt.name("opt").unwrap().as_str();
            match opt.trim() {
                "no_check" => mono_opts.no_check = true,
                other => return Err(format!("unknown mono option: {other:?}").into()),
            }
        }

        Ok(mono_opts)
    }

    fn parse_emit_options(data: &str) -> Result<EmitOptions, Failed> {
        let mut emit_opts = EmitOptions::default();

        let found_infer_opts = RE_EMIT.captures_iter(data);
        for infer_opt in found_infer_opts {
            let opt = infer_opt.name("opt").unwrap().as_str();
            match opt.trim() {
                "can_decls" => emit_opts.can_decls = true,
                "mono" => emit_opts.mono = true,
                other => return Err(format!("unknown emit option: {other:?}").into()),
            }
        }

        Ok(emit_opts)
    }
}

fn check_for_changes(path: &Path) -> Result<(), Failed> {
    Command::new("git").args(["add", "-N"]).arg(path).output()?;

    let has_changes = Command::new("git")
        .args(["diff", "--color=always"])
        .arg(path)
        .output()?;

    if !has_changes.stdout.is_empty() {
        return Err(format!(
            "{}\nOutput has changed. If it looks okay, `git` add the file.",
            std::str::from_utf8(&has_changes.stdout)?
        )
        .into());
    }

    Ok(())
}

/// Assemble the output for a test, with queries elaborated in-line.
fn assemble_query_output(
    writer: &mut impl io::Write,
    program: Modules<'_>,
    inferred_program: InferredProgram,
    compiler_settings: CompilerSettings,
    can_options: CanOptions,
    mono_options: MonoOptions,
    emit_options: EmitOptions,
) -> io::Result<()> {
    let Modules {
        before_any,
        other_modules,
        test_module,
    } = program;

    if !before_any.is_empty() {
        writeln!(writer, "{before_any}\n")?;
    }

    for (module, source) in other_modules.iter() {
        writeln!(writer, "## module {module}")?;
        writeln!(writer, "{source}\n")?;
    }

    if !other_modules.is_empty() {
        writeln!(writer, "## module Test")?;
    }

    // Reverse the queries so that we can pop them off the end as we pass through the lines.
    let (queries, program) = inferred_program.decompose();
    let mut sorted_queries = queries.into_sorted();
    sorted_queries.reverse();

    let mut reflow = Reflow::new_unindented(writer);
    write_source_with_answers(&mut reflow, test_module, sorted_queries, 0)?;

    // Finish up with any remaining emit options we were asked to provide.
    let EmitOptions { can_decls, mono } = emit_options;
    if can_decls {
        writeln!(writer, "\n{EMIT_HEADER}can_decls")?;
        program.write_can_decls(writer)?;
    }

    if mono {
        writeln!(writer, "\n{EMIT_HEADER}mono")?;
        // Unfortunately, with the current setup we must now recompile into the IR.
        // TODO: extend the data returned by a monomorphized module to include
        // that of a solved module.
        mono::write_compiled_ir(
            writer,
            test_module,
            other_modules,
            mono_options,
            compiler_settings,
            can_options.allow_errors,
        )?;
    }

    Ok(())
}

fn write_source_with_answers<W: io::Write>(
    reflow: &mut Reflow<'_, W>,
    source: &str,
    mut sorted_queries: Vec<InferredQuery>,
    offset_line: usize,
) -> io::Result<()> {
    for (i, line) in source.lines().enumerate() {
        let i = i + offset_line;

        let mut is_query_line = false;

        // Write all elaborated query lines if applicable.
        while matches!(
            sorted_queries.last(),
            Some(InferredQuery {
                source_line_column,
                ..
            }) if source_line_column.line == i as u32
        ) {
            let inferred = sorted_queries.pop().unwrap();

            reflow.scoped(|reflow| reconstruct_comment_line(reflow, inferred))?;

            reflow.write("\n")?;

            is_query_line = true;
        }

        // If this was previously a multi-line query output line, skip it, since we already wrote
        // the new output above.
        if line.contains(MUTLILINE_MARKER) {
            continue;
        }

        // Otherwise, write the Roc source line.
        if !is_query_line {
            reflow.write(line.trim_end())?;
            reflow.write("\n")?;
        }
    }

    let mut sorted_queries = sorted_queries.into_iter().peekable();
    while let Some(sorted_query) = sorted_queries.next() {
        reflow.scoped(|reflow| reconstruct_comment_line(reflow, sorted_query))?;

        // Only write a newline if we're not yet at the end of the source.
        // Otherwise, a newline will be written for us after exiting the reconstruction of the
        // comment line, since this must happen in the reconsutrction of a multi-line query.
        if sorted_queries.peek().is_some() {
            reflow.write("\n")?;
        }
    }

    Ok(())
}

fn reconstruct_comment_line<W: io::Write>(
    reflow: &mut Reflow<'_, W>,
    inferred: InferredQuery,
) -> io::Result<()> {
    let InferredQuery {
        comment_column,
        source_line_column,
        source,
        elaboration,
    } = inferred;

    reflow.add_layer(comment_column as _, source_line_column.column as _);
    reflow.write_and_bump(&format!("{source} "))?;

    match elaboration {
        Elaboration::Specialization {
            specialized_name,
            typ,
        } => {
            reflow.write_and_bump(&format!("{specialized_name}: "))?;
            reflow.write(&typ)
        }
        Elaboration::Source { source: _, typ } => reflow.write(&typ),
        Elaboration::Instantiation {
            typ,
            source,
            offset_line,
            queries_in_instantiation,
        } => {
            reflow.write(&typ)?;

            // Write the source on new line, but at the reflow column the comment is aligned at.
            reflow.set_content(source_line_column.column as _);
            reflow.write("\n")?;

            let queries = queries_in_instantiation.into_sorted();

            write_source_with_answers(reflow, source.trim_end(), queries, offset_line as _)
        }
    }
}

struct Reflow<'a, W: io::Write> {
    writer: &'a mut W,
    state: ReflowState,
}

#[derive(Clone, Debug)]
struct ReflowState {
    /// true if the first line of the elaboration comment has been written.
    top_line_written: bool,
    /// Number of `content columns` prefixes written.
    /// If this equals the number of content columns, the whole prefix for a line has been written.
    content_prefixes_written: usize,
    /// The column at which to insert the comment prefix "#".
    comment_column: usize,
    /// The columns at which content occurs.
    /// If the stack is >1, then
    ///   - at the first content column, the [MUTLILINE_MARKER] may be written as appropriate
    ///   - for subsequent columns, spaces are inserted until the column is reached.
    content_columns: Vec<usize>,
}

impl<'a, W: io::Write> std::ops::Deref for Reflow<'a, W> {
    type Target = ReflowState;

    fn deref(&self) -> &Self::Target {
        &self.state
    }
}

impl<'a, W: io::Write> std::ops::DerefMut for Reflow<'a, W> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.state
    }
}

impl<'a, W: io::Write> Reflow<'a, W> {
    fn new_unindented(writer: &'a mut W) -> Self {
        Self {
            writer,
            state: ReflowState {
                top_line_written: false,
                content_prefixes_written: 0,
                comment_column: 0,
                content_columns: vec![],
            },
        }
    }

    fn scoped<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let state = self.state.clone();
        let result = f(self);
        self.state = state;
        result
    }

    fn add_layer(&mut self, comment_column: usize, content_column: usize) {
        if self.comment_column == 0 {
            // If the comment column is not yet set, this is the top-level and we should update the
            // state; otherwise, we already have a comment column, only add to the content-ful
            // layer.
            self.comment_column = comment_column;
        }
        self.content_columns.push(content_column);
    }

    fn set_content(&mut self, content_column: usize) {
        let latest_column = self
            .content_columns
            .last_mut()
            .expect("cannot set content before adding a layer");
        *latest_column = content_column;
    }

    fn write(&mut self, content: &str) -> io::Result<()> {
        for (i, content_line) in content.split('\n').enumerate() {
            if i > 0 {
                // new line
                writeln!(self.writer)?;
                self.content_prefixes_written = 0;
            }

            // If the content columns are empty, this is top-level and we
            // have no prefix to write.
            if self.content_prefixes_written != self.content_columns.len() {
                if self.content_prefixes_written == 0 {
                    self.write_n_spaces(self.comment_column)?;
                    write!(self.writer, "#")?;

                    // For the first column content - write spaces up to the column, and then if we are
                    // in a multiline context, add the multi-line marker.
                    {
                        self.write_n_spaces(self.content_columns[0] - self.comment_column - 1)?;

                        if self.top_line_written {
                            write!(self.writer, "{MUTLILINE_MARKER} ")?;
                        }
                    }

                    self.content_prefixes_written = 1;
                }

                // For all remaining content columns, fill them in with spaces.
                let remaining_content_columns = self
                    .content_columns
                    .iter()
                    .skip(self.content_prefixes_written);
                self.write_n_spaces(remaining_content_columns.sum())?;

                self.content_prefixes_written = self.content_columns.len();
                self.top_line_written = true;
            }

            write!(self.writer, "{content_line}")?;
        }

        Ok(())
    }

    fn write_and_bump(&mut self, content: &str) -> io::Result<()> {
        assert!(
            content.lines().count() == 1,
            "cannot bump with multi-line content"
        );

        self.write(content)?;

        let column = self
            .content_columns
            .last_mut()
            .expect("cannot write_and_bump before adding layer");
        *column += content.len();

        Ok(())
    }

    fn write_n_spaces(&mut self, n: usize) -> io::Result<()> {
        for _ in 0..n {
            write!(self.writer, " ")?;
        }
        Ok(())
    }
}
