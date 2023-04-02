use std::{
    error::Error,
    ffi::OsStr,
    fs, io,
    path::{Path, PathBuf},
    process::Command,
};

use lazy_static::lazy_static;
use libtest_mimic::{run, Arguments, Failed, Trial};
use regex::Regex;
use test_solve_helpers::{
    infer_queries, Elaboration, InferOptions, InferredProgram, InferredQuery, MUTLILINE_MARKER,
};

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
        .join("solve")
        .join("tests")
        .join("uitest");

    /// # +opt infer:<opt>
    static ref RE_OPT_INFER: Regex =
        Regex::new(r#"# \+opt infer:(?P<opt>.*)"#).unwrap();

    /// # +opt print:<opt>
    static ref RE_OPT_PRINT: Regex =
        Regex::new(r#"# \+opt print:(?P<opt>.*)"#).unwrap();
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
        infer_options,
        print_options,
        source,
    } = TestCase::parse(data)?;

    let inferred_program = infer_queries(&source, infer_options)?;

    {
        let mut fd = fs::OpenOptions::new()
            .write(true)
            .truncate(true)
            .open(&path)?;

        assemble_query_output(&mut fd, &source, inferred_program, print_options)?;
    }

    check_for_changes(&path)?;

    Ok(())
}

const EMIT_HEADER: &str = "# -emit:";

struct TestCase {
    infer_options: InferOptions,
    print_options: PrintOptions,
    source: String,
}

#[derive(Default)]
struct PrintOptions {
    can_decls: bool,
}

impl TestCase {
    fn parse(mut data: String) -> Result<Self, Failed> {
        // Drop anything following `# -emit:` header lines; that's the output.
        if let Some(drop_at) = data.find(EMIT_HEADER) {
            data.truncate(drop_at);
            data.truncate(data.trim_end().len());
        }

        Ok(TestCase {
            infer_options: Self::parse_infer_options(&data)?,
            print_options: Self::parse_print_options(&data)?,
            source: data,
        })
    }

    fn parse_infer_options(data: &str) -> Result<InferOptions, Failed> {
        let mut infer_opts = InferOptions {
            no_promote: true,
            ..Default::default()
        };

        let found_infer_opts = RE_OPT_INFER.captures_iter(data);
        for infer_opt in found_infer_opts {
            let opt = infer_opt.name("opt").unwrap().as_str();
            match opt {
                "allow_errors" => infer_opts.allow_errors = true,
                "print_only_under_alias" => infer_opts.print_only_under_alias = true,
                other => return Err(format!("unknown infer option: {other}").into()),
            }
        }

        Ok(infer_opts)
    }

    fn parse_print_options(data: &str) -> Result<PrintOptions, Failed> {
        let mut print_opts = PrintOptions::default();

        let found_infer_opts = RE_OPT_PRINT.captures_iter(data);
        for infer_opt in found_infer_opts {
            let opt = infer_opt.name("opt").unwrap().as_str();
            match opt {
                "can_decls" => print_opts.can_decls = true,
                other => return Err(format!("unknown print option: {other}").into()),
            }
        }

        Ok(print_opts)
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
    source: &str,
    inferred_program: InferredProgram,
    print_options: PrintOptions,
) -> io::Result<()> {
    // Reverse the queries so that we can pop them off the end as we pass through the lines.
    let (queries, program) = inferred_program.decompose();
    let mut sorted_queries = queries.into_sorted();
    sorted_queries.reverse();

    let mut reflow = Reflow::new_unindented(writer);
    write_source_with_answers(&mut reflow, source, sorted_queries, 0)?;

    // Finish up with any remaining print options we were asked to provide.
    let PrintOptions { can_decls } = print_options;
    if can_decls {
        writeln!(writer, "\n{EMIT_HEADER}can_decls")?;
        program.write_can_decls(writer)?;
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
            }) if source_line_column.line == i as _
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
            mut queries_in_instantiation,
        } => {
            reflow.write(&typ)?;

            // Write the source on new line, but at the reflow column the comment is aligned at.
            reflow.set_content(source_line_column.column as _);
            reflow.write("\n")?;
            queries_in_instantiation.reverse();

            write_source_with_answers(
                reflow,
                source.trim_end(),
                queries_in_instantiation,
                offset_line as _,
            )
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
