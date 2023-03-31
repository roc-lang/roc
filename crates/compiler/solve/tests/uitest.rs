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
    infer_queries, InferOptions, InferredHeader, InferredProgram, InferredQuery,
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
        let mut infer_opts = InferOptions::default();
        infer_opts.no_promote = true;

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
    let (mut sorted_queries, program) = inferred_program.decompose();
    sorted_queries.reverse();

    for (i, line) in source.lines().enumerate() {
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

            reconstruct_comment_line(writer, inferred)?;

            writeln!(writer)?;

            is_query_line = true;
        }

        // Otherwise, write the Roc source line.
        if !is_query_line {
            writeln!(writer, "{line}")?;
        }
    }

    // Finish up with any remaining print options we were asked to provide.
    let PrintOptions { can_decls } = print_options;
    if can_decls {
        writeln!(writer, "\n{EMIT_HEADER}can_decls")?;
        program.write_can_decls(writer)?;
    }

    Ok(())
}

fn reconstruct_comment_line(
    writer: &mut impl io::Write,
    inferred: InferredQuery,
) -> io::Result<()> {
    let InferredQuery {
        comment_column,
        source_line_column,
        source,
        header,
        elaboration,
    } = inferred;

    for _ in 0..comment_column {
        write!(writer, " ")?;
    }
    write!(writer, "#")?;
    for _ in 0..(source_line_column.column - comment_column - 1) {
        write!(writer, " ")?;
    }
    write!(writer, "{source} ")?;

    match header {
        InferredHeader::Specialization(spec) => write!(writer, "{spec}: ")?,
        InferredHeader::Source(_) => {
            // source is inline above the query string, don't print it again.
        }
    }

    write!(writer, "{elaboration}")
}
