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
use test_solve_helpers::InferOptions;

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
        Regex::new(r#"# +opt infer:(?P<opt>.*)"#).unwrap();
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
        source,
    } = TestCase::parse(data)?;

    fs::write(&path, &source)?;
    check_for_changes(&path)?;

    Ok(())
}

const UITEST_HEADER: &str = "# uitest:";

struct TestCase {
    infer_options: InferOptions,
    source: String,
}

impl TestCase {
    fn parse(mut data: String) -> Result<Self, Failed> {
        // Drop anything following `# uitest:` header lines; that's the output.
        if let Some(drop_at) = data.find(UITEST_HEADER) {
            data.truncate(drop_at);
            data.truncate(data.trim_end().len());
        }

        Ok(TestCase {
            infer_options: Self::parse_infer_options(&data)?,
            source: data,
        })
    }

    fn parse_infer_options(data: &str) -> Result<InferOptions, Failed> {
        let mut infer_opts = InferOptions::default();

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
}

fn check_for_changes(path: &Path) -> Result<(), Failed> {
    let is_tracked = Command::new("git")
        .args(["ls-files", "--error-unmatch"])
        .arg(path)
        .output()?;

    if !is_tracked.status.success() {
        return Err(format!(
            "Test file {} is not tracked by git. Try using `git add` on it.",
            path.display(),
        )
        .into());
    }

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
