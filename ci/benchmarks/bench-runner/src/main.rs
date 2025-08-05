use clap::Parser;
use data_encoding::HEXUPPER;
use is_executable::IsExecutable;
use regex::Regex;
use ring::digest::{Context, Digest, SHA256};
use std::io::Read;
use std::{
    collections::{HashMap, HashSet, VecDeque},
    io::{self, BufRead, BufReader},
    path::Path,
    process::{self, Command, Stdio},
};

const BENCH_FOLDER_MAIN: &str = "bench-folder-main";
const BENCH_FOLDER_BRANCH: &str = "bench-folder-branch";

fn main() {
    let optional_args: OptionalArgs = OptionalArgs::parse();

    if Path::new(BENCH_FOLDER_MAIN).exists() && Path::new(BENCH_FOLDER_BRANCH).exists() {
        delete_old_bench_results();

        if optional_args.check_executables_changed {
            println!("\nDoing a test run to verify benchmarks are working correctly and generate executables.\n");

            std::env::set_var("BENCH_DRY_RUN", "1");

            do_benchmark("main");
            do_benchmark("branch");

            std::env::set_var("BENCH_DRY_RUN", "0");

            if check_if_bench_executables_changed() {
                println!(
                    "\n\nComparison of sha256 of executables reveals changes, doing full benchmarks...\n\n"
                );

                let all_regressed_benches = do_all_benches(optional_args.nr_repeat_benchmarks);

                finish(all_regressed_benches, optional_args.nr_repeat_benchmarks);
            } else {
                println!("No benchmark executables have changed");
            }
        } else {
            let all_regressed_benches = do_all_benches(optional_args.nr_repeat_benchmarks);

            finish(all_regressed_benches, optional_args.nr_repeat_benchmarks);
        }
    } else {
        eprintln!(
            r#"I can't find bench-folder-main and bench-folder-branch from the current directory.
        I should be executed from the repo root.
        Use `./ci/benchmarks/prep_folder.sh main` to generate bench-folder-main.
        Use `./ci/benchmarks/prep_folder.sh branch` to generate bench-folder-branch."#
        );

        process::exit(1)
    }
}

fn finish(all_regressed_benches: HashSet<String>, nr_repeat_benchmarks: usize) {
    if !all_regressed_benches.is_empty() {
        eprintln!(
            r#"

    FAILED: The following benchmarks have shown a regression {:?} times: {:?}

    TIP: It may be the case that you do not have a speedup that is on main, I recommend pulling in main to make sure.

    "#,
            nr_repeat_benchmarks, all_regressed_benches
        );

        process::exit(1);
    }
}

// returns all benchmarks that have regressed
fn do_all_benches(nr_repeat_benchmarks: usize) -> HashSet<String> {
    delete_old_bench_results();

    do_benchmark("main");
    let mut all_regressed_benches = do_benchmark("branch");

    // if no benches regressed this round, abort early
    if all_regressed_benches.is_empty() {
        return HashSet::new();
    }

    println!(
        "\n\nDoing benchmarks {:?} times to reduce flukes.\n\n",
        nr_repeat_benchmarks
    );

    for _ in 1..nr_repeat_benchmarks {
        delete_old_bench_results();

        do_benchmark("main");
        let regressed_benches = do_benchmark("branch");

        // if no benches regressed this round, abort early
        if regressed_benches.is_empty() {
            return HashSet::new();
        }

        all_regressed_benches = all_regressed_benches
            .intersection(&regressed_benches)
            .map(|bench_name_str| bench_name_str.to_owned())
            .collect();
    }

    all_regressed_benches
}

// returns Vec with names of regressed benchmarks
fn do_benchmark(branch_name: &'static str) -> HashSet<String> {
    let mut bench_cmd = Command::new(format!(
        "./bench-folder-{}/target/release/deps/time_bench",
        branch_name
    ));

    let bench_cmd_w_args = bench_cmd.args(&["--bench", "--noplot"]);

    let bench_cmd_as_str = format!("{bench_cmd_w_args:?}");

    let mut bench_cmd_child = bench_cmd_w_args
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .unwrap_or_else(|_| panic!("Failed to benchmark {}.", branch_name));

    let stdout = bench_cmd_child.stdout.as_mut().unwrap();
    let stdout_reader = BufReader::new(stdout);
    let stdout_lines = stdout_reader.lines();

    let mut regressed_benches: HashSet<String> = HashSet::new();

    let mut last_three_lines_queue: VecDeque<String> = VecDeque::with_capacity(3);
    let bench_name_regex = Regex::new(r#"".*""#).expect("Failed to build regex");

    for line in stdout_lines {
        let line_str = line.expect("Failed to get output from benchmark command.");

        if line_str.contains("regressed") {
            let regressed_bench_name_line = last_three_lines_queue.get(2).expect(
                "Failed to get line that contains benchmark name from last_three_lines_queue.",
            );

            let regex_match = bench_name_regex.find(regressed_bench_name_line).expect("This line should have the benchmark name between double quotes but I could not match it");

            regressed_benches.insert(regex_match.as_str().to_string().replace("\"", ""));
        }

        last_three_lines_queue.push_front(line_str.clone());

        println!(">>bench {:?}: {:?}", branch_name, line_str);
    }

    let exit_status = bench_cmd_child.wait().expect("Failed to wait on cmd_child");

    if !exit_status.success() {
        panic!(
            "Error: time-bench execution failed with exit code {}.\n\
            See output above for error info.\n\
            Command was:\n\t{}",
            exit_status, bench_cmd_as_str
        );
    }

    regressed_benches
}

fn delete_old_bench_results() {
    remove("target/criterion");
}

// does not error if fileOrFolder does not exist (-f flag)
fn remove(file_or_folder: &str) {
    Command::new("rm")
        .args(&["-rf", file_or_folder])
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .unwrap_or_else(|_| panic!("Something went wrong trying to remove {}", file_or_folder));
}

#[derive(Parser)]
struct OptionalArgs {
    /// How many times to repeat the benchmarks. A single benchmark has to fail every for a regression to be reported.
    #[clap(long, default_value = "3")]
    nr_repeat_benchmarks: usize,
    /// Do not run full benchmarks if no benchmark executable has changed
    #[clap(long)]
    check_executables_changed: bool,
}

fn sha256_digest<R: Read>(mut reader: R) -> Result<Digest, io::Error> {
    let mut context = Context::new(&SHA256);
    let mut buffer = [0; 1024];

    loop {
        let count = reader.read(&mut buffer)?;
        if count == 0 {
            break;
        }
        context.update(&buffer[..count]);
    }

    Ok(context.finish())
}

fn sha_file(file_path: &Path) -> Result<String, io::Error> {
    // only checking disassembly because of #6386
    let disassembly_output = Command::new("objdump")
        .args(["-d", file_path.to_str().unwrap()])
        .output()
        .expect("failed to execute objdump");

    assert!(disassembly_output.status.success());

    let mut reader = BufReader::new(disassembly_output.stdout.as_slice());

    // the first line contains the path, we want to skip it
    let mut _discard_lines = String::new();
    reader.read_line(&mut _discard_lines)?;
    reader.read_line(&mut _discard_lines)?;

    let digest = sha256_digest(reader)?;

    Ok(HEXUPPER.encode(digest.as_ref()))
}

fn calc_hashes_for_folder(benches_path_str: &str) -> HashMap<String, String> {
    let benches_path = Path::new(benches_path_str);
    let all_bench_files =
        std::fs::read_dir(benches_path).expect("Failed to create iterator for files in dir.");

    let non_src_files = all_bench_files
        .into_iter()
        .map(|file_res| {
            file_res
                .expect("Failed to get DirEntry from ReadDir all_bench_files")
                .file_name()
                .into_string()
                .expect("Failed to create String from OsString for file_name.")
        })
        .filter(|file_name_str| !file_name_str.contains(".roc"));

    let mut files_w_sha = HashMap::new();

    for file_name in non_src_files {
        let full_path_str = [benches_path_str, &file_name].join("");
        let full_path = Path::new(&full_path_str);

        if full_path.is_executable() {
            files_w_sha.insert(
                file_name.clone(),
                sha_file(full_path).expect("Failed to calculate sha of file"),
            );
        }
    }

    files_w_sha
}

fn check_if_bench_executables_changed() -> bool {
    let bench_folder_str = "/crates/cli/tests/benchmarks/";

    let main_benches_path_str = [BENCH_FOLDER_MAIN, bench_folder_str].join("");

    let main_bench_hashes = calc_hashes_for_folder(&main_benches_path_str);

    let branch_benches_path_str = [BENCH_FOLDER_BRANCH, bench_folder_str].join("");

    let branch_bench_hashes = calc_hashes_for_folder(&branch_benches_path_str);

    if main_bench_hashes.keys().len() == branch_bench_hashes.keys().len() {
        for key in main_bench_hashes.keys() {
            if let Some(main_hash_val) = main_bench_hashes.get(key) {
                if let Some(branch_hash_val) = branch_bench_hashes.get(key) {
                    if !main_hash_val.eq(branch_hash_val) {
                        return true;
                    }
                } else {
                    return true;
                }
            }
        }

        false
    } else {
        true
    }
}
