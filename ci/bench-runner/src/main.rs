use std::{collections::{HashSet, VecDeque}, io::{BufRead, BufReader}, path::Path, process::{self, Command, Stdio}, thread};
use clap::{AppSettings, Clap};
use regex::Regex;


fn main() {
    let optional_args: OptionalArgs = OptionalArgs::parse();

    if Path::new("bench-folder-trunk").exists() && Path::new("bench-folder-branch").exists() {

        delete_old_bench_results();

        if optional_args.test_run {
            println!("Doing a test run to verify benchmarks are working correctly")
        } else {

            let mut all_regressed_benches: HashSet<String> = HashSet::new();

            for _ in 0..optional_args.nr_repeat_benchmarks {
                
                do_benchmark("trunk");
                let regressed_benches = do_benchmark("branch");

                all_regressed_benches = all_regressed_benches.intersection(&regressed_benches).map(|bench_name_str| bench_name_str.to_owned()).collect();
            }

            dbg!(all_regressed_benches);
        }
    } else {
        eprintln!(r#"I can't find bench-folder-trunk and bench-folder-branch from the current directory.
        I should be executed from the repo root.
        Use `./ci/safe-earthly.sh --build-arg BENCH_SUFFIX=trunk +prep-bench-folder` to generate bench-folder-trunk.
        Use `./ci/safe-earthly.sh +prep-bench-folder` to generate bench-folder-branch."#);
        
        process::exit(1)
    }
}

// returns Vec with names of failed benchmarks
fn do_benchmark(branch_name: &'static str) -> HashSet<String> {
    /*let builder = thread::Builder::new()
                  .name("reductor".into())
                  .stack_size(32 * 1024 * 1024); // 32MB of stack space, necessary for cfold benchmark

    let handler = builder.spawn(move || {
        Command::new(format!("./bench-folder-{}/target/release/deps/time_bench", branch_name))
        .args(&BENCH_ARGS)
        .stdout(Stdio::inherit())
        .output()
        .expect(&format!("Failed to benchmark {}.", branch_name));
    }).unwrap();
    
    handler.join().unwrap();*/

    let mut cmd_child = Command::new(format!("./bench-folder-{}/target/release/deps/time_bench", branch_name))
        .args(&["--bench", "--noplot"])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect(&format!("Failed to benchmark {}.", branch_name));

    let stdout = cmd_child.stdout.as_mut().unwrap();
    let stdout_reader = BufReader::new(stdout);
    let stdout_lines = stdout_reader.lines();

    let mut regressed_benches: HashSet<String> = HashSet::new();

    let mut last_three_lines_queue: VecDeque<String> = VecDeque::with_capacity(3);
    let bench_name_regex = Regex::new(
        r#"".*""#
    ).expect("Failed to build regex");

    for line in stdout_lines {
        let line_str = line.expect("Failed to get output from banchmark command.");

        if line_str.contains("regressed") {
            let regressed_bench_name_line = last_three_lines_queue.get(2).expect("Failed to get line that contains benchmark name from last_three_lines_queue.");
            
            let regex_match = bench_name_regex.find(regressed_bench_name_line).expect("This line should hoave the benchmark name between double quotes but I could not match it");

            regressed_benches.insert(regex_match.as_str().to_string().replace("\"", ""));
        }

        last_three_lines_queue.push_front(line_str.clone());
        
        println!("bench {:?}: {:?}", branch_name, line_str);
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
        .expect(&format!("Something went wrong trying to remove {}", file_or_folder));
}

#[derive(Clap)]
#[clap(setting = AppSettings::ColoredHelp)]
struct OptionalArgs {
    /// Do a test run: short warmup and few repeats to verify benchmarks are working correctly
    #[clap(long)]
    test_run: bool,
    /// How many times to repeat the benchmarks. A single benchmark has to fail every for a regression to be reported.
    #[clap(long, default_value = "3")]
    nr_repeat_benchmarks: usize,
    /// Do not run full benchmarks if no benchmark executable has changed
    #[clap(long)]
    check_executables_changed: bool,
}
