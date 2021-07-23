use std::{path::Path, process::{self, Command, Stdio}, thread};
use clap::{AppSettings, Clap};


fn main() {
    let optional_args: OptionalArgs = OptionalArgs::parse();

    if Path::new("bench-folder-trunk").exists() && Path::new("bench-folder-branch").exists() {

        delete_old_bench_results();
        
        do_benchmark("trunk");
        //do_benchmark("branch");

        if optional_args.test_run {
            //println!("Doing a test run to verify benchmarks are working correctly")
        }
    } else {
        eprintln!(r#"I can't find bench-folder-trunk and bench-folder-branch from the current directory.
        I should be executed from the repo root.
        Use `./ci/safe-earthly.sh --build-arg BENCH_SUFFIX=trunk +prep-bench-folder` to generate bench-folder-trunk.
        Use `./ci/safe-earthly.sh +prep-bench-folder` to generate bench-folder-branch."#);
        
        process::exit(1)
    }
}

fn do_benchmark(branch_name: &'static str) {
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

    Command::new(format!("./bench-folder-{}/target/release/deps/time_bench", branch_name))
        .args(&["--bench", "--noplot"])
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .output()
        .expect(&format!("Failed to benchmark {}.", branch_name));

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
