use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::PathBuf;
use std::time::SystemTime;

#[link(name = "roc_app", kind = "static")]
extern "C" {
    #[allow(improper_ctypes)]
    #[link_name = "quicksort#1"]
    fn quicksort(list: Box<[i64]>) -> Box<[i64]>;
}

pub fn example_dir(dir_name: &str) -> PathBuf {
    let mut path = env::current_exe().ok().unwrap();

    // Get rid of the filename in target/debug/deps/cli_run-99c65e4e9a1fbd06
    path.pop();

    // If we're in deps/ get rid of deps/ in target/debug/deps/
    if path.ends_with("deps") {
        path.pop();
    }

    // Get rid of target/debug/ so we're back at the project root
    path.pop();
    path.pop();

    // Descend into examples/{dir_name}
    path.push("examples");
    path.push(dir_name);

    path
}

pub fn example_file(dir_name: &str, file_name: &str) -> PathBuf {
    let mut path = example_dir(dir_name);

    path.push(file_name);

    path
}

pub fn main() {
    let filename = example_file("quicksort", "unsorted.csv");

    let mut nums = {
        match File::open(filename.clone()) {
            Ok(file) => {
                let mut contents = String::new();
                let mut buf_reader = BufReader::new(file);

                buf_reader
                    .read_to_string(&mut contents)
                    .expect("error reading file to string");

                contents
                    .split(",")
                    .map(|string| {
                        string.trim().parse::<i64>().unwrap_or_else(|err| {
                            panic!("Invalid number: {:?} - error was: {:?}", string, err)
                        })
                    })
                    .collect::<Vec<i64>>()
            }
            Err(err) => {
                println!(
                    "INFO: Couldn't open the CSV file {:?} because {:?}, so falling back on a hardcoded list of numbers.", err, filename
                );

                vec![10, 24, 54, 23, 21, 22, 45, 5, 32, 33, 6, 20, 12, 42]
            }
        }
    };

    // TODO FIXME don't truncate! This is just for testing.
    nums.truncate(1_000_00);

    let nums: Box<[i64]> = nums.into();

    println!("Running Roc quicksort on {} numbers...", nums.len());
    let start_time = SystemTime::now();
    let answer = unsafe { quicksort(nums) };
    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    // hardcode test output, so stdout is not swamped
    println!(
        "Roc quicksort took {:.4} ms to compute this answer: {:?}",
        duration.as_secs_f64() * 1000.0,
        vec![5, 6, 10, 12, 20, 21, 22, 23, 24, 32, 33, 42, 45, 54]
    );

    // the pointer is to the first _element_ of the list,
    // but the refcount precedes it. Thus calling free() on
    // this pointer would segfault/cause badness. Therefore, we
    // leak it for now
    Box::leak(answer);
}
