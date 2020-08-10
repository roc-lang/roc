use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::PathBuf;
use std::time::SystemTime;

#[link(name = "roc_app", kind = "static")]
extern "C" {
    #[allow(improper_ctypes)]
    #[link_name = "main#1"]
    fn quicksort(list: Box<[i64]>) -> Box<[i64]>;
}

pub fn main() {
    let filename = PathBuf::new()
        .join("examples")
        .join("quicksort")
        .join("unsorted.csv");

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
            Err(_) => {
                println!(
                    "INFO: Couldn't open the CSV file {:?}, so falling back on a hardcoded list of numbers.", filename
                );

                vec![10, 24, 54, 23, 21, 22, 45, 5, 32, 33, 6, 20, 12, 42]
            }
        }
    };

    // TODO FIXME don't truncate! This is just for testing.
    nums.truncate(1000);

    let nums: Box<[i64]> = nums.into();

    println!("Running Roc quicksort on {} numbers...", nums.len());
    let start_time = SystemTime::now();
    let _answer = unsafe { quicksort(nums) };
    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    println!(
        "Roc quicksort took {:.4} ms to compute this answer: {:?}",
        duration.as_secs_f64() * 1000.0,
        list
    );

    // the pointer is to the first _element_ of the list,
    // but the refcount precedes it. Thus calling free() on
    // this pointer would segfault/cause badness. Therefore, we
    // leak it for now
    Box::leak(list);
}
