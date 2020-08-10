use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;
use std::path::PathBuf;
use std::time::SystemTime;

#[link(name = "roc_app", kind = "static")]
extern "C" {
    #[allow(improper_ctypes)]
    #[link_name = "0#1"]
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

    let start_time = SystemTime::now();
    let answer = unsafe { quicksort(nums) };
    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    println!(
        "Roc quicksort says: {:?} in {:?} ms",
        answer,
        duration.as_secs_f64() * 1000.0
    );
}
