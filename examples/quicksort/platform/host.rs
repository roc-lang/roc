#![crate_type = "staticlib"]

use std::time::SystemTime;

extern "C" {
    #[allow(improper_ctypes)]
    #[link_name = "quicksort_1"]
    fn quicksort(list: Box<[i64]>) -> Box<[i64]>;
}

const NUM_NUMS: usize = 1_000_000;

#[no_mangle]
pub fn rust_main() -> isize {
    let nums: Box<[i64]> = {
        let mut nums = Vec::with_capacity(NUM_NUMS);

        for index in 0..nums.capacity() {
            let num = index as i64 % 12345;

            nums.push(num);
        }
        nums.into()
    };

    println!("Running Roc quicksort on {} numbers...", nums.len());
    let start_time = SystemTime::now();
    let answer = unsafe { quicksort(nums) };
    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    println!(
        "Roc quicksort took {:.4} ms to compute this answer: {:?}",
        duration.as_secs_f64() * 1000.0,
        // truncate the answer, so stdout is not swamped
        &answer[0..20]
    );

    // the pointer is to the first _element_ of the list,
    // but the refcount precedes it. Thus calling free() on
    // this pointer would segfault/cause badness. Therefore, we
    // leak it for now
    Box::leak(answer);

    // Exit code
    0
}
