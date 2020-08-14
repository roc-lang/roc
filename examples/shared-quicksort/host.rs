use std::time::SystemTime;

#[link(name = "roc_app", kind = "static")]
extern "C" {
    #[allow(improper_ctypes)]
    #[link_name = "quicksort#1"]
    fn quicksort(list: &[i64]) -> Box<[i64]>;
}

const NUM_NUMS: usize = 1_00;

pub fn main() {
    let nums = {
        let mut nums = Vec::with_capacity(NUM_NUMS + 1);

        // give this list refcount 1
        nums.push((std::usize::MAX - 1) as i64);

        for index in 1..nums.capacity() {
            let num = index as i64 % 12345;

            nums.push(num);
        }

        nums
    };

    println!("Running Roc shared quicksort");
    let start_time = SystemTime::now();
    let answer = unsafe { quicksort(&nums[1..]) };
    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    println!(
        "Roc quicksort took {:.4} ms to compute this answer: {:?}",
        duration.as_secs_f64() * 1000.0,
        // truncate the answer, so stdout is not swamped
        // NOTE index 0 is the refcount!
        &answer[1..20]
    );

    // the pointer is to the first _element_ of the list,
    // but the refcount precedes it. Thus calling free() on
    // this pointer would segfault/cause badness. Therefore, we
    // leak it for now
    Box::leak(answer);
}
