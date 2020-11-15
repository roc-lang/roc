use roc_std::RocCallResult;
use roc_std::RocList;
use std::time::SystemTime;

extern "C" {
    #[link_name = "Main_quicksort_1_exposed"]
    fn quicksort(list: RocList<i64>, output: &mut RocCallResult<RocList<i64>>) -> ();
}

const NUM_NUMS: usize = 100;

#[no_mangle]
pub fn rust_main() -> isize {
    let nums: RocList<i64> = {
        let mut nums = Vec::with_capacity(NUM_NUMS);

        for index in 0..nums.capacity() {
            let num = index as i64 % 12;

            nums.push(num);
        }

        RocList::from_slice(&nums)
    };

    println!("Running Roc quicksort on {} numbers...", nums.len());
    let start_time = SystemTime::now();
    let answer = unsafe {
        use std::mem::MaybeUninit;
        let mut output = MaybeUninit::uninit();

        quicksort(nums, &mut *output.as_mut_ptr());

        match output.assume_init().into() {
            Ok(value) => value,
            Err(msg) => panic!("roc failed with message {}", msg),
        }
    };

    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    println!(
        "Roc quicksort took {:.4} ms to compute this answer: {:?}",
        duration.as_secs_f64() * 1000.0,
        // truncate the answer, so stdout is not swamped
        &answer.as_slice()[0..20]
    );

    // Exit code
    0
}
