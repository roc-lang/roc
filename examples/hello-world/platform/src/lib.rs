use roc_std::RocCallResult;
use roc_std::RocStr;
use std::str;

extern "C" {
    #[link_name = "Hello_main_1_exposed"]
    fn say_hello(output: &mut RocCallResult<RocStr>) -> ();
}

#[no_mangle]
pub fn rust_main() -> isize {
    let answer = unsafe {
        use std::mem::MaybeUninit;
        let mut output: MaybeUninit<RocCallResult<RocStr>> = MaybeUninit::uninit();

        say_hello(&mut *output.as_mut_ptr());

        match output.assume_init().into() {
            Ok(value) => value,
            Err(msg) => panic!("roc failed with message {}", msg),
        }
    };

    println!("Roc says: {}", str::from_utf8(answer.as_slice()).unwrap());

    // Exit code
    0
}
