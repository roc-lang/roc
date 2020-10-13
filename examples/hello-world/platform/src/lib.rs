use roc_std::RocStr;
use std::str;

extern "C" {
    #[link_name = "main_1"]
    fn main() -> RocStr;
}

#[no_mangle]
pub fn rust_main() -> isize {
    println!(
        "Roc says: {}",
        str::from_utf8(unsafe { main().as_slice() }).unwrap()
    );

    // Exit code
    0
}
