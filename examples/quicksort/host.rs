#[link(name = "roc_app", kind = "static")]
extern "C" {
    #[allow(improper_ctypes)]
    #[link_name = "$Test.main"]
    fn list_from_roc() -> Box<[i64]>;
}

pub fn main() {
    let list = unsafe { list_from_roc() };

    println!("Roc quicksort says: {:?}", list);
}
