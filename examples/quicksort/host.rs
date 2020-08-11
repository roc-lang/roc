#[link(name = "roc_app", kind = "static")]
extern "C" {
    #[allow(improper_ctypes)]
    #[link_name = "$main"]
    fn list_from_roc() -> Box<[i64]>;
}

pub fn main() {
    let list = unsafe { list_from_roc() };

    println!("Roc quicksort says: {:?}", list);

    // the pointer is to the first _element_ of the list,
    // but the refcount precedes it. Thus calling free() on
    // this pointer would segfault/cause badness. Therefore, we
    // leak it for now
    Box::leak(list);
}
