mod bindings;
mod roc_externs;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed_generic"]
    fn roc_main(_: *mut bindings::MyRcd);
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    use std::cmp::Ordering;
    use std::collections::hash_set::HashSet;

    let record = unsafe {
        let mut ret: core::mem::MaybeUninit<bindings::MyRcd> = core::mem::MaybeUninit::uninit();

        roc_main(ret.as_mut_ptr());

        ret.assume_init()
    };

    // Verify that the record has all the expected traits.

    assert!(record == record); // PartialEq
    assert!(record.clone() == record.clone()); // Clone

    // Since this is a move, later uses of `record` will fail unless `record` has Copy
    let rec2 = record; // Copy

    assert!(rec2 != Default::default()); // Default
    assert!(record.partial_cmp(&record) == Some(Ordering::Equal)); // PartialOrd
    assert!(record.cmp(&record) == Ordering::Equal); // Ord

    let mut set = HashSet::new();

    set.insert(record); // Eq, Hash
    set.insert(rec2);

    assert_eq!(set.len(), 1);

    println!("Record was: {:?}", record); // Debug

    // Exit code
    0
}

pub use roc_externs::roc_alloc;
pub use roc_externs::roc_dealloc;
pub use roc_externs::roc_memcpy;
pub use roc_externs::roc_memset;
pub use roc_externs::roc_panic;
pub use roc_externs::roc_realloc;
