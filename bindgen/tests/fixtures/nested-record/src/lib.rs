mod bindings;
mod roc_externs;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed_generic"]
    fn roc_main(_: *mut bindings::Outer);
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    use std::cmp::Ordering;

    let outer = unsafe {
        let mut ret: core::mem::MaybeUninit<bindings::Outer> = core::mem::MaybeUninit::uninit();

        roc_main(ret.as_mut_ptr());

        ret.assume_init()
    };

    // Verify that `inner` has all the expected traits.
    {
        let inner = outer.x;

        assert!(inner == inner); // PartialEq
        assert!(inner.clone() == inner.clone()); // Clone

        // Since this is a move, later uses of `inner` will fail unless `inner` has Copy
        let inner2 = inner; // Copy

        assert!(inner2 != Default::default()); // Default
        assert!(inner.partial_cmp(&inner) == Some(Ordering::Equal)); // PartialOrd
    }

    // Verify that `outer` has all the expected traits.
    {
        assert!(outer == outer); // PartialEq
        assert!(outer.clone() == outer.clone()); // Clone
        assert!(outer != Default::default()); // Default
        assert!(outer.partial_cmp(&outer) == Some(Ordering::Equal)); // PartialOrd
    }

    println!("Record was: {:?}", outer);

    // Exit code
    0
}

pub use roc_externs::roc_alloc;
pub use roc_externs::roc_dealloc;
pub use roc_externs::roc_memcpy;
pub use roc_externs::roc_memset;
pub use roc_externs::roc_panic;
pub use roc_externs::roc_realloc;
