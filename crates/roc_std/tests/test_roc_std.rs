#![allow(clippy::missing_safety_doc)]

#[macro_use]
extern crate pretty_assertions;
extern crate quickcheck;
extern crate roc_std;

use core::ffi::c_void;

const ROC_SMALL_STR_CAPACITY: usize = core::mem::size_of::<roc_std::RocStr>() - 1;

#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    libc::malloc(size)
}

#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    libc::realloc(c_ptr, new_size)
}

#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    libc::free(c_ptr)
}

#[cfg(test)]
#[no_mangle]
pub unsafe extern "C" fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
    use std::ffi::CStr;
    use std::os::raw::c_char;

    match tag_id {
        0 => {
            let c_str = CStr::from_ptr(c_ptr as *const c_char);
            let string = c_str.to_str().unwrap();
            panic!("roc_panic during test: {}", string);
        }
        _ => todo!(),
    }
}

#[cfg(test)]
#[no_mangle]
pub unsafe extern "C" fn roc_memcpy(dst: *mut c_void, src: *mut c_void, n: usize) -> *mut c_void {
    libc::memcpy(dst, src, n)
}

#[cfg(test)]
#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}

#[cfg(test)]
mod test_roc_std {
    use roc_std::{RocBox, RocDec, RocList, RocResult, RocStr, SendSafeRocList, SendSafeRocStr};

    fn roc_str_byte_representation(string: &RocStr) -> [u8; RocStr::SIZE] {
        unsafe { core::mem::transmute_copy(string) }
    }

    #[test]
    fn roc_str_empty() {
        let actual = roc_str_byte_representation(&RocStr::empty());

        let mut expected = [0u8; RocStr::SIZE];
        expected[RocStr::SIZE - 1] = RocStr::MASK;

        assert_eq!(actual, expected);
    }

    #[test]
    fn roc_str_single_char() {
        let actual = roc_str_byte_representation(&RocStr::from("a"));

        let mut expected = [0u8; RocStr::SIZE];
        expected[0] = b'a';
        expected[RocStr::SIZE - 1] = RocStr::MASK | 1;

        assert_eq!(actual, expected);
    }

    #[test]
    fn roc_str_max_small_string() {
        let s = str::repeat("a", RocStr::SIZE - 1);
        let actual = roc_str_byte_representation(&RocStr::from(s.as_str()));

        let mut expected = [0u8; RocStr::SIZE];
        expected[..RocStr::SIZE - 1].copy_from_slice(s.as_bytes());
        expected[RocStr::SIZE - 1] = RocStr::MASK | s.len() as u8;

        assert_eq!(actual, expected);
    }

    #[test]
    fn empty_string_from_str() {
        let a = RocStr::from("");
        let b = RocStr::empty();

        assert_eq!(a, b);
    }

    #[test]
    fn empty_string_length() {
        let string = RocStr::from("");

        assert_eq!(string.len(), 0);
    }

    #[test]
    fn empty_string_capacity() {
        let string = RocStr::empty();

        assert_eq!(string.capacity(), super::ROC_SMALL_STR_CAPACITY);
    }

    #[test]
    fn reserve_small_str() {
        let mut roc_str = RocStr::empty();

        roc_str.reserve(42);

        assert_eq!(roc_str.capacity() >= 42, true);
    }

    #[test]
    fn reserve_big_str() {
        let mut roc_str = RocStr::empty();

        roc_str.reserve(5000);

        assert_eq!(roc_str.capacity() >= 5000, true);
    }

    #[test]
    #[cfg(feature = "serde")]
    fn str_short_serde_roundtrip() {
        let orig = RocStr::from("x");

        let serialized = serde_json::to_string(&orig).expect("failed to serialize string");
        let deserialized = serde_json::from_str(&serialized).expect("failed to deserialize string");

        assert_eq!(orig, deserialized);
    }

    #[test]
    #[cfg(feature = "serde")]
    fn str_long_serde_roundtrip() {
        // How about a little philosophy to accompany test failures?
        let orig = RocStr::from("If there's a remedy when trouble strikes, what reason is there for dejection? And if there is no help for it, what use is there in being glum? -- Shantideva, The Way of the Bodhisattva");

        let serialized = serde_json::to_string(&orig).expect("failed to serialize string");
        let deserialized = serde_json::from_str(&serialized).expect("failed to deserialize string");

        assert_eq!(orig, deserialized);
    }

    #[test]
    fn reserve_small_list() {
        let mut roc_list = RocList::<RocStr>::empty();

        roc_list.reserve(42);

        assert_eq!(roc_list.capacity(), 42);
    }

    #[test]
    fn reserve_big_list() {
        let mut roc_list = RocList::<RocStr>::empty();

        roc_list.reserve(5000);

        assert_eq!(roc_list.capacity(), 5000);
    }

    #[test]
    #[cfg(feature = "serde")]
    fn short_list_roundtrip() {
        let items: [u8; 4] = [1, 3, 3, 7];
        let orig = RocList::from_slice(&items);

        let serialized = serde_json::to_string(&orig).expect("failed to serialize string");
        let deserialized =
            serde_json::from_str::<RocList<u8>>(&serialized).expect("failed to deserialize string");

        assert_eq!(orig, deserialized);
    }

    #[test]
    #[cfg(feature = "serde")]
    fn long_list_roundtrip() {
        let orig = RocList::from_iter(1..100);

        let serialized = serde_json::to_string(&orig).expect("failed to serialize string");
        let deserialized =
            serde_json::from_str::<RocList<u8>>(&serialized).expect("failed to deserialize string");

        assert_eq!(orig, deserialized);
    }

    #[test]
    fn list_from_iter() {
        let elems: [i64; 5] = [1, 2, 3, 4, 5];
        let from_slice = RocList::from_slice(&elems);
        let from_iter = RocList::from_iter(elems);
        assert_eq!(from_iter, from_slice);
        assert_eq!(from_iter.capacity(), from_slice.capacity());
    }

    #[test]
    fn list_from_iter_zero_size() {
        let elems: [(); 5] = [(), (), (), (), ()];
        let from_slice = RocList::from_slice(&elems);
        let from_iter = RocList::from_iter(elems);
        assert_eq!(from_iter, from_slice);
    }

    #[test]
    fn list_from_array() {
        let elems: [i64; 5] = [1, 2, 3, 4, 5];
        let from_slice = RocList::from_slice(&elems);
        let from_array = RocList::from(elems);
        assert_eq!(from_array, from_slice);
        assert_eq!(from_array.capacity(), from_slice.capacity());
    }

    #[test]
    fn list_from_array_zero_size() {
        let elems: [(); 5] = [(), (), (), (), ()];
        let from_slice = RocList::from_slice(&elems);
        let from_array = RocList::from(elems);
        assert_eq!(from_array, from_slice);
        assert_eq!(from_array.capacity(), from_slice.capacity());
    }

    #[test]
    fn roc_result_to_rust_result() {
        let greeting = "Hello, World!";
        let roc_result: RocResult<String, ()> = RocResult::ok(greeting.into());

        match roc_result.into() {
            Ok(answer) => {
                assert_eq!(answer.as_str(), greeting);
            }
            Err(()) => {
                panic!("Received an Err when Ok was expected.")
            }
        }
    }

    #[test]
    fn roc_result_is_ok() {
        let greeting = "Hello, World!";
        let roc_result: RocResult<String, ()> = RocResult::ok(greeting.into());

        assert!(roc_result.is_ok());
        assert!(!roc_result.is_err());
    }

    #[test]
    fn roc_result_is_err() {
        let greeting = "Hello, World!";
        let roc_result: RocResult<(), String> = RocResult::err(greeting.into());

        assert!(!roc_result.is_ok());
        assert!(roc_result.is_err());
    }

    #[test]
    fn create_roc_box() {
        let contents = 42i32;
        let roc_box = RocBox::new(contents);

        assert_eq!(roc_box.into_inner(), contents)
    }

    #[test]
    fn roc_dec_fmt() {
        assert_eq!(
            format!("{}", RocDec::MIN),
            "-1701411834604692317316.87303715884105728"
        );

        let half = RocDec::from_str("0.5").unwrap();
        assert_eq!(format!("{}", half), "0.5");

        let ten = RocDec::from_str("10").unwrap();
        assert_eq!(format!("{}", ten), "10");

        let example = RocDec::from_str("1234.5678").unwrap();
        assert_eq!(format!("{}", example), "1234.5678");

        let example = RocDec::from_str("1_000.5678").unwrap();
        assert_eq!(format!("{}", example), "1000.5678");
    }

    #[test]
    fn safe_send_no_copy() {
        let x = RocStr::from("This is a long string but still unique. Yay!!!");
        assert_eq!(x.is_unique(), true);

        let safe_x = SendSafeRocStr::from(x);
        let new_x = RocStr::from(safe_x);
        assert_eq!(new_x.is_unique(), true);
        assert_eq!(
            new_x.as_str(),
            "This is a long string but still unique. Yay!!!"
        );
    }

    #[test]
    fn safe_send_requires_copy() {
        let x = RocStr::from("This is a long string but still unique. Yay!!!");
        let y = x.clone();
        let z = y.clone();
        assert_eq!(x.is_unique(), false);
        assert_eq!(y.is_unique(), false);
        assert_eq!(z.is_unique(), false);

        let safe_x = SendSafeRocStr::from(x);
        let new_x = RocStr::from(safe_x);
        assert_eq!(new_x.is_unique(), true);
        assert_eq!(y.is_unique(), false);
        assert_eq!(z.is_unique(), false);
        assert_eq!(
            new_x.as_str(),
            "This is a long string but still unique. Yay!!!"
        );
    }

    #[test]
    fn safe_send_small_str() {
        let x = RocStr::from("short");
        let y = x.clone();
        let z = y.clone();
        assert_eq!(x.is_unique(), true);
        assert_eq!(y.is_unique(), true);
        assert_eq!(z.is_unique(), true);

        let safe_x = SendSafeRocStr::from(x);
        let new_x = RocStr::from(safe_x);
        assert_eq!(new_x.is_unique(), true);
        assert_eq!(y.is_unique(), true);
        assert_eq!(z.is_unique(), true);
        assert_eq!(new_x.as_str(), "short");
    }

    #[test]
    fn empty_list_is_unique() {
        let roc_list = RocList::<RocStr>::empty();
        assert_eq!(roc_list.is_unique(), true);
    }

    #[test]
    fn readonly_list_is_sendsafe() {
        let x = RocList::from_slice(&[1, 2, 3, 4, 5]);
        unsafe { x.set_readonly() };
        assert_eq!(x.is_readonly(), true);

        let y = x.clone();
        let z = y.clone();

        let safe_x = SendSafeRocList::from(x);
        let new_x = RocList::from(safe_x);
        assert_eq!(new_x.is_readonly(), true);
        assert_eq!(y.is_readonly(), true);
        assert_eq!(z.is_readonly(), true);
        assert_eq!(new_x.as_slice(), &[1, 2, 3, 4, 5]);
    }
}

#[cfg(test)]
mod with_terminator {
    use core::slice;
    use roc_std::RocStr;
    use std::ffi::CStr;

    fn verify_temp_c(string: &str, excess_capacity: usize) {
        let mut roc_str = RocStr::from(string);

        println!("-------------1--------------");
        if excess_capacity > 0 {
            roc_str.reserve(excess_capacity);
        }

        // utf8_nul_terminated
        {
            println!("-------------2--------------");
            let answer = roc_str.clone().utf8_nul_terminated(|ptr, len| {
                println!("-------------3--------------");
                let bytes = unsafe { slice::from_raw_parts(ptr, len + 1) };
                println!("-------------4--------------");
                let c_str = CStr::from_bytes_with_nul(bytes).unwrap();
                println!("-------------5--------------");

                assert_eq!(c_str.to_str(), Ok(string));
                println!("-------------6--------------");

                42
            });

            assert_eq!(Ok(42), answer);
        }

        // utf16_nul_terminated
        {
            let answer = roc_str.utf16_nul_terminated(|ptr, len| {
                let bytes: &[u16] = unsafe { slice::from_raw_parts(ptr.cast(), len + 1) };

                // Verify that it's nul-terminated
                assert_eq!(bytes[len], 0);

                let string = String::from_utf16(&bytes[0..len]).unwrap();

                assert_eq!(string.as_str(), string);

                42
            });

            assert_eq!(Ok(42), answer);
        }
    }

    #[test]
    fn empty_string() {
        verify_temp_c("", 0);
    }

    /// e.g. "a" or "abc" or "abcdefg" etc.
    fn string_for_len(len: usize) -> String {
        let first_index: usize = 97; // start with ASCII lowercase "a"
        let bytes: Vec<u8> = (0..len)
            .map(|index| {
                let letter = (index % 26) + first_index;

                letter.try_into().unwrap()
            })
            .collect();

        assert_eq!(bytes.len(), len);

        // The bytes should contain no nul characters.
        assert!(bytes.iter().all(|byte| *byte != 0));

        String::from_utf8(bytes).unwrap()
    }

    #[test]
    fn small_strings() {
        for len in 1..=super::ROC_SMALL_STR_CAPACITY {
            verify_temp_c(&string_for_len(len), 0);
        }
    }

    #[test]
    fn no_excess_capacity() {
        // This is small enough that it should be a stack allocation for UTF-8
        verify_temp_c(&string_for_len(33), 0);

        // This is big enough that it should be a heap allocation for UTF-8 and UTF-16
        verify_temp_c(&string_for_len(65), 0);
    }

    #[test]
    fn with_excess_capacity() {
        println!("Start!");
        // We should be able to use the excess capacity for all of these.
        verify_temp_c(&string_for_len(33), 1); // TODO why isn't this unique?! ohh because I CLONED IT
        println!("Success!");
        // verify_temp_c(&string_for_len(33), 33);
        // verify_temp_c(&string_for_len(65), 1);
        // verify_temp_c(&string_for_len(65), 64);
    }
}
