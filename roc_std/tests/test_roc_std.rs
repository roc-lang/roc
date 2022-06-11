#[macro_use]
extern crate pretty_assertions;
// #[macro_use]
// extern crate indoc;
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
    use roc_std::{RocBox, RocDec, RocList, RocResult, RocStr};

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

        assert_eq!(roc_str.capacity(), 42);
    }

    #[test]
    fn reserve_big_str() {
        let mut roc_str = RocStr::empty();

        roc_str.reserve(5000);

        assert_eq!(roc_str.capacity(), 5000);
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
    }
}

#[cfg(test)]
mod with_terminator {
    use core::slice;
    use roc_std::RocStr;
    use std::ffi::CStr;

    fn verify_temp_c(string: &str, excess_capacity: usize) {
        let mut roc_str = RocStr::from(string);

        if excess_capacity > 0 {
            roc_str.reserve(excess_capacity);
        }

        // utf8_nul_terminated
        {
            let answer = roc_str.clone().utf8_nul_terminated(|ptr, len| {
                let bytes = unsafe { slice::from_raw_parts(ptr.cast(), len + 1) };
                let c_str = CStr::from_bytes_with_nul(bytes).unwrap();

                assert_eq!(c_str.to_str(), Ok(string));

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
        // We should be able to use the excess capacity for all of these.
        verify_temp_c(&string_for_len(33), 1);
        verify_temp_c(&string_for_len(33), 33);
        verify_temp_c(&string_for_len(65), 1);
        verify_temp_c(&string_for_len(65), 64);
    }
}
