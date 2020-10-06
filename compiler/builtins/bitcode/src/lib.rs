// NOTE: Editing this file on its own does nothing! The procedure for
// incorporating changes here is in this crate' README.

#![crate_type = "lib"]
#![no_std]

mod libm;

/// TODO replace this with a normal Inkwell build_cast call - this was just
/// used as a proof of concept for getting bitcode importing working!
#[no_mangle]
pub fn i64_to_f64_(num: i64) -> f64 {
    num as f64
}

/// Adapted from Rust's core::num module, by the Rust core team,
/// licensed under the Apache License, version 2.0 - https://www.apache.org/licenses/LICENSE-2.0
///
/// Thank you, Rust core team!
#[no_mangle]
pub fn pow_int_(mut base: i64, mut exp: i64) -> i64 {
    let mut acc = 1;

    while exp > 1 {
        if (exp & 1) == 1 {
            acc *= base;
        }
        exp /= 2;
        base *= base;
    }

    // Deal with the final bit of the exponent separately, since
    // squaring the base afterwards is not necessary and may cause a
    // needless overflow.
    if exp == 1 {
        acc *= base;
    }

    acc
}

#[no_mangle]
pub fn count_delimiters_(str: &[u8], delimiter: &[u8]) -> i64 {
    let mut count: i64 = 0;

    let str_len = str.len();
    let delimiter_len = delimiter.len();

    if delimiter_len > str_len {
        0
    } else {
        for str_index in 0..(str_len - delimiter_len) {
            let mut delimiter_index = 0;

            let mut matches_delimiter = true;

            while matches_delimiter && delimiter_index < delimiter_len {
                let delimiter_char = delimiter[delimiter_index];
                let str_char = str[str_index + delimiter_index];

                if delimiter_char != str_char {
                    matches_delimiter = false;
                }

                delimiter_index += 1;
            }

            if matches_delimiter {
                count += 1;
            }
        }

        count
    }
}

/// Adapted from Rust's core::num module, by the Rust core team,
/// licensed under the Apache License, version 2.0 - https://www.apache.org/licenses/LICENSE-2.0
///
/// Thank you, Rust core team!
#[no_mangle]
pub fn is_finite_(num: f64) -> bool {
    f64::is_finite(num)
}

#[no_mangle]
pub fn atan_(x: f64) -> f64 {
    libm::atan(x)
}
