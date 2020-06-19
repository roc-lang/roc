#![crate_type = "lib"]
#![no_std]

struct Str {
    bytes: [16, u8];
}

#[no_mangle]
pub fn empty_() -> Str {
    Str {
        bytes : [0; 16]
    }
}

#[no_mangle]
pub fn len_(string: Str) -> usize {
    let disc = discriminant(str);

    if disc == 0 {
        // It's a 
    }
}

#[inline(always)]
fn discriminant(string: &Str) -> u8 {
    // cast the first 8 bytes to be u64, return its lsbyte
}