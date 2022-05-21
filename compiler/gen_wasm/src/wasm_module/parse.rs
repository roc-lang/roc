use super::serialize::MAX_SIZE_ENCODED_U32;
use bumpalo::Bump;
use roc_error_macros::internal_error;

/// Skip over serialized bytes for a type
/// This may, or may not, require looking at the byte values
pub trait SkipBytes {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize);
}

/// Decode an unsigned 32-bit integer from the provided buffer in LEB-128 format
/// Return the integer itself and the offset after it ends
pub fn decode_u32(bytes: &[u8]) -> Result<(u32, usize), String> {
    let mut value = 0;
    let mut shift = 0;
    for (i, byte) in bytes.iter().take(MAX_SIZE_ENCODED_U32).enumerate() {
        value += ((byte & 0x7f) as u32) << shift;
        if (byte & 0x80) == 0 {
            return Ok((value, i + 1));
        }
        shift += 7;
    }
    Err(format!(
        "Failed to decode u32 as LEB-128 from bytes: {:2x?}",
        std::vec::Vec::from_iter(bytes.iter().take(MAX_SIZE_ENCODED_U32))
    ))
}

pub fn parse_u32_or_panic(bytes: &[u8], cursor: &mut usize) -> u32 {
    let (value, len) = decode_u32(&bytes[*cursor..]).unwrap_or_else(|e| internal_error!("{}", e));
    *cursor += len;
    value
}

pub fn parse_string_bytes<'a>(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> &'a [u8] {
    let len = parse_u32_or_panic(bytes, cursor);
    let end = *cursor + len as usize;
    let bytes: &[u8] = &bytes[*cursor..end];
    let copy = arena.alloc_slice_copy(bytes);
    *cursor = end;
    copy
}

impl SkipBytes for u32 {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) {
        const MAX_LEN: usize = 5;
        for (i, byte) in bytes.iter().enumerate().skip(*cursor).take(MAX_LEN) {
            if byte & 0x80 == 0 {
                *cursor = i + 1;
                return;
            }
        }
        internal_error!("Invalid LEB encoding");
    }
}

impl SkipBytes for u64 {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) {
        const MAX_LEN: usize = 10;
        for (i, byte) in bytes.iter().enumerate().skip(*cursor).take(MAX_LEN) {
            if byte & 0x80 == 0 {
                *cursor = i + 1;
                return;
            }
        }
        internal_error!("Invalid LEB encoding");
    }
}

impl SkipBytes for u8 {
    fn skip_bytes(_bytes: &[u8], cursor: &mut usize) {
        *cursor += 1;
    }
}

/// Note: This is just for skipping over Wasm bytes. We don't actually care about String vs str!
impl SkipBytes for String {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) {
        let len = parse_u32_or_panic(bytes, cursor);

        if false {
            let str_bytes = &bytes[*cursor..(*cursor + len as usize)];
            println!(
                "Skipping string {:?}",
                std::str::from_utf8(str_bytes).unwrap()
            );
        }

        *cursor += len as usize;
    }
}
