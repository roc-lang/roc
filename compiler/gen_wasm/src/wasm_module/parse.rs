use super::serialize::MAX_SIZE_ENCODED_U32;
use bumpalo::Bump;

/// Parse serialized bytes into a data structure
/// Specific parsers may need contextual data from other parts of the .wasm file
pub trait Parse<ParseContext>: Sized {
    fn parse(ctx: ParseContext, bytes: &[u8], cursor: &mut usize) -> Result<Self, String>;
}

/// Skip over serialized bytes for a type
/// This may, or may not, require looking at the byte values
pub trait SkipBytes: Sized {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), String>;
}

/// Decode an unsigned 32-bit integer from the provided buffer in LEB-128 format
/// Return the integer itself and the offset after it ends
fn decode_u32(bytes: &[u8]) -> Result<(u32, usize), String> {
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

impl Parse<()> for u32 {
    fn parse(_ctx: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, String> {
        let (value, len) = decode_u32(&bytes[*cursor..])?;
        *cursor += len;
        Ok(value)
    }
}

// Parse string bytes without utf8 validation
impl<'a> Parse<&'a Bump> for &'a [u8] {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Result<Self, String> {
        let len = u32::parse((), bytes, cursor)?;
        let end = *cursor + len as usize;
        let bytes: &[u8] = &bytes[*cursor..end];
        let copy = arena.alloc_slice_copy(bytes);
        *cursor = end;
        Ok(copy)
    }
}

impl SkipBytes for u32 {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), String> {
        const MAX_LEN: usize = 5;
        for (i, byte) in bytes.iter().enumerate().skip(*cursor).take(MAX_LEN) {
            if byte & 0x80 == 0 {
                *cursor = i + 1;
                return Ok(());
            }
        }
        Err("Invalid LEB encoding".into())
    }
}

impl SkipBytes for u64 {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), String> {
        const MAX_LEN: usize = 10;
        for (i, byte) in bytes.iter().enumerate().skip(*cursor).take(MAX_LEN) {
            if byte & 0x80 == 0 {
                *cursor = i + 1;
                return Ok(());
            }
        }
        Err("Invalid LEB encoding".into())
    }
}

impl SkipBytes for u8 {
    fn skip_bytes(_bytes: &[u8], cursor: &mut usize) -> Result<(), String> {
        *cursor += 1;
        Ok(())
    }
}

/// Note: This is just for skipping over Wasm bytes. We don't actually care about String vs str!
impl SkipBytes for String {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), String> {
        let len = u32::parse((), bytes, cursor)?;

        if false {
            let str_bytes = &bytes[*cursor..(*cursor + len as usize)];
            println!(
                "Skipping string {:?}",
                std::str::from_utf8(str_bytes).unwrap()
            );
        }

        *cursor += len as usize;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::wasm_module::parse::decode_u32;

    #[test]
    fn test_decode_u32() {
        assert_eq!(decode_u32(&[0]), Ok((0, 1)));
        assert_eq!(decode_u32(&[64]), Ok((64, 1)));
        assert_eq!(decode_u32(&[0x7f]), Ok((0x7f, 1)));
        assert_eq!(decode_u32(&[0x80, 0x01]), Ok((0x80, 2)));
        assert_eq!(decode_u32(&[0xff, 0x7f]), Ok((0x3fff, 2)));
        assert_eq!(decode_u32(&[0x80, 0x80, 0x01]), Ok((0x4000, 3)));
        assert_eq!(
            decode_u32(&[0xff, 0xff, 0xff, 0xff, 0x0f]),
            Ok((u32::MAX, MAX_SIZE_ENCODED_U32))
        );
        assert!(matches!(decode_u32(&[0x80; 6]), Err(_)));
        assert!(matches!(decode_u32(&[0x80; 2]), Err(_)));
        assert!(matches!(decode_u32(&[]), Err(_)));
    }

    #[test]
    fn test_parse_u32_sequence() {
        let bytes = &[0, 0x80, 0x01, 0xff, 0xff, 0xff, 0xff, 0x0f];
        let expected = [0, 128, u32::MAX];
        let mut cursor = 0;

        assert_eq!(u32::parse((), bytes, &mut cursor).unwrap(), expected[0]);
        assert_eq!(cursor, 1);

        assert_eq!(u32::parse((), bytes, &mut cursor).unwrap(), expected[1]);
        assert_eq!(cursor, 3);

        assert_eq!(u32::parse((), bytes, &mut cursor).unwrap(), expected[2]);
        assert_eq!(cursor, 8);
    }
}
