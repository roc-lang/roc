use super::serialize::{MAX_SIZE_ENCODED_U32, MAX_SIZE_ENCODED_U64};
use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

/// Parse serialized bytes into a data structure
/// Specific parsers may need contextual data from other parts of the .wasm file
pub trait Parse<ParseContext>: Sized {
    fn parse(ctx: ParseContext, bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError>;
}

#[derive(Debug)]
pub struct ParseError {
    pub offset: usize,
    pub message: String,
}

/// Decode an unsigned 32-bit integer from the provided buffer in LEB-128 format
/// Return the integer itself and the offset after it ends
fn decode_u32(bytes: &[u8]) -> Result<(u32, usize), ()> {
    let mut value = 0;
    let mut shift = 0;
    for (i, byte) in bytes.iter().take(MAX_SIZE_ENCODED_U32).enumerate() {
        value += ((byte & 0x7f) as u32) << shift;
        if (byte & 0x80) == 0 {
            return Ok((value, i + 1));
        }
        shift += 7;
    }
    Err(())
}

impl Parse<()> for u32 {
    fn parse(_ctx: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        match decode_u32(&bytes[*cursor..]) {
            Ok((value, len)) => {
                *cursor += len;
                Ok(value)
            }
            Err(()) => Err(ParseError {
                offset: *cursor,
                message: format!(
                    "Failed to decode u32 as LEB-128 from bytes: {:2x?}",
                    &bytes[*cursor..][..MAX_SIZE_ENCODED_U32]
                ),
            }),
        }
    }
}

impl Parse<()> for u8 {
    fn parse(_ctx: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let byte = bytes[*cursor];
        *cursor += 1;
        Ok(byte)
    }
}

/// Decode a signed 32-bit integer from the provided buffer in LEB-128 format
/// Return the integer itself and the offset after it ends
fn decode_i32(bytes: &[u8]) -> Result<(i32, usize), ()> {
    let mut value = 0;
    let mut shift = 0;
    for (i, byte) in bytes.iter().take(MAX_SIZE_ENCODED_U32).enumerate() {
        value |= ((byte & 0x7f) as i32) << shift;
        shift += 7;
        if (byte & 0x80) == 0 {
            let is_negative = byte & 0x40 != 0;
            if shift < 32 && is_negative {
                value |= -1 << shift;
            }
            return Ok((value, i + 1));
        }
    }
    Err(())
}

impl Parse<()> for i32 {
    fn parse(_ctx: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        match decode_i32(&bytes[*cursor..]) {
            Ok((value, len)) => {
                *cursor += len;
                Ok(value)
            }
            Err(()) => Err(ParseError {
                offset: *cursor,
                message: format!(
                    "Failed to decode i32 as LEB-128 from bytes: {:2x?}",
                    &bytes[*cursor..][..MAX_SIZE_ENCODED_U32]
                ),
            }),
        }
    }
}

/// Decode a signed 64-bit integer from the provided buffer in LEB-128 format
/// Return the integer itself and the offset after it ends
fn decode_i64(bytes: &[u8]) -> Result<(i64, usize), ()> {
    let mut value = 0;
    let mut shift = 0;
    for (i, byte) in bytes.iter().take(MAX_SIZE_ENCODED_U64).enumerate() {
        value |= ((byte & 0x7f) as i64) << shift;
        shift += 7;
        if (byte & 0x80) == 0 {
            let is_negative = byte & 0x40 != 0;
            if shift < 64 && is_negative {
                value |= -1 << shift;
            }
            return Ok((value, i + 1));
        }
    }
    Err(())
}

impl Parse<()> for i64 {
    fn parse(_ctx: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        match decode_i64(&bytes[*cursor..]) {
            Ok((value, len)) => {
                *cursor += len;
                Ok(value)
            }
            Err(()) => Err(ParseError {
                offset: *cursor,
                message: format!(
                    "Failed to decode i64 as LEB-128 from bytes: {:2x?}",
                    &bytes[*cursor..][..MAX_SIZE_ENCODED_U64]
                ),
            }),
        }
    }
}

impl<'a> Parse<&'a Bump> for &'a str {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let len = u32::parse((), bytes, cursor)?;
        let end = *cursor + len as usize;
        let bytes: &[u8] = &bytes[*cursor..end];
        let copy = arena.alloc_slice_copy(bytes);
        let s = unsafe { std::str::from_utf8_unchecked(copy) };
        *cursor = end;
        Ok(s)
    }
}

pub fn parse_variable_size_items<'a, T>(
    arena: &'a Bump,
    bytes: &[u8],
    cursor: &mut usize,
) -> Result<Vec<'a, T>, ParseError>
where
    T: Parse<&'a Bump>,
{
    let len = u32::parse((), bytes, cursor)?;
    let mut vector: Vec<'a, T> = Vec::with_capacity_in(len as usize, arena);
    for _ in 0..len {
        let item = T::parse(arena, bytes, cursor)?;
        vector.push(item);
    }
    Ok(vector)
}

pub fn parse_fixed_size_items<'a, T>(
    arena: &'a Bump,
    bytes: &[u8],
    cursor: &mut usize,
) -> Result<Vec<'a, T>, ParseError>
where
    T: Parse<()>,
{
    let len = u32::parse((), bytes, cursor)?;
    let mut vector: Vec<'a, T> = Vec::with_capacity_in(len as usize, arena);
    for _ in 0..len {
        let item = T::parse((), bytes, cursor)?;
        vector.push(item);
    }
    Ok(vector)
}

/// Skip over serialized bytes for a type
/// This may, or may not, require looking at the byte values
pub trait SkipBytes: Sized {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), ParseError>;
}

impl SkipBytes for u32 {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), ParseError> {
        const MAX_LEN: usize = 5;
        for (i, byte) in bytes.iter().enumerate().skip(*cursor).take(MAX_LEN) {
            if byte & 0x80 == 0 {
                *cursor = i + 1;
                return Ok(());
            }
        }
        Err(ParseError {
            offset: *cursor,
            message: "Invalid LEB encoding".into(),
        })
    }
}

impl SkipBytes for u64 {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), ParseError> {
        const MAX_LEN: usize = 10;
        for (i, byte) in bytes.iter().enumerate().skip(*cursor).take(MAX_LEN) {
            if byte & 0x80 == 0 {
                *cursor = i + 1;
                return Ok(());
            }
        }
        Err(ParseError {
            offset: *cursor,
            message: "Invalid LEB encoding".into(),
        })
    }
}

impl SkipBytes for u8 {
    fn skip_bytes(_bytes: &[u8], cursor: &mut usize) -> Result<(), ParseError> {
        *cursor += 1;
        Ok(())
    }
}

/// Note: This is just for skipping over Wasm bytes. We don't actually care about String vs str!
impl SkipBytes for String {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), ParseError> {
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
    use crate::{parse::decode_u32, SerialBuffer};

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
        assert!(decode_u32(&[0x80; 6]).is_err());
        assert!(decode_u32(&[0x80; 2]).is_err());
        assert!(decode_u32(&[]).is_err());
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

    #[test]
    fn test_encode_decode_i32() {
        encode_decode_i32_help(3);
        encode_decode_i32_help(65535);
        encode_decode_i32_help(-2);
        encode_decode_i32_help(-65536);
        encode_decode_i32_help(i32::MIN);
        encode_decode_i32_help(i32::MAX);
    }

    fn encode_decode_i32_help(value: i32) {
        let arena = &Bump::new();
        let mut buffer = Vec::with_capacity_in(MAX_SIZE_ENCODED_U32, arena);
        buffer.encode_i32(value);
        let mut cursor = 0;
        let parsed = i32::parse((), &buffer, &mut cursor).unwrap();
        assert_eq!(parsed, value);
    }
}
