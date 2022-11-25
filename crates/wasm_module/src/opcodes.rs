use crate::Serialize;

use super::parse::{Parse, ParseError, SkipBytes};

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpCode {
    UNREACHABLE = 0x00,
    NOP = 0x01,
    BLOCK = 0x02,
    LOOP = 0x03,
    IF = 0x04,
    ELSE = 0x05,
    END = 0x0b,
    BR = 0x0c,
    BRIF = 0x0d,
    BRTABLE = 0x0e,
    RETURN = 0x0f,
    CALL = 0x10,
    CALLINDIRECT = 0x11,
    DROP = 0x1a,
    SELECT = 0x1b,
    GETLOCAL = 0x20,
    SETLOCAL = 0x21,
    TEELOCAL = 0x22,
    GETGLOBAL = 0x23,
    SETGLOBAL = 0x24,
    I32LOAD = 0x28,
    I64LOAD = 0x29,
    F32LOAD = 0x2a,
    F64LOAD = 0x2b,
    I32LOAD8S = 0x2c,
    I32LOAD8U = 0x2d,
    I32LOAD16S = 0x2e,
    I32LOAD16U = 0x2f,
    I64LOAD8S = 0x30,
    I64LOAD8U = 0x31,
    I64LOAD16S = 0x32,
    I64LOAD16U = 0x33,
    I64LOAD32S = 0x34,
    I64LOAD32U = 0x35,
    I32STORE = 0x36,
    I64STORE = 0x37,
    F32STORE = 0x38,
    F64STORE = 0x39,
    I32STORE8 = 0x3a,
    I32STORE16 = 0x3b,
    I64STORE8 = 0x3c,
    I64STORE16 = 0x3d,
    I64STORE32 = 0x3e,
    CURRENTMEMORY = 0x3f,
    GROWMEMORY = 0x40,
    I32CONST = 0x41,
    I64CONST = 0x42,
    F32CONST = 0x43,
    F64CONST = 0x44,
    I32EQZ = 0x45,
    I32EQ = 0x46,
    I32NE = 0x47,
    I32LTS = 0x48,
    I32LTU = 0x49,
    I32GTS = 0x4a,
    I32GTU = 0x4b,
    I32LES = 0x4c,
    I32LEU = 0x4d,
    I32GES = 0x4e,
    I32GEU = 0x4f,
    I64EQZ = 0x50,
    I64EQ = 0x51,
    I64NE = 0x52,
    I64LTS = 0x53,
    I64LTU = 0x54,
    I64GTS = 0x55,
    I64GTU = 0x56,
    I64LES = 0x57,
    I64LEU = 0x58,
    I64GES = 0x59,
    I64GEU = 0x5a,

    F32EQ = 0x5b,
    F32NE = 0x5c,
    F32LT = 0x5d,
    F32GT = 0x5e,
    F32LE = 0x5f,
    F32GE = 0x60,

    F64EQ = 0x61,
    F64NE = 0x62,
    F64LT = 0x63,
    F64GT = 0x64,
    F64LE = 0x65,
    F64GE = 0x66,

    I32CLZ = 0x67,
    I32CTZ = 0x68,
    I32POPCNT = 0x69,
    I32ADD = 0x6a,
    I32SUB = 0x6b,
    I32MUL = 0x6c,
    I32DIVS = 0x6d,
    I32DIVU = 0x6e,
    I32REMS = 0x6f,
    I32REMU = 0x70,
    I32AND = 0x71,
    I32OR = 0x72,
    I32XOR = 0x73,
    I32SHL = 0x74,
    I32SHRS = 0x75,
    I32SHRU = 0x76,
    I32ROTL = 0x77,
    I32ROTR = 0x78,

    I64CLZ = 0x79,
    I64CTZ = 0x7a,
    I64POPCNT = 0x7b,
    I64ADD = 0x7c,
    I64SUB = 0x7d,
    I64MUL = 0x7e,
    I64DIVS = 0x7f,
    I64DIVU = 0x80,
    I64REMS = 0x81,
    I64REMU = 0x82,
    I64AND = 0x83,
    I64OR = 0x84,
    I64XOR = 0x85,
    I64SHL = 0x86,
    I64SHRS = 0x87,
    I64SHRU = 0x88,
    I64ROTL = 0x89,
    I64ROTR = 0x8a,
    F32ABS = 0x8b,
    F32NEG = 0x8c,
    F32CEIL = 0x8d,
    F32FLOOR = 0x8e,
    F32TRUNC = 0x8f,
    F32NEAREST = 0x90,
    F32SQRT = 0x91,
    F32ADD = 0x92,
    F32SUB = 0x93,
    F32MUL = 0x94,
    F32DIV = 0x95,
    F32MIN = 0x96,
    F32MAX = 0x97,
    F32COPYSIGN = 0x98,
    F64ABS = 0x99,
    F64NEG = 0x9a,
    F64CEIL = 0x9b,
    F64FLOOR = 0x9c,
    F64TRUNC = 0x9d,
    F64NEAREST = 0x9e,
    F64SQRT = 0x9f,
    F64ADD = 0xa0,
    F64SUB = 0xa1,
    F64MUL = 0xa2,
    F64DIV = 0xa3,
    F64MIN = 0xa4,
    F64MAX = 0xa5,
    F64COPYSIGN = 0xa6,

    I32WRAPI64 = 0xa7,
    I32TRUNCSF32 = 0xa8,
    I32TRUNCUF32 = 0xa9,
    I32TRUNCSF64 = 0xaa,
    I32TRUNCUF64 = 0xab,
    I64EXTENDSI32 = 0xac,
    I64EXTENDUI32 = 0xad,
    I64TRUNCSF32 = 0xae,
    I64TRUNCUF32 = 0xaf,
    I64TRUNCSF64 = 0xb0,
    I64TRUNCUF64 = 0xb1,
    F32CONVERTSI32 = 0xb2,
    F32CONVERTUI32 = 0xb3,
    F32CONVERTSI64 = 0xb4,
    F32CONVERTUI64 = 0xb5,
    F32DEMOTEF64 = 0xb6,
    F64CONVERTSI32 = 0xb7,
    F64CONVERTUI32 = 0xb8,
    F64CONVERTSI64 = 0xb9,
    F64CONVERTUI64 = 0xba,
    F64PROMOTEF32 = 0xbb,

    I32REINTERPRETF32 = 0xbc,
    I64REINTERPRETF64 = 0xbd,
    F32REINTERPRETI32 = 0xbe,
    F64REINTERPRETI64 = 0xbf,
}

impl From<u8> for OpCode {
    fn from(x: u8) -> Self {
        use OpCode::*;
        match x {
            0x00 => UNREACHABLE,
            0x01 => NOP,
            0x02 => BLOCK,
            0x03 => LOOP,
            0x04 => IF,
            0x05 => ELSE,
            0x0b => END,
            0x0c => BR,
            0x0d => BRIF,
            0x0e => BRTABLE,
            0x0f => RETURN,
            0x10 => CALL,
            0x11 => CALLINDIRECT,
            0x1a => DROP,
            0x1b => SELECT,
            0x20 => GETLOCAL,
            0x21 => SETLOCAL,
            0x22 => TEELOCAL,
            0x23 => GETGLOBAL,
            0x24 => SETGLOBAL,
            0x28 => I32LOAD,
            0x29 => I64LOAD,
            0x2a => F32LOAD,
            0x2b => F64LOAD,
            0x2c => I32LOAD8S,
            0x2d => I32LOAD8U,
            0x2e => I32LOAD16S,
            0x2f => I32LOAD16U,
            0x30 => I64LOAD8S,
            0x31 => I64LOAD8U,
            0x32 => I64LOAD16S,
            0x33 => I64LOAD16U,
            0x34 => I64LOAD32S,
            0x35 => I64LOAD32U,
            0x36 => I32STORE,
            0x37 => I64STORE,
            0x38 => F32STORE,
            0x39 => F64STORE,
            0x3a => I32STORE8,
            0x3b => I32STORE16,
            0x3c => I64STORE8,
            0x3d => I64STORE16,
            0x3e => I64STORE32,
            0x3f => CURRENTMEMORY,
            0x40 => GROWMEMORY,
            0x41 => I32CONST,
            0x42 => I64CONST,
            0x43 => F32CONST,
            0x44 => F64CONST,
            0x45 => I32EQZ,
            0x46 => I32EQ,
            0x47 => I32NE,
            0x48 => I32LTS,
            0x49 => I32LTU,
            0x4a => I32GTS,
            0x4b => I32GTU,
            0x4c => I32LES,
            0x4d => I32LEU,
            0x4e => I32GES,
            0x4f => I32GEU,
            0x50 => I64EQZ,
            0x51 => I64EQ,
            0x52 => I64NE,
            0x53 => I64LTS,
            0x54 => I64LTU,
            0x55 => I64GTS,
            0x56 => I64GTU,
            0x57 => I64LES,
            0x58 => I64LEU,
            0x59 => I64GES,
            0x5a => I64GEU,

            0x5b => F32EQ,
            0x5c => F32NE,
            0x5d => F32LT,
            0x5e => F32GT,
            0x5f => F32LE,
            0x60 => F32GE,

            0x61 => F64EQ,
            0x62 => F64NE,
            0x63 => F64LT,
            0x64 => F64GT,
            0x65 => F64LE,
            0x66 => F64GE,

            0x67 => I32CLZ,
            0x68 => I32CTZ,
            0x69 => I32POPCNT,
            0x6a => I32ADD,
            0x6b => I32SUB,
            0x6c => I32MUL,
            0x6d => I32DIVS,
            0x6e => I32DIVU,
            0x6f => I32REMS,
            0x70 => I32REMU,
            0x71 => I32AND,
            0x72 => I32OR,
            0x73 => I32XOR,
            0x74 => I32SHL,
            0x75 => I32SHRS,
            0x76 => I32SHRU,
            0x77 => I32ROTL,
            0x78 => I32ROTR,

            0x79 => I64CLZ,
            0x7a => I64CTZ,
            0x7b => I64POPCNT,
            0x7c => I64ADD,
            0x7d => I64SUB,
            0x7e => I64MUL,
            0x7f => I64DIVS,
            0x80 => I64DIVU,
            0x81 => I64REMS,
            0x82 => I64REMU,
            0x83 => I64AND,
            0x84 => I64OR,
            0x85 => I64XOR,
            0x86 => I64SHL,
            0x87 => I64SHRS,
            0x88 => I64SHRU,
            0x89 => I64ROTL,
            0x8a => I64ROTR,
            0x8b => F32ABS,
            0x8c => F32NEG,
            0x8d => F32CEIL,
            0x8e => F32FLOOR,
            0x8f => F32TRUNC,
            0x90 => F32NEAREST,
            0x91 => F32SQRT,
            0x92 => F32ADD,
            0x93 => F32SUB,
            0x94 => F32MUL,
            0x95 => F32DIV,
            0x96 => F32MIN,
            0x97 => F32MAX,
            0x98 => F32COPYSIGN,
            0x99 => F64ABS,
            0x9a => F64NEG,
            0x9b => F64CEIL,
            0x9c => F64FLOOR,
            0x9d => F64TRUNC,
            0x9e => F64NEAREST,
            0x9f => F64SQRT,
            0xa0 => F64ADD,
            0xa1 => F64SUB,
            0xa2 => F64MUL,
            0xa3 => F64DIV,
            0xa4 => F64MIN,
            0xa5 => F64MAX,
            0xa6 => F64COPYSIGN,

            0xa7 => I32WRAPI64,
            0xa8 => I32TRUNCSF32,
            0xa9 => I32TRUNCUF32,
            0xaa => I32TRUNCSF64,
            0xab => I32TRUNCUF64,
            0xac => I64EXTENDSI32,
            0xad => I64EXTENDUI32,
            0xae => I64TRUNCSF32,
            0xaf => I64TRUNCUF32,
            0xb0 => I64TRUNCSF64,
            0xb1 => I64TRUNCUF64,
            0xb2 => F32CONVERTSI32,
            0xb3 => F32CONVERTUI32,
            0xb4 => F32CONVERTSI64,
            0xb5 => F32CONVERTUI64,
            0xb6 => F32DEMOTEF64,
            0xb7 => F64CONVERTSI32,
            0xb8 => F64CONVERTUI32,
            0xb9 => F64CONVERTSI64,
            0xba => F64CONVERTUI64,
            0xbb => F64PROMOTEF32,

            0xbc => I32REINTERPRETF32,
            0xbd => I64REINTERPRETF64,
            0xbe => F32REINTERPRETI32,
            0xbf => F64REINTERPRETI64,
            _ => unreachable!(),
        }
    }
}

/// The format of the *immediate* operands of an operator
/// Immediates appear directly in the byte stream after the opcode,
/// rather than being popped off the value stack. These are the possible forms.
#[derive(Debug)]
enum OpImmediates {
    NoImmediate,
    Byte1,
    Bytes4,
    Bytes8,
    Leb32x1,
    Leb64x1,
    Leb32x2,
    BrTable,
}

fn immediates_for(op: OpCode) -> Result<OpImmediates, String> {
    use OpCode::*;
    use OpImmediates::*;

    let imm = match op {
        UNREACHABLE => NoImmediate,
        NOP => NoImmediate,
        BLOCK | LOOP | IF => Byte1,
        ELSE => NoImmediate,
        END => NoImmediate,
        BR | BRIF => Leb32x1,
        BRTABLE => BrTable,
        RETURN => NoImmediate,
        CALL => Leb32x1,
        CALLINDIRECT => Leb32x2,
        DROP => NoImmediate,
        SELECT => NoImmediate,
        GETLOCAL | SETLOCAL | TEELOCAL => Leb32x1,
        GETGLOBAL | SETGLOBAL => Leb32x1,

        I32LOAD | I64LOAD | F32LOAD | F64LOAD | I32LOAD8S | I32LOAD8U | I32LOAD16S | I32LOAD16U
        | I64LOAD8S | I64LOAD8U | I64LOAD16S | I64LOAD16U | I64LOAD32S | I64LOAD32U | I32STORE
        | I64STORE | F32STORE | F64STORE | I32STORE8 | I32STORE16 | I64STORE8 | I64STORE16
        | I64STORE32 => Leb32x2,

        CURRENTMEMORY | GROWMEMORY => Byte1,

        I32CONST => Leb32x1,
        I64CONST => Leb64x1,
        F32CONST => Bytes4,
        F64CONST => Bytes8,

        I32EQZ | I32EQ | I32NE | I32LTS | I32LTU | I32GTS | I32GTU | I32LES | I32LEU | I32GES
        | I32GEU | I64EQZ | I64EQ | I64NE | I64LTS | I64LTU | I64GTS | I64GTU | I64LES | I64LEU
        | I64GES | I64GEU | F32EQ | F32NE | F32LT | F32GT | F32LE | F32GE | F64EQ | F64NE
        | F64LT | F64GT | F64LE | F64GE | I32CLZ | I32CTZ | I32POPCNT | I32ADD | I32SUB
        | I32MUL | I32DIVS | I32DIVU | I32REMS | I32REMU | I32AND | I32OR | I32XOR | I32SHL
        | I32SHRS | I32SHRU | I32ROTL | I32ROTR | I64CLZ | I64CTZ | I64POPCNT | I64ADD | I64SUB
        | I64MUL | I64DIVS | I64DIVU | I64REMS | I64REMU | I64AND | I64OR | I64XOR | I64SHL
        | I64SHRS | I64SHRU | I64ROTL | I64ROTR | F32ABS | F32NEG | F32CEIL | F32FLOOR
        | F32TRUNC | F32NEAREST | F32SQRT | F32ADD | F32SUB | F32MUL | F32DIV | F32MIN | F32MAX
        | F32COPYSIGN | F64ABS | F64NEG | F64CEIL | F64FLOOR | F64TRUNC | F64NEAREST | F64SQRT
        | F64ADD | F64SUB | F64MUL | F64DIV | F64MIN | F64MAX | F64COPYSIGN | I32WRAPI64
        | I32TRUNCSF32 | I32TRUNCUF32 | I32TRUNCSF64 | I32TRUNCUF64 | I64EXTENDSI32
        | I64EXTENDUI32 | I64TRUNCSF32 | I64TRUNCUF32 | I64TRUNCSF64 | I64TRUNCUF64
        | F32CONVERTSI32 | F32CONVERTUI32 | F32CONVERTSI64 | F32CONVERTUI64 | F32DEMOTEF64
        | F64CONVERTSI32 | F64CONVERTUI32 | F64CONVERTSI64 | F64CONVERTUI64 | F64PROMOTEF32
        | I32REINTERPRETF32 | I64REINTERPRETF64 | F32REINTERPRETI32 | F64REINTERPRETI64 => {
            NoImmediate
        }

        // Catch-all in case of an invalid cast from u8 to OpCode while parsing binary
        // (rustc keeps this code, I verified in Compiler Explorer)
        #[allow(unreachable_patterns)]
        _ => return Err(format!("Unknown Wasm instruction 0x{:02x}", op as u8)),
    };

    Ok(imm)
}

impl SkipBytes for OpCode {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), ParseError> {
        use OpImmediates::*;

        let opcode_byte: u8 = bytes[*cursor];

        let opcode: OpCode = OpCode::from(opcode_byte);
        // will return Err if transmute was invalid
        let immediates = immediates_for(opcode).map_err(|message| ParseError {
            message,
            offset: *cursor,
        })?;

        match immediates {
            NoImmediate => {
                *cursor += 1;
            }
            Byte1 => {
                *cursor += 1 + 1;
            }
            Bytes4 => {
                *cursor += 1 + 4;
            }
            Bytes8 => {
                *cursor += 1 + 8;
            }
            Leb32x1 => {
                *cursor += 1;
                u32::skip_bytes(bytes, cursor)?;
            }
            Leb64x1 => {
                *cursor += 1;
                u64::skip_bytes(bytes, cursor)?;
            }
            Leb32x2 => {
                *cursor += 1;
                u32::skip_bytes(bytes, cursor)?;
                u32::skip_bytes(bytes, cursor)?;
            }
            BrTable => {
                *cursor += 1;
                let n_labels = 1 + u32::parse((), bytes, cursor)?;
                for _ in 0..n_labels {
                    u32::skip_bytes(bytes, cursor)?;
                }
            }
        }
        Ok(())
    }
}

impl Serialize for OpCode {
    fn serialize<T: crate::SerialBuffer>(&self, buffer: &mut T) {
        (*self as u8).serialize(buffer)
    }
}
