use crate::Serialize;

use super::parse::{Parse, ParseError, SkipBytes};

// NOTE: when adding a new variant, be sure to add it to LOOKUP_TABLE below as well
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
    MEMORY = 0xFC,
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

    I32EXTEND8S = 0xc0,
    I32EXTEND16S = 0xc1,
    I64EXTEND8S = 0xc2,
    I64EXTEND16S = 0xc3,
    I64EXTEND32S = 0xc4,
}

pub const LOOKUP_TABLE: [Option<OpCode>; 256] = {
    use OpCode::*;

    let mut result = [None; 256];

    result[0x00] = Some(UNREACHABLE);
    result[0x01] = Some(NOP);
    result[0x02] = Some(BLOCK);
    result[0x03] = Some(LOOP);
    result[0x04] = Some(IF);
    result[0x05] = Some(ELSE);
    result[0x0b] = Some(END);
    result[0x0c] = Some(BR);
    result[0x0d] = Some(BRIF);
    result[0x0e] = Some(BRTABLE);
    result[0x0f] = Some(RETURN);
    result[0x10] = Some(CALL);
    result[0x11] = Some(CALLINDIRECT);
    result[0x1a] = Some(DROP);
    result[0x1b] = Some(SELECT);
    result[0x20] = Some(GETLOCAL);
    result[0x21] = Some(SETLOCAL);
    result[0x22] = Some(TEELOCAL);
    result[0x23] = Some(GETGLOBAL);
    result[0x24] = Some(SETGLOBAL);
    result[0x28] = Some(I32LOAD);
    result[0x29] = Some(I64LOAD);
    result[0x2a] = Some(F32LOAD);
    result[0x2b] = Some(F64LOAD);
    result[0x2c] = Some(I32LOAD8S);
    result[0x2d] = Some(I32LOAD8U);
    result[0x2e] = Some(I32LOAD16S);
    result[0x2f] = Some(I32LOAD16U);
    result[0x30] = Some(I64LOAD8S);
    result[0x31] = Some(I64LOAD8U);
    result[0x32] = Some(I64LOAD16S);
    result[0x33] = Some(I64LOAD16U);
    result[0x34] = Some(I64LOAD32S);
    result[0x35] = Some(I64LOAD32U);
    result[0x36] = Some(I32STORE);
    result[0x37] = Some(I64STORE);
    result[0x38] = Some(F32STORE);
    result[0x39] = Some(F64STORE);
    result[0x3a] = Some(I32STORE8);
    result[0x3b] = Some(I32STORE16);
    result[0x3c] = Some(I64STORE8);
    result[0x3d] = Some(I64STORE16);
    result[0x3e] = Some(I64STORE32);
    result[0x3f] = Some(CURRENTMEMORY);
    result[0x40] = Some(GROWMEMORY);
    result[0xfc] = Some(MEMORY);
    result[0x41] = Some(I32CONST);
    result[0x42] = Some(I64CONST);
    result[0x43] = Some(F32CONST);
    result[0x44] = Some(F64CONST);
    result[0x45] = Some(I32EQZ);
    result[0x46] = Some(I32EQ);
    result[0x47] = Some(I32NE);
    result[0x48] = Some(I32LTS);
    result[0x49] = Some(I32LTU);
    result[0x4a] = Some(I32GTS);
    result[0x4b] = Some(I32GTU);
    result[0x4c] = Some(I32LES);
    result[0x4d] = Some(I32LEU);
    result[0x4e] = Some(I32GES);
    result[0x4f] = Some(I32GEU);
    result[0x50] = Some(I64EQZ);
    result[0x51] = Some(I64EQ);
    result[0x52] = Some(I64NE);
    result[0x53] = Some(I64LTS);
    result[0x54] = Some(I64LTU);
    result[0x55] = Some(I64GTS);
    result[0x56] = Some(I64GTU);
    result[0x57] = Some(I64LES);
    result[0x58] = Some(I64LEU);
    result[0x59] = Some(I64GES);
    result[0x5a] = Some(I64GEU);
    result[0x5b] = Some(F32EQ);
    result[0x5c] = Some(F32NE);
    result[0x5d] = Some(F32LT);
    result[0x5e] = Some(F32GT);
    result[0x5f] = Some(F32LE);
    result[0x60] = Some(F32GE);
    result[0x61] = Some(F64EQ);
    result[0x62] = Some(F64NE);
    result[0x63] = Some(F64LT);
    result[0x64] = Some(F64GT);
    result[0x65] = Some(F64LE);
    result[0x66] = Some(F64GE);
    result[0x67] = Some(I32CLZ);
    result[0x68] = Some(I32CTZ);
    result[0x69] = Some(I32POPCNT);
    result[0x6a] = Some(I32ADD);
    result[0x6b] = Some(I32SUB);
    result[0x6c] = Some(I32MUL);
    result[0x6d] = Some(I32DIVS);
    result[0x6e] = Some(I32DIVU);
    result[0x6f] = Some(I32REMS);
    result[0x70] = Some(I32REMU);
    result[0x71] = Some(I32AND);
    result[0x72] = Some(I32OR);
    result[0x73] = Some(I32XOR);
    result[0x74] = Some(I32SHL);
    result[0x75] = Some(I32SHRS);
    result[0x76] = Some(I32SHRU);
    result[0x77] = Some(I32ROTL);
    result[0x78] = Some(I32ROTR);
    result[0x79] = Some(I64CLZ);
    result[0x7a] = Some(I64CTZ);
    result[0x7b] = Some(I64POPCNT);
    result[0x7c] = Some(I64ADD);
    result[0x7d] = Some(I64SUB);
    result[0x7e] = Some(I64MUL);
    result[0x7f] = Some(I64DIVS);
    result[0x80] = Some(I64DIVU);
    result[0x81] = Some(I64REMS);
    result[0x82] = Some(I64REMU);
    result[0x83] = Some(I64AND);
    result[0x84] = Some(I64OR);
    result[0x85] = Some(I64XOR);
    result[0x86] = Some(I64SHL);
    result[0x87] = Some(I64SHRS);
    result[0x88] = Some(I64SHRU);
    result[0x89] = Some(I64ROTL);
    result[0x8a] = Some(I64ROTR);
    result[0x8b] = Some(F32ABS);
    result[0x8c] = Some(F32NEG);
    result[0x8d] = Some(F32CEIL);
    result[0x8e] = Some(F32FLOOR);
    result[0x8f] = Some(F32TRUNC);
    result[0x90] = Some(F32NEAREST);
    result[0x91] = Some(F32SQRT);
    result[0x92] = Some(F32ADD);
    result[0x93] = Some(F32SUB);
    result[0x94] = Some(F32MUL);
    result[0x95] = Some(F32DIV);
    result[0x96] = Some(F32MIN);
    result[0x97] = Some(F32MAX);
    result[0x98] = Some(F32COPYSIGN);
    result[0x99] = Some(F64ABS);
    result[0x9a] = Some(F64NEG);
    result[0x9b] = Some(F64CEIL);
    result[0x9c] = Some(F64FLOOR);
    result[0x9d] = Some(F64TRUNC);
    result[0x9e] = Some(F64NEAREST);
    result[0x9f] = Some(F64SQRT);
    result[0xa0] = Some(F64ADD);
    result[0xa1] = Some(F64SUB);
    result[0xa2] = Some(F64MUL);
    result[0xa3] = Some(F64DIV);
    result[0xa4] = Some(F64MIN);
    result[0xa5] = Some(F64MAX);
    result[0xa6] = Some(F64COPYSIGN);
    result[0xa7] = Some(I32WRAPI64);
    result[0xa8] = Some(I32TRUNCSF32);
    result[0xa9] = Some(I32TRUNCUF32);
    result[0xaa] = Some(I32TRUNCSF64);
    result[0xab] = Some(I32TRUNCUF64);
    result[0xac] = Some(I64EXTENDSI32);
    result[0xad] = Some(I64EXTENDUI32);
    result[0xae] = Some(I64TRUNCSF32);
    result[0xaf] = Some(I64TRUNCUF32);
    result[0xb0] = Some(I64TRUNCSF64);
    result[0xb1] = Some(I64TRUNCUF64);
    result[0xb2] = Some(F32CONVERTSI32);
    result[0xb3] = Some(F32CONVERTUI32);
    result[0xb4] = Some(F32CONVERTSI64);
    result[0xb5] = Some(F32CONVERTUI64);
    result[0xb6] = Some(F32DEMOTEF64);
    result[0xb7] = Some(F64CONVERTSI32);
    result[0xb8] = Some(F64CONVERTUI32);
    result[0xb9] = Some(F64CONVERTSI64);
    result[0xba] = Some(F64CONVERTUI64);
    result[0xbb] = Some(F64PROMOTEF32);
    result[0xbc] = Some(I32REINTERPRETF32);
    result[0xbd] = Some(I64REINTERPRETF64);
    result[0xbe] = Some(F32REINTERPRETI32);
    result[0xbf] = Some(F64REINTERPRETI64);

    result[0xc0] = Some(I32EXTEND8S);
    result[0xc1] = Some(I32EXTEND16S);
    result[0xc2] = Some(I64EXTEND8S);
    result[0xc3] = Some(I64EXTEND16S);
    result[0xc4] = Some(I64EXTEND32S);

    result
};

impl From<u8> for OpCode {
    fn from(value: u8) -> Self {
        if false {
            // considerably faster in practice
            unsafe { std::mem::transmute(value) }
        } else {
            // invalid instruction bytes can be genuine because we don't support all instructions, and
            // new ones get added or stabilized. It could also be a bug in the interpreter, e.g. some
            // opcode does not move the instruction pointer correctly.
            match LOOKUP_TABLE[value as usize] {
                None => unreachable!("unsupported instruction byte {value:#x?}"),
                Some(op) => op,
            }
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MemoryInstruction {
    MemoryInit = 8,
    DataDrop = 9,
    MemoryCopy = 10,
    MemoryFill = 11,
}

impl TryFrom<u8> for MemoryInstruction {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            8 => Ok(Self::MemoryInit),
            9 => Ok(Self::DataDrop),
            10 => Ok(Self::MemoryCopy),
            11 => Ok(Self::MemoryFill),
            _ => Err(value),
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
    Memory,
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
        MEMORY => Memory,

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
        | I32REINTERPRETF32 | I64REINTERPRETF64 | F32REINTERPRETI32 | F64REINTERPRETI64
        | I32EXTEND8S | I32EXTEND16S | I64EXTEND8S | I64EXTEND16S | I64EXTEND32S => NoImmediate,

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
            Memory => {
                match MemoryInstruction::try_from(bytes[*cursor + 1]) {
                    Ok(op) => match op {
                        MemoryInstruction::MemoryInit => {
                            // memory.init
                            todo!("WASM instruction: memory.init")
                        }
                        MemoryInstruction::DataDrop => {
                            // data.drop x
                            todo!("WASM instruction: data.drop")
                        }
                        MemoryInstruction::MemoryCopy => {
                            // memory.copy
                            *cursor += 1 + 1 + 2;
                        }
                        MemoryInstruction::MemoryFill => {
                            // memory.fill
                            *cursor += 1 + 1 + 1;
                        }
                    },
                    Err(other) => unreachable!("invalid memory instruction {other:?}"),
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
