use roc_can::expr::IntValue;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Number {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    I128(i128),
    U128(u128),
    F32(f32),
    F64(f64),
    Dec(i128),
}

impl Number {
    fn u8(val: IntValue) -> Self {
        match val {
            IntValue::I128(i128) => Self::U8(i128::from_ne_bytes(i128) as u8),
            IntValue::U128(u128) => Self::U8(u128::from_ne_bytes(u128) as u8),
        }
    }

    fn i8(val: IntValue) -> Self {
        match val {
            IntValue::I128(i128) => Self::I8(i128::from_ne_bytes(i128) as i8),
            IntValue::U128(u128) => Self::I8(u128::from_ne_bytes(u128) as i8),
        }
    }

    fn u16(val: IntValue) -> Self {
        match val {
            IntValue::I128(i128) => Self::U16(i128::from_ne_bytes(i128) as u16),
            IntValue::U128(u128) => Self::U16(u128::from_ne_bytes(u128) as u16),
        }
    }

    fn i16(val: IntValue) -> Self {
        match val {
            IntValue::I128(i128) => Self::I16(i128::from_ne_bytes(i128) as i16),
            IntValue::U128(u128) => Self::I16(u128::from_ne_bytes(u128) as i16),
        }
    }

    fn u32(val: IntValue) -> Self {
        match val {
            IntValue::I128(i128) => Self::U32(i128::from_ne_bytes(i128) as u32),
            IntValue::U128(u128) => Self::U32(u128::from_ne_bytes(u128) as u32),
        }
    }

    fn i32(val: IntValue) -> Self {
        match val {
            IntValue::I128(i128) => Self::I32(i128::from_ne_bytes(i128) as i32),
            IntValue::U128(u128) => Self::I32(u128::from_ne_bytes(u128) as i32),
        }
    }

    fn u64(val: IntValue) -> Self {
        match val {
            IntValue::I128(i128) => Self::U64(i128::from_ne_bytes(i128) as u64),
            IntValue::U128(u128) => Self::U64(u128::from_ne_bytes(u128) as u64),
        }
    }

    fn i64(val: IntValue) -> Self {
        match val {
            IntValue::I128(i128) => Self::I64(i128::from_ne_bytes(i128) as i64),
            IntValue::U128(u128) => Self::I64(u128::from_ne_bytes(u128) as i64),
        }
    }

    fn u128(val: IntValue) -> Self {
        match val {
            IntValue::I128(i128) => Self::U128(i128::from_ne_bytes(i128) as u128),
            IntValue::U128(u128) => Self::U128(u128::from_ne_bytes(u128)),
        }
    }

    fn i128(val: IntValue) -> Self {
        match val {
            IntValue::I128(i128) => Self::I128(i128::from_ne_bytes(i128)),
            IntValue::U128(u128) => Self::I128(u128::from_ne_bytes(u128) as i128),
        }
    }
}
