use roc_module::symbol::Symbol;
use roc_target::Target;
use std::ops::Index;

#[derive(Debug, Default, Copy, Clone)]
pub struct IntrinsicName {
    pub options: [&'static str; 14],
}

impl IntrinsicName {
    pub const fn default() -> Self {
        Self { options: [""; 14] }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub enum FloatWidth {
    F32,
    F64,
}

impl FloatWidth {
    pub const fn stack_size(&self) -> u32 {
        use FloatWidth::*;

        // NOTE: this must never use mem::size_of, because that returns the size
        // for the target of *the compiler itself* (e.g. this Rust code), not what
        // the compiler is targeting (e.g. what the Roc code will be compiled to).
        match self {
            F32 => 4,
            F64 => 8,
        }
    }

    pub const fn alignment_bytes(&self, target: Target) -> u32 {
        use roc_target::Architecture::*;
        use FloatWidth::*;

        // NOTE: this must never use mem::align_of, because that returns the alignment
        // for the target of *the compiler itself* (e.g. this Rust code), not what
        // the compiler is targeting (e.g. what the Roc code will be compiled to).
        match self {
            F32 => 4,
            F64 => match target.architecture() {
                X86_64 | Aarch64 | Wasm32 => 8,
                X86_32 | Aarch32 => 4,
            },
        }
    }

    pub const fn try_from_symbol(symbol: Symbol) -> Option<Self> {
        match symbol {
            Symbol::NUM_F64 | Symbol::NUM_BINARY64 => Some(FloatWidth::F64),
            Symbol::NUM_F32 | Symbol::NUM_BINARY32 => Some(FloatWidth::F32),
            _ => None,
        }
    }

    pub const fn type_name(&self) -> &'static str {
        match self {
            Self::F32 => "f32",
            Self::F64 => "f64",
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub enum IntWidth {
    U8 = 0,
    U16 = 1,
    U32 = 2,
    U64 = 3,
    U128 = 4,
    I8 = 5,
    I16 = 6,
    I32 = 7,
    I64 = 8,
    I128 = 9,
}

impl IntWidth {
    pub const fn is_signed(&self) -> bool {
        use IntWidth::*;

        matches!(self, I8 | I16 | I32 | I64 | I128)
    }

    pub const fn stack_size(&self) -> u32 {
        use IntWidth::*;

        // NOTE: this must never use mem::size_of, because that returns the size
        // for the target of *the compiler itself* (e.g. this Rust code), not what
        // the compiler is targeting (e.g. what the Roc code will be compiled to).
        match self {
            U8 | I8 => 1,
            U16 | I16 => 2,
            U32 | I32 => 4,
            U64 | I64 => 8,
            U128 | I128 => 16,
        }
    }

    pub const fn alignment_bytes(&self, target: Target) -> u32 {
        use roc_target::Architecture;
        use IntWidth::*;

        // NOTE: this must never use mem::align_of, because that returns the alignment
        // for the target of *the compiler itself* (e.g. this Rust code), not what
        // the compiler is targeting (e.g. what the Roc code will be compiled to).
        match self {
            U8 | I8 => 1,
            U16 | I16 => 2,
            U32 | I32 => 4,
            U64 | I64 => match target.architecture() {
                Architecture::X86_64
                | Architecture::Aarch64
                | Architecture::Aarch32
                | Architecture::Wasm32 => 8,
                Architecture::X86_32 => 4,
            },
            U128 | I128 => {
                // the C ABI defines 128-bit integers to always be 16B aligned,
                // according to https://reviews.llvm.org/D28990#655487
                //
                // however, rust does not always think that this is true
                // Our alignmets here are correct, but they will not match rust/zig/llvm until they update to llvm version 18.
                match target.architecture() {
                    Architecture::X86_64 | Architecture::Aarch64 | Architecture::X86_32 => 16,
                    Architecture::Aarch32 | Architecture::Wasm32 => 8,
                }
            }
        }
    }

    pub const fn try_from_symbol(symbol: Symbol) -> Option<Self> {
        match symbol {
            Symbol::NUM_I128 | Symbol::NUM_SIGNED128 => Some(IntWidth::I128),
            Symbol::NUM_I64 | Symbol::NUM_SIGNED64 => Some(IntWidth::I64),
            Symbol::NUM_I32 | Symbol::NUM_SIGNED32 => Some(IntWidth::I32),
            Symbol::NUM_I16 | Symbol::NUM_SIGNED16 => Some(IntWidth::I16),
            Symbol::NUM_I8 | Symbol::NUM_SIGNED8 => Some(IntWidth::I8),
            Symbol::NUM_U128 | Symbol::NUM_UNSIGNED128 => Some(IntWidth::U128),
            Symbol::NUM_U64 | Symbol::NUM_UNSIGNED64 => Some(IntWidth::U64),
            Symbol::NUM_U32 | Symbol::NUM_UNSIGNED32 => Some(IntWidth::U32),
            Symbol::NUM_U16 | Symbol::NUM_UNSIGNED16 => Some(IntWidth::U16),
            Symbol::NUM_U8 | Symbol::NUM_UNSIGNED8 => Some(IntWidth::U8),
            _ => None,
        }
    }

    pub const fn type_name(&self) -> &'static str {
        match self {
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::I128 => "i128",
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::U128 => "u128",
        }
    }
}

impl Index<FloatWidth> for IntrinsicName {
    type Output = str;

    fn index(&self, index: FloatWidth) -> &Self::Output {
        match index {
            FloatWidth::F32 => self.options[1],
            FloatWidth::F64 => self.options[2],
        }
    }
}

impl Index<IntWidth> for IntrinsicName {
    type Output = str;

    fn index(&self, index: IntWidth) -> &Self::Output {
        match index {
            IntWidth::U8 => self.options[4],
            IntWidth::U16 => self.options[5],
            IntWidth::U32 => self.options[6],
            IntWidth::U64 => self.options[7],
            IntWidth::U128 => self.options[8],
            IntWidth::I8 => self.options[9],
            IntWidth::I16 => self.options[10],
            IntWidth::I32 => self.options[11],
            IntWidth::I64 => self.options[12],
            IntWidth::I128 => self.options[13],
        }
    }
}

#[macro_export]
macro_rules! float_intrinsic {
    ($name:literal) => {{
        let mut output = IntrinsicName::default();

        output.options[1] = concat!($name, ".f32");
        output.options[2] = concat!($name, ".f64");

        output
    }};
}

#[macro_export]
macro_rules! llvm_int_intrinsic {
    ($signed_name:literal, $unsigned_name:literal) => {{
        let mut output = IntrinsicName::default();

        // The indeces align with the `Index` impl for `IntrinsicName`.
        // LLVM uses the same types for both signed and unsigned integers.
        output.options[4] = concat!($unsigned_name, ".i8");
        output.options[5] = concat!($unsigned_name, ".i16");
        output.options[6] = concat!($unsigned_name, ".i32");
        output.options[7] = concat!($unsigned_name, ".i64");
        output.options[8] = concat!($unsigned_name, ".i128");

        output.options[9] = concat!($signed_name, ".i8");
        output.options[10] = concat!($signed_name, ".i16");
        output.options[11] = concat!($signed_name, ".i32");
        output.options[12] = concat!($signed_name, ".i64");
        output.options[13] = concat!($signed_name, ".i128");

        output
    }};

    ($name:literal) => {
        int_intrinsic!($name, $name)
    };
}

#[macro_export]
macro_rules! int_intrinsic {
    ($name:expr) => {{
        let mut output = IntrinsicName::default();

        // The indices align with the `Index` impl for `IntrinsicName`.
        output.options[4] = concat!($name, ".u8");
        output.options[5] = concat!($name, ".u16");
        output.options[6] = concat!($name, ".u32");
        output.options[7] = concat!($name, ".u64");
        output.options[8] = concat!($name, ".u128");

        output.options[9] = concat!($name, ".i8");
        output.options[10] = concat!($name, ".i16");
        output.options[11] = concat!($name, ".i32");
        output.options[12] = concat!($name, ".i64");
        output.options[13] = concat!($name, ".i128");

        output
    }};
}

pub const NUM_SIN: IntrinsicName = float_intrinsic!("roc_builtins.num.sin");
pub const NUM_COS: IntrinsicName = float_intrinsic!("roc_builtins.num.cos");
pub const NUM_TAN: IntrinsicName = float_intrinsic!("roc_builtins.num.tan");
pub const NUM_ASIN: IntrinsicName = float_intrinsic!("roc_builtins.num.asin");
pub const NUM_ACOS: IntrinsicName = float_intrinsic!("roc_builtins.num.acos");
pub const NUM_ATAN: IntrinsicName = float_intrinsic!("roc_builtins.num.atan");
pub const NUM_IS_NAN: IntrinsicName = float_intrinsic!("roc_builtins.num.is_nan");
pub const NUM_IS_INFINITE: IntrinsicName = float_intrinsic!("roc_builtins.num.is_infinite");
pub const NUM_IS_FINITE: IntrinsicName = float_intrinsic!("roc_builtins.num.is_finite");
pub const NUM_LOG: IntrinsicName = float_intrinsic!("roc_builtins.num.log");
pub const NUM_POW: IntrinsicName = float_intrinsic!("roc_builtins.num.pow");
pub const NUM_FABS: IntrinsicName = float_intrinsic!("roc_builtins.num.fabs");
pub const NUM_SQRT: IntrinsicName = float_intrinsic!("roc_builtins.num.sqrt");

pub const NUM_POW_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.pow_int");
pub const NUM_DIV_CEIL: IntrinsicName = int_intrinsic!("roc_builtins.num.div_ceil");
pub const NUM_CEILING_F32: IntrinsicName = int_intrinsic!("roc_builtins.num.ceiling_f32");
pub const NUM_CEILING_F64: IntrinsicName = int_intrinsic!("roc_builtins.num.ceiling_f64");
pub const NUM_FLOOR_F32: IntrinsicName = int_intrinsic!("roc_builtins.num.floor_f32");
pub const NUM_FLOOR_F64: IntrinsicName = int_intrinsic!("roc_builtins.num.floor_f64");
pub const NUM_ROUND_F32: IntrinsicName = int_intrinsic!("roc_builtins.num.round_f32");
pub const NUM_ROUND_F64: IntrinsicName = int_intrinsic!("roc_builtins.num.round_f64");
pub const INT_TO_FLOAT_CAST_F32: IntrinsicName =
    int_intrinsic!("roc_builtins.num.num_to_float_cast_f32");
pub const INT_TO_FLOAT_CAST_F64: IntrinsicName =
    int_intrinsic!("roc_builtins.num.num_to_float_cast_f64");

pub const NUM_ADD_OR_PANIC_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.add_or_panic");
pub const NUM_ADD_SATURATED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.add_saturated");
pub const NUM_ADD_WRAP_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.add_wrapped");
pub const NUM_ADD_CHECKED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.add_with_overflow");
pub const NUM_ADD_CHECKED_FLOAT: IntrinsicName =
    float_intrinsic!("roc_builtins.num.add_with_overflow");

pub const NUM_SUB_OR_PANIC_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.sub_or_panic");
pub const NUM_SUB_SATURATED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.sub_saturated");
pub const NUM_SUB_WRAP_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.sub_wrapped");
pub const NUM_SUB_CHECKED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.sub_with_overflow");
pub const NUM_SUB_CHECKED_FLOAT: IntrinsicName =
    float_intrinsic!("roc_builtins.num.sub_with_overflow");

pub const NUM_MUL_OR_PANIC_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.mul_or_panic");
pub const NUM_MUL_SATURATED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.mul_saturated");
pub const NUM_MUL_WRAP_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.mul_wrapped");
pub const NUM_MUL_CHECKED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.mul_with_overflow");
pub const NUM_MUL_CHECKED_FLOAT: IntrinsicName =
    float_intrinsic!("roc_builtins.num.mul_with_overflow");

pub const NUM_IS_MULTIPLE_OF: IntrinsicName = int_intrinsic!("roc_builtins.num.is_multiple_of");

pub const NUM_SHIFT_RIGHT_ZERO_FILL: IntrinsicName =
    int_intrinsic!("roc_builtins.num.shift_right_zero_fill");

pub const NUM_COMPARE: IntrinsicName = int_intrinsic!("roc_builtins.num.compare");
pub const NUM_LESS_THAN: IntrinsicName = int_intrinsic!("roc_builtins.num.less_than");
pub const NUM_LESS_THAN_OR_EQUAL: IntrinsicName =
    int_intrinsic!("roc_builtins.num.less_than_or_equal");
pub const NUM_GREATER_THAN: IntrinsicName = int_intrinsic!("roc_builtins.num.greater_than");
pub const NUM_GREATER_THAN_OR_EQUAL: IntrinsicName =
    int_intrinsic!("roc_builtins.num.greater_than_or_equal");

pub const NUM_COUNT_LEADING_ZERO_BITS: IntrinsicName =
    int_intrinsic!("roc_builtins.num.count_leading_zero_bits");
pub const NUM_COUNT_TRAILING_ZERO_BITS: IntrinsicName =
    int_intrinsic!("roc_builtins.num.count_trailing_zero_bits");
pub const NUM_COUNT_ONE_BITS: IntrinsicName = int_intrinsic!("roc_builtins.num.count_one_bits");
pub const NUM_F32_TO_PARTS: &str = "roc_builtins.num.f32_to_parts";
pub const NUM_F64_TO_PARTS: &str = "roc_builtins.num.f64_to_parts";
pub const NUM_F32_FROM_PARTS: &str = "roc_builtins.num.f32_from_parts";
pub const NUM_F64_FROM_PARTS: &str = "roc_builtins.num.f64_from_parts";
pub const NUM_F32_TO_BITS: &str = "roc_builtins.num.f32_to_bits";
pub const NUM_F64_TO_BITS: &str = "roc_builtins.num.f64_to_bits";
pub const NUM_I128_TO_BITS: &str = "roc_builtins.num.i128_to_bits";
pub const NUM_F32_FROM_BITS: &str = "roc_builtins.num.f32_from_bits";
pub const NUM_F64_FROM_BITS: &str = "roc_builtins.num.f64_from_bits";
pub const NUM_I128_FROM_BITS: &str = "roc_builtins.num.i128_from_bits";

pub const STR_INIT: &str = "roc_builtins.str.init";
pub const STR_COUNT_SEGMENTS: &str = "roc_builtins.str.count_segments";
pub const STR_CONCAT: &str = "roc_builtins.str.concat";
pub const STR_JOIN_WITH: &str = "roc_builtins.str.joinWith";
pub const STR_SPLIT_ON: &str = "roc_builtins.str.str_split_on";
pub const STR_COUNT_UTF8_BYTES: &str = "roc_builtins.str.count_utf8_bytes";
pub const STR_IS_EMPTY: &str = "roc_builtins.str.is_empty";
pub const STR_CAPACITY: &str = "roc_builtins.str.capacity";
pub const STR_STARTS_WITH: &str = "roc_builtins.str.starts_with";
pub const STR_ENDS_WITH: &str = "roc_builtins.str.ends_with";
pub const STR_NUMBER_OF_BYTES: &str = "roc_builtins.str.number_of_bytes";
pub const STR_FROM_INT: IntrinsicName = int_intrinsic!("roc_builtins.str.from_int");
pub const STR_FROM_FLOAT: IntrinsicName = float_intrinsic!("roc_builtins.str.from_float");
pub const STR_TO_INT: IntrinsicName = int_intrinsic!("roc_builtins.str.to_int");
pub const STR_TO_FLOAT: IntrinsicName = float_intrinsic!("roc_builtins.str.to_float");
pub const STR_TO_DECIMAL: &str = "roc_builtins.str.to_decimal";
pub const STR_EQUAL: &str = "roc_builtins.str.equal";
pub const STR_SUBSTRING_UNSAFE: &str = "roc_builtins.str.substring_unsafe";
pub const STR_TO_UTF8: &str = "roc_builtins.str.to_utf8";
pub const STR_FROM_UTF8: &str = "roc_builtins.str.from_utf8";
pub const STR_FROM_UTF8_LOSSY: &str = "roc_builtins.str.from_utf8_lossy";
pub const STR_REPEAT: &str = "roc_builtins.str.repeat";
pub const STR_TRIM: &str = "roc_builtins.str.trim";
pub const STR_TRIM_START: &str = "roc_builtins.str.trim_start";
pub const STR_TRIM_END: &str = "roc_builtins.str.trim_end";
pub const STR_GET_UNSAFE: &str = "roc_builtins.str.get_unsafe";
pub const STR_RESERVE: &str = "roc_builtins.str.reserve";
pub const STR_CLONE_TO: &str = "roc_builtins.str.clone_to";
pub const STR_WITH_CAPACITY: &str = "roc_builtins.str.with_capacity";
pub const STR_ALLOCATION_PTR: &str = "roc_builtins.str.allocation_ptr";
pub const STR_RELEASE_EXCESS_CAPACITY: &str = "roc_builtins.str.release_excess_capacity";
pub const STR_WITH_ASCII_LOWERCASED: &str = "roc_builtins.str.with_ascii_lowercased";
pub const STR_WITH_ASCII_UPPERCASED: &str = "roc_builtins.str.with_ascii_uppercased";
pub const STR_CASELESS_ASCII_EQUALS: &str = "roc_builtins.str.caseless_ascii_equals";

pub const LIST_MAP: &str = "roc_builtins.list.map";
pub const LIST_MAP2: &str = "roc_builtins.list.map2";
pub const LIST_MAP3: &str = "roc_builtins.list.map3";
pub const LIST_MAP4: &str = "roc_builtins.list.map4";
pub const LIST_SUBLIST: &str = "roc_builtins.list.sublist";
pub const LIST_DROP_AT: &str = "roc_builtins.list.drop_at";
pub const LIST_SWAP: &str = "roc_builtins.list.swap";
pub const LIST_WITH_CAPACITY: &str = "roc_builtins.list.with_capacity";
pub const LIST_SORT_WITH: &str = "roc_builtins.list.sort_with";
pub const LIST_CONCAT: &str = "roc_builtins.list.concat";
pub const LIST_REPLACE: &str = "roc_builtins.list.replace";
pub const LIST_REPLACE_IN_PLACE: &str = "roc_builtins.list.replace_in_place";
pub const LIST_IS_UNIQUE: &str = "roc_builtins.list.is_unique";
pub const LIST_CLONE: &str = "roc_builtins.list.clone";
pub const LIST_PREPEND: &str = "roc_builtins.list.prepend";
pub const LIST_APPEND_UNSAFE: &str = "roc_builtins.list.append_unsafe";
pub const LIST_RESERVE: &str = "roc_builtins.list.reserve";
pub const LIST_CAPACITY: &str = "roc_builtins.list.capacity";
pub const LIST_ALLOCATION_PTR: &str = "roc_builtins.list.allocation_ptr";
pub const LIST_RELEASE_EXCESS_CAPACITY: &str = "roc_builtins.list.release_excess_capacity";
pub const LIST_CONCAT_UTF8: &str = "roc_builtins.list.concat_utf8";
pub const LIST_INCREF: &str = "roc_builtins.list.incref";
pub const LIST_DECREF: &str = "roc_builtins.list.decref";

pub const DEC_ABS: &str = "roc_builtins.dec.abs";
pub const DEC_ACOS: &str = "roc_builtins.dec.acos";
pub const DEC_ADD_OR_PANIC: &str = "roc_builtins.dec.add_or_panic";
pub const DEC_ADD_SATURATED: &str = "roc_builtins.dec.add_saturated";
pub const DEC_ADD_WITH_OVERFLOW: &str = "roc_builtins.dec.add_with_overflow";
pub const DEC_ASIN: &str = "roc_builtins.dec.asin";
pub const DEC_ATAN: &str = "roc_builtins.dec.atan";
pub const DEC_COS: &str = "roc_builtins.dec.cos";
pub const DEC_DIV: &str = "roc_builtins.dec.div";
pub const DEC_EQ: &str = "roc_builtins.dec.eq";
pub const DEC_FROM_F64: &str = "roc_builtins.dec.from_f64";
pub const DEC_FROM_FLOAT: IntrinsicName = float_intrinsic!("roc_builtins.dec.from_float");
pub const DEC_FROM_INT: IntrinsicName = int_intrinsic!("roc_builtins.dec.from_int");
pub const DEC_FROM_STR: &str = "roc_builtins.dec.from_str";
pub const DEC_FROM_U64: &str = "roc_builtins.dec.from_u64";
pub const DEC_LOG: &str = "roc_builtins.dec.log";
pub const DEC_POW: &str = "roc_builtins.dec.pow";
pub const DEC_MUL_OR_PANIC: &str = "roc_builtins.dec.mul_or_panic";
pub const DEC_MUL_SATURATED: &str = "roc_builtins.dec.mul_saturated";
pub const DEC_MUL_WITH_OVERFLOW: &str = "roc_builtins.dec.mul_with_overflow";
pub const DEC_NEGATE: &str = "roc_builtins.dec.negate";
pub const DEC_NEQ: &str = "roc_builtins.dec.neq";
pub const DEC_SIN: &str = "roc_builtins.dec.sin";
pub const DEC_SUB_OR_PANIC: &str = "roc_builtins.dec.sub_or_panic";
pub const DEC_SUB_SATURATED: &str = "roc_builtins.dec.sub_saturated";
pub const DEC_SUB_WITH_OVERFLOW: &str = "roc_builtins.dec.sub_with_overflow";
pub const DEC_TAN: &str = "roc_builtins.dec.tan";
pub const DEC_TO_I128: &str = "roc_builtins.dec.to_i128";
pub const DEC_FROM_I128: &str = "roc_builtins.dec.from_i128";
pub const DEC_TO_STR: &str = "roc_builtins.dec.to_str";
pub const DEC_ROUND: IntrinsicName = int_intrinsic!("roc_builtins.dec.round");
pub const DEC_FLOOR: IntrinsicName = int_intrinsic!("roc_builtins.dec.floor");
pub const DEC_CEILING: IntrinsicName = int_intrinsic!("roc_builtins.dec.ceiling");

pub const UTILS_DBG_IMPL: &str = "roc_builtins.utils.dbg_impl";
pub const UTILS_TEST_PANIC: &str = "roc_builtins.utils.test_panic";
pub const UTILS_ALLOCATE_WITH_REFCOUNT: &str = "roc_builtins.utils.allocate_with_refcount";
pub const UTILS_INCREF_RC_PTR: &str = "roc_builtins.utils.incref_rc_ptr";
pub const UTILS_DECREF_RC_PTR: &str = "roc_builtins.utils.decref_rc_ptr";
pub const UTILS_FREE_RC_PTR: &str = "roc_builtins.utils.free_rc_ptr";
pub const UTILS_INCREF_DATA_PTR: &str = "roc_builtins.utils.incref_data_ptr";
pub const UTILS_DECREF_DATA_PTR: &str = "roc_builtins.utils.decref_data_ptr";
pub const UTILS_FREE_DATA_PTR: &str = "roc_builtins.utils.free_data_ptr";
pub const UTILS_IS_UNIQUE: &str = "roc_builtins.utils.is_unique";
pub const UTILS_DECREF_CHECK_NULL: &str = "roc_builtins.utils.decref_check_null";
pub const UTILS_DICT_PSEUDO_SEED: &str = "roc_builtins.utils.dict_pseudo_seed";

pub const UTILS_EXPECT_FAILED_START_SHARED_BUFFER: &str =
    "roc_builtins.utils.expect_failed_start_shared_buffer";
pub const UTILS_EXPECT_FAILED_START_SHARED_FILE: &str =
    "roc_builtins.utils.expect_failed_start_shared_file";
pub const UTILS_EXPECT_READ_ENV_SHARED_BUFFER: &str = "roc_builtins.utils.read_env_shared_buffer";
pub const NOTIFY_PARENT_EXPECT: &str = "roc_builtins.utils.notify_parent_expect";

pub const UTILS_LONGJMP: &str = "longjmp";
pub const UTILS_SETJMP: &str = "setjmp";

pub const UTILS_WINDOWS_SETJMP: &str = "windows_setjmp";
pub const UTILS_WINDOWS_LONGJMP: &str = "windows_longjmp";

#[derive(Debug, Default)]
pub struct IntToIntrinsicName {
    pub options: [IntrinsicName; 10],
}

impl IntToIntrinsicName {
    pub const fn default() -> Self {
        Self {
            options: [IntrinsicName::default(); 10],
        }
    }
}

impl Index<IntWidth> for IntToIntrinsicName {
    type Output = IntrinsicName;

    fn index(&self, index: IntWidth) -> &Self::Output {
        &self.options[index as usize]
    }
}

#[macro_export]
macro_rules! int_to_int_intrinsic {
    ($name_prefix:literal, $name_suffix:literal) => {{
        let mut output = IntToIntrinsicName::default();

        output.options[0] = int_intrinsic!(concat!($name_prefix, "u8", $name_suffix));
        output.options[1] = int_intrinsic!(concat!($name_prefix, "u16", $name_suffix));
        output.options[2] = int_intrinsic!(concat!($name_prefix, "u32", $name_suffix));
        output.options[3] = int_intrinsic!(concat!($name_prefix, "u64", $name_suffix));
        output.options[4] = int_intrinsic!(concat!($name_prefix, "u128", $name_suffix));

        output.options[5] = int_intrinsic!(concat!($name_prefix, "i8", $name_suffix));
        output.options[6] = int_intrinsic!(concat!($name_prefix, "i16", $name_suffix));
        output.options[7] = int_intrinsic!(concat!($name_prefix, "i32", $name_suffix));
        output.options[8] = int_intrinsic!(concat!($name_prefix, "i64", $name_suffix));
        output.options[9] = int_intrinsic!(concat!($name_prefix, "i128", $name_suffix));

        output
    }};
}

pub const NUM_INT_TO_INT_CHECKING_MAX: IntToIntrinsicName =
    int_to_int_intrinsic!("roc_builtins.num.int_to_", "_checking_max");
pub const NUM_INT_TO_INT_CHECKING_MAX_AND_MIN: IntToIntrinsicName =
    int_to_int_intrinsic!("roc_builtins.num.int_to_", "_checking_max_and_min");
