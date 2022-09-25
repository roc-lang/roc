use roc_module::symbol::Symbol;
use roc_target::TargetInfo;
use roc_utils::get_lib_path;
use std::ops::Index;

const LIB_DIR_ERROR: &str = "Failed to find the lib directory. Did you copy the roc binary without also copying the lib directory?\nIf you built roc from source, the lib dir should be in target/release.\nIf not, the lib dir should be included in the release tar.gz file.";

pub fn get_builtins_host_obj_path() -> String {
    let builtins_host_path = get_lib_path().expect(LIB_DIR_ERROR).join("builtins-host.o");

    builtins_host_path
        .into_os_string()
        .into_string()
        .expect("Failed to convert builtins_host_path to str")
}

pub fn get_builtins_windows_obj_path() -> String {
    let builtins_host_path = get_lib_path()
        .expect(LIB_DIR_ERROR)
        .join("builtins-windows-x86_64.obj");

    builtins_host_path
        .into_os_string()
        .into_string()
        .expect("Failed to convert builtins_host_path to str")
}

pub fn get_builtins_wasm32_obj_path() -> String {
    let builtins_wasm32_path = get_lib_path()
        .expect(LIB_DIR_ERROR)
        .join("builtins-wasm32.o");

    builtins_wasm32_path
        .into_os_string()
        .into_string()
        .expect("Failed to convert builtins_wasm32_path to str")
}

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
pub enum DecWidth {
    Dec,
}

#[repr(u8)]
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord)]
pub enum FloatWidth {
    F32,
    F64,
    F128,
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
            F128 => 16,
        }
    }

    pub const fn alignment_bytes(&self, target_info: TargetInfo) -> u32 {
        use roc_target::Architecture::*;
        use FloatWidth::*;

        // NOTE: this must never use mem::align_of, because that returns the alignment
        // for the target of *the compiler itself* (e.g. this Rust code), not what
        // the compiler is targeting (e.g. what the Roc code will be compiled to).
        match self {
            F32 => 4,
            F64 | F128 => match target_info.architecture {
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

    pub const fn alignment_bytes(&self, target_info: TargetInfo) -> u32 {
        use roc_target::Architecture;
        use IntWidth::*;

        // NOTE: this must never use mem::align_of, because that returns the alignment
        // for the target of *the compiler itself* (e.g. this Rust code), not what
        // the compiler is targeting (e.g. what the Roc code will be compiled to).
        match self {
            U8 | I8 => 1,
            U16 | I16 => 2,
            U32 | I32 => 4,
            U64 | I64 => match target_info.architecture {
                Architecture::X86_64
                | Architecture::Aarch64
                | Architecture::Aarch32
                | Architecture::Wasm32 => 8,
                Architecture::X86_32 => 4,
            },
            U128 | I128 => {
                // the C ABI defines 128-bit integers to always be 16B aligned,
                // according to https://reviews.llvm.org/D28990#655487
                16
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

impl Index<DecWidth> for IntrinsicName {
    type Output = str;

    fn index(&self, _: DecWidth) -> &Self::Output {
        self.options[0]
    }
}

impl Index<FloatWidth> for IntrinsicName {
    type Output = str;

    fn index(&self, index: FloatWidth) -> &Self::Output {
        match index {
            FloatWidth::F32 => self.options[1],
            FloatWidth::F64 => self.options[2],
            FloatWidth::F128 => self.options[3],
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
        output.options[3] = concat!($name, ".f128");

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
pub const NUM_ASIN: IntrinsicName = float_intrinsic!("roc_builtins.num.asin");
pub const NUM_ACOS: IntrinsicName = float_intrinsic!("roc_builtins.num.acos");
pub const NUM_ATAN: IntrinsicName = float_intrinsic!("roc_builtins.num.atan");
pub const NUM_IS_FINITE: IntrinsicName = float_intrinsic!("roc_builtins.num.is_finite");
pub const NUM_LOG: IntrinsicName = float_intrinsic!("roc_builtins.num.log");
pub const NUM_POW: IntrinsicName = float_intrinsic!("roc_builtins.num.pow");

pub const NUM_POW_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.pow_int");
pub const NUM_DIV_CEIL: IntrinsicName = int_intrinsic!("roc_builtins.num.div_ceil");
pub const NUM_ROUND_F32: IntrinsicName = int_intrinsic!("roc_builtins.num.round_f32");
pub const NUM_ROUND_F64: IntrinsicName = int_intrinsic!("roc_builtins.num.round_f64");

pub const NUM_ADD_OR_PANIC_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.add_or_panic");
pub const NUM_ADD_SATURATED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.add_saturated");
pub const NUM_ADD_CHECKED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.add_with_overflow");
pub const NUM_ADD_CHECKED_FLOAT: IntrinsicName =
    float_intrinsic!("roc_builtins.num.add_with_overflow");

pub const NUM_SUB_OR_PANIC_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.sub_or_panic");
pub const NUM_SUB_SATURATED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.sub_saturated");
pub const NUM_SUB_CHECKED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.sub_with_overflow");
pub const NUM_SUB_CHECKED_FLOAT: IntrinsicName =
    float_intrinsic!("roc_builtins.num.sub_with_overflow");

pub const NUM_MUL_OR_PANIC_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.mul_or_panic");
pub const NUM_MUL_SATURATED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.mul_saturated");
pub const NUM_MUL_CHECKED_INT: IntrinsicName = int_intrinsic!("roc_builtins.num.mul_with_overflow");
pub const NUM_MUL_CHECKED_FLOAT: IntrinsicName =
    float_intrinsic!("roc_builtins.num.mul_with_overflow");

pub const NUM_BYTES_TO_U16: &str = "roc_builtins.num.bytes_to_u16";
pub const NUM_BYTES_TO_U32: &str = "roc_builtins.num.bytes_to_u32";

pub const STR_INIT: &str = "roc_builtins.str.init";
pub const STR_COUNT_SEGMENTS: &str = "roc_builtins.str.count_segments";
pub const STR_CONCAT: &str = "roc_builtins.str.concat";
pub const STR_JOIN_WITH: &str = "roc_builtins.str.joinWith";
pub const STR_STR_SPLIT: &str = "roc_builtins.str.str_split";
pub const STR_TO_SCALARS: &str = "roc_builtins.str.to_scalars";
pub const STR_COUNT_GRAPEHEME_CLUSTERS: &str = "roc_builtins.str.count_grapheme_clusters";
pub const STR_COUNT_UTF8_BYTES: &str = "roc_builtins.str.count_utf8_bytes";
pub const STR_CAPACITY: &str = "roc_builtins.str.capacity";
pub const STR_STARTS_WITH: &str = "roc_builtins.str.starts_with";
pub const STR_STARTS_WITH_SCALAR: &str = "roc_builtins.str.starts_with_scalar";
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
pub const STR_FROM_UTF8_RANGE: &str = "roc_builtins.str.from_utf8_range";
pub const STR_REPEAT: &str = "roc_builtins.str.repeat";
pub const STR_TRIM: &str = "roc_builtins.str.trim";
pub const STR_TRIM_LEFT: &str = "roc_builtins.str.trim_left";
pub const STR_TRIM_RIGHT: &str = "roc_builtins.str.trim_right";
pub const STR_GET_UNSAFE: &str = "roc_builtins.str.get_unsafe";
pub const STR_RESERVE: &str = "roc_builtins.str.reserve";
pub const STR_APPEND_SCALAR: &str = "roc_builtins.str.append_scalar";
pub const STR_GET_SCALAR_UNSAFE: &str = "roc_builtins.str.get_scalar_unsafe";
pub const STR_CLONE_TO: &str = "roc_builtins.str.clone_to";

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
pub const LIST_PREPEND: &str = "roc_builtins.list.prepend";
pub const LIST_APPEND_UNSAFE: &str = "roc_builtins.list.append_unsafe";
pub const LIST_RESERVE: &str = "roc_builtins.list.reserve";

pub const DEC_FROM_STR: &str = "roc_builtins.dec.from_str";
pub const DEC_TO_STR: &str = "roc_builtins.dec.to_str";
pub const DEC_FROM_F64: &str = "roc_builtins.dec.from_f64";
pub const DEC_EQ: &str = "roc_builtins.dec.eq";
pub const DEC_NEQ: &str = "roc_builtins.dec.neq";
pub const DEC_NEGATE: &str = "roc_builtins.dec.negate";
pub const DEC_MUL_WITH_OVERFLOW: &str = "roc_builtins.dec.mul_with_overflow";
pub const DEC_DIV: &str = "roc_builtins.dec.div";
pub const DEC_ADD_WITH_OVERFLOW: &str = "roc_builtins.dec.add_with_overflow";
pub const DEC_ADD_OR_PANIC: &str = "roc_builtins.dec.add_or_panic";
pub const DEC_ADD_SATURATED: &str = "roc_builtins.dec.add_saturated";
pub const DEC_SUB_WITH_OVERFLOW: &str = "roc_builtins.dec.sub_with_overflow";
pub const DEC_SUB_OR_PANIC: &str = "roc_builtins.dec.sub_or_panic";
pub const DEC_SUB_SATURATED: &str = "roc_builtins.dec.sub_saturated";
pub const DEC_MUL_OR_PANIC: &str = "roc_builtins.dec.mul_or_panic";
pub const DEC_MUL_SATURATED: &str = "roc_builtins.dec.mul_saturated";

pub const UTILS_TEST_PANIC: &str = "roc_builtins.utils.test_panic";
pub const UTILS_ALLOCATE_WITH_REFCOUNT: &str = "roc_builtins.utils.allocate_with_refcount";
pub const UTILS_INCREF: &str = "roc_builtins.utils.incref";
pub const UTILS_DECREF: &str = "roc_builtins.utils.decref";
pub const UTILS_DECREF_CHECK_NULL: &str = "roc_builtins.utils.decref_check_null";

pub const UTILS_EXPECT_FAILED_START: &str = "roc_builtins.utils.expect_failed_start";
pub const UTILS_EXPECT_FAILED_FINALIZE: &str = "roc_builtins.utils.expect_failed_finalize";

pub const UTILS_LONGJMP: &str = "longjmp";
pub const UTILS_SETJMP: &str = "setjmp";

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
