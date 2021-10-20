use std::ops::Index;

pub const OBJ_PATH: &str = env!(
    "BUILTINS_O",
    "Env var BUILTINS_O not found. Is there a problem with the build script?"
);

#[derive(Default)]
pub struct IntrinsicName { 
    pub options: [ &'static str; 12 ],
    pub is_float: bool,
    pub is_int: bool,
}

impl IntrinsicName { 
    pub const fn default() -> Self { 
        Self { 
            options: [ ""; 12 ],
            is_float: false,
            is_int: false,
        }
    }

    pub fn intrinsics<'a>(&'a self) -> impl Iterator<Item = &&'a str> { 
        let start_index = if self.is_float { 0} else { 3 };
        let end_index = if self.is_int { 12 } else { 3 };

        let slice = &self.options[start_index..end_index];

        slice.iter()
    }
}


#[repr(u8)]
pub enum DecWidth {
    Dec,
}

#[repr(u8)]
pub enum FloatWidth {
    F32,
    F64,
    F128,
}

#[repr(u8)]
pub enum IntWidth {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
}


impl Index<FloatWidth> for IntrinsicName {
    type Output = str;

    fn index(&self, index: FloatWidth) -> &Self::Output {
        match index {
            FloatWidth::F32 => self.options[0], 
            FloatWidth::F64 => self.options[1], 
            FloatWidth::F128 => self.options[2], 
        }
    }
}

impl Index<IntWidth> for IntrinsicName {
    type Output = str;

    fn index(&self, index: IntWidth) -> &Self::Output {
        match index {
            IntWidth::U8 =>     self.options[3],
            IntWidth::U16 =>    self.options[4],
            IntWidth::U32 =>    self.options[5],
            IntWidth::U64 =>    self.options[6],
            IntWidth::U128 =>   self.options[7],
            IntWidth::I8 =>     self.options[8],
            IntWidth::I16 =>    self.options[9],
            IntWidth::I32 =>    self.options[10],
            IntWidth::I64 =>    self.options[11],
            IntWidth::I128 =>   self.options[12],
        }
    }
}




pub const NUM_ASIN: &str = "roc_builtins.num.asin";
pub const NUM_ACOS: &str = "roc_builtins.num.acos";
pub const NUM_ATAN: &str = "roc_builtins.num.atan";
pub const NUM_IS_FINITE: &str = "roc_builtins.num.is_finite";
pub const NUM_POW_INT: &str = "roc_builtins.num.pow_int";
pub const NUM_DIV_CEIL: &str = "roc_builtins.num.div_ceil";
pub const NUM_BYTES_TO_U16: &str = "roc_builtins.num.bytes_to_u16";
pub const NUM_BYTES_TO_U32: &str = "roc_builtins.num.bytes_to_u32";
pub const NUM_ROUND: &str = "roc_builtins.num.round";

pub const STR_INIT: &str = "roc_builtins.str.init";
pub const STR_COUNT_SEGMENTS: &str = "roc_builtins.str.count_segments";
pub const STR_CONCAT: &str = "roc_builtins.str.concat";
pub const STR_JOIN_WITH: &str = "roc_builtins.str.joinWith";
pub const STR_STR_SPLIT_IN_PLACE: &str = "roc_builtins.str.str_split_in_place";
pub const STR_COUNT_GRAPEHEME_CLUSTERS: &str = "roc_builtins.str.count_grapheme_clusters";
pub const STR_STARTS_WITH: &str = "roc_builtins.str.starts_with";
pub const STR_STARTS_WITH_CODE_PT: &str = "roc_builtins.str.starts_with_code_point";
pub const STR_ENDS_WITH: &str = "roc_builtins.str.ends_with";
pub const STR_NUMBER_OF_BYTES: &str = "roc_builtins.str.number_of_bytes";
pub const STR_FROM_INT: &str = "roc_builtins.str.from_int";
pub const STR_FROM_FLOAT: &str = "roc_builtins.str.from_float";
pub const STR_EQUAL: &str = "roc_builtins.str.equal";
pub const STR_TO_UTF8: &str = "roc_builtins.str.to_utf8";
pub const STR_FROM_UTF8: &str = "roc_builtins.str.from_utf8";
pub const STR_FROM_UTF8_RANGE: &str = "roc_builtins.str.from_utf8_range";
pub const STR_REPEAT: &str = "roc_builtins.str.repeat";

pub const DICT_HASH: &str = "roc_builtins.dict.hash";
pub const DICT_HASH_STR: &str = "roc_builtins.dict.hash_str";
pub const DICT_LEN: &str = "roc_builtins.dict.len";
pub const DICT_EMPTY: &str = "roc_builtins.dict.empty";
pub const DICT_INSERT: &str = "roc_builtins.dict.insert";
pub const DICT_REMOVE: &str = "roc_builtins.dict.remove";
pub const DICT_CONTAINS: &str = "roc_builtins.dict.contains";
pub const DICT_GET: &str = "roc_builtins.dict.get";
pub const DICT_ELEMENTS_RC: &str = "roc_builtins.dict.elementsRc";
pub const DICT_KEYS: &str = "roc_builtins.dict.keys";
pub const DICT_VALUES: &str = "roc_builtins.dict.values";
pub const DICT_UNION: &str = "roc_builtins.dict.union";
pub const DICT_DIFFERENCE: &str = "roc_builtins.dict.difference";
pub const DICT_INTERSECTION: &str = "roc_builtins.dict.intersection";
pub const DICT_WALK: &str = "roc_builtins.dict.walk";

pub const SET_FROM_LIST: &str = "roc_builtins.dict.set_from_list";

pub const LIST_MAP: &str = "roc_builtins.list.map";
pub const LIST_MAP2: &str = "roc_builtins.list.map2";
pub const LIST_MAP3: &str = "roc_builtins.list.map3";
pub const LIST_MAP_WITH_INDEX: &str = "roc_builtins.list.map_with_index";
pub const LIST_KEEP_IF: &str = "roc_builtins.list.keep_if";
pub const LIST_KEEP_OKS: &str = "roc_builtins.list.keep_oks";
pub const LIST_KEEP_ERRS: &str = "roc_builtins.list.keep_errs";
pub const LIST_WALK: &str = "roc_builtins.list.walk";
pub const LIST_WALK_UNTIL: &str = "roc_builtins.list.walkUntil";
pub const LIST_WALK_BACKWARDS: &str = "roc_builtins.list.walk_backwards";
pub const LIST_CONTAINS: &str = "roc_builtins.list.contains";
pub const LIST_REPEAT: &str = "roc_builtins.list.repeat";
pub const LIST_APPEND: &str = "roc_builtins.list.append";
pub const LIST_PREPEND: &str = "roc_builtins.list.prepend";
pub const LIST_DROP: &str = "roc_builtins.list.drop";
pub const LIST_DROP_AT: &str = "roc_builtins.list.drop_at";
pub const LIST_SWAP: &str = "roc_builtins.list.swap";
pub const LIST_SINGLE: &str = "roc_builtins.list.single";
pub const LIST_JOIN: &str = "roc_builtins.list.join";
pub const LIST_RANGE: &str = "roc_builtins.list.range";
pub const LIST_REVERSE: &str = "roc_builtins.list.reverse";
pub const LIST_SORT_WITH: &str = "roc_builtins.list.sort_with";
pub const LIST_CONCAT: &str = "roc_builtins.list.concat";
pub const LIST_SET: &str = "roc_builtins.list.set";
pub const LIST_SET_IN_PLACE: &str = "roc_builtins.list.set_in_place";

pub const DEC_FROM_F64: &str = "roc_builtins.dec.from_f64";
pub const DEC_EQ: &str = "roc_builtins.dec.eq";
pub const DEC_NEQ: &str = "roc_builtins.dec.neq";
pub const DEC_NEGATE: &str = "roc_builtins.dec.negate";
pub const DEC_ADD_WITH_OVERFLOW: &str = "roc_builtins.dec.add_with_overflow";
pub const DEC_SUB_WITH_OVERFLOW: &str = "roc_builtins.dec.sub_with_overflow";
pub const DEC_MUL_WITH_OVERFLOW: &str = "roc_builtins.dec.mul_with_overflow";
pub const DEC_DIV: &str = "roc_builtins.dec.div";

pub const UTILS_TEST_PANIC: &str = "roc_builtins.utils.test_panic";
pub const UTILS_DECREF: &str = "roc_builtins.utils.decref";
pub const UTILS_DECREF_CHECK_NULL: &str = "roc_builtins.utils.decref_check_null";
