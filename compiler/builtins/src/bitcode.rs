pub const OBJ_PATH: &str = env!(
    "BUILTINS_O",
    "Env var BUILTINS_O not found. Is there a problem with the build script?"
);

pub fn as_bytes() -> &'static [u8] {
    // In the build script for the builtins module,
    // we compile the builtins into LLVM bitcode
    include_bytes!("../bitcode/builtins.bc")
}

pub const NUM_ASIN: &str = "roc_builtins.num.asin";
pub const NUM_ACOS: &str = "roc_builtins.num.acos";
pub const NUM_ATAN: &str = "roc_builtins.num.atan";
pub const NUM_IS_FINITE: &str = "roc_builtins.num.is_finite";
pub const NUM_POW_INT: &str = "roc_builtins.num.pow_int";

pub const STR_INIT: &str = "roc_builtins.str.init";
pub const STR_COUNT_SEGMENTS: &str = "roc_builtins.str.count_segments";
pub const STR_CONCAT: &str = "roc_builtins.str.concat";
pub const STR_JOIN_WITH: &str = "roc_builtins.str.joinWith";
pub const STR_STR_SPLIT_IN_PLACE: &str = "roc_builtins.str.str_split_in_place";
pub const STR_COUNT_GRAPEHEME_CLUSTERS: &str = "roc_builtins.str.count_grapheme_clusters";
pub const STR_STARTS_WITH: &str = "roc_builtins.str.starts_with";
pub const STR_STARTS_WITH_CODE_POINT: &str = "roc_builtins.str.starts_with_code_point";
pub const STR_ENDS_WITH: &str = "roc_builtins.str.ends_with";
pub const STR_NUMBER_OF_BYTES: &str = "roc_builtins.str.number_of_bytes";
pub const STR_FROM_INT: &str = "roc_builtins.str.from_int";
pub const STR_FROM_FLOAT: &str = "roc_builtins.str.from_float";
pub const STR_EQUAL: &str = "roc_builtins.str.equal";
pub const STR_TO_BYTES: &str = "roc_builtins.str.to_bytes";
pub const STR_FROM_UTF8: &str = "roc_builtins.str.from_utf8";

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
pub const LIST_DROP: &str = "roc_builtins.list.drop";
pub const LIST_SINGLE: &str = "roc_builtins.list.single";
pub const LIST_JOIN: &str = "roc_builtins.list.join";
pub const LIST_RANGE: &str = "roc_builtins.list.range";
pub const LIST_REVERSE: &str = "roc_builtins.list.reverse";
pub const LIST_SORT_WITH: &str = "roc_builtins.list.sort_with";
pub const LIST_CONCAT: &str = "roc_builtins.list.concat";
pub const LIST_SET: &str = "roc_builtins.list.set";
