/// See specialize_type.rs for where these numbers comes from.
pub const MAX_ARGS_PER_FUNCTION: usize = (i16::MAX - 1) as usize;
pub const MAX_FIELDS_PER_RECORD: usize = u16::MAX as usize;
pub const MAX_ELEMS_PER_TUPLE: usize = u16::MAX as usize;
pub const MAX_TAG_UNION_VARIANTS: usize = u16::MAX as usize; // We don't support discriminants bigger than u16.
pub const MAX_DEC_INTEGER_COMPONENT: usize = (); // TODO figure out what this is
pub const MAX_DEC_DECIMAL_COMPONENT: usize = (); // TODO figure out what this is
