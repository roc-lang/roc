use roc_intern::{SingleThreadedInterner, ThreadLocalInterner};

use super::Layout;

pub type TLLayoutInterner<'a> = ThreadLocalInterner<'a, Layout<'a>>;
pub type STLayoutInterner<'a> = SingleThreadedInterner<'a, Layout<'a>>;
