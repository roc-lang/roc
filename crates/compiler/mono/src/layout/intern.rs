use std::sync::Arc;

use roc_builtins::bitcode::IntWidth;
use roc_intern::{GlobalInterner, Interned, Interner, SingleThreadedInterner, ThreadLocalInterner};

use super::{Builtin, Layout};

macro_rules! cache_interned_layouts {
    ($($i:literal, $name:ident, $layout:expr)*; $total_constants:literal) => {
        pub trait LayoutInterner<'a>: Interner<'a, Layout<'a>> {
            $(
            const $name: Interned<Layout<'a>> = unsafe { Interned::from_reserved_index($i) };
            )*
        }

        fn fill_reserved_layouts<'a>(interner: &mut STLayoutInterner<'a>) {
            assert!(interner.0.is_empty());
            $(
            interner.0.insert(&$layout);
            )*
        }

        const fn _are_constants_in_order_non_redundant() -> usize {
            let mut total_seen = 0;
            $(total_seen += ($i * 0) + 1;)*
            match 0usize {
                $($i => {})*
                _ => {}
            }
            total_seen
        }

        const _ASSERT_NON_REDUNDANT_CONSTANTS: () =
            assert!(_are_constants_in_order_non_redundant() == $total_constants);
    }
}

cache_interned_layouts! {
    0,  VOID, Layout::VOID
    1,  UNIT, Layout::UNIT
    2,  BOOL, Layout::Builtin(Builtin::Bool)
    3,  U8,   Layout::Builtin(Builtin::Int(IntWidth::U8))
    4,  U16,  Layout::Builtin(Builtin::Int(IntWidth::U16))
    5,  U32,  Layout::Builtin(Builtin::Int(IntWidth::U32))
    6,  U64,  Layout::Builtin(Builtin::Int(IntWidth::U64))
    7,  U128, Layout::Builtin(Builtin::Int(IntWidth::U128))
    8,  I8,   Layout::Builtin(Builtin::Int(IntWidth::I8))
    9,  I16,  Layout::Builtin(Builtin::Int(IntWidth::I16))
    10, I32,  Layout::Builtin(Builtin::Int(IntWidth::I32))
    11, I64,  Layout::Builtin(Builtin::Int(IntWidth::I64))
    12, I128, Layout::Builtin(Builtin::Int(IntWidth::I128))

    ; 13
}

#[derive(Debug)]
pub struct GlobalLayoutInterner<'a>(Arc<GlobalInterner<'a, Layout<'a>>>);
#[derive(Debug)]
pub struct TLLayoutInterner<'a>(ThreadLocalInterner<'a, Layout<'a>>);
#[derive(Debug)]
pub struct STLayoutInterner<'a>(SingleThreadedInterner<'a, Layout<'a>>);

impl<'a> GlobalLayoutInterner<'a> {
    pub fn with_capacity(capacity: usize) -> Self {
        STLayoutInterner::with_capacity(capacity).into_global()
    }
    pub fn fork(&self) -> TLLayoutInterner<'a> {
        TLLayoutInterner(self.0.fork())
    }
    pub fn unwrap(self) -> Result<STLayoutInterner<'a>, Self> {
        match self.0.unwrap() {
            Ok(st) => Ok(STLayoutInterner(st)),
            Err(global) => Err(Self(global)),
        }
    }
}

impl<'a> STLayoutInterner<'a> {
    pub fn with_capacity(capacity: usize) -> Self {
        let mut interner = Self(SingleThreadedInterner::with_capacity(capacity));
        fill_reserved_layouts(&mut interner);
        interner
    }
    pub fn into_global(self) -> GlobalLayoutInterner<'a> {
        GlobalLayoutInterner(self.0.into_global())
    }
}

impl<'a> Interner<'a, Layout<'a>> for TLLayoutInterner<'a> {
    fn insert(&mut self, value: &'a Layout<'a>) -> Interned<Layout<'a>> {
        self.0.insert(value)
    }

    fn get(&self, key: Interned<Layout<'a>>) -> &'a Layout<'a> {
        self.0.get(key)
    }
}
impl<'a> LayoutInterner<'a> for TLLayoutInterner<'a> {}

impl<'a> Interner<'a, Layout<'a>> for STLayoutInterner<'a> {
    fn insert(&mut self, value: &'a Layout<'a>) -> Interned<Layout<'a>> {
        self.0.insert(value)
    }

    fn get(&self, key: Interned<Layout<'a>>) -> &'a Layout<'a> {
        self.0.get(key)
    }
}
impl<'a> LayoutInterner<'a> for STLayoutInterner<'a> {}
