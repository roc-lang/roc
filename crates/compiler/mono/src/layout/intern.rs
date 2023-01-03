use std::sync::Arc;

use roc_intern::{GlobalInterner, Interned, Interner, SingleThreadedInterner, ThreadLocalInterner};

use super::Layout;

pub trait LayoutInterner<'a>: Interner<'a, Layout<'a>> {}

#[derive(Debug)]
pub struct GlobalLayoutInterner<'a>(Arc<GlobalInterner<'a, Layout<'a>>>);
#[derive(Debug)]
pub struct TLLayoutInterner<'a>(ThreadLocalInterner<'a, Layout<'a>>);
#[derive(Debug)]
pub struct STLayoutInterner<'a>(SingleThreadedInterner<'a, Layout<'a>>);

impl<'a> GlobalLayoutInterner<'a> {
    pub fn with_capacity(capacity: usize) -> Self {
        Self(GlobalInterner::with_capacity(capacity))
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
        Self(SingleThreadedInterner::with_capacity(capacity))
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
