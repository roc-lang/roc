use roc_module::ident::ForeignSymbol;
use soa::Id;

#[derive(Debug)]
pub struct ForeignSymbols {
    inner: Vec<ForeignSymbol>,
}

impl ForeignSymbols {
    pub fn get(&mut self, id: ForeignSymbolId) -> &ForeignSymbol {
        // Safety: we only ever get indices that correspond to actual Vec entries
        unsafe { self.inner.get_unchecked(id.inner.index()) }
    }

    pub fn new() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl ForeignSymbols {
    pub fn push(&mut self, entry: ForeignSymbol) -> ForeignSymbolId {
        let id = self.inner.len();

        self.inner.push(entry);

        ForeignSymbolId {
            inner: Id::new(id as u32),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ForeignSymbolId {
    inner: Id<ForeignSymbol>,
}
