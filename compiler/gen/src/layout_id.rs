use roc_collections::all::{default_hasher, MutMap};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::layout::Layout;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LayoutId(u32);

impl LayoutId {
    // Returns something like "foo#1" when given a symbol that interns to "foo"
    // and a LayoutId of 1.
    pub fn to_symbol_string(self, symbol: Symbol, interns: &Interns) -> String {
        format!("{}#{}", symbol.ident_string(interns), self.0)
    }
}

struct IdsByLayout<'a> {
    by_id: MutMap<Layout<'a>, u32>,
    next_id: u32,
}

#[derive(Default)]
pub struct LayoutIds<'a> {
    by_symbol: MutMap<Symbol, IdsByLayout<'a>>,
}

impl<'a> LayoutIds<'a> {
    /// Returns a LayoutId which is unique for the given symbol and layout.
    /// If given the same symbol and same layout, returns the same LayoutId.
    pub fn get(&mut self, symbol: Symbol, layout: &Layout<'a>) -> LayoutId {
        // Note: this function does some weird stuff to satisfy the borrow checker.
        // There's probably a nicer way to write it that still works.
        let ids = self.by_symbol.entry(symbol).or_insert_with(|| IdsByLayout {
            by_id: HashMap::with_capacity_and_hasher(1, default_hasher()),
            next_id: 1,
        });

        // Get the id associated with this layout, or default to next_id.
        let answer = ids.by_id.get(layout).copied().unwrap_or(ids.next_id);

        // If we had to default to next_id, it must not have been found;
        // store the ID we're going to return and increment next_id.
        if answer == ids.next_id {
            ids.by_id.insert(layout.clone(), ids.next_id);

            ids.next_id += 1;
        }

        LayoutId(answer)
    }
}
