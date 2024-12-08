use crate::scope::SymbolLookup;
use roc_module::symbol::{ModuleId, Symbol};

#[derive(Debug, Default, Clone, Copy)]
struct ReferencesBitflags(u8);

impl ReferencesBitflags {
    const VALUE_LOOKUP: Self = ReferencesBitflags(1);
    const TYPE_LOOKUP: Self = ReferencesBitflags(2);
    const CALL: Self = ReferencesBitflags(4);
    const BOUND: Self = ReferencesBitflags(8);
    const QUALIFIED: Self = ReferencesBitflags(16);
    const UNQUALIFIED: Self = ReferencesBitflags(32);
}

#[derive(Copy, Clone, Debug)]
pub enum QualifiedReference {
    Unqualified,
    Qualified,
}

impl QualifiedReference {
    fn flags(&self, flags: ReferencesBitflags) -> ReferencesBitflags {
        match self {
            Self::Unqualified => ReferencesBitflags(flags.0 | ReferencesBitflags::UNQUALIFIED.0),
            Self::Qualified => ReferencesBitflags(flags.0 | ReferencesBitflags::QUALIFIED.0),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct References {
    symbols: Vec<Symbol>,
    bitflags: Vec<ReferencesBitflags>,
}

impl References {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn union_mut(&mut self, other: &Self) {
        for (k, v) in other.symbols.iter().zip(other.bitflags.iter()) {
            self.insert(*k, *v);
        }
    }

    // iterators

    fn retain<'a, P: Fn(&'a ReferencesBitflags) -> bool>(
        &'a self,
        pred: P,
    ) -> impl Iterator<Item = &'a Symbol> {
        self.symbols
            .iter()
            .zip(self.bitflags.iter())
            .filter_map(move |(a, b)| if pred(b) { Some(a) } else { None })
    }

    pub fn value_lookups(&self) -> impl Iterator<Item = &Symbol> {
        self.retain(|b| b.0 & ReferencesBitflags::VALUE_LOOKUP.0 > 0)
    }

    pub fn type_lookups(&self) -> impl Iterator<Item = &Symbol> {
        self.retain(|b| b.0 & ReferencesBitflags::TYPE_LOOKUP.0 > 0)
    }

    pub fn bound_symbols(&self) -> impl Iterator<Item = &Symbol> {
        self.retain(|b| b.0 & ReferencesBitflags::BOUND.0 > 0)
    }

    pub fn calls(&self) -> impl Iterator<Item = &Symbol> {
        self.retain(|b| b.0 & ReferencesBitflags::CALL.0 > 0)
    }

    // insert

    fn insert(&mut self, symbol: Symbol, flags: ReferencesBitflags) {
        match self.symbols.iter().position(|x| *x == symbol) {
            None => {
                self.symbols.push(symbol);
                self.bitflags.push(flags);
            }
            Some(index) => {
                // idea: put some debug_asserts in here?
                self.bitflags[index].0 |= flags.0;
            }
        }
    }

    pub fn insert_value_lookup(&mut self, lookup: SymbolLookup, qualified: QualifiedReference) {
        self.insert(
            lookup.symbol,
            qualified.flags(ReferencesBitflags::VALUE_LOOKUP),
        );

        if let Some((_, params_symbol)) = lookup.module_params {
            self.insert(
                params_symbol,
                qualified.flags(ReferencesBitflags::VALUE_LOOKUP),
            );
        }
    }

    pub fn insert_type_lookup(&mut self, symbol: Symbol, qualified: QualifiedReference) {
        self.insert(symbol, qualified.flags(ReferencesBitflags::TYPE_LOOKUP));
    }

    pub fn insert_bound(&mut self, symbol: Symbol) {
        self.insert(symbol, ReferencesBitflags::BOUND);
    }

    pub fn insert_call(&mut self, symbol: Symbol) {
        self.insert(symbol, ReferencesBitflags::CALL);
    }

    // remove

    pub fn remove_value_lookup(&mut self, symbol: &Symbol) {
        match self.symbols.iter().position(|x| x == symbol) {
            None => {
                // it's not in there; do nothing
            }
            Some(index) => {
                // idea: put some debug_asserts in here?
                self.bitflags[index].0 ^= ReferencesBitflags::VALUE_LOOKUP.0;
            }
        }
    }

    // contains

    pub fn has_value_lookup(&self, symbol: Symbol) -> bool {
        // println!("has a value lookup? {} {:?}", self.symbols.len(), symbol);
        let it = self.symbols.iter().zip(self.bitflags.iter());

        for (a, b) in it {
            if *a == symbol && b.0 & ReferencesBitflags::VALUE_LOOKUP.0 > 0 {
                return true;
            }
        }

        false
    }

    fn has_type_lookup(&self, symbol: Symbol) -> bool {
        let it = self.symbols.iter().zip(self.bitflags.iter());

        for (a, b) in it {
            if *a == symbol && b.0 & ReferencesBitflags::TYPE_LOOKUP.0 > 0 {
                return true;
            }
        }

        false
    }

    pub fn has_type_or_value_lookup(&self, symbol: Symbol) -> bool {
        let mask = ReferencesBitflags::VALUE_LOOKUP.0 | ReferencesBitflags::TYPE_LOOKUP.0;
        let it = self.symbols.iter().zip(self.bitflags.iter());

        for (a, b) in it {
            if *a == symbol && b.0 & mask > 0 {
                return true;
            }
        }

        false
    }

    pub fn has_unqualified_type_or_value_lookup(&self, symbol: Symbol) -> bool {
        let mask = ReferencesBitflags::VALUE_LOOKUP.0 | ReferencesBitflags::TYPE_LOOKUP.0;
        let it = self.symbols.iter().zip(self.bitflags.iter());

        for (a, b) in it {
            if *a == symbol && b.0 & mask > 0 && b.0 & ReferencesBitflags::UNQUALIFIED.0 > 0 {
                return true;
            }
        }

        false
    }

    pub fn references_type_def(&self, symbol: Symbol) -> bool {
        self.has_type_lookup(symbol)
    }

    pub fn has_module_lookup(&self, module_id: ModuleId) -> bool {
        self.symbols.iter().any(|sym| sym.module_id() == module_id)
    }
}
