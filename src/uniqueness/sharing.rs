use crate::collections::ImMap;
use crate::module::symbol::Symbol;

#[derive(Clone, Debug, PartialEq)]
pub enum ReferenceCount {
    Unique,
    Shared,
}

impl ReferenceCount {
    pub fn add(_a: &ReferenceCount, _b: &ReferenceCount) -> Self {
        Self::Shared
    }

    pub fn or(a: &ReferenceCount, b: &ReferenceCount) -> Self {
        match (a, b) {
            (Self::Unique, Self::Unique) => Self::Unique,
            _ => Self::Shared,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VarUsage {
    usage: ImMap<Symbol, ReferenceCount>,
}

impl VarUsage {
    pub fn default() -> VarUsage {
        VarUsage {
            usage: (ImMap::default()),
        }
    }

    pub fn register_with(&mut self, symbol: &Symbol, rc: &ReferenceCount) {
        let value = match self.usage.get(symbol) {
            None => rc.clone(),
            Some(current) => ReferenceCount::add(current, rc),
        };

        self.usage.insert(symbol.clone(), value);
    }

    pub fn register(&mut self, symbol: &Symbol) {
        use self::ReferenceCount::*;
        self.register_with(symbol, &Unique);
    }

    pub fn unregister(&mut self, symbol: &Symbol) {
        self.usage.remove(symbol);
    }

    pub fn get_usage(&self, symbol: &Symbol) -> Option<&ReferenceCount> {
        self.usage.get(symbol)
    }

    pub fn add(&mut self, other: &Self) {
        for (symbol, v) in &other.usage {
            self.register_with(symbol, v);
        }
    }

    pub fn or(&mut self, other: &Self) {
        for (symbol, v) in &other.usage {
            let value = match self.usage.get(symbol) {
                None => v.clone(),
                Some(current) => ReferenceCount::or(current, v),
            };

            self.usage.insert(symbol.clone(), value);
        }
    }
}
