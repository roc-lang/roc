use crate::can::ident::Ident;
use crate::can::problem::RuntimeError;
use crate::collections::{ImMap, MutSet};
use crate::module::symbol::{IdentIds, ModuleId, Symbol};
use crate::region::{Located, Region};

#[derive(Debug, PartialEq)]
pub struct Scope {
    /// All the identifiers in scope, mapped to were they were defined and
    /// the Symbol they resolve to.
    idents: ImMap<Ident, (Symbol, Region)>,

    /// The current module being processed. This will be used to turn
    /// unqualified idents into Symbols.
    home: ModuleId,

    /// Symbols which have actually been referenced in this scope.
    /// Every time a Scope::lookup is performed, this gets updated.
    /// This is eventually used to determine unused defs.
    referenced: MutSet<Symbol>,

    /// Symbols which were introduced in this Scope (as opposed to those
    /// which were inherited from a parent scope).
    /// This is eventually used to determine unused defs.
    introduced: MutSet<Symbol>,
}

impl Clone for Scope {
    fn clone(&self) -> Self {
        Scope {
            idents: self.idents.clone(),
            home: self.home,

            // Always begin with an empty introduced and referenced,
            // because these should only record things that were
            // introduced and referenced since the scope was freshly cloned.
            introduced: MutSet::default(),
            referenced: MutSet::default(),
        }
    }
}

impl Scope {
    pub fn new(home: ModuleId) -> Scope {
        Scope {
            home,
            idents: ImMap::default(),
            referenced: MutSet::default(),
            introduced: MutSet::default(),
        }
    }

    pub fn idents(&self) -> impl Iterator<Item = &(Ident, (Symbol, Region))> {
        self.idents.iter()
    }

    pub fn num_idents(&self) -> usize {
        self.idents.len()
    }

    pub fn lookup(&mut self, ident: &Ident, region: Region) -> Result<Symbol, RuntimeError> {
        match self.idents.get(ident) {
            Some((symbol, _)) => {
                let symbol = *symbol;

                // Record that this symbol has been referenced.
                self.referenced.insert(symbol);

                Ok(symbol)
            }
            None => Err(RuntimeError::UnrecognizedLookup(Located {
                region,
                value: ident.clone().into(),
            })),
        }
    }

    /// Introduce a new ident to scope.
    ///
    /// Returns Err if this would shadow an existing ident, including the
    /// Symbol and Region of the ident we already had in scope under that name.
    pub fn introduce(
        &mut self,
        ident: Ident,
        ident_ids: &mut IdentIds,
        region: Region,
    ) -> Result<Symbol, (Region, Located<Ident>)> {
        match self.idents.get(&ident) {
            Some((_, original_region)) => {
                let shadow = Located {
                    value: ident,
                    region,
                };

                Err((original_region.clone(), shadow))
            }
            None => {
                let ident_id = ident_ids.add(ident.clone().into());
                let symbol = Symbol::new(self.home, ident_id);

                self.introduced.insert(symbol);
                self.idents.insert(ident, (symbol, region));

                Ok(symbol)
            }
        }
    }

    /// Import a Symbol from another module into this module's top-level scope.
    ///
    /// Returns Err if this would shadow an existing ident, including the
    /// Symbol and Region of the ident we already had in scope under that name.
    pub fn import(
        &mut self,
        ident: Ident,
        symbol: Symbol,
        region: Region,
    ) -> Result<Symbol, (Symbol, Region)> {
        match self.idents.get(&ident) {
            Some(shadowed) => Err(shadowed.clone()),
            None => {
                self.introduced.insert(symbol);
                self.idents.insert(ident, (symbol, region));

                Ok(symbol)
            }
        }
    }
}
