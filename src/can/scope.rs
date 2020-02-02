use crate::can::ident::{Ident, Lowercase};
use crate::can::problem::RuntimeError;
use crate::collections::ImMap;
use crate::module::symbol::{IdentIds, ModuleId, Symbol};
use crate::region::{Located, Region};
use crate::types::Type;

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    /// All the identifiers in scope, mapped to were they were defined and
    /// the Symbol they resolve to.
    idents: ImMap<Ident, (Symbol, Region)>,

    /// A cache of all the symbols in scope. This makes lookups much
    /// faster when checking for unused defs and unused arguments.
    symbols: ImMap<Symbol, Region>,

    /// The type aliases currently in scope
    aliases: ImMap<Symbol, (Region, Vec<Located<Lowercase>>, Type)>,

    /// The current module being processed. This will be used to turn
    /// unqualified idents into Symbols.
    home: ModuleId,
}

impl Scope {
    pub fn new(home: ModuleId) -> Scope {
        Scope {
            home,
            idents: ImMap::default(),
            symbols: ImMap::default(),
            aliases: ImMap::default(),
        }
    }

    pub fn idents(&self) -> impl Iterator<Item = &(Ident, (Symbol, Region))> {
        self.idents.iter()
    }

    pub fn symbols(&self) -> impl Iterator<Item = &(Symbol, Region)> {
        self.symbols.iter()
    }

    pub fn contains_ident(&self, ident: &Ident) -> bool {
        self.idents.contains_key(ident)
    }

    pub fn contains_symbol(&self, symbol: Symbol) -> bool {
        self.symbols.contains_key(&symbol)
    }

    pub fn num_idents(&self) -> usize {
        self.idents.len()
    }

    pub fn lookup(&mut self, ident: &Ident, region: Region) -> Result<Symbol, RuntimeError> {
        match self.idents.get(ident) {
            Some((symbol, _)) => Ok(*symbol),
            None => Err(RuntimeError::LookupNotInScope(Located {
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

                Err((*original_region, shadow))
            }
            None => {
                let ident_id = ident_ids.add(ident.clone().into());
                let symbol = Symbol::new(self.home, ident_id);

                self.symbols.insert(symbol, region);
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
            Some(shadowed) => Err(*shadowed),
            None => {
                self.symbols.insert(symbol, region);
                self.idents.insert(ident, (symbol, region));

                Ok(symbol)
            }
        }
    }

    pub fn add_alias(
        &mut self,
        name: Symbol,
        region: Region,
        vars: Vec<Located<Lowercase>>,
        typ: Type,
    ) {
        self.aliases.insert(name, (region, vars, typ));
    }
}
