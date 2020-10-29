use roc_collections::all::{ImMap, MutSet};
use roc_module::ident::{Ident, Lowercase};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_problem::can::RuntimeError;
use roc_region::all::{Located, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{Alias, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    /// All the identifiers in scope, mapped to were they were defined and
    /// the Symbol they resolve to.
    idents: ImMap<Ident, (Symbol, Region)>,

    /// A cache of all the symbols in scope. This makes lookups much
    /// faster when checking for unused defs and unused arguments.
    symbols: ImMap<Symbol, Region>,

    /// The type aliases currently in scope
    aliases: ImMap<Symbol, Alias>,

    /// The current module being processed. This will be used to turn
    /// unqualified idents into Symbols.
    home: ModuleId,
}

impl Scope {
    pub fn new(home: ModuleId, var_store: &mut VarStore) -> Scope {
        use roc_types::solved_types::{BuiltinAlias, FreeVars};
        let solved_aliases = roc_types::builtin_aliases::aliases();
        let mut aliases = ImMap::default();

        for (symbol, builtin_alias) in solved_aliases {
            let BuiltinAlias { region, vars, typ } = builtin_alias;

            let mut free_vars = FreeVars::default();
            let typ = roc_types::solved_types::to_type(&typ, &mut free_vars, var_store);

            // NOTE: we make assumptions about the order of type arguments here!
            // we assume the type variables occur in the body of the alias in the same order as
            // they occur in the type

            // TODO fix that assumption
            // TODO aliases depend on one another currently, fix that
            let mut variables = Vec::new();
            for (loc_name, (_, var)) in vars.iter().zip(free_vars.unnamed_vars.iter()) {
                variables.push(Located::at(loc_name.region, (loc_name.value.clone(), *var)));
            }

            let alias = Alias {
                region,
                typ,
                hidden_variables: MutSet::default(),
                vars: variables,
                uniqueness: None,
            };

            aliases.insert(symbol, alias);
        }

        Scope {
            home,
            idents: Symbol::default_in_scope(),
            symbols: ImMap::default(),
            aliases,
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
            None => Err(RuntimeError::LookupNotInScope(
                Located {
                    region,
                    value: ident.clone().into(),
                },
                self.idents.keys().map(|v| v.as_ref().into()).collect(),
            )),
        }
    }

    pub fn lookup_alias(&self, symbol: Symbol) -> Option<&Alias> {
        self.aliases.get(&symbol)
    }

    /// Introduce a new ident to scope.
    ///
    /// Returns Err if this would shadow an existing ident, including the
    /// Symbol and Region of the ident we already had in scope under that name.
    pub fn introduce(
        &mut self,
        ident: Ident,
        exposed_ident_ids: &IdentIds,
        all_ident_ids: &mut IdentIds,
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
                // If this IdentId was already added previously
                // when the value was exposed in the module header,
                // use that existing IdentId. Otherwise, create a fresh one.
                let ident_id = match exposed_ident_ids.get_id(&ident.as_inline_str()) {
                    Some(ident_id) => *ident_id,
                    None => all_ident_ids.add(ident.clone().into()),
                };

                let symbol = Symbol::new(self.home, ident_id);

                self.symbols.insert(symbol, region);
                self.idents.insert(ident, (symbol, region));

                Ok(symbol)
            }
        }
    }

    /// Ignore an identifier.
    ///
    /// Used for record guards like { x: Just _ }
    pub fn ignore(&mut self, ident: Ident, all_ident_ids: &mut IdentIds) -> Symbol {
        let ident_id = all_ident_ids.add(ident.into());
        Symbol::new(self.home, ident_id)
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
    ) -> Result<(), (Symbol, Region)> {
        match self.idents.get(&ident) {
            Some(shadowed) => Err(*shadowed),
            None => {
                self.symbols.insert(symbol, region);
                self.idents.insert(ident, (symbol, region));

                Ok(())
            }
        }
    }

    pub fn add_alias(
        &mut self,
        name: Symbol,
        region: Region,
        vars: Vec<Located<(Lowercase, Variable)>>,
        typ: Type,
    ) {
        let mut hidden_variables = MutSet::default();
        hidden_variables.extend(typ.variables());

        for loc_var in vars.iter() {
            hidden_variables.remove(&loc_var.value.1);
        }

        let alias = Alias {
            region,
            vars,
            hidden_variables,
            uniqueness: None,
            typ,
        };

        self.aliases.insert(name, alias);
    }
}
