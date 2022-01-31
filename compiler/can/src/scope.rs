use roc_collections::all::{MutSet, SendMap};
use roc_module::ident::{Ident, Lowercase};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_problem::can::RuntimeError;
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{Alias, Type};

#[derive(Clone, Debug, PartialEq)]
pub struct Scope {
    /// All the identifiers in scope, mapped to were they were defined and
    /// the Symbol they resolve to.
    idents: SendMap<Ident, (Symbol, Region)>,

    /// A cache of all the symbols in scope. This makes lookups much
    /// faster when checking for unused defs and unused arguments.
    symbols: SendMap<Symbol, Region>,

    /// The type aliases currently in scope
    aliases: SendMap<Symbol, Alias>,

    /// The current module being processed. This will be used to turn
    /// unqualified idents into Symbols.
    home: ModuleId,
}

impl Scope {
    pub fn new(home: ModuleId, var_store: &mut VarStore) -> Scope {
        use roc_types::solved_types::{BuiltinAlias, FreeVars};
        let solved_aliases = roc_types::builtin_aliases::aliases();
        let mut aliases = SendMap::default();

        for (symbol, builtin_alias) in solved_aliases {
            let BuiltinAlias { region, vars, typ } = builtin_alias;

            let mut free_vars = FreeVars::default();
            let typ = roc_types::solved_types::to_type(&typ, &mut free_vars, var_store);

            let mut variables = Vec::new();
            // make sure to sort these variables to make them line up with the type arguments
            let mut type_variables: Vec<_> = free_vars.unnamed_vars.into_iter().collect();
            type_variables.sort();
            for (loc_name, (_, var)) in vars.iter().zip(type_variables) {
                variables.push(Loc::at(loc_name.region, (loc_name.value.clone(), var)));
            }

            let alias = Alias {
                region,
                typ,
                lambda_set_variables: Vec::new(),
                recursion_variables: MutSet::default(),
                type_variables: variables,
            };

            aliases.insert(symbol, alias);
        }

        Scope {
            home,
            idents: Symbol::default_in_scope(),
            symbols: SendMap::default(),
            aliases,
        }
    }

    pub fn idents(&self) -> impl Iterator<Item = (&Ident, &(Symbol, Region))> {
        self.idents.iter()
    }

    pub fn symbols(&self) -> impl Iterator<Item = (&Symbol, &Region)> {
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

    pub fn lookup(&self, ident: &Ident, region: Region) -> Result<Symbol, RuntimeError> {
        match self.idents.get(ident) {
            Some((symbol, _)) => Ok(*symbol),
            None => Err(RuntimeError::LookupNotInScope(
                Loc {
                    region,
                    value: ident.clone(),
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
    ) -> Result<Symbol, (Region, Loc<Ident>, Symbol)> {
        match self.idents.get(&ident) {
            Some(&(_, original_region)) => {
                let shadow = Loc {
                    value: ident.clone(),
                    region,
                };

                let ident_id = all_ident_ids.add(ident.clone());
                let symbol = Symbol::new(self.home, ident_id);

                self.symbols.insert(symbol, region);
                self.idents.insert(ident, (symbol, region));

                Err((original_region, shadow, symbol))
            }
            None => {
                // If this IdentId was already added previously
                // when the value was exposed in the module header,
                // use that existing IdentId. Otherwise, create a fresh one.
                let ident_id = match exposed_ident_ids.get_id(&ident) {
                    Some(ident_id) => *ident_id,
                    None => all_ident_ids.add(ident.clone()),
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
        let ident_id = all_ident_ids.add(ident);
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
        vars: Vec<Loc<(Lowercase, Variable)>>,
        typ: Type,
    ) {
        let roc_types::types::VariableDetail {
            type_variables,
            lambda_set_variables,
            recursion_variables,
        } = typ.variables_detail();

        debug_assert!({
            let mut hidden = type_variables;

            for loc_var in vars.iter() {
                hidden.remove(&loc_var.value.1);
            }

            if !hidden.is_empty() {
                panic!(
                    "Found unbound type variables {:?} \n in type alias {:?} {:?} : {:?}",
                    hidden, name, &vars, &typ
                )
            }

            true
        });

        let lambda_set_variables: Vec<_> = lambda_set_variables
            .into_iter()
            .map(|v| roc_types::types::LambdaSet(Type::Variable(v)))
            .collect();

        let alias = Alias {
            region,
            type_variables: vars,
            lambda_set_variables,
            recursion_variables,
            typ,
        };

        self.aliases.insert(name, alias);
    }

    pub fn contains_alias(&mut self, name: Symbol) -> bool {
        self.aliases.contains_key(&name)
    }
}
