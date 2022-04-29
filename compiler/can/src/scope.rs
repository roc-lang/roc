use roc_collections::{MutSet, SmallStringInterner, VecMap};
use roc_module::ident::{Ident, Lowercase};
use roc_module::symbol::{IdentId, IdentIds, ModuleId, Symbol};
use roc_problem::can::RuntimeError;
use roc_region::all::{Loc, Region};
use roc_types::subs::{VarStore, Variable};
use roc_types::types::{Alias, AliasKind, Type};

use crate::abilities::AbilitiesStore;

use bitvec::vec::BitVec;

#[derive(Clone, Debug)]
pub struct Scope {
    idents: IdentStore,

    locals: ScopedIdentIds,

    /// The type aliases currently in scope
    pub aliases: VecMap<Symbol, Alias>,

    /// The abilities currently in scope, and their implementors.
    pub abilities_store: AbilitiesStore,

    /// The current module being processed. This will be used to turn
    /// unqualified idents into Symbols.
    home: ModuleId,

    pub ident_ids: IdentIds,

    /// The first `exposed_ident_count` identifiers are exposed
    exposed_ident_count: usize,

    imports: Vec<(Ident, Symbol, Region)>,
}

fn add_aliases(var_store: &mut VarStore) -> VecMap<Symbol, Alias> {
    use roc_types::solved_types::{BuiltinAlias, FreeVars};

    let solved_aliases = roc_types::builtin_aliases::aliases();
    let mut aliases = VecMap::default();

    for (symbol, builtin_alias) in solved_aliases {
        let BuiltinAlias {
            region,
            vars,
            typ,
            kind,
        } = builtin_alias;

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
            kind,
        };

        aliases.insert(symbol, alias);
    }

    aliases
}

impl Scope {
    pub fn new(home: ModuleId, initial_ident_ids: IdentIds) -> Scope {
        let imports = Symbol::default_in_scope()
            .into_iter()
            .map(|(a, (b, c))| (a, b, c))
            .collect();

        Scope {
            home,
            exposed_ident_count: initial_ident_ids.len(),
            locals: ScopedIdentIds::from_ident_ids(initial_ident_ids.clone()),
            ident_ids: initial_ident_ids,
            idents: IdentStore::new(),
            aliases: VecMap::default(),
            // TODO(abilities): default abilities in scope
            abilities_store: AbilitiesStore::default(),
            imports,
        }
    }

    pub fn new_with_aliases(
        home: ModuleId,
        var_store: &mut VarStore,
        initial_ident_ids: IdentIds,
    ) -> Scope {
        let imports = Symbol::default_in_scope()
            .into_iter()
            .map(|(a, (b, c))| (a, b, c))
            .collect();

        Scope {
            home,
            exposed_ident_count: initial_ident_ids.len(),
            locals: ScopedIdentIds::from_ident_ids(initial_ident_ids.clone()),
            ident_ids: initial_ident_ids,
            idents: IdentStore::new(),
            aliases: add_aliases(var_store),
            // TODO(abilities): default abilities in scope
            abilities_store: AbilitiesStore::default(),
            imports,
        }
    }

    pub fn lookup(&self, ident: &Ident, region: Region) -> Result<Symbol, RuntimeError> {
        match self.idents.get_symbol(ident) {
            Some(symbol) => Ok(symbol),
            None => {
                for (import, symbol, _) in self.imports.iter() {
                    if ident == import {
                        return Ok(*symbol);
                    }
                }

                let error = RuntimeError::LookupNotInScope(
                    Loc {
                        region,
                        value: ident.clone(),
                    },
                    self.idents_in_scope().map(|v| v.as_ref().into()).collect(),
                );

                Err(error)
            }
        }
    }

    fn idents_in_scope(&self) -> impl Iterator<Item = Ident> + '_ {
        let it1 = self.locals.idents_in_scope();
        let it2 = self.imports.iter().map(|t| t.0.clone());

        it2.chain(it1)
    }

    pub fn lookup_alias(&self, symbol: Symbol) -> Option<&Alias> {
        self.aliases.get(&symbol)
    }

    /// Check if there is an opaque type alias referenced by `opaque_ref` referenced in the
    /// current scope. E.g. `@Age` must reference an opaque `Age` declared in this module, not any
    /// other!
    pub fn lookup_opaque_ref(
        &self,
        opaque_ref: &str,
        lookup_region: Region,
    ) -> Result<(Symbol, &Alias), RuntimeError> {
        debug_assert!(opaque_ref.starts_with('@'));
        let opaque = opaque_ref[1..].into();

        match self.locals.has_in_scope(&opaque) {
            Some((ident_id, _)) => {
                let symbol = Symbol::new(self.home, ident_id);

                match self.aliases.get(&symbol) {
                    None => Err(self.opaque_not_defined_error(opaque, lookup_region, None)),

                    Some(alias) => match alias.kind {
                        // The reference is to a proper alias like `Age : U32`, not an opaque type!
                        AliasKind::Structural => Err(self.opaque_not_defined_error(
                            opaque,
                            lookup_region,
                            Some(alias.header_region()),
                        )),
                        // All is good
                        AliasKind::Opaque => Ok((symbol, alias)),
                    },
                }
            }
            None => {
                for (import, _, decl_region) in self.imports.iter() {
                    if &opaque == import {
                        // The reference is to an opaque type declared in another module - this is
                        // illegal, as opaque types can only be wrapped/unwrapped in the scope they're
                        // declared.
                        return Err(RuntimeError::OpaqueOutsideScope {
                            opaque,
                            referenced_region: lookup_region,
                            imported_region: *decl_region,
                        });
                    }
                }

                Err(self.opaque_not_defined_error(opaque, lookup_region, None))
            }
        }
    }

    fn opaque_not_defined_error(
        &self,
        opaque: Ident,
        lookup_region: Region,
        opt_defined_alias: Option<Region>,
    ) -> RuntimeError {
        let opaques_in_scope = self
            .idents
            .iter_idents_symbols()
            .filter(|(_, sym)| {
                self.aliases
                    .get(sym)
                    .map(|alias| alias.kind)
                    .unwrap_or(AliasKind::Structural)
                    == AliasKind::Opaque
            })
            .map(|(v, _)| v.as_ref().into())
            .collect();

        RuntimeError::OpaqueNotDefined {
            usage: Loc::at(lookup_region, opaque),
            opaques_in_scope,
            opt_defined_alias,
        }
    }

    /// Introduce a new ident to scope.
    ///
    /// Returns Err if this would shadow an existing ident, including the
    /// Symbol and Region of the ident we already had in scope under that name.
    ///
    /// If this ident shadows an existing one, a new ident is allocated for the shadow. This is
    /// done so that all identifiers have unique symbols, which is important in particular when
    /// we generate code for value identifiers.
    /// If this behavior is undesirable, use [`Self::introduce_without_shadow_symbol`].
    pub fn introduce(
        &mut self,
        ident: Ident,
        region: Region,
    ) -> Result<Symbol, (Region, Loc<Ident>, Symbol)> {
        match self.introduce_without_shadow_symbol(&ident, region) {
            Ok(symbol) => Ok(symbol),
            Err((original_region, shadow)) => {
                let ident_id = self.ident_ids.add_ident(&ident);
                let symbol = Symbol::new(self.home, ident_id);

                Err((original_region, shadow, symbol))
            }
        }
    }

    /// Like [Self::introduce], but does not introduce a new symbol for the shadowing symbol.
    pub fn introduce_without_shadow_symbol(
        &mut self,
        ident: &Ident,
        region: Region,
    ) -> Result<Symbol, (Region, Loc<Ident>)> {
        match self.locals.has_in_scope(ident) {
            Some((_, original_region)) => {
                let shadow = Loc {
                    value: ident.clone(),
                    region,
                };
                Err((original_region, shadow))
            }
            None => {
                assert!(self.locals.has_in_scope(ident).is_none());

                for (import, _, original_region) in self.imports.iter() {
                    if ident == import {
                        let shadow = Loc {
                            value: ident.clone(),
                            region,
                        };
                        return Err((*original_region, shadow));
                    }
                }

                Ok(self.commit_introduction(ident, region))
            }
        }
    }

    /// Like [Self::introduce], but handles the case of when an ident matches an ability member
    /// name. In such cases a new symbol is created for the ident (since it's expected to be a
    /// specialization of the ability member), but the ident is not added to the ident->symbol map.
    ///
    /// If the ident does not match an ability name, the behavior of this function is exactly that
    /// of `introduce`.
    #[allow(clippy::type_complexity)]
    pub fn introduce_or_shadow_ability_member(
        &mut self,
        ident: Ident,
        region: Region,
    ) -> Result<(Symbol, Option<Symbol>), (Region, Loc<Ident>, Symbol)> {
        match self.idents.get_index(&ident) {
            Some(index) => {
                let original_symbol = self.idents.symbols[index];
                let original_region = self.idents.regions[index];

                let shadow_ident_id = self.ident_ids.add_ident(&ident);
                let shadow_symbol = Symbol::new(self.home, shadow_ident_id);

                if self.abilities_store.is_ability_member_name(original_symbol) {
                    self.abilities_store
                        .register_specializing_symbol(shadow_symbol, original_symbol);

                    // Add a symbol for the shadow, but don't re-associate the member name.
                    let dummy = Ident::default();
                    self.idents.insert_unchecked(&dummy, shadow_symbol, region);

                    Ok((shadow_symbol, Some(original_symbol)))
                } else {
                    // This is an illegal shadow.
                    let shadow = Loc {
                        value: ident.clone(),
                        region,
                    };

                    Err((original_region, shadow, shadow_symbol))
                }
            }
            None => {
                let new_symbol = self.commit_introduction(&ident, region);
                Ok((new_symbol, None))
            }
        }
    }

    fn commit_introduction(&mut self, ident: &Ident, region: Region) -> Symbol {
        // if the identifier is exposed, use the IdentId we already have for it
        let ident_id = match self.ident_ids.get_id(ident) {
            Some(ident_id) if ident_id.index() < self.exposed_ident_count => ident_id,
            _ => self.ident_ids.add_ident(ident),
        };

        let symbol = Symbol::new(self.home, ident_id);

        self.idents.insert_unchecked(ident, symbol, region);

        self.locals.introduce_into_scope(ident, region);

        symbol
    }

    /// Ignore an identifier.
    ///
    /// Used for record guards like { x: Just _ }
    pub fn ignore(&mut self, ident: &Ident) -> Symbol {
        let ident_id = self.ident_ids.add_ident(ident);
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
        for t in self.imports.iter() {
            if t.0 == ident {
                return Err((t.1, t.2));
            }
        }

        self.imports.push((ident, symbol, region));

        Ok(())
    }

    pub fn add_alias(
        &mut self,
        name: Symbol,
        region: Region,
        vars: Vec<Loc<(Lowercase, Variable)>>,
        typ: Type,
        kind: AliasKind,
    ) {
        let alias = create_alias(name, region, vars, typ, kind);
        self.aliases.insert(name, alias);
    }

    pub fn contains_alias(&mut self, name: Symbol) -> bool {
        self.aliases.contains_key(&name)
    }

    pub fn inner_scope<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Scope) -> T,
    {
        // store enough information to roll back to the original outer scope
        //
        // - abilities_store: ability definitions not allowed in inner scopes
        // - ident_ids: identifiers in inner scopes should still be available in the ident_ids
        // - idents: we have to clone for now
        // - aliases: stored in a VecMap, we just discard anything added in an inner scope
        // - exposed_ident_count: unchanged
        let idents = self.idents.clone();
        let aliases_count = self.aliases.len();
        let locals_snapshot = self.locals.snapshot();

        let result = f(self);

        self.idents = idents;
        self.aliases.truncate(aliases_count);
        self.locals.revert(locals_snapshot);

        result
    }

    pub fn register_debug_idents(&self) {
        self.home.register_debug_idents(&self.ident_ids)
    }

    /// Generates a unique, new symbol like "$1" or "$5",
    /// using the home module as the module_id.
    ///
    /// This is used, for example, during canonicalization of an Expr::Closure
    /// to generate a unique symbol to refer to that closure.
    pub fn gen_unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }
}

pub fn create_alias(
    name: Symbol,
    region: Region,
    vars: Vec<Loc<(Lowercase, Variable)>>,
    typ: Type,
    kind: AliasKind,
) -> Alias {
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

    Alias {
        region,
        type_variables: vars,
        lambda_set_variables,
        recursion_variables,
        typ,
        kind,
    }
}

#[derive(Clone, Debug)]
struct IdentStore {
    interner: SmallStringInterner,

    /// A Symbol for each Ident
    symbols: Vec<Symbol>,

    /// A Region for each Ident
    regions: Vec<Region>,
}

impl IdentStore {
    fn new() -> Self {
        let capacity = 64;

        Self {
            interner: SmallStringInterner::with_capacity(capacity),
            symbols: Vec::with_capacity(capacity),
            regions: Vec::with_capacity(capacity),
        }
    }

    fn iter_idents_symbols(&self) -> impl Iterator<Item = (Ident, Symbol)> + '_ {
        self.interner
            .iter()
            .zip(self.symbols.iter())
            .filter_map(move |(string, symbol)| {
                // empty slice is used when ability members are shadowed
                if string.is_empty() {
                    None
                } else {
                    Some((Ident::from(string), *symbol))
                }
            })
    }

    fn get_index(&self, ident: &Ident) -> Option<usize> {
        let ident_str = ident.as_inline_str().as_str();

        self.interner.find_index(ident_str)
    }

    fn get_symbol(&self, ident: &Ident) -> Option<Symbol> {
        Some(self.symbols[self.get_index(ident)?])
    }

    fn get_symbol_and_region(&self, ident: &Ident) -> Option<(Symbol, Region)> {
        let index = self.get_index(ident)?;

        Some((self.symbols[index], self.regions[index]))
    }

    /// Does not check that the ident is unique
    fn insert_unchecked(&mut self, ident: &Ident, symbol: Symbol, region: Region) {
        let ident_str = ident.as_inline_str().as_str();

        let index = self.interner.insert(ident_str);

        debug_assert_eq!(index, self.symbols.len());
        debug_assert_eq!(index, self.regions.len());

        self.symbols.push(symbol);
        self.regions.push(region);
    }
}

#[derive(Clone, Debug)]
struct ScopedIdentIds {
    ident_ids: IdentIds,
    in_scope: BitVec,
    regions: Vec<Region>,
}

impl ScopedIdentIds {
    fn from_ident_ids(ident_ids: IdentIds) -> Self {
        let capacity = ident_ids.len();

        Self {
            in_scope: BitVec::repeat(false, capacity),
            ident_ids,
            regions: std::iter::repeat(Region::zero()).take(capacity).collect(),
        }
    }

    fn len(&self) -> usize {
        self.in_scope.count_ones()
    }

    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    fn snapshot(&self) -> usize {
        debug_assert_eq!(self.ident_ids.len(), self.in_scope.len());

        self.ident_ids.len()
    }

    fn revert(&mut self, snapshot: usize) {
        for i in snapshot..self.in_scope.len() {
            self.in_scope.set(i, false);
        }
    }

    fn has_in_scope(&self, ident: &Ident) -> Option<(IdentId, Region)> {
        self.ident_ids
            .ident_strs()
            .zip(self.in_scope.iter())
            .find_map(|((ident_id, string), keep)| {
                if *keep && string == ident.as_str() {
                    Some((ident_id, self.regions[ident_id.index()]))
                } else {
                    None
                }
            })
    }

    fn idents_in_scope(&self) -> impl Iterator<Item = Ident> + '_ {
        self.ident_ids
            .ident_strs()
            .zip(self.in_scope.iter())
            .filter_map(|((_, string), keep)| {
                if *keep {
                    Some(Ident::from(string))
                } else {
                    None
                }
            })
    }

    fn introduce_into_scope(&mut self, ident_name: &Ident, region: Region) -> IdentId {
        let id = self.ident_ids.add_ident(ident_name);

        debug_assert_eq!(id.index(), self.in_scope.len());
        debug_assert_eq!(id.index(), self.regions.len());

        self.in_scope.push(true);
        self.regions.push(region);

        id
    }

    fn scopeless(&mut self, ident_name: &Ident, region: Region) -> IdentId {
        let id = self.ident_ids.add_ident(ident_name);

        debug_assert_eq!(id.index(), self.in_scope.len());
        debug_assert_eq!(id.index(), self.regions.len());

        self.in_scope.push(false);
        self.regions.push(region);

        id
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use roc_module::symbol::ModuleIds;
    use roc_region::all::Position;

    use pretty_assertions::{assert_eq, assert_ne};

    #[test]
    fn scope_contains_introduced() {
        let _register_module_debug_names = ModuleIds::default();
        let mut scope = Scope::new(ModuleId::ATTR, IdentIds::default());

        let region = Region::zero();
        let ident = Ident::from("mezolit");

        assert!(scope.lookup(&ident, region).is_err());

        assert!(scope.introduce(ident.clone(), region).is_ok());

        assert!(scope.lookup(&ident, region).is_ok());
    }

    #[test]
    fn second_introduce_shadows() {
        let _register_module_debug_names = ModuleIds::default();
        let mut scope = Scope::new(ModuleId::ATTR, IdentIds::default());

        let region1 = Region::from_pos(Position { offset: 10 });
        let region2 = Region::from_pos(Position { offset: 20 });
        let ident = Ident::from("mezolit");

        assert!(scope.lookup(&ident, Region::zero()).is_err());

        let first = scope.introduce(ident.clone(), region1).unwrap();
        let (original_region, _ident, shadow_symbol) =
            scope.introduce(ident.clone(), region2).unwrap_err();

        scope.register_debug_idents();

        assert_ne!(first, shadow_symbol);
        assert_eq!(original_region, region1);

        let lookup = scope.lookup(&ident, Region::zero()).unwrap();

        assert_eq!(first, lookup);
    }

    #[test]
    fn inner_scope_does_not_influence_outer() {
        let _register_module_debug_names = ModuleIds::default();
        let mut scope = Scope::new(ModuleId::ATTR, IdentIds::default());

        let region = Region::zero();
        let ident = Ident::from("uránia");

        assert!(scope.lookup(&ident, region).is_err());

        scope.inner_scope(|inner| {
            assert!(inner.introduce(ident.clone(), region).is_ok());
        });

        assert!(scope.lookup(&ident, region).is_err());
    }

    #[test]
    fn default_idents_in_scope() {
        let _register_module_debug_names = ModuleIds::default();
        let scope = Scope::new(ModuleId::ATTR, IdentIds::default());

        let idents: Vec<_> = scope.idents_in_scope().collect();

        assert_eq!(
            &idents,
            &[
                Ident::from("Box"),
                Ident::from("Set"),
                Ident::from("Dict"),
                Ident::from("Str"),
                Ident::from("Ok"),
                Ident::from("False"),
                Ident::from("List"),
                Ident::from("True"),
                Ident::from("Err"),
            ]
        );
    }

    #[test]
    fn idents_with_inner_scope() {
        let _register_module_debug_names = ModuleIds::default();
        let mut scope = Scope::new(ModuleId::ATTR, IdentIds::default());

        let idents: Vec<_> = scope.idents_in_scope().collect();

        assert_eq!(
            &idents,
            &[
                Ident::from("Box"),
                Ident::from("Set"),
                Ident::from("Dict"),
                Ident::from("Str"),
                Ident::from("Ok"),
                Ident::from("False"),
                Ident::from("List"),
                Ident::from("True"),
                Ident::from("Err"),
            ]
        );

        let builtin_count = idents.len();

        let region = Region::zero();

        let ident1 = Ident::from("uránia");
        let ident2 = Ident::from("malmok");
        let ident3 = Ident::from("Járnak");

        scope.introduce(ident1.clone(), region).unwrap();
        scope.introduce(ident2.clone(), region).unwrap();
        scope.introduce(ident3.clone(), region).unwrap();

        let idents: Vec<_> = scope.idents_in_scope().collect();

        assert_eq!(
            &idents[builtin_count..],
            &[ident1.clone(), ident2.clone(), ident3.clone(),]
        );

        scope.inner_scope(|inner| {
            let ident4 = Ident::from("Ångström");
            let ident5 = Ident::from("Sirály");

            inner.introduce(ident4.clone(), region).unwrap();
            inner.introduce(ident5.clone(), region).unwrap();

            let idents: Vec<_> = inner.idents_in_scope().collect();

            assert_eq!(
                &idents[builtin_count..],
                &[
                    ident1.clone(),
                    ident2.clone(),
                    ident3.clone(),
                    ident4,
                    ident5
                ]
            );
        });

        let idents: Vec<_> = scope.idents_in_scope().collect();

        assert_eq!(&idents[builtin_count..], &[ident1, ident2, ident3,]);
    }

    #[test]
    fn import_is_in_scope() {
        let _register_module_debug_names = ModuleIds::default();
        let mut scope = Scope::new(ModuleId::ATTR, IdentIds::default());

        let ident = Ident::from("product");
        let symbol = Symbol::LIST_PRODUCT;
        let region = Region::zero();

        assert!(scope.lookup(&ident, region).is_err());

        assert!(scope.import(ident.clone(), symbol, region).is_ok());

        assert!(scope.lookup(&ident, region).is_ok());

        assert!(scope.idents_in_scope().any(|x| x == ident));
    }

    #[test]
    fn shadow_of_import() {
        let _register_module_debug_names = ModuleIds::default();
        let mut scope = Scope::new(ModuleId::ATTR, IdentIds::default());

        let ident = Ident::from("product");
        let symbol = Symbol::LIST_PRODUCT;

        let region1 = Region::from_pos(Position { offset: 10 });
        let region2 = Region::from_pos(Position { offset: 20 });

        scope.import(ident.clone(), symbol, region1).unwrap();

        let (original_region, _ident, shadow_symbol) =
            scope.introduce(ident.clone(), region2).unwrap_err();

        scope.register_debug_idents();

        assert_ne!(symbol, shadow_symbol);
        assert_eq!(original_region, region1);

        let lookup = scope.lookup(&ident, Region::zero()).unwrap();

        assert_eq!(symbol, lookup);
    }
}
