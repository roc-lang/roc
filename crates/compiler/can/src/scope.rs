use roc_collections::{VecMap, VecSet};
use roc_module::ident::Ident;
use roc_module::symbol::{IdentId, IdentIds, ModuleId, Symbol};
use roc_problem::can::RuntimeError;
use roc_region::all::{Loc, Region};
use roc_types::types::{Alias, AliasKind, AliasVar, Type};

use crate::abilities::PendingAbilitiesStore;

use bitvec::vec::BitVec;

// ability -> member names
pub(crate) type PendingAbilitiesInScope = VecMap<Symbol, VecSet<Symbol>>;

#[derive(Clone, Debug)]
pub struct Scope {
    /// The type aliases currently in scope
    pub aliases: VecMap<Symbol, Alias>,

    /// The abilities currently in scope, and their implementors.
    pub abilities_store: PendingAbilitiesStore,

    /// The current module being processed. This will be used to turn
    /// unqualified idents into Symbols.
    home: ModuleId,

    /// The first `exposed_ident_count` identifiers are exposed
    exposed_ident_count: usize,

    /// Identifiers that are imported (and introduced in the header)
    imports: Vec<(Ident, Symbol, Region)>,

    /// Shadows of an ability member, for example a local specialization of `eq` for the ability
    /// member `Eq has eq : a, a -> Bool | a has Eq` gets a shadow symbol it can use for its
    /// implementation.
    ///
    /// Only one shadow of an ability member is permitted per scope.
    shadows: VecMap<Symbol, Loc<Symbol>>,

    /// Identifiers that are in scope, and defined in the current module
    pub locals: ScopedIdentIds,
}

impl Scope {
    pub fn new(
        home: ModuleId,
        initial_ident_ids: IdentIds,
        starting_abilities_store: PendingAbilitiesStore,
    ) -> Scope {
        let imports = Symbol::default_in_scope()
            .into_iter()
            .map(|(a, (b, c))| (a, b, c))
            .collect();

        Scope {
            home,
            exposed_ident_count: initial_ident_ids.len(),
            locals: ScopedIdentIds::from_ident_ids(home, initial_ident_ids),
            aliases: VecMap::default(),
            abilities_store: starting_abilities_store,
            shadows: VecMap::default(),
            imports,
        }
    }

    pub fn lookup(&self, ident: &Ident, region: Region) -> Result<Symbol, RuntimeError> {
        self.lookup_str(ident.as_str(), region)
    }

    pub fn lookup_ability_member_shadow(&self, member: Symbol) -> Option<Symbol> {
        self.shadows.get(&member).map(|loc_shadow| loc_shadow.value)
    }

    pub fn add_docs_imports(&mut self) {
        self.imports
            .push(("Dict".into(), Symbol::DICT_DICT, Region::zero()));
        self.imports
            .push(("Set".into(), Symbol::SET_SET, Region::zero()));
    }

    pub fn lookup_str(&self, ident: &str, region: Region) -> Result<Symbol, RuntimeError> {
        use ContainsIdent::*;

        match self.scope_contains_ident(ident) {
            InScope(symbol, _) => Ok(symbol),
            NotInScope(_) | NotPresent => {
                let error = RuntimeError::LookupNotInScope(
                    Loc {
                        region,
                        value: Ident::from(ident),
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

    /// Check if there is an opaque type alias referenced by `opaque_ref` referenced in the
    /// current scope. E.g. `@Age` must reference an opaque `Age` declared in this module, not any
    /// other!
    pub fn lookup_opaque_ref(
        &self,
        opaque_ref: &str,
        lookup_region: Region,
    ) -> Result<(Symbol, &Alias), RuntimeError> {
        debug_assert!(opaque_ref.starts_with('@'));
        let opaque_str = &opaque_ref[1..];
        let opaque = opaque_str.into();

        match self.locals.has_in_scope(&opaque) {
            Some((symbol, _)) => match self.lookup_opaque_alias(symbol) {
                Ok(alias) => Ok((symbol, alias)),
                Err(opt_alias_def_region) => {
                    Err(self.opaque_not_defined_error(opaque, lookup_region, opt_alias_def_region))
                }
            },
            None => {
                // opaque types can only be wrapped/unwrapped in the scope they are defined in (and below)
                let error = if let Some((_, decl_region)) = self.has_imported(opaque_str) {
                    // specific error for when the opaque is imported, which definitely does not work
                    RuntimeError::OpaqueOutsideScope {
                        opaque,
                        referenced_region: lookup_region,
                        imported_region: decl_region,
                    }
                } else {
                    self.opaque_not_defined_error(opaque, lookup_region, None)
                };

                Err(error)
            }
        }
    }

    fn lookup_opaque_alias(&self, symbol: Symbol) -> Result<&Alias, Option<Region>> {
        match self.aliases.get(&symbol) {
            None => Err(None),

            Some(alias) => match alias.kind {
                AliasKind::Opaque => Ok(alias),
                AliasKind::Structural => Err(Some(alias.header_region())),
            },
        }
    }

    fn is_opaque(&self, ident_id: IdentId, string: &str) -> Option<Box<str>> {
        if string.is_empty() {
            return None;
        }

        let symbol = Symbol::new(self.home, ident_id);

        if let Some(AliasKind::Opaque) = self.aliases.get(&symbol).map(|alias| alias.kind) {
            Some(string.into())
        } else {
            None
        }
    }

    fn opaque_not_defined_error(
        &self,
        opaque: Ident,
        lookup_region: Region,
        opt_defined_alias: Option<Region>,
    ) -> RuntimeError {
        // for opaques, we only look at the locals because opaques can only be matched
        // on in the module that defines them.
        let opaques_in_scope = self
            .locals
            .ident_ids
            .ident_strs()
            .filter_map(|(ident_id, string)| self.is_opaque(ident_id, string))
            .collect();

        RuntimeError::OpaqueNotDefined {
            usage: Loc::at(lookup_region, opaque),
            opaques_in_scope,
            opt_defined_alias,
        }
    }

    fn has_imported(&self, ident: &str) -> Option<(Symbol, Region)> {
        for (import, shadow, original_region) in self.imports.iter() {
            if ident == import.as_str() {
                return Some((*shadow, *original_region));
            }
        }

        None
    }

    /// Is an identifier in scope, either in the locals or imports
    fn scope_contains_ident(&self, ident: &str) -> ContainsIdent {
        // exposed imports are likely to be small
        match self.has_imported(ident) {
            Some((symbol, region)) => ContainsIdent::InScope(symbol, region),
            None => self.locals.contains_ident(ident),
        }
    }

    fn introduce_help(&mut self, ident: &str, region: Region) -> Result<Symbol, (Symbol, Region)> {
        match self.scope_contains_ident(ident) {
            ContainsIdent::InScope(original_symbol, original_region) => {
                // the ident is already in scope; up to the caller how to handle that
                // (usually it's shadowing, but it is valid to shadow ability members)
                Err((original_symbol, original_region))
            }
            ContainsIdent::NotPresent => {
                // We know nothing about this ident yet; introduce it to the scope
                let ident_id = self.locals.introduce_into_scope(ident, region);
                Ok(Symbol::new(self.home, ident_id))
            }
            ContainsIdent::NotInScope(existing) => {
                // The ident is not in scope, but its name is already in the string interner
                if existing.index() < self.exposed_ident_count {
                    // if the identifier is exposed, use the IdentId we already have for it
                    // other modules depend on the symbol having that IdentId
                    let symbol = Symbol::new(self.home, existing);

                    self.locals.in_scope.set(existing.index(), true);
                    self.locals.regions[existing.index()] = region;

                    Ok(symbol)
                } else {
                    // create a new IdentId that under the hood uses the same string bytes as an existing one
                    let ident_id = self.locals.introduce_into_scope_duplicate(existing, region);

                    Ok(Symbol::new(self.home, ident_id))
                }
            }
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
    ) -> Result<Symbol, (Loc<Symbol>, Loc<Ident>, Symbol)> {
        self.introduce_str(ident.as_str(), region)
    }

    pub fn introduce_str(
        &mut self,
        ident: &str,
        region: Region,
    ) -> Result<Symbol, (Loc<Symbol>, Loc<Ident>, Symbol)> {
        match self.introduce_help(ident, region) {
            Ok(symbol) => Ok(symbol),
            Err((shadowed_symbol, original_region)) => {
                let shadow = Loc {
                    value: Ident::from(ident),
                    region,
                };
                let symbol = self.locals.scopeless_symbol(ident, region);

                Err((Loc::at(original_region, shadowed_symbol), shadow, symbol))
            }
        }
    }

    /// Like [Self::introduce], but does not introduce a new symbol for the shadowing symbol.
    pub fn introduce_without_shadow_symbol(
        &mut self,
        ident: &Ident,
        region: Region,
    ) -> Result<Symbol, (Region, Loc<Ident>)> {
        match self.introduce_help(ident.as_str(), region) {
            Err((_, original_region)) => {
                let shadow = Loc {
                    value: ident.clone(),
                    region,
                };
                Err((original_region, shadow))
            }
            Ok(symbol) => Ok(symbol),
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
        pending_abilities_in_scope: &PendingAbilitiesInScope,
        ident: Ident,
        region: Region,
    ) -> Result<(Symbol, Option<Symbol>), (Region, Loc<Ident>, Symbol)> {
        let ident = &ident;

        match self.introduce_help(ident.as_str(), region) {
            Err((original_symbol, original_region)) => {
                let shadow_symbol = self.scopeless_symbol(ident, region);

                if self.abilities_store.is_ability_member_name(original_symbol)
                    || pending_abilities_in_scope
                        .iter()
                        .any(|(_, members)| members.iter().any(|m| *m == original_symbol))
                {
                    match self.shadows.get(&original_symbol) {
                        Some(loc_original_shadow) => {
                            // Duplicate shadow of an ability members; that's illegal.
                            let shadow = Loc {
                                value: ident.clone(),
                                region,
                            };
                            Err((loc_original_shadow.region, shadow, shadow_symbol))
                        }
                        None => {
                            self.shadows
                                .insert(original_symbol, Loc::at(region, shadow_symbol));

                            Ok((shadow_symbol, Some(original_symbol)))
                        }
                    }
                } else {
                    // This is an illegal shadow.
                    let shadow = Loc {
                        value: ident.clone(),
                        region,
                    };

                    Err((original_region, shadow, shadow_symbol))
                }
            }
            Ok(symbol) => Ok((symbol, None)),
        }
    }

    pub fn get_member_shadow(&self, ability_member: Symbol) -> Option<&Loc<Symbol>> {
        self.shadows.get(&ability_member)
    }

    /// Create a new symbol, but don't add it to the scope (yet)
    ///
    /// Used for record guards like { x: Just _ } where the `x` is not added to the scope,
    /// but also in other places where we need to create a symbol and we don't have the right
    /// scope information yet. An identifier can be introduced later, and will use the same IdentId
    pub fn scopeless_symbol(&mut self, ident: &Ident, region: Region) -> Symbol {
        self.locals.scopeless_symbol(ident.as_str(), region)
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
        if let Some((s, r)) = self.has_imported(ident.as_str()) {
            return Err((s, r));
        }

        self.imports.push((ident, symbol, region));

        Ok(())
    }

    pub fn add_alias(
        &mut self,
        name: Symbol,
        region: Region,
        vars: Vec<Loc<AliasVar>>,
        typ: Type,
        kind: AliasKind,
    ) {
        let alias = create_alias(name, region, vars, typ, kind);
        self.aliases.insert(name, alias);
    }

    pub fn lookup_alias(&self, symbol: Symbol) -> Option<&Alias> {
        self.aliases.get(&symbol)
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
        // - locals: everything introduced in the inner scope is marked as not in scope in the rollback
        // - aliases: stored in a VecMap, we just discard anything added in an inner scope
        // - exposed_ident_count: unchanged
        // - home: unchanged
        let aliases_count = self.aliases.len();
        let locals_snapshot = self.locals.in_scope.len();

        let result = f(self);

        self.aliases.truncate(aliases_count);

        // anything added in the inner scope is no longer in scope now
        for i in locals_snapshot..self.locals.in_scope.len() {
            self.locals.in_scope.set(i, false);
        }

        result
    }

    pub fn register_debug_idents(&self) {
        self.home.register_debug_idents(&self.locals.ident_ids)
    }

    /// Generates a unique, new symbol like "$1" or "$5",
    /// using the home module as the module_id.
    ///
    /// This is used, for example, during canonicalization of an Expr::Closure
    /// to generate a unique symbol to refer to that closure.
    pub fn gen_unique_symbol(&mut self) -> Symbol {
        Symbol::new(self.home, self.locals.gen_unique())
    }
}

pub fn create_alias(
    name: Symbol,
    region: Region,
    vars: Vec<Loc<AliasVar>>,
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
            hidden.remove(&loc_var.value.var);
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

#[derive(Debug)]
enum ContainsIdent {
    InScope(Symbol, Region),
    NotInScope(IdentId),
    NotPresent,
}

#[derive(Clone, Debug)]
pub struct ScopedIdentIds {
    pub ident_ids: IdentIds,
    in_scope: BitVec,
    regions: Vec<Region>,
    home: ModuleId,
}

impl ScopedIdentIds {
    fn from_ident_ids(home: ModuleId, ident_ids: IdentIds) -> Self {
        let capacity = ident_ids.len();

        Self {
            in_scope: BitVec::repeat(false, capacity),
            ident_ids,
            regions: vec![Region::zero(); capacity],
            home,
        }
    }

    fn has_in_scope(&self, ident: &Ident) -> Option<(Symbol, Region)> {
        match self.contains_ident(ident.as_str()) {
            ContainsIdent::InScope(symbol, region) => Some((symbol, region)),
            ContainsIdent::NotInScope(_) | ContainsIdent::NotPresent => None,
        }
    }

    fn contains_ident(&self, ident: &str) -> ContainsIdent {
        use ContainsIdent::*;

        let mut result = NotPresent;

        for ident_id in self.ident_ids.get_id_many(ident) {
            let index = ident_id.index();
            if self.in_scope[index] {
                return InScope(Symbol::new(self.home, ident_id), self.regions[index]);
            } else {
                result = NotInScope(ident_id)
            }
        }

        result
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

    fn introduce_into_scope(&mut self, ident_name: &str, region: Region) -> IdentId {
        let id = self.ident_ids.add_str(ident_name);

        debug_assert_eq!(id.index(), self.in_scope.len());
        debug_assert_eq!(id.index(), self.regions.len());

        self.in_scope.push(true);
        self.regions.push(region);

        id
    }

    fn introduce_into_scope_duplicate(&mut self, existing: IdentId, region: Region) -> IdentId {
        let id = self.ident_ids.duplicate_ident(existing);

        debug_assert_eq!(id.index(), self.in_scope.len());
        debug_assert_eq!(id.index(), self.regions.len());

        self.in_scope.push(true);
        self.regions.push(region);

        id
    }

    /// Adds an IdentId, but does not introduce it to the scope
    fn scopeless_symbol(&mut self, ident_name: &str, region: Region) -> Symbol {
        let id = self.ident_ids.add_str(ident_name);

        debug_assert_eq!(id.index(), self.in_scope.len());
        debug_assert_eq!(id.index(), self.regions.len());

        self.in_scope.push(false);
        self.regions.push(region);

        Symbol::new(self.home, id)
    }

    fn gen_unique(&mut self) -> IdentId {
        let id = self.ident_ids.gen_unique();

        debug_assert_eq!(id.index(), self.in_scope.len());
        debug_assert_eq!(id.index(), self.regions.len());

        self.in_scope.push(false);
        self.regions.push(Region::zero());

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
        let mut scope = Scope::new(
            ModuleId::ATTR,
            IdentIds::default(),
            PendingAbilitiesStore::default(),
        );

        let region = Region::zero();
        let ident = Ident::from("mezolit");

        assert!(scope.lookup(&ident, region).is_err());

        assert!(scope.introduce(ident.clone(), region).is_ok());

        assert!(scope.lookup(&ident, region).is_ok());
    }

    #[test]
    fn second_introduce_shadows() {
        let _register_module_debug_names = ModuleIds::default();
        let mut scope = Scope::new(
            ModuleId::ATTR,
            IdentIds::default(),
            PendingAbilitiesStore::default(),
        );

        let region1 = Region::from_pos(Position { offset: 10 });
        let region2 = Region::from_pos(Position { offset: 20 });
        let ident = Ident::from("mezolit");

        assert!(scope.lookup(&ident, Region::zero()).is_err());

        let first = scope.introduce(ident.clone(), region1).unwrap();
        let (original, _ident, shadow_symbol) =
            scope.introduce(ident.clone(), region2).unwrap_err();

        scope.register_debug_idents();

        assert_ne!(first, shadow_symbol);
        assert_eq!(original.region, region1);

        let lookup = scope.lookup(&ident, Region::zero()).unwrap();

        assert_eq!(first, lookup);
    }

    #[test]
    fn inner_scope_does_not_influence_outer() {
        let _register_module_debug_names = ModuleIds::default();
        let mut scope = Scope::new(
            ModuleId::ATTR,
            IdentIds::default(),
            PendingAbilitiesStore::default(),
        );

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
        let scope = Scope::new(
            ModuleId::ATTR,
            IdentIds::default(),
            PendingAbilitiesStore::default(),
        );

        let idents: Vec<_> = scope.idents_in_scope().collect();

        assert_eq!(
            &idents,
            &[
                Ident::from("Str"),
                Ident::from("List"),
                Ident::from("Ok"),
                Ident::from("Err"),
                Ident::from("Box"),
            ]
        );
    }

    #[test]
    fn idents_with_inner_scope() {
        let _register_module_debug_names = ModuleIds::default();
        let mut scope = Scope::new(
            ModuleId::ATTR,
            IdentIds::default(),
            PendingAbilitiesStore::default(),
        );

        let idents: Vec<_> = scope.idents_in_scope().collect();

        assert_eq!(
            &idents,
            &[
                Ident::from("Str"),
                Ident::from("List"),
                Ident::from("Ok"),
                Ident::from("Err"),
                Ident::from("Box"),
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
        let mut scope = Scope::new(
            ModuleId::ATTR,
            IdentIds::default(),
            PendingAbilitiesStore::default(),
        );

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
        let mut scope = Scope::new(
            ModuleId::ATTR,
            IdentIds::default(),
            PendingAbilitiesStore::default(),
        );

        let ident = Ident::from("product");
        let symbol = Symbol::LIST_PRODUCT;

        let region1 = Region::from_pos(Position { offset: 10 });
        let region2 = Region::from_pos(Position { offset: 20 });

        scope.import(ident.clone(), symbol, region1).unwrap();

        let (original, _ident, shadow_symbol) =
            scope.introduce(ident.clone(), region2).unwrap_err();

        scope.register_debug_idents();

        assert_ne!(symbol, shadow_symbol);
        assert_eq!(original.region, region1);

        let lookup = scope.lookup(&ident, Region::zero()).unwrap();

        assert_eq!(symbol, lookup);
    }
}
