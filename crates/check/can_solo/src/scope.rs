use roc_collections::VecMap;
use roc_module::ident::ModuleName;
use roc_module::symbol::{IdentId, IdentIds, ModuleId, ModuleIds, Symbol};
use roc_problem::can::ScopeModuleSource;
use roc_region::all::Region;
use roc_types::subs::Variable;
use roc_types::types::{Alias, EarlyReturnKind};

use bitvec::vec::BitVec;

#[derive(Clone, Debug)]
pub struct SoloScope {
    /// The type aliases currently in scope
    pub aliases: VecMap<Symbol, Alias>,

    #[allow(dead_code)]
    /// The current module being processed. This will be used to turn
    /// unqualified idents into Symbols.
    solo_home: ModuleId,

    /// Modules that are imported
    pub modules: ScopeModules,

    /// Identifiers that are in scope, and defined in the current module
    pub locals: ScopedIdentIds,

    pub early_returns: Vec<(Variable, Region, EarlyReturnKind)>,
}

impl Default for SoloScope {
    fn default() -> Self {
        Self::new()
    }
}

impl SoloScope {
    pub fn new() -> Self {
        let solo_home = ModuleId::first_after_builtins();

        Self {
            solo_home,
            locals: ScopedIdentIds::from_ident_ids(solo_home),
            aliases: VecMap::default(),
            modules: ScopeModules::new(solo_home),
            early_returns: Vec::default(),
        }
    }

    /// Generates a unique new symbol and return the symbol's unqualified identifier name.
    pub fn gen_unique_symbol_name(&mut self) -> &str {
        let ident_id = self.locals.gen_unique();
        self.locals.ident_ids.get_name(ident_id).unwrap()
    }
}

#[derive(Clone, Debug)]
pub struct ScopedIdentIds {
    pub ident_ids: IdentIds,
    in_scope: BitVec,
    regions: Vec<Region>,
    #[allow(dead_code)]
    solo_home: ModuleId,
}

impl ScopedIdentIds {
    fn from_ident_ids(solo_home: ModuleId) -> Self {
        Self {
            in_scope: BitVec::repeat(false, 0),
            ident_ids: IdentIds::default(),
            regions: vec![Region::zero(); 0],
            solo_home,
        }
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

#[derive(Debug, Clone)]
pub struct ScopeModules {
    /// The ids of all modules in scope
    ids: Vec<ModuleId>,
    /// The alias or original name of each module in scope
    names: Vec<ModuleName>,
    /// Why is this module in scope?
    sources: Vec<ScopeModuleSource>,
    /// The params of a module if any
    params: Vec<Option<(Variable, Symbol)>>,
}

impl ScopeModules {
    pub fn new(home_id: ModuleId) -> Self {
        let builtins = ModuleIds::default();
        let builtins_iter = builtins.iter();
        let count = builtins_iter.len();

        let mut ids = Vec::with_capacity(count + 1);
        let mut names = Vec::with_capacity(count + 1);
        let mut sources = vec![ScopeModuleSource::Builtin; count];
        let mut params = vec![None; count];

        for (module_id, module_name) in builtins_iter {
            ids.push(module_id);
            names.push(module_name.clone());
        }

        if !home_id.is_builtin() {
            ids.push(home_id);
            names.push("".into());
            sources.push(ScopeModuleSource::Current);
            params.push(None);
        }

        Self {
            ids,
            names,
            sources,
            params,
        }
    }

    pub fn lookup(&self, module_name: &ModuleName) -> Option<ModuleLookup> {
        self.names
            .iter()
            .position(|name| name == module_name)
            .map(|index| ModuleLookup {
                id: self.ids[index],
                params: self.params[index],
            })
    }

    pub fn lookup_by_id(&self, module_id: &ModuleId) -> Option<ModuleLookup> {
        self.ids
            .iter()
            .position(|id| id == module_id)
            .map(|index| ModuleLookup {
                id: self.ids[index],
                params: self.params[index],
            })
    }

    pub fn available_names(&self) -> impl Iterator<Item = &ModuleName> {
        self.names.iter()
    }

    pub fn insert(
        &mut self,
        module_name: ModuleName,
        module_id: ModuleId,
        params: Option<(Variable, Symbol)>,
        region: Region,
    ) -> Result<(), ScopeModuleSource> {
        if let Some(index) = self.names.iter().position(|name| name == &module_name) {
            if self.ids[index] == module_id {
                return Ok(());
            }

            return Err(self.sources[index]);
        }

        self.ids.push(module_id);
        self.names.push(module_name);
        self.sources.push(ScopeModuleSource::Import(region));
        self.params.push(params);
        Ok(())
    }

    pub fn len(&self) -> usize {
        debug_assert_eq!(self.ids.len(), self.names.len());
        debug_assert_eq!(self.ids.len(), self.sources.len());
        debug_assert_eq!(self.ids.len(), self.params.len());
        self.ids.len()
    }

    pub fn is_empty(&self) -> bool {
        self.ids.is_empty()
    }

    pub fn truncate(&mut self, len: usize) {
        self.ids.truncate(len);
        self.names.truncate(len);
        self.sources.truncate(len);
        self.params.truncate(len);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct SymbolLookup {
    pub symbol: Symbol,
    pub module_params: Option<(Variable, Symbol)>,
}

impl SymbolLookup {
    pub fn new(symbol: Symbol, params: Option<(Variable, Symbol)>) -> Self {
        Self {
            symbol,
            module_params: params,
        }
    }

    pub fn no_params(symbol: Symbol) -> Self {
        Self::new(symbol, None)
    }
}

pub struct ModuleLookup {
    pub id: ModuleId,
    pub params: Option<(Variable, Symbol)>,
}

impl ModuleLookup {
    pub fn into_symbol(&self, symbol: Symbol) -> SymbolLookup {
        debug_assert_eq!(symbol.module_id(), self.id);

        SymbolLookup {
            symbol,
            module_params: self.params,
        }
    }
}
