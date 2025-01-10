use crate::docs::ModuleDocumentation;
use crate::module::{
    CheckedModule, ConstrainedModule, FoundSpecializationsModule, LateSpecializationsModule,
    ModuleHeader, ParsedModule, TypeCheckedModule,
};
use roc_can::abilities::PendingAbilitiesStore;
use roc_can::module::ModuleParams;
use roc_collections::{MutMap, MutSet, VecMap};
use roc_module::ident::ModuleName;
use roc_module::symbol::{ModuleId, PQModuleName, Symbol};
use roc_mono::ir::ExternalSpecializations;
use roc_problem::Severity;
use roc_region::all::Region;
use roc_solve_problem::TypeError;
use roc_types::subs::Variable;
use roc_types::types::Alias;
use std::path::PathBuf;

/// Struct storing various intermediate stages by their ModuleId
#[derive(Debug)]
pub(crate) struct ModuleCache<'a> {
    pub(crate) module_names: MutMap<ModuleId, PQModuleName<'a>>,

    /// Phases
    pub(crate) headers: MutMap<ModuleId, ModuleHeader<'a>>,
    pub(crate) parsed: MutMap<ModuleId, ParsedModule<'a>>,
    pub(crate) aliases: MutMap<ModuleId, MutMap<Symbol, (bool, Alias)>>,
    pub(crate) pending_abilities: MutMap<ModuleId, PendingAbilitiesStore>,
    pub(crate) constrained: MutMap<ModuleId, ConstrainedModule>,
    pub(crate) module_params: MutMap<ModuleId, ModuleParams>,
    pub(crate) typechecked: MutMap<ModuleId, TypeCheckedModule<'a>>,
    pub(crate) checked: MutMap<ModuleId, CheckedModule>,
    pub(crate) found_specializations: MutMap<ModuleId, FoundSpecializationsModule<'a>>,
    pub(crate) late_specializations: MutMap<ModuleId, LateSpecializationsModule<'a>>,
    pub(crate) external_specializations_requested:
        MutMap<ModuleId, Vec<ExternalSpecializations<'a>>>,

    /// Various information
    pub(crate) imports: MutMap<ModuleId, MutSet<ModuleId>>,
    pub(crate) exposes: MutMap<ModuleId, Vec<(Symbol, Variable)>>,
    pub(crate) exposed_imports: MutMap<ModuleId, MutMap<Symbol, Region>>,
    pub(crate) top_level_thunks: MutMap<ModuleId, MutSet<Symbol>>,
    pub(crate) documentation: VecMap<ModuleId, ModuleDocumentation>,
    pub(crate) can_problems: MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    pub(crate) type_problems: MutMap<ModuleId, Vec<TypeError>>,

    pub(crate) sources: MutMap<ModuleId, (PathBuf, &'a str)>,
}

impl<'a> ModuleCache<'a> {
    pub(crate) fn has_can_errors(&self) -> bool {
        self.can_problems
            .values()
            .flatten()
            .any(|problem| problem.severity() == Severity::RuntimeError)
    }

    pub(crate) fn has_type_errors(&self) -> bool {
        self.type_problems
            .values()
            .flatten()
            .any(|problem| problem.severity() == Severity::RuntimeError)
    }

    pub fn has_errors(&self) -> bool {
        self.has_can_errors() || self.has_type_errors()
    }
}

impl Default for ModuleCache<'_> {
    fn default() -> Self {
        let mut module_names = MutMap::default();

        macro_rules! insert_builtins {
                ($($name:ident,)*) => {$(
                    module_names.insert(
                        ModuleId::$name,
                        PQModuleName::Unqualified(ModuleName::from(ModuleName::$name)),
                    );
                )*}
            }

        insert_builtins! {
            RESULT,
            LIST,
            STR,
            DICT,
            SET,
            BOOL,
            NUM,
            BOX,
            ENCODE,
            DECODE,
            HASH,
            INSPECT,
        }

        Self {
            module_names,
            headers: Default::default(),
            parsed: Default::default(),
            aliases: Default::default(),
            pending_abilities: Default::default(),
            constrained: Default::default(),
            module_params: Default::default(),
            typechecked: Default::default(),
            checked: Default::default(),
            found_specializations: Default::default(),
            late_specializations: Default::default(),
            external_specializations_requested: Default::default(),
            imports: Default::default(),
            exposed_imports: Default::default(),
            exposes: Default::default(),
            top_level_thunks: Default::default(),
            documentation: Default::default(),
            can_problems: Default::default(),
            type_problems: Default::default(),
            sources: Default::default(),
        }
    }
}
