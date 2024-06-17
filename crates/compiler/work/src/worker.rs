use crate::work::Dependencies;

/// This is a trait so that we can implement both a single-threaded and a multithreaded version.
/// We *need* a single-threaded version for wasm, but it's also desirable in tests.
///
/// The general design here is "mutex granularity" - basically, try to have a mutex around each
/// individual piece of global state, and lock it for as little time as possible.
pub trait Worker<'global, Event, ModuleId> {
    fn enqueue_event(&mut self, event: Event);
    fn with_deps(&mut self, func: impl FnMut(&mut Dependencies<'global, ModuleId>));
    // fn with_module_names(&mut self, func: impl FnMut(MutMap<ModuleId, PQModuleName<'a>>));

    // /// Phases
    // fn with_headers(&mut self, func: impl FnMut(MutMap<ModuleId, ModuleHeader<'a>>));
    // fn with_parsed(&mut self, func: impl FnMut(MutMap<ModuleId, ParsedModule<'a>>));
    // fn with_aliases(&mut self, func: impl FnMut(MutMap<ModuleId, MutMap<Symbol, (bool, Alias)>>));
    // fn with_pending_abilities(&mut self, func: impl FnMut(MutMap<ModuleId, PendingAbilitiesStore>));
    // fn with_constrained(&mut self, func: impl FnMut(MutMap<ModuleId, ConstrainedModule>));
    // fn with_typechecked(&mut self, func: impl FnMut(MutMap<ModuleId, TypeCheckedModule<'a>>));
    // fn with_checked(&mut self, func: impl FnMut(MutMap<ModuleId, CheckedModule>));
    // fn with_found_specializations(
    //     &mut self,
    //     func: impl FnMut(MutMap<ModuleId, FoundSpecializationsModule<'a>>),
    // );
    // fn with_late_specializations(
    //     &mut self,
    //     func: impl FnMut(MutMap<ModuleId, LateSpecializationsModule<'a>>),
    // );
    // fn with_external_specializations_requested(
    //     &mut self,
    //     func: impl FnMut(MutMap<ModuleId, Vec<ExternalSpecializations<'a>>>),
    // );

    // /// Various information
    // fn with_imports(&mut self, func: impl FnMut(MutMap<ModuleId, MutSet<ModuleId>>));
    // fn with_exposes(&mut self, func: impl FnMut(MutMap<ModuleId, Vec<(Symbol, Variable)>>));
    // fn with_exposed_imports(&mut self, func: impl FnMut(MutMap<ModuleId, MutMap<Symbol, Region>>));
    // fn with_top_level_thunks(&mut self, func: impl FnMut(MutMap<ModuleId, MutSet<Symbol>>));
    // fn with_documentation(&mut self, func: impl FnMut(VecMap<ModuleId, ModuleDocumentation>));
    // fn with_can_problems(
    //     &mut self,
    //     func: impl FnMut(MutMap<ModuleId, Vec<roc_problem::can::Problem>>),
    // );
    // fn with_type_problems(&mut self, func: impl FnMut(MutMap<ModuleId, Vec<TypeError>>));

    // fn with_sources(&mut self, func: impl FnMut(MutMap<ModuleId, (PathBuf, &'a str)>));
}
