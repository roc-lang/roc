use crate::docs::ModuleDocumentation;
use bumpalo::Bump;
use crossbeam::channel::{bounded, Sender};
use crossbeam::deque::{Injector, Stealer, Worker};
use crossbeam::thread;
use parking_lot::Mutex;
use roc_builtins::std::StdLib;
use roc_can::constraint::Constraint;
use roc_can::def::{Declaration, Def};
use roc_can::module::{canonicalize_module_defs, Module};
use roc_collections::all::{default_hasher, BumpMap, MutMap, MutSet};
use roc_constrain::module::{
    constrain_imports, pre_constrain_imports, ConstrainableImports, Import,
};
use roc_constrain::module::{constrain_module, ExposedModuleTypes, SubsByModule};
use roc_module::ident::{Ident, ModuleName, QualifiedModuleName, TagName};
use roc_module::symbol::{
    IdentIds, Interns, ModuleId, ModuleIds, PQModuleName, PackageModuleIds, PackageQualified,
    Symbol,
};
use roc_mono::ir::{
    CapturedSymbols, EntryPoint, ExternalSpecializations, PartialProc, Proc, ProcLayout, Procs,
    UpdateModeIds,
};
use roc_mono::layout::{Layout, LayoutCache, LayoutProblem};
use roc_parse::ast::{self, ExtractSpaces, Spaced, StrLiteral, TypeAnnotation};
use roc_parse::header::PackageName;
use roc_parse::header::{ExposedName, ImportsEntry, PackageEntry, PlatformHeader, To, TypedIdent};
use roc_parse::module::module_defs;
use roc_parse::parser::{ParseProblem, Parser, SyntaxError};
use roc_region::all::{LineInfo, Loc, Region};
use roc_solve::module::SolvedModule;
use roc_solve::solve;
use roc_types::solved_types::Solved;
use roc_types::subs::{Subs, VarStore, Variable};
use roc_types::types::{Alias, Type};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::{HashMap, HashSet};
use std::io;
use std::iter;
use std::path::{Path, PathBuf};
use std::str::from_utf8_unchecked;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use std::{env, fs};

/// Default name for the binary generated for an app, if an invalid one was specified.
const DEFAULT_APP_OUTPUT_PATH: &str = "app";

/// Filename extension for normal Roc modules
const ROC_FILE_EXTENSION: &str = "roc";

/// Roc-Config file name
const PKG_CONFIG_FILE_NAME: &str = "Package-Config";

/// The . in between module names like Foo.Bar.Baz
const MODULE_SEPARATOR: char = '.';

const SHOW_MESSAGE_LOG: bool = false;

const EXPANDED_STACK_SIZE: usize = 8 * 1024 * 1024;

macro_rules! log {
    () => (if SHOW_MESSAGE_LOG { println!()} else {});
    ($($arg:tt)*) => (if SHOW_MESSAGE_LOG { println!($($arg)*); } else {})
}

/// NOTE the order of definition of the phases is used by the ord instance
/// make sure they are ordered from first to last!
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
pub enum Phase {
    LoadHeader,
    Parse,
    CanonicalizeAndConstrain,
    SolveTypes,
    FindSpecializations,
    MakeSpecializations,
}

/// NOTE keep up to date manually, from ParseAndGenerateConstraints to the highest phase we support
const PHASES: [Phase; 6] = [
    Phase::LoadHeader,
    Phase::Parse,
    Phase::CanonicalizeAndConstrain,
    Phase::SolveTypes,
    Phase::FindSpecializations,
    Phase::MakeSpecializations,
];

#[derive(Debug)]
enum Status {
    NotStarted,
    Pending,
    Done,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Job<'a> {
    Step(ModuleId, Phase),
    ResolveShorthand(&'a str),
}

#[derive(Default, Debug)]
struct Dependencies<'a> {
    waiting_for: MutMap<Job<'a>, MutSet<Job<'a>>>,
    notifies: MutMap<Job<'a>, MutSet<Job<'a>>>,
    status: MutMap<Job<'a>, Status>,
}

impl<'a> Dependencies<'a> {
    /// Add all the dependencies for a module, return (module, phase) pairs that can make progress
    pub fn add_module(
        &mut self,
        module_id: ModuleId,
        dependencies: &MutSet<PackageQualified<'a, ModuleId>>,
        goal_phase: Phase,
    ) -> MutSet<(ModuleId, Phase)> {
        use Phase::*;

        let mut output = MutSet::default();

        for dep in dependencies.iter() {
            let has_package_dependency = self.add_package_dependency(dep, Phase::LoadHeader);

            let dep = *dep.as_inner();

            if !has_package_dependency {
                // loading can start immediately on this dependency
                output.insert((dep, Phase::LoadHeader));
            }

            // to parse and generate constraints, the headers of all dependencies must be loaded!
            // otherwise, we don't know whether an imported symbol is actually exposed
            self.add_dependency_help(module_id, dep, Phase::Parse, Phase::LoadHeader);

            // to canonicalize a module, all its dependencies must be canonicalized
            self.add_dependency(module_id, dep, Phase::CanonicalizeAndConstrain);

            // to typecheck a module, all its dependencies must be type checked already
            self.add_dependency(module_id, dep, Phase::SolveTypes);

            if goal_phase >= FindSpecializations {
                self.add_dependency(module_id, dep, Phase::FindSpecializations);
            }

            if goal_phase >= MakeSpecializations {
                self.add_dependency(dep, module_id, Phase::MakeSpecializations);
            }
        }

        // add dependencies for self
        // phase i + 1 of a file always depends on phase i being completed
        {
            let mut i = 0;
            while PHASES[i] < goal_phase {
                self.add_dependency_help(module_id, module_id, PHASES[i + 1], PHASES[i]);
                i += 1;
            }
        }

        self.add_to_status(module_id, goal_phase);

        output
    }

    pub fn add_effect_module(
        &mut self,
        module_id: ModuleId,
        dependencies: &MutSet<ModuleId>,
        goal_phase: Phase,
    ) -> MutSet<(ModuleId, Phase)> {
        // add dependencies for self
        // phase i + 1 of a file always depends on phase i being completed
        {
            let mut i = 2;

            // platform modules should only start at CanonicalizeAndConstrain
            debug_assert!(PHASES[i] == Phase::CanonicalizeAndConstrain);
            while PHASES[i] < goal_phase {
                self.add_dependency_help(module_id, module_id, PHASES[i + 1], PHASES[i]);
                i += 1;
            }
        }

        self.add_to_status(module_id, goal_phase);

        let mut output = MutSet::default();

        // all the dependencies can be loaded
        for dep in dependencies {
            output.insert((*dep, Phase::LoadHeader));
        }

        output
    }

    fn add_to_status(&mut self, module_id: ModuleId, goal_phase: Phase) {
        for phase in PHASES.iter() {
            if *phase > goal_phase {
                break;
            }

            if let Vacant(entry) = self.status.entry(Job::Step(module_id, *phase)) {
                entry.insert(Status::NotStarted);
            }
        }
    }

    /// Propagate a notification, return (module, phase) pairs that can make progress
    pub fn notify(&mut self, module_id: ModuleId, phase: Phase) -> MutSet<(ModuleId, Phase)> {
        self.notify_help(Job::Step(module_id, phase))
    }

    /// Propagate a notification, return (module, phase) pairs that can make progress
    pub fn notify_package(&mut self, shorthand: &'a str) -> MutSet<(ModuleId, Phase)> {
        self.notify_help(Job::ResolveShorthand(shorthand))
    }

    fn notify_help(&mut self, key: Job<'a>) -> MutSet<(ModuleId, Phase)> {
        self.status.insert(key.clone(), Status::Done);

        let mut output = MutSet::default();

        if let Some(to_notify) = self.notifies.get(&key) {
            for notify_key in to_notify {
                let mut is_empty = false;
                if let Some(waiting_for_pairs) = self.waiting_for.get_mut(notify_key) {
                    waiting_for_pairs.remove(&key);
                    is_empty = waiting_for_pairs.is_empty();
                }

                if is_empty {
                    self.waiting_for.remove(notify_key);

                    if let Job::Step(module, phase) = *notify_key {
                        output.insert((module, phase));
                    }
                }
            }
        }

        self.notifies.remove(&key);

        output
    }

    fn add_package_dependency(
        &mut self,
        module: &PackageQualified<'a, ModuleId>,
        next_phase: Phase,
    ) -> bool {
        match module {
            PackageQualified::Unqualified(_) => {
                // no dependency, we can just start loading the file
                false
            }
            PackageQualified::Qualified(shorthand, module_id) => {
                let job = Job::ResolveShorthand(shorthand);
                let next_step = Job::Step(*module_id, next_phase);
                match self.status.get(&job) {
                    None | Some(Status::NotStarted) | Some(Status::Pending) => {
                        // this shorthand is not resolved, add a dependency
                        {
                            let entry = self
                                .waiting_for
                                .entry(next_step.clone())
                                .or_insert_with(Default::default);

                            entry.insert(job.clone());
                        }

                        {
                            let entry = self.notifies.entry(job).or_insert_with(Default::default);

                            entry.insert(next_step);
                        }

                        true
                    }
                    Some(Status::Done) => {
                        // shorthand is resolved; no dependency
                        false
                    }
                }
            }
        }
    }

    /// A waits for B, and B will notify A when it completes the phase
    fn add_dependency(&mut self, a: ModuleId, b: ModuleId, phase: Phase) {
        self.add_dependency_help(a, b, phase, phase);
    }

    /// phase_a of module a is waiting for phase_b of module_b
    fn add_dependency_help(&mut self, a: ModuleId, b: ModuleId, phase_a: Phase, phase_b: Phase) {
        // no need to wait if the dependency is already done!
        if let Some(Status::Done) = self.status.get(&Job::Step(b, phase_b)) {
            return;
        }

        let key = Job::Step(a, phase_a);
        let value = Job::Step(b, phase_b);
        match self.waiting_for.get_mut(&key) {
            Some(existing) => {
                existing.insert(value);
            }
            None => {
                let mut set = MutSet::default();
                set.insert(value);
                self.waiting_for.insert(key, set);
            }
        }

        let key = Job::Step(b, phase_b);
        let value = Job::Step(a, phase_a);
        match self.notifies.get_mut(&key) {
            Some(existing) => {
                existing.insert(value);
            }
            None => {
                let mut set = MutSet::default();
                set.insert(value);
                self.notifies.insert(key, set);
            }
        }
    }

    fn solved_all(&self) -> bool {
        debug_assert_eq!(self.notifies.is_empty(), self.waiting_for.is_empty());

        for status in self.status.values() {
            match status {
                Status::Done => {
                    continue;
                }
                _ => {
                    return false;
                }
            }
        }

        true
    }
}

/// Struct storing various intermediate stages by their ModuleId
#[derive(Debug, Default)]
struct ModuleCache<'a> {
    module_names: MutMap<ModuleId, PQModuleName<'a>>,

    /// Phases
    headers: MutMap<ModuleId, ModuleHeader<'a>>,
    parsed: MutMap<ModuleId, ParsedModule<'a>>,
    aliases: MutMap<ModuleId, MutMap<Symbol, Alias>>,
    constrained: MutMap<ModuleId, ConstrainedModule>,
    typechecked: MutMap<ModuleId, TypeCheckedModule<'a>>,
    found_specializations: MutMap<ModuleId, FoundSpecializationsModule<'a>>,
    external_specializations_requested: MutMap<ModuleId, Vec<ExternalSpecializations>>,

    /// Various information
    imports: MutMap<ModuleId, MutSet<ModuleId>>,
    top_level_thunks: MutMap<ModuleId, MutSet<Symbol>>,
    documentation: MutMap<ModuleId, ModuleDocumentation>,
    can_problems: MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    type_problems: MutMap<ModuleId, Vec<solve::TypeError>>,
    mono_problems: MutMap<ModuleId, Vec<roc_mono::ir::MonoProblem>>,

    sources: MutMap<ModuleId, (PathBuf, &'a str)>,
    header_sources: MutMap<ModuleId, (PathBuf, &'a str)>,
}

fn start_phase<'a>(
    module_id: ModuleId,
    phase: Phase,
    arena: &'a Bump,
    state: &mut State<'a>,
) -> Vec<BuildTask<'a>> {
    // we blindly assume all dependencies are met

    match state
        .dependencies
        .status
        .get_mut(&Job::Step(module_id, phase))
    {
        Some(current @ Status::NotStarted) => {
            // start this phase!
            *current = Status::Pending;
        }
        Some(Status::Pending) => {
            // don't start this task again!
            return vec![];
        }
        Some(Status::Done) => {
            // don't start this task again, but tell those waiting for it they can continue
            return state
                .dependencies
                .notify(module_id, phase)
                .into_iter()
                .map(|(module_id, phase)| start_phase(module_id, phase, arena, state))
                .flatten()
                .collect();
        }
        None => match phase {
            Phase::LoadHeader => {
                // this is fine, mark header loading as pending
                state
                    .dependencies
                    .status
                    .insert(Job::Step(module_id, Phase::LoadHeader), Status::Pending);
            }
            _ => unreachable!(
                "Pair {:?} is not in dependencies.status, that should never happen!",
                (module_id, phase)
            ),
        },
    }

    let task = {
        match phase {
            Phase::LoadHeader => {
                let dep_name = state
                    .module_cache
                    .module_names
                    .get(&module_id)
                    .expect("module id is present")
                    .clone();

                BuildTask::LoadModule {
                    module_name: dep_name,
                    // Provide mutexes of ModuleIds and IdentIds by module,
                    // so other modules can populate them as they load.
                    module_ids: Arc::clone(&state.arc_modules),
                    shorthands: Arc::clone(&state.arc_shorthands),
                    ident_ids_by_module: Arc::clone(&state.ident_ids_by_module),
                }
            }
            Phase::Parse => {
                // parse the file
                let header = state.module_cache.headers.remove(&module_id).unwrap();

                BuildTask::Parse { header }
            }
            Phase::CanonicalizeAndConstrain => {
                // canonicalize the file
                let parsed = state.module_cache.parsed.remove(&module_id).unwrap();

                let deps_by_name = &parsed.deps_by_name;
                let num_deps = deps_by_name.len();
                let mut dep_idents: MutMap<ModuleId, IdentIds> =
                    IdentIds::exposed_builtins(num_deps);

                let State {
                    ident_ids_by_module,
                    ..
                } = &state;

                {
                    let ident_ids_by_module = (*ident_ids_by_module).lock();

                    // Populate dep_idents with each of their IdentIds,
                    // which we'll need during canonicalization to translate
                    // identifier strings into IdentIds, which we need to build Symbols.
                    // We only include the modules we care about (the ones we import).
                    //
                    // At the end of this loop, dep_idents contains all the information to
                    // resolve a symbol from another module: if it's in here, that means
                    // we have both imported the module and the ident was exported by that mdoule.
                    for dep_id in deps_by_name.values() {
                        // We already verified that these are all present,
                        // so unwrapping should always succeed here.
                        let idents = ident_ids_by_module.get(dep_id).unwrap();

                        dep_idents.insert(*dep_id, idents.clone());
                    }
                }

                // Clone the module_ids we'll need for canonicalization.
                // This should be small, and cloning it should be quick.
                // We release the lock as soon as we're done cloning, so we don't have
                // to lock the global module_ids while canonicalizing any given module.
                let qualified_module_ids = Arc::clone(&state.arc_modules);
                let qualified_module_ids = { (*qualified_module_ids).lock().clone() };

                let module_ids = qualified_module_ids.into_module_ids();

                let exposed_symbols = state
                    .exposed_symbols_by_module
                    .remove(&module_id)
                    .expect("Could not find listener ID in exposed_symbols_by_module");

                let mut aliases = MutMap::default();

                for imported in parsed.imported_modules.keys() {
                    match state.module_cache.aliases.get(imported) {
                        None => unreachable!(
                            r"imported module {:?} did not register its aliases, so {:?} cannot use them",
                            imported, parsed.module_id,
                        ),
                        Some(new) => {
                            // TODO filter to only add imported aliases
                            aliases.extend(new.iter().map(|(s, a)| (*s, a.clone())));
                        }
                    }
                }

                BuildTask::CanonicalizeAndConstrain {
                    parsed,
                    dep_idents,
                    exposed_symbols,
                    module_ids,
                    aliases,
                }
            }

            Phase::SolveTypes => {
                let constrained = state.module_cache.constrained.remove(&module_id).unwrap();

                let ConstrainedModule {
                    module,
                    ident_ids,
                    module_timing,
                    constraint,
                    var_store,
                    imported_modules,
                    declarations,
                    dep_idents,
                    ..
                } = constrained;

                BuildTask::solve_module(
                    module,
                    ident_ids,
                    module_timing,
                    constraint,
                    var_store,
                    imported_modules,
                    &mut state.exposed_types,
                    state.stdlib,
                    dep_idents,
                    declarations,
                )
            }
            Phase::FindSpecializations => {
                let typechecked = state.module_cache.typechecked.remove(&module_id).unwrap();

                let TypeCheckedModule {
                    layout_cache,
                    module_id,
                    module_timing,
                    solved_subs,
                    decls,
                    ident_ids,
                } = typechecked;

                let mut imported_module_thunks = bumpalo::collections::Vec::new_in(arena);

                if let Some(imports) = state.module_cache.imports.get(&module_id) {
                    for imported in imports.iter() {
                        imported_module_thunks.extend(
                            state.module_cache.top_level_thunks[imported]
                                .iter()
                                .copied(),
                        );
                    }
                }

                BuildTask::BuildPendingSpecializations {
                    layout_cache,
                    module_id,
                    module_timing,
                    solved_subs,
                    imported_module_thunks: imported_module_thunks.into_bump_slice(),
                    decls,
                    ident_ids,
                    exposed_to_host: state.exposed_to_host.clone(),
                }
            }
            Phase::MakeSpecializations => {
                let found_specializations = state
                    .module_cache
                    .found_specializations
                    .remove(&module_id)
                    .unwrap();

                let specializations_we_must_make = state
                    .module_cache
                    .external_specializations_requested
                    .remove(&module_id)
                    .unwrap_or_default();

                let FoundSpecializationsModule {
                    module_id,
                    ident_ids,
                    subs,
                    procs_base,
                    layout_cache,
                    module_timing,
                } = found_specializations;

                BuildTask::MakeSpecializations {
                    module_id,
                    ident_ids,
                    subs,
                    procs_base,
                    layout_cache,
                    specializations_we_must_make,
                    module_timing,
                }
            }
        }
    };

    vec![task]
}

#[derive(Debug)]
pub struct LoadedModule {
    pub module_id: ModuleId,
    pub interns: Interns,
    pub solved: Solved<Subs>,
    pub can_problems: MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    pub type_problems: MutMap<ModuleId, Vec<solve::TypeError>>,
    pub declarations_by_id: MutMap<ModuleId, Vec<Declaration>>,
    pub exposed_to_host: MutMap<Symbol, Variable>,
    pub dep_idents: MutMap<ModuleId, IdentIds>,
    pub exposed_aliases: MutMap<Symbol, Alias>,
    pub exposed_values: Vec<Symbol>,
    pub header_sources: MutMap<ModuleId, (PathBuf, Box<str>)>,
    pub sources: MutMap<ModuleId, (PathBuf, Box<str>)>,
    pub timings: MutMap<ModuleId, ModuleTiming>,
    pub documentation: MutMap<ModuleId, ModuleDocumentation>,
}

impl LoadedModule {
    pub fn total_problems(&self) -> usize {
        let mut total = 0;

        for problems in self.can_problems.values() {
            total += problems.len();
        }

        for problems in self.type_problems.values() {
            total += problems.len();
        }

        total
    }

    pub fn exposed_values_str(&self) -> Vec<&str> {
        self.exposed_values
            .iter()
            .map(|symbol| symbol.ident_str(&self.interns).as_str())
            .collect()
    }
}

#[derive(Debug)]
pub enum BuildProblem<'a> {
    FileNotFound(&'a Path),
}

#[derive(Debug)]
struct ModuleHeader<'a> {
    module_id: ModuleId,
    module_name: ModuleNameEnum<'a>,
    module_path: PathBuf,
    is_root_module: bool,
    exposed_ident_ids: IdentIds,
    deps_by_name: MutMap<PQModuleName<'a>, ModuleId>,
    packages: MutMap<&'a str, PackageName<'a>>,
    imported_modules: MutMap<ModuleId, Region>,
    package_qualified_imported_modules: MutSet<PackageQualified<'a, ModuleId>>,
    exposes: Vec<Symbol>,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    header_src: &'a str,
    parse_state: roc_parse::state::State<'a>,
    module_timing: ModuleTiming,
}

#[derive(Debug)]
enum HeaderFor<'a> {
    App {
        to_platform: To<'a>,
    },
    PkgConfig {
        /// usually `pf`
        config_shorthand: &'a str,
        /// the type scheme of the main function (required by the platform)
        /// (currently unused)
        #[allow(dead_code)]
        platform_main_type: TypedIdent<'a>,
        /// provided symbol to host (commonly `mainForHost`)
        main_for_host: Symbol,
    },
    Interface,
}

#[derive(Debug)]
struct ConstrainedModule {
    module: Module,
    declarations: Vec<Declaration>,
    imported_modules: MutMap<ModuleId, Region>,
    constraint: Constraint,
    ident_ids: IdentIds,
    var_store: VarStore,
    dep_idents: MutMap<ModuleId, IdentIds>,
    module_timing: ModuleTiming,
}

#[derive(Debug)]
pub struct TypeCheckedModule<'a> {
    pub module_id: ModuleId,
    pub layout_cache: LayoutCache<'a>,
    pub module_timing: ModuleTiming,
    pub solved_subs: Solved<Subs>,
    pub decls: Vec<Declaration>,
    pub ident_ids: IdentIds,
}

#[derive(Debug)]
struct FoundSpecializationsModule<'a> {
    module_id: ModuleId,
    ident_ids: IdentIds,
    layout_cache: LayoutCache<'a>,
    procs_base: ProcsBase<'a>,
    subs: Subs,
    module_timing: ModuleTiming,
}

#[derive(Debug)]
pub struct MonomorphizedModule<'a> {
    pub module_id: ModuleId,
    pub interns: Interns,
    pub subs: Subs,
    pub output_path: Box<str>,
    pub platform_path: Box<str>,
    pub can_problems: MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    pub type_problems: MutMap<ModuleId, Vec<solve::TypeError>>,
    pub mono_problems: MutMap<ModuleId, Vec<roc_mono::ir::MonoProblem>>,
    pub procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    pub entry_point: EntryPoint<'a>,
    pub exposed_to_host: MutMap<Symbol, Variable>,
    pub header_sources: MutMap<ModuleId, (PathBuf, Box<str>)>,
    pub sources: MutMap<ModuleId, (PathBuf, Box<str>)>,
    pub timings: MutMap<ModuleId, ModuleTiming>,
}

impl<'a> MonomorphizedModule<'a> {
    pub fn total_problems(&self) -> usize {
        let mut total = 0;

        for problems in self.can_problems.values() {
            total += problems.len();
        }

        for problems in self.type_problems.values() {
            total += problems.len();
        }

        for problems in self.mono_problems.values() {
            total += problems.len();
        }

        total
    }
}

#[derive(Debug)]
struct ParsedModule<'a> {
    module_id: ModuleId,
    module_name: ModuleNameEnum<'a>,
    module_path: PathBuf,
    src: &'a str,
    module_timing: ModuleTiming,
    deps_by_name: MutMap<PQModuleName<'a>, ModuleId>,
    imported_modules: MutMap<ModuleId, Region>,
    exposed_ident_ids: IdentIds,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    parsed_defs: &'a [Loc<roc_parse::ast::Def<'a>>],
}

/// A message sent out _from_ a worker thread,
/// representing a result of work done, or a request for further work
#[derive(Debug)]
enum Msg<'a> {
    Many(Vec<Msg<'a>>),
    Header(ModuleHeader<'a>, HeaderFor<'a>),
    Parsed(ParsedModule<'a>),
    CanonicalizedAndConstrained {
        constrained_module: ConstrainedModule,
        canonicalization_problems: Vec<roc_problem::can::Problem>,
        module_docs: Option<ModuleDocumentation>,
    },
    MadeEffectModule {
        type_shortname: &'a str,
        constrained_module: ConstrainedModule,
        canonicalization_problems: Vec<roc_problem::can::Problem>,
        module_docs: ModuleDocumentation,
    },
    SolvedTypes {
        module_id: ModuleId,
        ident_ids: IdentIds,
        solved_module: SolvedModule,
        solved_subs: Solved<Subs>,
        decls: Vec<Declaration>,
        dep_idents: MutMap<ModuleId, IdentIds>,
        module_timing: ModuleTiming,
        unused_imports: MutMap<ModuleId, Region>,
    },
    FinishedAllTypeChecking {
        solved_subs: Solved<Subs>,
        exposed_vars_by_symbol: MutMap<Symbol, Variable>,
        exposed_aliases_by_symbol: MutMap<Symbol, Alias>,
        exposed_values: Vec<Symbol>,
        dep_idents: MutMap<ModuleId, IdentIds>,
        documentation: MutMap<ModuleId, ModuleDocumentation>,
    },
    FoundSpecializations {
        module_id: ModuleId,
        ident_ids: IdentIds,
        layout_cache: LayoutCache<'a>,
        procs_base: ProcsBase<'a>,
        problems: Vec<roc_mono::ir::MonoProblem>,
        solved_subs: Solved<Subs>,
        module_timing: ModuleTiming,
    },
    MadeSpecializations {
        module_id: ModuleId,
        ident_ids: IdentIds,
        layout_cache: LayoutCache<'a>,
        external_specializations_requested: BumpMap<ModuleId, ExternalSpecializations>,
        procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
        problems: Vec<roc_mono::ir::MonoProblem>,
        update_mode_ids: UpdateModeIds,
        module_timing: ModuleTiming,
        subs: Subs,
    },

    /// The task is to only typecheck AND monomorphize modules
    /// all modules are now monomorphized, we are done
    FinishedAllSpecialization {
        subs: Subs,
        exposed_to_host: MutMap<Symbol, Variable>,
    },

    FailedToParse(ParseProblem<'a, SyntaxError<'a>>),
    FailedToReadFile {
        filename: PathBuf,
        error: io::ErrorKind,
    },
}

#[derive(Debug)]
enum PlatformPath<'a> {
    NotSpecified,
    Valid(To<'a>),
    RootIsInterface,
    RootIsPkgConfig,
}

#[derive(Debug)]
struct PlatformData {
    module_id: ModuleId,
    provides: Symbol,
}

#[derive(Debug)]
struct State<'a> {
    pub root_id: ModuleId,
    pub platform_data: Option<PlatformData>,
    pub goal_phase: Phase,
    pub stdlib: &'a StdLib,
    pub exposed_types: SubsByModule,
    pub output_path: Option<&'a str>,
    pub platform_path: PlatformPath<'a>,
    pub ptr_bytes: u32,

    pub module_cache: ModuleCache<'a>,
    pub dependencies: Dependencies<'a>,
    pub procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    pub exposed_to_host: MutMap<Symbol, Variable>,

    /// This is the "final" list of IdentIds, after canonicalization and constraint gen
    /// have completed for a given module.
    pub constrained_ident_ids: MutMap<ModuleId, IdentIds>,

    /// From now on, these will be used by multiple threads; time to make an Arc<Mutex<_>>!
    pub arc_modules: Arc<Mutex<PackageModuleIds<'a>>>,
    pub arc_shorthands: Arc<Mutex<MutMap<&'a str, PackageName<'a>>>>,

    pub ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,

    pub declarations_by_id: MutMap<ModuleId, Vec<Declaration>>,

    pub exposed_symbols_by_module: MutMap<ModuleId, MutSet<Symbol>>,

    pub timings: MutMap<ModuleId, ModuleTiming>,

    // Each thread gets its own layout cache. When one "pending specializations"
    // pass completes, it returns its layout cache so another thread can use it.
    // We don't bother trying to union them all together to maximize cache hits,
    // since the unioning process could potentially take longer than the savings.
    // (Granted, this has not been attempted or measured!)
    pub layout_caches: std::vec::Vec<LayoutCache<'a>>,
}

#[derive(Debug)]
pub struct ModuleTiming {
    pub read_roc_file: Duration,
    pub parse_header: Duration,
    pub parse_body: Duration,
    pub canonicalize: Duration,
    pub constrain: Duration,
    pub solve: Duration,
    pub find_specializations: Duration,
    pub make_specializations: Duration,
    // TODO pub monomorphize: Duration,
    /// Total duration will always be more than the sum of the other fields, due
    /// to things like state lookups in between phases, waiting on other threads, etc.
    start_time: SystemTime,
    end_time: SystemTime,
}

impl ModuleTiming {
    pub fn new(start_time: SystemTime) -> Self {
        ModuleTiming {
            read_roc_file: Duration::default(),
            parse_header: Duration::default(),
            parse_body: Duration::default(),
            canonicalize: Duration::default(),
            constrain: Duration::default(),
            solve: Duration::default(),
            find_specializations: Duration::default(),
            make_specializations: Duration::default(),
            start_time,
            end_time: start_time, // just for now; we'll overwrite this at the end
        }
    }

    pub fn total(&self) -> Duration {
        self.end_time.duration_since(self.start_time).unwrap()
    }

    /// Subtract all the other fields from total_start_to_finish
    pub fn other(&self) -> Duration {
        let Self {
            read_roc_file,
            parse_header,
            parse_body,
            canonicalize,
            constrain,
            solve,
            find_specializations,
            make_specializations,
            start_time,
            end_time,
        } = self;

        let calculate = |t: Result<Duration, std::time::SystemTimeError>| -> Option<Duration> {
            t.ok()?
                .checked_sub(*make_specializations)?
                .checked_sub(*find_specializations)?
                .checked_sub(*solve)?
                .checked_sub(*constrain)?
                .checked_sub(*canonicalize)?
                .checked_sub(*parse_body)?
                .checked_sub(*parse_header)?
                .checked_sub(*read_roc_file)
        };

        calculate(end_time.duration_since(*start_time)).unwrap_or_default()
    }
}

/// A message sent _to_ a worker thread, describing the work to be done
#[derive(Debug)]
#[allow(dead_code)]
enum BuildTask<'a> {
    LoadModule {
        module_name: PQModuleName<'a>,
        module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
        shorthands: Arc<Mutex<MutMap<&'a str, PackageName<'a>>>>,
        ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    },
    Parse {
        header: ModuleHeader<'a>,
    },
    CanonicalizeAndConstrain {
        parsed: ParsedModule<'a>,
        module_ids: ModuleIds,
        dep_idents: MutMap<ModuleId, IdentIds>,
        exposed_symbols: MutSet<Symbol>,
        aliases: MutMap<Symbol, Alias>,
    },
    Solve {
        module: Module,
        ident_ids: IdentIds,
        imported_symbols: Vec<Import>,
        module_timing: ModuleTiming,
        constraint: Constraint,
        var_store: VarStore,
        declarations: Vec<Declaration>,
        dep_idents: MutMap<ModuleId, IdentIds>,
        unused_imports: MutMap<ModuleId, Region>,
    },
    BuildPendingSpecializations {
        module_timing: ModuleTiming,
        layout_cache: LayoutCache<'a>,
        solved_subs: Solved<Subs>,
        imported_module_thunks: &'a [Symbol],
        module_id: ModuleId,
        ident_ids: IdentIds,
        decls: Vec<Declaration>,
        exposed_to_host: MutMap<Symbol, Variable>,
    },
    MakeSpecializations {
        module_id: ModuleId,
        ident_ids: IdentIds,
        subs: Subs,
        procs_base: ProcsBase<'a>,
        layout_cache: LayoutCache<'a>,
        specializations_we_must_make: Vec<ExternalSpecializations>,
        module_timing: ModuleTiming,
    },
}

enum WorkerMsg {
    Shutdown,
    TaskAdded,
}

#[derive(Debug)]
pub enum LoadingProblem<'a> {
    FileProblem {
        filename: PathBuf,
        error: io::ErrorKind,
    },
    ParsingFailed(ParseProblem<'a, SyntaxError<'a>>),
    UnexpectedHeader(String),

    MsgChannelDied,
    ErrJoiningWorkerThreads,
    TriedToImportAppModule,

    /// a formatted report
    FormattedReport(String),
}

pub enum Phases {
    /// Parse, canonicalize, check types
    TypeCheck,
    /// Parse, canonicalize, check types, monomorphize
    Monomorphize,
}

type MsgSender<'a> = Sender<Msg<'a>>;

/// Add a task to the queue, and notify all the listeners.
fn enqueue_task<'a>(
    injector: &Injector<BuildTask<'a>>,
    listeners: &[Sender<WorkerMsg>],
    task: BuildTask<'a>,
) -> Result<(), LoadingProblem<'a>> {
    injector.push(task);

    for listener in listeners {
        listener
            .send(WorkerMsg::TaskAdded)
            .map_err(|_| LoadingProblem::MsgChannelDied)?;
    }

    Ok(())
}

pub fn load_and_typecheck<'a, F>(
    arena: &'a Bump,
    filename: PathBuf,
    stdlib: &'a StdLib,
    src_dir: &Path,
    exposed_types: SubsByModule,
    ptr_bytes: u32,
    look_up_builtin: F,
) -> Result<LoadedModule, LoadingProblem<'a>>
where
    F: Fn(Symbol, &mut VarStore) -> Option<Def> + 'static + Send + Copy,
{
    use LoadResult::*;

    let load_start = LoadStart::from_path(arena, filename)?;

    match load(
        arena,
        load_start,
        stdlib,
        src_dir,
        exposed_types,
        Phase::SolveTypes,
        ptr_bytes,
        look_up_builtin,
    )? {
        Monomorphized(_) => unreachable!(""),
        TypeChecked(module) => Ok(module),
    }
}

/// Main entry point to the compiler from the CLI and tests
pub fn load_and_monomorphize<'a, F>(
    arena: &'a Bump,
    filename: PathBuf,
    stdlib: &'a StdLib,
    src_dir: &Path,
    exposed_types: SubsByModule,
    ptr_bytes: u32,
    look_up_builtin: F,
) -> Result<MonomorphizedModule<'a>, LoadingProblem<'a>>
where
    F: Fn(Symbol, &mut VarStore) -> Option<Def> + 'static + Send + Copy,
{
    use LoadResult::*;

    let load_start = LoadStart::from_path(arena, filename)?;

    match load(
        arena,
        load_start,
        stdlib,
        src_dir,
        exposed_types,
        Phase::MakeSpecializations,
        ptr_bytes,
        look_up_builtin,
    )? {
        Monomorphized(module) => Ok(module),
        TypeChecked(_) => unreachable!(""),
    }
}

#[allow(clippy::too_many_arguments)]
pub fn load_and_monomorphize_from_str<'a, F>(
    arena: &'a Bump,
    filename: PathBuf,
    src: &'a str,
    stdlib: &'a StdLib,
    src_dir: &Path,
    exposed_types: SubsByModule,
    ptr_bytes: u32,
    look_up_builtin: F,
) -> Result<MonomorphizedModule<'a>, LoadingProblem<'a>>
where
    F: Fn(Symbol, &mut VarStore) -> Option<Def> + 'static + Send + Copy,
{
    use LoadResult::*;

    let load_start = LoadStart::from_str(arena, filename, src)?;

    match load(
        arena,
        load_start,
        stdlib,
        src_dir,
        exposed_types,
        Phase::MakeSpecializations,
        ptr_bytes,
        look_up_builtin,
    )? {
        Monomorphized(module) => Ok(module),
        TypeChecked(_) => unreachable!(""),
    }
}

struct LoadStart<'a> {
    pub arc_modules: Arc<Mutex<PackageModuleIds<'a>>>,
    pub ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    pub root_id: ModuleId,
    pub root_msg: Msg<'a>,
}

impl<'a> LoadStart<'a> {
    pub fn from_path(arena: &'a Bump, filename: PathBuf) -> Result<Self, LoadingProblem<'a>> {
        let arc_modules = Arc::new(Mutex::new(PackageModuleIds::default()));
        let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
        let ident_ids_by_module = Arc::new(Mutex::new(root_exposed_ident_ids));

        // Load the root module synchronously; we can't proceed until we have its id.
        let (root_id, root_msg) = {
            let root_start_time = SystemTime::now();

            let res_loaded = load_filename(
                arena,
                filename,
                true,
                None,
                Arc::clone(&arc_modules),
                Arc::clone(&ident_ids_by_module),
                root_start_time,
            );

            match res_loaded {
                Ok(good) => good,

                Err(LoadingProblem::ParsingFailed(problem)) => {
                    let module_ids = Arc::try_unwrap(arc_modules)
                        .unwrap_or_else(|_| {
                            panic!("There were still outstanding Arc references to module_ids")
                        })
                        .into_inner()
                        .into_module_ids();

                    // if parsing failed, this module did not add any identifiers
                    let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
                    let buf = to_parse_problem_report(problem, module_ids, root_exposed_ident_ids);
                    return Err(LoadingProblem::FormattedReport(buf));
                }
                Err(e) => return Err(e),
            }
        };

        Ok(LoadStart {
            arc_modules,
            ident_ids_by_module,
            root_id,
            root_msg,
        })
    }

    pub fn from_str(
        arena: &'a Bump,
        filename: PathBuf,
        src: &'a str,
    ) -> Result<Self, LoadingProblem<'a>> {
        let arc_modules = Arc::new(Mutex::new(PackageModuleIds::default()));
        let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
        let ident_ids_by_module = Arc::new(Mutex::new(root_exposed_ident_ids));

        // Load the root module synchronously; we can't proceed until we have its id.
        let (root_id, root_msg) = {
            let root_start_time = SystemTime::now();

            load_from_str(
                arena,
                filename,
                src,
                Arc::clone(&arc_modules),
                Arc::clone(&ident_ids_by_module),
                root_start_time,
            )?
        };

        Ok(LoadStart {
            arc_modules,
            ident_ids_by_module,
            root_id,
            root_msg,
        })
    }
}

enum LoadResult<'a> {
    TypeChecked(LoadedModule),
    Monomorphized(MonomorphizedModule<'a>),
}

/// The loading process works like this, starting from the given filename (e.g. "main.roc"):
///
/// 1. Open the file.
/// 2. Parse the module's header.
/// 3. For each of its imports, send a message on the channel to the coordinator thread, which
///    will repeat this process to load that module - starting with step 1.
/// 4. Add everything we were able to import unqualified to the module's default scope.
/// 5. Parse the module's defs.
/// 6. Canonicalize the module.
/// 7. Before type checking, block on waiting for type checking to complete on all imports.
///    (Since Roc doesn't allow cyclic dependencies, this cannot deadlock.)
/// 8. Type check the module and create type annotations for its top-level declarations.
/// 9. Report the completed type annotation to the coordinator thread, so other modules
///    that are waiting in step 7 can unblock.
///
/// The loaded_modules argument specifies which modules have already been loaded.
/// It typically contains *at least* the standard modules, but is empty when loading
/// the standard modules themselves.
///
/// If we're just type-checking everything (e.g. running `roc check` at the command line),
/// we can stop there. However, if we're generating code, then there are additional steps.
///
/// 10. After reporting the completed type annotation, we have all the information necessary
///     to monomorphize. However, since we want to monomorphize in parallel without
///     duplicating work, we do monomorphization in two steps. First, we go through and
///     determine all the specializations this module *wants*. We compute the hashes
///     and report them to the coordinator thread, along with the mono::expr::Expr values of
///     the current function's body. At this point, we have not yet begun to assemble Procs;
///     all we've done is send a list of requetsted specializations to the coordinator.
/// 11. The coordinator works through the specialization requests in parallel, adding them
///     to a global map once they're finished. Performing one specialization may result
///     in requests for others; these are added to the queue and worked through as normal.
///     This process continues until *both* all modules have reported that they've finished
///     adding specialization requests to the queue, *and* the queue is empty (including
///     of any requests that were added in the course of completing other requests). Now
///     we have a map of specializations, and everything was assembled in parallel with
///     no unique specialization ever getting assembled twice (meaning no wasted effort).
/// 12. Now that we have our final map of specializations, we can proceed to code gen!
///     As long as the specializations are stored in a per-ModuleId map, we can also
///     parallelize this code gen. (e.g. in dev builds, building separate LLVM modules
///     and then linking them together, and possibly caching them by the hash of their
///     specializations, so if none of their specializations changed, we don't even need
///     to rebuild the module and can link in the cached one directly.)
#[allow(clippy::too_many_arguments)]
fn load<'a, F>(
    arena: &'a Bump,
    //filename: PathBuf,
    load_start: LoadStart<'a>,
    stdlib: &'a StdLib,
    src_dir: &Path,
    exposed_types: SubsByModule,
    goal_phase: Phase,
    ptr_bytes: u32,
    look_up_builtins: F,
) -> Result<LoadResult<'a>, LoadingProblem<'a>>
where
    F: Fn(Symbol, &mut VarStore) -> Option<Def> + 'static + Send + Copy,
{
    let LoadStart {
        arc_modules,
        ident_ids_by_module,
        root_id,
        root_msg,
    } = load_start;

    let arc_shorthands = Arc::new(Mutex::new(MutMap::default()));

    let (msg_tx, msg_rx) = bounded(1024);
    msg_tx
        .send(root_msg)
        .map_err(|_| LoadingProblem::MsgChannelDied)?;

    // Reserve one CPU for the main thread, and let all the others be eligible
    // to spawn workers. We use .max(2) to enforce that we always
    // end up with at least 1 worker - since (.max(2) - 1) will
    // always return a number that's at least 1. Using
    // .max(2) on the initial number of CPUs instead of
    // doing .max(1) on the entire expression guards against
    // num_cpus returning 0, while also avoiding wrapping
    // unsigned subtraction overflow.
    let default_num_workers = num_cpus::get().max(2) - 1;

    let num_workers = match env::var("ROC_NUM_WORKERS") {
        Ok(env_str) => env_str.parse::<usize>().unwrap_or(default_num_workers),
        Err(_) => default_num_workers,
    };

    let worker_arenas = arena.alloc(bumpalo::collections::Vec::with_capacity_in(
        num_workers,
        arena,
    ));

    for _ in 0..num_workers {
        worker_arenas.push(Bump::new());
    }

    // We'll add tasks to this, and then worker threads will take tasks from it.
    let injector = Injector::new();

    // We need to allocate worker *queues* on the main thread and then move them
    // into the worker threads, because those workers' stealers need to be
    // shared bet,een all threads, and this coordination work is much easier
    // on the main thread.
    let mut worker_queues = bumpalo::collections::Vec::with_capacity_in(num_workers, arena);
    let mut stealers = bumpalo::collections::Vec::with_capacity_in(num_workers, arena);

    let it = worker_arenas.iter_mut();

    {
        thread::scope(|thread_scope| {
            for _ in 0..num_workers {
                let worker = Worker::new_lifo();

                stealers.push(worker.stealer());
                worker_queues.push(worker);
            }

            // Get a reference to the completed stealers, so we can send that
            // reference to each worker. (Slices are Sync, but bumpalo Vecs are not.)
            let stealers = stealers.into_bump_slice();

            let mut worker_listeners =
                bumpalo::collections::Vec::with_capacity_in(num_workers, arena);

            for worker_arena in it {
                let msg_tx = msg_tx.clone();
                let worker = worker_queues.pop().unwrap();
                let (worker_msg_tx, worker_msg_rx) = bounded(1024);

                worker_listeners.push(worker_msg_tx);

                // We only want to move a *reference* to the main task queue's
                // injector in the thread, not the injector itself
                // (since other threads need to reference it too).
                let injector = &injector;

                // Record this thread's handle so the main thread can join it later.
                let res_join_handle = thread_scope
                    .builder()
                    .stack_size(EXPANDED_STACK_SIZE)
                    .spawn(move |_| {
                        // Keep listening until we receive a Shutdown msg

                        for msg in worker_msg_rx.iter() {
                            match msg {
                                WorkerMsg::Shutdown => {
                                    // We've finished all our work. It's time to
                                    // shut down the thread, so when the main thread
                                    // blocks on joining with all the worker threads,
                                    // it can finally exit too!
                                    return Ok(());
                                }
                                WorkerMsg::TaskAdded => {
                                    // Find a task - either from this thread's queue,
                                    // or from the main queue, or from another worker's
                                    // queue - and run it.
                                    //
                                    // There might be no tasks to work on! That could
                                    // happen if another thread is working on a task
                                    // which will later result in more tasks being
                                    // added. In that case, do nothing, and keep waiting
                                    // until we receive a Shutdown message.
                                    if let Some(task) = find_task(&worker, injector, stealers) {
                                        let result = run_task(
                                            task,
                                            worker_arena,
                                            src_dir,
                                            msg_tx.clone(),
                                            ptr_bytes,
                                            look_up_builtins,
                                        );

                                        match result {
                                            Ok(()) => {}
                                            Err(LoadingProblem::MsgChannelDied) => {
                                                panic!("Msg channel closed unexpectedly.")
                                            }
                                            Err(LoadingProblem::ParsingFailed(problem)) => {
                                                msg_tx.send(Msg::FailedToParse(problem)).unwrap();
                                            }
                                            Err(LoadingProblem::FileProblem {
                                                filename,
                                                error,
                                            }) => {
                                                msg_tx
                                                    .send(Msg::FailedToReadFile { filename, error })
                                                    .unwrap();
                                            }
                                            Err(other) => {
                                                return Err(other);
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // Needed to prevent a borrow checker error about this closure
                        // outliving its enclosing function.
                        drop(worker_msg_rx);

                        Ok(())
                    });

                res_join_handle.unwrap();
            }

            let mut state = State {
                root_id,
                ptr_bytes,
                platform_data: None,
                goal_phase,
                stdlib,
                output_path: None,
                platform_path: PlatformPath::NotSpecified,
                module_cache: ModuleCache::default(),
                dependencies: Dependencies::default(),
                procedures: MutMap::default(),
                exposed_to_host: MutMap::default(),
                exposed_types,
                arc_modules,
                arc_shorthands,
                constrained_ident_ids: IdentIds::exposed_builtins(0),
                ident_ids_by_module,
                declarations_by_id: MutMap::default(),
                exposed_symbols_by_module: MutMap::default(),
                timings: MutMap::default(),
                layout_caches: std::vec::Vec::with_capacity(num_cpus::get()),
            };

            // We've now distributed one worker queue to each thread.
            // There should be no queues left to distribute!
            debug_assert!(worker_queues.is_empty());
            drop(worker_queues);

            // Grab a reference to these Senders outside the loop, so we can share
            // it across each iteration of the loop.
            let worker_listeners = worker_listeners.into_bump_slice();
            let msg_tx = msg_tx.clone();

            macro_rules! shut_down_worker_threads {
                () => {
                    for listener in worker_listeners {
                        listener
                            .send(WorkerMsg::Shutdown)
                            .map_err(|_| LoadingProblem::MsgChannelDied)?;
                    }
                };
            }

            // The root module will have already queued up messages to process,
            // and processing those messages will in turn queue up more messages.
            for msg in msg_rx.iter() {
                match msg {
                    Msg::FinishedAllTypeChecking {
                        solved_subs,
                        exposed_vars_by_symbol,
                        exposed_aliases_by_symbol,
                        exposed_values,
                        dep_idents,
                        documentation,
                    } => {
                        // We're done! There should be no more messages pending.
                        debug_assert!(msg_rx.is_empty());

                        // Shut down all the worker threads.
                        for listener in worker_listeners {
                            listener
                                .send(WorkerMsg::Shutdown)
                                .map_err(|_| LoadingProblem::MsgChannelDied)?;
                        }

                        return Ok(LoadResult::TypeChecked(finish(
                            state,
                            solved_subs,
                            exposed_values,
                            exposed_aliases_by_symbol,
                            exposed_vars_by_symbol,
                            dep_idents,
                            documentation,
                        )));
                    }
                    Msg::FinishedAllSpecialization {
                        subs,
                        exposed_to_host,
                    } => {
                        // We're done! There should be no more messages pending.
                        debug_assert!(msg_rx.is_empty());

                        shut_down_worker_threads!();

                        return Ok(LoadResult::Monomorphized(finish_specialization(
                            state,
                            subs,
                            exposed_to_host,
                        )?));
                    }
                    Msg::FailedToReadFile { filename, error } => {
                        shut_down_worker_threads!();

                        let buf = to_file_problem_report(&filename, error);
                        return Err(LoadingProblem::FormattedReport(buf));
                    }

                    Msg::FailedToParse(problem) => {
                        shut_down_worker_threads!();

                        let module_ids = Arc::try_unwrap(state.arc_modules)
                            .unwrap_or_else(|_| {
                                panic!("There were still outstanding Arc references to module_ids")
                            })
                            .into_inner()
                            .into_module_ids();

                        let buf = to_parse_problem_report(
                            problem,
                            module_ids,
                            state.constrained_ident_ids,
                        );
                        return Err(LoadingProblem::FormattedReport(buf));
                    }
                    msg => {
                        // This is where most of the main thread's work gets done.
                        // Everything up to this point has been setting up the threading
                        // system which lets this logic work efficiently.
                        let constrained_ident_ids = state.constrained_ident_ids.clone();
                        let arc_modules = state.arc_modules.clone();

                        let res_state = update(
                            state,
                            msg,
                            msg_tx.clone(),
                            &injector,
                            worker_listeners,
                            arena,
                        );

                        match res_state {
                            Ok(new_state) => {
                                state = new_state;
                            }
                            Err(LoadingProblem::ParsingFailed(problem)) => {
                                shut_down_worker_threads!();

                                let module_ids = Arc::try_unwrap(arc_modules)
                            .unwrap_or_else(|_| {
                                panic!(r"There were still outstanding Arc references to module_ids")
                            })
                            .into_inner()
                            .into_module_ids();

                                let buf = to_parse_problem_report(
                                    problem,
                                    module_ids,
                                    constrained_ident_ids,
                                );
                                return Err(LoadingProblem::FormattedReport(buf));
                            }
                            Err(e) => return Err(e),
                        }
                    }
                }
            }

            // The msg_rx receiver closed unexpectedly before we finished solving everything
            Err(LoadingProblem::MsgChannelDied)
        })
    }
    .unwrap()
}

fn start_tasks<'a>(
    arena: &'a Bump,
    state: &mut State<'a>,
    work: MutSet<(ModuleId, Phase)>,
    injector: &Injector<BuildTask<'a>>,
    worker_listeners: &'a [Sender<WorkerMsg>],
) -> Result<(), LoadingProblem<'a>> {
    for (module_id, phase) in work {
        for task in start_phase(module_id, phase, arena, state) {
            enqueue_task(injector, worker_listeners, task)?
        }
    }

    Ok(())
}

fn update<'a>(
    mut state: State<'a>,
    msg: Msg<'a>,
    msg_tx: MsgSender<'a>,
    injector: &Injector<BuildTask<'a>>,
    worker_listeners: &'a [Sender<WorkerMsg>],
    arena: &'a Bump,
) -> Result<State<'a>, LoadingProblem<'a>> {
    use self::Msg::*;

    match msg {
        Many(messages) => {
            // enqueue all these message
            for msg in messages {
                msg_tx
                    .send(msg)
                    .map_err(|_| LoadingProblem::MsgChannelDied)?;
            }

            Ok(state)
        }
        Header(header, header_extra) => {
            use HeaderFor::*;

            log!("loaded header for {:?}", header.module_id);
            let home = header.module_id;

            let mut work = MutSet::default();

            {
                let mut shorthands = (*state.arc_shorthands).lock();

                for (shorthand, package_name) in header.packages.iter() {
                    shorthands.insert(shorthand, *package_name);
                }

                if let PkgConfig {
                    config_shorthand, ..
                } = header_extra
                {
                    work.extend(state.dependencies.notify_package(config_shorthand));
                }
            }

            match header_extra {
                App { to_platform } => {
                    debug_assert!(matches!(state.platform_path, PlatformPath::NotSpecified));
                    state.platform_path = PlatformPath::Valid(to_platform);
                }
                PkgConfig { main_for_host, .. } => {
                    debug_assert!(matches!(state.platform_data, None));

                    state.platform_data = Some(PlatformData {
                        module_id: header.module_id,
                        provides: main_for_host,
                    });

                    if header.is_root_module {
                        debug_assert!(matches!(state.platform_path, PlatformPath::NotSpecified));
                        state.platform_path = PlatformPath::RootIsPkgConfig;
                    }
                }
                Interface => {
                    if header.is_root_module {
                        debug_assert!(matches!(state.platform_path, PlatformPath::NotSpecified));
                        state.platform_path = PlatformPath::RootIsInterface;
                    }
                }
            }

            // store an ID to name mapping, so we know the file to read when fetching dependencies' headers
            for (name, id) in header.deps_by_name.iter() {
                state.module_cache.module_names.insert(*id, name.clone());
            }

            // This was a dependency. Write it down and keep processing messaages.
            let mut exposed_symbols: MutSet<Symbol> =
                HashSet::with_capacity_and_hasher(header.exposes.len(), default_hasher());

            // TODO can we avoid this loop by storing them as a Set in Header to begin with?
            for symbol in header.exposes.iter() {
                exposed_symbols.insert(*symbol);
            }

            // NOTE we currently re-parse the headers when a module is imported twice.
            // We need a proper solution that marks a phase as in-progress so it's not repeated
            // debug_assert!(!state.exposed_symbols_by_module.contains_key(&home));

            state
                .exposed_symbols_by_module
                .insert(home, exposed_symbols);

            state
                .module_cache
                .header_sources
                .insert(home, (header.module_path.clone(), header.header_src));

            state
                .module_cache
                .imports
                .entry(header.module_id)
                .or_default()
                .extend(
                    header
                        .package_qualified_imported_modules
                        .iter()
                        .map(|x| *x.as_inner()),
                );

            work.extend(state.dependencies.add_module(
                header.module_id,
                &header.package_qualified_imported_modules,
                state.goal_phase,
            ));

            state.module_cache.headers.insert(header.module_id, header);

            start_tasks(arena, &mut state, work, injector, worker_listeners)?;

            let work = state.dependencies.notify(home, Phase::LoadHeader);

            start_tasks(arena, &mut state, work, injector, worker_listeners)?;

            Ok(state)
        }
        Parsed(parsed) => {
            state
                .module_cache
                .sources
                .insert(parsed.module_id, (parsed.module_path.clone(), parsed.src));

            // If this was an app module, set the output path to be
            // the module's declared "name".
            //
            // e.g. for `app "blah"` we should generate an output file named "blah"
            match &parsed.module_name {
                ModuleNameEnum::PkgConfig => {}
                ModuleNameEnum::App(output_str) => match output_str {
                    StrLiteral::PlainLine(path) => {
                        state.output_path = Some(path);
                    }
                    _ => {
                        todo!("TODO gracefully handle a malformed string literal after `app` keyword.");
                    }
                },
                ModuleNameEnum::Interface(_) => {}
            }

            let module_id = parsed.module_id;

            state.module_cache.parsed.insert(parsed.module_id, parsed);

            let work = state.dependencies.notify(module_id, Phase::Parse);

            start_tasks(arena, &mut state, work, injector, worker_listeners)?;

            Ok(state)
        }

        CanonicalizedAndConstrained {
            constrained_module,
            canonicalization_problems,
            module_docs,
        } => {
            let module_id = constrained_module.module.module_id;
            log!("generated constraints for {:?}", module_id);
            state
                .module_cache
                .can_problems
                .insert(module_id, canonicalization_problems);

            if let Some(docs) = module_docs {
                state.module_cache.documentation.insert(module_id, docs);
            }

            state
                .module_cache
                .aliases
                .insert(module_id, constrained_module.module.aliases.clone());

            state
                .module_cache
                .constrained
                .insert(module_id, constrained_module);

            let work = state
                .dependencies
                .notify(module_id, Phase::CanonicalizeAndConstrain);

            start_tasks(arena, &mut state, work, injector, worker_listeners)?;

            Ok(state)
        }
        MadeEffectModule {
            constrained_module,
            canonicalization_problems,
            module_docs,
            type_shortname,
        } => {
            let module_id = constrained_module.module.module_id;

            log!("made effect module for {:?}", module_id);
            state
                .module_cache
                .can_problems
                .insert(module_id, canonicalization_problems);

            state
                .module_cache
                .documentation
                .insert(module_id, module_docs);

            state
                .module_cache
                .aliases
                .insert(module_id, constrained_module.module.aliases.clone());

            state
                .module_cache
                .constrained
                .insert(module_id, constrained_module);

            let mut work = state.dependencies.add_effect_module(
                module_id,
                &MutSet::default(),
                state.goal_phase,
            );

            work.extend(state.dependencies.notify_package(type_shortname));

            work.extend(state.dependencies.notify(module_id, Phase::LoadHeader));

            work.extend(state.dependencies.notify(module_id, Phase::Parse));

            work.extend(
                state
                    .dependencies
                    .notify(module_id, Phase::CanonicalizeAndConstrain),
            );

            start_tasks(arena, &mut state, work, injector, worker_listeners)?;

            Ok(state)
        }
        SolvedTypes {
            module_id,
            ident_ids,
            solved_module,
            solved_subs,
            decls,
            dep_idents,
            mut module_timing,
            mut unused_imports,
        } => {
            log!("solved types for {:?}", module_id);
            module_timing.end_time = SystemTime::now();

            state
                .module_cache
                .type_problems
                .insert(module_id, solved_module.problems);

            let existing = match state.module_cache.can_problems.entry(module_id) {
                Vacant(entry) => entry.insert(std::vec::Vec::new()),
                Occupied(entry) => entry.into_mut(),
            };

            for (unused, region) in unused_imports.drain() {
                existing.push(roc_problem::can::Problem::UnusedImport(unused, region));
            }

            let work = state.dependencies.notify(module_id, Phase::SolveTypes);

            // if there is a platform, the Package-Config module provides host-exposed,
            // otherwise the App module exposes host-exposed
            let is_host_exposed = match state.platform_data {
                None => module_id == state.root_id,
                Some(ref platform_data) => module_id == platform_data.module_id,
            };

            if is_host_exposed {
                state.exposed_to_host.extend(
                    solved_module
                        .exposed_vars_by_symbol
                        .iter()
                        .map(|(k, v)| (*k, *v)),
                );
            }

            if is_host_exposed && state.goal_phase == Phase::SolveTypes {
                debug_assert!(work.is_empty());
                debug_assert!(state.dependencies.solved_all());

                state.timings.insert(module_id, module_timing);

                let documentation = {
                    let mut empty = MutMap::default();
                    std::mem::swap(&mut empty, &mut state.module_cache.documentation);

                    empty
                };

                msg_tx
                    .send(Msg::FinishedAllTypeChecking {
                        solved_subs,
                        exposed_vars_by_symbol: solved_module.exposed_vars_by_symbol,
                        exposed_values: solved_module.exposed_symbols,
                        exposed_aliases_by_symbol: solved_module.aliases,
                        dep_idents,
                        documentation,
                    })
                    .map_err(|_| LoadingProblem::MsgChannelDied)?;

                // bookkeeping
                state.declarations_by_id.insert(module_id, decls);
                state.constrained_ident_ids.insert(module_id, ident_ids);

                // As far as type-checking goes, once we've solved
                // the originally requested module, we're all done!
                return Ok(state);
            } else {
                state.exposed_types.insert(
                    module_id,
                    ExposedModuleTypes::Valid(solved_module.solved_types, solved_module.aliases),
                );

                if state.goal_phase > Phase::SolveTypes {
                    let layout_cache = state
                        .layout_caches
                        .pop()
                        .unwrap_or_else(|| LayoutCache::new(state.ptr_bytes));

                    let typechecked = TypeCheckedModule {
                        module_id,
                        layout_cache,
                        module_timing,
                        solved_subs,
                        decls,
                        ident_ids,
                    };

                    state
                        .module_cache
                        .typechecked
                        .insert(module_id, typechecked);
                } else {
                    state.constrained_ident_ids.insert(module_id, ident_ids);
                }

                start_tasks(arena, &mut state, work, injector, worker_listeners)?;
            }

            Ok(state)
        }
        FoundSpecializations {
            module_id,
            procs_base,
            solved_subs,
            ident_ids,
            layout_cache,
            problems,
            module_timing,
        } => {
            log!("found specializations for {:?}", module_id);

            debug_assert!(problems.is_empty());

            let subs = solved_subs.into_inner();

            state
                .module_cache
                .top_level_thunks
                .entry(module_id)
                .or_default()
                .extend(procs_base.module_thunks.iter().copied());

            let found_specializations_module = FoundSpecializationsModule {
                module_id,
                ident_ids,
                layout_cache,
                procs_base,
                subs,
                module_timing,
            };

            state
                .module_cache
                .found_specializations
                .insert(module_id, found_specializations_module);

            let work = state
                .dependencies
                .notify(module_id, Phase::FindSpecializations);

            start_tasks(arena, &mut state, work, injector, worker_listeners)?;

            Ok(state)
        }
        MadeSpecializations {
            module_id,
            mut ident_ids,
            mut update_mode_ids,
            subs,
            procedures,
            external_specializations_requested,
            problems,
            module_timing,
            layout_cache,
            ..
        } => {
            log!("made specializations for {:?}", module_id);

            // in the future, layouts will be in SoA form and we'll want to hold on to this data
            let _ = layout_cache;

            state.module_cache.mono_problems.insert(module_id, problems);

            state.procedures.extend(procedures);
            state.timings.insert(module_id, module_timing);

            let work = state
                .dependencies
                .notify(module_id, Phase::MakeSpecializations);

            if work.is_empty()
                && state.dependencies.solved_all()
                && state.goal_phase == Phase::MakeSpecializations
            {
                Proc::insert_reset_reuse_operations(
                    arena,
                    module_id,
                    &mut ident_ids,
                    &mut update_mode_ids,
                    &mut state.procedures,
                );

                // display the mono IR of the module, for debug purposes
                if roc_mono::ir::PRETTY_PRINT_IR_SYMBOLS {
                    let procs_string = state
                        .procedures
                        .values()
                        .map(|proc| proc.to_pretty(200))
                        .collect::<Vec<_>>();

                    let result = procs_string.join("\n");

                    println!("{}", result);
                }

                Proc::insert_refcount_operations(arena, &mut state.procedures);

                // This is not safe with the new non-recursive RC updates that we do for tag unions
                //
                //                Proc::optimize_refcount_operations(
                //                    arena,
                //                    module_id,
                //                    &mut ident_ids,
                //                    &mut state.procedures,
                //                );

                state.constrained_ident_ids.insert(module_id, ident_ids);

                for (module_id, requested) in external_specializations_requested {
                    let existing = match state
                        .module_cache
                        .external_specializations_requested
                        .entry(module_id)
                    {
                        Vacant(entry) => entry.insert(vec![]),
                        Occupied(entry) => entry.into_mut(),
                    };

                    existing.push(requested);
                }

                msg_tx
                    .send(Msg::FinishedAllSpecialization {
                        subs,
                        // TODO thread through mono problems
                        exposed_to_host: state.exposed_to_host.clone(),
                    })
                    .map_err(|_| LoadingProblem::MsgChannelDied)?;

                // As far as type-checking goes, once we've solved
                // the originally requested module, we're all done!
                return Ok(state);
            } else {
                state.constrained_ident_ids.insert(module_id, ident_ids);

                for (module_id, requested) in external_specializations_requested {
                    let existing = match state
                        .module_cache
                        .external_specializations_requested
                        .entry(module_id)
                    {
                        Vacant(entry) => entry.insert(vec![]),
                        Occupied(entry) => entry.into_mut(),
                    };

                    existing.push(requested);
                }

                start_tasks(arena, &mut state, work, injector, worker_listeners)?;
            }

            Ok(state)
        }
        Msg::FinishedAllTypeChecking { .. } => {
            unreachable!();
        }
        Msg::FinishedAllSpecialization { .. } => {
            unreachable!();
        }
        Msg::FailedToParse(_) => {
            unreachable!();
        }
        Msg::FailedToReadFile { .. } => {
            unreachable!();
        }
    }
}

fn finish_specialization(
    state: State,
    subs: Subs,
    exposed_to_host: MutMap<Symbol, Variable>,
) -> Result<MonomorphizedModule, LoadingProblem> {
    let module_ids = Arc::try_unwrap(state.arc_modules)
        .unwrap_or_else(|_| panic!("There were still outstanding Arc references to module_ids"))
        .into_inner()
        .into_module_ids();

    let interns = Interns {
        module_ids,
        all_ident_ids: state.constrained_ident_ids,
    };

    let State {
        procedures,
        module_cache,
        output_path,
        platform_path,
        platform_data,
        ..
    } = state;

    let ModuleCache {
        mono_problems,
        type_problems,
        can_problems,
        sources,
        header_sources,
        ..
    } = module_cache;

    let sources: MutMap<ModuleId, (PathBuf, Box<str>)> = sources
        .into_iter()
        .map(|(id, (path, src))| (id, (path, src.into())))
        .collect();

    let header_sources: MutMap<ModuleId, (PathBuf, Box<str>)> = header_sources
        .into_iter()
        .map(|(id, (path, src))| (id, (path, src.into())))
        .collect();

    let path_to_platform = {
        use PlatformPath::*;
        let package_name = match platform_path {
            Valid(To::ExistingPackage(shorthand)) => {
                match (*state.arc_shorthands).lock().get(shorthand) {
                    Some(p_or_p) => *p_or_p,
                    None => unreachable!(),
                }
            }
            Valid(To::NewPackage(p_or_p)) => p_or_p,
            other => {
                let buf = to_missing_platform_report(state.root_id, other);
                return Err(LoadingProblem::FormattedReport(buf));
            }
        };

        package_name.0
    };

    let platform_path = path_to_platform.into();

    let entry_point = {
        let symbol = match platform_data {
            None => {
                debug_assert_eq!(exposed_to_host.len(), 1);
                *exposed_to_host.iter().next().unwrap().0
            }
            Some(PlatformData { provides, .. }) => provides,
        };

        match procedures.keys().find(|(s, _)| *s == symbol) {
            Some((_, layout)) => EntryPoint {
                layout: *layout,
                symbol,
            },
            None => {
                // the entry point is not specialized. This can happen if the repl output
                // is a function value
                EntryPoint {
                    layout: roc_mono::ir::ProcLayout {
                        arguments: &[],
                        result: Layout::Struct(&[]),
                    },
                    symbol,
                }
            }
        }
    };

    Ok(MonomorphizedModule {
        can_problems,
        mono_problems,
        type_problems,
        output_path: output_path.unwrap_or(DEFAULT_APP_OUTPUT_PATH).into(),
        platform_path,
        exposed_to_host,
        module_id: state.root_id,
        subs,
        interns,
        procedures,
        entry_point,
        sources,
        header_sources,
        timings: state.timings,
    })
}

fn finish(
    state: State,
    solved: Solved<Subs>,
    exposed_values: Vec<Symbol>,
    exposed_aliases_by_symbol: MutMap<Symbol, Alias>,
    exposed_vars_by_symbol: MutMap<Symbol, Variable>,
    dep_idents: MutMap<ModuleId, IdentIds>,
    documentation: MutMap<ModuleId, ModuleDocumentation>,
) -> LoadedModule {
    let module_ids = Arc::try_unwrap(state.arc_modules)
        .unwrap_or_else(|_| panic!("There were still outstanding Arc references to module_ids"))
        .into_inner()
        .into_module_ids();

    let interns = Interns {
        module_ids,
        all_ident_ids: state.constrained_ident_ids,
    };

    let sources = state
        .module_cache
        .sources
        .into_iter()
        .map(|(id, (path, src))| (id, (path, src.into())))
        .collect();

    let header_sources = state
        .module_cache
        .header_sources
        .into_iter()
        .map(|(id, (path, src))| (id, (path, src.into())))
        .collect();

    LoadedModule {
        module_id: state.root_id,
        interns,
        solved,
        can_problems: state.module_cache.can_problems,
        type_problems: state.module_cache.type_problems,
        declarations_by_id: state.declarations_by_id,
        dep_idents,
        exposed_aliases: exposed_aliases_by_symbol,
        exposed_values,
        exposed_to_host: exposed_vars_by_symbol.into_iter().collect(),
        header_sources,
        sources,
        timings: state.timings,
        documentation,
    }
}

/// Load a PkgConfig.roc file
fn load_pkg_config<'a>(
    arena: &'a Bump,
    src_dir: &Path,
    shorthand: &'a str,
    app_module_id: ModuleId,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
) -> Result<Msg<'a>, LoadingProblem<'a>> {
    let module_start_time = SystemTime::now();

    let filename = PathBuf::from(src_dir);

    let file_io_start = SystemTime::now();
    let file = fs::read(&filename);
    let file_io_duration = file_io_start.elapsed().unwrap();

    match file {
        Ok(bytes_vec) => {
            let parse_start = SystemTime::now();
            let bytes = arena.alloc(bytes_vec);
            let parse_state = roc_parse::state::State::new(bytes);
            let parsed = roc_parse::module::parse_header(arena, parse_state.clone());
            let parse_header_duration = parse_start.elapsed().unwrap();

            // Insert the first entries for this module's timings
            let mut pkg_module_timing = ModuleTiming::new(module_start_time);
            let mut effect_module_timing = ModuleTiming::new(module_start_time);

            pkg_module_timing.read_roc_file = file_io_duration;
            pkg_module_timing.parse_header = parse_header_duration;

            effect_module_timing.read_roc_file = file_io_duration;
            effect_module_timing.parse_header = parse_header_duration;

            match parsed {
                Ok((ast::Module::Interface { header }, _parse_state)) => {
                    Err(LoadingProblem::UnexpectedHeader(format!(
                        "expected platform/package module, got Interface with header\n{:?}",
                        header
                    )))
                }
                Ok((ast::Module::App { header }, _parse_state)) => {
                    Err(LoadingProblem::UnexpectedHeader(format!(
                        "expected platform/package module, got App with header\n{:?}",
                        header
                    )))
                }
                Ok((ast::Module::Platform { header }, parser_state)) => {
                    let delta = bytes.len() - parser_state.bytes().len();
                    let chomped = &bytes[..delta];
                    let header_src = unsafe { std::str::from_utf8_unchecked(chomped) };

                    // make a Package-Config module that ultimately exposes `main` to the host
                    let pkg_config_module_msg = fabricate_pkg_config_module(
                        arena,
                        shorthand,
                        app_module_id,
                        filename,
                        parser_state,
                        module_ids.clone(),
                        ident_ids_by_module.clone(),
                        &header,
                        header_src,
                        pkg_module_timing,
                    )
                    .1;

                    let effects_module_msg = fabricate_effects_module(
                        arena,
                        header.effects.effect_shortname,
                        module_ids,
                        ident_ids_by_module,
                        header,
                        effect_module_timing,
                    )
                    .1;

                    Ok(Msg::Many(vec![effects_module_msg, pkg_config_module_msg]))
                }
                Err(fail) => Err(LoadingProblem::ParsingFailed(
                    fail.map_problem(SyntaxError::Header)
                        .into_parse_problem(filename),
                )),
            }
        }

        Err(err) => Err(LoadingProblem::FileProblem {
            filename,
            error: err.kind(),
        }),
    }
}

/// Load a module by its module name, rather than by its filename
fn load_module<'a>(
    arena: &'a Bump,
    src_dir: &Path,
    module_name: PQModuleName<'a>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    arc_shorthands: Arc<Mutex<MutMap<&'a str, PackageName<'a>>>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem<'a>> {
    let module_start_time = SystemTime::now();
    let mut filename = PathBuf::new();

    filename.push(src_dir);

    let opt_shorthand;
    match module_name {
        PQModuleName::Unqualified(name) => {
            opt_shorthand = None;
            // Convert dots in module name to directories
            for part in name.split(MODULE_SEPARATOR) {
                filename.push(part);
            }
        }
        PQModuleName::Qualified(shorthand, name) => {
            opt_shorthand = Some(shorthand);
            let shorthands = arc_shorthands.lock();

            match shorthands.get(shorthand) {
                Some(PackageName(path)) => {
                    filename.push(path);
                }
                None => unreachable!("there is no shorthand named {:?}", shorthand),
            }

            // Convert dots in module name to directories
            for part in name.split(MODULE_SEPARATOR) {
                filename.push(part);
            }
        }
    }

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    load_filename(
        arena,
        filename,
        false,
        opt_shorthand,
        module_ids,
        ident_ids_by_module,
        module_start_time,
    )
}

/// Find a task according to the following algorithm:
///
/// 1. Look in a local Worker queue. If it has a task, pop it off the queue and return it.
/// 2. If that queue was empty, ask the global queue for a task.
/// 3. If the global queue is also empty, iterate through each Stealer (each Worker queue has a
///    corresponding Stealer, which can steal from it. Stealers can be shared across threads.)
///
/// Based on https://docs.rs/crossbeam/0.7.3/crossbeam/deque/index.html#examples
fn find_task<T>(local: &Worker<T>, global: &Injector<T>, stealers: &[Stealer<T>]) -> Option<T> {
    // Pop a task from the local queue, if not empty.
    local.pop().or_else(|| {
        // Otherwise, we need to look for a task elsewhere.
        iter::repeat_with(|| {
            // Try stealing a task from the global queue.
            global
                .steal()
                // Or try stealing a task from one of the other threads.
                .or_else(|| stealers.iter().map(|s| s.steal()).collect())
        })
        // Loop while no task was stolen and any steal operation needs to be retried.
        .find(|s| !s.is_retry())
        // Extract the stolen task, if there is one.
        .and_then(|s| s.success())
    })
}

#[allow(clippy::too_many_arguments)]
fn parse_header<'a>(
    arena: &'a Bump,
    read_file_duration: Duration,
    filename: PathBuf,
    is_root_module: bool,
    opt_shorthand: Option<&'a str>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    src_bytes: &'a [u8],
    start_time: SystemTime,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem<'a>> {
    let parse_start = SystemTime::now();
    let parse_state = roc_parse::state::State::new(src_bytes);
    let parsed = roc_parse::module::parse_header(arena, parse_state.clone());
    let parse_header_duration = parse_start.elapsed().unwrap();

    // Insert the first entries for this module's timings
    let mut module_timing = ModuleTiming::new(start_time);

    module_timing.read_roc_file = read_file_duration;
    module_timing.parse_header = parse_header_duration;

    match parsed {
        Ok((ast::Module::Interface { header }, parse_state)) => {
            let header_src = unsafe {
                let chomped = src_bytes.len() - parse_state.bytes().len();
                std::str::from_utf8_unchecked(&src_bytes[..chomped])
            };

            let info = HeaderInfo {
                loc_name: Loc {
                    region: header.name.region,
                    value: ModuleNameEnum::Interface(header.name.value),
                },
                filename,
                is_root_module,
                opt_shorthand,
                header_src,
                packages: &[],
                exposes: unspace(arena, header.exposes.items),
                imports: unspace(arena, header.imports.items),
                to_platform: None,
            };

            Ok(send_header(
                info,
                parse_state,
                module_ids,
                ident_ids_by_module,
                module_timing,
            ))
        }
        Ok((ast::Module::App { header }, parse_state)) => {
            let mut pkg_config_dir = filename.clone();
            pkg_config_dir.pop();

            let header_src = unsafe {
                let chomped = src_bytes.len() - parse_state.bytes().len();
                std::str::from_utf8_unchecked(&src_bytes[..chomped])
            };

            let packages = unspace(arena, header.packages.items);

            let info = HeaderInfo {
                loc_name: Loc {
                    region: header.name.region,
                    value: ModuleNameEnum::App(header.name.value),
                },
                filename,
                is_root_module,
                opt_shorthand,
                header_src,
                packages,
                exposes: unspace(arena, header.provides.items),
                imports: unspace(arena, header.imports.items),
                to_platform: Some(header.to.value),
            };

            let (module_id, app_module_header_msg) = send_header(
                info,
                parse_state,
                module_ids.clone(),
                ident_ids_by_module.clone(),
                module_timing,
            );

            match header.to.value {
                To::ExistingPackage(existing_package) => {
                    let opt_base_package = packages.iter().find_map(|loc_package_entry| {
                        let Loc { value, .. } = loc_package_entry;

                        if value.shorthand == existing_package {
                            Some(value)
                        } else {
                            None
                        }
                    });

                    if let Some(PackageEntry {
                        shorthand,
                        package_name:
                            Loc {
                                value: package_name,
                                ..
                            },
                        ..
                    }) = opt_base_package
                    {
                        let package = package_name.0;

                        // check whether we can find a Package-Config.roc file
                        let mut pkg_config_roc = pkg_config_dir;
                        pkg_config_roc.push(package);
                        pkg_config_roc.push(PKG_CONFIG_FILE_NAME);
                        pkg_config_roc.set_extension(ROC_FILE_EXTENSION);

                        if pkg_config_roc.as_path().exists() {
                            let load_pkg_config_msg = load_pkg_config(
                                arena,
                                &pkg_config_roc,
                                shorthand,
                                module_id,
                                module_ids,
                                ident_ids_by_module,
                            )?;

                            Ok((
                                module_id,
                                Msg::Many(vec![app_module_header_msg, load_pkg_config_msg]),
                            ))
                        } else {
                            Ok((module_id, app_module_header_msg))
                        }
                    } else {
                        panic!("could not find base")
                    }
                }
                To::NewPackage(_package_name) => Ok((module_id, app_module_header_msg)),
            }
        }
        Ok((ast::Module::Platform { header }, _parse_state)) => Ok(fabricate_effects_module(
            arena,
            "",
            module_ids,
            ident_ids_by_module,
            header,
            module_timing,
        )),
        Err(fail) => Err(LoadingProblem::ParsingFailed(
            fail.map_problem(SyntaxError::Header)
                .into_parse_problem(filename),
        )),
    }
}

/// Load a module by its filename
#[allow(clippy::too_many_arguments)]
fn load_filename<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    is_root_module: bool,
    opt_shorthand: Option<&'a str>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    module_start_time: SystemTime,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem<'a>> {
    let file_io_start = SystemTime::now();
    let file = fs::read(&filename);
    let file_io_duration = file_io_start.elapsed().unwrap();

    match file {
        Ok(bytes) => parse_header(
            arena,
            file_io_duration,
            filename,
            is_root_module,
            opt_shorthand,
            module_ids,
            ident_ids_by_module,
            arena.alloc(bytes),
            module_start_time,
        ),
        Err(err) => Err(LoadingProblem::FileProblem {
            filename,
            error: err.kind(),
        }),
    }
}

/// Load a module from a str
/// the `filename` is never read, but used for the module name
#[allow(clippy::too_many_arguments)]
fn load_from_str<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    src: &'a str,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    module_start_time: SystemTime,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem<'a>> {
    let file_io_start = SystemTime::now();
    let file_io_duration = file_io_start.elapsed().unwrap();

    parse_header(
        arena,
        file_io_duration,
        filename,
        false,
        None,
        module_ids,
        ident_ids_by_module,
        src.as_bytes(),
        module_start_time,
    )
}

#[derive(Debug)]
enum ModuleNameEnum<'a> {
    /// A filename
    App(StrLiteral<'a>),
    Interface(roc_parse::header::ModuleName<'a>),
    PkgConfig,
}

#[derive(Debug)]
struct HeaderInfo<'a> {
    loc_name: Loc<ModuleNameEnum<'a>>,
    filename: PathBuf,
    is_root_module: bool,
    opt_shorthand: Option<&'a str>,
    header_src: &'a str,
    packages: &'a [Loc<PackageEntry<'a>>],
    exposes: &'a [Loc<ExposedName<'a>>],
    imports: &'a [Loc<ImportsEntry<'a>>],
    to_platform: Option<To<'a>>,
}

#[allow(clippy::too_many_arguments)]
fn send_header<'a>(
    info: HeaderInfo<'a>,
    parse_state: roc_parse::state::State<'a>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    module_timing: ModuleTiming,
) -> (ModuleId, Msg<'a>) {
    use ModuleNameEnum::*;

    let HeaderInfo {
        loc_name,
        filename,
        is_root_module,
        opt_shorthand,
        packages,
        exposes,
        imports,
        to_platform,
        header_src,
    } = info;

    let declared_name: ModuleName = match &loc_name.value {
        PkgConfig => unreachable!(),
        App(_) => ModuleName::APP.into(),
        Interface(module_name) => {
            // TODO check to see if module_name is consistent with filename.
            // If it isn't, report a problem!

            module_name.as_str().into()
        }
    };

    let mut imported: Vec<(QualifiedModuleName, Vec<Ident>, Region)> =
        Vec::with_capacity(imports.len());
    let mut imported_modules: MutMap<ModuleId, Region> = MutMap::default();
    let mut scope_size = 0;

    for loc_entry in imports {
        let (qualified_module_name, exposed) = exposed_from_import(&loc_entry.value);

        scope_size += exposed.len();

        imported.push((qualified_module_name, exposed, loc_entry.region));
    }

    let num_exposes = exposes.len();
    let mut deps_by_name: MutMap<PQModuleName, ModuleId> =
        HashMap::with_capacity_and_hasher(num_exposes, default_hasher());
    let mut exposed: Vec<Symbol> = Vec::with_capacity(num_exposes);

    // Make sure the module_ids has ModuleIds for all our deps,
    // then record those ModuleIds in can_module_ids for later.
    let mut scope: MutMap<Ident, (Symbol, Region)> =
        HashMap::with_capacity_and_hasher(scope_size, default_hasher());
    let home: ModuleId;

    let ident_ids = {
        // Lock just long enough to perform the minimal operations necessary.
        let mut module_ids = (*module_ids).lock();
        let mut ident_ids_by_module = (*ident_ids_by_module).lock();

        let name = match opt_shorthand {
            Some(shorthand) => PQModuleName::Qualified(shorthand, declared_name),
            None => PQModuleName::Unqualified(declared_name),
        };
        home = module_ids.get_or_insert(&name);

        // Ensure this module has an entry in the exposed_ident_ids map.
        ident_ids_by_module
            .entry(home)
            .or_insert_with(IdentIds::default);

        // For each of our imports, add an entry to deps_by_name
        //
        // e.g. for `imports [ pf.Foo.{ bar } ]`, add `Foo` to deps_by_name
        //
        // Also build a list of imported_values_to_expose (like `bar` above.)
        for (qualified_module_name, exposed_idents, region) in imported.into_iter() {
            let cloned_module_name = qualified_module_name.module.clone();
            let pq_module_name = match qualified_module_name.opt_package {
                None => match opt_shorthand {
                    Some(shorthand) => {
                        PQModuleName::Qualified(shorthand, qualified_module_name.module)
                    }
                    None => PQModuleName::Unqualified(qualified_module_name.module),
                },
                Some(package) => PQModuleName::Qualified(package, cloned_module_name),
            };

            let module_id = module_ids.get_or_insert(&pq_module_name);
            imported_modules.insert(module_id, region);

            deps_by_name.insert(pq_module_name, module_id);

            // Add the new exposed idents to the dep module's IdentIds, so
            // once that module later gets loaded, its lookups will resolve
            // to the same symbols as the ones we're using here.
            let ident_ids = ident_ids_by_module
                .entry(module_id)
                .or_insert_with(IdentIds::default);

            for ident in exposed_idents {
                let ident_id = ident_ids.get_or_insert(&ident);
                let symbol = Symbol::new(module_id, ident_id);

                // Since this value is exposed, add it to our module's default scope.
                debug_assert!(!scope.contains_key(&ident.clone()));

                scope.insert(ident, (symbol, region));
            }
        }

        let ident_ids = ident_ids_by_module.get_mut(&home).unwrap();

        // Generate IdentIds entries for all values this module exposes.
        // This way, when we encounter them in Defs later, they already
        // have an IdentIds entry.
        //
        // We must *not* add them to scope yet, or else the Defs will
        // incorrectly think they're shadowing them!
        for loc_exposed in exposes.iter() {
            // Use get_or_insert here because the ident_ids may already
            // created an IdentId for this, when it was imported exposed
            // in a dependent module.
            //
            // For example, if module A has [ B.{ foo } ], then
            // when we get here for B, `foo` will already have
            // an IdentId. We must reuse that!
            let ident_id = ident_ids.get_or_insert(&loc_exposed.value.as_str().into());
            let symbol = Symbol::new(home, ident_id);

            exposed.push(symbol);
        }

        if cfg!(debug_assertions) {
            home.register_debug_idents(ident_ids);
        }

        ident_ids.clone()
    };

    let package_entries = packages
        .iter()
        .map(|pkg| {
            let pkg = pkg.value;
            (pkg.shorthand, pkg.package_name.value)
        })
        .collect::<MutMap<_, _>>();

    // Send the deps to the coordinator thread for processing,
    // then continue on to parsing and canonicalizing defs.
    //
    // We always need to send these, even if deps is empty,
    // because the coordinator thread needs to receive this message
    // to decrement its "pending" count.
    let extra = match to_platform {
        Some(to_platform) => HeaderFor::App { to_platform },
        None => HeaderFor::Interface,
    };

    let mut package_qualified_imported_modules = MutSet::default();
    for (pq_module_name, module_id) in &deps_by_name {
        match pq_module_name {
            PackageQualified::Unqualified(_) => {
                package_qualified_imported_modules
                    .insert(PackageQualified::Unqualified(*module_id));
            }
            PackageQualified::Qualified(shorthand, _) => {
                package_qualified_imported_modules
                    .insert(PackageQualified::Qualified(shorthand, *module_id));
            }
        }
    }

    (
        home,
        Msg::Header(
            ModuleHeader {
                module_id: home,
                module_path: filename,
                is_root_module,
                exposed_ident_ids: ident_ids,
                module_name: loc_name.value,
                packages: package_entries,
                imported_modules,
                package_qualified_imported_modules,
                deps_by_name,
                exposes: exposed,
                header_src,
                parse_state,
                exposed_imports: scope,
                module_timing,
            },
            extra,
        ),
    )
}

#[derive(Debug)]
struct PlatformHeaderInfo<'a> {
    filename: PathBuf,
    is_root_module: bool,
    shorthand: &'a str,
    header_src: &'a str,
    app_module_id: ModuleId,
    packages: &'a [Loc<PackageEntry<'a>>],
    provides: &'a [Loc<ExposedName<'a>>],
    requires: &'a [Loc<TypedIdent<'a>>],
    imports: &'a [Loc<ImportsEntry<'a>>],
}

// TODO refactor so more logic is shared with `send_header`
#[allow(clippy::too_many_arguments)]
fn send_header_two<'a>(
    info: PlatformHeaderInfo<'a>,
    parse_state: roc_parse::state::State<'a>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    module_timing: ModuleTiming,
) -> (ModuleId, Msg<'a>) {
    let PlatformHeaderInfo {
        filename,
        shorthand,
        is_root_module,
        header_src,
        app_module_id,
        packages,
        provides,
        requires,
        imports,
    } = info;

    let declared_name: ModuleName = "".into();

    let mut imported: Vec<(QualifiedModuleName, Vec<Ident>, Region)> =
        Vec::with_capacity(imports.len());
    let mut imported_modules: MutMap<ModuleId, Region> = MutMap::default();

    let num_exposes = provides.len();
    let mut deps_by_name: MutMap<PQModuleName, ModuleId> =
        HashMap::with_capacity_and_hasher(num_exposes, default_hasher());

    // add standard imports
    // TODO add Effect by default
    imported_modules.insert(app_module_id, Region::zero());
    deps_by_name.insert(
        PQModuleName::Unqualified(ModuleName::APP.into()),
        app_module_id,
    );

    let mut scope_size = 0;

    for loc_entry in imports {
        let (qualified_module_name, exposed) = exposed_from_import(&loc_entry.value);

        scope_size += exposed.len();

        imported.push((qualified_module_name, exposed, loc_entry.region));
    }

    let mut exposed: Vec<Symbol> = Vec::with_capacity(num_exposes);

    // Make sure the module_ids has ModuleIds for all our deps,
    // then record those ModuleIds in can_module_ids for later.
    let mut scope: MutMap<Ident, (Symbol, Region)> =
        HashMap::with_capacity_and_hasher(scope_size, default_hasher());
    let home: ModuleId;

    let mut ident_ids = {
        // Lock just long enough to perform the minimal operations necessary.
        let mut module_ids = (*module_ids).lock();
        let mut ident_ids_by_module = (*ident_ids_by_module).lock();

        let name = PQModuleName::Qualified(shorthand, declared_name);
        home = module_ids.get_or_insert(&name);

        // Ensure this module has an entry in the exposed_ident_ids map.
        ident_ids_by_module
            .entry(home)
            .or_insert_with(IdentIds::default);

        // For each of our imports, add an entry to deps_by_name
        //
        // e.g. for `imports [ pf.Foo.{ bar } ]`, add `Foo` to deps_by_name
        //
        // Also build a list of imported_values_to_expose (like `bar` above.)
        for (qualified_module_name, exposed_idents, region) in imported.into_iter() {
            let cloned_module_name = qualified_module_name.module.clone();
            let pq_module_name = match qualified_module_name.opt_package {
                None => PQModuleName::Qualified(shorthand, qualified_module_name.module),
                Some(package) => PQModuleName::Qualified(package, cloned_module_name.clone()),
            };

            let module_id = module_ids.get_or_insert(&pq_module_name);
            imported_modules.insert(module_id, region);

            deps_by_name.insert(pq_module_name, module_id);

            // Add the new exposed idents to the dep module's IdentIds, so
            // once that module later gets loaded, its lookups will resolve
            // to the same symbols as the ones we're using here.
            let ident_ids = ident_ids_by_module
                .entry(module_id)
                .or_insert_with(IdentIds::default);

            for ident in exposed_idents {
                let ident_id = ident_ids.get_or_insert(&ident);
                let symbol = Symbol::new(module_id, ident_id);

                // Since this value is exposed, add it to our module's default scope.
                debug_assert!(!scope.contains_key(&ident.clone()));

                scope.insert(ident, (symbol, region));
            }
        }

        {
            let ident_ids = ident_ids_by_module
                .entry(app_module_id)
                .or_insert_with(IdentIds::default);

            for entry in requires {
                let entry = entry.value;

                let ident: Ident = entry.ident.value.into();
                let ident_id = ident_ids.get_or_insert(&ident);
                let symbol = Symbol::new(app_module_id, ident_id);

                // Since this value is exposed, add it to our module's default scope.
                debug_assert!(!scope.contains_key(&ident.clone()));

                scope.insert(ident, (symbol, entry.ident.region));
            }
        }

        let ident_ids = ident_ids_by_module.get_mut(&home).unwrap();

        // Generate IdentIds entries for all values this module exposes.
        // This way, when we encounter them in Defs later, they already
        // have an IdentIds entry.
        //
        // We must *not* add them to scope yet, or else the Defs will
        // incorrectly think they're shadowing them!
        for loc_exposed in provides.iter() {
            // Use get_or_insert here because the ident_ids may already
            // created an IdentId for this, when it was imported exposed
            // in a dependent module.
            //
            // For example, if module A has [ B.{ foo } ], then
            // when we get here for B, `foo` will already have
            // an IdentId. We must reuse that!
            let ident_id = ident_ids.get_or_insert(&loc_exposed.value.as_str().into());
            let symbol = Symbol::new(home, ident_id);

            exposed.push(symbol);
        }

        if cfg!(debug_assertions) {
            home.register_debug_idents(ident_ids);
        }

        ident_ids.clone()
    };

    let package_entries = packages
        .iter()
        .map(|pkg| (pkg.value.shorthand, pkg.value.package_name.value))
        .collect::<MutMap<_, _>>();

    // Send the deps to the coordinator thread for processing,
    // then continue on to parsing and canonicalizing defs.
    //
    // We always need to send these, even if deps is empty,
    // because the coordinator thread needs to receive this message
    // to decrement its "pending" count.
    let module_name = ModuleNameEnum::PkgConfig;

    let main_for_host = {
        let ident_str: Ident = provides[0].value.as_str().into();
        let ident_id = ident_ids.get_or_insert(&ident_str);

        Symbol::new(home, ident_id)
    };

    let extra = HeaderFor::PkgConfig {
        config_shorthand: shorthand,
        platform_main_type: requires[0].value,
        main_for_host,
    };

    let mut package_qualified_imported_modules = MutSet::default();
    for (pq_module_name, module_id) in &deps_by_name {
        match pq_module_name {
            PackageQualified::Unqualified(_) => {
                package_qualified_imported_modules
                    .insert(PackageQualified::Unqualified(*module_id));
            }
            PackageQualified::Qualified(shorthand, _) => {
                package_qualified_imported_modules
                    .insert(PackageQualified::Qualified(shorthand, *module_id));
            }
        }
    }

    (
        home,
        Msg::Header(
            ModuleHeader {
                module_id: home,
                module_path: filename,
                is_root_module,
                exposed_ident_ids: ident_ids,
                module_name,
                packages: package_entries,
                imported_modules,
                package_qualified_imported_modules,
                deps_by_name,
                exposes: exposed,
                header_src,
                parse_state,
                exposed_imports: scope,
                module_timing,
            },
            extra,
        ),
    )
}

impl<'a> BuildTask<'a> {
    // TODO trim down these arguments - possibly by moving Constraint into Module
    #[allow(clippy::too_many_arguments)]
    pub fn solve_module(
        module: Module,
        ident_ids: IdentIds,
        module_timing: ModuleTiming,
        constraint: Constraint,
        var_store: VarStore,
        imported_modules: MutMap<ModuleId, Region>,
        exposed_types: &mut SubsByModule,
        stdlib: &StdLib,
        dep_idents: MutMap<ModuleId, IdentIds>,
        declarations: Vec<Declaration>,
    ) -> Self {
        let home = module.module_id;

        // Get the constraints for this module's imports. We do this on the main thread
        // to avoid having to lock the map of exposed types, or to clone it
        // (which would be more expensive for the main thread).
        let ConstrainableImports {
            imported_symbols,
            imported_aliases: _,
            unused_imports,
        } = pre_constrain_imports(
            home,
            &module.references,
            imported_modules,
            exposed_types,
            stdlib,
        );

        // Next, solve this module in the background.
        Self::Solve {
            module,
            ident_ids,
            imported_symbols,
            constraint,
            var_store,
            declarations,
            dep_idents,
            module_timing,
            unused_imports,
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn run_solve<'a>(
    module: Module,
    ident_ids: IdentIds,
    mut module_timing: ModuleTiming,
    imported_symbols: Vec<Import>,
    constraint: Constraint,
    mut var_store: VarStore,
    decls: Vec<Declaration>,
    dep_idents: MutMap<ModuleId, IdentIds>,
    unused_imports: MutMap<ModuleId, Region>,
) -> Msg<'a> {
    // We have more constraining work to do now, so we'll add it to our timings.
    let constrain_start = SystemTime::now();

    // Finish constraining the module by wrapping the existing Constraint
    // in the ones we just computed. We can do this off the main thread.
    let constraint = constrain_imports(imported_symbols, constraint, &mut var_store);

    let constrain_end = SystemTime::now();

    let module_id = module.module_id;

    let Module {
        exposed_symbols,
        aliases,
        rigid_variables,
        ..
    } = module;

    if false {
        debug_assert!(constraint.validate(), "{:?}", &constraint);
    }

    let (solved_subs, solved_env, problems) =
        roc_solve::module::run_solve(aliases, rigid_variables, constraint, var_store);

    let mut exposed_vars_by_symbol: MutMap<Symbol, Variable> = solved_env.vars_by_symbol.clone();
    exposed_vars_by_symbol.retain(|k, _| exposed_symbols.contains(k));

    let solved_types =
        roc_solve::module::make_solved_types(&solved_env, &solved_subs, &exposed_vars_by_symbol);

    let solved_module = SolvedModule {
        exposed_vars_by_symbol,
        exposed_symbols: exposed_symbols.into_iter().collect::<Vec<_>>(),
        solved_types,
        problems,
        aliases: solved_env.aliases,
    };

    // Record the final timings
    let solve_end = SystemTime::now();
    let constrain_elapsed = constrain_end.duration_since(constrain_start).unwrap();

    module_timing.constrain += constrain_elapsed;
    module_timing.solve = solve_end.duration_since(constrain_end).unwrap();

    // Send the subs to the main thread for processing,
    Msg::SolvedTypes {
        module_id,
        solved_subs,
        ident_ids,
        decls,
        dep_idents,
        solved_module,
        module_timing,
        unused_imports,
    }
}

fn unspace<'a, T: Copy>(arena: &'a Bump, items: &[Loc<Spaced<'a, T>>]) -> &'a [Loc<T>] {
    bumpalo::collections::Vec::from_iter_in(
        items
            .iter()
            .map(|item| Loc::at(item.region, item.value.extract_spaces().item)),
        arena,
    )
    .into_bump_slice()
}

#[allow(clippy::too_many_arguments)]
fn fabricate_pkg_config_module<'a>(
    arena: &'a Bump,
    shorthand: &'a str,
    app_module_id: ModuleId,
    filename: PathBuf,
    parse_state: roc_parse::state::State<'a>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    header: &PlatformHeader<'a>,
    header_src: &'a str,
    module_timing: ModuleTiming,
) -> (ModuleId, Msg<'a>) {
    let info = PlatformHeaderInfo {
        filename,
        is_root_module: false,
        shorthand,
        header_src,
        app_module_id,
        packages: &[],
        provides: unspace(arena, header.provides.items),
        requires: &*arena.alloc([Loc::at(
            header.requires.signature.region,
            header.requires.signature.extract_spaces().item,
        )]),
        imports: unspace(arena, header.imports.items),
    };

    send_header_two(
        info,
        parse_state,
        module_ids,
        ident_ids_by_module,
        module_timing,
    )
}

#[allow(clippy::too_many_arguments)]
fn fabricate_effects_module<'a>(
    arena: &'a Bump,
    shorthand: &'a str,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    header: PlatformHeader<'a>,
    module_timing: ModuleTiming,
) -> (ModuleId, Msg<'a>) {
    let num_exposes = header.provides.len() + 1;
    let mut exposed: Vec<Symbol> = Vec::with_capacity(num_exposes);

    let effects = header.effects;

    let module_id: ModuleId;

    let effect_entries = unpack_exposes_entries(arena, effects.entries.items);
    let name = effects.effect_type_name;
    let declared_name: ModuleName = name.into();

    let hardcoded_effect_symbols = {
        let mut functions: Vec<_> = crate::effect_module::BUILTIN_EFFECT_FUNCTIONS
            .iter()
            .map(|x| x.0)
            .collect();
        functions.push(name);

        functions
    };

    {
        let mut module_ids = (*module_ids).lock();

        for exposed in header.exposes.iter() {
            let module_name = exposed.value.extract_spaces().item;

            module_ids.get_or_insert(&PQModuleName::Qualified(
                shorthand,
                module_name.as_str().into(),
            ));
        }
    }

    let exposed_ident_ids = {
        // Lock just long enough to perform the minimal operations necessary.
        let mut module_ids = (*module_ids).lock();
        let mut ident_ids_by_module = (*ident_ids_by_module).lock();

        let name = PQModuleName::Qualified(shorthand, declared_name);
        module_id = module_ids.get_or_insert(&name);

        // Ensure this module has an entry in the exposed_ident_ids map.
        ident_ids_by_module
            .entry(module_id)
            .or_insert_with(IdentIds::default);

        let ident_ids = ident_ids_by_module.get_mut(&module_id).unwrap();

        // Generate IdentIds entries for all values this module exposes.
        // This way, when we encounter them in Defs later, they already
        // have an IdentIds entry.
        //
        // We must *not* add them to scope yet, or else the Defs will
        // incorrectly think they're shadowing them!
        for (loc_exposed, _) in effect_entries.iter() {
            // Use get_or_insert here because the ident_ids may already
            // created an IdentId for this, when it was imported exposed
            // in a dependent module.
            //
            // For example, if module A has [ B.{ foo } ], then
            // when we get here for B, `foo` will already have
            // an IdentId. We must reuse that!
            let ident_id = ident_ids.get_or_insert(&loc_exposed.value.into());
            let symbol = Symbol::new(module_id, ident_id);

            exposed.push(symbol);
        }

        for hardcoded in hardcoded_effect_symbols {
            // Use get_or_insert here because the ident_ids may already
            // created an IdentId for this, when it was imported exposed
            // in a dependent module.
            //
            // For example, if module A has [ B.{ foo } ], then
            // when we get here for B, `foo` will already have
            // an IdentId. We must reuse that!
            let ident_id = ident_ids.get_or_insert(&hardcoded.into());
            let symbol = Symbol::new(module_id, ident_id);

            exposed.push(symbol);
        }

        if cfg!(debug_assertions) {
            module_id.register_debug_idents(ident_ids);
        }

        ident_ids.clone()
    };

    // a platform module has no dependencies, hence empty
    let dep_idents: MutMap<ModuleId, IdentIds> = IdentIds::exposed_builtins(0);

    let mut var_store = VarStore::default();

    let module_ids = { (*module_ids).lock().clone() }.into_module_ids();

    let mut scope = roc_can::scope::Scope::new(module_id, &mut var_store);
    let mut can_env =
        roc_can::env::Env::new(module_id, &dep_idents, &module_ids, exposed_ident_ids);

    let effect_symbol = scope
        .introduce(
            name.into(),
            &can_env.exposed_ident_ids,
            &mut can_env.ident_ids,
            Region::zero(),
        )
        .unwrap();

    let effect_tag_name = TagName::Private(effect_symbol);

    let mut aliases = MutMap::default();
    let alias = {
        let a_var = var_store.fresh();

        let actual = crate::effect_module::build_effect_actual(
            effect_tag_name,
            Type::Variable(a_var),
            &mut var_store,
        );

        scope.add_alias(
            effect_symbol,
            Region::zero(),
            vec![Loc::at_zero(("a".into(), a_var))],
            actual,
        );

        scope.lookup_alias(effect_symbol).unwrap().clone()
    };

    aliases.insert(effect_symbol, alias);

    let mut declarations = Vec::new();

    let exposed_symbols: MutSet<Symbol> = {
        let mut exposed_symbols = MutSet::default();

        {
            for (ident, ann) in effect_entries {
                let symbol = {
                    scope
                        .introduce(
                            ident.value.into(),
                            &can_env.exposed_ident_ids,
                            &mut can_env.ident_ids,
                            Region::zero(),
                        )
                        .unwrap()
                };

                let annotation = roc_can::annotation::canonicalize_annotation(
                    &mut can_env,
                    &mut scope,
                    &ann.value,
                    Region::zero(),
                    &mut var_store,
                );

                let def = crate::effect_module::build_host_exposed_def(
                    &mut can_env,
                    &mut scope,
                    symbol,
                    ident.value,
                    TagName::Private(effect_symbol),
                    &mut var_store,
                    annotation,
                );
                exposed_symbols.insert(symbol);

                declarations.push(Declaration::Declare(def));
            }
        }

        // define Effect.after, Effect.map etc.
        crate::effect_module::build_effect_builtins(
            &mut can_env,
            &mut scope,
            effect_symbol,
            &mut var_store,
            &mut exposed_symbols,
            &mut declarations,
        );

        exposed_symbols
    };

    use roc_can::module::ModuleOutput;
    let module_output = ModuleOutput {
        aliases,
        rigid_variables: MutMap::default(),
        declarations,
        exposed_imports: MutMap::default(),
        lookups: Vec::new(),
        problems: can_env.problems,
        ident_ids: can_env.ident_ids,
        references: MutSet::default(),
        scope,
    };

    let constraint = constrain_module(&module_output.declarations, module_id);

    let module = Module {
        module_id,
        exposed_imports: module_output.exposed_imports,
        exposed_symbols,
        references: module_output.references,
        aliases: module_output.aliases,
        rigid_variables: module_output.rigid_variables,
    };

    let imported_modules = MutMap::default();

    // Should a effect module ever have a ModuleDocumentation?
    let module_docs = ModuleDocumentation {
        name: String::from(name),
        entries: Vec::new(),
        scope: module_output.scope,
    };

    let constrained_module = ConstrainedModule {
        module,
        declarations: module_output.declarations,
        imported_modules,
        var_store,
        constraint,
        ident_ids: module_output.ident_ids,
        dep_idents,
        module_timing,
    };

    (
        module_id,
        Msg::MadeEffectModule {
            type_shortname: effects.effect_shortname,
            constrained_module,
            canonicalization_problems: module_output.problems,
            module_docs,
        },
    )
}

fn unpack_exposes_entries<'a>(
    arena: &'a Bump,
    entries: &'a [Loc<Spaced<'a, TypedIdent<'a>>>],
) -> bumpalo::collections::Vec<'a, (Loc<&'a str>, Loc<TypeAnnotation<'a>>)> {
    use bumpalo::collections::Vec;

    let iter = entries.iter().map(|entry| {
        let entry: TypedIdent<'a> = entry.value.extract_spaces().item;
        (entry.ident, entry.ann)
    });

    Vec::from_iter_in(iter, arena)
}

#[allow(clippy::too_many_arguments)]
#[allow(clippy::unnecessary_wraps)]
fn canonicalize_and_constrain<'a, F>(
    arena: &'a Bump,
    module_ids: &ModuleIds,
    dep_idents: MutMap<ModuleId, IdentIds>,
    exposed_symbols: MutSet<Symbol>,
    aliases: MutMap<Symbol, Alias>,
    parsed: ParsedModule<'a>,
    look_up_builtins: F,
) -> Result<Msg<'a>, LoadingProblem<'a>>
where
    F: Fn(Symbol, &mut VarStore) -> Option<Def> + 'static + Send + Copy,
{
    let canonicalize_start = SystemTime::now();

    let ParsedModule {
        module_id,
        module_name,
        exposed_ident_ids,
        parsed_defs,
        exposed_imports,
        imported_modules,
        mut module_timing,
        ..
    } = parsed;

    let mut var_store = VarStore::default();
    let canonicalized = canonicalize_module_defs(
        arena,
        parsed_defs,
        module_id,
        module_ids,
        exposed_ident_ids,
        &dep_idents,
        aliases,
        exposed_imports,
        &exposed_symbols,
        &mut var_store,
        look_up_builtins,
    );
    let canonicalize_end = SystemTime::now();

    module_timing.canonicalize = canonicalize_end.duration_since(canonicalize_start).unwrap();

    match canonicalized {
        Ok(module_output) => {
            // Generate documentation information
            // TODO: store timing information?
            let module_docs = match module_name {
                ModuleNameEnum::PkgConfig => None,
                ModuleNameEnum::App(_) => None,
                ModuleNameEnum::Interface(name) => Some(crate::docs::generate_module_docs(
                    module_output.scope,
                    name.as_str().into(),
                    &module_output.ident_ids,
                    parsed_defs,
                )),
            };

            let constraint = constrain_module(&module_output.declarations, module_id);

            let module = Module {
                module_id,
                exposed_imports: module_output.exposed_imports,
                exposed_symbols,
                references: module_output.references,
                aliases: module_output.aliases,
                rigid_variables: module_output.rigid_variables,
            };

            let constrained_module = ConstrainedModule {
                module,
                declarations: module_output.declarations,
                imported_modules,
                var_store,
                constraint,
                ident_ids: module_output.ident_ids,
                dep_idents,
                module_timing,
            };

            Ok(Msg::CanonicalizedAndConstrained {
                constrained_module,
                canonicalization_problems: module_output.problems,
                module_docs,
            })
        }
        Err(runtime_error) => {
            panic!(
                "TODO gracefully handle module canonicalization error {:?}",
                runtime_error
            );
        }
    }
}

fn parse<'a>(arena: &'a Bump, header: ModuleHeader<'a>) -> Result<Msg<'a>, LoadingProblem<'a>> {
    let mut module_timing = header.module_timing;
    let parse_start = SystemTime::now();
    let source = header.parse_state.bytes();
    let parse_state = header.parse_state;
    let parsed_defs = match module_defs().parse(arena, parse_state) {
        Ok((_, success, _state)) => success,
        Err((_, fail, state)) => {
            return Err(LoadingProblem::ParsingFailed(
                fail.into_parse_problem(header.module_path, &state),
            ));
        }
    };

    let parsed_defs = parsed_defs.into_bump_slice();

    // Record the parse end time once, to avoid checking the time a second time
    // immediately afterward (for the beginning of canonicalization).
    let parse_end = SystemTime::now();

    module_timing.parse_body = parse_end.duration_since(parse_start).unwrap();

    let imported_modules = header.imported_modules;

    // SAFETY: By this point we've already incrementally verified that there
    // are no UTF-8 errors in these bytes. If there had been any UTF-8 errors,
    // we'd have bailed out before now.
    let src = unsafe { from_utf8_unchecked(source) };

    let ModuleHeader {
        module_id,
        module_name,
        deps_by_name,
        exposed_ident_ids,
        exposed_imports,
        module_path,
        ..
    } = header;

    let parsed = ParsedModule {
        module_id,
        module_name,
        module_path,
        src,
        module_timing,
        deps_by_name,
        imported_modules,
        exposed_ident_ids,
        exposed_imports,
        parsed_defs,
    };

    Ok(Msg::Parsed(parsed))
}

fn exposed_from_import<'a>(entry: &ImportsEntry<'a>) -> (QualifiedModuleName<'a>, Vec<Ident>) {
    use roc_parse::header::ImportsEntry::*;

    match entry {
        Module(module_name, exposes) => {
            let mut exposed = Vec::with_capacity(exposes.len());

            for loc_entry in exposes.iter() {
                exposed.push(ident_from_exposed(&loc_entry.value));
            }

            let qualified_module_name = QualifiedModuleName {
                opt_package: None,
                module: module_name.as_str().into(),
            };

            (qualified_module_name, exposed)
        }

        Package(package_name, module_name, exposes) => {
            let mut exposed = Vec::with_capacity(exposes.len());

            for loc_entry in exposes.iter() {
                exposed.push(ident_from_exposed(&loc_entry.value));
            }

            let qualified_module_name = QualifiedModuleName {
                opt_package: Some(package_name),
                module: module_name.as_str().into(),
            };

            (qualified_module_name, exposed)
        }
    }
}

fn ident_from_exposed(entry: &Spaced<'_, ExposedName<'_>>) -> Ident {
    entry.extract_spaces().item.as_str().into()
}

#[allow(clippy::too_many_arguments)]
fn make_specializations<'a>(
    arena: &'a Bump,
    home: ModuleId,
    mut ident_ids: IdentIds,
    mut subs: Subs,
    procs_base: ProcsBase<'a>,
    mut layout_cache: LayoutCache<'a>,
    specializations_we_must_make: Vec<ExternalSpecializations>,
    mut module_timing: ModuleTiming,
    ptr_bytes: u32,
) -> Msg<'a> {
    let make_specializations_start = SystemTime::now();
    let mut mono_problems = Vec::new();
    let mut update_mode_ids = UpdateModeIds::new();
    // do the thing
    let mut mono_env = roc_mono::ir::Env {
        arena,
        problems: &mut mono_problems,
        subs: &mut subs,
        home,
        ident_ids: &mut ident_ids,
        ptr_bytes,
        update_mode_ids: &mut update_mode_ids,
        // call_specialization_counter=0 is reserved
        call_specialization_counter: 1,
    };

    let mut procs = Procs::new_in(arena);

    for (symbol, partial_proc) in procs_base.partial_procs.into_iter() {
        procs.partial_procs.insert(symbol, partial_proc);
    }

    procs.module_thunks = procs_base.module_thunks;
    procs.runtime_errors = procs_base.runtime_errors;
    procs.imported_module_thunks = procs_base.imported_module_thunks;

    // TODO: for now this final specialization pass is sequential,
    // with no parallelization at all. We should try to parallelize
    // this, but doing so will require a redesign of Procs.
    procs = roc_mono::ir::specialize_all(
        &mut mono_env,
        procs,
        specializations_we_must_make,
        procs_base.host_specializations,
        &mut layout_cache,
    );

    let external_specializations_requested = procs.externals_we_need.clone();
    let procedures = procs.get_specialized_procs_without_rc(&mut mono_env);

    // Turn `Bytes.Decode.IdentId(238)` into `Bytes.Decode.238`, we rely on this in mono tests
    mono_env.home.register_debug_idents(mono_env.ident_ids);

    let make_specializations_end = SystemTime::now();
    module_timing.make_specializations = make_specializations_end
        .duration_since(make_specializations_start)
        .unwrap();

    Msg::MadeSpecializations {
        module_id: home,
        ident_ids,
        layout_cache,
        procedures,
        problems: mono_problems,
        update_mode_ids,
        subs,
        external_specializations_requested,
        module_timing,
    }
}

#[derive(Clone, Debug)]
struct ProcsBase<'a> {
    partial_procs: BumpMap<Symbol, PartialProc<'a>>,
    module_thunks: &'a [Symbol],
    /// A host-exposed function must be specialized; it's a seed for subsequent specializations
    host_specializations: roc_mono::ir::HostSpecializations,
    runtime_errors: BumpMap<Symbol, &'a str>,
    imported_module_thunks: &'a [Symbol],
}

#[allow(clippy::too_many_arguments)]
fn build_pending_specializations<'a>(
    arena: &'a Bump,
    solved_subs: Solved<Subs>,
    imported_module_thunks: &'a [Symbol],
    home: ModuleId,
    mut ident_ids: IdentIds,
    decls: Vec<Declaration>,
    mut module_timing: ModuleTiming,
    mut layout_cache: LayoutCache<'a>,
    ptr_bytes: u32,
    // TODO remove
    exposed_to_host: MutMap<Symbol, Variable>,
) -> Msg<'a> {
    let find_specializations_start = SystemTime::now();

    let mut module_thunks = bumpalo::collections::Vec::new_in(arena);

    let mut procs_base = ProcsBase {
        partial_procs: BumpMap::default(),
        module_thunks: &[],
        host_specializations: roc_mono::ir::HostSpecializations::new(),
        runtime_errors: BumpMap::default(),
        imported_module_thunks,
    };

    let mut mono_problems = std::vec::Vec::new();
    let mut update_mode_ids = UpdateModeIds::new();
    let mut subs = solved_subs.into_inner();
    let mut mono_env = roc_mono::ir::Env {
        arena,
        problems: &mut mono_problems,
        subs: &mut subs,
        home,
        ident_ids: &mut ident_ids,
        ptr_bytes,
        update_mode_ids: &mut update_mode_ids,
        // call_specialization_counter=0 is reserved
        call_specialization_counter: 1,
    };

    // Add modules' decls to Procs
    for decl in decls {
        use roc_can::def::Declaration::*;

        match decl {
            Declare(def) | Builtin(def) => add_def_to_module(
                &mut layout_cache,
                &mut procs_base,
                &mut module_thunks,
                &mut mono_env,
                def,
                &exposed_to_host,
                false,
            ),
            DeclareRec(defs) => {
                for def in defs {
                    add_def_to_module(
                        &mut layout_cache,
                        &mut procs_base,
                        &mut module_thunks,
                        &mut mono_env,
                        def,
                        &exposed_to_host,
                        true,
                    )
                }
            }
            InvalidCycle(_entries) => {
                // do nothing?
                // this may mean the loc_symbols are not defined during codegen; is that a problem?
            }
        }
    }

    procs_base.module_thunks = module_thunks.into_bump_slice();

    let problems = mono_env.problems.to_vec();

    let find_specializations_end = SystemTime::now();
    module_timing.find_specializations = find_specializations_end
        .duration_since(find_specializations_start)
        .unwrap();

    Msg::FoundSpecializations {
        module_id: home,
        solved_subs: roc_types::solved_types::Solved(subs),
        ident_ids,
        layout_cache,
        procs_base,
        problems,
        module_timing,
    }
}

fn add_def_to_module<'a>(
    layout_cache: &mut LayoutCache<'a>,
    procs: &mut ProcsBase<'a>,
    module_thunks: &mut bumpalo::collections::Vec<'a, Symbol>,
    mono_env: &mut roc_mono::ir::Env<'a, '_>,
    def: roc_can::def::Def,
    exposed_to_host: &MutMap<Symbol, Variable>,
    is_recursive: bool,
) {
    use roc_can::expr::ClosureData;
    use roc_can::expr::Expr::*;
    use roc_can::pattern::Pattern::*;

    match def.loc_pattern.value {
        Identifier(symbol) => {
            let is_host_exposed = exposed_to_host.contains_key(&symbol);

            match def.loc_expr.value {
                Closure(ClosureData {
                    function_type: annotation,
                    return_type: ret_var,
                    arguments: loc_args,
                    loc_body,
                    captured_symbols,
                    ..
                }) => {
                    // this is a top-level definition, it should not capture anything
                    debug_assert!(captured_symbols.is_empty());

                    // If this is an exposed symbol, we need to
                    // register it as such. Otherwise, since it
                    // never gets called by Roc code, it will never
                    // get specialized!
                    if is_host_exposed {
                        let layout_result =
                            layout_cache.raw_from_var(mono_env.arena, annotation, mono_env.subs);

                        // cannot specialize when e.g. main's type contains type variables
                        if let Err(e) = layout_result {
                            match e {
                                LayoutProblem::Erroneous => {
                                    let message = "top level function has erroneous type";
                                    procs.runtime_errors.insert(symbol, message);
                                    return;
                                }
                                LayoutProblem::UnresolvedTypeVar(v) => {
                                    let message = format!(
                                        "top level function has unresolved type variable {:?}",
                                        v
                                    );
                                    procs
                                        .runtime_errors
                                        .insert(symbol, mono_env.arena.alloc(message));
                                    return;
                                }
                            }
                        }

                        procs.host_specializations.insert_host_exposed(
                            mono_env.subs,
                            symbol,
                            def.annotation,
                            annotation,
                        );
                    }

                    let partial_proc = PartialProc::from_named_function(
                        mono_env,
                        layout_cache,
                        annotation,
                        loc_args,
                        *loc_body,
                        CapturedSymbols::None,
                        is_recursive,
                        ret_var,
                    );

                    procs.partial_procs.insert(symbol, partial_proc);
                }
                body => {
                    // mark this symbols as a top-level thunk before any other work on the procs
                    module_thunks.push(symbol);

                    let annotation = def.expr_var;

                    // If this is an exposed symbol, we need to
                    // register it as such. Otherwise, since it
                    // never gets called by Roc code, it will never
                    // get specialized!
                    if is_host_exposed {
                        let layout_result =
                            layout_cache.raw_from_var(mono_env.arena, annotation, mono_env.subs);

                        // cannot specialize when e.g. main's type contains type variables
                        if let Err(e) = layout_result {
                            match e {
                                LayoutProblem::Erroneous => {
                                    let message = "top level function has erroneous type";
                                    procs.runtime_errors.insert(symbol, message);
                                    return;
                                }
                                LayoutProblem::UnresolvedTypeVar(v) => {
                                    let message = format!(
                                        "top level function has unresolved type variable {:?}",
                                        v
                                    );
                                    procs
                                        .runtime_errors
                                        .insert(symbol, mono_env.arena.alloc(message));
                                    return;
                                }
                            }
                        }

                        procs.host_specializations.insert_host_exposed(
                            mono_env.subs,
                            symbol,
                            def.annotation,
                            annotation,
                        );
                    }

                    let proc = PartialProc {
                        annotation,
                        // This is a 0-arity thunk, so it has no arguments.
                        pattern_symbols: &[],
                        // This is a top-level definition, so it cannot capture anything
                        captured_symbols: CapturedSymbols::None,
                        body,
                        // This is a 0-arity thunk, so it cannot be recursive
                        is_self_recursive: false,
                    };

                    procs.partial_procs.insert(symbol, proc);
                }
            };
        }
        other => {
            todo!("TODO gracefully handle Declare({:?})", other);
        }
    }
}

fn run_task<'a, F>(
    task: BuildTask<'a>,
    arena: &'a Bump,
    src_dir: &Path,
    msg_tx: MsgSender<'a>,
    ptr_bytes: u32,
    look_up_builtins: F,
) -> Result<(), LoadingProblem<'a>>
where
    F: Fn(Symbol, &mut VarStore) -> Option<Def> + 'static + Send + Copy,
{
    use BuildTask::*;

    let msg = match task {
        LoadModule {
            module_name,
            module_ids,
            shorthands,
            ident_ids_by_module,
        } => load_module(
            arena,
            src_dir,
            module_name,
            module_ids,
            shorthands,
            ident_ids_by_module,
        )
        .map(|(_, msg)| msg),
        Parse { header } => parse(arena, header),
        CanonicalizeAndConstrain {
            parsed,
            module_ids,
            dep_idents,
            exposed_symbols,
            aliases,
        } => canonicalize_and_constrain(
            arena,
            &module_ids,
            dep_idents,
            exposed_symbols,
            aliases,
            parsed,
            look_up_builtins,
        ),
        Solve {
            module,
            module_timing,
            imported_symbols,
            constraint,
            var_store,
            ident_ids,
            declarations,
            dep_idents,
            unused_imports,
        } => Ok(run_solve(
            module,
            ident_ids,
            module_timing,
            imported_symbols,
            constraint,
            var_store,
            declarations,
            dep_idents,
            unused_imports,
        )),
        BuildPendingSpecializations {
            module_id,
            ident_ids,
            decls,
            module_timing,
            layout_cache,
            solved_subs,
            imported_module_thunks,
            exposed_to_host,
        } => Ok(build_pending_specializations(
            arena,
            solved_subs,
            imported_module_thunks,
            module_id,
            ident_ids,
            decls,
            module_timing,
            layout_cache,
            ptr_bytes,
            exposed_to_host,
        )),
        MakeSpecializations {
            module_id,
            ident_ids,
            subs,
            procs_base,
            layout_cache,
            specializations_we_must_make,
            module_timing,
        } => Ok(make_specializations(
            arena,
            module_id,
            ident_ids,
            subs,
            procs_base,
            layout_cache,
            specializations_we_must_make,
            module_timing,
            ptr_bytes,
        )),
    }?;

    msg_tx
        .send(msg)
        .map_err(|_| LoadingProblem::MsgChannelDied)?;

    Ok(())
}

fn to_file_problem_report(filename: &Path, error: io::ErrorKind) -> String {
    use roc_reporting::report::{Report, RocDocAllocator, Severity, DEFAULT_PALETTE};
    use ven_pretty::DocAllocator;

    let src_lines: Vec<&str> = Vec::new();

    let mut module_ids = ModuleIds::default();

    let module_id = module_ids.get_or_insert(&"find module name somehow?".into());

    let interns = Interns::default();

    // Report parsing and canonicalization problems
    let alloc = RocDocAllocator::new(&src_lines, module_id, &interns);

    let report = match error {
        io::ErrorKind::NotFound => {
            let doc = alloc.stack(vec![
                alloc.reflow(r"I am looking for this file, but it's not there:"),
                alloc
                    .parser_suggestion(filename.to_str().unwrap())
                    .indent(4),
                alloc.concat(vec![
                    alloc.reflow(r"Is the file supposed to be there? "),
                    alloc.reflow("Maybe there is a typo in the file name?"),
                ]),
            ]);

            Report {
                filename: "UNKNOWN.roc".into(),
                doc,
                title: "FILE NOT FOUND".to_string(),
                severity: Severity::RuntimeError,
            }
        }
        io::ErrorKind::PermissionDenied => {
            let doc = alloc.stack(vec![
                alloc.reflow(r"I don't have the required permissions to read this file:"),
                alloc
                    .parser_suggestion(filename.to_str().unwrap())
                    .indent(4),
                alloc.concat(vec![
                    alloc.reflow(r"Is it the right file? Maybe change its permissions?")
                ]),
            ]);

            Report {
                filename: "UNKNOWN.roc".into(),
                doc,
                title: "FILE PERMISSION DENIED".to_string(),
                severity: Severity::RuntimeError,
            }
        }
        _ => {
            let error = std::io::Error::from(error);
            let formatted = format!("{}", error);
            let doc = alloc.concat(vec![
                alloc.reflow(r"I tried to read this file, but ran into a "),
                alloc.text(formatted),
                alloc.reflow(r" problem."),
            ]);

            Report {
                filename: "UNKNOWN.roc".into(),
                doc,
                title: "FILE PROBLEM".to_string(),
                severity: Severity::RuntimeError,
            }
        }
    };

    let mut buf = String::new();
    let palette = DEFAULT_PALETTE;
    report.render_color_terminal(&mut buf, &alloc, &palette);

    buf
}

fn to_parse_problem_report<'a>(
    problem: ParseProblem<'a, SyntaxError<'a>>,
    mut module_ids: ModuleIds,
    all_ident_ids: MutMap<ModuleId, IdentIds>,
) -> String {
    use roc_reporting::report::{parse_problem, RocDocAllocator, DEFAULT_PALETTE};

    // TODO this is not in fact safe
    let src = unsafe { from_utf8_unchecked(problem.bytes) };
    let src_lines = src.lines().collect::<Vec<_>>();
    // let mut src_lines: Vec<&str> = problem.prefix.lines().collect();
    // src_lines.extend(src.lines().skip(1));

    let module_id = module_ids.get_or_insert(&"find module name somehow?".into());

    let interns = Interns {
        module_ids,
        all_ident_ids,
    };

    // Report parsing and canonicalization problems
    let alloc = RocDocAllocator::new(&src_lines, module_id, &interns);

    let starting_line = 0;

    let lines = LineInfo::new(src);

    let report = parse_problem(
        &alloc,
        &lines,
        problem.filename.clone(),
        starting_line,
        problem,
    );

    let mut buf = String::new();
    let palette = DEFAULT_PALETTE;

    report.render_color_terminal(&mut buf, &alloc, &palette);

    buf
}

fn to_missing_platform_report(module_id: ModuleId, other: PlatformPath) -> String {
    use roc_reporting::report::{Report, RocDocAllocator, Severity, DEFAULT_PALETTE};
    use ven_pretty::DocAllocator;
    use PlatformPath::*;

    // Report parsing and canonicalization problems
    let interns = Interns::default();
    let alloc = RocDocAllocator::new(&[], module_id, &interns);

    let report = {
        match other {
            Valid(_) => unreachable!(),
            NotSpecified => {
                let doc = alloc.stack(vec![
                    alloc.reflow("I could not find a platform based on your input file."),
                    alloc.reflow(r"Does the module header contain an entry that looks like this:"),
                    alloc
                        .parser_suggestion(" packages { pf: \"platform\" }")
                        .indent(4),
                    alloc.reflow("See also TODO."),
                ]);

                Report {
                    filename: "UNKNOWN.roc".into(),
                    doc,
                    title: "NO PLATFORM".to_string(),
                    severity: Severity::RuntimeError,
                }
            }
            RootIsInterface => {
                let doc = alloc.stack(vec![
                                alloc.reflow(r"The input file is a interface file, but only app modules can be ran."),
                                alloc.concat(vec![
                                    alloc.reflow(r"I will still parse and typecheck the input file and its dependencies, "),
                                    alloc.reflow(r"but won't output any executable."),
                                ])
                            ]);

                Report {
                    filename: "UNKNOWN.roc".into(),
                    doc,
                    title: "NO PLATFORM".to_string(),
                    severity: Severity::RuntimeError,
                }
            }
            RootIsPkgConfig => {
                let doc = alloc.stack(vec![
                                alloc.reflow(r"The input file is a package config file, but only app modules can be ran."),
                                alloc.concat(vec![
                                    alloc.reflow(r"I will still parse and typecheck the input file and its dependencies, "),
                                    alloc.reflow(r"but won't output any executable."),
                                ])
                            ]);

                Report {
                    filename: "UNKNOWN.roc".into(),
                    doc,
                    title: "NO PLATFORM".to_string(),
                    severity: Severity::RuntimeError,
                }
            }
        }
    };

    let palette = DEFAULT_PALETTE;
    let mut buf = String::new();
    report.render_color_terminal(&mut buf, &alloc, &palette);

    buf
}
