use crate::docs::ModuleDocumentation;
use bumpalo::Bump;
use crossbeam::channel::{bounded, Sender};
use crossbeam::deque::{Injector, Stealer, Worker};
use crossbeam::thread;
use parking_lot::Mutex;
use roc_builtins::std::{Mode, StdLib};
use roc_can::constraint::Constraint;
use roc_can::def::Declaration;
use roc_can::module::{canonicalize_module_defs, Module};
use roc_collections::all::{default_hasher, MutMap, MutSet, SendMap};
use roc_constrain::module::{
    constrain_imports, pre_constrain_imports, ConstrainableImports, Import,
};
use roc_constrain::module::{constrain_module, ExposedModuleTypes, SubsByModule};
use roc_module::ident::{Ident, Lowercase, ModuleName, TagName};
use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds, Symbol};
use roc_mono::ir::{
    CapturedSymbols, ExternalSpecializations, PartialProc, PendingSpecialization, Proc, Procs,
};
use roc_mono::layout::{Layout, LayoutCache};
use roc_parse::ast::{
    self, Attempting, ExposesEntry, ImportsEntry, PlatformHeader, TypeAnnotation, TypedIdent,
};
use roc_parse::module::module_defs;
use roc_parse::parser::{self, Fail, Parser};
use roc_region::all::{Located, Region};
use roc_solve::module::SolvedModule;
use roc_solve::solve;
use roc_types::solved_types::Solved;
use roc_types::subs::{Subs, VarStore, Variable};
use roc_types::types::{Alias, Type};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::iter;
use std::path::{Path, PathBuf};
use std::str::from_utf8_unchecked;
use std::sync::Arc;
use std::time::{Duration, SystemTime};

/// Filename extension for normal Roc modules
const ROC_FILE_EXTENSION: &str = "roc";

/// Roc-Config file name
const PKG_CONFIG_FILE_NAME: &str = "Pkg-Config";

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

#[derive(Default, Debug)]
struct Dependencies {
    waiting_for: MutMap<(ModuleId, Phase), MutSet<(ModuleId, Phase)>>,
    notifies: MutMap<(ModuleId, Phase), MutSet<(ModuleId, Phase)>>,
}

impl Dependencies {
    /// Add all the dependencies for a module, return (module, phase) pairs that can make progress
    pub fn add_module(
        &mut self,
        module_id: ModuleId,
        dependencies: &MutSet<ModuleId>,
        goal_phase: Phase,
    ) -> MutSet<(ModuleId, Phase)> {
        use Phase::*;

        for dep in dependencies.iter().copied() {
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

        let mut output = MutSet::default();

        // all the dependencies can be loaded
        for dep in dependencies {
            // TODO figure out how to "load" (because it doesn't exist on the file system) the Effect module
            if !format!("{:?}", dep).contains("Effect") {
                output.insert((*dep, LoadHeader));
            }
        }

        output
    }

    pub fn add_platform_module(
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

        let mut output = MutSet::default();

        // all the dependencies can be loaded
        for dep in dependencies {
            output.insert((*dep, Phase::LoadHeader));
        }

        output
    }

    /// Propagate a notification, return (module, phase) pairs that can make progress
    pub fn notify(&mut self, module_id: ModuleId, phase: Phase) -> MutSet<(ModuleId, Phase)> {
        let mut output = MutSet::default();

        let key = (module_id, phase);
        if let Some(to_notify) = self.notifies.get(&key) {
            for notify_key in to_notify {
                let mut is_empty = false;
                if let Some(waiting_for_pairs) = self.waiting_for.get_mut(&notify_key) {
                    waiting_for_pairs.remove(&key);
                    is_empty = waiting_for_pairs.is_empty();
                }

                if is_empty {
                    self.waiting_for.remove(notify_key);
                    output.insert(*notify_key);
                }
            }
        }

        self.notifies.remove(&key);

        output
    }

    /// A waits for B, and B will notify A when it completes the phase
    fn add_dependency(&mut self, a: ModuleId, b: ModuleId, phase: Phase) {
        self.add_dependency_help(a, b, phase, phase);
    }

    fn add_dependency_help(&mut self, a: ModuleId, b: ModuleId, phase_a: Phase, phase_b: Phase) {
        let key = (a, phase_a);
        let value = (b, phase_b);
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

        let key = (b, phase_b);
        let value = (a, phase_a);
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

        self.notifies.is_empty()
    }
}

/// Struct storing various intermediate stages by their ModuleId
#[derive(Debug, Default)]
struct ModuleCache<'a> {
    module_names: MutMap<ModuleId, ModuleName>,

    /// Phases
    headers: MutMap<ModuleId, ModuleHeader<'a>>,
    parsed: MutMap<ModuleId, ParsedModule<'a>>,
    aliases: MutMap<ModuleId, MutMap<Symbol, Alias>>,
    constrained: MutMap<ModuleId, ConstrainedModule>,
    typechecked: MutMap<ModuleId, TypeCheckedModule<'a>>,
    found_specializations: MutMap<ModuleId, FoundSpecializationsModule<'a>>,
    external_specializations_requested: MutMap<ModuleId, ExternalSpecializations>,

    /// Various information
    documentation: MutMap<ModuleId, ModuleDocumentation>,
    can_problems: MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    type_problems: MutMap<ModuleId, Vec<solve::TypeError>>,
    mono_problems: MutMap<ModuleId, Vec<roc_mono::ir::MonoProblem>>,

    sources: MutMap<ModuleId, (PathBuf, &'a str)>,
}

fn start_phase<'a>(module_id: ModuleId, phase: Phase, state: &mut State<'a>) -> BuildTask<'a> {
    // we blindly assume all dependencies are met
    match phase {
        Phase::LoadHeader => {
            let dep_name = state
                .module_cache
                .module_names
                .remove(&module_id)
                .expect("module id is present");

            BuildTask::LoadModule {
                module_name: dep_name,
                // Provide mutexes of ModuleIds and IdentIds by module,
                // so other modules can populate them as they load.
                module_ids: Arc::clone(&state.arc_modules),
                ident_ids_by_module: Arc::clone(&state.ident_ids_by_module),
                mode: state.stdlib.mode,
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
            let mut dep_idents: MutMap<ModuleId, IdentIds> = IdentIds::exposed_builtins(num_deps);

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
                    let idents = ident_ids_by_module.get(&dep_id).unwrap();

                    dep_idents.insert(*dep_id, idents.clone());
                }
            }

            // Clone the module_ids we'll need for canonicalization.
            // This should be small, and cloning it should be quick.
            // We release the lock as soon as we're done cloning, so we don't have
            // to lock the global module_ids while canonicalizing any given module.
            let module_ids = Arc::clone(&state.arc_modules);
            let module_ids = { (*module_ids).lock().clone() };

            let exposed_symbols = state
                .exposed_symbols_by_module
                .remove(&module_id)
                .expect("Could not find listener ID in exposed_symbols_by_module");

            let mut aliases = MutMap::default();

            for imported in parsed.imported_modules.iter() {
                match state.module_cache.aliases.get(imported) {
                    None => unreachable!(
                        "imported module {:?} did not register its aliases, so {:?} cannot use them",
                        imported,
                        parsed.module_id,
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
                mode: state.stdlib.mode,
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
                &state.stdlib,
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

            BuildTask::BuildPendingSpecializations {
                layout_cache,
                module_id,
                module_timing,
                solved_subs,
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
                procs,
                layout_cache,
            } = found_specializations;

            BuildTask::MakeSpecializations {
                module_id,
                ident_ids,
                subs,
                procs,
                layout_cache,
                specializations_we_must_make,
            }
        }
    }
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
    pub sources: MutMap<ModuleId, (PathBuf, Box<str>)>,
    pub timings: MutMap<ModuleId, ModuleTiming>,
    pub documentation: MutMap<ModuleId, ModuleDocumentation>,
}

#[derive(Debug)]
pub enum BuildProblem<'a> {
    FileNotFound(&'a Path),
}

#[derive(Debug)]
struct ModuleHeader<'a> {
    module_id: ModuleId,
    module_name: ModuleName,
    module_path: PathBuf,
    exposed_ident_ids: IdentIds,
    deps_by_name: MutMap<ModuleName, ModuleId>,
    imported_modules: MutSet<ModuleId>,
    exposes: Vec<Symbol>,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    src: &'a [u8],
    module_timing: ModuleTiming,
}

#[derive(Debug)]
struct ConstrainedModule {
    module: Module,
    declarations: Vec<Declaration>,
    imported_modules: MutSet<ModuleId>,
    constraint: Constraint,
    ident_ids: IdentIds,
    var_store: VarStore,
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
pub struct FoundSpecializationsModule<'a> {
    pub module_id: ModuleId,
    pub ident_ids: IdentIds,
    pub layout_cache: LayoutCache<'a>,
    pub procs: Procs<'a>,
    pub subs: Subs,
}

#[derive(Debug)]
pub struct MonomorphizedModule<'a> {
    pub module_id: ModuleId,
    pub interns: Interns,
    pub subs: Subs,
    pub can_problems: MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    pub type_problems: MutMap<ModuleId, Vec<solve::TypeError>>,
    pub mono_problems: MutMap<ModuleId, Vec<roc_mono::ir::MonoProblem>>,
    pub procedures: MutMap<(Symbol, Layout<'a>), Proc<'a>>,
    pub exposed_to_host: MutMap<Symbol, Variable>,
    pub sources: MutMap<ModuleId, (PathBuf, Box<str>)>,
    pub timings: MutMap<ModuleId, ModuleTiming>,
}

#[derive(Debug, Default)]
pub struct VariablySizedLayouts<'a> {
    rigids: MutMap<Lowercase, Layout<'a>>,
    aliases: MutMap<Symbol, Layout<'a>>,
}

#[derive(Debug)]
struct ParsedModule<'a> {
    module_id: ModuleId,
    module_name: ModuleName,
    module_path: PathBuf,
    src: &'a str,
    module_timing: ModuleTiming,
    deps_by_name: MutMap<ModuleName, ModuleId>,
    imported_modules: MutSet<ModuleId>,
    exposed_ident_ids: IdentIds,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    parsed_defs: &'a [Located<roc_parse::ast::Def<'a>>],
}

#[derive(Debug)]
enum Msg<'a> {
    Many(Vec<Msg<'a>>),
    Header(ModuleHeader<'a>),
    Parsed(ParsedModule<'a>),
    CanonicalizedAndConstrained {
        constrained_module: ConstrainedModule,
        canonicalization_problems: Vec<roc_problem::can::Problem>,
        module_docs: ModuleDocumentation,
    },
    MadeEffectModule {
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
        module_timing: ModuleTiming,
    },
    FinishedAllTypeChecking {
        solved_subs: Solved<Subs>,
        exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
        documentation: MutMap<ModuleId, ModuleDocumentation>,
    },
    FoundSpecializations {
        module_id: ModuleId,
        ident_ids: IdentIds,
        layout_cache: LayoutCache<'a>,
        procs: Procs<'a>,
        problems: Vec<roc_mono::ir::MonoProblem>,
        solved_subs: Solved<Subs>,
    },
    MadeSpecializations {
        module_id: ModuleId,
        ident_ids: IdentIds,
        layout_cache: LayoutCache<'a>,
        external_specializations_requested: MutMap<ModuleId, ExternalSpecializations>,
        procedures: MutMap<(Symbol, Layout<'a>), Proc<'a>>,
        problems: Vec<roc_mono::ir::MonoProblem>,
        subs: Subs,
    },

    /// The task is to only typecheck AND monomorphize modules
    /// all modules are now monomorphized, we are done
    FinishedAllSpecialization {
        subs: Subs,
        exposed_to_host: MutMap<Symbol, Variable>,
    },
}

#[derive(Debug)]
struct State<'a> {
    pub root_id: ModuleId,
    pub goal_phase: Phase,
    pub stdlib: StdLib,
    pub exposed_types: SubsByModule,

    pub headers_parsed: MutSet<ModuleId>,

    pub module_cache: ModuleCache<'a>,
    pub dependencies: Dependencies,
    pub procedures: MutMap<(Symbol, Layout<'a>), Proc<'a>>,
    pub exposed_to_host: MutMap<Symbol, Variable>,

    /// This is the "final" list of IdentIds, after canonicalization and constraint gen
    /// have completed for a given module.
    pub constrained_ident_ids: MutMap<ModuleId, IdentIds>,

    /// From now on, these will be used by multiple threads; time to make an Arc<Mutex<_>>!
    pub arc_modules: Arc<Mutex<ModuleIds>>,

    pub ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,

    /// All the dependent modules we've already begun loading -
    /// meaning we should never kick off another load_module on them!
    pub loading_started: MutSet<ModuleId>,

    pub declarations_by_id: MutMap<ModuleId, Vec<Declaration>>,

    pub exposed_symbols_by_module: MutMap<ModuleId, MutSet<Symbol>>,

    pub unsolved_modules: MutMap<ModuleId, UnsolvedModule<'a>>,

    /// These are the modules which need to add their pending specializations to
    /// the queue. Adding specializations to the queue can be done completely in
    /// parallel, and order doesn't matter, so as soon as a module has been solved,
    /// it gets an entry in here, and then immediately begins working on its
    /// pending specializations in the same thread.
    pub needs_specialization: MutSet<ModuleId>,

    pub all_pending_specializations: MutMap<Symbol, MutMap<Layout<'a>, PendingSpecialization>>,

    pub specializations_in_flight: u32,

    pub timings: MutMap<ModuleId, ModuleTiming>,

    // Each thread gets its own layout cache. When one "pending specializations"
    // pass completes, it returns its layout cache so another thread can use it.
    // We don't bother trying to union them all together to maximize cache hits,
    // since the unioning process could potentially take longer than the savings.
    // (Granted, this has not been attempted or measured!)
    pub layout_caches: std::vec::Vec<LayoutCache<'a>>,

    pub procs: Procs<'a>,
}

#[derive(Debug)]
struct UnsolvedModule<'a> {
    module: Module,
    src: &'a str,
    imported_modules: MutSet<ModuleId>,
    ident_ids: IdentIds,
    constraint: Constraint,
    var_store: VarStore,
    module_timing: ModuleTiming,
    declarations: Vec<Declaration>,
}

#[derive(Debug)]
pub struct ModuleTiming {
    pub read_roc_file: Duration,
    pub parse_header: Duration,
    pub parse_body: Duration,
    pub canonicalize: Duration,
    pub constrain: Duration,
    pub solve: Duration,
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
            start_time,
            end_time,
        } = self;

        end_time
            .duration_since(*start_time)
            .ok()
            .and_then(|t| {
                t.checked_sub(*solve).and_then(|t| {
                    t.checked_sub(*constrain).and_then(|t| {
                        t.checked_sub(*canonicalize).and_then(|t| {
                            t.checked_sub(*parse_body).and_then(|t| {
                                t.checked_sub(*parse_header)
                                    .and_then(|t| t.checked_sub(*read_roc_file))
                            })
                        })
                    })
                })
            })
            .unwrap_or_else(Duration::default)
    }
}

#[derive(Debug)]
#[allow(dead_code)]
enum BuildTask<'a> {
    LoadModule {
        module_name: ModuleName,
        module_ids: Arc<Mutex<ModuleIds>>,
        ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
        mode: Mode,
    },
    LoadPkgConfig {
        module_ids: Arc<Mutex<ModuleIds>>,
        ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
        mode: Mode,
    },
    Parse {
        header: ModuleHeader<'a>,
    },
    CanonicalizeAndConstrain {
        parsed: ParsedModule<'a>,
        module_ids: ModuleIds,
        dep_idents: MutMap<ModuleId, IdentIds>,
        mode: Mode,
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
    },
    BuildPendingSpecializations {
        module_timing: ModuleTiming,
        layout_cache: LayoutCache<'a>,
        solved_subs: Solved<Subs>,
        module_id: ModuleId,
        ident_ids: IdentIds,
        decls: Vec<Declaration>,
        exposed_to_host: MutMap<Symbol, Variable>,
    },
    MakeSpecializations {
        module_id: ModuleId,
        ident_ids: IdentIds,
        subs: Subs,
        procs: Procs<'a>,
        layout_cache: LayoutCache<'a>,
        specializations_we_must_make: ExternalSpecializations,
    },
}

enum WorkerMsg {
    Shutdown,
    TaskAdded,
}

#[derive(Debug)]
pub enum LoadingProblem {
    FileProblem {
        filename: PathBuf,
        error: io::ErrorKind,
    },
    ParsingFailed {
        filename: PathBuf,
        fail: Fail,
    },
    UnexpectedHeader(String),
    MsgChannelDied,
    ErrJoiningWorkerThreads,
    TriedToImportAppModule,
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
) -> Result<(), LoadingProblem> {
    injector.push(task);

    for listener in listeners {
        listener
            .send(WorkerMsg::TaskAdded)
            .map_err(|_| LoadingProblem::MsgChannelDied)?;
    }

    Ok(())
}

pub fn load_and_typecheck(
    arena: &Bump,
    filename: PathBuf,
    stdlib: StdLib,
    src_dir: &Path,
    exposed_types: SubsByModule,
) -> Result<LoadedModule, LoadingProblem> {
    use LoadResult::*;

    let load_start = LoadStart::from_path(arena, filename, stdlib.mode)?;

    match load(
        arena,
        load_start,
        stdlib,
        src_dir,
        exposed_types,
        Phase::SolveTypes,
    )? {
        Monomorphized(_) => unreachable!(""),
        TypeChecked(module) => Ok(module),
    }
}

pub fn load_and_monomorphize<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    stdlib: StdLib,
    src_dir: &Path,
    exposed_types: SubsByModule,
) -> Result<MonomorphizedModule<'a>, LoadingProblem> {
    use LoadResult::*;

    let load_start = LoadStart::from_path(arena, filename, stdlib.mode)?;

    match load(
        arena,
        load_start,
        stdlib,
        src_dir,
        exposed_types,
        Phase::MakeSpecializations,
    )? {
        Monomorphized(module) => Ok(module),
        TypeChecked(_) => unreachable!(""),
    }
}

pub fn load_and_monomorphize_from_str<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    src: &'a str,
    stdlib: StdLib,
    src_dir: &Path,
    exposed_types: SubsByModule,
) -> Result<MonomorphizedModule<'a>, LoadingProblem> {
    use LoadResult::*;

    let load_start = LoadStart::from_str(arena, filename, src, stdlib.mode)?;

    match load(
        arena,
        load_start,
        stdlib,
        src_dir,
        exposed_types,
        Phase::MakeSpecializations,
    )? {
        Monomorphized(module) => Ok(module),
        TypeChecked(_) => unreachable!(""),
    }
}

struct LoadStart<'a> {
    pub arc_modules: Arc<Mutex<ModuleIds>>,
    pub ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    pub root_id: ModuleId,
    pub root_msg: Msg<'a>,
}

impl<'a> LoadStart<'a> {
    pub fn from_path(
        arena: &'a Bump,
        filename: PathBuf,
        mode: Mode,
    ) -> Result<Self, LoadingProblem> {
        let arc_modules = Arc::new(Mutex::new(ModuleIds::default()));
        let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
        let ident_ids_by_module = Arc::new(Mutex::new(root_exposed_ident_ids));

        // Load the root module synchronously; we can't proceed until we have its id.
        let (root_id, root_msg) = {
            let root_start_time = SystemTime::now();

            load_filename(
                arena,
                filename,
                Arc::clone(&arc_modules),
                Arc::clone(&ident_ids_by_module),
                root_start_time,
                mode,
            )?
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
        mode: Mode,
    ) -> Result<Self, LoadingProblem> {
        let arc_modules = Arc::new(Mutex::new(ModuleIds::default()));
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
                mode,
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
///    (Since Roc doesn't allow cyclic dependencies, this ctypeot deadlock.)
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
///     of any requestss that were added in the course of completing other requests). Now
///     we have a map of specializations, and everything was assembled in parallel with
///     no unique specialization ever getting assembled twice (meanaing no wasted effort).
/// 12. Now that we have our final map of specializations, we can proceed to code gen!
///     As long as the specializations are stored in a per-ModuleId map, we can also
///     parallelize this code gen. (e.g. in dev builds, building separate LLVM modules
///     and then linking them together, and possibly caching them by the hash of their
///     specializations, so if none of their specializations changed, we don't even need
///     to rebuild the module and can link in the cached one directly.)
fn load<'a>(
    arena: &'a Bump,
    //filename: PathBuf,
    load_start: LoadStart<'a>,
    stdlib: StdLib,
    src_dir: &Path,
    exposed_types: SubsByModule,
    goal_phase: Phase,
) -> Result<LoadResult<'a>, LoadingProblem>
where
{
    let LoadStart {
        arc_modules,
        ident_ids_by_module,
        root_id,
        root_msg,
    } = load_start;

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
    let num_workers = num_cpus::get().max(2) - 1;

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

            let mut headers_parsed = MutSet::default();

            // We've already parsed the root's header. (But only its header, so far.)
            headers_parsed.insert(root_id);

            let mut loading_started = MutSet::default();

            // If the root module we're still processing happens to be an interface,
            // it's possible that something else will import it. That will
            // necessarily cause a cyclic import error, but in the meantime
            // we still shouldn't load it.
            loading_started.insert(root_id);

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
                thread_scope
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
                                    return;
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
                                        run_task(task, worker_arena, src_dir, msg_tx.clone())
                                            .expect("Msg channel closed unexpectedly.");
                                    }
                                }
                            }
                        }

                        // Needed to prevent a borrow checker error about this closure
                        // outliving its enclosing function.
                        drop(worker_msg_rx);
                    })
                    .unwrap();
            }

            let mut state = State {
                root_id,
                goal_phase,
                stdlib,
                module_cache: ModuleCache::default(),
                dependencies: Dependencies::default(),
                procedures: MutMap::default(),
                exposed_to_host: MutMap::default(),
                exposed_types,
                headers_parsed,
                loading_started,
                arc_modules,
                constrained_ident_ids: IdentIds::exposed_builtins(0),
                ident_ids_by_module,
                declarations_by_id: MutMap::default(),
                exposed_symbols_by_module: MutMap::default(),
                unsolved_modules: MutMap::default(),
                timings: MutMap::default(),
                needs_specialization: MutSet::default(),
                all_pending_specializations: MutMap::default(),
                specializations_in_flight: 0,
                layout_caches: std::vec::Vec::with_capacity(num_cpus::get()),
                procs: Procs::default(),
            };

            // We've now distributed one worker queue to each thread.
            // There should be no queues left to distribute!
            debug_assert!(worker_queues.is_empty());
            drop(worker_queues);

            // Grab a reference to these Senders outside the loop, so we can share
            // it across each iteration of the loop.
            let worker_listeners = worker_listeners.into_bump_slice();
            let msg_tx = msg_tx.clone();

            // The root module will have already queued up messages to process,
            // and processing those messages will in turn queue up more messages.
            for msg in msg_rx.iter() {
                match msg {
                    Msg::FinishedAllTypeChecking {
                        solved_subs,
                        exposed_vars_by_symbol,
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
                            exposed_vars_by_symbol,
                            documentation,
                        )));
                    }
                    Msg::FinishedAllSpecialization {
                        subs,
                        exposed_to_host,
                    } => {
                        // We're done! There should be no more messages pending.
                        debug_assert!(msg_rx.is_empty());

                        // Shut down all the worker threads.
                        for listener in worker_listeners {
                            listener
                                .send(WorkerMsg::Shutdown)
                                .map_err(|_| LoadingProblem::MsgChannelDied)?;
                        }

                        return Ok(LoadResult::Monomorphized(finish_specialization(
                            state,
                            subs,
                            exposed_to_host,
                        )));
                    }
                    msg => {
                        // This is where most of the main thread's work gets done.
                        // Everything up to this point has been setting up the threading
                        // system which lets this logic work efficiently.
                        state = update(
                            state,
                            msg,
                            msg_tx.clone(),
                            &injector,
                            worker_listeners,
                            arena,
                        )?;
                    }
                }
            }

            // The msg_rx receiver closed unexpectedly before we finished solving everything
            Err(LoadingProblem::MsgChannelDied)
        })
    }
    .unwrap()
}

fn update<'a>(
    mut state: State<'a>,
    msg: Msg<'a>,
    msg_tx: MsgSender<'a>,
    injector: &Injector<BuildTask<'a>>,
    worker_listeners: &'a [Sender<WorkerMsg>],
    arena: &'a Bump,
) -> Result<State<'a>, LoadingProblem> {
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
        Header(header) => {
            log!("loaded header for {:?}", header.module_id);
            let home = header.module_id;

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

            let work = state.dependencies.add_module(
                header.module_id,
                &header.imported_modules,
                state.goal_phase,
            );

            state.module_cache.headers.insert(header.module_id, header);

            for (module_id, phase) in work {
                let task = start_phase(module_id, phase, &mut state);

                enqueue_task(&injector, worker_listeners, task)?
            }

            let work = state.dependencies.notify(home, Phase::LoadHeader);

            for (module_id, phase) in work {
                let task = start_phase(module_id, phase, &mut state);

                enqueue_task(&injector, worker_listeners, task)?
            }

            Ok(state)
        }
        Parsed(parsed) => {
            state
                .module_cache
                .sources
                .insert(parsed.module_id, (parsed.module_path.clone(), parsed.src));

            let module_id = parsed.module_id;

            state.module_cache.parsed.insert(parsed.module_id, parsed);

            let work = state.dependencies.notify(module_id, Phase::Parse);

            for (module_id, phase) in work {
                let task = start_phase(module_id, phase, &mut state);

                enqueue_task(&injector, worker_listeners, task)?
            }

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

            let work = state
                .dependencies
                .notify(module_id, Phase::CanonicalizeAndConstrain);

            for (module_id, phase) in work {
                let task = start_phase(module_id, phase, &mut state);

                enqueue_task(&injector, worker_listeners, task)?
            }

            Ok(state)
        }
        MadeEffectModule {
            constrained_module,
            canonicalization_problems,
            module_docs,
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

            let mut work = state.dependencies.add_platform_module(
                module_id,
                &MutSet::default(),
                state.goal_phase,
            );

            work.extend(state.dependencies.notify(module_id, Phase::LoadHeader));

            work.extend(state.dependencies.notify(module_id, Phase::Parse));

            work.extend(
                state
                    .dependencies
                    .notify(module_id, Phase::CanonicalizeAndConstrain),
            );

            for (module_id, phase) in work {
                let task = start_phase(module_id, phase, &mut state);

                enqueue_task(&injector, worker_listeners, task)?
            }

            Ok(state)
        }
        SolvedTypes {
            module_id,
            ident_ids,
            solved_module,
            solved_subs,
            decls,
            mut module_timing,
        } => {
            log!("solved types for {:?}", module_id);
            module_timing.end_time = SystemTime::now();

            state
                .module_cache
                .type_problems
                .insert(module_id, solved_module.problems);

            let work = state.dependencies.notify(module_id, Phase::SolveTypes);

            if module_id == state.root_id {
                state
                    .exposed_to_host
                    .extend(solved_module.exposed_vars_by_symbol.iter().copied());
            }

            if module_id == state.root_id && state.goal_phase == Phase::SolveTypes {
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
                if module_id != state.root_id {
                    state.exposed_types.insert(
                        module_id,
                        ExposedModuleTypes::Valid(
                            solved_module.solved_types,
                            solved_module.aliases,
                        ),
                    );
                }

                if state.goal_phase > Phase::SolveTypes {
                    let layout_cache = state.layout_caches.pop().unwrap_or_default();

                    let typechecked = TypeCheckedModule {
                        module_id,
                        decls,
                        solved_subs,
                        ident_ids,
                        module_timing,
                        layout_cache,
                    };

                    state
                        .module_cache
                        .typechecked
                        .insert(module_id, typechecked);
                } else {
                    state.constrained_ident_ids.insert(module_id, ident_ids);
                }

                for (module_id, phase) in work {
                    let task = start_phase(module_id, phase, &mut state);

                    enqueue_task(&injector, worker_listeners, task)?
                }
            }

            Ok(state)
        }
        FoundSpecializations {
            module_id,
            procs,
            solved_subs,
            ident_ids,
            layout_cache,
            problems: _,
        } => {
            log!("found specializations for {:?}", module_id);
            let subs = solved_subs.into_inner();

            if let Some(pending) = &procs.pending_specializations {
                for (symbol, specs) in pending {
                    let existing = match state.all_pending_specializations.entry(*symbol) {
                        Vacant(entry) => entry.insert(MutMap::default()),
                        Occupied(entry) => entry.into_mut(),
                    };

                    for (layout, pend) in specs {
                        existing.insert(layout.clone(), pend.clone());
                    }
                }
            }

            let found_specializations_module = FoundSpecializationsModule {
                layout_cache,
                module_id,
                procs,
                ident_ids,
                subs,
            };

            state
                .module_cache
                .found_specializations
                .insert(module_id, found_specializations_module);

            let work = state
                .dependencies
                .notify(module_id, Phase::FindSpecializations);

            for (module_id, phase) in work {
                let task = start_phase(module_id, phase, &mut state);

                enqueue_task(&injector, worker_listeners, task)?
            }
            Ok(state)
        }
        MadeSpecializations {
            module_id,
            ident_ids,
            subs,
            procedures,
            external_specializations_requested,
            problems,
            ..
        } => {
            log!("made specializations for {:?}", module_id);

            state.module_cache.mono_problems.insert(module_id, problems);

            for (module_id, requested) in external_specializations_requested {
                let existing = match state
                    .module_cache
                    .external_specializations_requested
                    .entry(module_id)
                {
                    Vacant(entry) => entry.insert(ExternalSpecializations::default()),
                    Occupied(entry) => entry.into_mut(),
                };

                existing.extend(requested);
            }

            state.procedures.extend(procedures);

            let work = state
                .dependencies
                .notify(module_id, Phase::MakeSpecializations);

            state.constrained_ident_ids.insert(module_id, ident_ids);

            if work.is_empty()
                && state.dependencies.solved_all()
                && state.goal_phase == Phase::MakeSpecializations
            {
                // state.timings.insert(module_id, module_timing);

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
                for (module_id, phase) in work {
                    let task = start_phase(module_id, phase, &mut state);

                    enqueue_task(&injector, worker_listeners, task)?
                }
            }

            Ok(state)
        }
        Msg::FinishedAllTypeChecking { .. } => {
            unreachable!();
        }
        Msg::FinishedAllSpecialization { .. } => {
            unreachable!();
        }
    }
}

fn finish_specialization<'a>(
    state: State<'a>,
    subs: Subs,
    exposed_to_host: MutMap<Symbol, Variable>,
) -> MonomorphizedModule<'a> {
    let module_ids = Arc::try_unwrap(state.arc_modules)
        .unwrap_or_else(|_| panic!("There were still outstanding Arc references to module_ids"))
        .into_inner();

    let interns = Interns {
        module_ids,
        all_ident_ids: state.constrained_ident_ids,
    };

    let State {
        procedures,
        module_cache,
        ..
    } = state;

    let ModuleCache {
        mono_problems,
        type_problems,
        can_problems,
        sources,
        ..
    } = module_cache;

    let sources = sources
        .into_iter()
        .map(|(id, (path, src))| (id, (path, src.into())))
        .collect();

    MonomorphizedModule {
        can_problems,
        mono_problems,
        type_problems,
        exposed_to_host,
        module_id: state.root_id,
        subs,
        interns,
        procedures,
        sources,
        timings: state.timings,
    }
}

fn finish<'a>(
    state: State<'a>,
    solved: Solved<Subs>,
    exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    documentation: MutMap<ModuleId, ModuleDocumentation>,
) -> LoadedModule {
    let module_ids = Arc::try_unwrap(state.arc_modules)
        .unwrap_or_else(|_| panic!("There were still outstanding Arc references to module_ids"))
        .into_inner();

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

    LoadedModule {
        module_id: state.root_id,
        interns,
        solved,
        can_problems: state.module_cache.can_problems,
        type_problems: state.module_cache.type_problems,
        declarations_by_id: state.declarations_by_id,
        exposed_to_host: exposed_vars_by_symbol.into_iter().collect(),
        sources,
        timings: state.timings,
        documentation,
    }
}

/// Load a PkgConfig.roc file
fn load_pkg_config<'a>(
    arena: &'a Bump,
    src_dir: &Path,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    mode: Mode,
) -> Result<Msg<'a>, LoadingProblem> {
    let module_start_time = SystemTime::now();

    let mut filename = PathBuf::from(src_dir);

    filename.push("platform");
    filename.push(PKG_CONFIG_FILE_NAME);

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    let file_io_start = SystemTime::now();
    let file = fs::read(&filename);
    let file_io_duration = file_io_start.elapsed().unwrap();

    match file {
        Ok(bytes) => {
            let parse_start = SystemTime::now();
            let parse_state = parser::State::new(arena.alloc(bytes), Attempting::Module);
            let parsed = roc_parse::module::header().parse(&arena, parse_state);
            let parse_header_duration = parse_start.elapsed().unwrap();

            // Insert the first entries for this module's timings
            let mut module_timing = ModuleTiming::new(module_start_time);

            module_timing.read_roc_file = file_io_duration;
            module_timing.parse_header = parse_header_duration;

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
                Ok((ast::Module::Platform { header }, _parse_state)) => fabricate_effects_module(
                    arena,
                    module_ids,
                    ident_ids_by_module,
                    mode,
                    header,
                    module_timing,
                )
                .map(|x| x.1),
                Err((fail, _)) => Err(LoadingProblem::ParsingFailed { filename, fail }),
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
    module_name: ModuleName,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    mode: Mode,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem> {
    let module_start_time = SystemTime::now();
    let mut filename = PathBuf::new();

    filename.push(src_dir);

    // Convert dots in module name to directories
    for part in module_name.as_str().split(MODULE_SEPARATOR) {
        filename.push(part);
    }

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    load_filename(
        arena,
        filename,
        module_ids,
        ident_ids_by_module,
        module_start_time,
        mode,
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
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    mode: Mode,
    src_bytes: &'a [u8],
    start_time: SystemTime,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem> {
    let parse_start = SystemTime::now();
    let parse_state = parser::State::new(src_bytes, Attempting::Module);
    let parsed = roc_parse::module::header().parse(&arena, parse_state);
    let parse_header_duration = parse_start.elapsed().unwrap();

    // Insert the first entries for this module's timings
    let mut module_timing = ModuleTiming::new(start_time);

    module_timing.read_roc_file = read_file_duration;
    module_timing.parse_header = parse_header_duration;

    match parsed {
        Ok((ast::Module::Interface { header }, parse_state)) => Ok(send_header(
            header.name,
            filename,
            header.exposes.into_bump_slice(),
            header.imports.into_bump_slice(),
            parse_state,
            module_ids,
            ident_ids_by_module,
            module_timing,
        )),
        Ok((ast::Module::App { header }, parse_state)) => {
            let mut pkg_config_dir = filename.clone();
            pkg_config_dir.pop();

            let (module_id, app_module_header_msg) = send_header(
                header.name,
                filename,
                header.provides.into_bump_slice(),
                header.imports.into_bump_slice(),
                parse_state,
                module_ids.clone(),
                ident_ids_by_module.clone(),
                module_timing,
            );

            // check whether we can find a Pkg-Config.roc file
            let mut pkg_config_roc = pkg_config_dir.clone();
            pkg_config_roc.push("platform");
            pkg_config_roc.push(PKG_CONFIG_FILE_NAME);
            pkg_config_roc.set_extension(ROC_FILE_EXTENSION);

            if pkg_config_roc.as_path().exists() {
                let load_pkg_config_msg = load_pkg_config(
                    arena,
                    &pkg_config_dir,
                    module_ids,
                    ident_ids_by_module,
                    mode,
                )?;

                Ok((
                    module_id,
                    Msg::Many(vec![app_module_header_msg, load_pkg_config_msg]),
                ))
            } else {
                Ok((module_id, app_module_header_msg))
            }
        }
        Ok((ast::Module::Platform { header }, _parse_state)) => fabricate_effects_module(
            arena,
            module_ids,
            ident_ids_by_module,
            mode,
            header,
            module_timing,
        ),
        Err((fail, _)) => Err(LoadingProblem::ParsingFailed { filename, fail }),
    }
}

/// Load a module by its filename
fn load_filename<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    module_start_time: SystemTime,
    mode: Mode,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem> {
    let file_io_start = SystemTime::now();
    let file = fs::read(&filename);
    let file_io_duration = file_io_start.elapsed().unwrap();

    match file {
        Ok(bytes) => parse_header(
            arena,
            file_io_duration,
            filename,
            module_ids,
            ident_ids_by_module,
            mode,
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
fn load_from_str<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    src: &'a str,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    module_start_time: SystemTime,
    mode: Mode,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem> {
    let file_io_start = SystemTime::now();
    let file_io_duration = file_io_start.elapsed().unwrap();

    parse_header(
        arena,
        file_io_duration,
        filename,
        module_ids,
        ident_ids_by_module,
        mode,
        src.as_bytes(),
        module_start_time,
    )
}

#[allow(clippy::too_many_arguments)]
fn send_header<'a>(
    name: Located<roc_parse::header::ModuleName<'a>>,
    filename: PathBuf,
    exposes: &'a [Located<ExposesEntry<'a>>],
    imports: &'a [Located<ImportsEntry<'a>>],
    parse_state: parser::State<'a>,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    module_timing: ModuleTiming,
) -> (ModuleId, Msg<'a>) {
    let declared_name: ModuleName = name.value.as_str().into();

    // TODO check to see if declared_name is consistent with filename.
    // If it isn't, report a problem!

    let mut imported: Vec<(ModuleName, Vec<Ident>, Region)> = Vec::with_capacity(imports.len());
    let mut imported_modules: MutSet<ModuleId> = MutSet::default();
    let mut scope_size = 0;

    for loc_entry in imports {
        let (module_name, exposed) = exposed_from_import(&loc_entry.value);

        scope_size += exposed.len();

        imported.push((module_name, exposed, loc_entry.region));
    }

    let num_exposes = exposes.len();
    let mut deps_by_name: MutMap<ModuleName, ModuleId> =
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

        home = module_ids.get_or_insert(&declared_name.as_inline_str());

        // Ensure this module has an entry in the exposed_ident_ids map.
        ident_ids_by_module
            .entry(home)
            .or_insert_with(IdentIds::default);

        // For each of our imports, add an entry to deps_by_name
        //
        // e.g. for `imports [ Foo.{ bar } ]`, add `Foo` to deps_by_name
        //
        // Also build a list of imported_values_to_expose (like `bar` above.)
        for (module_name, exposed_idents, region) in imported.into_iter() {
            let cloned_module_name = module_name.clone();
            let module_id = module_ids.get_or_insert(&module_name.into());

            deps_by_name.insert(cloned_module_name, module_id);

            imported_modules.insert(module_id);

            // Add the new exposed idents to the dep module's IdentIds, so
            // once that module later gets loaded, its lookups will resolve
            // to the same symbols as the ones we're using here.
            let ident_ids = ident_ids_by_module
                .entry(module_id)
                .or_insert_with(IdentIds::default);

            for ident in exposed_idents {
                let ident_id = ident_ids.get_or_insert(ident.as_inline_str());
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
            home.register_debug_idents(&ident_ids);
        }

        ident_ids.clone()
    };

    // Send the deps to the coordinator thread for processing,
    // then continue on to parsing and canonicalizing defs.
    //
    // We always need to send these, even if deps is empty,
    // because the coordinator thread needs to receive this message
    // to decrement its "pending" count.

    // Send the header the main thread for processing,
    (
        home,
        Msg::Header(ModuleHeader {
            module_id: home,
            module_path: filename,
            exposed_ident_ids: ident_ids,
            module_name: declared_name,
            imported_modules,
            deps_by_name,
            exposes: exposed,
            src: parse_state.bytes,
            exposed_imports: scope,
            module_timing,
        }),
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
        imported_modules: MutSet<ModuleId>,
        exposed_types: &mut SubsByModule,
        stdlib: &StdLib,
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

        if !unused_imports.is_empty() {
            todo!(
                "TODO gracefully handle unused import {:?} from module {:?}",
                &unused_imports,
                home,
            );
        }

        // Next, solve this module in the background.
        Self::Solve {
            module,
            ident_ids,
            imported_symbols,
            constraint,
            var_store,
            declarations,
            module_timing,
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
) -> Msg<'a> {
    // We have more constraining work to do now, so we'll add it to our timings.
    let constrain_start = SystemTime::now();

    // Finish constraining the module by wrapping the existing Constraint
    // in the ones we just computed. We can do this off the main thread.
    let constraint = constrain_imports(imported_symbols, constraint, &mut var_store);

    let constrain_end = SystemTime::now();

    let module_id = module.module_id;

    let Module {
        exposed_vars_by_symbol,
        aliases,
        rigid_variables,
        ..
    } = module;

    let (solved_subs, solved_env, problems) =
        roc_solve::module::run_solve(aliases, rigid_variables, constraint, var_store);

    let solved_types =
        roc_solve::module::make_solved_types(&solved_env, &solved_subs, &exposed_vars_by_symbol);

    let solved_module = SolvedModule {
        exposed_vars_by_symbol,
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
        solved_module,
        module_timing,
    }
}

fn fabricate_effect_after(
    env: &mut roc_can::env::Env,
    scope: &mut roc_can::scope::Scope,
    effect_symbol: Symbol,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
) -> (Symbol, roc_can::def::Def) {
    use roc_can::expr::Expr;
    use roc_can::expr::Recursive;
    use roc_module::operator::CalledVia;

    let thunk_symbol = {
        scope
            .introduce(
                "thunk".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let to_effect_symbol = {
        scope
            .introduce(
                "toEffect".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    let after_symbol = {
        scope
            .introduce(
                "after".into(),
                &env.exposed_ident_ids,
                &mut env.ident_ids,
                Region::zero(),
            )
            .unwrap()
    };

    // `thunk {}`
    let force_thunk_call = {
        let boxed = (
            var_store.fresh(),
            Located::at_zero(Expr::Var(thunk_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Located::at_zero(Expr::EmptyRecord))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    // `toEffect (thunk {})`
    let to_effect_call = {
        let boxed = (
            var_store.fresh(),
            Located::at_zero(Expr::Var(to_effect_symbol)),
            var_store.fresh(),
            var_store.fresh(),
        );

        let arguments = vec![(var_store.fresh(), Located::at_zero(force_thunk_call))];
        Expr::Call(Box::new(boxed), arguments, CalledVia::Space)
    };

    use roc_can::pattern::Pattern;
    let arguments = vec![
        (
            var_store.fresh(),
            Located::at_zero(Pattern::AppliedTag {
                whole_var: var_store.fresh(),
                ext_var: var_store.fresh(),
                tag_name: effect_tag_name.clone(),
                arguments: vec![(
                    var_store.fresh(),
                    Located::at_zero(Pattern::Identifier(thunk_symbol)),
                )],
            }),
        ),
        (
            var_store.fresh(),
            Located::at_zero(Pattern::Identifier(to_effect_symbol)),
        ),
    ];

    let function_var = var_store.fresh();
    let after_closure = Expr::Closure {
        function_type: function_var,
        closure_type: var_store.fresh(),
        closure_ext_var: var_store.fresh(),
        return_type: var_store.fresh(),
        name: after_symbol,
        captured_symbols: Vec::new(),
        recursive: Recursive::NotRecursive,
        arguments,
        loc_body: Box::new(Located::at_zero(to_effect_call)),
    };

    use roc_can::annotation::IntroducedVariables;

    let mut introduced_variables = IntroducedVariables::default();

    let signature = {
        let var_a = var_store.fresh();
        let var_b = var_store.fresh();

        introduced_variables.insert_named("a".into(), var_a);
        introduced_variables.insert_named("b".into(), var_b);

        let effect_a = {
            let actual =
                build_effect_actual(effect_tag_name.clone(), Type::Variable(var_a), var_store);

            Type::Alias(
                effect_symbol,
                vec![("a".into(), Type::Variable(var_a))],
                Box::new(actual),
            )
        };

        let effect_b = {
            let actual = build_effect_actual(effect_tag_name, Type::Variable(var_b), var_store);

            Type::Alias(
                effect_symbol,
                vec![("b".into(), Type::Variable(var_b))],
                Box::new(actual),
            )
        };

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(closure_var);
        let a_to_effect_b = Type::Function(
            vec![Type::Variable(var_a)],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_b.clone()),
        );

        let closure_var = var_store.fresh();
        introduced_variables.insert_wildcard(closure_var);
        Type::Function(
            vec![effect_a, a_to_effect_b],
            Box::new(Type::Variable(closure_var)),
            Box::new(effect_b),
        )
    };

    let def_annotation = roc_can::def::Annotation {
        signature,
        introduced_variables,
        aliases: SendMap::default(),
        region: Region::zero(),
    };

    let pattern = Pattern::Identifier(after_symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(after_symbol, function_var);
    let def = roc_can::def::Def {
        loc_pattern: Located::at_zero(pattern),
        loc_expr: Located::at_zero(after_closure),
        expr_var: function_var,
        pattern_vars,
        annotation: Some(def_annotation),
    };

    (after_symbol, def)
}

fn fabricate_host_exposed_def(
    env: &mut roc_can::env::Env,
    scope: &mut roc_can::scope::Scope,
    symbol: Symbol,
    ident: &str,
    effect_tag_name: TagName,
    var_store: &mut VarStore,
    annotation: roc_can::annotation::Annotation,
) -> roc_can::def::Def {
    use roc_can::pattern::Pattern;
    let expr_var = var_store.fresh();
    let pattern = Pattern::Identifier(symbol);
    let mut pattern_vars = SendMap::default();
    pattern_vars.insert(symbol, expr_var);

    use roc_can::expr::Expr;
    use roc_can::expr::Recursive;

    let mut arguments: Vec<(Variable, Located<Pattern>)> = Vec::new();
    let mut linked_symbol_arguments: Vec<(Variable, Expr)> = Vec::new();
    let mut captured_symbols: Vec<(Symbol, Variable)> = Vec::new();

    let def_body = {
        match annotation.typ.shallow_dealias() {
            Type::Function(args, _, _) => {
                for i in 0..args.len() {
                    let name = format!("closure_arg_{}_{}", ident, i);

                    let arg_symbol = {
                        let ident = name.clone().into();
                        scope
                            .introduce(
                                ident,
                                &env.exposed_ident_ids,
                                &mut env.ident_ids,
                                Region::zero(),
                            )
                            .unwrap()
                    };

                    let arg_var = var_store.fresh();

                    arguments.push((arg_var, Located::at_zero(Pattern::Identifier(arg_symbol))));

                    captured_symbols.push((arg_symbol, arg_var));
                    linked_symbol_arguments.push((arg_var, Expr::Var(arg_symbol)));
                }

                let foreign_symbol_name = format!("roc_fx_{}", ident);
                let low_level_call = Expr::ForeignCall {
                    foreign_symbol: foreign_symbol_name.into(),
                    args: linked_symbol_arguments,
                    ret_var: var_store.fresh(),
                };

                let effect_closure_symbol = {
                    let name = format!("effect_closure_{}", ident);

                    let ident = name.into();
                    scope
                        .introduce(
                            ident,
                            &env.exposed_ident_ids,
                            &mut env.ident_ids,
                            Region::zero(),
                        )
                        .unwrap()
                };

                let empty_record_pattern = Pattern::RecordDestructure {
                    whole_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    destructs: vec![],
                };

                let effect_closure = Expr::Closure {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    closure_ext_var: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: effect_closure_symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: vec![(var_store.fresh(), Located::at_zero(empty_record_pattern))],
                    loc_body: Box::new(Located::at_zero(low_level_call)),
                };

                let body = Expr::Tag {
                    variant_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    name: effect_tag_name,
                    arguments: vec![(var_store.fresh(), Located::at_zero(effect_closure))],
                };

                Expr::Closure {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    closure_ext_var: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: symbol,
                    captured_symbols: std::vec::Vec::new(),
                    recursive: Recursive::NotRecursive,
                    arguments,
                    loc_body: Box::new(Located::at_zero(body)),
                }
            }
            _ => {
                // not a function

                let foreign_symbol_name = format!("roc_fx_{}", ident);
                let low_level_call = Expr::ForeignCall {
                    foreign_symbol: foreign_symbol_name.into(),
                    args: linked_symbol_arguments,
                    ret_var: var_store.fresh(),
                };

                let effect_closure_symbol = {
                    let name = format!("effect_closure_{}", ident);

                    let ident = name.into();
                    scope
                        .introduce(
                            ident,
                            &env.exposed_ident_ids,
                            &mut env.ident_ids,
                            Region::zero(),
                        )
                        .unwrap()
                };

                let empty_record_pattern = Pattern::RecordDestructure {
                    whole_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    destructs: vec![],
                };

                let effect_closure = Expr::Closure {
                    function_type: var_store.fresh(),
                    closure_type: var_store.fresh(),
                    closure_ext_var: var_store.fresh(),
                    return_type: var_store.fresh(),
                    name: effect_closure_symbol,
                    captured_symbols,
                    recursive: Recursive::NotRecursive,
                    arguments: vec![(var_store.fresh(), Located::at_zero(empty_record_pattern))],
                    loc_body: Box::new(Located::at_zero(low_level_call)),
                };
                Expr::Tag {
                    variant_var: var_store.fresh(),
                    ext_var: var_store.fresh(),
                    name: effect_tag_name,
                    arguments: vec![(var_store.fresh(), Located::at_zero(effect_closure))],
                }
            }
        }
    };

    let def_annotation = roc_can::def::Annotation {
        signature: annotation.typ,
        introduced_variables: annotation.introduced_variables,
        aliases: annotation.aliases,
        region: Region::zero(),
    };

    roc_can::def::Def {
        loc_pattern: Located::at_zero(pattern),
        loc_expr: Located::at_zero(def_body),
        expr_var,
        pattern_vars,
        annotation: Some(def_annotation),
    }
}

fn build_effect_actual(effect_tag_name: TagName, a_type: Type, var_store: &mut VarStore) -> Type {
    let closure_var = var_store.fresh();

    Type::TagUnion(
        vec![(
            effect_tag_name,
            vec![Type::Function(
                vec![Type::EmptyRec],
                Box::new(Type::Variable(closure_var)),
                Box::new(a_type),
            )],
        )],
        Box::new(Type::EmptyTagUnion),
    )
}

fn unpack_exposes_entries<'a>(
    arena: &'a Bump,
    entries: &'a [Located<TypedIdent<'a>>],
) -> bumpalo::collections::Vec<'a, (&'a Located<&'a str>, &'a Located<TypeAnnotation<'a>>)> {
    use bumpalo::collections::Vec;

    let mut stack: Vec<&TypedIdent> = Vec::with_capacity_in(entries.len(), arena);
    let mut output = Vec::with_capacity_in(entries.len(), arena);

    for entry in entries.iter() {
        stack.push(&entry.value);
    }

    while let Some(effects_entry) = stack.pop() {
        match effects_entry {
            TypedIdent::Entry {
                ident,
                spaces_before_colon: _,
                ann,
            } => {
                output.push((ident, ann));
            }
            TypedIdent::SpaceAfter(nested, _) | TypedIdent::SpaceBefore(nested, _) => {
                stack.push(nested);
            }
        }
    }

    output
}

fn fabricate_effects_module<'a>(
    arena: &'a Bump,
    module_ids: Arc<Mutex<ModuleIds>>,
    ident_ids_by_module: Arc<Mutex<MutMap<ModuleId, IdentIds>>>,
    mode: Mode,
    header: PlatformHeader<'a>,
    module_timing: ModuleTiming,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem> {
    let num_exposes = header.provides.len() + 1;
    let mut exposed: Vec<Symbol> = Vec::with_capacity(num_exposes);

    let module_id: ModuleId;

    let PlatformHeader { effects, .. } = header;
    let effect_entries = unpack_exposes_entries(arena, &effects.entries);
    let name = effects.type_name;
    let declared_name: ModuleName = name.into();

    let hardcoded_exposed_functions = vec![name, "after"];

    let exposed_ident_ids = {
        // Lock just long enough to perform the minimal operations necessary.
        let mut module_ids = (*module_ids).lock();
        let mut ident_ids_by_module = (*ident_ids_by_module).lock();

        module_id = module_ids.get_or_insert(&declared_name.as_inline_str());

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

        for hardcoded in hardcoded_exposed_functions {
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
            module_id.register_debug_idents(&ident_ids);
        }

        ident_ids.clone()
    };

    // a platform module has no dependencies, hence empty
    let dep_idents: MutMap<ModuleId, IdentIds> = IdentIds::exposed_builtins(0);

    let mut var_store = VarStore::default();

    let module_ids = { (*module_ids).lock().clone() };

    let mut scope = roc_can::scope::Scope::new(module_id, &mut var_store);
    let mut can_env = roc_can::env::Env::new(module_id, dep_idents, &module_ids, exposed_ident_ids);

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

        let actual = build_effect_actual(
            effect_tag_name.clone(),
            Type::Variable(a_var),
            &mut var_store,
        );

        scope.add_alias(
            effect_symbol,
            Region::zero(),
            vec![Located::at_zero(("a".into(), a_var))],
            actual,
        );

        scope.lookup_alias(effect_symbol).unwrap().clone()
    };

    aliases.insert(effect_symbol, alias);

    let mut declarations = Vec::new();

    let exposed_vars_by_symbol = {
        let mut exposed_vars_by_symbol = Vec::new();

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

                let def = fabricate_host_exposed_def(
                    &mut can_env,
                    &mut scope,
                    symbol,
                    ident.value,
                    TagName::Private(effect_symbol),
                    &mut var_store,
                    annotation,
                );

                exposed_vars_by_symbol.push((symbol, def.expr_var));

                declarations.push(Declaration::Declare(def));
            }
        }

        let (effect_after_symbol, def) = fabricate_effect_after(
            &mut can_env,
            &mut scope,
            effect_symbol,
            effect_tag_name,
            &mut var_store,
        );
        exposed_vars_by_symbol.push((effect_after_symbol, def.expr_var));
        declarations.push(Declaration::Declare(def));

        exposed_vars_by_symbol
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
        exposed_vars_by_symbol,
        references: MutSet::default(),
    };

    let constraint = constrain_module(&module_output, module_id, mode, &mut var_store);

    let module = Module {
        module_id,
        exposed_imports: module_output.exposed_imports,
        exposed_vars_by_symbol: module_output.exposed_vars_by_symbol,
        references: module_output.references,
        aliases: module_output.aliases,
        rigid_variables: module_output.rigid_variables,
    };

    let imported_modules = MutSet::default();

    // Should a effect module ever have a ModuleDocumentation?
    let module_docs = ModuleDocumentation {
        name: String::from(name),
        docs: String::from("idk fix this later"),
        entries: Vec::new(),
    };

    let constrained_module = ConstrainedModule {
        module,
        declarations: module_output.declarations,
        imported_modules,
        var_store,
        constraint,
        ident_ids: module_output.ident_ids,
        module_timing,
    };

    Ok((
        module_id,
        Msg::MadeEffectModule {
            constrained_module,
            canonicalization_problems: module_output.problems,
            module_docs,
        },
    ))
}

fn canonicalize_and_constrain<'a>(
    arena: &'a Bump,
    module_ids: &ModuleIds,
    dep_idents: MutMap<ModuleId, IdentIds>,
    exposed_symbols: MutSet<Symbol>,
    aliases: MutMap<Symbol, Alias>,
    mode: Mode,
    parsed: ParsedModule<'a>,
) -> Result<Msg<'a>, LoadingProblem> {
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

    // Generate documentation information
    // TODO: store timing information?
    let module_docs =
        crate::docs::generate_module_docs(module_name, &exposed_ident_ids, &parsed_defs);

    let mut var_store = VarStore::default();
    let canonicalized = canonicalize_module_defs(
        &arena,
        parsed_defs,
        module_id,
        module_ids,
        exposed_ident_ids,
        dep_idents,
        aliases,
        exposed_imports,
        exposed_symbols,
        &mut var_store,
    );
    let canonicalize_end = SystemTime::now();

    module_timing.canonicalize = canonicalize_end.duration_since(canonicalize_start).unwrap();

    match canonicalized {
        Ok(mut module_output) => {
            // Add builtin defs (e.g. List.get) to the module's defs
            let builtin_defs = roc_can::builtins::builtin_defs(&mut var_store);
            let references = &module_output.references;

            for (symbol, def) in builtin_defs {
                if references.contains(&symbol) {
                    module_output.declarations.push(Declaration::Builtin(def));
                }
            }

            let constraint = constrain_module(&module_output, module_id, mode, &mut var_store);

            let module = Module {
                module_id,
                exposed_imports: module_output.exposed_imports,
                exposed_vars_by_symbol: module_output.exposed_vars_by_symbol,
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

fn parse<'a>(arena: &'a Bump, header: ModuleHeader<'a>) -> Result<Msg<'a>, LoadingProblem> {
    let mut module_timing = header.module_timing;
    let parse_start = SystemTime::now();
    let parse_state = parser::State::new(&header.src, Attempting::Module);
    let (parsed_defs, _) = module_defs()
        .parse(&arena, parse_state)
        .expect("TODO gracefully handle parse error on module defs. IMPORTANT: Bail out entirely if there are any BadUtf8 problems! That means the whole source file is not valid UTF-8 and any other errors we report may get mis-reported. We rely on this for safety in an `unsafe` block later on in this function.");

    let parsed_defs = parsed_defs.into_bump_slice();

    // Record the parse end time once, to avoid checking the time a second time
    // immediately afterward (for the beginning of canonicalization).
    let parse_end = SystemTime::now();

    module_timing.parse_body = parse_end.duration_since(parse_start).unwrap();

    let imported_modules = header.imported_modules;

    // SAFETY: By this point we've already incrementally verified that there
    // are no UTF-8 errors in these bytes. If there had been any UTF-8 errors,
    // we'd have bailed out before now.
    let src = unsafe { from_utf8_unchecked(header.src) };

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
        deps_by_name,
        exposed_ident_ids,
        exposed_imports,
        src,
        parsed_defs,
        imported_modules,
        module_timing,
    };

    Ok(Msg::Parsed(parsed))
}

fn exposed_from_import(entry: &ImportsEntry<'_>) -> (ModuleName, Vec<Ident>) {
    use roc_parse::ast::ImportsEntry::*;

    match entry {
        Module(module_name, exposes) => {
            let mut exposed = Vec::with_capacity(exposes.len());

            for loc_entry in exposes {
                exposed.push(ident_from_exposed(&loc_entry.value));
            }

            (module_name.as_str().into(), exposed)
        }

        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => {
            // Ignore spaces.
            exposed_from_import(*sub_entry)
        }
    }
}

fn ident_from_exposed(entry: &ExposesEntry<'_>) -> Ident {
    use roc_parse::ast::ExposesEntry::*;

    match entry {
        Ident(ident) => (*ident).into(),
        SpaceBefore(sub_entry, _) | SpaceAfter(sub_entry, _) => ident_from_exposed(sub_entry),
    }
}

#[allow(clippy::too_many_arguments)]
fn make_specializations<'a>(
    arena: &'a Bump,
    home: ModuleId,
    mut ident_ids: IdentIds,
    mut subs: Subs,
    mut procs: Procs<'a>,
    mut layout_cache: LayoutCache<'a>,
    specializations_we_must_make: ExternalSpecializations,
) -> Msg<'a> {
    let mut mono_problems = Vec::new();
    // do the thing
    let mut mono_env = roc_mono::ir::Env {
        arena,
        problems: &mut mono_problems,
        subs: &mut subs,
        home,
        ident_ids: &mut ident_ids,
    };

    procs
        .externals_others_need
        .extend(specializations_we_must_make);

    // TODO: for now this final specialization pass is sequential,
    // with no parallelization at all. We should try to parallelize
    // this, but doing so will require a redesign of Procs.
    procs = roc_mono::ir::specialize_all(
        &mut mono_env,
        procs,
        &mut layout_cache,
        // &finished_info.vars_by_symbol,
    );

    let external_specializations_requested = procs.externals_we_need.clone();
    let procedures = procs.get_specialized_procs_without_rc(mono_env.arena);

    Msg::MadeSpecializations {
        module_id: home,
        ident_ids,
        layout_cache,
        procedures,
        problems: mono_problems,
        subs,
        external_specializations_requested,
    }
}

#[allow(clippy::too_many_arguments)]
fn build_pending_specializations<'a>(
    arena: &'a Bump,
    solved_subs: Solved<Subs>,
    home: ModuleId,
    mut ident_ids: IdentIds,
    decls: Vec<Declaration>,
    // TODO use this?
    _module_timing: ModuleTiming,
    mut layout_cache: LayoutCache<'a>,
    // TODO remove
    exposed_to_host: MutMap<Symbol, Variable>,
) -> Msg<'a> {
    let mut procs = Procs::default();

    let mut mono_problems = std::vec::Vec::new();
    let mut subs = solved_subs.into_inner();
    let mut mono_env = roc_mono::ir::Env {
        arena,
        problems: &mut mono_problems,
        subs: &mut subs,
        home,
        ident_ids: &mut ident_ids,
    };

    // Add modules' decls to Procs
    for decl in decls {
        use roc_can::def::Declaration::*;

        match decl {
            Declare(def) | Builtin(def) => add_def_to_module(
                &mut layout_cache,
                &mut procs,
                &mut mono_env,
                def,
                &exposed_to_host,
                false,
            ),
            DeclareRec(defs) => {
                for def in defs {
                    add_def_to_module(
                        &mut layout_cache,
                        &mut procs,
                        &mut mono_env,
                        def,
                        &exposed_to_host,
                        true,
                    )
                }
            }
            InvalidCycle(_loc_idents, _regions) => {
                todo!("TODO handle InvalidCycle");
            }
        }
    }

    let problems = mono_env.problems.to_vec();

    Msg::FoundSpecializations {
        module_id: home,
        solved_subs: roc_types::solved_types::Solved(subs),
        ident_ids,
        layout_cache,
        procs,
        problems,
    }
}

fn add_def_to_module<'a>(
    layout_cache: &mut LayoutCache<'a>,
    procs: &mut Procs<'a>,
    mono_env: &mut roc_mono::ir::Env<'a, '_>,
    def: roc_can::def::Def,
    exposed_to_host: &MutMap<Symbol, Variable>,
    is_recursive: bool,
) {
    use roc_can::expr::Expr::*;
    use roc_can::pattern::Pattern::*;

    match def.loc_pattern.value {
        Identifier(symbol) => {
            let is_exposed = exposed_to_host.contains_key(&symbol);

            match def.loc_expr.value {
                Closure {
                    function_type: annotation,
                    return_type: ret_var,
                    arguments: loc_args,
                    loc_body,
                    captured_symbols,
                    ..
                } => {
                    // this is a top-level definition, it should not capture anything
                    debug_assert!(captured_symbols.is_empty());

                    // If this is an exposed symbol, we need to
                    // register it as such. Otherwise, since it
                    // never gets called by Roc code, it will never
                    // get specialized!
                    if is_exposed {
                        let mut pattern_vars = bumpalo::collections::Vec::with_capacity_in(
                            loc_args.len(),
                            mono_env.arena,
                        );

                        for (var, _) in loc_args.iter() {
                            pattern_vars.push(*var);
                        }

                        let layout = match layout_cache.from_var(
                            mono_env.arena,
                            annotation,
                            mono_env.subs,
                        ) {
                            Ok(l) => l,
                            Err(err) => {
                                // a host-exposed function is not monomorphized
                                todo!("The host-exposed function {:?} does not have a valid layout (e.g. maybe the function wasn't monomorphic): {:?}", symbol, err)
                            }
                        };

                        procs.insert_exposed(
                            symbol,
                            layout,
                            mono_env.subs,
                            def.annotation,
                            annotation,
                        );
                    }

                    procs.insert_named(
                        mono_env,
                        layout_cache,
                        symbol,
                        annotation,
                        loc_args,
                        *loc_body,
                        CapturedSymbols::None,
                        is_recursive,
                        ret_var,
                    );
                }
                body => {
                    // If this is an exposed symbol, we need to
                    // register it as such. Otherwise, since it
                    // never gets called by Roc code, it will never
                    // get specialized!
                    if is_exposed {
                        let annotation = def.expr_var;
                        let layout = layout_cache.from_var(mono_env.arena, annotation, mono_env.subs).unwrap_or_else(|err|
                                        todo!("TODO gracefully handle the situation where we expose a function to the host which doesn't have a valid layout (e.g. maybe the function wasn't monomorphic): {:?}", err)
                                    );

                        procs.insert_exposed(
                            symbol,
                            layout,
                            mono_env.subs,
                            def.annotation,
                            annotation,
                        );
                    }

                    let proc = PartialProc {
                        annotation: def.expr_var,
                        // This is a 0-arity thunk, so it has no arguments.
                        pattern_symbols: &[],
                        // This is a top-level definition, so it cannot capture anything
                        captured_symbols: CapturedSymbols::None,
                        body,
                        // This is a 0-arity thunk, so it cannot be recursive
                        is_self_recursive: false,
                    };

                    procs.partial_procs.insert(symbol, proc);
                    procs.module_thunks.insert(symbol);
                }
            };
        }
        other => {
            todo!("TODO gracefully handle Declare({:?})", other);
        }
    }
}

fn run_task<'a>(
    task: BuildTask<'a>,
    arena: &'a Bump,
    src_dir: &Path,
    msg_tx: MsgSender<'a>,
) -> Result<(), LoadingProblem> {
    use BuildTask::*;

    let msg = match task {
        LoadModule {
            module_name,
            module_ids,
            ident_ids_by_module,
            mode,
        } => load_module(
            arena,
            src_dir,
            module_name,
            module_ids,
            ident_ids_by_module,
            mode,
        )
        .map(|(_, msg)| msg),
        LoadPkgConfig {
            module_ids,
            ident_ids_by_module,
            mode,
        } => load_pkg_config(arena, src_dir, module_ids, ident_ids_by_module, mode),
        Parse { header } => parse(arena, header),
        CanonicalizeAndConstrain {
            parsed,
            module_ids,
            dep_idents,
            mode,
            exposed_symbols,
            aliases,
        } => canonicalize_and_constrain(
            arena,
            &module_ids,
            dep_idents,
            exposed_symbols,
            aliases,
            mode,
            parsed,
        ),
        Solve {
            module,
            module_timing,
            imported_symbols,
            constraint,
            var_store,
            ident_ids,
            declarations,
        } => Ok(run_solve(
            module,
            ident_ids,
            module_timing,
            imported_symbols,
            constraint,
            var_store,
            declarations,
        )),
        BuildPendingSpecializations {
            module_id,
            ident_ids,
            decls,
            module_timing,
            layout_cache,
            solved_subs,
            exposed_to_host,
        } => Ok(build_pending_specializations(
            arena,
            solved_subs,
            module_id,
            ident_ids,
            decls,
            module_timing,
            layout_cache,
            exposed_to_host,
        )),
        MakeSpecializations {
            module_id,
            ident_ids,
            subs,
            procs,
            layout_cache,
            specializations_we_must_make,
        } => Ok(make_specializations(
            arena,
            module_id,
            ident_ids,
            subs,
            procs,
            layout_cache,
            specializations_we_must_make,
        )),
    }?;

    msg_tx
        .send(msg)
        .map_err(|_| LoadingProblem::MsgChannelDied)?;

    Ok(())
}
