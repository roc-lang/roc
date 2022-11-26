use crate::docs::ModuleDocumentation;
use bumpalo::Bump;
use crossbeam::channel::{bounded, Sender};
use crossbeam::deque::{Injector, Stealer, Worker};
use crossbeam::thread;
use parking_lot::Mutex;
use roc_builtins::roc::module_source;
use roc_can::abilities::{AbilitiesStore, PendingAbilitiesStore, ResolvedImpl};
use roc_can::constraint::{Constraint as ConstraintSoa, Constraints, TypeOrVar};
use roc_can::expr::{DbgLookup, Declarations, ExpectLookup, PendingDerives};
use roc_can::module::{
    canonicalize_module_defs, ExposedByModule, ExposedForModule, ExposedModuleTypes, Module,
    ResolvedImplementations, TypeState,
};
use roc_collections::{default_hasher, BumpMap, MutMap, MutSet, VecMap, VecSet};
use roc_constrain::module::constrain_module;
use roc_debug_flags::dbg_do;
#[cfg(debug_assertions)]
use roc_debug_flags::{
    ROC_PRINT_IR_AFTER_REFCOUNT, ROC_PRINT_IR_AFTER_RESET_REUSE, ROC_PRINT_IR_AFTER_SPECIALIZATION,
    ROC_PRINT_LOAD_LOG,
};
use roc_derive::SharedDerivedModule;
use roc_error_macros::internal_error;
use roc_intern::{GlobalInterner, SingleThreadedInterner};
use roc_late_solve::{AbilitiesView, WorldAbilities};
use roc_module::ident::{Ident, ModuleName, QualifiedModuleName};
use roc_module::symbol::{
    IdentIds, IdentIdsByModule, Interns, ModuleId, ModuleIds, PQModuleName, PackageModuleIds,
    PackageQualified, Symbol,
};
use roc_mono::ir::{
    CapturedSymbols, ExternalSpecializations, PartialProc, Proc, ProcLayout, Procs, ProcsBase,
    UpdateModeIds,
};
use roc_mono::layout::{
    CapturesNiche, LambdaName, Layout, LayoutCache, LayoutProblem, STLayoutInterner,
};
use roc_packaging::cache::{self, RocCacheDir};
#[cfg(not(target_family = "wasm"))]
use roc_packaging::https::PackageMetadata;
use roc_parse::ast::{self, Defs, ExtractSpaces, Spaced, StrLiteral, TypeAnnotation};
use roc_parse::header::{ExposedName, ImportsEntry, PackageEntry, PlatformHeader, To, TypedIdent};
use roc_parse::header::{HeaderFor, ModuleNameEnum, PackageName};
use roc_parse::ident::UppercaseIdent;
use roc_parse::module::module_defs;
use roc_parse::parser::{FileError, Parser, SourceError, SyntaxError};
use roc_region::all::{LineInfo, Loc, Region};
use roc_reporting::report::{Annotation, Palette, RenderTarget};
use roc_solve::module::{extract_module_owned_implementations, Solved, SolvedModule};
use roc_solve_problem::TypeError;
use roc_target::TargetInfo;
use roc_types::subs::{ExposedTypesStorageSubs, Subs, VarStore, Variable};
use roc_types::types::{Alias, Types};
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::collections::HashMap;
use std::env::current_dir;
use std::io;
use std::iter;
use std::ops::ControlFlow;
use std::path::{Path, PathBuf};
use std::str::from_utf8_unchecked;
use std::sync::Arc;
use std::{env, fs};

pub use crate::work::Phase;
use crate::work::{DepCycle, Dependencies};

#[cfg(target_family = "wasm")]
use crate::wasm_instant::{Duration, Instant};
#[cfg(not(target_family = "wasm"))]
use std::time::{Duration, Instant};

/// Default name for the binary generated for an app, if an invalid one was specified.
const DEFAULT_APP_OUTPUT_PATH: &str = "app";

/// Filename extension for normal Roc modules
const ROC_FILE_EXTENSION: &str = "roc";

/// The . in between module names like Foo.Bar.Baz
const MODULE_SEPARATOR: char = '.';

const EXPANDED_STACK_SIZE: usize = 8 * 1024 * 1024;

macro_rules! log {
    ($($arg:tt)*) => (dbg_do!(ROC_PRINT_LOAD_LOG, println!($($arg)*)))
}

#[derive(Debug)]
pub struct LoadConfig {
    pub target_info: TargetInfo,
    pub render: RenderTarget,
    pub palette: Palette,
    pub threading: Threading,
    pub exec_mode: ExecutionMode,
}

#[derive(Debug, Clone, Copy)]
pub enum ExecutionMode {
    Test,
    Check,
    Executable,
    /// Like [`ExecutionMode::Executable`], but stops in the presence of type errors.
    ExecutableIfCheck,
}

impl ExecutionMode {
    fn goal_phase(&self) -> Phase {
        match self {
            ExecutionMode::Test | ExecutionMode::Executable => Phase::MakeSpecializations,
            ExecutionMode::Check | ExecutionMode::ExecutableIfCheck => Phase::SolveTypes,
        }
    }
}

/// Struct storing various intermediate stages by their ModuleId
#[derive(Debug)]
struct ModuleCache<'a> {
    module_names: MutMap<ModuleId, PQModuleName<'a>>,

    /// Phases
    headers: MutMap<ModuleId, ModuleHeader<'a>>,
    parsed: MutMap<ModuleId, ParsedModule<'a>>,
    aliases: MutMap<ModuleId, MutMap<Symbol, (bool, Alias)>>,
    pending_abilities: MutMap<ModuleId, PendingAbilitiesStore>,
    constrained: MutMap<ModuleId, ConstrainedModule>,
    typechecked: MutMap<ModuleId, TypeCheckedModule<'a>>,
    found_specializations: MutMap<ModuleId, FoundSpecializationsModule<'a>>,
    late_specializations: MutMap<ModuleId, LateSpecializationsModule<'a>>,
    external_specializations_requested: MutMap<ModuleId, Vec<ExternalSpecializations<'a>>>,
    expectations: VecMap<ModuleId, Expectations>,

    /// Various information
    imports: MutMap<ModuleId, MutSet<ModuleId>>,
    top_level_thunks: MutMap<ModuleId, MutSet<Symbol>>,
    documentation: MutMap<ModuleId, ModuleDocumentation>,
    can_problems: MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    type_problems: MutMap<ModuleId, Vec<TypeError>>,

    sources: MutMap<ModuleId, (PathBuf, &'a str)>,
}

impl<'a> ModuleCache<'a> {
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
            JSON,
        }

        Self {
            module_names,
            headers: Default::default(),
            parsed: Default::default(),
            aliases: Default::default(),
            pending_abilities: Default::default(),
            constrained: Default::default(),
            typechecked: Default::default(),
            found_specializations: Default::default(),
            late_specializations: Default::default(),
            external_specializations_requested: Default::default(),
            imports: Default::default(),
            top_level_thunks: Default::default(),
            documentation: Default::default(),
            can_problems: Default::default(),
            type_problems: Default::default(),
            sources: Default::default(),
            expectations: Default::default(),
        }
    }
}

type SharedIdentIdsByModule = Arc<Mutex<roc_module::symbol::IdentIdsByModule>>;

fn start_phase<'a>(
    module_id: ModuleId,
    phase: Phase,
    arena: &'a Bump,
    state: &mut State<'a>,
) -> Vec<BuildTask<'a>> {
    // we blindly assume all dependencies are met

    use crate::work::PrepareStartPhase::*;
    match state.dependencies.prepare_start_phase(module_id, phase) {
        Continue => {
            // fall through
        }
        Done => {
            // no more work to do
            return vec![];
        }
        Recurse(new) => {
            return new
                .into_iter()
                .flat_map(|(module_id, phase)| start_phase(module_id, phase, arena, state))
                .collect()
        }
    }

    let task = {
        match phase {
            Phase::LoadHeader => {
                let opt_dep_name = state.module_cache.module_names.get(&module_id);

                match opt_dep_name {
                    None => {
                        panic!("Module {:?} is not in module_cache.module_names", module_id)
                    }
                    Some(dep_name) => {
                        let module_name = dep_name.clone();

                        BuildTask::LoadModule {
                            module_name,
                            // Provide mutexes of ModuleIds and IdentIds by module,
                            // so other modules can populate them as they load.
                            module_ids: Arc::clone(&state.arc_modules),
                            shorthands: Arc::clone(&state.arc_shorthands),
                            ident_ids_by_module: Arc::clone(&state.ident_ids_by_module),
                        }
                    }
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
                let mut dep_idents: IdentIdsByModule = IdentIds::exposed_builtins(num_deps);

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
                    .get(&module_id)
                    .expect("Could not find listener ID in exposed_symbols_by_module")
                    .clone();

                let mut aliases = MutMap::default();
                let mut abilities_store = PendingAbilitiesStore::default();

                for imported in parsed.imported_modules.keys() {
                    match state.module_cache.aliases.get(imported) {
                        None => unreachable!(
                            r"imported module {:?} did not register its aliases, so {:?} cannot use them",
                            imported, parsed.module_id,
                        ),
                        Some(new) => {
                            aliases.extend(new.iter().filter_map(|(s, (exposed, a))| {
                                // only pass this on if it's exposed, or the alias is a transitive import
                                if *exposed || s.module_id() != *imported {
                                    Some((*s, a.clone()))
                                } else {
                                    None
                                }
                            }));
                        }
                    }

                    match state.module_cache.pending_abilities.get(imported) {
                        None => unreachable!(
                            r"imported module {:?} did not register its abilities, so {:?} cannot use them",
                            imported, parsed.module_id,
                        ),
                        Some(import_store) => {
                            let exposed_symbols = state
                                .exposed_symbols_by_module
                                .get(imported)
                                .unwrap_or_else(|| {
                                    internal_error!(
                                        "Could not find exposed symbols of imported {:?}",
                                        imported
                                    )
                                });

                            // Add the declared abilities from the modules we import;
                            // we may not know all their types yet since type-solving happens in
                            // parallel, but we'll fill that in during type-checking our module.
                            abilities_store
                                .union(import_store.closure_from_imported(exposed_symbols));
                        }
                    }
                }

                let skip_constraint_gen = {
                    // Give this its own scope to make sure that the Guard from the lock() is dropped
                    // immediately after contains_key returns
                    state.cached_types.lock().contains_key(&module_id)
                };

                BuildTask::CanonicalizeAndConstrain {
                    parsed,
                    dep_idents,
                    exposed_symbols,
                    module_ids,
                    aliases,
                    abilities_store,
                    skip_constraint_gen,
                }
            }

            Phase::SolveTypes => {
                let constrained = state.module_cache.constrained.remove(&module_id).unwrap();

                let ConstrainedModule {
                    module,
                    ident_ids,
                    module_timing,
                    constraints,
                    constraint,
                    var_store,
                    imported_modules,
                    declarations,
                    dep_idents,
                    pending_derives,
                    types,
                    ..
                } = constrained;

                let derived_module = SharedDerivedModule::clone(&state.derived_module);

                BuildTask::solve_module(
                    module,
                    ident_ids,
                    module_timing,
                    types,
                    constraints,
                    constraint,
                    pending_derives,
                    var_store,
                    imported_modules,
                    &state.exposed_types,
                    dep_idents,
                    declarations,
                    state.cached_types.clone(),
                    derived_module,
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
                    abilities_store,
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

                let derived_module = SharedDerivedModule::clone(&state.derived_module);

                let build_expects = matches!(state.exec_mode, ExecutionMode::Test)
                    && state.module_cache.expectations.contains_key(&module_id);

                BuildTask::BuildPendingSpecializations {
                    layout_cache,
                    module_id,
                    module_timing,
                    solved_subs,
                    imported_module_thunks: imported_module_thunks.into_bump_slice(),
                    decls,
                    ident_ids,
                    exposed_to_host: state.exposed_to_host.clone(),
                    abilities_store,
                    // TODO: awful, how can we get rid of the clone?
                    exposed_by_module: state.exposed_types.clone(),
                    derived_module,
                    build_expects,
                }
            }
            Phase::MakeSpecializations => {
                let mut specializations_we_must_make = state
                    .module_cache
                    .external_specializations_requested
                    .remove(&module_id)
                    .unwrap_or_default();

                if module_id == ModuleId::DERIVED_GEN {
                    // The derived gen module must also fulfill also specializations asked of the
                    // derived synth module.
                    let derived_synth_specializations = state
                        .module_cache
                        .external_specializations_requested
                        .remove(&ModuleId::DERIVED_SYNTH)
                        .unwrap_or_default();
                    specializations_we_must_make.extend(derived_synth_specializations)
                }

                let (mut ident_ids, mut subs, mut procs_base, layout_cache, mut module_timing) =
                    if state.make_specializations_pass.current_pass() == 1
                        && module_id == ModuleId::DERIVED_GEN
                    {
                        // This is the first time the derived module is introduced into the load
                        // graph. It has no abilities of its own or anything, just generate fresh
                        // information for it.
                        (
                            IdentIds::default(),
                            Subs::default(),
                            ProcsBase::default(),
                            LayoutCache::new(state.layout_interner.fork(), state.target_info),
                            ModuleTiming::new(Instant::now()),
                        )
                    } else if state.make_specializations_pass.current_pass() == 1 {
                        let found_specializations = state
                            .module_cache
                            .found_specializations
                            .remove(&module_id)
                            .unwrap();

                        let FoundSpecializationsModule {
                            ident_ids,
                            subs,
                            procs_base,
                            layout_cache,
                            module_timing,
                            abilities_store,
                        } = found_specializations;

                        let our_exposed_types = state
                            .exposed_types
                            .get(&module_id)
                            .unwrap_or_else(|| {
                                internal_error!("Exposed types for {:?} missing", module_id)
                            })
                            .clone();

                        // Add our abilities to the world.
                        state.world_abilities.insert(
                            module_id,
                            abilities_store,
                            our_exposed_types.exposed_types_storage_subs,
                        );

                        (ident_ids, subs, procs_base, layout_cache, module_timing)
                    } else {
                        let LateSpecializationsModule {
                            ident_ids,
                            subs,
                            module_timing,
                            layout_cache,
                            procs_base,
                        } = state
                            .module_cache
                            .late_specializations
                            .remove(&module_id)
                            .unwrap();

                        (ident_ids, subs, procs_base, layout_cache, module_timing)
                    };

                if module_id == ModuleId::DERIVED_GEN {
                    load_derived_partial_procs(
                        module_id,
                        arena,
                        &mut subs,
                        &mut ident_ids,
                        &state.derived_module,
                        &mut module_timing,
                        state.target_info,
                        &state.exposed_types,
                        &mut procs_base,
                        &mut state.world_abilities,
                    );
                }

                let derived_module = SharedDerivedModule::clone(&state.derived_module);

                BuildTask::MakeSpecializations {
                    module_id,
                    ident_ids,
                    subs,
                    procs_base,
                    layout_cache,
                    specializations_we_must_make,
                    module_timing,
                    world_abilities: state.world_abilities.clone_ref(),
                    // TODO: awful, how can we get rid of the clone?
                    exposed_by_module: state.exposed_types.clone(),
                    derived_module,
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
    pub type_problems: MutMap<ModuleId, Vec<TypeError>>,
    pub declarations_by_id: MutMap<ModuleId, Declarations>,
    pub exposed_to_host: MutMap<Symbol, Variable>,
    pub dep_idents: IdentIdsByModule,
    pub exposed_aliases: MutMap<Symbol, Alias>,
    pub exposed_values: Vec<Symbol>,
    pub exposed_types_storage: ExposedTypesStorageSubs,
    pub resolved_implementations: ResolvedImplementations,
    pub sources: MutMap<ModuleId, (PathBuf, Box<str>)>,
    pub timings: MutMap<ModuleId, ModuleTiming>,
    pub documentation: MutMap<ModuleId, ModuleDocumentation>,
    pub abilities_store: AbilitiesStore,
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
            .map(|symbol| symbol.as_str(&self.interns))
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
    parse_state: roc_parse::state::State<'a>,
    header_for: HeaderFor<'a>,
    symbols_from_requires: Vec<(Loc<Symbol>, Loc<TypeAnnotation<'a>>)>,
    module_timing: ModuleTiming,
}

#[derive(Debug)]
struct ConstrainedModule {
    module: Module,
    declarations: Declarations,
    imported_modules: MutMap<ModuleId, Region>,
    constraints: Constraints,
    constraint: ConstraintSoa,
    ident_ids: IdentIds,
    var_store: VarStore,
    dep_idents: IdentIdsByModule,
    module_timing: ModuleTiming,
    types: Types,
    // Rather than adding pending derives as constraints, hand them directly to solve because they
    // must be solved at the end of a module.
    pending_derives: PendingDerives,
}

#[derive(Debug)]
pub struct TypeCheckedModule<'a> {
    pub module_id: ModuleId,
    pub layout_cache: LayoutCache<'a>,
    pub module_timing: ModuleTiming,
    pub solved_subs: Solved<Subs>,
    pub decls: Declarations,
    pub ident_ids: IdentIds,
    pub abilities_store: AbilitiesStore,
}

#[derive(Debug)]
struct FoundSpecializationsModule<'a> {
    ident_ids: IdentIds,
    layout_cache: LayoutCache<'a>,
    procs_base: ProcsBase<'a>,
    subs: Subs,
    module_timing: ModuleTiming,
    abilities_store: AbilitiesStore,
}

#[derive(Debug)]
struct LateSpecializationsModule<'a> {
    ident_ids: IdentIds,
    subs: Subs,
    module_timing: ModuleTiming,
    layout_cache: LayoutCache<'a>,
    procs_base: ProcsBase<'a>,
}

#[derive(Debug, Default)]
pub struct ToplevelExpects {
    pub pure: VecMap<Symbol, Region>,
    pub fx: VecMap<Symbol, Region>,
}

#[derive(Debug)]
pub struct MonomorphizedModule<'a> {
    pub module_id: ModuleId,
    pub interns: Interns,
    pub subs: Subs,
    pub layout_interner: SingleThreadedInterner<'a, Layout<'a>>,
    pub output_path: Box<Path>,
    pub can_problems: MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    pub type_problems: MutMap<ModuleId, Vec<TypeError>>,
    pub procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    pub toplevel_expects: ToplevelExpects,
    pub entry_point: EntryPoint<'a>,
    pub exposed_to_host: ExposedToHost,
    pub sources: MutMap<ModuleId, (PathBuf, Box<str>)>,
    pub timings: MutMap<ModuleId, ModuleTiming>,
    pub expectations: VecMap<ModuleId, Expectations>,
    pub uses_prebuilt_platform: bool,
}

/// Values used to render expect output
pub struct ExpectMetadata<'a> {
    pub interns: Interns,
    pub layout_interner: SingleThreadedInterner<'a, Layout<'a>>,
    pub expectations: VecMap<ModuleId, Expectations>,
}

#[derive(Debug)]
pub enum EntryPoint<'a> {
    Executable {
        symbol: Symbol,
        layout: ProcLayout<'a>,
        platform_path: PathBuf,
    },
    Test,
}

#[derive(Debug)]
pub struct Expectations {
    pub subs: roc_types::subs::Subs,
    pub path: PathBuf,
    pub expectations: VecMap<Region, Vec<ExpectLookup>>,
    pub dbgs: VecMap<Symbol, DbgLookup>,
    pub ident_ids: IdentIds,
}

#[derive(Clone, Debug, Default)]
pub struct ExposedToHost {
    /// usually `mainForHost`
    pub values: MutMap<Symbol, Variable>,
    /// exposed closure types, typically `Fx`
    pub closure_types: Vec<Symbol>,
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

        total
    }
}

#[derive(Debug)]
struct ParsedModule<'a> {
    module_id: ModuleId,
    module_path: PathBuf,
    src: &'a str,
    module_timing: ModuleTiming,
    deps_by_name: MutMap<PQModuleName<'a>, ModuleId>,
    imported_modules: MutMap<ModuleId, Region>,
    exposed_ident_ids: IdentIds,
    exposed_imports: MutMap<Ident, (Symbol, Region)>,
    parsed_defs: Defs<'a>,
    module_name: ModuleNameEnum<'a>,
    symbols_from_requires: Vec<(Loc<Symbol>, Loc<TypeAnnotation<'a>>)>,
    header_for: HeaderFor<'a>,
}

type LocExpects = VecMap<Region, Vec<ExpectLookup>>;
type LocDbgs = VecMap<Symbol, DbgLookup>;

/// A message sent out _from_ a worker thread,
/// representing a result of work done, or a request for further work
#[derive(Debug)]
enum Msg<'a> {
    Many(Vec<Msg<'a>>),
    Header(ModuleHeader<'a>),
    Parsed(ParsedModule<'a>),
    CanonicalizedAndConstrained(CanAndCon),
    SolvedTypes {
        module_id: ModuleId,
        ident_ids: IdentIds,
        solved_module: SolvedModule,
        solved_subs: Solved<Subs>,
        decls: Declarations,
        dep_idents: IdentIdsByModule,
        module_timing: ModuleTiming,
        abilities_store: AbilitiesStore,
        loc_expects: LocExpects,
        loc_dbgs: LocDbgs,
    },
    FinishedAllTypeChecking {
        solved_subs: Solved<Subs>,
        exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
        exposed_aliases_by_symbol: MutMap<Symbol, (bool, Alias)>,
        exposed_types_storage: ExposedTypesStorageSubs,
        resolved_implementations: ResolvedImplementations,
        dep_idents: IdentIdsByModule,
        documentation: MutMap<ModuleId, ModuleDocumentation>,
        abilities_store: AbilitiesStore,
    },
    FoundSpecializations {
        module_id: ModuleId,
        ident_ids: IdentIds,
        layout_cache: LayoutCache<'a>,
        procs_base: ProcsBase<'a>,
        solved_subs: Solved<Subs>,
        module_timing: ModuleTiming,
        abilities_store: AbilitiesStore,
        toplevel_expects: ToplevelExpects,
    },
    MadeSpecializations {
        module_id: ModuleId,
        ident_ids: IdentIds,
        layout_cache: LayoutCache<'a>,
        external_specializations_requested: BumpMap<ModuleId, ExternalSpecializations<'a>>,
        procs_base: ProcsBase<'a>,
        procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
        update_mode_ids: UpdateModeIds,
        module_timing: ModuleTiming,
        subs: Subs,
    },

    /// The task is to only typecheck AND monomorphize modules
    /// all modules are now monomorphized, we are done
    FinishedAllSpecialization {
        subs: Subs,
        /// The layout interner after all passes in mono are done.
        /// DO NOT use the one on state; that is left in an empty state after specialization is complete!
        layout_interner: STLayoutInterner<'a>,
        exposed_to_host: ExposedToHost,
    },

    FailedToParse(FileError<'a, SyntaxError<'a>>),
    FailedToReadFile {
        filename: PathBuf,
        error: io::ErrorKind,
    },

    IncorrectModuleName(FileError<'a, IncorrectModuleName<'a>>),
}

#[derive(Debug)]
struct CanAndCon {
    constrained_module: ConstrainedModule,
    canonicalization_problems: Vec<roc_problem::can::Problem>,
    module_docs: Option<ModuleDocumentation>,
}

#[derive(Debug)]
enum PlatformPath<'a> {
    NotSpecified,
    Valid(To<'a>),
    RootIsInterface,
    RootIsHosted,
    RootIsPlatformModule,
}

#[derive(Debug)]
struct PlatformData {
    module_id: ModuleId,
    provides: Symbol,
    is_prebuilt: bool,
}

#[derive(Debug, Clone, Copy)]
enum MakeSpecializationsPass {
    Pass(u8),
}

impl MakeSpecializationsPass {
    fn inc(&mut self) {
        match self {
            &mut Self::Pass(n) => {
                *self = Self::Pass(
                    n.checked_add(1)
                        .expect("way too many specialization passes!"),
                )
            }
        }
    }

    fn current_pass(&self) -> u8 {
        match self {
            MakeSpecializationsPass::Pass(n) => *n,
        }
    }
}

#[derive(Debug)]
struct State<'a> {
    pub root_id: ModuleId,
    pub root_subs: Option<Subs>,
    pub cache_dir: PathBuf,
    pub platform_data: Option<PlatformData>,
    pub exposed_types: ExposedByModule,
    pub output_path: Option<&'a str>,
    pub platform_path: PlatformPath<'a>,
    pub target_info: TargetInfo,

    pub module_cache: ModuleCache<'a>,
    pub dependencies: Dependencies<'a>,
    pub procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    pub toplevel_expects: ToplevelExpects,
    pub exposed_to_host: ExposedToHost,

    /// This is the "final" list of IdentIds, after canonicalization and constraint gen
    /// have completed for a given module.
    pub constrained_ident_ids: IdentIdsByModule,

    /// From now on, these will be used by multiple threads; time to make an Arc<Mutex<_>>!
    pub arc_modules: Arc<Mutex<PackageModuleIds<'a>>>,
    pub arc_shorthands: Arc<Mutex<MutMap<&'a str, ShorthandPath>>>,
    #[allow(unused)]
    pub derived_module: SharedDerivedModule,

    pub ident_ids_by_module: SharedIdentIdsByModule,

    pub declarations_by_id: MutMap<ModuleId, Declarations>,

    pub exposed_symbols_by_module: MutMap<ModuleId, VecSet<Symbol>>,

    pub timings: MutMap<ModuleId, ModuleTiming>,

    // Each thread gets its own layout cache. When one "pending specializations"
    // pass completes, it returns its layout cache so another thread can use it.
    // We don't bother trying to union them all together to maximize cache hits,
    // since the unioning process could potentially take longer than the savings.
    // (Granted, this has not been attempted or measured!)
    pub layout_caches: std::vec::Vec<LayoutCache<'a>>,

    pub render: RenderTarget,
    pub palette: Palette,
    pub exec_mode: ExecutionMode,

    /// All abilities across all modules.
    pub world_abilities: WorldAbilities,

    make_specializations_pass: MakeSpecializationsPass,

    // cached types (used for builtin modules, could include packages in the future too)
    cached_types: CachedTypeState,

    layout_interner: Arc<GlobalInterner<'a, Layout<'a>>>,
}

type CachedTypeState = Arc<Mutex<MutMap<ModuleId, TypeState>>>;

impl<'a> State<'a> {
    fn goal_phase(&self) -> Phase {
        self.exec_mode.goal_phase()
    }

    #[allow(clippy::too_many_arguments)]
    fn new(
        root_id: ModuleId,
        target_info: TargetInfo,
        exposed_types: ExposedByModule,
        arc_modules: Arc<Mutex<PackageModuleIds<'a>>>,
        ident_ids_by_module: SharedIdentIdsByModule,
        cached_types: MutMap<ModuleId, TypeState>,
        render: RenderTarget,
        palette: Palette,
        number_of_workers: usize,
        exec_mode: ExecutionMode,
    ) -> Self {
        let arc_shorthands = Arc::new(Mutex::new(MutMap::default()));
        let cache_dir = roc_packaging::cache::roc_cache_dir();
        let dependencies = Dependencies::new(exec_mode.goal_phase());

        Self {
            root_id,
            root_subs: None,
            cache_dir,
            target_info,
            platform_data: None,
            output_path: None,
            platform_path: PlatformPath::NotSpecified,
            module_cache: ModuleCache::default(),
            dependencies,
            procedures: MutMap::default(),
            toplevel_expects: ToplevelExpects::default(),
            exposed_to_host: ExposedToHost::default(),
            exposed_types,
            arc_modules,
            arc_shorthands,
            derived_module: Default::default(),
            constrained_ident_ids: IdentIds::exposed_builtins(0),
            ident_ids_by_module,
            declarations_by_id: MutMap::default(),
            exposed_symbols_by_module: MutMap::default(),
            timings: MutMap::default(),
            layout_caches: std::vec::Vec::with_capacity(number_of_workers),
            cached_types: Arc::new(Mutex::new(cached_types)),
            render,
            palette,
            exec_mode,
            make_specializations_pass: MakeSpecializationsPass::Pass(1),
            world_abilities: Default::default(),
            layout_interner: GlobalInterner::with_capacity(128),
        }
    }
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
    // indexed by make specializations pass
    pub make_specializations: Vec<Duration>,
    // TODO pub monomorphize: Duration,
    /// Total duration will always be more than the sum of the other fields, due
    /// to things like state lookups in between phases, waiting on other threads, etc.
    start_time: Instant,
    end_time: Instant,
}

impl ModuleTiming {
    pub fn new(start_time: Instant) -> Self {
        ModuleTiming {
            read_roc_file: Duration::default(),
            parse_header: Duration::default(),
            parse_body: Duration::default(),
            canonicalize: Duration::default(),
            constrain: Duration::default(),
            solve: Duration::default(),
            find_specializations: Duration::default(),
            make_specializations: Vec::with_capacity(2),
            start_time,
            end_time: start_time, // just for now; we'll overwrite this at the end
        }
    }

    pub fn total(&self) -> Duration {
        self.end_time.duration_since(self.start_time)
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

        let calculate = |d: Option<Duration>| -> Option<Duration> {
            make_specializations
                .iter()
                .fold(d, |d, pass_time| d?.checked_sub(*pass_time))?
                .checked_sub(*find_specializations)?
                .checked_sub(*solve)?
                .checked_sub(*constrain)?
                .checked_sub(*canonicalize)?
                .checked_sub(*parse_body)?
                .checked_sub(*parse_header)?
                .checked_sub(*read_roc_file)
        };

        calculate(Some(end_time.duration_since(*start_time))).unwrap_or_default()
    }
}

/// A message sent _to_ a worker thread, describing the work to be done
#[derive(Debug)]
#[allow(dead_code)]
enum BuildTask<'a> {
    LoadModule {
        module_name: PQModuleName<'a>,
        module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
        shorthands: Arc<Mutex<MutMap<&'a str, ShorthandPath>>>,
        ident_ids_by_module: SharedIdentIdsByModule,
    },
    Parse {
        header: ModuleHeader<'a>,
    },
    CanonicalizeAndConstrain {
        parsed: ParsedModule<'a>,
        module_ids: ModuleIds,
        dep_idents: IdentIdsByModule,
        exposed_symbols: VecSet<Symbol>,
        aliases: MutMap<Symbol, Alias>,
        abilities_store: PendingAbilitiesStore,
        skip_constraint_gen: bool,
    },
    Solve {
        module: Module,
        ident_ids: IdentIds,
        exposed_for_module: ExposedForModule,
        module_timing: ModuleTiming,
        types: Types,
        constraints: Constraints,
        constraint: ConstraintSoa,
        pending_derives: PendingDerives,
        var_store: VarStore,
        declarations: Declarations,
        dep_idents: IdentIdsByModule,
        cached_subs: CachedTypeState,
        derived_module: SharedDerivedModule,
    },
    BuildPendingSpecializations {
        module_timing: ModuleTiming,
        layout_cache: LayoutCache<'a>,
        solved_subs: Solved<Subs>,
        imported_module_thunks: &'a [Symbol],
        module_id: ModuleId,
        ident_ids: IdentIds,
        decls: Declarations,
        exposed_to_host: ExposedToHost,
        exposed_by_module: ExposedByModule,
        abilities_store: AbilitiesStore,
        derived_module: SharedDerivedModule,
        build_expects: bool,
    },
    MakeSpecializations {
        module_id: ModuleId,
        ident_ids: IdentIds,
        subs: Subs,
        procs_base: ProcsBase<'a>,
        layout_cache: LayoutCache<'a>,
        specializations_we_must_make: Vec<ExternalSpecializations<'a>>,
        module_timing: ModuleTiming,
        exposed_by_module: ExposedByModule,
        world_abilities: WorldAbilities,
        derived_module: SharedDerivedModule,
    },
}

enum WorkerMsg {
    Shutdown,
    TaskAdded,
}

#[derive(Debug)]
pub struct IncorrectModuleName<'a> {
    pub module_id: ModuleId,
    pub found: Loc<PQModuleName<'a>>,
    pub expected: PQModuleName<'a>,
}

#[derive(Debug)]
pub enum LoadingProblem<'a> {
    FileProblem {
        filename: PathBuf,
        error: io::ErrorKind,
    },
    ParsingFailed(FileError<'a, SyntaxError<'a>>),
    UnexpectedHeader(String),

    MsgChannelDied,
    ErrJoiningWorkerThreads,
    TriedToImportAppModule,

    /// a formatted report
    FormattedReport(String),

    ImportCycle(PathBuf, Vec<ModuleId>),
    IncorrectModuleName(FileError<'a, IncorrectModuleName<'a>>),
    CouldNotFindCacheDir,
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

#[allow(clippy::too_many_arguments)]
pub fn load_and_typecheck_str<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    source: &'a str,
    src_dir: PathBuf,
    exposed_types: ExposedByModule,
    target_info: TargetInfo,
    render: RenderTarget,
    palette: Palette,
    roc_cache_dir: RocCacheDir<'_>,
    threading: Threading,
) -> Result<LoadedModule, LoadingProblem<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_str(arena, filename, source, roc_cache_dir, src_dir)?;

    // this function is used specifically in the case
    // where we want to regenerate the cached data
    let cached_subs = MutMap::default();

    let load_config = LoadConfig {
        target_info,
        render,
        palette,
        threading,
        exec_mode: ExecutionMode::Check,
    };

    match load(
        arena,
        load_start,
        exposed_types,
        cached_subs,
        roc_cache_dir,
        load_config,
    )? {
        Monomorphized(_) => unreachable!(""),
        TypeChecked(module) => Ok(module),
    }
}

#[derive(Clone, Copy)]
pub enum PrintTarget {
    ColorTerminal,
    Generic,
}

pub struct LoadStart<'a> {
    arc_modules: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: SharedIdentIdsByModule,
    root_id: ModuleId,
    root_msg: Msg<'a>,
    src_dir: PathBuf,
}

impl<'a> LoadStart<'a> {
    pub fn from_path(
        arena: &'a Bump,
        filename: PathBuf,
        render: RenderTarget,
        roc_cache_dir: RocCacheDir<'_>,
        palette: Palette,
    ) -> Result<Self, LoadingProblem<'a>> {
        let arc_modules = Arc::new(Mutex::new(PackageModuleIds::default()));
        let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
        let ident_ids_by_module = Arc::new(Mutex::new(root_exposed_ident_ids));
        let mut src_dir = filename.parent().unwrap().to_path_buf();

        // Load the root module synchronously; we can't proceed until we have its id.
        let (root_id, root_msg) = {
            let root_start_time = Instant::now();

            let res_loaded = load_filename(
                arena,
                filename,
                true,
                None,
                None,
                Arc::clone(&arc_modules),
                Arc::clone(&ident_ids_by_module),
                roc_cache_dir,
                root_start_time,
            );

            match res_loaded {
                Ok((module_id, msg)) => {
                    if let Msg::Header(ModuleHeader {
                        module_id: header_id,
                        module_name,
                        is_root_module,
                        ..
                    }) = &msg
                    {
                        debug_assert_eq!(*header_id, module_id);
                        debug_assert!(is_root_module);

                        if let ModuleNameEnum::Interface(name) = module_name {
                            // Interface modules can have names like Foo.Bar.Baz,
                            // in which case we need to adjust the src_dir to
                            // remove the "Bar/Baz" directories in order to correctly
                            // resolve this interface module's imports!
                            let dirs_to_pop = name.as_str().matches('.').count();

                            for _ in 0..dirs_to_pop {
                                src_dir.pop();
                            }
                        }
                    }

                    (module_id, msg)
                }

                Err(LoadingProblem::ParsingFailed(problem)) => {
                    let module_ids = Arc::try_unwrap(arc_modules)
                        .unwrap_or_else(|_| {
                            panic!("There were still outstanding Arc references to module_ids")
                        })
                        .into_inner()
                        .into_module_ids();

                    // if parsing failed, this module did not add any identifiers
                    let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
                    let buf = to_parse_problem_report(
                        problem,
                        module_ids,
                        root_exposed_ident_ids,
                        render,
                        palette,
                    );
                    return Err(LoadingProblem::FormattedReport(buf));
                }
                Err(LoadingProblem::FileProblem { filename, error }) => {
                    let buf = to_file_problem_report(&filename, error);
                    return Err(LoadingProblem::FormattedReport(buf));
                }
                Err(LoadingProblem::ImportCycle(filename, cycle)) => {
                    let module_ids = Arc::try_unwrap(arc_modules)
                        .unwrap_or_else(|_| {
                            panic!("There were still outstanding Arc references to module_ids")
                        })
                        .into_inner()
                        .into_module_ids();

                    let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
                    let buf = to_import_cycle_report(
                        module_ids,
                        root_exposed_ident_ids,
                        cycle,
                        filename,
                        render,
                    );
                    return Err(LoadingProblem::FormattedReport(buf));
                }
                Err(LoadingProblem::IncorrectModuleName(FileError {
                    problem: SourceError { problem, bytes },
                    filename,
                })) => {
                    let module_ids = Arc::try_unwrap(arc_modules)
                        .unwrap_or_else(|_| {
                            panic!("There were still outstanding Arc references to module_ids")
                        })
                        .into_inner()
                        .into_module_ids();

                    let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
                    let buf = to_incorrect_module_name_report(
                        module_ids,
                        root_exposed_ident_ids,
                        problem,
                        filename,
                        bytes,
                        render,
                    );
                    return Err(LoadingProblem::FormattedReport(buf));
                }
                Err(e) => return Err(e),
            }
        };

        Ok(LoadStart {
            arc_modules,
            ident_ids_by_module,
            src_dir,
            root_id,
            root_msg,
        })
    }

    pub fn from_str(
        arena: &'a Bump,
        filename: PathBuf,
        src: &'a str,
        roc_cache_dir: RocCacheDir<'_>,
        src_dir: PathBuf,
    ) -> Result<Self, LoadingProblem<'a>> {
        let arc_modules = Arc::new(Mutex::new(PackageModuleIds::default()));
        let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
        let ident_ids_by_module = Arc::new(Mutex::new(root_exposed_ident_ids));

        // Load the root module synchronously; we can't proceed until we have its id.
        let (root_id, root_msg) = {
            let root_start_time = Instant::now();

            load_from_str(
                arena,
                filename,
                src,
                Arc::clone(&arc_modules),
                Arc::clone(&ident_ids_by_module),
                roc_cache_dir,
                root_start_time,
            )?
        };

        Ok(LoadStart {
            arc_modules,
            src_dir,
            ident_ids_by_module,
            root_id,
            root_msg,
        })
    }
}

pub enum LoadResult<'a> {
    TypeChecked(LoadedModule),
    Monomorphized(MonomorphizedModule<'a>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Threading {
    Single,
    AllAvailable,
    AtMost(usize),
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
///
///     a. Note that this might mean that we have to specialize certain modules multiple times.
///        When might this happen? Well, abilities can introduce implicit edges in the dependency
///        graph, and even cycles. For example, suppose module Ab provides "ability1" and a function
///        "f" that uses "ability1", and module App implements "ability1" and calls "f" with the
///        implementing type. Then the specialization of "Ab#f" depends on the specialization of
///        "ability1" back in the App module.
/// 12. Now that we have our final map of specializations, we can proceed to code gen!
///     As long as the specializations are stored in a per-ModuleId map, we can also
///     parallelize this code gen. (e.g. in dev builds, building separate LLVM modules
///     and then linking them together, and possibly caching them by the hash of their
///     specializations, so if none of their specializations changed, we don't even need
///     to rebuild the module and can link in the cached one directly.)
#[allow(clippy::too_many_arguments)]
pub fn load<'a>(
    arena: &'a Bump,
    load_start: LoadStart<'a>,
    exposed_types: ExposedByModule,
    cached_types: MutMap<ModuleId, TypeState>,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
) -> Result<LoadResult<'a>, LoadingProblem<'a>> {
    enum Threads {
        Single,
        Many(usize),
    }

    let threads = {
        if cfg!(target_family = "wasm") {
            // When compiling to wasm, we cannot spawn extra threads
            // so we have a single-threaded implementation
            Threads::Single
        } else {
            match std::thread::available_parallelism().map(|v| v.get()) {
                Err(_) => Threads::Single,
                Ok(0) => unreachable!("NonZeroUsize"),
                Ok(1) => Threads::Single,
                Ok(reported) => match load_config.threading {
                    Threading::Single => Threads::Single,
                    Threading::AllAvailable => Threads::Many(reported),
                    Threading::AtMost(at_most) => Threads::Many(Ord::min(reported, at_most)),
                },
            }
        }
    };

    match threads {
        Threads::Single => load_single_threaded(
            arena,
            load_start,
            exposed_types,
            load_config.target_info,
            cached_types,
            load_config.render,
            load_config.palette,
            load_config.exec_mode,
            roc_cache_dir,
        ),
        Threads::Many(threads) => load_multi_threaded(
            arena,
            load_start,
            exposed_types,
            load_config.target_info,
            cached_types,
            load_config.render,
            load_config.palette,
            threads,
            load_config.exec_mode,
            roc_cache_dir,
        ),
    }
}

/// Load using only a single thread; used when compiling to webassembly
#[allow(clippy::too_many_arguments)]
pub fn load_single_threaded<'a>(
    arena: &'a Bump,
    load_start: LoadStart<'a>,
    exposed_types: ExposedByModule,
    target_info: TargetInfo,
    cached_types: MutMap<ModuleId, TypeState>,
    render: RenderTarget,
    palette: Palette,
    exec_mode: ExecutionMode,
    roc_cache_dir: RocCacheDir<'_>,
) -> Result<LoadResult<'a>, LoadingProblem<'a>> {
    let LoadStart {
        arc_modules,
        ident_ids_by_module,
        root_id,
        root_msg,
        src_dir,
        ..
    } = load_start;

    let (msg_tx, msg_rx) = bounded(1024);

    msg_tx
        .send(root_msg)
        .map_err(|_| LoadingProblem::MsgChannelDied)?;

    let number_of_workers = 1;
    let mut state = State::new(
        root_id,
        target_info,
        exposed_types,
        arc_modules,
        ident_ids_by_module,
        cached_types,
        render,
        palette,
        number_of_workers,
        exec_mode,
    );

    // We'll add tasks to this, and then worker threads will take tasks from it.
    let injector = Injector::new();

    let (worker_msg_tx, worker_msg_rx) = bounded(1024);
    let worker_listener = worker_msg_tx;
    let worker_listeners = arena.alloc([worker_listener]);

    let worker = Worker::new_fifo();
    let stealer = worker.stealer();
    let stealers = &[stealer];

    // now we just manually interleave stepping the state "thread" and the worker "thread"
    loop {
        match state_thread_step(
            arena,
            state,
            &src_dir,
            worker_listeners,
            &injector,
            &msg_tx,
            &msg_rx,
        ) {
            Ok(ControlFlow::Break(done)) => return Ok(done),
            Ok(ControlFlow::Continue(new_state)) => {
                state = new_state;
            }
            Err(e) => return Err(e),
        }

        // then check if the worker can step
        let control_flow = worker_task_step(
            arena,
            &worker,
            &injector,
            stealers,
            &worker_msg_rx,
            &msg_tx,
            &src_dir,
            roc_cache_dir,
            target_info,
        );

        match control_flow {
            Ok(ControlFlow::Break(())) => panic!("the worker should not break!"),
            Ok(ControlFlow::Continue(())) => {
                // progress was made
            }
            Err(e) => return Err(e),
        }
    }
}

fn state_thread_step<'a>(
    arena: &'a Bump,
    state: State<'a>,
    src_dir: &Path,
    worker_listeners: &'a [Sender<WorkerMsg>],
    injector: &Injector<BuildTask<'a>>,
    msg_tx: &crossbeam::channel::Sender<Msg<'a>>,
    msg_rx: &crossbeam::channel::Receiver<Msg<'a>>,
) -> Result<ControlFlow<LoadResult<'a>, State<'a>>, LoadingProblem<'a>> {
    match msg_rx.try_recv() {
        Ok(msg) => {
            match msg {
                Msg::FinishedAllTypeChecking {
                    solved_subs,
                    exposed_vars_by_symbol,
                    exposed_aliases_by_symbol,
                    exposed_types_storage,
                    resolved_implementations,
                    dep_idents,
                    documentation,
                    abilities_store,
                } => {
                    // We're done! There should be no more messages pending.
                    debug_assert!(msg_rx.is_empty());

                    let exposed_aliases_by_symbol = exposed_aliases_by_symbol
                        .into_iter()
                        .map(|(k, (_, v))| (k, v))
                        .collect();

                    let typechecked = finish(
                        state,
                        solved_subs,
                        exposed_aliases_by_symbol,
                        exposed_vars_by_symbol,
                        exposed_types_storage,
                        resolved_implementations,
                        dep_idents,
                        documentation,
                        abilities_store,
                    );

                    Ok(ControlFlow::Break(LoadResult::TypeChecked(typechecked)))
                }
                Msg::FinishedAllSpecialization {
                    subs,
                    layout_interner,
                    exposed_to_host,
                } => {
                    // We're done! There should be no more messages pending.
                    debug_assert!(msg_rx.is_empty());

                    let monomorphized =
                        finish_specialization(state, subs, layout_interner, exposed_to_host)?;

                    Ok(ControlFlow::Break(LoadResult::Monomorphized(monomorphized)))
                }
                Msg::FailedToReadFile { filename, error } => {
                    let buf = to_file_problem_report(&filename, error);
                    Err(LoadingProblem::FormattedReport(buf))
                }

                Msg::FailedToParse(problem) => {
                    let module_ids = (*state.arc_modules).lock().clone().into_module_ids();
                    let buf = to_parse_problem_report(
                        problem,
                        module_ids,
                        state.constrained_ident_ids,
                        state.render,
                        state.palette,
                    );
                    Err(LoadingProblem::FormattedReport(buf))
                }
                Msg::IncorrectModuleName(FileError {
                    problem: SourceError { problem, bytes },
                    filename,
                }) => {
                    let module_ids = (*state.arc_modules).lock().clone().into_module_ids();
                    let buf = to_incorrect_module_name_report(
                        module_ids,
                        state.constrained_ident_ids,
                        problem,
                        filename,
                        bytes,
                        state.render,
                    );
                    Err(LoadingProblem::FormattedReport(buf))
                }
                msg => {
                    // This is where most of the main thread's work gets done.
                    // Everything up to this point has been setting up the threading
                    // system which lets this logic work efficiently.
                    let arc_modules = state.arc_modules.clone();

                    let render = state.render;
                    let palette = state.palette;

                    let res_state = update(
                        state,
                        src_dir,
                        msg,
                        msg_tx.clone(),
                        injector,
                        worker_listeners,
                        arena,
                    );

                    match res_state {
                        Ok(new_state) => Ok(ControlFlow::Continue(new_state)),
                        Err(LoadingProblem::ParsingFailed(problem)) => {
                            let module_ids = Arc::try_unwrap(arc_modules)
                                .unwrap_or_else(|_| {
                                    panic!(
                                        r"There were still outstanding Arc references to module_ids"
                                    )
                                })
                                .into_inner()
                                .into_module_ids();

                            // if parsing failed, this module did not add anything to IdentIds
                            let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
                            let buf = to_parse_problem_report(
                                problem,
                                module_ids,
                                root_exposed_ident_ids,
                                render,
                                palette,
                            );
                            Err(LoadingProblem::FormattedReport(buf))
                        }
                        Err(LoadingProblem::ImportCycle(filename, cycle)) => {
                            let module_ids = arc_modules.lock().clone().into_module_ids();

                            let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
                            let buf = to_import_cycle_report(
                                module_ids,
                                root_exposed_ident_ids,
                                cycle,
                                filename,
                                render,
                            );
                            return Err(LoadingProblem::FormattedReport(buf));
                        }
                        Err(LoadingProblem::IncorrectModuleName(FileError {
                            problem: SourceError { problem, bytes },
                            filename,
                        })) => {
                            let module_ids = arc_modules.lock().clone().into_module_ids();

                            let root_exposed_ident_ids = IdentIds::exposed_builtins(0);
                            let buf = to_incorrect_module_name_report(
                                module_ids,
                                root_exposed_ident_ids,
                                problem,
                                filename,
                                bytes,
                                render,
                            );
                            return Err(LoadingProblem::FormattedReport(buf));
                        }
                        Err(e) => Err(e),
                    }
                }
            }
        }
        Err(err) => match err {
            crossbeam::channel::TryRecvError::Empty => Ok(ControlFlow::Continue(state)),
            crossbeam::channel::TryRecvError::Disconnected => Err(LoadingProblem::MsgChannelDied),
        },
    }
}

#[allow(clippy::too_many_arguments)]
fn load_multi_threaded<'a>(
    arena: &'a Bump,
    load_start: LoadStart<'a>,
    exposed_types: ExposedByModule,
    target_info: TargetInfo,
    cached_types: MutMap<ModuleId, TypeState>,
    render: RenderTarget,
    palette: Palette,
    available_threads: usize,
    exec_mode: ExecutionMode,
    roc_cache_dir: RocCacheDir<'_>,
) -> Result<LoadResult<'a>, LoadingProblem<'a>> {
    let LoadStart {
        arc_modules,
        ident_ids_by_module,
        root_id,
        root_msg,
        src_dir,
        ..
    } = load_start;

    let (msg_tx, msg_rx) = bounded(1024);
    msg_tx
        .send(root_msg)
        .map_err(|_| LoadingProblem::MsgChannelDied)?;

    // Reserve one CPU for the main thread, and let all the others be eligible
    // to spawn workers.
    let available_workers = available_threads - 1;

    let num_workers = match env::var("ROC_NUM_WORKERS") {
        Ok(env_str) => env_str
            .parse::<usize>()
            .unwrap_or(available_workers)
            .min(available_workers),
        Err(_) => available_workers,
    };

    assert!(
        num_workers >= 1,
        "`load_multi_threaded` needs at least one worker"
    );

    let mut state = State::new(
        root_id,
        target_info,
        exposed_types,
        arc_modules,
        ident_ids_by_module,
        cached_types,
        render,
        palette,
        num_workers,
        exec_mode,
    );

    // an arena for every worker, stored in an arena-allocated bumpalo vec to make the lifetimes work
    let arenas = std::iter::repeat_with(Bump::new).take(num_workers);
    let worker_arenas = arena.alloc(bumpalo::collections::Vec::from_iter_in(arenas, arena));

    // We'll add tasks to this, and then worker threads will take tasks from it.
    let injector = Injector::new();

    // We need to allocate worker *queues* on the main thread and then move them
    // into the worker threads, because those workers' stealers need to be
    // shared between all threads, and this coordination work is much easier
    // on the main thread.
    let mut worker_queues = bumpalo::collections::Vec::with_capacity_in(num_workers, arena);
    let mut stealers = bumpalo::collections::Vec::with_capacity_in(num_workers, arena);

    for _ in 0..num_workers {
        let worker = Worker::new_fifo();

        stealers.push(worker.stealer());
        worker_queues.push(worker);
    }

    // Get a reference to the completed stealers, so we can send that
    // reference to each worker. (Slices are Sync, but bumpalo Vecs are not.)
    let stealers = stealers.into_bump_slice();

    let it = worker_arenas.iter_mut();
    {
        thread::scope(|thread_scope| {
            let mut worker_listeners =
                bumpalo::collections::Vec::with_capacity_in(num_workers, arena);

            for worker_arena in it {
                let msg_tx = msg_tx.clone();
                let worker = worker_queues.pop().unwrap();

                let (worker_msg_tx, worker_msg_rx) = bounded(1024);
                worker_listeners.push(worker_msg_tx);

                // We only want to move a *reference* to the main task queue's
                // injector in the thread, not the injector itself
                // (since other threads need to reference it too). Same with src_dir.
                let injector = &injector;
                let src_dir = &src_dir;

                // Record this thread's handle so the main thread can join it later.
                let res_join_handle = thread_scope
                    .builder()
                    .stack_size(EXPANDED_STACK_SIZE)
                    .spawn(move |_| {
                        // will process messages until we run out
                        worker_task(
                            worker_arena,
                            worker,
                            injector,
                            stealers,
                            worker_msg_rx,
                            msg_tx,
                            src_dir,
                            roc_cache_dir,
                            target_info,
                        )
                    });

                res_join_handle.unwrap();
            }

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
            loop {
                match state_thread_step(
                    arena,
                    state,
                    &src_dir,
                    worker_listeners,
                    &injector,
                    &msg_tx,
                    &msg_rx,
                ) {
                    Ok(ControlFlow::Break(load_result)) => {
                        shut_down_worker_threads!();

                        return Ok(load_result);
                    }
                    Ok(ControlFlow::Continue(new_state)) => {
                        state = new_state;
                        continue;
                    }
                    Err(e) => {
                        shut_down_worker_threads!();

                        return Err(e);
                    }
                }
            }
        })
    }
    .unwrap()
}

#[allow(clippy::too_many_arguments)]
fn worker_task_step<'a>(
    worker_arena: &'a Bump,
    worker: &Worker<BuildTask<'a>>,
    injector: &Injector<BuildTask<'a>>,
    stealers: &[Stealer<BuildTask<'a>>],
    worker_msg_rx: &crossbeam::channel::Receiver<WorkerMsg>,
    msg_tx: &MsgSender<'a>,
    src_dir: &Path,
    roc_cache_dir: RocCacheDir<'_>,
    target_info: TargetInfo,
) -> Result<ControlFlow<(), ()>, LoadingProblem<'a>> {
    match worker_msg_rx.try_recv() {
        Ok(msg) => {
            match msg {
                WorkerMsg::Shutdown => {
                    // We've finished all our work. It's time to
                    // shut down the thread, so when the main thread
                    // blocks on joining with all the worker threads,
                    // it can finally exit too!
                    Ok(ControlFlow::Break(()))
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
                    if let Some(task) = find_task(worker, injector, stealers) {
                        let result = run_task(
                            task,
                            worker_arena,
                            src_dir,
                            msg_tx.clone(),
                            roc_cache_dir,
                            target_info,
                        );

                        match result {
                            Ok(()) => {}
                            Err(LoadingProblem::MsgChannelDied) => {
                                panic!("Msg channel closed unexpectedly.")
                            }
                            Err(LoadingProblem::ParsingFailed(problem)) => {
                                msg_tx.send(Msg::FailedToParse(problem)).unwrap();
                            }
                            Err(LoadingProblem::FileProblem { filename, error }) => {
                                msg_tx
                                    .send(Msg::FailedToReadFile { filename, error })
                                    .unwrap();
                            }
                            Err(LoadingProblem::IncorrectModuleName(err)) => {
                                msg_tx.send(Msg::IncorrectModuleName(err)).unwrap();
                            }
                            Err(other) => {
                                return Err(other);
                            }
                        }
                    }

                    Ok(ControlFlow::Continue(()))
                }
            }
        }
        Err(err) => match err {
            crossbeam::channel::TryRecvError::Empty => Ok(ControlFlow::Continue(())),
            crossbeam::channel::TryRecvError::Disconnected => Ok(ControlFlow::Break(())),
        },
    }
}

#[allow(clippy::too_many_arguments)]
fn worker_task<'a>(
    worker_arena: &'a Bump,
    worker: Worker<BuildTask<'a>>,
    injector: &Injector<BuildTask<'a>>,
    stealers: &[Stealer<BuildTask<'a>>],
    worker_msg_rx: crossbeam::channel::Receiver<WorkerMsg>,
    msg_tx: MsgSender<'a>,
    src_dir: &Path,
    roc_cache_dir: RocCacheDir<'_>,
    target_info: TargetInfo,
) -> Result<(), LoadingProblem<'a>> {
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
                        roc_cache_dir,
                        target_info,
                    );

                    match result {
                        Ok(()) => {}
                        Err(LoadingProblem::MsgChannelDied) => {
                            panic!("Msg channel closed unexpectedly.")
                        }
                        Err(LoadingProblem::ParsingFailed(problem)) => {
                            msg_tx.send(Msg::FailedToParse(problem)).unwrap();
                        }
                        Err(LoadingProblem::FileProblem { filename, error }) => {
                            msg_tx
                                .send(Msg::FailedToReadFile { filename, error })
                                .unwrap();
                        }
                        Err(LoadingProblem::IncorrectModuleName(err)) => {
                            msg_tx.send(Msg::IncorrectModuleName(err)).unwrap();
                        }
                        Err(other) => {
                            return Err(other);
                        }
                    }
                }
            }
        }
    }

    Ok(())
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

macro_rules! debug_print_ir {
    ($state:expr, $interner:expr, $flag:path) => {
        dbg_do!($flag, {
            let procs_string = $state
                .procedures
                .values()
                .map(|proc| proc.to_pretty($interner, 200))
                .collect::<Vec<_>>();

            let result = procs_string.join("\n");

            eprintln!("{}", result);
        })
    };
}

/// Report modules that are imported, but from which nothing is used
fn report_unused_imported_modules<'a>(
    state: &mut State<'a>,
    module_id: ModuleId,
    constrained_module: &ConstrainedModule,
) {
    let mut unused_imported_modules = constrained_module.imported_modules.clone();
    let mut unused_imports = constrained_module.module.exposed_imports.clone();

    for symbol in constrained_module.module.referenced_values.iter() {
        unused_imported_modules.remove(&symbol.module_id());
        unused_imports.remove(symbol);
    }

    for symbol in constrained_module.module.referenced_types.iter() {
        unused_imported_modules.remove(&symbol.module_id());
        unused_imports.remove(symbol);
    }

    let existing = match state.module_cache.can_problems.entry(module_id) {
        Vacant(entry) => entry.insert(std::vec::Vec::new()),
        Occupied(entry) => entry.into_mut(),
    };

    for (unused, region) in unused_imported_modules.drain() {
        if !unused.is_builtin() {
            existing.push(roc_problem::can::Problem::UnusedModuleImport(
                unused, region,
            ));
        }
    }

    for (unused, region) in unused_imports.drain() {
        existing.push(roc_problem::can::Problem::UnusedImport(unused, region));
    }
}

fn extend_header_with_builtin(header: &mut ModuleHeader, module: ModuleId) {
    header
        .package_qualified_imported_modules
        .insert(PackageQualified::Unqualified(module));

    header.imported_modules.insert(module, Region::zero());

    let types = Symbol::builtin_types_in_scope(module)
        .iter()
        .map(|(name, info)| (Ident::from(*name), *info));
    header.exposed_imports.extend(types);
}

fn update<'a>(
    mut state: State<'a>,
    src_dir: &Path,
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
        Header(header) => {
            use HeaderFor::*;

            log!("loaded header for {:?}", header.module_id);
            let home = header.module_id;

            let mut work = MutSet::default();

            // Register the package's path under its shorthand
            // (e.g. for { pf: "blah" }, register that "pf" should resolve to "blah")
            {
                let mut shorthands = (*state.arc_shorthands).lock();

                for (shorthand, package_name) in header.packages.iter() {
                    let package_str = package_name.as_str();
                    let shorthand_path = if package_str.starts_with("https://") {
                        #[cfg(not(target_family = "wasm"))]
                        {
                            let url = package_str;
                            match PackageMetadata::try_from(url) {
                                Ok(url_metadata) => {
                                    // This was a valid URL
                                    let root_module_dir = state
                                        .cache_dir
                                        .join(url_metadata.cache_subdir)
                                        .join(url_metadata.content_hash);
                                    let root_module = root_module_dir.join(
                                        url_metadata.root_module_filename.unwrap_or("main.roc"),
                                    );

                                    ShorthandPath::FromHttpsUrl {
                                        root_module_dir,
                                        root_module,
                                    }
                                }
                                Err(url_err) => {
                                    todo!(
                                        "Gracefully report URL error for {:?} - {:?}",
                                        url,
                                        url_err
                                    );
                                }
                            }
                        }

                        #[cfg(target_family = "wasm")]
                        {
                            panic!("Specifying packages via URLs is curently unsupported in wasm.");
                        }
                    } else {
                        // This wasn't a URL, so it must be a filesystem path.
                        let root_module: PathBuf = src_dir.join(package_str);
                        let root_module_dir = root_module.parent().unwrap_or_else(|| {
                                if root_module.is_file() {
                                    // Files must have parents!
                                    internal_error!("Somehow I got a file path to a real file on the filesystem that has no parent!");
                                } else {
                                    // TODO make this a nice report
                                    todo!(
                                        "platform module {:?} was not a file.",
                                        package_str
                                    )
                                }
                            }).into();

                        ShorthandPath::RelativeToSrc {
                            root_module_dir,
                            root_module,
                        }
                    };

                    shorthands.insert(shorthand, shorthand_path);
                }

                match header.header_for {
                    App { to_platform } => {
                        debug_assert!(matches!(state.platform_path, PlatformPath::NotSpecified));
                        state.platform_path = PlatformPath::Valid(to_platform);
                    }
                    Platform {
                        main_for_host,
                        config_shorthand,
                        ..
                    } => {
                        debug_assert!(matches!(state.platform_data, None));

                        work.extend(state.dependencies.notify_package(config_shorthand));

                        let is_prebuilt = if header.is_root_module {
                            debug_assert!(matches!(
                                state.platform_path,
                                PlatformPath::NotSpecified
                            ));
                            state.platform_path = PlatformPath::RootIsPlatformModule;

                            // If the root module is a platform, then the platform is the very
                            // thing we're rebuilding!
                            false
                        } else {
                            // platforms from HTTPS URLs are always prebuilt
                            matches!(
                                shorthands.get(config_shorthand),
                                Some(ShorthandPath::FromHttpsUrl { .. })
                            )
                        };

                        state.platform_data = Some(PlatformData {
                            module_id: header.module_id,
                            provides: main_for_host,
                            is_prebuilt,
                        });
                    }
                    Builtin { .. } | Interface => {
                        if header.is_root_module {
                            debug_assert!(matches!(
                                state.platform_path,
                                PlatformPath::NotSpecified
                            ));
                            state.platform_path = PlatformPath::RootIsInterface;
                        }
                    }
                    Hosted { .. } => {
                        if header.is_root_module {
                            debug_assert!(matches!(
                                state.platform_path,
                                PlatformPath::NotSpecified
                            ));
                            state.platform_path = PlatformPath::RootIsHosted;
                        }
                    }
                }
            }

            // store an ID to name mapping, so we know the file to read when fetching dependencies' headers
            for (name, id) in header.deps_by_name.iter() {
                state.module_cache.module_names.insert(*id, name.clone());
            }

            // This was a dependency. Write it down and keep processing messages.
            let mut exposed_symbols: VecSet<Symbol> = VecSet::with_capacity(header.exposes.len());

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

            // add the prelude
            let mut header = header;

            if !header.module_id.is_builtin() {
                let header = &mut header;

                extend_header_with_builtin(header, ModuleId::NUM);
                extend_header_with_builtin(header, ModuleId::BOOL);
                extend_header_with_builtin(header, ModuleId::STR);
                extend_header_with_builtin(header, ModuleId::LIST);
                extend_header_with_builtin(header, ModuleId::RESULT);
                extend_header_with_builtin(header, ModuleId::DICT);
                extend_header_with_builtin(header, ModuleId::SET);
                extend_header_with_builtin(header, ModuleId::BOX);
                extend_header_with_builtin(header, ModuleId::ENCODE);
                extend_header_with_builtin(header, ModuleId::DECODE);
                extend_header_with_builtin(header, ModuleId::HASH);
            }

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

            let added_deps_result = state.dependencies.add_module(
                header.module_id,
                &header.package_qualified_imported_modules,
                state.exec_mode.goal_phase(),
            );

            let new_work = match added_deps_result {
                Ok(work) => work,
                Err(DepCycle { cycle }) => {
                    return Err(LoadingProblem::ImportCycle(
                        header.module_path.clone(),
                        cycle,
                    ));
                }
            };

            work.extend(new_work);

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
                ModuleNameEnum::App(output_str) => match output_str {
                    StrLiteral::PlainLine(path) => {
                        state.output_path = Some(path);
                    }
                    _ => {
                        todo!("TODO gracefully handle a malformed string literal after `app` keyword.");
                    }
                },
                ModuleNameEnum::Platform
                | ModuleNameEnum::Interface(_)
                | ModuleNameEnum::Hosted(_) => {}
            }

            let module_id = parsed.module_id;

            state.module_cache.parsed.insert(parsed.module_id, parsed);

            let work = state.dependencies.notify(module_id, Phase::Parse);

            start_tasks(arena, &mut state, work, injector, worker_listeners)?;

            Ok(state)
        }

        CanonicalizedAndConstrained(CanAndCon {
            constrained_module,
            canonicalization_problems,
            module_docs,
        }) => {
            let module_id = constrained_module.module.module_id;
            log!("generated constraints for {:?}", module_id);
            state
                .module_cache
                .can_problems
                .insert(module_id, canonicalization_problems);

            if let Some(docs) = module_docs {
                state.module_cache.documentation.insert(module_id, docs);
            }

            report_unused_imported_modules(&mut state, module_id, &constrained_module);

            state
                .module_cache
                .aliases
                .insert(module_id, constrained_module.module.aliases.clone());

            state
                .module_cache
                .pending_abilities
                .insert(module_id, constrained_module.module.abilities_store.clone());

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
        SolvedTypes {
            module_id,
            ident_ids,
            solved_module,
            solved_subs,
            decls,
            dep_idents,
            mut module_timing,
            abilities_store,
            loc_expects,
            loc_dbgs,
        } => {
            log!("solved types for {:?}", module_id);
            module_timing.end_time = Instant::now();

            state
                .module_cache
                .type_problems
                .insert(module_id, solved_module.problems);

            let should_include_expects = (!loc_expects.is_empty() || !loc_dbgs.is_empty()) && {
                let modules = state.arc_modules.lock();
                modules
                    .package_eq(module_id, state.root_id)
                    .expect("root or this module is not yet known - that's a bug!")
            };

            if should_include_expects {
                let (path, _) = state.module_cache.sources.get(&module_id).unwrap();

                let expectations = Expectations {
                    expectations: loc_expects,
                    dbgs: loc_dbgs,
                    subs: solved_subs.clone().into_inner(),
                    path: path.to_owned(),
                    ident_ids: ident_ids.clone(),
                };

                state
                    .module_cache
                    .expectations
                    .insert(module_id, expectations);
            }

            let work = state.dependencies.notify(module_id, Phase::SolveTypes);

            // if there is a platform, the `platform` module provides host-exposed,
            // otherwise the App module exposes host-exposed
            let is_host_exposed = match state.platform_data {
                None => module_id == state.root_id,
                Some(ref platform_data) => module_id == platform_data.module_id,
            };

            let add_to_host_exposed = is_host_exposed &&
                // During testing, we don't need to expose anything to the host.
                !matches!(state.exec_mode, ExecutionMode::Test);

            if add_to_host_exposed {
                state.exposed_to_host.values.extend(
                    solved_module
                        .exposed_vars_by_symbol
                        .iter()
                        .filter_map(|(k, v)| {
                            if abilities_store.is_specialization_name(*k) {
                                None
                            } else {
                                Some((*k, *v))
                            }
                        }),
                );

                state
                    .exposed_to_host
                    .closure_types
                    .extend(solved_module.aliases.keys().copied());
            }

            let finish_type_checking = is_host_exposed &&
                (state.goal_phase() == Phase::SolveTypes)
                // If we're running in check-and-then-build mode, only exit now there are errors.
                && (!matches!(state.exec_mode, ExecutionMode::ExecutableIfCheck) || state.module_cache.total_problems() > 0);

            if finish_type_checking {
                debug_assert!(work.is_empty());
                debug_assert!(state.dependencies.solved_all());

                state.timings.insert(module_id, module_timing);

                if matches!(state.exec_mode, ExecutionMode::ExecutableIfCheck) {
                    // We there may outstanding modules in the typecheked cache whose ident IDs
                    // aren't registered; transfer all of their idents over to the state, since
                    // we're now done and ready to report errors.
                    for (
                        module_id,
                        TypeCheckedModule {
                            ident_ids,
                            module_timing,
                            ..
                        },
                    ) in state.module_cache.typechecked.drain()
                    {
                        state.constrained_ident_ids.insert(module_id, ident_ids);
                        state.timings.insert(module_id, module_timing);
                    }
                }

                let documentation = {
                    let mut empty = MutMap::default();
                    std::mem::swap(&mut empty, &mut state.module_cache.documentation);

                    empty
                };

                msg_tx
                    .send(Msg::FinishedAllTypeChecking {
                        solved_subs,
                        exposed_vars_by_symbol: solved_module.exposed_vars_by_symbol,
                        exposed_aliases_by_symbol: solved_module.aliases,
                        exposed_types_storage: solved_module.exposed_types,
                        resolved_implementations: solved_module.solved_implementations,
                        dep_idents,
                        documentation,
                        abilities_store,
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
                    ExposedModuleTypes {
                        exposed_types_storage_subs: solved_module.exposed_types,
                        resolved_implementations: solved_module.solved_implementations,
                    },
                );

                if state.goal_phase() > Phase::SolveTypes
                    || matches!(state.exec_mode, ExecutionMode::ExecutableIfCheck)
                {
                    let layout_cache = state.layout_caches.pop().unwrap_or_else(|| {
                        LayoutCache::new(state.layout_interner.fork(), state.target_info)
                    });

                    let typechecked = TypeCheckedModule {
                        module_id,
                        layout_cache,
                        module_timing,
                        solved_subs,
                        decls,
                        ident_ids,
                        abilities_store,
                    };

                    state
                        .module_cache
                        .typechecked
                        .insert(module_id, typechecked);
                } else {
                    state.constrained_ident_ids.insert(module_id, ident_ids);
                    state.timings.insert(module_id, module_timing);
                }

                let work = if is_host_exposed
                    && matches!(state.exec_mode, ExecutionMode::ExecutableIfCheck)
                {
                    debug_assert!(
                        work.is_empty(),
                        "work left over after host exposed is checked"
                    );

                    // Update the goal phase to target full codegen.
                    state.exec_mode = ExecutionMode::Executable;

                    // Load the find + make specializations portion of the dependency graph.
                    state
                        .dependencies
                        .load_find_and_make_specializations_after_check()
                } else {
                    work
                };

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
            module_timing,
            abilities_store,
            toplevel_expects,
        } => {
            log!("found specializations for {:?}", module_id);

            let subs = solved_subs.into_inner();

            state.toplevel_expects.pure.extend(toplevel_expects.pure);
            state.toplevel_expects.fx.extend(toplevel_expects.fx);

            state
                .module_cache
                .top_level_thunks
                .entry(module_id)
                .or_default()
                .extend(procs_base.module_thunks.iter().copied());

            let found_specializations_module = FoundSpecializationsModule {
                ident_ids,
                layout_cache,
                procs_base,
                subs,
                module_timing,
                abilities_store,
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
            ident_ids,
            mut update_mode_ids,
            subs,
            procs_base,
            procedures,
            external_specializations_requested,
            module_timing,
            layout_cache,
            ..
        } => {
            debug_assert!(state.goal_phase() == Phase::MakeSpecializations);

            log!("made specializations for {:?}", module_id);

            // in the future, layouts will be in SoA form and we'll want to hold on to this data
            let _ = layout_cache;

            state.procedures.extend(procedures);
            state.module_cache.late_specializations.insert(
                module_id,
                LateSpecializationsModule {
                    ident_ids,
                    module_timing,
                    subs,
                    layout_cache,
                    procs_base,
                },
            );

            let work = state
                .dependencies
                .notify(module_id, Phase::MakeSpecializations);

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

            enum NextStep {
                Done,
                RelaunchPhase,
                MakingInPhase,
            }

            let all_work_done = work.is_empty() && state.dependencies.solved_all();
            let next_step = if all_work_done {
                if state
                    .module_cache
                    .external_specializations_requested
                    .is_empty()
                {
                    NextStep::Done
                } else {
                    NextStep::RelaunchPhase
                }
            } else {
                NextStep::MakingInPhase
            };

            match next_step {
                NextStep::Done => {
                    // We are all done with specializations across all modules.
                    // Insert post-specialization operations and report our completion.

                    if !state
                        .module_cache
                        .external_specializations_requested
                        .is_empty()
                    {
                        internal_error!(
                            "No more work left, but external specializations left over: {:?}",
                            state.module_cache.external_specializations_requested
                        );
                    }

                    // Flush late-specialization module information to the top-level of the state
                    // where it will be visible to others, since we don't need late specialization
                    // anymore.
                    for (
                        module_id,
                        LateSpecializationsModule {
                            ident_ids,
                            subs,
                            module_timing,
                            layout_cache: _layout_cache,
                            procs_base: _,
                        },
                    ) in state.module_cache.late_specializations.drain()
                    {
                        state.constrained_ident_ids.insert(module_id, ident_ids);
                        if module_id == state.root_id {
                            state.root_subs = Some(subs);
                        }
                        state.timings.insert(module_id, module_timing);

                        #[cfg(debug_assertions)]
                        {
                            log_layout_stats(module_id, &_layout_cache);
                        }
                    }

                    let layout_interner = {
                        let mut taken = GlobalInterner::with_capacity(0);
                        std::mem::swap(&mut state.layout_interner, &mut taken);
                        taken
                    };
                    let layout_interner = layout_interner
                        .unwrap()
                        .expect("outstanding references to global layout interener, but we just drained all layout caches");

                    log!("specializations complete from {:?}", module_id);

                    debug_print_ir!(state, &layout_interner, ROC_PRINT_IR_AFTER_SPECIALIZATION);

                    let ident_ids = state.constrained_ident_ids.get_mut(&module_id).unwrap();

                    Proc::insert_reset_reuse_operations(
                        arena,
                        module_id,
                        ident_ids,
                        &mut update_mode_ids,
                        &mut state.procedures,
                    );

                    debug_print_ir!(state, &layout_interner, ROC_PRINT_IR_AFTER_RESET_REUSE);

                    Proc::insert_refcount_operations(
                        arena,
                        &layout_interner,
                        module_id,
                        ident_ids,
                        &mut update_mode_ids,
                        &mut state.procedures,
                    );

                    debug_print_ir!(state, &layout_interner, ROC_PRINT_IR_AFTER_REFCOUNT);

                    // This is not safe with the new non-recursive RC updates that we do for tag unions
                    //
                    // Proc::optimize_refcount_operations(
                    //     arena,
                    //     module_id,
                    //     &mut ident_ids,
                    //     &mut state.procedures,
                    // );

                    // use the subs of the root module;
                    // this is used in the repl to find the type of `main`
                    let subs = state.root_subs.clone().unwrap();

                    msg_tx
                        .send(Msg::FinishedAllSpecialization {
                            subs,
                            layout_interner,
                            exposed_to_host: state.exposed_to_host.clone(),
                        })
                        .map_err(|_| LoadingProblem::MsgChannelDied)?;

                    Ok(state)
                }

                NextStep::RelaunchPhase => {
                    // We passed through the dependency graph of modules to be specialized, but
                    // there are still specializations left over. Restart the make specializations
                    // phase in reverse topological order.
                    //
                    // This happens due to abilities. In detail, consider
                    //
                    //   # Default module
                    //   interface Default exposes [default, getDefault]
                    //
                    //   Default has default : {} -> a | a has Default
                    //
                    //   getDefault = \{} -> default {}
                    //
                    //   # App module
                    //   app "test" provides [main] imports [Default.{default, getDefault}]
                    //
                    //   Foo := {}
                    //
                    //   default = \{} -> @Foo {}
                    //
                    //   main =
                    //     f : Foo
                    //     f = getDefault {}
                    //     f
                    //
                    // The syntactic make specializations graph (based on imports) will be
                    // App -> Default, and in a pass will build the specializations `App#main` and
                    // `Default#getDefault for Foo`. But now notice that `Default#getDefault` will
                    // have gained an implicit dependency on the specialized `default` for `Foo`,
                    // `App#Foo#default`. So for abilities, the syntactic import graph is not
                    // enough to express the entire dependency graph.
                    //
                    // The simplest way to resolve these leftover, possibly circular
                    // specializations is to relaunch the make-specializations phase in the import
                    // order until there are no more specializations left to be made. This is a bit
                    // unfortunate in that we may look again into modules that don't need any
                    // specializations made, but there are also some nice properties:
                    //
                    // - no more specializations will be made than needed
                    // - the number of phase relaunches scales linearly with the largest number of
                    //   "bouncing back and forth" between ability calls, which is likely to be
                    //   small in practice
                    // - the phases will always terminate. suppose they didn't; then there must be
                    //   an infinite chain of calls all of which have different layouts. In Roc
                    //   this can only be true if the calls are all mutually recursive, and
                    //   furthermore are polymorphically recursive. But polymorphic recursion is
                    //   illegal in Roc, will have been enforced during type inference.

                    if state
                        .module_cache
                        .external_specializations_requested
                        .is_empty()
                    {
                        internal_error!(
                            "No specializations left over, but we were told to loop making specializations"
                        );
                    }

                    log!("re-launching specializations pass");

                    state.make_specializations_pass.inc();

                    let work = state.dependencies.reload_make_specialization_pass();

                    start_tasks(arena, &mut state, work, injector, worker_listeners)?;

                    Ok(state)
                }

                NextStep::MakingInPhase => {
                    start_tasks(arena, &mut state, work, injector, worker_listeners)?;

                    Ok(state)
                }
            }
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
        Msg::IncorrectModuleName(..) => {
            internal_error!();
        }
    }
}

#[cfg(debug_assertions)]
fn log_layout_stats(module_id: ModuleId, layout_cache: &LayoutCache) {
    let (cache_stats, raw_function_cache_stats) = layout_cache.statistics();
    roc_tracing::info!(
        module = ?module_id,
        insertions = cache_stats.insertions,
        hits = cache_stats.hits,
        misses = cache_stats.misses,
        non_insertable = cache_stats.non_insertable,
        non_reusable = cache_stats.non_reusable,
        "cache stats"
    );
    roc_tracing::info!(
        module = ?module_id,
        insertions = raw_function_cache_stats.insertions,
        hits = raw_function_cache_stats.hits,
        misses = raw_function_cache_stats.misses,
        non_insertable = raw_function_cache_stats.non_insertable,
        non_reusable = raw_function_cache_stats.non_reusable,
        "raw function cache stats"
    );
}

fn finish_specialization<'a>(
    state: State<'a>,
    subs: Subs,
    layout_interner: STLayoutInterner<'a>,
    exposed_to_host: ExposedToHost,
) -> Result<MonomorphizedModule<'a>, LoadingProblem<'a>> {
    if false {
        println!(
            "total Type clones: {} ",
            roc_types::types::get_type_clone_count()
        );
    }
    let module_ids = Arc::try_unwrap(state.arc_modules)
        .unwrap_or_else(|_| panic!("There were still outstanding Arc references to module_ids"))
        .into_inner()
        .into_module_ids();

    let mut all_ident_ids = state.constrained_ident_ids;

    // Associate the ident IDs from the derived synth module
    let (_, derived_synth_ident_ids) = Arc::try_unwrap(state.derived_module)
        .unwrap_or_else(|_| internal_error!("Outstanding references to the derived module"))
        .into_inner()
        .unwrap()
        .decompose();
    ModuleId::DERIVED_SYNTH.register_debug_idents(&derived_synth_ident_ids);
    all_ident_ids.insert(ModuleId::DERIVED_SYNTH, derived_synth_ident_ids);

    let interns = Interns {
        module_ids,
        all_ident_ids,
    };

    let State {
        toplevel_expects,
        procedures,
        module_cache,
        output_path,
        platform_path,
        platform_data,
        exec_mode,
        ..
    } = state;

    let ModuleCache {
        expectations,
        type_problems,
        can_problems,
        sources,
        ..
    } = module_cache;

    let sources: MutMap<ModuleId, (PathBuf, Box<str>)> = sources
        .into_iter()
        .map(|(id, (path, src))| (id, (path, src.into())))
        .collect();

    let entry_point = {
        match exec_mode {
            ExecutionMode::Test => EntryPoint::Test,
            ExecutionMode::Executable | ExecutionMode::ExecutableIfCheck => {
                use PlatformPath::*;
                let platform_path = match platform_path {
                    Valid(To::ExistingPackage(shorthand)) => {
                        match (*state.arc_shorthands).lock().get(shorthand) {
                            Some(shorthand_path) => shorthand_path.root_module().to_path_buf(),
                            None => unreachable!(),
                        }
                    }
                    Valid(To::NewPackage(p_or_p)) => PathBuf::from(p_or_p.as_str()),
                    other => {
                        let buf = to_missing_platform_report(state.root_id, other);
                        return Err(LoadingProblem::FormattedReport(buf));
                    }
                };

                let symbol = match platform_data {
                    None => {
                        debug_assert_eq!(exposed_to_host.values.len(), 1);
                        *exposed_to_host.values.iter().next().unwrap().0
                    }
                    Some(PlatformData { provides, .. }) => provides,
                };

                match procedures.keys().find(|(s, _)| *s == symbol) {
                    Some((_, layout)) => EntryPoint::Executable {
                        layout: *layout,
                        symbol,
                        platform_path,
                    },
                    None => {
                        // the entry point is not specialized. This can happen if the repl output
                        // is a function value
                        EntryPoint::Executable {
                            layout: roc_mono::ir::ProcLayout {
                                arguments: &[],
                                result: Layout::struct_no_name_order(&[]),
                                captures_niche: CapturesNiche::no_niche(),
                            },
                            symbol,
                            platform_path,
                        }
                    }
                }
            }
            ExecutionMode::Check => unreachable!(),
        }
    };

    let output_path = match output_path {
        Some(path_str) => Path::new(path_str).into(),
        None => current_dir().unwrap().join(DEFAULT_APP_OUTPUT_PATH).into(),
    };

    let uses_prebuilt_platform = match platform_data {
        Some(data) => data.is_prebuilt,
        // If there's no platform data (e.g. because we're building an interface module)
        // then there's no prebuilt platform either!
        None => false,
    };

    Ok(MonomorphizedModule {
        can_problems,
        type_problems,
        output_path,
        expectations,
        exposed_to_host,
        module_id: state.root_id,
        subs,
        interns,
        layout_interner,
        procedures,
        entry_point,
        sources,
        timings: state.timings,
        toplevel_expects,
        uses_prebuilt_platform,
    })
}

#[allow(clippy::too_many_arguments)]
fn finish(
    mut state: State,
    solved: Solved<Subs>,
    exposed_aliases_by_symbol: MutMap<Symbol, Alias>,
    exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
    exposed_types_storage: ExposedTypesStorageSubs,
    resolved_implementations: ResolvedImplementations,
    dep_idents: IdentIdsByModule,
    documentation: MutMap<ModuleId, ModuleDocumentation>,
    abilities_store: AbilitiesStore,
) -> LoadedModule {
    let module_ids = Arc::try_unwrap(state.arc_modules)
        .unwrap_or_else(|_| panic!("There were still outstanding Arc references to module_ids"))
        .into_inner()
        .into_module_ids();

    // Associate the ident IDs from the derived synth module
    let (_, derived_synth_ident_ids) = Arc::try_unwrap(state.derived_module)
        .unwrap_or_else(|_| internal_error!("Outstanding references to the derived module"))
        .into_inner()
        .unwrap()
        .decompose();
    ModuleId::DERIVED_SYNTH.register_debug_idents(&derived_synth_ident_ids);
    state
        .constrained_ident_ids
        .insert(ModuleId::DERIVED_SYNTH, derived_synth_ident_ids);

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

    let exposed_values = exposed_vars_by_symbol.iter().map(|x| x.0).collect();

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
        exposed_types_storage,
        resolved_implementations,
        sources,
        timings: state.timings,
        documentation,
        abilities_store,
    }
}

/// Load a `platform` module from disk
fn load_platform_module<'a>(
    arena: &'a Bump,
    filename: &Path,
    shorthand: &'a str,
    app_module_id: ModuleId,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: SharedIdentIdsByModule,
) -> Result<Msg<'a>, LoadingProblem<'a>> {
    let module_start_time = Instant::now();
    let file_io_start = Instant::now();
    let file = fs::read(filename);
    let file_io_duration = file_io_start.elapsed();

    match file {
        Ok(bytes_vec) => {
            let parse_start = Instant::now();
            let bytes = arena.alloc(bytes_vec);
            let parse_state = roc_parse::state::State::new(bytes);
            let parsed = roc_parse::module::parse_header(arena, parse_state.clone());
            let parse_header_duration = parse_start.elapsed();

            // Insert the first entries for this module's timings
            let mut pkg_module_timing = ModuleTiming::new(module_start_time);

            pkg_module_timing.read_roc_file = file_io_duration;
            pkg_module_timing.parse_header = parse_header_duration;

            match parsed {
                Ok((ast::Module::Interface { header }, _parse_state)) => {
                    Err(LoadingProblem::UnexpectedHeader(format!(
                        "expected platform/package module, got Interface with header\n{:?}",
                        header
                    )))
                }
                Ok((ast::Module::Hosted { header }, _parse_state)) => {
                    Err(LoadingProblem::UnexpectedHeader(format!(
                        "expected platform/package module, got Hosted module with header\n{:?}",
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
                    // make a `platform` module that ultimately exposes `main` to the host
                    let platform_module_msg = fabricate_platform_module(
                        arena,
                        Some(shorthand),
                        Some(app_module_id),
                        filename.to_path_buf(),
                        parser_state,
                        module_ids.clone(),
                        ident_ids_by_module,
                        &header,
                        pkg_module_timing,
                    )
                    .1;

                    Ok(platform_module_msg)
                }
                Err(fail) => Err(LoadingProblem::ParsingFailed(
                    fail.map_problem(SyntaxError::Header)
                        .into_file_error(filename.to_path_buf()),
                )),
            }
        }

        Err(err) => Err(LoadingProblem::FileProblem {
            filename: filename.to_path_buf(),
            error: err.kind(),
        }),
    }
}

fn load_builtin_module_help<'a>(
    arena: &'a Bump,
    filename: &str,
    src_bytes: &'a str,
) -> (HeaderInfo<'a>, roc_parse::state::State<'a>) {
    let is_root_module = false;
    let opt_shorthand = None;

    let filename = PathBuf::from(filename);

    let parse_state = roc_parse::state::State::new(src_bytes.as_bytes());
    let parsed = roc_parse::module::parse_header(arena, parse_state.clone());

    match parsed {
        Ok((ast::Module::Interface { header }, parse_state)) => {
            let info = HeaderInfo {
                loc_name: Loc {
                    region: header.name.region,
                    value: ModuleNameEnum::Interface(header.name.value),
                },
                filename,
                is_root_module,
                opt_shorthand,
                packages: &[],
                exposes: unspace(arena, header.exposes.items),
                imports: unspace(arena, header.imports.items),
                extra: HeaderFor::Builtin {
                    generates_with: &[],
                },
            };

            (info, parse_state)
        }
        Ok(_) => panic!("invalid header format for builtin module"),
        Err(e) => panic!(
            "Hit a parse error in the header of {:?}:\n{:?}",
            filename, e
        ),
    }
}

fn load_builtin_module<'a>(
    arena: &'a Bump,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: SharedIdentIdsByModule,
    module_timing: ModuleTiming,
    module_id: ModuleId,
    module_name: &str,
) -> (ModuleId, Msg<'a>) {
    let src_bytes = module_source(module_id);

    let (info, parse_state) = load_builtin_module_help(arena, module_name, src_bytes);

    let (module_id, _, header) = build_header(
        info,
        parse_state,
        module_ids,
        ident_ids_by_module,
        module_timing,
    );
    (module_id, Msg::Header(header))
}

/// Load a module by its module name, rather than by its filename
fn load_module<'a>(
    arena: &'a Bump,
    src_dir: &Path,
    module_name: PQModuleName<'a>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    arc_shorthands: Arc<Mutex<MutMap<&'a str, ShorthandPath>>>,
    roc_cache_dir: RocCacheDir<'_>,
    ident_ids_by_module: SharedIdentIdsByModule,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem<'a>> {
    let module_start_time = Instant::now();

    let parse_start = Instant::now();
    let parse_header_duration = parse_start.elapsed();

    // Insert the first entries for this module's timings
    let mut module_timing = ModuleTiming::new(module_start_time);

    module_timing.read_roc_file = Default::default();
    module_timing.parse_header = parse_header_duration;

    macro_rules! load_builtins {
        ($($name:literal, $module_id:path)*) => {
            match module_name.as_inner().as_str() {
            $(
                $name => {
                    return Ok(load_builtin_module(
                        arena,
                        module_ids,
                        ident_ids_by_module,
                        module_timing,
                        $module_id,
                        concat!($name, ".roc")
                    ));
                }
            )*
                _ => { /* fall through */ }
            }}
    }

    load_builtins! {
        "Result", ModuleId::RESULT
        "List", ModuleId::LIST
        "Str", ModuleId::STR
        "Dict", ModuleId::DICT
        "Set", ModuleId::SET
        "Num", ModuleId::NUM
        "Bool", ModuleId::BOOL
        "Box", ModuleId::BOX
        "Encode", ModuleId::ENCODE
        "Decode", ModuleId::DECODE
        "Hash", ModuleId::HASH
        "Json", ModuleId::JSON
    }

    let (filename, opt_shorthand) = module_name_to_path(src_dir, &module_name, arc_shorthands);

    load_filename(
        arena,
        filename,
        false,
        opt_shorthand,
        Some(module_name),
        module_ids,
        ident_ids_by_module,
        roc_cache_dir,
        module_start_time,
    )
}

#[derive(Debug)]
enum ShorthandPath {
    /// e.g. "/home/rtfeldman/.cache/roc/0.1.0/oUkxSOI9zFGtSoIaMB40QPdrXphr1p1780eiui2iO9Mz"
    FromHttpsUrl {
        /// e.g. "/home/rtfeldman/.cache/roc/0.1.0/oUkxSOI9zFGtSoIaMB40QPdrXphr1p1780eiui2iO9Mz"
        root_module_dir: PathBuf,
        /// e.g. "/home/rtfeldman/.cache/roc/0.1.0/oUkxSOI9zFGtSoIaMB40QPdrXphr1p1780eiui2iO9Mz/main.roc"
        root_module: PathBuf,
    },
    RelativeToSrc {
        /// e.g. "/home/rtfeldman/my-roc-code/examples/cli/cli-platform/"
        root_module_dir: PathBuf,
        /// e.g. "/home/rtfeldman/my-roc-code/examples/cli/cli-platform/main.roc"
        root_module: PathBuf,
    },
}

impl ShorthandPath {
    pub fn root_module(&self) -> &Path {
        match self {
            ShorthandPath::FromHttpsUrl { root_module, .. }
            | ShorthandPath::RelativeToSrc { root_module, .. } => root_module.as_path(),
        }
    }

    pub fn root_module_dir(&self) -> &Path {
        match self {
            ShorthandPath::FromHttpsUrl {
                root_module_dir, ..
            }
            | ShorthandPath::RelativeToSrc {
                root_module_dir, ..
            } => root_module_dir.as_path(),
        }
    }
}

fn module_name_to_path<'a>(
    src_dir: &Path,
    module_name: &PQModuleName<'a>,
    arc_shorthands: Arc<Mutex<MutMap<&'a str, ShorthandPath>>>,
) -> (PathBuf, Option<&'a str>) {
    let mut filename;
    let opt_shorthand;

    match module_name {
        PQModuleName::Unqualified(name) => {
            filename = src_dir.to_path_buf();

            opt_shorthand = None;
            // Convert dots in module name to directories
            for part in name.split(MODULE_SEPARATOR) {
                filename.push(part);
            }
        }
        PQModuleName::Qualified(shorthand, name) => {
            opt_shorthand = Some(*shorthand);
            let shorthands = arc_shorthands.lock();
            filename = shorthands
                .get(shorthand)
                .expect("All shorthands should have been validated by now.")
                .root_module_dir()
                .to_path_buf();

            // Convert dots in module name to directories
            for part in name.split(MODULE_SEPARATOR) {
                filename.push(part);
            }
        }
    }

    // End with .roc
    filename.set_extension(ROC_FILE_EXTENSION);

    (filename, opt_shorthand)
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

fn verify_interface_matches_file_path<'a>(
    interface_name: Loc<roc_parse::header::ModuleName<'a>>,
    path: &Path,
    state: &roc_parse::state::State<'a>,
) -> Result<(), LoadingProblem<'a>> {
    let module_parts = interface_name.value.as_str().split(MODULE_SEPARATOR).rev();

    let mut is_mismatched = false;
    let mut opt_path = Some(path);
    for part in module_parts {
        match opt_path.and_then(|path| path.file_stem().map(|fi| (path, fi))) {
            None => {
                is_mismatched = true;
                break;
            }
            Some((path, fi)) => {
                if fi != part {
                    is_mismatched = true;
                    break;
                }
                opt_path = path.parent();
            }
        }
    }

    if !is_mismatched {
        return Ok(());
    }

    use roc_parse::parser::EHeader;
    let syntax_problem =
        SyntaxError::Header(EHeader::InconsistentModuleName(interface_name.region));
    let problem = LoadingProblem::ParsingFailed(FileError {
        problem: SourceError::new(syntax_problem, state),
        filename: path.to_path_buf(),
    });
    Err(problem)
}

#[allow(clippy::too_many_arguments)]
fn parse_header<'a>(
    arena: &'a Bump,
    read_file_duration: Duration,
    filename: PathBuf,
    is_root_module: bool,
    opt_shorthand: Option<&'a str>,
    opt_expected_module_name: Option<PackageQualified<'a, ModuleName>>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: SharedIdentIdsByModule,
    src_bytes: &'a [u8],
    roc_cache_dir: RocCacheDir<'_>,
    start_time: Instant,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem<'a>> {
    let parse_start = Instant::now();
    let parse_state = roc_parse::state::State::new(src_bytes);
    let parsed = roc_parse::module::parse_header(arena, parse_state.clone());
    let parse_header_duration = parse_start.elapsed();

    // Insert the first entries for this module's timings
    let mut module_timing = ModuleTiming::new(start_time);

    module_timing.read_roc_file = read_file_duration;
    module_timing.parse_header = parse_header_duration;

    match parsed {
        Ok((ast::Module::Interface { header }, parse_state)) => {
            verify_interface_matches_file_path(header.name, &filename, &parse_state)?;

            let header_name_region = header.name.region;

            let info = HeaderInfo {
                loc_name: Loc {
                    region: header_name_region,
                    value: ModuleNameEnum::Interface(header.name.value),
                },
                filename,
                is_root_module,
                opt_shorthand,
                packages: &[],
                exposes: unspace(arena, header.exposes.items),
                imports: unspace(arena, header.imports.items),
                extra: HeaderFor::Interface,
            };

            let (module_id, module_name, header) = build_header(
                info,
                parse_state.clone(),
                module_ids,
                ident_ids_by_module,
                module_timing,
            );

            if let Some(expected_module_name) = opt_expected_module_name {
                if expected_module_name != module_name {
                    let problem = SourceError::new(
                        IncorrectModuleName {
                            module_id,
                            found: Loc::at(header_name_region, module_name),
                            expected: expected_module_name,
                        },
                        &parse_state,
                    );
                    let problem = LoadingProblem::IncorrectModuleName(FileError {
                        problem,
                        filename: header.module_path,
                    });
                    return Err(problem);
                }
            }

            Ok((module_id, Msg::Header(header)))
        }
        Ok((ast::Module::Hosted { header }, parse_state)) => {
            let info = HeaderInfo {
                loc_name: Loc {
                    region: header.name.region,
                    value: ModuleNameEnum::Hosted(header.name.value),
                },
                filename,
                is_root_module,
                opt_shorthand,
                packages: &[],
                exposes: unspace(arena, header.exposes.items),
                imports: unspace(arena, header.imports.items),
                extra: HeaderFor::Hosted {
                    generates: header.generates,
                    generates_with: unspace(arena, header.generates_with.items),
                },
            };

            let (module_id, _, header) = build_header(
                info,
                parse_state,
                module_ids,
                ident_ids_by_module,
                module_timing,
            );

            Ok((module_id, Msg::Header(header)))
        }
        Ok((ast::Module::App { header }, parse_state)) => {
            let mut app_file_dir = filename.clone();
            app_file_dir.pop();

            let packages = unspace(arena, header.packages.items);

            let mut exposes = bumpalo::collections::Vec::new_in(arena);
            exposes.extend(unspace(arena, header.provides.items));

            if let Some(provided_types) = header.provides_types {
                for provided_type in unspace(arena, provided_types.items) {
                    let string: &str = provided_type.value.into();
                    let exposed_name = ExposedName::new(string);

                    exposes.push(Loc::at(provided_type.region, exposed_name));
                }
            }

            let exposes = exposes.into_bump_slice();

            let info = HeaderInfo {
                loc_name: Loc {
                    region: header.name.region,
                    value: ModuleNameEnum::App(header.name.value),
                },
                filename,
                is_root_module,
                opt_shorthand,
                packages,
                exposes,
                imports: unspace(arena, header.imports.items),
                extra: HeaderFor::App {
                    to_platform: header.to.value,
                },
            };

            let (module_id, _, resolved_header) = build_header(
                info,
                parse_state,
                module_ids.clone(),
                ident_ids_by_module.clone(),
                module_timing,
            );
            let app_module_header_msg = Msg::Header(resolved_header);

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
                                value: package_path,
                                ..
                            },
                        ..
                    }) = opt_base_package
                    {
                        let src = package_path.to_str();

                        // check whether we can find a `platform` module file on disk
                        let platform_module_path = if src.starts_with("https://") {
                            #[cfg(not(target_family = "wasm"))]
                            {
                                // If this is a HTTPS package, synchronously download it
                                // to the cache before proceeding.

                                // TODO we should do this async; however, with the current
                                // architecture of file.rs (which doesn't use async/await),
                                // this would be very difficult!
                                let (package_dir, opt_root_module) = cache::install_package(
                                    roc_cache_dir,
                                    src,
                                )
                                .unwrap_or_else(|err| {
                                    todo!("TODO gracefully handle package install error {:?}", err);
                                });

                                // You can optionally specify the root module using the URL fragment,
                                // e.g. #foo.roc
                                // (defaults to main.roc)
                                match opt_root_module {
                                    Some(root_module) => package_dir.join(root_module),
                                    None => package_dir.join("main.roc"),
                                }
                            }

                            #[cfg(target_family = "wasm")]
                            {
                                panic!(
                                    "Specifying packages via URLs is curently unsupported in wasm."
                                );
                            }
                        } else {
                            app_file_dir.join(src)
                        };

                        if platform_module_path.as_path().exists() {
                            let load_platform_module_msg = load_platform_module(
                                arena,
                                &platform_module_path,
                                shorthand,
                                module_id,
                                module_ids,
                                ident_ids_by_module,
                            )?;

                            Ok((
                                module_id,
                                Msg::Many(vec![app_module_header_msg, load_platform_module_msg]),
                            ))
                        } else {
                            Err(LoadingProblem::FileProblem {
                                filename: platform_module_path,
                                error: io::ErrorKind::NotFound,
                            })
                        }
                    } else {
                        panic!("could not find base")
                    }
                }
                To::NewPackage(_package_name) => Ok((module_id, app_module_header_msg)),
            }
        }
        Ok((ast::Module::Platform { header }, parse_state)) => Ok(fabricate_platform_module(
            arena,
            None,
            None,
            filename,
            parse_state,
            module_ids.clone(),
            ident_ids_by_module,
            &header,
            module_timing,
        )),

        Err(fail) => Err(LoadingProblem::ParsingFailed(
            fail.map_problem(SyntaxError::Header)
                .into_file_error(filename),
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
    opt_expected_module_name: Option<PackageQualified<'a, ModuleName>>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: SharedIdentIdsByModule,
    roc_cache_dir: RocCacheDir<'_>,
    module_start_time: Instant,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem<'a>> {
    let file_io_start = Instant::now();
    let file = fs::read(&filename);
    let file_io_duration = file_io_start.elapsed();

    match file {
        Ok(bytes) => parse_header(
            arena,
            file_io_duration,
            filename,
            is_root_module,
            opt_shorthand,
            opt_expected_module_name,
            module_ids,
            ident_ids_by_module,
            arena.alloc(bytes),
            roc_cache_dir,
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
    ident_ids_by_module: SharedIdentIdsByModule,
    roc_cache_dir: RocCacheDir<'_>,
    module_start_time: Instant,
) -> Result<(ModuleId, Msg<'a>), LoadingProblem<'a>> {
    let file_io_start = Instant::now();
    let file_io_duration = file_io_start.elapsed();

    parse_header(
        arena,
        file_io_duration,
        filename,
        false,
        None,
        None,
        module_ids,
        ident_ids_by_module,
        src.as_bytes(),
        roc_cache_dir,
        module_start_time,
    )
}

#[derive(Debug)]
struct HeaderInfo<'a> {
    loc_name: Loc<ModuleNameEnum<'a>>,
    filename: PathBuf,
    is_root_module: bool,
    opt_shorthand: Option<&'a str>,
    packages: &'a [Loc<PackageEntry<'a>>],
    exposes: &'a [Loc<ExposedName<'a>>],
    imports: &'a [Loc<ImportsEntry<'a>>],
    extra: HeaderFor<'a>,
}

#[allow(clippy::too_many_arguments)]
fn build_header<'a>(
    info: HeaderInfo<'a>,
    parse_state: roc_parse::state::State<'a>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: SharedIdentIdsByModule,
    module_timing: ModuleTiming,
) -> (ModuleId, PQModuleName<'a>, ModuleHeader<'a>) {
    use ModuleNameEnum::*;

    let HeaderInfo {
        loc_name,
        filename,
        is_root_module,
        opt_shorthand,
        packages,
        exposes,
        imports,
        extra,
    } = info;

    let declared_name: ModuleName = match &loc_name.value {
        Platform => unreachable!(),
        App(_) => ModuleName::APP.into(),
        Interface(module_name) | Hosted(module_name) => {
            // TODO check to see if module_name is consistent with filename.
            // If it isn't, report a problem!

            module_name.as_str().into()
        }
    };

    let mut imported: Vec<(QualifiedModuleName, Vec<Loc<Ident>>, Region)> =
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
    let name: PQModuleName;

    let ident_ids = {
        // Lock just long enough to perform the minimal operations necessary.
        let mut module_ids = (*module_ids).lock();
        let mut ident_ids_by_module = (*ident_ids_by_module).lock();

        name = match opt_shorthand {
            Some(shorthand) => PQModuleName::Qualified(shorthand, declared_name),
            None => PQModuleName::Unqualified(declared_name),
        };
        home = module_ids.get_or_insert(&name);

        // Ensure this module has an entry in the exposed_ident_ids map.
        ident_ids_by_module.get_or_insert(home);

        // For each of our imports, add an entry to deps_by_name
        //
        // e.g. for `imports [pf.Foo.{ bar }]`, add `Foo` to deps_by_name
        //
        // Also build a list of imported_values_to_expose (like `bar` above.)
        for (qualified_module_name, exposed_idents, region) in imported.into_iter() {
            let cloned_module_name = qualified_module_name.module.clone();
            let pq_module_name = if qualified_module_name.is_builtin() {
                // If this is a builtin, it must be unqualified, and we should *never* prefix it
                // with the package shorthand! The user intended to import the module as-is here.
                debug_assert!(qualified_module_name.opt_package.is_none());
                PQModuleName::Unqualified(qualified_module_name.module)
            } else {
                match qualified_module_name.opt_package {
                    None => match opt_shorthand {
                        Some(shorthand) => {
                            PQModuleName::Qualified(shorthand, qualified_module_name.module)
                        }
                        None => PQModuleName::Unqualified(qualified_module_name.module),
                    },
                    Some(package) => PQModuleName::Qualified(package, cloned_module_name),
                }
            };

            let module_id = module_ids.get_or_insert(&pq_module_name);
            imported_modules.insert(module_id, region);

            deps_by_name.insert(pq_module_name, module_id);

            // Add the new exposed idents to the dep module's IdentIds, so
            // once that module later gets loaded, its lookups will resolve
            // to the same symbols as the ones we're using here.
            let ident_ids = ident_ids_by_module.get_or_insert(module_id);

            for Loc {
                region,
                value: ident,
            } in exposed_idents
            {
                let ident_id = ident_ids.get_or_insert(ident.as_str());
                let symbol = Symbol::new(module_id, ident_id);

                // Since this value is exposed, add it to our module's default scope.
                debug_assert!(!scope.contains_key(&ident));

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
            // For example, if module A has [B.{ foo }], then
            // when we get here for B, `foo` will already have
            // an IdentId. We must reuse that!
            let ident_id = ident_ids.get_or_insert(loc_exposed.value.as_str());
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

    // make sure when we run the bulitin modules in /compiler/builtins/roc that we
    // mark these modules as Builtin. Otherwise the builtin functions are not instantiated
    // and we just have a bunch of definitions with runtime errors in their bodies
    let extra = {
        match extra {
            HeaderFor::Interface if home.is_builtin() => HeaderFor::Builtin {
                generates_with: &[],
            },
            _ => extra,
        }
    };

    (
        home,
        name,
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
            parse_state,
            exposed_imports: scope,
            symbols_from_requires: Vec::new(),
            header_for: extra,
            module_timing,
        },
    )
}

#[derive(Debug)]
struct PlatformHeaderInfo<'a> {
    filename: PathBuf,
    is_root_module: bool,
    opt_shorthand: Option<&'a str>,
    opt_app_module_id: Option<ModuleId>,
    packages: &'a [Loc<PackageEntry<'a>>],
    provides: &'a [Loc<ExposedName<'a>>],
    requires: &'a [Loc<TypedIdent<'a>>],
    requires_types: &'a [Loc<UppercaseIdent<'a>>],
    imports: &'a [Loc<ImportsEntry<'a>>],
}

// TODO refactor so more logic is shared with `send_header`
#[allow(clippy::too_many_arguments)]
fn send_header_two<'a>(
    info: PlatformHeaderInfo<'a>,
    parse_state: roc_parse::state::State<'a>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: SharedIdentIdsByModule,
    module_timing: ModuleTiming,
) -> (ModuleId, Msg<'a>) {
    let PlatformHeaderInfo {
        filename,
        opt_shorthand,
        is_root_module,
        opt_app_module_id,
        packages,
        provides,
        requires,
        requires_types,
        imports,
    } = info;

    let declared_name: ModuleName = "".into();
    let mut symbols_from_requires = Vec::with_capacity(requires.len());

    let mut imported: Vec<(QualifiedModuleName, Vec<Loc<Ident>>, Region)> =
        Vec::with_capacity(imports.len());
    let mut imported_modules: MutMap<ModuleId, Region> = MutMap::default();

    let num_exposes = provides.len();
    let mut deps_by_name: MutMap<PQModuleName, ModuleId> =
        HashMap::with_capacity_and_hasher(num_exposes, default_hasher());

    // Add standard imports, if there is an app module.
    // (There might not be, e.g. when running `roc check myplatform.roc` or
    // when generating bindings.)
    if let Some(app_module_id) = opt_app_module_id {
        imported_modules.insert(app_module_id, Region::zero());
        deps_by_name.insert(
            PQModuleName::Unqualified(ModuleName::APP.into()),
            app_module_id,
        );
    }

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

        let name = match opt_shorthand {
            Some(shorthand) => PQModuleName::Qualified(shorthand, declared_name),
            None => PQModuleName::Unqualified(declared_name),
        };
        home = module_ids.get_or_insert(&name);

        // Ensure this module has an entry in the exposed_ident_ids map.
        ident_ids_by_module.get_or_insert(home);

        // For each of our imports, add an entry to deps_by_name
        //
        // e.g. for `imports [pf.Foo.{ bar }]`, add `Foo` to deps_by_name
        //
        // Also build a list of imported_values_to_expose (like `bar` above.)
        for (qualified_module_name, exposed_idents, region) in imported.into_iter() {
            let cloned_module_name = qualified_module_name.module.clone();
            let pq_module_name = if qualified_module_name.is_builtin() {
                // If this is a builtin, it must be unqualified, and we should *never* prefix it
                // with the package shorthand! The user intended to import the module as-is here.
                debug_assert!(qualified_module_name.opt_package.is_none());
                PQModuleName::Unqualified(qualified_module_name.module)
            } else {
                match qualified_module_name.opt_package {
                    None => match opt_shorthand {
                        Some(shorthand) => {
                            PQModuleName::Qualified(shorthand, qualified_module_name.module)
                        }
                        None => PQModuleName::Unqualified(qualified_module_name.module),
                    },
                    Some(package) => PQModuleName::Qualified(package, cloned_module_name),
                }
            };

            let module_id = module_ids.get_or_insert(&pq_module_name);
            imported_modules.insert(module_id, region);

            deps_by_name.insert(pq_module_name, module_id);

            // Add the new exposed idents to the dep module's IdentIds, so
            // once that module later gets loaded, its lookups will resolve
            // to the same symbols as the ones we're using here.
            let ident_ids = ident_ids_by_module.get_or_insert(module_id);

            for Loc {
                region,
                value: ident,
            } in exposed_idents
            {
                let ident_id = ident_ids.get_or_insert(ident.as_str());
                let symbol = Symbol::new(module_id, ident_id);

                // Since this value is exposed, add it to our module's default scope.
                debug_assert!(!scope.contains_key(&ident.clone()));

                scope.insert(ident, (symbol, region));
            }
        }

        {
            // If we don't have an app module id (e.g. because we're doing
            // `roc check myplatform.roc` or because we're generating glue code),
            // insert the `requires` symbols into the platform module's IdentIds.
            //
            // Otherwise, get them from the app module's IdentIds, because it
            // should already have a symbol for each `requires` entry, and we
            // want to make sure we're referencing the same symbols!
            let module_id = opt_app_module_id.unwrap_or(home);
            let ident_ids = ident_ids_by_module.get_or_insert(module_id);

            for entry in requires {
                let entry = entry.value;
                let ident: Ident = entry.ident.value.into();
                let ident_id = ident_ids.get_or_insert(entry.ident.value);
                let symbol = Symbol::new(module_id, ident_id);

                // Since this value is exposed, add it to our module's default scope.
                debug_assert!(!scope.contains_key(&ident.clone()));

                scope.insert(ident, (symbol, entry.ident.region));
                symbols_from_requires.push((Loc::at(entry.ident.region, symbol), entry.ann));
            }

            for entry in requires_types {
                let string: &str = entry.value.into();
                let ident: Ident = string.into();
                let ident_id = ident_ids.get_or_insert(string);
                let symbol = Symbol::new(module_id, ident_id);

                // Since this value is exposed, add it to our module's default scope.
                debug_assert!(!scope.contains_key(&ident));
                scope.insert(ident, (symbol, entry.region));
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
            // For example, if module A has [B.{ foo }], then
            // when we get here for B, `foo` will already have
            // an IdentId. We must reuse that!
            let ident_id = ident_ids.get_or_insert(loc_exposed.value.as_str());
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
    let module_name = ModuleNameEnum::Platform;

    let main_for_host = {
        let ident_id = ident_ids.get_or_insert(provides[0].value.as_str());

        Symbol::new(home, ident_id)
    };

    let extra = HeaderFor::Platform {
        // A config_shorthand of "" should be fine
        config_shorthand: opt_shorthand.unwrap_or_default(),
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
        Msg::Header(ModuleHeader {
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
            parse_state,
            exposed_imports: scope,
            module_timing,
            symbols_from_requires,
            header_for: extra,
        }),
    )
}

impl<'a> BuildTask<'a> {
    // TODO trim down these arguments - possibly by moving Constraint into Module
    #[allow(clippy::too_many_arguments)]
    fn solve_module(
        module: Module,
        ident_ids: IdentIds,
        module_timing: ModuleTiming,
        types: Types,
        constraints: Constraints,
        constraint: ConstraintSoa,
        pending_derives: PendingDerives,
        var_store: VarStore,
        imported_modules: MutMap<ModuleId, Region>,
        exposed_types: &ExposedByModule,
        dep_idents: IdentIdsByModule,
        declarations: Declarations,
        cached_subs: CachedTypeState,
        derived_module: SharedDerivedModule,
    ) -> Self {
        let exposed_by_module = exposed_types.retain_modules(imported_modules.keys());

        let exposed_for_module =
            ExposedForModule::new(module.referenced_values.iter(), exposed_by_module);

        // Next, solve this module in the background.
        Self::Solve {
            module,
            ident_ids,
            exposed_for_module,
            types,
            constraints,
            constraint,
            pending_derives,
            var_store,
            declarations,
            dep_idents,
            module_timing,
            cached_subs,
            derived_module,
        }
    }
}

fn synth_import(subs: &mut Subs, content: roc_types::subs::Content) -> Variable {
    use roc_types::subs::{Descriptor, Mark, OptVariable, Rank};
    subs.fresh(Descriptor {
        content,
        rank: Rank::import(),
        mark: Mark::NONE,
        copy: OptVariable::NONE,
    })
}

fn synth_list_len_type(subs: &mut Subs) -> Variable {
    use roc_types::subs::{Content, FlatType, LambdaSet, OptVariable, SubsSlice, UnionLabels};

    // List.len : List a -> Nat
    let a = synth_import(subs, Content::FlexVar(None));
    let a_slice = SubsSlice::extend_new(&mut subs.variables, [a]);
    let list_a = synth_import(
        subs,
        Content::Structure(FlatType::Apply(Symbol::LIST_LIST, a_slice)),
    );
    let fn_var = synth_import(subs, Content::Error);
    let solved_list_len = UnionLabels::insert_into_subs(subs, [(Symbol::LIST_LEN, [])]);
    let clos_list_len = synth_import(
        subs,
        Content::LambdaSet(LambdaSet {
            solved: solved_list_len,
            recursion_var: OptVariable::NONE,
            unspecialized: SubsSlice::default(),
            ambient_function: fn_var,
        }),
    );
    let fn_args_slice = SubsSlice::extend_new(&mut subs.variables, [list_a]);
    subs.set_content(
        fn_var,
        Content::Structure(FlatType::Func(fn_args_slice, clos_list_len, Variable::NAT)),
    );
    fn_var
}

pub fn add_imports(
    my_module: ModuleId,
    constraints: &mut Constraints,
    subs: &mut Subs,
    mut pending_abilities: PendingAbilitiesStore,
    exposed_for_module: &ExposedForModule,
    def_types: &mut Vec<(Symbol, Loc<TypeOrVar>)>,
    rigid_vars: &mut Vec<Variable>,
) -> (Vec<Variable>, AbilitiesStore) {
    let mut import_variables = Vec::new();

    let mut cached_symbol_vars = VecMap::default();

    macro_rules! import_var_for_symbol  {
        ($subs:expr, $exposed_by_module:expr, $symbol:ident, $break:stmt) => {
            let module_id = $symbol.module_id();
            match $exposed_by_module.get(&module_id) {
                Some(ExposedModuleTypes {
                    exposed_types_storage_subs: exposed_types,
                    resolved_implementations: _,
                }) => {
                    let variable = match exposed_types.stored_vars_by_symbol.iter().find(|(s, _)| **s == $symbol) {
                        None => {
                            // Today we define builtins in each module that uses them
                            // so even though they have a different module name from
                            // the surrounding module, they are not technically imported
                            debug_assert!($symbol.is_builtin());
                            $break
                        }
                        Some((_, x)) => *x,
                    };

                    let copied_import = exposed_types.storage_subs.export_variable_to($subs, variable);
                    let copied_import_index = constraints.push_variable(copied_import.variable);

                    def_types.push((
                        $symbol,
                        Loc::at_zero(copied_import_index),
                    ));

                    // not a typo; rigids are turned into flex during type inference, but when imported we must
                    // consider them rigid variables
                    rigid_vars.extend(copied_import.rigid);
                    rigid_vars.extend(copied_import.flex);

                    // Rigid vars bound to abilities are also treated like rigids.
                    rigid_vars.extend(copied_import.rigid_able);
                    rigid_vars.extend(copied_import.flex_able);

                    import_variables.extend(copied_import.registered);

                    cached_symbol_vars.insert($symbol, copied_import.variable);
                }
                None => {
                    internal_error!("Imported module {:?} is not available", module_id)
                }
            }
        }
    }

    for &symbol in &exposed_for_module.imported_values {
        import_var_for_symbol!(subs, exposed_for_module.exposed_by_module, symbol, continue);
    }

    // Patch used symbols from circular dependencies.
    if my_module == ModuleId::NUM {
        // Num needs List.len, but List imports Num.
        let list_len_type_var = synth_list_len_type(subs);
        let list_len_type_index = constraints.push_variable(list_len_type_var);
        def_types.push((Symbol::LIST_LEN, Loc::at_zero(list_len_type_index)));
        import_variables.push(list_len_type_var);
    }

    // Fill in the implementation information of the abilities from the modules we import, which we
    // now know because all imported modules should be solved by now.
    //
    // TODO: see if we can reduce the amount of specializations we need to import.
    // One idea is to just always assume external modules fulfill their specialization obligations
    // and save lambda set resolution for mono.
    for (_, module_types) in exposed_for_module.exposed_by_module.iter_all() {
        for (impl_key, resolved_impl) in module_types.resolved_implementations.iter() {
            pending_abilities.import_implementation(*impl_key, resolved_impl);
        }
    }

    struct Ctx<'a> {
        subs: &'a mut Subs,
        exposed_by_module: &'a ExposedByModule,
    }

    let abilities_store = pending_abilities.resolve_for_module(
        my_module,
        &mut Ctx {
            subs,
            exposed_by_module: &exposed_for_module.exposed_by_module,
        },
        |ctx, symbol| match cached_symbol_vars.get(&symbol).copied() {
            Some(var) => var,
            None => {
                import_var_for_symbol!(
                    ctx.subs,
                    ctx.exposed_by_module,
                    symbol,
                    internal_error!("Import ability member {:?} not available", symbol)
                );
                *cached_symbol_vars.get(&symbol).unwrap()
            }
        },
        |ctx, module, lset_var| match ctx.exposed_by_module.get(&module) {
            Some(ExposedModuleTypes {
                exposed_types_storage_subs: exposed_types,
                resolved_implementations: _,
            }) => {
                let var = exposed_types
                    .stored_specialization_lambda_set_vars
                    .get(&lset_var)
                    .expect("Lambda set var from other module not available");

                let copied_import = exposed_types
                    .storage_subs
                    .export_variable_to(ctx.subs, *var);

                copied_import.variable
            }
            None => internal_error!("Imported module {:?} is not available", module),
        },
    );

    (import_variables, abilities_store)
}

#[allow(clippy::complexity)]
fn run_solve_solve(
    exposed_for_module: ExposedForModule,
    mut types: Types,
    mut constraints: Constraints,
    constraint: ConstraintSoa,
    pending_derives: PendingDerives,
    var_store: VarStore,
    module: Module,
    derived_module: SharedDerivedModule,
) -> (
    Solved<Subs>,
    ResolvedImplementations,
    Vec<(Symbol, Variable)>,
    Vec<TypeError>,
    AbilitiesStore,
) {
    let Module {
        exposed_symbols,
        aliases,
        rigid_variables,
        abilities_store: pending_abilities,
        ..
    } = module;

    let mut rigid_vars: Vec<Variable> = Vec::new();
    let mut def_types: Vec<(Symbol, Loc<TypeOrVar>)> = Vec::new();

    let mut subs = Subs::new_from_varstore(var_store);

    let (import_variables, abilities_store) = add_imports(
        module.module_id,
        &mut constraints,
        &mut subs,
        pending_abilities,
        &exposed_for_module,
        &mut def_types,
        &mut rigid_vars,
    );

    let actual_constraint =
        constraints.let_import_constraint(rigid_vars, def_types, constraint, &import_variables);

    let mut solve_aliases = roc_solve::solve::Aliases::with_capacity(aliases.len());
    for (name, (_, alias)) in aliases.iter() {
        solve_aliases.insert(&mut types, *name, alias.clone());
    }

    let (solved_subs, solved_implementations, exposed_vars_by_symbol, problems, abilities_store) = {
        let module_id = module.module_id;

        let (solved_subs, solved_env, problems, abilities_store) = roc_solve::module::run_solve(
            module_id,
            types,
            &constraints,
            actual_constraint,
            rigid_variables,
            subs,
            solve_aliases,
            abilities_store,
            pending_derives,
            &exposed_for_module.exposed_by_module,
            derived_module,
        );

        let solved_implementations =
            extract_module_owned_implementations(module_id, &abilities_store);

        let is_specialization_symbol = |sym| {
            solved_implementations
                .values()
                .any(|resolved_impl| match resolved_impl {
                    ResolvedImpl::Impl(specialization) => specialization.symbol == sym,
                    ResolvedImpl::Error => false,
                })
        };

        // Expose anything that is explicitly exposed by the header, or is a specialization of an
        // ability.
        let exposed_vars_by_symbol: Vec<_> = solved_env
            .vars_by_symbol()
            .filter(|(k, _)| {
                exposed_symbols.contains(k)
                    || is_specialization_symbol(*k)
                    || k.is_exposed_for_builtin_derivers()
            })
            .collect();

        (
            solved_subs,
            solved_implementations,
            exposed_vars_by_symbol,
            problems,
            abilities_store,
        )
    };

    (
        solved_subs,
        solved_implementations,
        exposed_vars_by_symbol,
        problems,
        abilities_store,
    )
}

#[allow(clippy::too_many_arguments)]
fn run_solve<'a>(
    module: Module,
    ident_ids: IdentIds,
    mut module_timing: ModuleTiming,
    exposed_for_module: ExposedForModule,
    types: Types,
    constraints: Constraints,
    constraint: ConstraintSoa,
    pending_derives: PendingDerives,
    var_store: VarStore,
    decls: Declarations,
    dep_idents: IdentIdsByModule,
    cached_types: CachedTypeState,
    derived_module: SharedDerivedModule,
) -> Msg<'a> {
    let solve_start = Instant::now();

    let module_id = module.module_id;

    // TODO remove when we write builtins in roc
    let aliases = module.aliases.clone();

    let mut module = module;
    let loc_expects = std::mem::take(&mut module.loc_expects);
    let loc_dbgs = std::mem::take(&mut module.loc_dbgs);
    let module = module;

    let (solved_subs, solved_implementations, exposed_vars_by_symbol, problems, abilities_store) = {
        if module_id.is_builtin() {
            match cached_types.lock().remove(&module_id) {
                None => run_solve_solve(
                    exposed_for_module,
                    types,
                    constraints,
                    constraint,
                    pending_derives,
                    var_store,
                    module,
                    derived_module,
                ),
                Some(TypeState {
                    subs,
                    exposed_vars_by_symbol,
                    abilities,
                    solved_implementations,
                }) => (
                    Solved(subs),
                    solved_implementations,
                    exposed_vars_by_symbol,
                    vec![],
                    abilities,
                ),
            }
        } else {
            run_solve_solve(
                exposed_for_module,
                types,
                constraints,
                constraint,
                pending_derives,
                var_store,
                module,
                derived_module,
            )
        }
    };

    let mut solved_subs = solved_subs;
    let exposed_types = roc_solve::module::exposed_types_storage_subs(
        module_id,
        &mut solved_subs,
        &exposed_vars_by_symbol,
        &solved_implementations,
        &abilities_store,
    );

    let solved_module = SolvedModule {
        exposed_vars_by_symbol,
        problems,
        aliases,
        solved_implementations,
        exposed_types,
    };

    // Record the final timings
    let solve_end = Instant::now();
    module_timing.solve = solve_end.duration_since(solve_start);

    // Send the subs to the main thread for processing,
    Msg::SolvedTypes {
        module_id,
        solved_subs,
        ident_ids,
        decls,
        dep_idents,
        solved_module,
        module_timing,
        abilities_store,
        loc_expects,
        loc_dbgs,
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
fn fabricate_platform_module<'a>(
    arena: &'a Bump,
    opt_shorthand: Option<&'a str>,
    opt_app_module_id: Option<ModuleId>,
    filename: PathBuf,
    parse_state: roc_parse::state::State<'a>,
    module_ids: Arc<Mutex<PackageModuleIds<'a>>>,
    ident_ids_by_module: SharedIdentIdsByModule,
    header: &PlatformHeader<'a>,
    module_timing: ModuleTiming,
) -> (ModuleId, Msg<'a>) {
    // If we have an app module, then it's the root module;
    // otherwise, we must be the root.
    let is_root_module = opt_app_module_id.is_none();

    let info = PlatformHeaderInfo {
        filename,
        is_root_module,
        opt_shorthand,
        opt_app_module_id,
        packages: &[],
        provides: unspace(arena, header.provides.items),
        requires: &*arena.alloc([Loc::at(
            header.requires.signature.region,
            header.requires.signature.extract_spaces().item,
        )]),
        requires_types: unspace(arena, header.requires.rigids.items),
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
#[allow(clippy::unnecessary_wraps)]
fn canonicalize_and_constrain<'a>(
    arena: &'a Bump,
    module_ids: &ModuleIds,
    dep_idents: IdentIdsByModule,
    exposed_symbols: VecSet<Symbol>,
    aliases: MutMap<Symbol, Alias>,
    imported_abilities_state: PendingAbilitiesStore,
    parsed: ParsedModule<'a>,
    skip_constraint_gen: bool,
) -> CanAndCon {
    let canonicalize_start = Instant::now();

    let ParsedModule {
        module_id,
        module_name,
        header_for,
        exposed_ident_ids,
        parsed_defs,
        exposed_imports,
        imported_modules,
        mut module_timing,
        symbols_from_requires,
        ..
    } = parsed;

    // _before has an underscore because it's unused in --release builds
    let _before = roc_types::types::get_type_clone_count();

    let parsed_defs_for_docs = parsed_defs.clone();
    let parsed_defs = arena.alloc(parsed_defs);

    let mut var_store = VarStore::default();
    let module_output = canonicalize_module_defs(
        arena,
        parsed_defs,
        &header_for,
        module_id,
        module_ids,
        exposed_ident_ids,
        &dep_idents,
        aliases,
        imported_abilities_state,
        exposed_imports,
        &exposed_symbols,
        &symbols_from_requires,
        &mut var_store,
    );
    let mut types = Types::new();

    // _after has an underscore because it's unused in --release builds
    let _after = roc_types::types::get_type_clone_count();

    log!(
        "canonicalize of {:?} cloned Type {} times ({} -> {})",
        module_id,
        _after - _before,
        _before,
        _after
    );

    let canonicalize_end = Instant::now();

    module_timing.canonicalize = canonicalize_end.duration_since(canonicalize_start);

    // Generate documentation information
    // TODO: store timing information?
    let module_docs = match module_name {
        ModuleNameEnum::Platform => None,
        ModuleNameEnum::App(_) => None,
        ModuleNameEnum::Interface(name) | ModuleNameEnum::Hosted(name) => {
            let mut scope = module_output.scope.clone();
            scope.add_docs_imports();
            let docs = crate::docs::generate_module_docs(
                scope,
                name.as_str().into(),
                &parsed_defs_for_docs,
            );

            Some(docs)
        }
    };

    // _before has an underscore because it's unused in --release builds
    let _before = roc_types::types::get_type_clone_count();

    let mut constraints = Constraints::new();

    let constraint = if skip_constraint_gen {
        roc_can::constraint::Constraint::True
    } else {
        constrain_module(
            &mut types,
            &mut constraints,
            module_output.symbols_from_requires,
            &module_output.scope.abilities_store,
            &module_output.declarations,
            module_id,
        )
    };

    // _after has an underscore because it's unused in --release builds
    let _after = roc_types::types::get_type_clone_count();

    log!(
        "constraint gen of {:?} cloned Type {} times ({} -> {})",
        module_id,
        _after - _before,
        _before,
        _after
    );

    // scope has imported aliases, but misses aliases from inner scopes
    // module_output.aliases does have those aliases, so we combine them
    let mut aliases: MutMap<Symbol, (bool, Alias)> = module_output
        .aliases
        .into_iter()
        .map(|(k, v)| (k, (true, v)))
        .collect();

    for (name, alias) in module_output.scope.aliases {
        match aliases.entry(name) {
            Occupied(_) => {
                // do nothing
            }
            Vacant(vacant) => {
                let should_include_builtin = matches!(
                    name.module_id(),
                    ModuleId::ENCODE
                        | ModuleId::DECODE
                        | ModuleId::DICT
                        | ModuleId::SET
                        | ModuleId::HASH
                );

                if !name.is_builtin() || should_include_builtin {
                    vacant.insert((false, alias));
                }
            }
        }
    }

    let module = Module {
        module_id,
        exposed_imports: module_output.exposed_imports,
        exposed_symbols,
        referenced_values: module_output.referenced_values,
        referenced_types: module_output.referenced_types,
        aliases,
        rigid_variables: module_output.rigid_variables,
        abilities_store: module_output.scope.abilities_store,
        loc_expects: module_output.loc_expects,
        loc_dbgs: module_output.loc_dbgs,
    };

    let constrained_module = ConstrainedModule {
        module,
        declarations: module_output.declarations,
        imported_modules,
        var_store,
        constraints,
        constraint,
        ident_ids: module_output.scope.locals.ident_ids,
        dep_idents,
        module_timing,
        types,
        pending_derives: module_output.pending_derives,
    };

    CanAndCon {
        constrained_module,
        canonicalization_problems: module_output.problems,
        module_docs,
    }
}

fn parse<'a>(arena: &'a Bump, header: ModuleHeader<'a>) -> Result<Msg<'a>, LoadingProblem<'a>> {
    let mut module_timing = header.module_timing;
    let parse_start = Instant::now();
    let source = header.parse_state.original_bytes();
    let parse_state = header.parse_state;
    let parsed_defs = match module_defs().parse(arena, parse_state.clone(), 0) {
        Ok((_, success, _state)) => success,
        Err((_, fail)) => {
            return Err(LoadingProblem::ParsingFailed(
                fail.into_file_error(header.module_path, &parse_state),
            ));
        }
    };

    // Record the parse end time once, to avoid checking the time a second time
    // immediately afterward (for the beginning of canonicalization).
    let parse_end = Instant::now();

    module_timing.parse_body = parse_end.duration_since(parse_start);

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
        header_for,
        symbols_from_requires,
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
        symbols_from_requires,
        header_for,
    };

    Ok(Msg::Parsed(parsed))
}

fn exposed_from_import<'a>(entry: &ImportsEntry<'a>) -> (QualifiedModuleName<'a>, Vec<Loc<Ident>>) {
    use roc_parse::header::ImportsEntry::*;

    match entry {
        Module(module_name, exposes) => {
            let mut exposed = Vec::with_capacity(exposes.len());

            for loc_entry in exposes.iter() {
                exposed.push(loc_entry.map(ident_from_exposed));
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
                exposed.push(loc_entry.map(ident_from_exposed));
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
    specializations_we_must_make: Vec<ExternalSpecializations<'a>>,
    mut module_timing: ModuleTiming,
    target_info: TargetInfo,
    world_abilities: WorldAbilities,
    exposed_by_module: &ExposedByModule,
    derived_module: SharedDerivedModule,
) -> Msg<'a> {
    let make_specializations_start = Instant::now();
    let mut update_mode_ids = UpdateModeIds::new();
    // do the thing
    let mut mono_env = roc_mono::ir::Env {
        arena,
        subs: &mut subs,
        home,
        ident_ids: &mut ident_ids,
        target_info,
        update_mode_ids: &mut update_mode_ids,
        // call_specialization_counter=0 is reserved
        call_specialization_counter: 1,
        abilities: AbilitiesView::World(&world_abilities),
        exposed_by_module,
        derived_module: &derived_module,
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
    let (procedures, restored_procs_base) = procs.get_specialized_procs_without_rc(&mut mono_env);

    // Turn `Bytes.Decode.IdentId(238)` into `Bytes.Decode.238`, we rely on this in mono tests
    mono_env.home.register_debug_idents(mono_env.ident_ids);

    let make_specializations_end = Instant::now();
    module_timing
        .make_specializations
        .push(make_specializations_end.duration_since(make_specializations_start));

    Msg::MadeSpecializations {
        module_id: home,
        ident_ids,
        layout_cache,
        procs_base: restored_procs_base,
        procedures,
        update_mode_ids,
        subs,
        external_specializations_requested,
        module_timing,
    }
}

#[allow(clippy::too_many_arguments)]
fn build_pending_specializations<'a>(
    arena: &'a Bump,
    solved_subs: Solved<Subs>,
    imported_module_thunks: &'a [Symbol],
    home: ModuleId,
    mut ident_ids: IdentIds,
    declarations: Declarations,
    mut module_timing: ModuleTiming,
    mut layout_cache: LayoutCache<'a>,
    target_info: TargetInfo,
    exposed_to_host: ExposedToHost,
    exposed_by_module: &ExposedByModule,
    abilities_store: AbilitiesStore,
    derived_module: SharedDerivedModule,
    build_expects: bool,
) -> Msg<'a> {
    let find_specializations_start = Instant::now();

    let mut module_thunks = bumpalo::collections::Vec::new_in(arena);
    let mut toplevel_expects = ToplevelExpects::default();

    let mut procs_base = ProcsBase {
        partial_procs: BumpMap::default(),
        module_thunks: &[],
        host_specializations: roc_mono::ir::HostSpecializations::new(),
        runtime_errors: BumpMap::default(),
        imported_module_thunks,
    };

    let mut update_mode_ids = UpdateModeIds::new();
    let mut subs = solved_subs.into_inner();
    let mut mono_env = roc_mono::ir::Env {
        arena,
        subs: &mut subs,
        home,
        ident_ids: &mut ident_ids,
        target_info,
        update_mode_ids: &mut update_mode_ids,
        // call_specialization_counter=0 is reserved
        call_specialization_counter: 1,
        // NB: for getting pending specializations the module view is enough because we only need
        // to know the types and abilities in our modules. Only for building *all* specializations
        // do we need a global view.
        abilities: AbilitiesView::Module(&abilities_store),
        exposed_by_module,
        derived_module: &derived_module,
    };

    // Add modules' decls to Procs
    for index in 0..declarations.len() {
        use roc_can::expr::DeclarationTag::*;

        let symbol = declarations.symbols[index].value;
        let expr_var = declarations.variables[index];

        let is_host_exposed = exposed_to_host.values.contains_key(&symbol);

        // TODO remove clones (with drain)
        let annotation = declarations.annotations[index].clone();
        let body = declarations.expressions[index].clone();

        let tag = declarations.declarations[index];
        match tag {
            Value => {
                // mark this symbols as a top-level thunk before any other work on the procs
                module_thunks.push(symbol);

                // If this is an exposed symbol, we need to
                // register it as such. Otherwise, since it
                // never gets called by Roc code, it will never
                // get specialized!
                if is_host_exposed {
                    let layout_result =
                        layout_cache.raw_from_var(mono_env.arena, expr_var, mono_env.subs);

                    // cannot specialize when e.g. main's type contains type variables
                    if let Err(e) = layout_result {
                        match e {
                            LayoutProblem::Erroneous => {
                                let message = "top level function has erroneous type";
                                procs_base.runtime_errors.insert(symbol, message);
                                continue;
                            }
                            LayoutProblem::UnresolvedTypeVar(v) => {
                                let message = format!(
                                    "top level function has unresolved type variable {:?}",
                                    v
                                );
                                procs_base
                                    .runtime_errors
                                    .insert(symbol, mono_env.arena.alloc(message));
                                continue;
                            }
                        }
                    }

                    procs_base.host_specializations.insert_host_exposed(
                        mono_env.subs,
                        LambdaName::no_niche(symbol),
                        annotation,
                        expr_var,
                    );
                }

                let proc = PartialProc {
                    annotation: expr_var,
                    // This is a 0-arity thunk, so it has no arguments.
                    pattern_symbols: &[],
                    // This is a top-level definition, so it cannot capture anything
                    captured_symbols: CapturedSymbols::None,
                    body: body.value,
                    body_var: expr_var,
                    // This is a 0-arity thunk, so it cannot be recursive
                    is_self_recursive: false,
                };

                procs_base.partial_procs.insert(symbol, proc);
            }
            Function(f_index) | Recursive(f_index) | TailRecursive(f_index) => {
                let function_def = &declarations.function_bodies[f_index.index()].value;
                // this is a top-level definition, it should not capture anything
                debug_assert!(
                    function_def.captured_symbols.is_empty(),
                    "{:?}",
                    (symbol, symbol.module_id(), &function_def.captured_symbols)
                );

                // If this is an exposed symbol, we need to
                // register it as such. Otherwise, since it
                // never gets called by Roc code, it will never
                // get specialized!
                if is_host_exposed {
                    let layout_result =
                        layout_cache.raw_from_var(mono_env.arena, expr_var, mono_env.subs);

                    // cannot specialize when e.g. main's type contains type variables
                    if let Err(e) = layout_result {
                        match e {
                            LayoutProblem::Erroneous => {
                                let message = "top level function has erroneous type";
                                procs_base.runtime_errors.insert(symbol, message);
                                continue;
                            }
                            LayoutProblem::UnresolvedTypeVar(v) => {
                                let message = format!(
                                    "top level function has unresolved type variable {:?}",
                                    v
                                );
                                procs_base
                                    .runtime_errors
                                    .insert(symbol, mono_env.arena.alloc(message));
                                continue;
                            }
                        }
                    }

                    procs_base.host_specializations.insert_host_exposed(
                        mono_env.subs,
                        LambdaName::no_niche(symbol),
                        annotation,
                        expr_var,
                    );
                }

                let is_recursive = matches!(tag, Recursive(_) | TailRecursive(_));

                let partial_proc = PartialProc::from_named_function(
                    &mut mono_env,
                    expr_var,
                    function_def.arguments.clone(),
                    body,
                    CapturedSymbols::None,
                    is_recursive,
                    function_def.return_type,
                );

                procs_base.partial_procs.insert(symbol, partial_proc);
            }
            Destructure(d_index) => {
                let loc_pattern = &declarations.destructs[d_index.index()].loc_pattern;

                use roc_can::pattern::Pattern;
                let symbol = match &loc_pattern.value {
                    Pattern::Identifier(_) => {
                        debug_assert!(false, "identifier ended up in Destructure {:?}", symbol);
                        symbol
                    }
                    Pattern::AbilityMemberSpecialization { ident, specializes } => {
                        debug_assert!(
                            false,
                            "ability member ended up in Destructure {:?} specializes {:?}",
                            ident, specializes
                        );
                        symbol
                    }
                    Pattern::Shadowed(_, _, shadowed) => {
                        // this seems to work for now
                        *shadowed
                    }
                    _ => todo!("top-level destrucuture patterns are not implemented"),
                };

                // mark this symbols as a top-level thunk before any other work on the procs
                module_thunks.push(symbol);

                // If this is an exposed symbol, we need to
                // register it as such. Otherwise, since it
                // never gets called by Roc code, it will never
                // get specialized!
                if is_host_exposed {
                    let layout_result =
                        layout_cache.raw_from_var(mono_env.arena, expr_var, mono_env.subs);

                    // cannot specialize when e.g. main's type contains type variables
                    if let Err(e) = layout_result {
                        match e {
                            LayoutProblem::Erroneous => {
                                let message = "top level function has erroneous type";
                                procs_base.runtime_errors.insert(symbol, message);
                                continue;
                            }
                            LayoutProblem::UnresolvedTypeVar(v) => {
                                let message = format!(
                                    "top level function has unresolved type variable {:?}",
                                    v
                                );
                                procs_base
                                    .runtime_errors
                                    .insert(symbol, mono_env.arena.alloc(message));
                                continue;
                            }
                        }
                    }

                    procs_base.host_specializations.insert_host_exposed(
                        mono_env.subs,
                        LambdaName::no_niche(symbol),
                        annotation,
                        expr_var,
                    );
                }

                let proc = PartialProc {
                    annotation: expr_var,
                    // This is a 0-arity thunk, so it has no arguments.
                    pattern_symbols: &[],
                    // This is a top-level definition, so it cannot capture anything
                    captured_symbols: CapturedSymbols::None,
                    body: body.value,
                    body_var: expr_var,
                    // This is a 0-arity thunk, so it cannot be recursive
                    is_self_recursive: false,
                };

                procs_base.partial_procs.insert(symbol, proc);
            }
            MutualRecursion { .. } => {
                // the declarations of this group will be treaded individually by later iterations
            }
            Expectation => {
                // skip expectations if we're not going to run them
                if !build_expects {
                    continue;
                }

                // mark this symbol as a top-level thunk before any other work on the procs
                module_thunks.push(symbol);

                let expr_var = Variable::EMPTY_RECORD;

                let is_host_exposed = true;

                // If this is an exposed symbol, we need to
                // register it as such. Otherwise, since it
                // never gets called by Roc code, it will never
                // get specialized!
                if is_host_exposed {
                    let layout_result =
                        layout_cache.raw_from_var(mono_env.arena, expr_var, mono_env.subs);

                    // cannot specialize when e.g. main's type contains type variables
                    if let Err(e) = layout_result {
                        match e {
                            LayoutProblem::Erroneous => {
                                let message = "top level function has erroneous type";
                                procs_base.runtime_errors.insert(symbol, message);
                                continue;
                            }
                            LayoutProblem::UnresolvedTypeVar(v) => {
                                let message = format!(
                                    "top level function has unresolved type variable {:?}",
                                    v
                                );
                                procs_base
                                    .runtime_errors
                                    .insert(symbol, mono_env.arena.alloc(message));
                                continue;
                            }
                        }
                    }

                    procs_base.host_specializations.insert_host_exposed(
                        mono_env.subs,
                        LambdaName::no_niche(symbol),
                        annotation,
                        expr_var,
                    );
                }

                let body = roc_can::expr::toplevel_expect_to_inline_expect_pure(body);

                let proc = PartialProc {
                    annotation: expr_var,
                    // This is a 0-arity thunk, so it has no arguments.
                    pattern_symbols: &[],
                    // This is a top-level definition, so it cannot capture anything
                    captured_symbols: CapturedSymbols::None,
                    body: body.value,
                    body_var: expr_var,
                    // This is a 0-arity thunk, so it cannot be recursive
                    is_self_recursive: false,
                };

                // extend the region of the expect expression with the region of the preceding
                // comment, so it is shown in failure/panic messages
                let name_region = declarations.symbols[index].region;
                let expr_region = declarations.expressions[index].region;
                let region = Region::span_across(&name_region, &expr_region);

                toplevel_expects.pure.insert(symbol, region);
                procs_base.partial_procs.insert(symbol, proc);
            }
            ExpectationFx => {
                // skip expectations if we're not going to run them
                if !build_expects {
                    continue;
                }

                // mark this symbol as a top-level thunk before any other work on the procs
                module_thunks.push(symbol);

                let expr_var = Variable::EMPTY_RECORD;

                let is_host_exposed = true;

                // If this is an exposed symbol, we need to
                // register it as such. Otherwise, since it
                // never gets called by Roc code, it will never
                // get specialized!
                if is_host_exposed {
                    let layout_result =
                        layout_cache.raw_from_var(mono_env.arena, expr_var, mono_env.subs);

                    // cannot specialize when e.g. main's type contains type variables
                    if let Err(e) = layout_result {
                        match e {
                            LayoutProblem::Erroneous => {
                                let message = "top level function has erroneous type";
                                procs_base.runtime_errors.insert(symbol, message);
                                continue;
                            }
                            LayoutProblem::UnresolvedTypeVar(v) => {
                                let message = format!(
                                    "top level function has unresolved type variable {:?}",
                                    v
                                );
                                procs_base
                                    .runtime_errors
                                    .insert(symbol, mono_env.arena.alloc(message));
                                continue;
                            }
                        }
                    }

                    procs_base.host_specializations.insert_host_exposed(
                        mono_env.subs,
                        LambdaName::no_niche(symbol),
                        annotation,
                        expr_var,
                    );
                }

                let body = roc_can::expr::toplevel_expect_to_inline_expect_fx(body);

                let proc = PartialProc {
                    annotation: expr_var,
                    // This is a 0-arity thunk, so it has no arguments.
                    pattern_symbols: &[],
                    // This is a top-level definition, so it cannot capture anything
                    captured_symbols: CapturedSymbols::None,
                    body: body.value,
                    body_var: expr_var,
                    // This is a 0-arity thunk, so it cannot be recursive
                    is_self_recursive: false,
                };

                // extend the region of the expect expression with the region of the preceding
                // comment, so it is shown in failure/panic messages
                let name_region = declarations.symbols[index].region;
                let expr_region = declarations.expressions[index].region;
                let region = Region::span_across(&name_region, &expr_region);

                toplevel_expects.fx.insert(symbol, region);
                procs_base.partial_procs.insert(symbol, proc);
            }
        }
    }

    procs_base.module_thunks = module_thunks.into_bump_slice();

    let find_specializations_end = Instant::now();
    module_timing.find_specializations =
        find_specializations_end.duration_since(find_specializations_start);

    Msg::FoundSpecializations {
        module_id: home,
        solved_subs: Solved(subs),
        ident_ids,
        layout_cache,
        procs_base,
        module_timing,
        abilities_store,
        toplevel_expects,
    }
}

/// Loads derived ability members up for specialization into the Derived module, prior to making
/// their specializations.
// TODO: right now, this runs sequentially, and no other modules are mono'd in parallel to the
// derived module.
#[allow(clippy::too_many_arguments)]
fn load_derived_partial_procs<'a>(
    home: ModuleId,
    arena: &'a Bump,
    subs: &mut Subs,
    ident_ids: &mut IdentIds,
    derived_module: &SharedDerivedModule,
    module_timing: &mut ModuleTiming,
    target_info: TargetInfo,
    exposed_by_module: &ExposedByModule,
    procs_base: &mut ProcsBase<'a>,
    world_abilities: &mut WorldAbilities,
) {
    debug_assert_eq!(home, ModuleId::DERIVED_GEN);

    let load_derived_procs_start = Instant::now();

    let mut new_module_thunks = bumpalo::collections::Vec::new_in(arena);

    let mut update_mode_ids = UpdateModeIds::new();

    let derives_to_add = {
        let mut derived_module = derived_module.lock().unwrap();

        derived_module.iter_load_for_gen_module(subs, |symbol| {
            !procs_base.partial_procs.contains_key(&symbol)
        })
    };

    // TODO: we can be even lazier here if we move `add_def_to_module` to happen in mono. Also, the
    // timings would be more accurate.
    for (derived_symbol, (derived_expr, derived_expr_var)) in derives_to_add.into_iter() {
        let mut mono_env = roc_mono::ir::Env {
            arena,
            subs,
            home,
            ident_ids,
            target_info,
            update_mode_ids: &mut update_mode_ids,
            // call_specialization_counter=0 is reserved
            call_specialization_counter: 1,
            // NB: for getting pending specializations the module view is enough because we only need
            // to know the types and abilities in our modules. Only for building *all* specializations
            // do we need a global view.
            abilities: AbilitiesView::World(world_abilities),
            exposed_by_module,
            derived_module,
        };

        let partial_proc = match derived_expr {
            roc_can::expr::Expr::Closure(roc_can::expr::ClosureData {
                function_type,
                arguments,
                loc_body,
                captured_symbols,
                return_type,
                recursive,
                ..
            }) => {
                debug_assert!(captured_symbols.is_empty());
                PartialProc::from_named_function(
                    &mut mono_env,
                    function_type,
                    arguments.clone(),
                    *loc_body,
                    CapturedSymbols::None,
                    recursive.is_recursive(),
                    return_type,
                )
            }
            _ => {
                // mark this symbols as a top-level thunk before any other work on the procs
                new_module_thunks.push(derived_symbol);

                PartialProc {
                    annotation: derived_expr_var,
                    // This is a 0-arity thunk, so it has no arguments.
                    pattern_symbols: &[],
                    // This is a top-level definition, so it cannot capture anything
                    captured_symbols: CapturedSymbols::None,
                    body: derived_expr,
                    body_var: derived_expr_var,
                    // This is a 0-arity thunk, so it cannot be recursive
                    is_self_recursive: false,
                }
            }
        };

        procs_base
            .partial_procs
            .insert(derived_symbol, partial_proc);
    }

    if !new_module_thunks.is_empty() {
        new_module_thunks.extend(procs_base.module_thunks);
        procs_base.module_thunks = new_module_thunks.into_bump_slice();
    }

    let load_derived_procs_end = Instant::now();

    module_timing.find_specializations =
        load_derived_procs_end.duration_since(load_derived_procs_start);
}

fn run_task<'a>(
    task: BuildTask<'a>,
    arena: &'a Bump,
    src_dir: &Path,
    msg_tx: MsgSender<'a>,
    roc_cache_dir: RocCacheDir<'_>,
    target_info: TargetInfo,
) -> Result<(), LoadingProblem<'a>> {
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
            roc_cache_dir,
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
            abilities_store,
            skip_constraint_gen,
        } => {
            let can_and_con = canonicalize_and_constrain(
                arena,
                &module_ids,
                dep_idents,
                exposed_symbols,
                aliases,
                abilities_store,
                parsed,
                skip_constraint_gen,
            );

            Ok(Msg::CanonicalizedAndConstrained(can_and_con))
        }
        Solve {
            module,
            module_timing,
            exposed_for_module,
            types,
            constraints,
            constraint,
            pending_derives,
            var_store,
            ident_ids,
            declarations,
            dep_idents,
            cached_subs,
            derived_module,
        } => Ok(run_solve(
            module,
            ident_ids,
            module_timing,
            exposed_for_module,
            types,
            constraints,
            constraint,
            pending_derives,
            var_store,
            declarations,
            dep_idents,
            cached_subs,
            derived_module,
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
            abilities_store,
            exposed_by_module,
            derived_module,
            build_expects,
        } => Ok(build_pending_specializations(
            arena,
            solved_subs,
            imported_module_thunks,
            module_id,
            ident_ids,
            decls,
            module_timing,
            layout_cache,
            target_info,
            exposed_to_host,
            &exposed_by_module,
            abilities_store,
            derived_module,
            build_expects,
        )),
        MakeSpecializations {
            module_id,
            ident_ids,
            subs,
            procs_base,
            layout_cache,
            specializations_we_must_make,
            module_timing,
            world_abilities,
            exposed_by_module,
            derived_module,
        } => Ok(make_specializations(
            arena,
            module_id,
            ident_ids,
            subs,
            procs_base,
            layout_cache,
            specializations_we_must_make,
            module_timing,
            target_info,
            world_abilities,
            &exposed_by_module,
            derived_module,
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
            let doc = alloc.stack([
                alloc.reflow(r"I am looking for this file, but it's not there:"),
                alloc
                    .parser_suggestion(filename.to_str().unwrap())
                    .indent(4),
                alloc.concat([
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
            let doc = alloc.stack([
                alloc.reflow(r"I don't have the required permissions to read this file:"),
                alloc
                    .parser_suggestion(filename.to_str().unwrap())
                    .indent(4),
                alloc
                    .concat([alloc.reflow(r"Is it the right file? Maybe change its permissions?")]),
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
            let doc = alloc.stack([
                alloc.reflow(r"I tried to read this file:"),
                alloc
                    .text(filename.to_str().unwrap())
                    .annotate(Annotation::Error)
                    .indent(4),
                alloc.reflow(r"But ran into:"),
                alloc.text(formatted).annotate(Annotation::Error).indent(4),
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

fn to_import_cycle_report(
    module_ids: ModuleIds,
    all_ident_ids: IdentIdsByModule,
    import_cycle: Vec<ModuleId>,
    filename: PathBuf,
    render: RenderTarget,
) -> String {
    use roc_reporting::report::{Report, RocDocAllocator, Severity, DEFAULT_PALETTE};
    use ven_pretty::DocAllocator;

    // import_cycle looks like CycleModule, Import1, ..., ImportN, CycleModule
    // In a self-referential case, it just looks like CycleModule, CycleModule.
    debug_assert!(import_cycle.len() >= 2);
    let source_of_cycle = import_cycle.first().unwrap();

    // We won't be printing any lines for this report, so this is okay.
    // TODO: it would be nice to show how each module imports another in the cycle.
    let src_lines = &[];

    let interns = Interns {
        module_ids,
        all_ident_ids,
    };
    let alloc = RocDocAllocator::new(src_lines, *source_of_cycle, &interns);

    let doc = alloc.stack([
        alloc.concat([
            alloc.reflow("I can't compile "),
            alloc.module(*source_of_cycle),
            alloc.reflow(
                " because it depends on itself through the following chain of module imports:",
            ),
        ]),
        roc_reporting::report::cycle(
            &alloc,
            4,
            alloc.module(*source_of_cycle),
            import_cycle
                .into_iter()
                .skip(1)
                .map(|module| alloc.module(module))
                .collect(),
        ),
        alloc.reflow("Cyclic dependencies are not allowed in Roc! Can you restructure a module in this import chain so that it doesn't have to depend on itself?")
    ]);

    let report = Report {
        filename,
        doc,
        title: "IMPORT CYCLE".to_string(),
        severity: Severity::RuntimeError,
    };

    let mut buf = String::new();
    let palette = DEFAULT_PALETTE;
    report.render(render, &mut buf, &alloc, &palette);
    buf
}

fn to_incorrect_module_name_report<'a>(
    module_ids: ModuleIds,
    all_ident_ids: IdentIdsByModule,
    problem: IncorrectModuleName<'a>,
    filename: PathBuf,
    src: &'a [u8],
    render: RenderTarget,
) -> String {
    use roc_reporting::report::{Report, RocDocAllocator, Severity, DEFAULT_PALETTE};
    use ven_pretty::DocAllocator;

    let IncorrectModuleName {
        module_id,
        found,
        expected,
    } = problem;

    // SAFETY: if the module was not UTF-8, that would be reported as a parsing problem, rather
    // than an incorrect module name problem (the latter can happen only after parsing).
    let src = unsafe { from_utf8_unchecked(src) };
    let src_lines = src.lines().collect::<Vec<_>>();
    let lines = LineInfo::new(src);

    let interns = Interns {
        module_ids,
        all_ident_ids,
    };
    let alloc = RocDocAllocator::new(&src_lines, module_id, &interns);

    let doc = alloc.stack([
        alloc.reflow("This module has a different name than I expected:"),
        alloc.region(lines.convert_region(found.region)),
        alloc.reflow("Based on the nesting and use of this module, I expect it to have name"),
        alloc.pq_module_name(expected).indent(4),
    ]);

    let report = Report {
        filename,
        doc,
        title: "INCORRECT MODULE NAME".to_string(),
        severity: Severity::RuntimeError,
    };

    let mut buf = String::new();
    let palette = DEFAULT_PALETTE;
    report.render(render, &mut buf, &alloc, &palette);
    buf
}

fn to_parse_problem_report<'a>(
    problem: FileError<'a, SyntaxError<'a>>,
    mut module_ids: ModuleIds,
    all_ident_ids: IdentIdsByModule,
    render: RenderTarget,
    palette: Palette,
) -> String {
    use roc_reporting::report::{parse_problem, RocDocAllocator};

    // TODO this is not in fact safe
    let src = unsafe { from_utf8_unchecked(problem.problem.bytes) };
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

    report.render(render, &mut buf, &alloc, &palette);

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
                let doc = alloc.stack([
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
                let doc = alloc.stack([
                    alloc.reflow(
                        r"The input file is an `interface` module, but only `app` modules can be run.",
                    ),
                    alloc.reflow(r"Tip: You can use `roc check` or `roc test` to verify an interface module like this one."),
                ]);

                Report {
                    filename: "UNKNOWN.roc".into(),
                    doc,
                    title: "NO PLATFORM".to_string(),
                    severity: Severity::RuntimeError,
                }
            }
            RootIsHosted => {
                let doc = alloc.stack([
                    alloc.reflow(
                        r"The input file is a `hosted` module, but only `app` modules can be run.",
                    ),
                    alloc.reflow(r"Tip: You can use `roc check` or `roc test` to verify a hosted module like this one."),
                ]);

                Report {
                    filename: "UNKNOWN.roc".into(),
                    doc,
                    title: "NO PLATFORM".to_string(),
                    severity: Severity::RuntimeError,
                }
            }
            RootIsPlatformModule => {
                let doc = alloc.stack([
                    alloc.reflow(
                        r"The input file is a `platform` module, but only `app` modules can be run.",
                    ),
                    alloc.reflow(r"Tip: You can use `roc check` or `roc test` to verify a platform module like this one."),
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
