use crate::docs::ModuleDocumentation;
use roc_can::constraint::{Constraint as ConstraintSoa, Constraints};
use roc_can::expr::ExpectLookup;
use roc_can::{
    abilities::AbilitiesStore,
    expr::{Declarations, PendingDerives},
    module::{Module, ResolvedImplementations},
};
use roc_collections::{MutMap, MutSet, VecMap};
use roc_module::ident::Ident;
use roc_module::symbol::{
    IdentIds, IdentIdsByModule, Interns, ModuleId, PQModuleName, PackageQualified, Symbol,
};
use roc_mono::ir::{GlueLayouts, HostExposedLambdaSets, LambdaSetId, Proc, ProcLayout, ProcsBase};
use roc_mono::layout::{LayoutCache, STLayoutInterner};
use roc_parse::ast::{CommentOrNewline, Defs, TypeAnnotation};
use roc_parse::header::{HeaderType, PackageName};
use roc_region::all::{Loc, Region};
use roc_solve::module::Solved;
use roc_solve_problem::TypeError;
use roc_types::subs::{ExposedTypesStorageSubs, Subs, VarStore, Variable};
use roc_types::types::{Alias, Types};
use std::path::PathBuf;

#[cfg(target_family = "wasm")]
use crate::wasm_instant::{Duration, Instant};
#[cfg(not(target_family = "wasm"))]
use std::time::{Duration, Instant};

#[derive(Debug)]
pub struct LoadedModule {
    pub module_id: ModuleId,
    pub filename: PathBuf,
    pub interns: Interns,
    pub solved: Solved<Subs>,
    pub can_problems: MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    pub type_problems: MutMap<ModuleId, Vec<TypeError>>,
    pub declarations_by_id: MutMap<ModuleId, Declarations>,
    pub exposed_to_host: MutMap<Symbol, Variable>,
    pub dep_idents: IdentIdsByModule,
    pub exposed_aliases: MutMap<Symbol, Alias>,
    pub exposed_modules: Vec<ModuleId>,
    pub exposed_values: Vec<Symbol>,
    pub exposed_types_storage: ExposedTypesStorageSubs,
    pub resolved_implementations: ResolvedImplementations,
    pub sources: MutMap<ModuleId, (PathBuf, Box<str>)>,
    pub timings: MutMap<ModuleId, ModuleTiming>,
    pub docs_by_module: VecMap<ModuleId, ModuleDocumentation>,
    pub abilities_store: AbilitiesStore,
    pub typechecked: MutMap<ModuleId, CheckedModule>,

    pub imports: MutMap<ModuleId, MutSet<ModuleId>>,
    pub exposed_imports: MutMap<ModuleId, MutMap<Symbol, Region>>,
    pub exposes: MutMap<ModuleId, Vec<(Symbol, Variable)>>,
}

impl LoadedModule {
    /// Infer the filename for the given ModuleId, based on this root module's filename.
    pub fn filename(&self, module_id: ModuleId) -> PathBuf {
        let module_name = self.interns.module_name(module_id);

        module_name.filename(&self.filename)
    }

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

    pub fn exposed_aliases_str(&self) -> Vec<&str> {
        self.exposed_aliases
            .keys()
            .map(|symbol| symbol.as_str(&self.interns))
            .collect()
    }
}

#[derive(Debug)]
pub(crate) struct ModuleHeader<'a> {
    pub(crate) module_id: ModuleId,
    pub(crate) module_path: PathBuf,
    pub(crate) is_root_module: bool,
    pub(crate) packages: MutMap<&'a str, PackageName<'a>>,
    pub(crate) parse_state: roc_parse::state::State<'a>,
    pub(crate) header_type: HeaderType<'a>,
    pub(crate) header_comments: &'a [CommentOrNewline<'a>],
    pub(crate) header_imports: Option<roc_parse::header::ImportsKeywordItem<'a>>,
    pub(crate) module_timing: ModuleTiming,
    pub(crate) opt_shorthand: Option<&'a str>,
}

#[derive(Debug)]
pub(crate) struct ConstrainedModule {
    pub(crate) module: Module,
    pub(crate) declarations: Declarations,
    pub(crate) available_modules: MutMap<ModuleId, Region>,
    pub(crate) constraints: Constraints,
    pub(crate) constraint: ConstraintSoa,
    pub(crate) ident_ids: IdentIds,
    pub(crate) var_store: VarStore,
    pub(crate) dep_idents: IdentIdsByModule,
    pub(crate) module_timing: ModuleTiming,
    pub(crate) types: Types,
    // Rather than adding pending derives as constraints, hand them directly to solve because they
    // must be solved at the end of a module.
    pub(crate) pending_derives: PendingDerives,
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
    pub expectations: Option<Expectations>,

    #[cfg(debug_assertions)]
    pub checkmate: Option<roc_checkmate::Collector>,
}

#[derive(Debug)]
pub struct CheckedModule {
    pub solved_subs: Solved<Subs>,
    pub decls: Declarations,
    pub abilities_store: AbilitiesStore,
}

#[derive(Debug)]
pub(crate) struct FoundSpecializationsModule<'a> {
    pub(crate) ident_ids: IdentIds,
    pub(crate) layout_cache: LayoutCache<'a>,
    pub(crate) procs_base: ProcsBase<'a>,
    pub(crate) subs: Subs,
    pub(crate) module_timing: ModuleTiming,
    pub(crate) expectations: Option<Expectations>,
}

#[derive(Debug)]
pub(crate) struct LateSpecializationsModule<'a> {
    pub(crate) ident_ids: IdentIds,
    pub(crate) subs: Subs,
    pub(crate) module_timing: ModuleTiming,
    pub(crate) layout_cache: LayoutCache<'a>,
    pub(crate) procs_base: ProcsBase<'a>,
    pub(crate) expectations: Option<Expectations>,
}

#[derive(Debug, Default)]
pub struct ToplevelExpects {
    pub pure: VecMap<Symbol, Region>,
}

#[derive(Debug)]
pub struct MonomorphizedModule<'a> {
    pub module_id: ModuleId,
    pub interns: Interns,
    pub subs: Subs,
    pub layout_interner: STLayoutInterner<'a>,
    pub can_problems: MutMap<ModuleId, Vec<roc_problem::can::Problem>>,
    pub type_problems: MutMap<ModuleId, Vec<TypeError>>,
    pub procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    pub host_exposed_lambda_sets: HostExposedLambdaSets<'a>,
    pub toplevel_expects: MutMap<ModuleId, ToplevelExpects>,
    pub entry_point: EntryPoint<'a>,
    pub exposed_to_host: ExposedToHost,
    pub sources: MutMap<ModuleId, (PathBuf, Box<str>)>,
    pub timings: MutMap<ModuleId, ModuleTiming>,
    pub expectations: VecMap<ModuleId, Expectations>,
    pub needs_prebuilt_host: bool,
    pub glue_layouts: GlueLayouts<'a>,
}

#[derive(Debug, Clone)]
pub struct ParsedModule<'a> {
    pub module_id: ModuleId,
    pub module_path: PathBuf,
    pub src: &'a str,
    pub module_timing: ModuleTiming,
    pub deps_by_name: MutMap<PQModuleName<'a>, ModuleId>,
    pub exposed_ident_ids: IdentIds,
    pub parsed_defs: Defs<'a>,
    pub symbols_from_requires: Vec<(Loc<Symbol>, Loc<TypeAnnotation<'a>>)>,
    pub header_type: HeaderType<'a>,
    pub header_comments: &'a [CommentOrNewline<'a>],
    pub available_modules: MutMap<ModuleId, Region>,
    pub package_qualified_available_modules: MutSet<PackageQualified<'a, ModuleId>>,
    pub packages: MutMap<&'a str, PackageName<'a>>,
    pub initial_scope: MutMap<Ident, (Symbol, Region)>,
    pub exposes: Vec<Symbol>,
    pub opt_shorthand: Option<&'a str>,
}

#[derive(Debug)]
pub enum EntryPoint<'a> {
    Executable {
        exposed_to_host: &'a [(&'a str, Symbol, ProcLayout<'a>)],
        platform_path: PathBuf,
    },
    Test,
}

#[derive(Debug)]
pub struct Expectations {
    pub subs: roc_types::subs::Subs,
    pub path: PathBuf,
    pub expectations: VecMap<Region, Vec<ExpectLookup>>,
    pub ident_ids: IdentIds,
}

#[derive(Clone, Debug, Default)]
pub struct ExposedToHost {
    /// usually `main_for_host`
    pub top_level_values: MutMap<Symbol, Variable>,
    /// exposed closure types, typically `Fx`
    pub closure_types: Vec<Symbol>,
    /// lambda_sets
    pub lambda_sets: Vec<(Symbol, LambdaSetId)>,
    pub getters: Vec<Symbol>,
}

#[derive(Debug, Clone)]
pub struct ModuleTiming {
    pub read_roc_file: Duration,
    pub parse_header: Duration,
    pub parse_body: Duration,
    pub canonicalize_solo: Duration,
    pub canonicalize: Duration,
    pub constrain: Duration,
    pub solve: Duration,
    pub find_specializations: Duration,
    // indexed by make specializations pass
    pub make_specializations: Vec<Duration>,
    // TODO pub monomorphize: Duration,
    /// Total duration will always be more than the sum of the other fields, due
    /// to things like state lookups in between phases, waiting on other threads, etc.
    pub start_time: Instant,
    pub end_time: Instant,
}

impl ModuleTiming {
    pub fn new(start_time: Instant) -> Self {
        ModuleTiming {
            read_roc_file: Duration::default(),
            parse_header: Duration::default(),
            parse_body: Duration::default(),
            canonicalize_solo: Duration::default(),
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
            canonicalize_solo,
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
                .checked_sub(*canonicalize_solo)?
                .checked_sub(*canonicalize)?
                .checked_sub(*parse_body)?
                .checked_sub(*parse_header)?
                .checked_sub(*read_roc_file)
        };

        calculate(Some(end_time.duration_since(*start_time))).unwrap_or_default()
    }
}
