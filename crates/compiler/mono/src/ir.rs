#![allow(clippy::manual_map)]

use crate::layout::{
    self, Builtin, CapturesNiche, ClosureCallOptions, ClosureRepresentation, EnumDispatch,
    LambdaName, LambdaSet, Layout, LayoutCache, LayoutInterner, LayoutProblem, RawFunctionLayout,
    STLayoutInterner, TagIdIntType, UnionLayout, WrappedVariant,
};
use bumpalo::collections::{CollectIn, Vec};
use bumpalo::Bump;
use roc_builtins::bitcode::{FloatWidth, IntWidth};
use roc_can::abilities::SpecializationId;
use roc_can::expr::{AnnotatedMark, ClosureData, ExpectLookup, IntValue};
use roc_can::module::ExposedByModule;
use roc_collections::all::{default_hasher, BumpMap, BumpMapDefault, MutMap};
use roc_collections::VecMap;
use roc_debug_flags::dbg_do;
#[cfg(debug_assertions)]
use roc_debug_flags::{
    ROC_PRINT_IR_AFTER_REFCOUNT, ROC_PRINT_IR_AFTER_RESET_REUSE, ROC_PRINT_IR_AFTER_SPECIALIZATION,
    ROC_PRINT_RUNTIME_ERROR_GEN,
};
use roc_derive::SharedDerivedModule;
use roc_error_macros::{internal_error, todo_abilities};
use roc_exhaustive::{Ctor, CtorName, ListArity, RenderAs, TagId};
use roc_intern::Interner;
use roc_late_solve::storage::{ExternalModuleStorage, ExternalModuleStorageSnapshot};
use roc_late_solve::{resolve_ability_specialization, AbilitiesView, Resolved, UnificationFailed};
use roc_module::ident::{ForeignSymbol, Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_problem::can::{RuntimeError, ShadowKind};
use roc_region::all::{Loc, Region};
use roc_std::RocDec;
use roc_target::TargetInfo;
use roc_types::subs::{
    instantiate_rigids, Content, ExhaustiveMark, FlatType, RedundantMark, StorageSubs, Subs,
    Variable, VariableSubsSlice,
};
use std::collections::HashMap;
use ven_pretty::{BoxAllocator, DocAllocator, DocBuilder};

#[inline(always)]
pub fn pretty_print_ir_symbols() -> bool {
    dbg_do!(ROC_PRINT_IR_AFTER_SPECIALIZATION, {
        return true;
    });
    dbg_do!(ROC_PRINT_IR_AFTER_RESET_REUSE, {
        return true;
    });
    dbg_do!(ROC_PRINT_IR_AFTER_REFCOUNT, {
        return true;
    });
    false
}

// if your changes cause this number to go down, great!
// please change it to the lower number.
// if it went up, maybe check that the change is really required

roc_error_macros::assert_sizeof_wasm!(Literal, 24);
roc_error_macros::assert_sizeof_wasm!(Expr, 48);
roc_error_macros::assert_sizeof_wasm!(Stmt, 120);
roc_error_macros::assert_sizeof_wasm!(ProcLayout, 40);
roc_error_macros::assert_sizeof_wasm!(Call, 44);
roc_error_macros::assert_sizeof_wasm!(CallType, 36);

roc_error_macros::assert_sizeof_non_wasm!(Literal, 3 * 8);
roc_error_macros::assert_sizeof_non_wasm!(Expr, 10 * 8);
roc_error_macros::assert_sizeof_non_wasm!(Stmt, 19 * 8);
roc_error_macros::assert_sizeof_non_wasm!(ProcLayout, 8 * 8);
roc_error_macros::assert_sizeof_non_wasm!(Call, 9 * 8);
roc_error_macros::assert_sizeof_non_wasm!(CallType, 7 * 8);

fn runtime_error<'a>(env: &mut Env<'a, '_>, msg: &'a str) -> Stmt<'a> {
    let sym = env.unique_symbol();
    Stmt::Let(
        sym,
        Expr::Literal(Literal::Str(msg)),
        Layout::Builtin(Builtin::Str),
        env.arena.alloc(Stmt::Crash(sym, CrashTag::Roc)),
    )
}

macro_rules! return_on_layout_error {
    ($env:expr, $layout_result:expr, $context_msg:expr) => {
        match $layout_result {
            Ok(cached) => cached,
            Err(error) => return_on_layout_error_help!($env, error, $context_msg),
        }
    };
}

macro_rules! return_on_layout_error_help {
    ($env:expr, $error:expr, $context_msg:expr) => {{
        match $error {
            LayoutProblem::UnresolvedTypeVar(_) => {
                return runtime_error(
                    $env,
                    $env.arena
                        .alloc(format!("UnresolvedTypeVar: {}", $context_msg,)),
                )
            }
            LayoutProblem::Erroneous => {
                return runtime_error(
                    $env,
                    $env.arena.alloc(format!("Erroneous: {}", $context_msg,)),
                )
            }
        }
    }};
}

#[derive(Debug, Clone, Copy)]
pub enum OptLevel {
    Development,
    Normal,
    Size,
    Optimize,
}

#[derive(Debug, Clone, Copy)]
pub struct EntryPoint<'a> {
    pub symbol: Symbol,
    pub layout: ProcLayout<'a>,
}

#[derive(Clone, Copy, Debug)]
pub struct PartialProcId(usize);

#[derive(Clone, Debug)]
pub struct PartialProcs<'a> {
    /// maps a function name (symbol) to an index
    symbols: Vec<'a, Symbol>,

    /// An entry (a, b) means `a` directly references the lambda value of `b`,
    /// i.e. this came from a `let a = b in ...` where `b` was defined as a
    /// lambda earlier.
    references: Vec<'a, (Symbol, Symbol)>,

    partial_procs: Vec<'a, PartialProc<'a>>,
}

impl<'a> PartialProcs<'a> {
    fn new_in(arena: &'a Bump) -> Self {
        Self {
            symbols: Vec::new_in(arena),
            references: Vec::new_in(arena),
            partial_procs: Vec::new_in(arena),
        }
    }
    fn contains_key(&self, symbol: Symbol) -> bool {
        self.symbol_to_id(symbol).is_some()
    }

    fn symbol_to_id(&self, mut symbol: Symbol) -> Option<PartialProcId> {
        while let Some(real_symbol) = self
            .references
            .iter()
            .find(|(alias, _)| *alias == symbol)
            .map(|(_, real)| real)
        {
            symbol = *real_symbol;
        }

        self.symbols
            .iter()
            .position(|s| *s == symbol)
            .map(PartialProcId)
    }

    fn get_symbol(&self, symbol: Symbol) -> Option<&PartialProc<'a>> {
        let id = self.symbol_to_id(symbol)?;

        Some(self.get_id(id))
    }

    fn get_id(&self, id: PartialProcId) -> &PartialProc<'a> {
        &self.partial_procs[id.0]
    }

    pub fn insert(&mut self, symbol: Symbol, partial_proc: PartialProc<'a>) -> PartialProcId {
        debug_assert!(
            !self.contains_key(symbol),
            "The {:?} is inserted as a partial proc twice: that's a bug!",
            symbol,
        );

        let id = PartialProcId(self.symbols.len());

        self.symbols.push(symbol);
        self.partial_procs.push(partial_proc);

        id
    }

    pub fn insert_alias(&mut self, alias: Symbol, real_symbol: Symbol) {
        debug_assert!(
            !self.contains_key(alias),
            "{:?} is inserted as a partial proc twice: that's a bug!",
            alias,
        );
        debug_assert!(
            self.contains_key(real_symbol),
            "{:?} is not a partial proc or another alias: that's a bug!",
            real_symbol,
        );

        self.references.push((alias, real_symbol));
    }

    pub fn drain(self) -> impl Iterator<Item = (Symbol, PartialProc<'a>)> {
        debug_assert_eq!(self.symbols.len(), self.partial_procs.len());

        self.symbols.into_iter().zip(self.partial_procs.into_iter())
    }
}

#[derive(Clone, Debug)]
pub struct PartialProc<'a> {
    pub annotation: Variable,
    pub pattern_symbols: &'a [Symbol],
    pub captured_symbols: CapturedSymbols<'a>,
    pub body: roc_can::expr::Expr,
    pub body_var: Variable,
    pub is_self_recursive: bool,
}

impl<'a> PartialProc<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn from_named_function(
        env: &mut Env<'a, '_>,
        annotation: Variable,
        loc_args: std::vec::Vec<(Variable, AnnotatedMark, Loc<roc_can::pattern::Pattern>)>,
        loc_body: Loc<roc_can::expr::Expr>,
        captured_symbols: CapturedSymbols<'a>,
        is_self_recursive: bool,
        ret_var: Variable,
    ) -> PartialProc<'a> {
        let number_of_arguments = loc_args.len();

        match patterns_to_when(env, loc_args, ret_var, loc_body) {
            Ok((_, pattern_symbols, body)) => {
                // a named closure. Since these aren't specialized by the surrounding
                // context, we can't add pending specializations for them yet.
                // (If we did, all named polymorphic functions would immediately error
                // on trying to convert a flex var to a Layout.)
                let pattern_symbols = pattern_symbols.into_bump_slice();
                PartialProc {
                    annotation,
                    pattern_symbols,
                    captured_symbols,
                    body: body.value,
                    body_var: ret_var,
                    is_self_recursive,
                }
            }

            Err(error) => {
                let mut pattern_symbols = Vec::with_capacity_in(number_of_arguments, env.arena);

                for _ in 0..number_of_arguments {
                    pattern_symbols.push(env.unique_symbol());
                }

                PartialProc {
                    annotation,
                    pattern_symbols: pattern_symbols.into_bump_slice(),
                    captured_symbols: CapturedSymbols::None,
                    body: roc_can::expr::Expr::RuntimeError(error.value),
                    body_var: ret_var,
                    is_self_recursive: false,
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct AbilityMember(Symbol);

/// A table of aliases of ability member symbols.
#[derive(Clone, Debug)]
struct AbilityAliases(BumpMap<Symbol, AbilityMember>);

impl AbilityAliases {
    fn new_in(arena: &Bump) -> Self {
        Self(BumpMap::new_in(arena))
    }

    fn insert(&mut self, symbol: Symbol, member: AbilityMember) {
        self.0.insert(symbol, member);
    }

    fn get(&self, symbol: Symbol) -> Option<&AbilityMember> {
        self.0.get(&symbol)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CapturedSymbols<'a> {
    None,
    Captured(&'a [(Symbol, Variable)]),
}

impl<'a> CapturedSymbols<'a> {
    fn captures(&self) -> bool {
        match self {
            CapturedSymbols::None => false,
            CapturedSymbols::Captured(_) => true,
        }
    }
}

impl<'a> Default for CapturedSymbols<'a> {
    fn default() -> Self {
        CapturedSymbols::None
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Proc<'a> {
    pub name: LambdaName<'a>,
    pub args: &'a [(Layout<'a>, Symbol)],
    pub body: Stmt<'a>,
    pub closure_data_layout: Option<Layout<'a>>,
    pub ret_layout: Layout<'a>,
    pub is_self_recursive: SelfRecursive,
    pub must_own_arguments: bool,
    pub host_exposed_layouts: HostExposedLayouts<'a>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HostExposedLayouts<'a> {
    NotHostExposed,
    HostExposed {
        rigids: BumpMap<Lowercase, Layout<'a>>,
        aliases: BumpMap<Symbol, (Symbol, ProcLayout<'a>, RawFunctionLayout<'a>)>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SelfRecursive {
    NotSelfRecursive,
    SelfRecursive(JoinPointId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Parens {
    NotNeeded,
    InTypeParam,
    InFunction,
}

impl<'a> Proc<'a> {
    pub fn to_doc<'b, D, A, I>(
        &'b self,
        alloc: &'b D,
        interner: &'b I,
        _parens: Parens,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: Interner<'a, Layout<'a>>,
    {
        let args_doc = self.args.iter().map(|(layout, symbol)| {
            let arg_doc = symbol_to_doc(alloc, *symbol);
            if pretty_print_ir_symbols() {
                arg_doc.append(alloc.reflow(": ")).append(layout.to_doc(
                    alloc,
                    interner,
                    Parens::NotNeeded,
                ))
            } else {
                arg_doc
            }
        });

        if pretty_print_ir_symbols() {
            alloc
                .text("procedure : ")
                .append(symbol_to_doc(alloc, self.name.name()))
                .append(" ")
                .append(self.ret_layout.to_doc(alloc, interner, Parens::NotNeeded))
                .append(alloc.hardline())
                .append(alloc.text("procedure = "))
                .append(symbol_to_doc(alloc, self.name.name()))
                .append(" (")
                .append(alloc.intersperse(args_doc, ", "))
                .append("):")
                .append(alloc.hardline())
                .append(self.body.to_doc(alloc, interner).indent(4))
        } else {
            alloc
                .text("procedure ")
                .append(symbol_to_doc(alloc, self.name.name()))
                .append(" (")
                .append(alloc.intersperse(args_doc, ", "))
                .append("):")
                .append(alloc.hardline())
                .append(self.body.to_doc(alloc, interner).indent(4))
        }
    }

    pub fn to_pretty<I>(&self, interner: &I, width: usize) -> String
    where
        I: Interner<'a, Layout<'a>>,
    {
        let allocator = BoxAllocator;
        let mut w = std::vec::Vec::new();
        self.to_doc::<_, (), _>(&allocator, interner, Parens::NotNeeded)
            .1
            .render(width, &mut w)
            .unwrap();
        w.push(b'\n');
        String::from_utf8(w).unwrap()
    }

    pub fn insert_refcount_operations<'i>(
        arena: &'a Bump,
        layout_interner: &'i STLayoutInterner<'a>,
        home: ModuleId,
        ident_ids: &'i mut IdentIds,
        update_mode_ids: &'i mut UpdateModeIds,
        procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    ) {
        let borrow_params = arena.alloc(crate::borrow::infer_borrow(arena, procs));

        crate::inc_dec::visit_procs(
            arena,
            layout_interner,
            home,
            ident_ids,
            update_mode_ids,
            borrow_params,
            procs,
        );
    }

    pub fn insert_reset_reuse_operations<'i>(
        arena: &'a Bump,
        home: ModuleId,
        ident_ids: &'i mut IdentIds,
        update_mode_ids: &'i mut UpdateModeIds,
        procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    ) {
        for (_, proc) in procs.iter_mut() {
            let new_proc = crate::reset_reuse::insert_reset_reuse(
                arena,
                home,
                ident_ids,
                update_mode_ids,
                proc.clone(),
            );
            *proc = new_proc;
        }
    }

    fn make_tail_recursive(&mut self, env: &mut Env<'a, '_>) {
        let mut args = Vec::with_capacity_in(self.args.len(), env.arena);
        let mut proc_args = Vec::with_capacity_in(self.args.len(), env.arena);

        for (layout, symbol) in self.args {
            let new = env.unique_symbol();
            args.push((*layout, *symbol, new));
            proc_args.push((*layout, new));
        }

        use self::SelfRecursive::*;
        if let SelfRecursive(id) = self.is_self_recursive {
            let transformed = crate::tail_recursion::make_tail_recursive(
                env.arena,
                id,
                self.name,
                self.body.clone(),
                args.into_bump_slice(),
                self.ret_layout,
            );

            if let Some(with_tco) = transformed {
                self.body = with_tco;
                self.args = proc_args.into_bump_slice();
            }
        }
    }
}

/// A host-exposed function must be specialized; it's a seed for subsequent specializations
#[derive(Clone, Debug)]
pub struct HostSpecializations<'a> {
    /// Not a bumpalo vec because bumpalo is not thread safe
    /// Separate array so we can search for membership quickly
    /// If it's a value and not a lambda, the value is recorded as LambdaName::no_niche.
    symbol_or_lambdas: std::vec::Vec<LambdaName<'a>>,
    storage_subs: StorageSubs,
    /// For each symbol, what types to specialize it for, points into the storage_subs
    types_to_specialize: std::vec::Vec<Variable>,
    /// Variables for an exposed alias
    exposed_aliases: std::vec::Vec<std::vec::Vec<(Symbol, Variable)>>,
}

impl Default for HostSpecializations<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> HostSpecializations<'a> {
    pub fn new() -> Self {
        Self {
            symbol_or_lambdas: std::vec::Vec::new(),
            storage_subs: StorageSubs::new(Subs::default()),
            types_to_specialize: std::vec::Vec::new(),
            exposed_aliases: std::vec::Vec::new(),
        }
    }

    pub fn insert_host_exposed(
        &mut self,
        env_subs: &mut Subs,
        symbol_or_lambda: LambdaName<'a>,
        opt_annotation: Option<roc_can::def::Annotation>,
        variable: Variable,
    ) {
        let variable = self.storage_subs.extend_with_variable(env_subs, variable);

        let mut host_exposed_aliases = std::vec::Vec::new();

        if let Some(annotation) = opt_annotation {
            host_exposed_aliases.extend(annotation.introduced_variables.host_exposed_aliases);
        }

        match self
            .symbol_or_lambdas
            .iter()
            .position(|s| *s == symbol_or_lambda)
        {
            None => {
                self.symbol_or_lambdas.push(symbol_or_lambda);
                self.types_to_specialize.push(variable);
                self.exposed_aliases.push(host_exposed_aliases);
            }
            Some(_) => {
                // we assume that only one specialization of a function is directly exposed to the
                // host. Other host-exposed symbols may (transitively) specialize this symbol,
                // but then the existing specialization mechanism will find those specializations
                panic!("A host-exposed symbol can only be exposed once");
            }
        }

        debug_assert_eq!(self.types_to_specialize.len(), self.exposed_aliases.len());
    }

    fn decompose(
        self,
    ) -> (
        StorageSubs,
        impl Iterator<Item = (LambdaName<'a>, Variable, std::vec::Vec<(Symbol, Variable)>)>,
    ) {
        let it1 = self.symbol_or_lambdas.into_iter();

        let it2 = self.types_to_specialize.into_iter();
        let it3 = self.exposed_aliases.into_iter();

        (
            self.storage_subs,
            it1.zip(it2).zip(it3).map(|((a, b), c)| (a, b, c)),
        )
    }
}

/// Specializations of this module's symbols that other modules need.
/// One struct represents one pair of modules, e.g. what module A wants of module B.
#[derive(Clone, Debug)]
pub struct ExternalSpecializations<'a> {
    /// Not a bumpalo vec because bumpalo is not thread safe
    /// Separate array so we can search for membership quickly
    /// If it's a value and not a lambda, the value is recorded as LambdaName::no_niche.
    pub symbol_or_lambda: std::vec::Vec<LambdaName<'a>>,
    storage: ExternalModuleStorage,
    /// For each symbol, what types to specialize it for, points into the storage_subs
    types_to_specialize: std::vec::Vec<std::vec::Vec<Variable>>,
}

impl Default for ExternalSpecializations<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> ExternalSpecializations<'a> {
    pub fn new() -> Self {
        Self {
            symbol_or_lambda: std::vec::Vec::new(),
            storage: ExternalModuleStorage::new(Subs::default()),
            types_to_specialize: std::vec::Vec::new(),
        }
    }

    fn insert_external(
        &mut self,
        symbol_or_lambda: LambdaName<'a>,
        env_subs: &mut Subs,
        variable: Variable,
    ) {
        let stored_variable = self.storage.extend_with_variable(env_subs, variable);
        roc_tracing::debug!(original = ?variable, stored = ?stored_variable, "stored needed external");

        match self
            .symbol_or_lambda
            .iter()
            .position(|s| *s == symbol_or_lambda)
        {
            None => {
                self.symbol_or_lambda.push(symbol_or_lambda);
                self.types_to_specialize.push(vec![stored_variable]);
            }
            Some(index) => {
                let types_to_specialize = &mut self.types_to_specialize[index];
                types_to_specialize.push(stored_variable);
            }
        }
    }

    fn decompose(
        self,
    ) -> (
        StorageSubs,
        impl Iterator<Item = (LambdaName<'a>, std::vec::Vec<Variable>)>,
    ) {
        (
            self.storage.into_storage_subs(),
            self.symbol_or_lambda
                .into_iter()
                .zip(self.types_to_specialize.into_iter()),
        )
    }

    fn snapshot_cache(&mut self) -> ExternalModuleStorageSnapshot {
        self.storage.snapshot_cache()
    }

    fn rollback_cache(&mut self, snapshot: ExternalModuleStorageSnapshot) {
        self.storage.rollback_cache(snapshot)
    }

    fn invalidate_cache(&mut self, changed_variables: &[Variable]) {
        self.storage.invalidate_cache(changed_variables)
    }

    fn invalidate_whole_cache(&mut self) {
        self.storage.invalidate_whole_cache()
    }
}

#[derive(Clone, Debug)]
pub struct Suspended<'a> {
    pub store: StorageSubs,
    /// LambdaName::no_niche if it's a value
    pub symbol_or_lambdas: Vec<'a, LambdaName<'a>>,
    pub layouts: Vec<'a, ProcLayout<'a>>,
    pub variables: Vec<'a, Variable>,
}

impl<'a> Suspended<'a> {
    fn new_in(arena: &'a Bump) -> Self {
        Self {
            store: StorageSubs::new(Subs::new_from_varstore(Default::default())),
            symbol_or_lambdas: Vec::new_in(arena),
            layouts: Vec::new_in(arena),
            variables: Vec::new_in(arena),
        }
    }

    fn is_empty(&self) -> bool {
        self.symbol_or_lambdas.is_empty()
    }

    fn specialization(
        &mut self,
        subs: &mut Subs,
        symbol_or_lambda: LambdaName<'a>,
        proc_layout: ProcLayout<'a>,
        variable: Variable,
    ) {
        // de-duplicate
        for (i, s) in self.symbol_or_lambdas.iter().enumerate() {
            if *s == symbol_or_lambda {
                let existing = &self.layouts[i];
                if &proc_layout == existing {
                    // symbol + layout combo exists
                    return;
                }
            }
        }

        self.symbol_or_lambdas.push(symbol_or_lambda);
        self.layouts.push(proc_layout);

        let variable = self.store.import_variable_from(subs, variable).variable;

        self.variables.push(variable);
    }
}

#[derive(Clone, Debug)]
enum PendingSpecializations<'a> {
    /// We are finding specializations we need. This is a separate step so
    /// that we can give specializations we need to modules higher up in the dependency chain, so
    /// that they can start making specializations too
    Finding(Suspended<'a>),
    /// We are making specializations.
    /// If any new one comes up while specializing a body, we can do one of two things:
    ///   - if the new specialization is for a symbol that is not in the current stack of symbols
    ///     being specialized, make it immediately
    ///   - if it is, we must suspend the specialization, and we'll do it once the stack is clear
    ///     again.
    Making(Suspended<'a>),
}

impl<'a> PendingSpecializations<'a> {
    fn is_empty(&self) -> bool {
        match self {
            PendingSpecializations::Finding(suspended)
            | PendingSpecializations::Making(suspended) => suspended.is_empty(),
        }
    }
}

#[derive(Clone, Debug, Default)]
struct Specialized<'a> {
    symbols: std::vec::Vec<Symbol>,
    proc_layouts: std::vec::Vec<ProcLayout<'a>>,
    procedures: std::vec::Vec<InProgressProc<'a>>,
}

impl<'a> Specialized<'a> {
    fn len(&self) -> usize {
        self.symbols.len()
    }

    #[allow(dead_code)]
    fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }

    fn into_iter_assert_done(self) -> impl Iterator<Item = (Symbol, ProcLayout<'a>, Proc<'a>)> {
        self.symbols
            .into_iter()
            .zip(self.proc_layouts.into_iter())
            .zip(self.procedures.into_iter())
            .filter_map(|((s, l), in_progress)| {
                if let Symbol::REMOVED_SPECIALIZATION = s {
                    None
                } else {
                    match in_progress {
                        InProgressProc::InProgress => {
                            panic!("Function {:?} ({:?}) is not done specializing", s, l)
                        }
                        InProgressProc::Done(proc) => Some((s, l, proc)),
                    }
                }
            })
    }

    fn is_specialized(&self, symbol: Symbol, layout: &ProcLayout<'a>) -> bool {
        for (i, s) in self.symbols.iter().enumerate() {
            if *s == symbol && &self.proc_layouts[i] == layout {
                return true;
            }
        }

        false
    }

    fn mark_in_progress(&mut self, symbol: Symbol, layout: ProcLayout<'a>) {
        for (i, s) in self.symbols.iter().enumerate() {
            if *s == symbol && self.proc_layouts[i] == layout {
                match &self.procedures[i] {
                    InProgressProc::InProgress => {
                        return;
                    }
                    InProgressProc::Done(_) => {
                        panic!("marking in progress, but this proc is already done!")
                    }
                }
            }
        }

        // the key/layout combo was not found; insert it
        self.symbols.push(symbol);
        self.proc_layouts.push(layout);
        self.procedures.push(InProgressProc::InProgress);
    }

    fn remove_specialized(&mut self, symbol: Symbol, layout: &ProcLayout<'a>) -> bool {
        let mut index = None;

        for (i, s) in self.symbols.iter().enumerate() {
            if *s == symbol && &self.proc_layouts[i] == layout {
                index = Some(i);
            }
        }

        if let Some(index) = index {
            self.symbols[index] = Symbol::REMOVED_SPECIALIZATION;

            true
        } else {
            false
        }
    }

    fn insert_specialized(&mut self, symbol: Symbol, layout: ProcLayout<'a>, proc: Proc<'a>) {
        for (i, s) in self.symbols.iter().enumerate() {
            if *s == symbol && self.proc_layouts[i] == layout {
                match &self.procedures[i] {
                    InProgressProc::InProgress => {
                        self.procedures[i] = InProgressProc::Done(proc);
                        return;
                    }
                    InProgressProc::Done(_) => {
                        // overwrite existing! this is important in practice
                        // TODO investigate why we generate the wrong proc in some cases and then
                        // correct later
                        self.procedures[i] = InProgressProc::Done(proc);
                        return;
                    }
                }
            }
        }

        // the key/layout combo was not found; insert it
        self.symbols.push(symbol);
        self.proc_layouts.push(layout);
        self.procedures.push(InProgressProc::Done(proc));
    }
}

/// Uniquely determines the specialization of a polymorphic (non-proc) value symbol.
/// Two specializations are equivalent if their [`SpecializationMark`]s are equal.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
struct SpecializationMark<'a> {
    /// The layout of the symbol itself.
    layout: Layout<'a>,

    /// If this symbol is a closure def, we must also keep track of what function it specializes,
    /// because the [`layout`] field will only keep track of its closure and lambda set - which can
    /// be the same for two different function specializations. For example,
    ///
    ///   id = if True then \x -> x else \y -> y
    ///   { a: id "", b: id 1u8 }
    ///
    /// The lambda set and captures of `id` is the same in both usages inside the record, but the
    /// reified specializations of `\x -> x` and `\y -> y` must be for Str and U8.
    ///
    /// Note that this field is not relevant for anything that is not a function.
    function_mark: Option<RawFunctionLayout<'a>>,
}

/// The deepest closure in the current stack of procedures under specialization a symbol specialization
/// was used in.
///
/// This is necessary to understand what symbol specializations are used in what capture sets. For
/// example, consider
///
/// main =
///   x = 1
///
///   y = \{} -> 1u8 + x
///   z = \{} -> 1u16 + x
///
/// Here, we have a two specializations of `x` to U8 and U16 with deepest uses of
/// (2, y) and (2, z), respectively. This tells us that both of those specializations must be
/// preserved by `main` (which is at depth 1), but that `y` and `z` respectively only need to
/// capture one particular specialization of `x` each.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UseDepth {
    depth: usize,
    symbol: Symbol,
}

impl UseDepth {
    fn is_nested_use_in(&self, outer: &Self) -> bool {
        if self.symbol == outer.symbol {
            debug_assert!(self.depth == outer.depth);
            return true;
        }
        self.depth > outer.depth
    }
}

/// When walking a function body, we may encounter specialized usages of polymorphic symbols. For
/// example
///
///  myTag = A
///  use1 : [A, B]
///  use1 = myTag
///  use2 : [A, B, C]
///  use2 = myTag
///
/// We keep track of the specializations of `myTag` and create fresh symbols when there is more
/// than one, so that a unique def can be created for each.
#[derive(Default, Debug, Clone)]
struct SymbolSpecializations<'a>(
    // THEORY:
    //  1. the number of symbols in a def is very small
    //  2. the number of specializations of a symbol in a def is even smaller (almost always only one)
    // So, a linear VecMap is preferrable. Use a two-layered one to make (1) extraction of defs easy
    // and (2) reads of a certain symbol be determined by its first occurrence, not its last.
    VecMap<Symbol, VecMap<SpecializationMark<'a>, (Variable, Symbol, UseDepth)>>,
);

impl<'a> SymbolSpecializations<'a> {
    /// Inserts a known specialization for a symbol. Returns the overwritten specialization, if any.
    pub fn get_or_insert_known(
        &mut self,
        symbol: Symbol,
        mark: SpecializationMark<'a>,
        specialization_var: Variable,
        specialization_symbol: Symbol,
        deepest_use: UseDepth,
    ) -> Option<(Variable, Symbol, UseDepth)> {
        self.0.get_or_insert(symbol, Default::default).insert(
            mark,
            (specialization_var, specialization_symbol, deepest_use),
        )
    }

    /// Removes all specializations for a symbol, returning the type and symbol of each specialization.
    pub fn remove(
        &mut self,
        symbol: Symbol,
    ) -> impl ExactSizeIterator<Item = (SpecializationMark<'a>, (Variable, Symbol, UseDepth))> {
        self.0
            .remove(&symbol)
            .map(|(_, specializations)| specializations)
            .unwrap_or_default()
            .into_iter()
    }

    /// Expects and removes at most a single specialization symbol for the given requested symbol.
    /// A symbol may have no specializations if it is never referenced in a body, so it is possible
    /// for this to return None.
    pub fn remove_single(&mut self, symbol: Symbol) -> Option<Symbol> {
        let mut specializations = self.remove(symbol);

        debug_assert!(
            specializations.len() < 2,
            "Symbol {:?} has multiple specializations",
            symbol
        );

        specializations.next().map(|(_, (_, symbol, _))| symbol)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

#[derive(Clone, Debug, Default)]
pub struct ProcsBase<'a> {
    pub partial_procs: BumpMap<Symbol, PartialProc<'a>>,
    pub module_thunks: &'a [Symbol],
    /// A host-exposed function must be specialized; it's a seed for subsequent specializations
    pub host_specializations: HostSpecializations<'a>,
    pub runtime_errors: BumpMap<Symbol, &'a str>,
    pub imported_module_thunks: &'a [Symbol],
}

/// The current set of functions under specialization. They form a stack where the latest
/// specialization to be seen is at the head of the stack.
#[derive(Clone, Debug)]
struct SpecializationStack<'a>(Vec<'a, Symbol>);

impl<'a> SpecializationStack<'a> {
    fn current_use_depth(&self) -> UseDepth {
        UseDepth {
            depth: self.0.len(),
            symbol: *self.0.last().unwrap(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Procs<'a> {
    pub partial_procs: PartialProcs<'a>,
    ability_member_aliases: AbilityAliases,
    pub imported_module_thunks: &'a [Symbol],
    pub module_thunks: &'a [Symbol],
    pending_specializations: PendingSpecializations<'a>,
    specialized: Specialized<'a>,
    pub runtime_errors: BumpMap<Symbol, &'a str>,
    pub externals_we_need: BumpMap<ModuleId, ExternalSpecializations<'a>>,
    symbol_specializations: SymbolSpecializations<'a>,
    specialization_stack: SpecializationStack<'a>,
}

impl<'a> Procs<'a> {
    pub fn new_in(arena: &'a Bump) -> Self {
        Self {
            partial_procs: PartialProcs::new_in(arena),
            ability_member_aliases: AbilityAliases::new_in(arena),
            imported_module_thunks: &[],
            module_thunks: &[],
            pending_specializations: PendingSpecializations::Finding(Suspended::new_in(arena)),
            specialized: Specialized::default(),
            runtime_errors: BumpMap::new_in(arena),
            externals_we_need: BumpMap::new_in(arena),
            symbol_specializations: Default::default(),
            specialization_stack: SpecializationStack(Vec::with_capacity_in(16, arena)),
        }
    }

    fn push_active_specialization(&mut self, specialization: Symbol) {
        self.specialization_stack.0.push(specialization);
    }

    fn pop_active_specialization(&mut self, specialization: Symbol) {
        let popped = self
            .specialization_stack
            .0
            .pop()
            .expect("specialization stack is empty");
        debug_assert_eq!(
            popped, specialization,
            "incorrect popped specialization: passed {:?}, but was {:?}",
            specialization, popped
        );
    }

    /// If we need to specialize a function that is already in the stack, we must wait to do so
    /// until that function is popped off. That's because the type environment will be configured
    /// for the existing specialization on the stack.
    ///
    /// For example, in
    ///
    ///   foo = \val, b -> if b then "done" else bar val
    ///   bar = \_ -> foo {} True
    ///   foo "" False
    ///
    /// During the specialization of `foo : Str False -> Str`, we specialize `bar : Str -> Str`,
    /// which in turn needs a specialization of `foo : {} False -> Str`. However, we can't
    /// specialize both `foo : Str False -> Str` and `foo : {} False -> Str` at the same time, so
    /// the latter specialization must be deferred.
    fn symbol_needs_suspended_specialization(&self, specialization: Symbol) -> bool {
        self.specialization_stack.0.contains(&specialization)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum InProgressProc<'a> {
    InProgress,
    Done(Proc<'a>),
}

impl<'a> Procs<'a> {
    fn is_imported_module_thunk(&self, symbol: Symbol) -> bool {
        self.imported_module_thunks.iter().any(|x| *x == symbol)
    }

    fn is_module_thunk(&self, symbol: Symbol) -> bool {
        self.module_thunks.iter().any(|x| *x == symbol)
    }

    fn get_partial_proc<'b>(&'b self, symbol: Symbol) -> Option<&'b PartialProc<'a>> {
        self.partial_procs.get_symbol(symbol)
    }

    pub fn get_specialized_procs_without_rc(
        self,
        env: &mut Env<'a, '_>,
    ) -> (MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>, ProcsBase<'a>) {
        let mut specialized_procs =
            MutMap::with_capacity_and_hasher(self.specialized.len(), default_hasher());

        for (symbol, layout, mut proc) in self.specialized.into_iter_assert_done() {
            proc.make_tail_recursive(env);

            let key = (symbol, layout);
            specialized_procs.insert(key, proc);
        }

        let restored_procs_base = ProcsBase {
            partial_procs: self.partial_procs.drain().collect(),
            module_thunks: self.module_thunks,
            // This must now be empty
            host_specializations: HostSpecializations::default(),
            runtime_errors: self.runtime_errors,
            imported_module_thunks: self.imported_module_thunks,
        };

        (specialized_procs, restored_procs_base)
    }

    // TODO trim these down
    #[allow(clippy::too_many_arguments)]
    fn insert_anonymous(
        &mut self,
        env: &mut Env<'a, '_>,
        name: LambdaName<'a>,
        annotation: Variable,
        loc_args: std::vec::Vec<(Variable, AnnotatedMark, Loc<roc_can::pattern::Pattern>)>,
        loc_body: Loc<roc_can::expr::Expr>,
        captured_symbols: CapturedSymbols<'a>,
        ret_var: Variable,
        layout_cache: &mut LayoutCache<'a>,
    ) -> Result<ProcLayout<'a>, RuntimeError> {
        let raw_layout = layout_cache
            .raw_from_var(env.arena, annotation, env.subs)
            .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

        let top_level = ProcLayout::from_raw(
            env.arena,
            &layout_cache.interner,
            raw_layout,
            name.captures_niche(),
        );

        // anonymous functions cannot reference themselves, therefore cannot be tail-recursive
        // EXCEPT when the closure conversion makes it tail-recursive.
        let is_self_recursive = match top_level.arguments.last() {
            Some(Layout::LambdaSet(lambda_set)) => lambda_set.contains(name.name()),
            _ => false,
        };

        match patterns_to_when(env, loc_args, ret_var, loc_body) {
            Ok((_, pattern_symbols, body)) => {
                // an anonymous closure. These will always be specialized already
                // by the surrounding context, so we can add pending specializations
                // for them immediately.

                let already_specialized = self.specialized.is_specialized(name.name(), &top_level);

                let layout = top_level;

                // if we've already specialized this one, no further work is needed.
                if !already_specialized {
                    if self.is_module_thunk(name.name()) {
                        debug_assert!(layout.arguments.is_empty());
                    }

                    let needs_suspended_specialization =
                        self.symbol_needs_suspended_specialization(name.name());
                    match (
                        &mut self.pending_specializations,
                        needs_suspended_specialization,
                    ) {
                        (PendingSpecializations::Finding(suspended), _)
                        | (PendingSpecializations::Making(suspended), true) => {
                            // register the pending specialization, so this gets code genned later
                            suspended.specialization(env.subs, name, layout, annotation);

                            match self.partial_procs.symbol_to_id(name.name()) {
                                Some(occupied) => {
                                    let existing = self.partial_procs.get_id(occupied);
                                    // if we're adding the same partial proc twice, they must be the actual same!
                                    //
                                    // NOTE we can't skip extra work! we still need to make the specialization for this
                                    // invocation. The content of the `annotation` can be different, even if the variable
                                    // number is the same
                                    debug_assert_eq!(annotation, existing.annotation);
                                    debug_assert_eq!(captured_symbols, existing.captured_symbols);
                                    debug_assert_eq!(is_self_recursive, existing.is_self_recursive);

                                    // the partial proc is already in there, do nothing
                                }
                                None => {
                                    let pattern_symbols = pattern_symbols.into_bump_slice();

                                    let partial_proc = PartialProc {
                                        annotation,
                                        pattern_symbols,
                                        captured_symbols,
                                        body: body.value,
                                        body_var: ret_var,
                                        is_self_recursive,
                                    };

                                    self.partial_procs.insert(name.name(), partial_proc);
                                }
                            }
                        }
                        (PendingSpecializations::Making(_), false) => {
                            // Mark this proc as in-progress, so if we're dealing with
                            // mutually recursive functions, we don't loop forever.
                            // (We had a bug around this before this system existed!)
                            self.specialized.mark_in_progress(name.name(), layout);

                            let partial_proc_id = if let Some(partial_proc_id) =
                                self.partial_procs.symbol_to_id(name.name())
                            {
                                let existing = self.partial_procs.get_id(partial_proc_id);
                                // if we're adding the same partial proc twice, they must be the actual same!
                                //
                                // NOTE we can't skip extra work! we still need to make the specialization for this
                                // invocation. The content of the `annotation` can be different, even if the variable
                                // number is the same
                                debug_assert_eq!(annotation, existing.annotation);
                                debug_assert_eq!(captured_symbols, existing.captured_symbols);
                                debug_assert_eq!(is_self_recursive, existing.is_self_recursive);

                                partial_proc_id
                            } else {
                                let pattern_symbols = pattern_symbols.into_bump_slice();

                                let partial_proc = PartialProc {
                                    annotation,
                                    pattern_symbols,
                                    captured_symbols,
                                    body: body.value,
                                    body_var: ret_var,
                                    is_self_recursive,
                                };

                                self.partial_procs.insert(name.name(), partial_proc)
                            };

                            match specialize_variable(
                                env,
                                self,
                                name,
                                layout_cache,
                                annotation,
                                &[],
                                partial_proc_id,
                            ) {
                                Ok((proc, _ignore_layout)) => {
                                    // the `layout` is a function pointer, while `_ignore_layout` can be a
                                    // closure. We only specialize functions, storing this value with a closure
                                    // layout will give trouble.
                                    let arguments = Vec::from_iter_in(
                                        proc.args.iter().map(|(l, _)| *l),
                                        env.arena,
                                    )
                                    .into_bump_slice();

                                    let proper_layout = ProcLayout {
                                        arguments,
                                        result: proc.ret_layout,
                                        captures_niche: proc.name.captures_niche(),
                                    };

                                    // NOTE: some functions are specialized to have a closure, but don't actually
                                    // need any closure argument. Here is where we correct this sort of thing,
                                    // by trusting the layout of the Proc, not of what we specialize for
                                    self.specialized.remove_specialized(name.name(), &layout);
                                    self.specialized.insert_specialized(
                                        name.name(),
                                        proper_layout,
                                        proc,
                                    );
                                }
                                Err(error) => {
                                    panic!("TODO generate a RuntimeError message for {:?}", error);
                                }
                            }
                        }
                    }
                }

                Ok(layout)
            }
            Err(loc_error) => Err(loc_error.value),
        }
    }

    fn insert_passed_by_name(
        &mut self,
        env: &mut Env<'a, '_>,
        fn_var: Variable,
        name: LambdaName<'a>,
        layout: ProcLayout<'a>,
        layout_cache: &mut LayoutCache<'a>,
    ) {
        // If we've already specialized this one, no further work is needed.
        if self.specialized.is_specialized(name.name(), &layout) {
            return;
        }

        // If this is an imported symbol, let its home module make this specialization
        if env.is_imported_symbol(name.name()) || env.is_unloaded_derived_symbol(name.name(), self)
        {
            add_needed_external(self, env, fn_var, name);
            return;
        }

        // register the pending specialization, so this gets code genned later
        if self.module_thunks.contains(&name.name()) {
            debug_assert!(layout.arguments.is_empty());
        }

        // This should only be called when pending_specializations is Some.
        // Otherwise, it's being called in the wrong pass!
        let needs_suspended_specialization =
            self.symbol_needs_suspended_specialization(name.name());
        match (
            &mut self.pending_specializations,
            needs_suspended_specialization,
        ) {
            (PendingSpecializations::Finding(suspended), _)
            | (PendingSpecializations::Making(suspended), true) => {
                suspended.specialization(env.subs, name, layout, fn_var);
            }
            (PendingSpecializations::Making(_), false) => {
                let symbol = name;

                let partial_proc_id = match self.partial_procs.symbol_to_id(symbol.name()) {
                    Some(p) => p,
                    None => panic!("no partial_proc for {:?} in module {:?}", symbol, env.home),
                };

                // Mark this proc as in-progress, so if we're dealing with
                // mutually recursive functions, we don't loop forever.
                // (We had a bug around this before this system existed!)
                self.specialized.mark_in_progress(symbol.name(), layout);

                // See https://github.com/roc-lang/roc/issues/1600
                //
                // The annotation variable is the generic/lifted/top-level annotation.
                // It is connected to the variables of the function's body
                //
                // fn_var is the variable representing the type that we actually need for the
                // function right here.
                //
                // For some reason, it matters that we unify with the original variable. Extracting
                // that variable into a SolvedType and then introducing it again severs some
                // connection that turns out to be important
                match specialize_variable(
                    env,
                    self,
                    symbol,
                    layout_cache,
                    fn_var,
                    Default::default(),
                    partial_proc_id,
                ) {
                    Ok((proc, _ignore_layout)) => {
                        // the `layout` is a function pointer, while `_ignore_layout` can be a
                        // closure. We only specialize functions, storing this value with a closure
                        // layout will give trouble.
                        let arguments =
                            Vec::from_iter_in(proc.args.iter().map(|(l, _)| *l), env.arena)
                                .into_bump_slice();

                        let proper_layout = ProcLayout {
                            arguments,
                            result: proc.ret_layout,
                            captures_niche: proc.name.captures_niche(),
                        };

                        // NOTE: some functions are specialized to have a closure, but don't actually
                        // need any closure argument. Here is where we correct this sort of thing,
                        // by trusting the layout of the Proc, not of what we specialize for
                        self.specialized.remove_specialized(symbol.name(), &layout);
                        self.specialized
                            .insert_specialized(symbol.name(), proper_layout, proc);
                    }
                    Err(error) => {
                        panic!("TODO generate a RuntimeError message for {:?}", error);
                    }
                }
            }
        }
    }

    /// Gets a specialization for a symbol, or creates a new one.
    #[inline(always)]
    fn get_or_insert_symbol_specialization(
        &mut self,
        env: &mut Env<'a, '_>,
        layout_cache: &mut LayoutCache<'a>,
        symbol: Symbol,
        specialization_var: Variable,
    ) -> Symbol {
        let arena = env.arena;
        let subs: &Subs = env.subs;

        let layout = match layout_cache.from_var(arena, specialization_var, subs) {
            Ok(layout) => layout,
            // This can happen when the def symbol has a type error. In such cases just use the
            // def symbol, which is erroring.
            Err(_) => return symbol,
        };

        let is_closure = matches!(
            subs.get_content_without_compacting(specialization_var),
            Content::Structure(FlatType::Func(..))
        );
        let function_mark = if is_closure {
            let fn_layout = match layout_cache.raw_from_var(arena, specialization_var, subs) {
                Ok(layout) => layout,
                // This can happen when the def symbol has a type error. In such cases just use the
                // def symbol, which is erroring.
                Err(_) => return symbol,
            };
            Some(fn_layout)
        } else {
            None
        };

        let specialization_mark = SpecializationMark {
            layout,
            function_mark,
        };

        let symbol_specializations = self
            .symbol_specializations
            .0
            .get_or_insert(symbol, Default::default);

        // For the first specialization, always reuse the current symbol. The vast majority of defs
        // only have one instance type, so this preserves readability of the IR.
        // TODO: turn me off and see what breaks.
        let needs_fresh_symbol = !symbol_specializations.is_empty();

        let mut make_specialized_symbol = || {
            if needs_fresh_symbol {
                env.unique_symbol()
            } else {
                symbol
            }
        };

        let current_use = self.specialization_stack.current_use_depth();
        let (_var, specialized_symbol, deepest_use) = symbol_specializations
            .get_or_insert(specialization_mark, || {
                (specialization_var, make_specialized_symbol(), current_use)
            });

        if deepest_use.is_nested_use_in(&current_use) {
            *deepest_use = current_use;
        }

        *specialized_symbol
    }

    /// Get the symbol specializations used in the active specialization's body.
    pub fn get_symbol_specializations_used_in_body(
        &self,
        symbol: Symbol,
    ) -> Option<impl Iterator<Item = (Variable, Symbol)> + '_> {
        let this_use = self.specialization_stack.current_use_depth();
        self.symbol_specializations.0.get(&symbol).map(move |l| {
            l.iter().filter_map(move |(_, (var, sym, deepest_use))| {
                if deepest_use.is_nested_use_in(&this_use) {
                    Some((*var, *sym))
                } else {
                    None
                }
            })
        })
    }
}

#[derive(Default)]
pub struct Specializations<'a> {
    by_symbol: MutMap<Symbol, MutMap<Layout<'a>, Proc<'a>>>,
}

impl<'a> Specializations<'a> {
    pub fn insert(&mut self, symbol: Symbol, layout: Layout<'a>, proc: Proc<'a>) {
        let procs_by_layout = self
            .by_symbol
            .entry(symbol)
            .or_insert_with(|| HashMap::with_capacity_and_hasher(1, default_hasher()));

        // If we already have an entry for this, it should be no different
        // from what we're about to insert.
        debug_assert!(
            !procs_by_layout.contains_key(&layout) || procs_by_layout.get(&layout) == Some(&proc)
        );

        procs_by_layout.insert(layout, proc);
    }

    pub fn len(&self) -> usize {
        self.by_symbol.len()
    }

    pub fn is_empty(&self) -> bool {
        self.by_symbol.is_empty()
    }
}

pub struct Env<'a, 'i> {
    pub arena: &'a Bump,
    pub subs: &'i mut Subs,
    pub home: ModuleId,
    pub ident_ids: &'i mut IdentIds,
    pub target_info: TargetInfo,
    pub update_mode_ids: &'i mut UpdateModeIds,
    pub call_specialization_counter: u32,
    // TODO: WorldAbilities and exposed_by_module share things, think about how to combine them
    pub abilities: AbilitiesView<'i>,
    pub exposed_by_module: &'i ExposedByModule,
    pub derived_module: &'i SharedDerivedModule,
}

impl<'a, 'i> Env<'a, 'i> {
    pub fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }

    pub fn next_update_mode_id(&mut self) -> UpdateModeId {
        self.update_mode_ids.next_id()
    }

    pub fn next_call_specialization_id(&mut self) -> CallSpecId {
        let id = CallSpecId {
            id: self.call_specialization_counter,
        };

        self.call_specialization_counter += 1;

        id
    }

    pub fn is_imported_symbol(&self, symbol: Symbol) -> bool {
        let sym_module = symbol.module_id();
        sym_module != self.home
            // The Derived_gen module takes responsibility for code-generating symbols in the
            // Derived_synth module.
            && !(self.home == ModuleId::DERIVED_GEN && sym_module == ModuleId::DERIVED_SYNTH)
    }

    /// While specializing the Derived_gen module, derived implementation symbols from the
    /// Derived_synth module may be discovered. These implementations may not have yet been loaded
    /// into the Derived_gen module, because we only load them before making specializations, and
    /// not during mono itself (yet).
    ///
    /// When this procedure returns `true`, the symbol should be marked as an external specialization,
    /// so that a subsequent specializations pass loads the derived implementation into Derived_gen
    /// and then code-generates appropriately.
    pub fn is_unloaded_derived_symbol(&self, symbol: Symbol, procs: &Procs<'a>) -> bool {
        self.home == ModuleId::DERIVED_GEN
            && symbol.module_id() == ModuleId::DERIVED_SYNTH
            && !procs.partial_procs.contains_key(symbol)
            // TODO: locking to find the answer in the `Derived_gen` module is not great, since
            // Derived_gen also blocks other modules specializing. Improve this later.
            && self
                .derived_module
                .lock()
                .expect("derived module is poisoned")
                .is_derived_def(symbol)
    }

    /// Unifies two variables and performs lambda set compaction.
    /// Use this rather than [roc_unify::unify] directly!
    fn unify<'b, 'c: 'b>(
        &mut self,
        external_specializations: impl IntoIterator<Item = &'b mut ExternalSpecializations<'c>>,
        layout_cache: &mut LayoutCache,
        left: Variable,
        right: Variable,
    ) -> Result<(), UnificationFailed> {
        debug_assert_ne!(
            self.home,
            ModuleId::DERIVED_SYNTH,
            "should never be monomorphizing the derived synth module!"
        );

        let changed_variables = roc_late_solve::unify(
            self.home,
            self.arena,
            self.subs,
            &self.abilities,
            self.derived_module,
            self.exposed_by_module,
            left,
            right,
        )?;

        layout_cache.invalidate(changed_variables.iter().copied());
        external_specializations
            .into_iter()
            .for_each(|e| e.invalidate_cache(&changed_variables));

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub struct JoinPointId(pub Symbol);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Param<'a> {
    pub symbol: Symbol,
    pub borrow: bool,
    pub layout: Layout<'a>,
}

impl<'a> Param<'a> {
    pub const EMPTY: Self = Param {
        symbol: Symbol::EMPTY_PARAM,
        borrow: false,
        layout: Layout::UNIT,
    };
}

pub fn cond<'a>(
    env: &mut Env<'a, '_>,
    cond_symbol: Symbol,
    cond_layout: Layout<'a>,
    pass: Stmt<'a>,
    fail: Stmt<'a>,
    ret_layout: Layout<'a>,
) -> Stmt<'a> {
    let branches = env.arena.alloc([(1u64, BranchInfo::None, pass)]);
    let default_branch = (BranchInfo::None, &*env.arena.alloc(fail));

    Stmt::Switch {
        cond_symbol,
        cond_layout,
        ret_layout,
        branches,
        default_branch,
    }
}

pub type Stores<'a> = &'a [(Symbol, Layout<'a>, Expr<'a>)];

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'a> {
    Let(Symbol, Expr<'a>, Layout<'a>, &'a Stmt<'a>),
    Switch {
        /// This *must* stand for an integer, because Switch potentially compiles to a jump table.
        cond_symbol: Symbol,
        cond_layout: Layout<'a>,
        /// The u64 in the tuple will be compared directly to the condition Expr.
        /// If they are equal, this branch will be taken.
        branches: &'a [(u64, BranchInfo<'a>, Stmt<'a>)],
        /// If no other branches pass, this default branch will be taken.
        default_branch: (BranchInfo<'a>, &'a Stmt<'a>),
        /// Each branch must return a value of this type.
        ret_layout: Layout<'a>,
    },
    Ret(Symbol),
    Refcounting(ModifyRc, &'a Stmt<'a>),
    Expect {
        condition: Symbol,
        region: Region,
        lookups: &'a [Symbol],
        layouts: &'a [Layout<'a>],
        /// what happens after the expect
        remainder: &'a Stmt<'a>,
    },
    ExpectFx {
        condition: Symbol,
        region: Region,
        lookups: &'a [Symbol],
        layouts: &'a [Layout<'a>],
        /// what happens after the expect
        remainder: &'a Stmt<'a>,
    },
    /// a join point `join f <params> = <continuation> in remainder`
    Join {
        id: JoinPointId,
        parameters: &'a [Param<'a>],
        /// body of the join point
        /// what happens after _jumping to_ the join point
        body: &'a Stmt<'a>,
        /// what happens after _defining_ the join point
        remainder: &'a Stmt<'a>,
    },
    Jump(JoinPointId, &'a [Symbol]),
    Crash(Symbol, CrashTag),
}

/// Source of crash, and its runtime representation to roc_panic.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum CrashTag {
    /// The crash is due to Roc, either via a builtin or type error.
    Roc = 0,
    /// The crash is user-defined.
    User = 1,
}

impl TryFrom<u32> for CrashTag {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Roc),
            1 => Ok(Self::User),
            _ => Err(()),
        }
    }
}

/// in the block below, symbol `scrutinee` is assumed be be of shape `tag_id`
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BranchInfo<'a> {
    None,
    Constructor {
        scrutinee: Symbol,
        layout: Layout<'a>,
        tag_id: TagIdIntType,
    },
}

impl<'a> BranchInfo<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use BranchInfo::*;

        match self {
            Constructor {
                tag_id,
                scrutinee,
                layout: _,
            } if pretty_print_ir_symbols() => alloc
                .hardline()
                .append("    BranchInfo: { scrutinee: ")
                .append(symbol_to_doc(alloc, *scrutinee))
                .append(", tag_id: ")
                .append(format!("{}", tag_id))
                .append("} "),

            _ => {
                if pretty_print_ir_symbols() {
                    alloc.text(" <no branch info>")
                } else {
                    alloc.text("")
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ModifyRc {
    /// Increment a reference count
    Inc(Symbol, u64),
    /// Decrement a reference count
    Dec(Symbol),
    /// A DecRef is a non-recursive reference count decrement
    /// e.g. If we Dec a list of lists, then if the reference count of the outer list is one,
    /// a Dec will recursively decrement all elements, then free the memory of the outer list.
    /// A DecRef would just free the outer list.
    /// That is dangerous because you may not free the elements, but in our Zig builtins,
    /// sometimes we know we already dealt with the elements (e.g. by copying them all over
    /// to a new list) and so we can just do a DecRef, which is much cheaper in such a case.
    DecRef(Symbol),
}

impl ModifyRc {
    pub fn to_doc<'a, D, A>(self, alloc: &'a D) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use ModifyRc::*;

        match self {
            Inc(symbol, 1) => alloc
                .text("inc ")
                .append(symbol_to_doc(alloc, symbol))
                .append(";"),
            Inc(symbol, n) => alloc
                .text("inc ")
                .append(alloc.text(format!("{} ", n)))
                .append(symbol_to_doc(alloc, symbol))
                .append(";"),
            Dec(symbol) => alloc
                .text("dec ")
                .append(symbol_to_doc(alloc, symbol))
                .append(";"),
            DecRef(symbol) => alloc
                .text("decref ")
                .append(symbol_to_doc(alloc, symbol))
                .append(";"),
        }
    }

    pub fn get_symbol(&self) -> Symbol {
        use ModifyRc::*;

        match self {
            Inc(symbol, _) => *symbol,
            Dec(symbol) => *symbol,
            DecRef(symbol) => *symbol,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Literal<'a> {
    // Literals
    /// stored as raw bytes rather than a number to avoid an alignment bump
    Int([u8; 16]),
    /// stored as raw bytes rather than a number to avoid an alignment bump
    U128([u8; 16]),
    Float(f64),
    /// stored as raw bytes rather than a number to avoid an alignment bump
    Decimal([u8; 16]),
    Str(&'a str),
    /// Closed tag unions containing exactly two (0-arity) tags compile to Expr::Bool,
    /// so they can (at least potentially) be emitted as 1-bit machine bools.
    ///
    /// So [True, False] compiles to this, and so do [A, B] and [Foo, Bar].
    /// However, a union like [True, False, Other Int] would not.
    Bool(bool),
    /// Closed tag unions containing between 3 and 256 tags (all of 0 arity)
    /// compile to bytes, e.g. [Blue, Black, Red, Green, White]
    Byte(u8),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ListLiteralElement<'a> {
    Literal(Literal<'a>),
    Symbol(Symbol),
}

impl<'a> ListLiteralElement<'a> {
    pub fn to_symbol(&self) -> Option<Symbol> {
        match self {
            Self::Symbol(s) => Some(*s),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Call<'a> {
    pub call_type: CallType<'a>,
    pub arguments: &'a [Symbol],
}

impl<'a> Call<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use CallType::*;

        let arguments = self.arguments;

        match self.call_type {
            CallType::ByName { name, .. } => {
                let it = std::iter::once(name.name())
                    .chain(arguments.iter().copied())
                    .map(|s| symbol_to_doc(alloc, s));

                alloc.text("CallByName ").append(alloc.intersperse(it, " "))
            }
            LowLevel { op: lowlevel, .. } => {
                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text(format!("lowlevel {:?} ", lowlevel))
                    .append(alloc.intersperse(it, " "))
            }
            HigherOrder(higher_order) => {
                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text(format!("lowlevel {:?} ", higher_order.op))
                    .append(alloc.intersperse(it, " "))
            }
            Foreign {
                ref foreign_symbol, ..
            } => {
                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text(format!("foreign {:?} ", foreign_symbol.as_str()))
                    .append(alloc.intersperse(it, " "))
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct CallSpecId {
    id: u32,
}

impl CallSpecId {
    pub fn to_bytes(self) -> [u8; 4] {
        self.id.to_ne_bytes()
    }

    /// Dummy value for generating refcount helper procs in the backends
    /// This happens *after* specialization so it's safe
    pub const BACKEND_DUMMY: Self = Self { id: 0 };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UpdateModeId {
    id: u32,
}

impl UpdateModeId {
    pub fn to_bytes(self) -> [u8; 4] {
        self.id.to_ne_bytes()
    }

    /// Dummy value for generating refcount helper procs in the backends
    /// This happens *after* alias analysis so it's safe
    pub const BACKEND_DUMMY: Self = Self { id: 0 };
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct UpdateModeIds {
    next: u32,
}

impl UpdateModeIds {
    pub const fn new() -> Self {
        Self { next: 0 }
    }

    pub fn next_id(&mut self) -> UpdateModeId {
        let id = UpdateModeId { id: self.next };
        self.next += 1;
        id
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CallType<'a> {
    ByName {
        name: LambdaName<'a>,
        ret_layout: &'a Layout<'a>,
        arg_layouts: &'a [Layout<'a>],
        specialization_id: CallSpecId,
    },
    Foreign {
        foreign_symbol: ForeignSymbol,
        ret_layout: &'a Layout<'a>,
    },
    LowLevel {
        op: LowLevel,
        update_mode: UpdateModeId,
    },
    HigherOrder(&'a HigherOrderLowLevel<'a>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct PassedFunction<'a> {
    /// name of the top-level function that is passed as an argument
    /// e.g. in `List.map xs Num.abs` this would be `Num.abs`
    pub name: LambdaName<'a>,

    pub argument_layouts: &'a [Layout<'a>],
    pub return_layout: Layout<'a>,

    pub specialization_id: CallSpecId,

    /// Symbol of the environment captured by the function argument
    pub captured_environment: Symbol,

    pub owns_captured_environment: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HigherOrderLowLevel<'a> {
    pub op: crate::low_level::HigherOrder,

    /// TODO I _think_  we can get rid of this, perhaps only keeping track of
    /// the layout of the closure argument, if any
    pub closure_env_layout: Option<Layout<'a>>,

    /// update mode of the higher order lowlevel itself
    pub update_mode: UpdateModeId,

    pub passed_function: PassedFunction<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    Literal(Literal<'a>),

    // Functions
    Call(Call<'a>),

    Tag {
        tag_layout: UnionLayout<'a>,
        tag_id: TagIdIntType,
        arguments: &'a [Symbol],
    },
    Struct(&'a [Symbol]),

    StructAtIndex {
        index: u64,
        field_layouts: &'a [Layout<'a>],
        structure: Symbol,
    },

    GetTagId {
        structure: Symbol,
        union_layout: UnionLayout<'a>,
    },

    UnionAtIndex {
        structure: Symbol,
        tag_id: TagIdIntType,
        union_layout: UnionLayout<'a>,
        index: u64,
    },

    Array {
        elem_layout: Layout<'a>,
        elems: &'a [ListLiteralElement<'a>],
    },
    EmptyArray,

    ExprBox {
        symbol: Symbol,
    },

    ExprUnbox {
        symbol: Symbol,
    },

    Reuse {
        symbol: Symbol,
        update_tag_id: bool,
        update_mode: UpdateModeId,
        // normal Tag fields
        tag_layout: UnionLayout<'a>,
        tag_id: TagIdIntType,
        arguments: &'a [Symbol],
    },
    Reset {
        symbol: Symbol,
        update_mode: UpdateModeId,
    },

    RuntimeErrorFunction(&'a str),
}

impl<'a> Literal<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use Literal::*;

        match self {
            Int(bytes) => alloc.text(format!("{}i64", i128::from_ne_bytes(*bytes))),
            U128(bytes) => alloc.text(format!("{}u128", u128::from_ne_bytes(*bytes))),
            Float(lit) => alloc.text(format!("{}f64", lit)),
            Decimal(bytes) => alloc.text(format!("{}dec", RocDec::from_ne_bytes(*bytes))),
            Bool(lit) => alloc.text(format!("{}", lit)),
            Byte(lit) => alloc.text(format!("{}u8", lit)),
            Str(lit) => alloc.text(format!("{:?}", lit)),
        }
    }
}

pub(crate) fn symbol_to_doc_string(symbol: Symbol) -> String {
    use roc_module::ident::ModuleName;

    if pretty_print_ir_symbols() {
        format!("{:?}", symbol)
    } else {
        let text = format!("{}", symbol);

        if text.starts_with(ModuleName::APP) {
            let name: String = text.trim_start_matches(ModuleName::APP).into();
            format!("Test{}", name)
        } else {
            text
        }
    }
}

fn symbol_to_doc<'b, D, A>(alloc: &'b D, symbol: Symbol) -> DocBuilder<'b, D, A>
where
    D: DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    alloc.text(symbol_to_doc_string(symbol))
}

fn join_point_to_doc<'b, D, A>(alloc: &'b D, symbol: JoinPointId) -> DocBuilder<'b, D, A>
where
    D: DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    symbol_to_doc(alloc, symbol.0)
}

impl<'a> Expr<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use Expr::*;

        match self {
            Literal(lit) => lit.to_doc(alloc),

            Call(call) => call.to_doc(alloc),

            Tag {
                tag_id, arguments, ..
            } => {
                let doc_tag = alloc
                    .text("TagId(")
                    .append(alloc.text(tag_id.to_string()))
                    .append(")");

                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s));

                doc_tag
                    .append(alloc.space())
                    .append(alloc.intersperse(it, " "))
            }
            Reuse {
                symbol,
                tag_id,
                arguments,
                update_mode,
                ..
            } => {
                let doc_tag = alloc
                    .text("TagId(")
                    .append(alloc.text(tag_id.to_string()))
                    .append(")");

                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text("Reuse ")
                    .append(symbol_to_doc(alloc, *symbol))
                    .append(alloc.space())
                    .append(format!("{:?}", update_mode))
                    .append(alloc.space())
                    .append(doc_tag)
                    .append(alloc.space())
                    .append(alloc.intersperse(it, " "))
            }
            Reset {
                symbol,
                update_mode,
            } => alloc.text(format!(
                "Reset {{ symbol: {:?}, id: {} }}",
                symbol, update_mode.id
            )),

            Struct(args) => {
                let it = args.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text("Struct {")
                    .append(alloc.intersperse(it, ", "))
                    .append(alloc.text("}"))
            }
            Array { elems, .. } => {
                let it = elems.iter().map(|e| match e {
                    ListLiteralElement::Literal(l) => l.to_doc(alloc),
                    ListLiteralElement::Symbol(s) => symbol_to_doc(alloc, *s),
                });

                alloc
                    .text("Array [")
                    .append(alloc.intersperse(it, ", "))
                    .append(alloc.text("]"))
            }
            EmptyArray => alloc.text("Array []"),

            StructAtIndex {
                index, structure, ..
            } => alloc
                .text(format!("StructAtIndex {} ", index))
                .append(symbol_to_doc(alloc, *structure)),

            RuntimeErrorFunction(s) => alloc.text(format!("ErrorFunction {}", s)),

            GetTagId { structure, .. } => alloc
                .text("GetTagId ")
                .append(symbol_to_doc(alloc, *structure)),

            ExprBox { symbol, .. } => alloc.text("Box ").append(symbol_to_doc(alloc, *symbol)),

            ExprUnbox { symbol, .. } => alloc.text("Unbox ").append(symbol_to_doc(alloc, *symbol)),

            UnionAtIndex {
                tag_id,
                structure,
                index,
                ..
            } => alloc
                .text(format!("UnionAtIndex (Id {}) (Index {}) ", tag_id, index))
                .append(symbol_to_doc(alloc, *structure)),
        }
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let allocator = BoxAllocator;
        let mut w = std::vec::Vec::new();
        self.to_doc::<_, ()>(&allocator)
            .1
            .render(width, &mut w)
            .unwrap();
        w.push(b'\n');
        String::from_utf8(w).unwrap()
    }
}

impl<'a> Stmt<'a> {
    pub fn new(
        env: &mut Env<'a, '_>,
        can_expr: roc_can::expr::Expr,
        var: Variable,
        procs: &mut Procs<'a>,
        layout_cache: &mut LayoutCache<'a>,
    ) -> Self {
        from_can(env, var, can_expr, procs, layout_cache)
    }

    pub fn to_doc<'b, D, A, I>(&'b self, alloc: &'b D, interner: &I) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: Interner<'a, Layout<'a>>,
    {
        use Stmt::*;

        match self {
            Let(symbol, expr, layout, cont) => alloc
                .text("let ")
                .append(symbol_to_doc(alloc, *symbol))
                .append(" : ")
                .append(layout.to_doc(alloc, interner, Parens::NotNeeded))
                .append(" = ")
                .append(expr.to_doc(alloc))
                .append(";")
                .append(alloc.hardline())
                .append(cont.to_doc(alloc, interner)),

            Refcounting(modify, cont) => modify
                .to_doc(alloc)
                .append(alloc.hardline())
                .append(cont.to_doc(alloc, interner)),

            Expect {
                condition,
                remainder,
                ..
            } => alloc
                .text("expect ")
                .append(symbol_to_doc(alloc, *condition))
                .append(";")
                .append(alloc.hardline())
                .append(remainder.to_doc(alloc, interner)),

            ExpectFx {
                condition,
                remainder,
                ..
            } => alloc
                .text("expect-fx ")
                .append(symbol_to_doc(alloc, *condition))
                .append(";")
                .append(alloc.hardline())
                .append(remainder.to_doc(alloc, interner)),

            Ret(symbol) => alloc
                .text("ret ")
                .append(symbol_to_doc(alloc, *symbol))
                .append(";"),

            Switch {
                cond_symbol,
                branches,
                default_branch,
                ..
            } => {
                match branches {
                    [(1, info, pass)] => {
                        let fail = default_branch.1;
                        alloc
                            .text("if ")
                            .append(symbol_to_doc(alloc, *cond_symbol))
                            .append(" then")
                            .append(info.to_doc(alloc))
                            .append(alloc.hardline())
                            .append(pass.to_doc(alloc, interner).indent(4))
                            .append(alloc.hardline())
                            .append(alloc.text("else"))
                            .append(default_branch.0.to_doc(alloc))
                            .append(alloc.hardline())
                            .append(fail.to_doc(alloc, interner).indent(4))
                    }

                    _ => {
                        let default_doc = alloc
                            .text("default:")
                            .append(alloc.hardline())
                            .append(default_branch.1.to_doc(alloc, interner).indent(4))
                            .indent(4);

                        let branches_docs = branches
                            .iter()
                            .map(|(tag, _info, expr)| {
                                alloc
                                    .text(format!("case {}:", tag))
                                    .append(alloc.hardline())
                                    .append(expr.to_doc(alloc, interner).indent(4))
                                    .indent(4)
                            })
                            .chain(std::iter::once(default_doc));
                        //
                        alloc
                            .text("switch ")
                            .append(symbol_to_doc(alloc, *cond_symbol))
                            .append(":")
                            .append(alloc.hardline())
                            .append(alloc.intersperse(
                                branches_docs,
                                alloc.hardline().append(alloc.hardline()),
                            ))
                            .append(alloc.hardline())
                    }
                }
            }

            Crash(s, _src) => alloc.text("Crash ").append(symbol_to_doc(alloc, *s)),

            Join {
                id,
                parameters,
                body: continuation,
                remainder,
            } => {
                let it = parameters.iter().map(|p| symbol_to_doc(alloc, p.symbol));

                alloc.intersperse(
                    vec![
                        alloc
                            .text("joinpoint ")
                            .append(join_point_to_doc(alloc, *id))
                            .append(" ".repeat(parameters.len().min(1)))
                            .append(alloc.intersperse(it, alloc.space()))
                            .append(":"),
                        continuation.to_doc(alloc, interner).indent(4),
                        alloc.text("in"),
                        remainder.to_doc(alloc, interner),
                    ],
                    alloc.hardline(),
                )
            }
            Jump(id, arguments) => {
                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text("jump ")
                    .append(join_point_to_doc(alloc, *id))
                    .append(" ".repeat(arguments.len().min(1)))
                    .append(alloc.intersperse(it, alloc.space()))
                    .append(";")
            }
        }
    }

    pub fn to_pretty<I>(&self, interner: &I, width: usize) -> String
    where
        I: Interner<'a, Layout<'a>>,
    {
        let allocator = BoxAllocator;
        let mut w = std::vec::Vec::new();
        self.to_doc::<_, (), _>(&allocator, interner)
            .1
            .render(width, &mut w)
            .unwrap();
        w.push(b'\n');
        String::from_utf8(w).unwrap()
    }

    pub fn is_terminal(&self) -> bool {
        use Stmt::*;

        matches!(
            self,
            Switch { .. } | Ret(_) | Jump(_, _) // TODO for Switch; is this the reason Lean only looks at the outermost `when`?
        )
    }

    pub fn if_then_else(
        arena: &'a Bump,
        condition_symbol: Symbol,
        return_layout: Layout<'a>,
        then_branch_stmt: Stmt<'a>,
        else_branch_stmt: &'a Stmt<'a>,
    ) -> Self {
        let then_branch_info = BranchInfo::Constructor {
            scrutinee: condition_symbol,
            layout: Layout::bool(),
            tag_id: 1,
        };
        let then_branch = (1u64, then_branch_info, then_branch_stmt);

        let else_branch_info = BranchInfo::Constructor {
            scrutinee: condition_symbol,
            layout: Layout::bool(),
            tag_id: 0,
        };
        let else_branch = (else_branch_info, else_branch_stmt);

        Stmt::Switch {
            cond_symbol: condition_symbol,
            cond_layout: Layout::bool(),
            branches: &*arena.alloc([then_branch]),
            default_branch: else_branch,
            ret_layout: return_layout,
        }
    }
}

fn from_can_let<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    def: Box<roc_can::def::Def>,
    cont: Box<Loc<roc_can::expr::Expr>>,
    variable: Variable,
    opt_assigned_and_hole: Option<(Symbol, &'a Stmt<'a>)>,
) -> Stmt<'a> {
    use roc_can::expr::Expr::*;

    macro_rules! lower_rest {
        ($variable:expr, $expr:expr) => {
            lower_rest!(env, procs, layout_cache, $variable, $expr)
        };
        ($env:expr, $procs:expr, $layout_cache:expr, $variable:expr, $expr:expr) => {
            match opt_assigned_and_hole {
                None => from_can($env, $variable, $expr, $procs, $layout_cache),
                Some((assigned, hole)) => with_hole(
                    $env,
                    $expr,
                    $variable,
                    $procs,
                    $layout_cache,
                    assigned,
                    hole,
                ),
            }
        };
    }

    if let roc_can::pattern::Pattern::Identifier(symbol) = &def.loc_pattern.value {
        return match def.loc_expr.value {
            Closure(closure_data) => {
                register_capturing_closure(env, procs, layout_cache, *symbol, closure_data);

                lower_rest!(variable, cont.value)
            }
            Accessor(accessor_data) => {
                let fresh_record_symbol = env.unique_symbol();
                register_noncapturing_closure(
                    env,
                    procs,
                    *symbol,
                    accessor_data.to_closure_data(fresh_record_symbol),
                );

                lower_rest!(variable, cont.value)
            }
            Var(original, _) | AbilityMember(original, _, _) => {
                // a variable is aliased, e.g.
                //
                //  foo = bar
                //
                // or
                //
                //  foo = RBTRee.empty

                // TODO: right now we need help out rustc with the closure types;
                // it isn't able to infer the right lifetime bounds. See if we
                // can remove the annotations in the future.
                let build_rest =
                    |env: &mut Env<'a, '_>,
                     procs: &mut Procs<'a>,
                     layout_cache: &mut LayoutCache<'a>| {
                        lower_rest!(env, procs, layout_cache, variable, cont.value)
                    };

                return handle_variable_aliasing(
                    env,
                    procs,
                    layout_cache,
                    def.expr_var,
                    *symbol,
                    original,
                    build_rest,
                );
            }
            LetNonRec(nested_def, nested_cont) => {
                use roc_can::expr::Expr::*;
                // We must transform
                //
                //      let answer = 1337
                //      in
                //          let unused =
                //                  let nested = 17
                //                  in
                //                      nested
                //          in
                //              answer
                //
                // into
                //
                //      let answer = 1337
                //      in
                //          let nested = 17
                //          in
                //              let unused = nested
                //              in
                //                  answer

                let new_def = roc_can::def::Def {
                    loc_pattern: def.loc_pattern,
                    loc_expr: *nested_cont,
                    pattern_vars: def.pattern_vars,
                    annotation: def.annotation,
                    expr_var: def.expr_var,
                };

                let new_inner = LetNonRec(Box::new(new_def), cont);

                let new_outer = LetNonRec(nested_def, Box::new(Loc::at_zero(new_inner)));

                lower_rest!(variable, new_outer)
            }
            LetRec(nested_defs, nested_cont, cycle_mark) => {
                use roc_can::expr::Expr::*;
                // We must transform
                //
                //      let answer = 1337
                //      in
                //          let unused =
                //                  let nested = \{} -> nested {}
                //                  in
                //                      nested
                //          in
                //              answer
                //
                // into
                //
                //      let answer = 1337
                //      in
                //          let nested = \{} -> nested {}
                //          in
                //              let unused = nested
                //              in
                //                  answer

                let new_def = roc_can::def::Def {
                    loc_pattern: def.loc_pattern,
                    loc_expr: *nested_cont,
                    pattern_vars: def.pattern_vars,
                    annotation: def.annotation,
                    expr_var: def.expr_var,
                };

                let new_inner = LetNonRec(Box::new(new_def), cont);

                let new_outer = LetRec(nested_defs, Box::new(Loc::at_zero(new_inner)), cycle_mark);

                lower_rest!(variable, new_outer)
            }
            _ => {
                let rest = lower_rest!(variable, cont.value);

                // Remove all the requested symbol specializations now, since this is the
                // def site and hence we won't need them any higher up.
                let mut needed_specializations = procs.symbol_specializations.remove(*symbol);

                match needed_specializations.len() {
                    0 => {
                        // We don't need any specializations, that means this symbol is never
                        // referenced.
                        with_hole(
                            env,
                            def.loc_expr.value,
                            def.expr_var,
                            procs,
                            layout_cache,
                            *symbol,
                            env.arena.alloc(rest),
                        )
                    }

                    // We do need specializations
                    1 => {
                        let (_specialization_mark, (var, specialized_symbol, _deepest_use)) =
                            needed_specializations.next().unwrap();

                        // Make sure rigid variables in the annotation are converted to flex variables.
                        instantiate_rigids(env.subs, def.expr_var);
                        // Unify the expr_var with the requested specialization once.
                        let _res = env.unify(
                            procs.externals_we_need.values_mut(),
                            layout_cache,
                            var,
                            def.expr_var,
                        );

                        with_hole(
                            env,
                            def.loc_expr.value,
                            def.expr_var,
                            procs,
                            layout_cache,
                            specialized_symbol,
                            env.arena.alloc(rest),
                        )
                    }
                    _n => {
                        let mut stmt = rest;

                        // Make sure rigid variables in the annotation are converted to flex variables.
                        instantiate_rigids(env.subs, def.expr_var);

                        // Need to eat the cost and create a specialized version of the body for
                        // each specialization.
                        for (_specialization_mark, (var, specialized_symbol, _deepest_use)) in
                            needed_specializations
                        {
                            use roc_can::copy::deep_copy_type_vars_into_expr;

                            let (new_def_expr_var, specialized_expr) = deep_copy_type_vars_into_expr(
                            env.subs,
                            def.expr_var,
                            &def.loc_expr.value,
                        )
                        .expect(
                            "expr marked as having specializations, but it has no type variables!",
                        );

                            let _res = env.unify(
                                procs.externals_we_need.values_mut(),
                                layout_cache,
                                var,
                                new_def_expr_var,
                            );

                            stmt = with_hole(
                                env,
                                specialized_expr,
                                new_def_expr_var,
                                procs,
                                layout_cache,
                                specialized_symbol,
                                env.arena.alloc(stmt),
                            );
                        }

                        stmt
                    }
                }
            }
        };
    }

    // this may be a destructure pattern
    let (mono_pattern, assignments) =
        match from_can_pattern(env, procs, layout_cache, &def.loc_pattern.value) {
            Ok(v) => v,
            Err(_) => todo!(),
        };

    // convert the continuation
    let mut stmt = lower_rest!(variable, cont.value);

    // layer on any default record fields
    for (symbol, variable, expr) in assignments {
        let specialization_symbol = procs
            .symbol_specializations
            .remove_single(symbol)
            // Can happen when the symbol was never used under this body, and hence has no
            // requested specialization.
            .unwrap_or(symbol);

        let hole = env.arena.alloc(stmt);
        stmt = with_hole(
            env,
            expr,
            variable,
            procs,
            layout_cache,
            specialization_symbol,
            hole,
        );
    }

    match def.loc_expr.value {
        roc_can::expr::Expr::Var(outer_symbol, _) if !procs.is_module_thunk(outer_symbol) => {
            store_pattern(env, procs, layout_cache, &mono_pattern, outer_symbol, stmt)
        }
        _ => {
            let outer_symbol = env.unique_symbol();
            stmt = store_pattern(env, procs, layout_cache, &mono_pattern, outer_symbol, stmt);

            // convert the def body, store in outer_symbol
            with_hole(
                env,
                def.loc_expr.value,
                def.expr_var,
                procs,
                layout_cache,
                outer_symbol,
                env.arena.alloc(stmt),
            )
        }
    }
}

/// turn record/tag patterns into a when expression, e.g.
///
/// foo = \{ x } -> body
///
/// becomes
///
/// foo = \r -> when r is { x } -> body
///
/// conversion of one-pattern when expressions will do the most optimal thing
#[allow(clippy::type_complexity)]
fn patterns_to_when<'a>(
    env: &mut Env<'a, '_>,
    patterns: std::vec::Vec<(Variable, AnnotatedMark, Loc<roc_can::pattern::Pattern>)>,
    body_var: Variable,
    body: Loc<roc_can::expr::Expr>,
) -> Result<(Vec<'a, Variable>, Vec<'a, Symbol>, Loc<roc_can::expr::Expr>), Loc<RuntimeError>> {
    let mut arg_vars = Vec::with_capacity_in(patterns.len(), env.arena);
    let mut symbols = Vec::with_capacity_in(patterns.len(), env.arena);
    let mut body = Ok(body);

    // patterns that are not yet in a when (e.g. in let or function arguments) must be irrefutable
    // to pass type checking. So the order in which we add them to the body does not matter: there
    // are only stores anyway, no branches.
    //
    // NOTE this fails if the pattern contains rigid variables,
    // see https://github.com/roc-lang/roc/issues/786
    // this must be fixed when moving exhaustiveness checking to the new canonical AST
    for (pattern_var, annotated_mark, pattern) in patterns.into_iter() {
        if annotated_mark.exhaustive.is_non_exhaustive(env.subs) {
            // Even if the body was Ok, replace it with this Err.
            // If it was already an Err, leave it at that Err, so the first
            // RuntimeError we encountered remains the first.
            let value = RuntimeError::UnsupportedPattern(pattern.region);
            body = body.and({
                Err(Loc {
                    region: pattern.region,
                    value,
                })
            });
        } else if let Ok(unwrapped_body) = body {
            let (new_symbol, new_body) =
                pattern_to_when(env, pattern_var, pattern, body_var, unwrapped_body);

            symbols.push(new_symbol);
            arg_vars.push(pattern_var);

            body = Ok(new_body)
        }
    }

    match body {
        Ok(body) => Ok((arg_vars, symbols, body)),
        Err(loc_error) => Err(loc_error),
    }
}

/// turn irrefutable patterns into when. For example
///
/// foo = \{ x } -> body
///
/// Assuming the above program typechecks, the pattern match cannot fail
/// (it is irrefutable). It becomes
///
/// foo = \r ->
///      when r is
///          { x } -> body
///
/// conversion of one-pattern when expressions will do the most optimal thing
fn pattern_to_when<'a>(
    env: &mut Env<'a, '_>,
    pattern_var: Variable,
    pattern: Loc<roc_can::pattern::Pattern>,
    body_var: Variable,
    body: Loc<roc_can::expr::Expr>,
) -> (Symbol, Loc<roc_can::expr::Expr>) {
    use roc_can::expr::Expr::*;
    use roc_can::expr::{WhenBranch, WhenBranchPattern};
    use roc_can::pattern::Pattern::{self, *};

    match &pattern.value {
        Identifier(symbol) => (*symbol, body),
        Underscore => {
            // for underscore we generate a dummy Symbol
            (env.unique_symbol(), body)
        }
        Shadowed(region, loc_ident, new_symbol) => {
            let error = roc_problem::can::RuntimeError::Shadowing {
                original_region: *region,
                shadow: loc_ident.clone(),
                kind: ShadowKind::Variable,
            };
            (*new_symbol, Loc::at_zero(RuntimeError(error)))
        }

        UnsupportedPattern(region) => {
            // create the runtime error here, instead of delegating to When.
            // UnsupportedPattern should then never occur in When
            let error = roc_problem::can::RuntimeError::UnsupportedPattern(*region);
            (env.unique_symbol(), Loc::at_zero(RuntimeError(error)))
        }

        MalformedPattern(problem, region) => {
            // create the runtime error here, instead of delegating to When.
            let error = roc_problem::can::RuntimeError::MalformedPattern(*problem, *region);
            (env.unique_symbol(), Loc::at_zero(RuntimeError(error)))
        }

        OpaqueNotInScope(loc_ident) => {
            // create the runtime error here, instead of delegating to When.
            // TODO(opaques) should be `RuntimeError::OpaqueNotDefined`
            let error = roc_problem::can::RuntimeError::UnsupportedPattern(loc_ident.region);
            (env.unique_symbol(), Loc::at_zero(RuntimeError(error)))
        }

        AppliedTag { .. } | RecordDestructure { .. } | UnwrappedOpaque { .. } => {
            let symbol = env.unique_symbol();

            let wrapped_body = When {
                cond_var: pattern_var,
                expr_var: body_var,
                region: Region::zero(),
                loc_cond: Box::new(Loc::at_zero(Var(symbol, pattern_var))),
                branches: vec![WhenBranch {
                    patterns: vec![WhenBranchPattern {
                        pattern,
                        degenerate: false,
                    }],
                    value: body,
                    guard: None,
                    // If this type-checked, it's non-redundant
                    redundant: RedundantMark::known_non_redundant(),
                }],
                branches_cond_var: pattern_var,
                // If this type-checked, it's exhaustive
                exhaustive: ExhaustiveMark::known_exhaustive(),
            };

            (symbol, Loc::at_zero(wrapped_body))
        }

        Pattern::List { .. } => todo!(),

        IntLiteral(..)
        | NumLiteral(..)
        | FloatLiteral(..)
        | StrLiteral(..)
        | roc_can::pattern::Pattern::SingleQuote(..) => {
            // These patters are refutable, and thus should never occur outside a `when` expression
            // They should have been replaced with `UnsupportedPattern` during canonicalization
            unreachable!("refutable pattern {:?} where irrefutable pattern is expected. This should never happen!", pattern.value)
        }

        AbilityMemberSpecialization { .. } => {
            unreachable!(
                "Ability member specialization {:?} should never appear in a when!",
                pattern.value
            )
        }
    }
}

fn specialize_suspended<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    suspended: Suspended<'a>,
) {
    let offset_variable = StorageSubs::merge_into(suspended.store, env.subs);

    for (i, (symbol_or_lambda, var)) in suspended
        .symbol_or_lambdas
        .iter()
        .zip(suspended.variables.iter())
        .enumerate()
    {
        let name = *symbol_or_lambda;
        let outside_layout = suspended.layouts[i];

        let var = offset_variable(*var);

        // TODO define our own Entry for Specialized?
        let partial_proc = if procs
            .specialized
            .is_specialized(name.name(), &outside_layout)
        {
            // already specialized, just continue
            continue;
        } else {
            match procs.partial_procs.symbol_to_id(name.name()) {
                Some(v) => {
                    // Mark this proc as in-progress, so if we're dealing with
                    // mutually recursive functions, we don't loop forever.
                    // (We had a bug around this before this system existed!)
                    procs
                        .specialized
                        .mark_in_progress(name.name(), outside_layout);

                    v
                }
                None => {
                    // TODO this assumes the specialization is done by another module
                    // make sure this does not become a problem down the road!
                    debug_assert!(name.name().module_id() != name.name().module_id());
                    continue;
                }
            }
        };

        match specialize_variable(env, procs, name, layout_cache, var, &[], partial_proc) {
            Ok((proc, _layout)) => {
                // TODO this code is duplicated elsewhere

                // the `layout` is a function pointer, while `_ignore_layout` can be a
                // closure. We only specialize functions, storing this value with a closure
                // layout will give trouble.
                let arguments = Vec::from_iter_in(proc.args.iter().map(|(l, _)| *l), env.arena)
                    .into_bump_slice();

                let proper_layout = ProcLayout {
                    arguments,
                    result: proc.ret_layout,
                    captures_niche: proc.name.captures_niche(),
                };
                if procs.is_module_thunk(proc.name.name()) {
                    debug_assert!(
                        proper_layout.arguments.is_empty(),
                        "{:?} from {:?}",
                        name,
                        proper_layout
                    );
                }

                // NOTE: some functions are specialized to have a closure, but don't actually
                // need any closure argument. Here is where we correct this sort of thing,
                // by trusting the layout of the Proc, not of what we specialize for
                procs
                    .specialized
                    .remove_specialized(name.name(), &outside_layout);
                procs
                    .specialized
                    .insert_specialized(name.name(), proper_layout, proc);
            }
            Err(SpecializeFailure {
                attempted_layout, ..
            }) => {
                let proc = generate_runtime_error_function(
                    env,
                    layout_cache,
                    name.name(),
                    attempted_layout,
                );

                let top_level = ProcLayout::from_raw(
                    env.arena,
                    &layout_cache.interner,
                    attempted_layout,
                    CapturesNiche::no_niche(),
                );

                procs
                    .specialized
                    .insert_specialized(name.name(), top_level, proc);
            }
        }
    }
}

pub fn specialize_all<'a>(
    env: &mut Env<'a, '_>,
    mut procs: Procs<'a>,
    externals_others_need: std::vec::Vec<ExternalSpecializations<'a>>,
    specializations_for_host: HostSpecializations<'a>,
    layout_cache: &mut LayoutCache<'a>,
) -> Procs<'a> {
    // When calling from_can, pending_specializations should be unavailable.
    // This must be a single pass, and we must not add any more entries to it!
    let pending_specializations = std::mem::replace(
        &mut procs.pending_specializations,
        PendingSpecializations::Making(Suspended::new_in(env.arena)),
    );

    // Add all of our existing pending specializations.
    match pending_specializations {
        PendingSpecializations::Finding(suspended) => {
            specialize_suspended(env, &mut procs, layout_cache, suspended)
        }
        PendingSpecializations::Making(suspended) => {
            debug_assert!(
                suspended.is_empty(),
                "suspended specializations cannot ever start off non-empty when making"
            );
        }
    }

    // Specialize all the symbols everyone else needs.
    for externals in externals_others_need {
        specialize_external_specializations(env, &mut procs, layout_cache, externals);
    }

    // Specialize any symbols the host needs.
    specialize_host_specializations(env, &mut procs, layout_cache, specializations_for_host);

    // Now, we must go through and continuously complete any new suspended specializations that were
    // discovered in specializing the other demanded symbols.
    while !procs.pending_specializations.is_empty() {
        let pending_specializations = std::mem::replace(
            &mut procs.pending_specializations,
            PendingSpecializations::Making(Suspended::new_in(env.arena)),
        );
        match pending_specializations {
            PendingSpecializations::Making(suspended) => {
                specialize_suspended(env, &mut procs, layout_cache, suspended);
            }
            PendingSpecializations::Finding(_) => {
                internal_error!("should not have this variant after making specializations")
            }
        }
    }

    debug_assert!(
        procs.symbol_specializations.is_empty(),
        "{:?}",
        &procs.symbol_specializations
    );

    procs
}

fn specialize_host_specializations<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    host_specializations: HostSpecializations<'a>,
) {
    let (store, it) = host_specializations.decompose();

    let offset_variable = StorageSubs::merge_into(store, env.subs);

    for (symbol, variable, host_exposed_aliases) in it {
        specialize_external_help(
            env,
            procs,
            layout_cache,
            symbol,
            offset_variable(variable),
            &host_exposed_aliases,
        )
    }
}

fn specialize_external_specializations<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    externals_others_need: ExternalSpecializations<'a>,
) {
    let (store, it) = externals_others_need.decompose();

    let offset_variable = StorageSubs::merge_into(store, env.subs);

    for (symbol, solved_types) in it {
        for store_variable in solved_types {
            let imported_variable = offset_variable(store_variable);

            roc_tracing::debug!(proc_name = ?symbol, ?store_variable, ?imported_variable, "specializing needed external");

            // historical note: we used to deduplicate with a hash here,
            // but the cost of that hash is very high. So for now we make
            // duplicate specializations, and the insertion into a hash map
            // below will deduplicate them.

            specialize_external_help(env, procs, layout_cache, symbol, imported_variable, &[])
        }
    }
}

fn specialize_external_help<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    name: LambdaName<'a>,
    variable: Variable,
    host_exposed_aliases: &[(Symbol, Variable)],
) {
    let partial_proc_id = match procs.partial_procs.symbol_to_id(name.name()) {
        Some(v) => v,
        None => {
            panic!("Cannot find a partial proc for {:?}", name);
        }
    };

    let specialization_result = specialize_variable(
        env,
        procs,
        name,
        layout_cache,
        variable,
        host_exposed_aliases,
        partial_proc_id,
    );

    match specialization_result {
        Ok((proc, layout)) => {
            let top_level = ProcLayout::from_raw(
                env.arena,
                &layout_cache.interner,
                layout,
                proc.name.captures_niche(),
            );

            if procs.is_module_thunk(name.name()) {
                debug_assert!(top_level.arguments.is_empty());
            }

            procs
                .specialized
                .insert_specialized(name.name(), top_level, proc);
        }
        Err(SpecializeFailure { attempted_layout }) => {
            let proc =
                generate_runtime_error_function(env, layout_cache, name.name(), attempted_layout);

            let top_level = ProcLayout::from_raw(
                env.arena,
                &layout_cache.interner,
                attempted_layout,
                proc.name.captures_niche(),
            );

            procs
                .specialized
                .insert_specialized(name.name(), top_level, proc);
        }
    }
}

fn generate_runtime_error_function<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &LayoutCache<'a>,
    name: Symbol,
    layout: RawFunctionLayout<'a>,
) -> Proc<'a> {
    let mut msg = bumpalo::collections::string::String::with_capacity_in(80, env.arena);
    use std::fmt::Write;
    write!(
        &mut msg,
        "The {:?} function could not be generated, likely due to a type error.",
        name
    )
    .unwrap();

    dbg_do!(ROC_PRINT_RUNTIME_ERROR_GEN, {
        eprintln!(
            "emitted runtime error function {:?} for layout {:?}",
            &msg, layout
        );
    });

    let runtime_error = runtime_error(env, msg.into_bump_str());

    let (args, ret_layout) = match layout {
        RawFunctionLayout::Function(arg_layouts, lambda_set, ret_layout) => {
            let real_arg_layouts =
                lambda_set.extend_argument_list(env.arena, &layout_cache.interner, arg_layouts);
            let mut args = Vec::with_capacity_in(real_arg_layouts.len(), env.arena);

            for arg in arg_layouts {
                args.push((*arg, env.unique_symbol()));
            }
            if real_arg_layouts.len() != arg_layouts.len() {
                args.push((Layout::LambdaSet(lambda_set), Symbol::ARG_CLOSURE));
            }

            (args.into_bump_slice(), *ret_layout)
        }
        RawFunctionLayout::ZeroArgumentThunk(ret_layout) => (&[] as &[_], ret_layout),
    };

    Proc {
        name: LambdaName::no_niche(name),
        args,
        body: runtime_error,
        closure_data_layout: None,
        ret_layout,
        is_self_recursive: SelfRecursive::NotSelfRecursive,
        must_own_arguments: false,
        host_exposed_layouts: HostExposedLayouts::NotHostExposed,
    }
}

/// A snapshot of the state of types at a moment in time.
/// Includes the exact types, but also auxiliary information like layouts.
struct TypeStateSnapshot {
    subs_snapshot: roc_types::subs::SubsSnapshot,
    layout_snapshot: crate::layout::CacheSnapshot,
    external_storage_snapshot: VecMap<ModuleId, ExternalModuleStorageSnapshot>,
}

/// Takes a snapshot of the type state. Snapshots should be taken before new specializations, and
/// accordingly [rolled back][rollback_typestate] a specialization is complete, so as to not
/// interfere with other specializations.
fn snapshot_typestate(
    subs: &mut Subs,
    procs: &mut Procs,
    layout_cache: &mut LayoutCache<'_>,
) -> TypeStateSnapshot {
    TypeStateSnapshot {
        subs_snapshot: subs.snapshot(),
        layout_snapshot: layout_cache.snapshot(),
        external_storage_snapshot: procs
            .externals_we_need
            .iter_mut()
            .map(|(module, es)| (*module, es.snapshot_cache()))
            .collect(),
    }
}

/// Rolls back the type state to the given [snapshot].
/// Should be called after a specialization is complete to avoid interfering with other
/// specializations.
fn rollback_typestate(
    subs: &mut Subs,
    procs: &mut Procs,
    layout_cache: &mut LayoutCache<'_>,
    snapshot: TypeStateSnapshot,
) {
    let TypeStateSnapshot {
        subs_snapshot,
        layout_snapshot,
        mut external_storage_snapshot,
    } = snapshot;

    subs.rollback_to(subs_snapshot);
    layout_cache.rollback_to(layout_snapshot);

    for (module, es) in procs.externals_we_need.iter_mut() {
        if let Some((_, snapshot)) = external_storage_snapshot.remove(module) {
            es.rollback_cache(snapshot);
        } else {
            es.invalidate_whole_cache();
        }
    }
}

/// Specialize a single proc.
///
/// The caller should snapshot and rollback the type state before and after calling this function,
/// respectively. This function will not take snapshots itself, but will modify the type state.
fn specialize_proc_help<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    lambda_name: LambdaName<'a>,
    layout_cache: &mut LayoutCache<'a>,
    fn_var: Variable,
    host_exposed_variables: &[(Symbol, Variable)],
    partial_proc_id: PartialProcId,
) -> Result<Proc<'a>, LayoutProblem> {
    let partial_proc = procs.partial_procs.get_id(partial_proc_id);
    let captured_symbols = partial_proc.captured_symbols;

    let _unified = env.unify(
        procs.externals_we_need.values_mut(),
        layout_cache,
        partial_proc.annotation,
        fn_var,
    );

    // This will not hold for programs with type errors
    // let is_valid = matches!(unified, roc_unify::unify::Unified::Success(_));
    // debug_assert!(is_valid, "unificaton failure for {:?}", proc_name);

    // if this is a closure, add the closure record argument
    let pattern_symbols = match partial_proc.captured_symbols {
        CapturedSymbols::None => partial_proc.pattern_symbols,
        CapturedSymbols::Captured([]) => partial_proc.pattern_symbols,
        CapturedSymbols::Captured(_) => {
            let mut temp =
                Vec::from_iter_in(partial_proc.pattern_symbols.iter().copied(), env.arena);
            temp.push(Symbol::ARG_CLOSURE);
            temp.into_bump_slice()
        }
    };

    let specialized =
        build_specialized_proc_from_var(env, layout_cache, lambda_name, pattern_symbols, fn_var)?;

    let recursivity = if partial_proc.is_self_recursive {
        SelfRecursive::SelfRecursive(JoinPointId(env.unique_symbol()))
    } else {
        SelfRecursive::NotSelfRecursive
    };

    let body = partial_proc.body.clone();
    let body_var = partial_proc.body_var;

    // determine the layout of aliases/rigids exposed to the host
    let host_exposed_layouts = if host_exposed_variables.is_empty() {
        HostExposedLayouts::NotHostExposed
    } else {
        let mut aliases = BumpMap::new_in(env.arena);

        for (symbol, variable) in host_exposed_variables {
            let layout = layout_cache
                .raw_from_var(env.arena, *variable, env.subs)
                .unwrap();

            let name = env.unique_symbol();

            match layout {
                RawFunctionLayout::Function(argument_layouts, lambda_set, return_layout) => {
                    let assigned = env.unique_symbol();

                    let mut argument_symbols =
                        Vec::with_capacity_in(argument_layouts.len(), env.arena);
                    let mut proc_arguments =
                        Vec::with_capacity_in(argument_layouts.len() + 1, env.arena);
                    let mut top_level_arguments =
                        Vec::with_capacity_in(argument_layouts.len() + 1, env.arena);

                    for layout in argument_layouts {
                        let symbol = env.unique_symbol();

                        proc_arguments.push((*layout, symbol));

                        argument_symbols.push(symbol);
                        top_level_arguments.push(*layout);
                    }

                    // the proc needs to take an extra closure argument
                    let lambda_set_layout = Layout::LambdaSet(lambda_set);
                    proc_arguments.push((lambda_set_layout, Symbol::ARG_CLOSURE));

                    // this should also be reflected in the TopLevel signature
                    top_level_arguments.push(lambda_set_layout);

                    let hole = env.arena.alloc(Stmt::Ret(assigned));

                    let body = match_on_lambda_set(
                        env,
                        layout_cache,
                        procs,
                        lambda_set,
                        Symbol::ARG_CLOSURE,
                        argument_symbols.into_bump_slice(),
                        argument_layouts,
                        return_layout,
                        assigned,
                        hole,
                    );

                    let proc = Proc {
                        name: LambdaName::no_niche(name),
                        args: proc_arguments.into_bump_slice(),
                        body,
                        closure_data_layout: None,
                        ret_layout: *return_layout,
                        is_self_recursive: SelfRecursive::NotSelfRecursive,
                        must_own_arguments: false,
                        host_exposed_layouts: HostExposedLayouts::NotHostExposed,
                    };

                    let top_level = ProcLayout::new(
                        env.arena,
                        top_level_arguments.into_bump_slice(),
                        CapturesNiche::no_niche(),
                        *return_layout,
                    );

                    procs.specialized.insert_specialized(name, top_level, proc);

                    aliases.insert(*symbol, (name, top_level, layout));
                }
                RawFunctionLayout::ZeroArgumentThunk(result) => {
                    let assigned = env.unique_symbol();
                    let hole = env.arena.alloc(Stmt::Ret(assigned));
                    let forced = force_thunk(env, lambda_name.name(), result, assigned, hole);

                    let proc = Proc {
                        name: LambdaName::no_niche(name),
                        args: &[],
                        body: forced,
                        closure_data_layout: None,
                        ret_layout: result,
                        is_self_recursive: SelfRecursive::NotSelfRecursive,
                        must_own_arguments: false,
                        host_exposed_layouts: HostExposedLayouts::NotHostExposed,
                    };

                    let top_level = ProcLayout::from_raw(
                        env.arena,
                        &layout_cache.interner,
                        layout,
                        CapturesNiche::no_niche(),
                    );

                    procs.specialized.insert_specialized(name, top_level, proc);

                    aliases.insert(
                        *symbol,
                        (
                            name,
                            ProcLayout::new(env.arena, &[], CapturesNiche::no_niche(), result),
                            layout,
                        ),
                    );
                }
            }
        }

        HostExposedLayouts::HostExposed {
            rigids: BumpMap::new_in(env.arena),
            aliases,
        }
    };

    let mut specialized_body = from_can(env, body_var, body, procs, layout_cache);

    let specialized_proc = match specialized {
        SpecializedLayout::FunctionPointerBody {
            ret_layout,
            closure: opt_closure_layout,
        } => {
            // this is a function body like
            //
            //      foo = Num.add
            //
            // we need to expand this to
            //
            //      foo = \x,y -> Num.add x y

            let closure_data_layout = match opt_closure_layout {
                Some(lambda_set) => Layout::LambdaSet(lambda_set),
                None => Layout::UNIT,
            };

            // I'm not sure how to handle the closure case, does it ever occur?
            debug_assert!(matches!(captured_symbols, CapturedSymbols::None));

            Proc {
                name: lambda_name,
                args: &[],
                body: specialized_body,
                closure_data_layout: Some(closure_data_layout),
                ret_layout,
                is_self_recursive: recursivity,
                must_own_arguments: false,
                host_exposed_layouts,
            }
        }
        SpecializedLayout::FunctionBody {
            arguments: proc_args,
            closure: opt_closure_layout,
            ret_layout,
        } => {
            let mut proc_args = Vec::from_iter_in(proc_args.iter().copied(), env.arena);

            // unpack the closure symbols, if any
            match (opt_closure_layout, captured_symbols) {
                (Some(closure_layout), CapturedSymbols::Captured(captured)) => {
                    // debug_assert!(!captured.is_empty());

                    // An argument from the closure list may have taken on a specialized symbol
                    // name during the evaluation of the def body. If this is the case, load the
                    // specialized name rather than the original captured name!
                    let get_specialized_name = |symbol| {
                        let specs_used_in_body =
                            procs.get_symbol_specializations_used_in_body(symbol);

                        match specs_used_in_body {
                            Some(mut specs) => {
                                let spec_symbol =
                                    specs.next().map(|(_, sym)| sym).unwrap_or(symbol);
                                if specs.next().is_some() {
                                    internal_error!(
                                        "polymorphic symbol captures not supported yet"
                                    );
                                }
                                spec_symbol
                            }
                            None => symbol,
                        }
                    };

                    match closure_layout
                        .layout_for_member_with_lambda_name(&layout_cache.interner, lambda_name)
                    {
                        ClosureRepresentation::Union {
                            alphabetic_order_fields: field_layouts,
                            union_layout,
                            tag_id,
                            ..
                        } => {
                            debug_assert!(matches!(
                                union_layout,
                                UnionLayout::NonRecursive(_)
                                    | UnionLayout::Recursive(_)
                                    | UnionLayout::NullableUnwrapped { .. }
                            ));
                            debug_assert_eq!(field_layouts.len(), captured.len());

                            // captured variables are in symbol-alphabetic order, but now we want
                            // them ordered by their alignment requirements
                            let mut combined = Vec::from_iter_in(
                                captured.iter().map(|(x, _)| x).zip(field_layouts.iter()),
                                env.arena,
                            );

                            let ptr_bytes = env.target_info;

                            combined.sort_by(|(_, layout1), (_, layout2)| {
                                let size1 =
                                    layout1.alignment_bytes(&layout_cache.interner, ptr_bytes);
                                let size2 =
                                    layout2.alignment_bytes(&layout_cache.interner, ptr_bytes);

                                size2.cmp(&size1)
                            });

                            for (index, (symbol, _)) in combined.iter().enumerate() {
                                let layout = union_layout.layout_at(tag_id, index);

                                let expr = Expr::UnionAtIndex {
                                    tag_id,
                                    structure: Symbol::ARG_CLOSURE,
                                    index: index as u64,
                                    union_layout,
                                };

                                let symbol = get_specialized_name(**symbol);

                                specialized_body = Stmt::Let(
                                    symbol,
                                    expr,
                                    layout,
                                    env.arena.alloc(specialized_body),
                                );
                            }
                        }
                        ClosureRepresentation::AlphabeticOrderStruct(field_layouts) => {
                            // captured variables are in symbol-alphabetic order, but now we want
                            // them ordered by their alignment requirements
                            //
                            // TODO: sort only the fields and apply the found permutation to the symbols
                            // TODO: can we move this ordering to `layout_for_member`?
                            let mut combined = Vec::from_iter_in(
                                captured.iter().map(|(x, _)| x).zip(field_layouts.iter()),
                                env.arena,
                            );

                            let ptr_bytes = env.target_info;

                            combined.sort_by(|(_, layout1), (_, layout2)| {
                                let size1 =
                                    layout1.alignment_bytes(&layout_cache.interner, ptr_bytes);
                                let size2 =
                                    layout2.alignment_bytes(&layout_cache.interner, ptr_bytes);

                                size2.cmp(&size1)
                            });

                            let ordered_field_layouts = Vec::from_iter_in(
                                combined.iter().map(|(_, layout)| **layout),
                                env.arena,
                            );
                            let ordered_field_layouts = ordered_field_layouts.into_bump_slice();

                            debug_assert_eq!(
                                captured.len(),
                                ordered_field_layouts.len(),
                                "{:?} captures {:?} but has layout {:?}",
                                lambda_name,
                                &captured,
                                &ordered_field_layouts
                            );

                            for (index, (symbol, layout)) in combined.iter().enumerate() {
                                let expr = Expr::StructAtIndex {
                                    index: index as _,
                                    field_layouts: ordered_field_layouts,
                                    structure: Symbol::ARG_CLOSURE,
                                };

                                let symbol = get_specialized_name(**symbol);

                                specialized_body = Stmt::Let(
                                    symbol,
                                    expr,
                                    **layout,
                                    env.arena.alloc(specialized_body),
                                );
                            }
                        }

                        ClosureRepresentation::UnwrappedCapture(_layout) => {
                            debug_assert_eq!(captured.len(), 1);
                            let (captured_symbol, _captured_layout) = captured[0];

                            // The capture set is unwrapped, so simply replace the closure argument
                            // to the function with the unwrapped capture name.
                            let captured_symbol = get_specialized_name(captured_symbol);
                            let closure_arg = proc_args.last_mut().unwrap();
                            debug_assert_eq!(closure_arg.1, Symbol::ARG_CLOSURE);
                            closure_arg.1 = captured_symbol;
                        }

                        ClosureRepresentation::EnumDispatch(_) => {
                            // just ignore this value, since it's not a capture
                            // IDEA don't pass this value in the future
                        }
                    }
                }
                (None, CapturedSymbols::None) | (None, CapturedSymbols::Captured([])) => {}
                _ => unreachable!("to closure or not to closure?"),
            }

            proc_args.iter_mut().for_each(|(_layout, symbol)| {
                // Grab the specialization symbol, if it exists.
                *symbol = procs
                    .symbol_specializations
                    .remove_single(*symbol)
                    .unwrap_or(*symbol);
            });

            let closure_data_layout = match opt_closure_layout {
                Some(lambda_set) => Some(Layout::LambdaSet(lambda_set)),
                None => None,
            };

            Proc {
                name: lambda_name,
                args: proc_args.into_bump_slice(),
                body: specialized_body,
                closure_data_layout,
                ret_layout,
                is_self_recursive: recursivity,
                must_own_arguments: false,
                host_exposed_layouts,
            }
        }
    };

    Ok(specialized_proc)
}

#[derive(Debug)]
enum SpecializedLayout<'a> {
    /// A body like `foo = \a,b,c -> ...`
    FunctionBody {
        arguments: &'a [(Layout<'a>, Symbol)],
        closure: Option<LambdaSet<'a>>,
        ret_layout: Layout<'a>,
    },
    /// A body like `foo = Num.add`
    FunctionPointerBody {
        closure: Option<LambdaSet<'a>>,
        ret_layout: Layout<'a>,
    },
}

#[allow(clippy::type_complexity)]
fn build_specialized_proc_from_var<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    lambda_name: LambdaName<'a>,
    pattern_symbols: &[Symbol],
    fn_var: Variable,
) -> Result<SpecializedLayout<'a>, LayoutProblem> {
    match layout_cache.raw_from_var(env.arena, fn_var, env.subs)? {
        RawFunctionLayout::Function(pattern_layouts, closure_layout, ret_layout) => {
            let mut pattern_layouts_vec = Vec::with_capacity_in(pattern_layouts.len(), env.arena);
            pattern_layouts_vec.extend_from_slice(pattern_layouts);

            build_specialized_proc(
                env.arena,
                lambda_name,
                pattern_symbols,
                pattern_layouts_vec,
                Some(closure_layout),
                *ret_layout,
            )
        }
        RawFunctionLayout::ZeroArgumentThunk(ret_layout) => {
            // a top-level constant 0-argument thunk
            build_specialized_proc(
                env.arena,
                lambda_name,
                pattern_symbols,
                Vec::new_in(env.arena),
                None,
                ret_layout,
            )
        }
    }
}

#[allow(clippy::type_complexity)]
fn build_specialized_proc<'a>(
    arena: &'a Bump,
    lambda_name: LambdaName<'a>,
    pattern_symbols: &[Symbol],
    pattern_layouts: Vec<'a, Layout<'a>>,
    lambda_set: Option<LambdaSet<'a>>,
    ret_layout: Layout<'a>,
) -> Result<SpecializedLayout<'a>, LayoutProblem> {
    use SpecializedLayout::*;

    let mut proc_args = Vec::with_capacity_in(pattern_layouts.len(), arena);

    let pattern_layouts_len = pattern_layouts.len();

    for (arg_layout, arg_name) in pattern_layouts.into_iter().zip(pattern_symbols.iter()) {
        proc_args.push((arg_layout, *arg_name));
    }

    // Given
    //
    //     foo =
    //         x = 42
    //
    //         f = \{} -> x
    //
    // We desugar that into
    //
    //     f = \{}, x -> x
    //
    //     foo =
    //         x = 42
    //
    //         f_closure = { ptr: f, closure: x }
    //
    // then

    let proc_name = lambda_name.name();
    match lambda_set {
        Some(lambda_set) if pattern_symbols.last() == Some(&Symbol::ARG_CLOSURE) => {
            // here we define the lifted (now top-level) f function. Its final argument is `Symbol::ARG_CLOSURE`,
            // it stores the closure structure (just an integer in this case)
            proc_args.push((Layout::LambdaSet(lambda_set), Symbol::ARG_CLOSURE));

            debug_assert_eq!(
                pattern_layouts_len + 1,
                pattern_symbols.len(),
                "Tried to zip two vecs with different lengths in {:?}!",
                proc_name,
            );

            let proc_args = proc_args.into_bump_slice();

            Ok(FunctionBody {
                arguments: proc_args,
                closure: Some(lambda_set),
                ret_layout,
            })
        }
        Some(lambda_set) => {
            // a function that returns a function, but is not itself a closure
            // e.g.  f = Num.add

            // make sure there is not arg_closure argument without a closure layout
            debug_assert!(pattern_symbols.last() != Some(&Symbol::ARG_CLOSURE));

            use std::cmp::Ordering;
            match pattern_layouts_len.cmp(&pattern_symbols.len()) {
                Ordering::Equal => {
                    let proc_args = proc_args.into_bump_slice();

                    Ok(FunctionBody {
                        arguments: proc_args,
                        closure: None,
                        ret_layout,
                    })
                }
                Ordering::Greater => {
                    if pattern_symbols.is_empty() {
                        let ret_layout = Layout::LambdaSet(lambda_set);
                        Ok(FunctionPointerBody {
                            closure: None,
                            ret_layout,
                        })
                    } else {
                        // so far, the problem when hitting this branch was always somewhere else
                        // I think this branch should not be reachable in a bugfree compiler
                        panic!(
                            "more arguments (according to the layout) than argument symbols for {:?}",
                            proc_name
                        )
                    }
                }
                Ordering::Less => panic!(
                    "more argument symbols than arguments (according to the layout) for {:?}",
                    proc_name
                ),
            }
        }
        None => {
            // else we're making a normal function, no closure problems to worry about
            // we'll just assert some things

            // make sure there is not arg_closure argument without a closure layout
            debug_assert!(pattern_symbols.last() != Some(&Symbol::ARG_CLOSURE));

            use std::cmp::Ordering;
            match pattern_layouts_len.cmp(&pattern_symbols.len()) {
                Ordering::Equal => {
                    let proc_args = proc_args.into_bump_slice();

                    Ok(FunctionBody {
                        arguments: proc_args,
                        closure: None,
                        ret_layout,
                    })
                }
                Ordering::Greater => {
                    if pattern_symbols.is_empty() {
                        Ok(FunctionPointerBody {
                            closure: None,
                            ret_layout,
                        })
                    } else {
                        // so far, the problem when hitting this branch was always somewhere else
                        // I think this branch should not be reachable in a bugfree compiler
                        panic!(
                            "more arguments (according to the layout) than argument symbols for {:?}",
                            proc_name
                        )
                    }
                }
                Ordering::Less => panic!(
                    "more argument symbols than arguments (according to the layout) for {:?}",
                    proc_name
                ),
            }
        }
    }
}

#[derive(Debug)]
struct SpecializeFailure<'a> {
    /// The layout we attempted to create
    attempted_layout: RawFunctionLayout<'a>,
}

type SpecializeSuccess<'a> = (Proc<'a>, RawFunctionLayout<'a>);

fn specialize_variable<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    proc_name: LambdaName<'a>,
    layout_cache: &mut LayoutCache<'a>,
    fn_var: Variable,
    host_exposed_variables: &[(Symbol, Variable)],
    partial_proc_id: PartialProcId,
) -> Result<SpecializeSuccess<'a>, SpecializeFailure<'a>> {
    let snapshot = snapshot_typestate(env.subs, procs, layout_cache);

    // for debugging only
    let raw = layout_cache
        .raw_from_var(env.arena, fn_var, env.subs)
        .unwrap_or_else(|err| panic!("TODO handle invalid function {:?}", err));

    let raw = if procs.is_module_thunk(proc_name.name()) {
        match raw {
            RawFunctionLayout::Function(_, lambda_set, _) => {
                RawFunctionLayout::ZeroArgumentThunk(Layout::LambdaSet(lambda_set))
            }
            _ => raw,
        }
    } else {
        raw
    };

    // make sure rigid variables in the annotation are converted to flex variables
    let annotation_var = procs.partial_procs.get_id(partial_proc_id).annotation;
    instantiate_rigids(env.subs, annotation_var);

    procs.push_active_specialization(proc_name.name());
    roc_tracing::debug!(?proc_name, ?fn_var, fn_content = ?roc_types::subs::SubsFmtContent(env.subs.get_content_without_compacting(fn_var), env.subs), "specialization start");

    let specialized = specialize_proc_help(
        env,
        procs,
        proc_name,
        layout_cache,
        fn_var,
        host_exposed_variables,
        partial_proc_id,
    );

    roc_tracing::debug!(
        ?proc_name,
        succeeded = specialized.is_ok(),
        "specialization end"
    );
    procs.pop_active_specialization(proc_name.name());

    let result = match specialized {
        Ok(proc) => {
            // when successful, the layout after unification should be the layout before unification
            //            debug_assert_eq!(
            //                attempted_layout,
            //                layout_cache
            //                    .from_var(env.arena, fn_var, env.subs)
            //                    .unwrap_or_else(|err| panic!("TODO handle invalid function {:?}", err))
            //            );

            Ok((proc, raw))
        }
        Err(error) => {
            // earlier we made this information available where we handle the failure
            // but we didn't do anything useful with it. So it's here if we ever need it again
            let _ = error;

            Err(SpecializeFailure {
                attempted_layout: raw,
            })
        }
    };

    rollback_typestate(env.subs, procs, layout_cache, snapshot);

    result
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ProcLayout<'a> {
    pub arguments: &'a [Layout<'a>],
    pub result: Layout<'a>,
    pub captures_niche: CapturesNiche<'a>,
}

impl<'a> ProcLayout<'a> {
    pub fn new(
        arena: &'a Bump,
        old_arguments: &'a [Layout<'a>],
        old_captures_niche: CapturesNiche<'a>,
        result: Layout<'a>,
    ) -> Self {
        let mut arguments = Vec::with_capacity_in(old_arguments.len(), arena);

        for old in old_arguments {
            let other = old;
            arguments.push(*other);
        }

        let other = result;
        let new_result = other;

        ProcLayout {
            arguments: arguments.into_bump_slice(),
            captures_niche: old_captures_niche,
            result: new_result,
        }
    }

    pub fn from_raw(
        arena: &'a Bump,
        interner: &LayoutInterner<'a>,
        raw: RawFunctionLayout<'a>,
        captures_niche: CapturesNiche<'a>,
    ) -> Self {
        match raw {
            RawFunctionLayout::Function(arguments, lambda_set, result) => {
                let arguments = lambda_set.extend_argument_list(arena, interner, arguments);
                ProcLayout::new(arena, arguments, captures_niche, *result)
            }
            RawFunctionLayout::ZeroArgumentThunk(result) => {
                ProcLayout::new(arena, &[], CapturesNiche::no_niche(), result)
            }
        }
    }
}

fn specialize_naked_symbol<'a>(
    env: &mut Env<'a, '_>,
    variable: Variable,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
    symbol: Symbol,
) -> Stmt<'a> {
    if procs.is_module_thunk(symbol) {
        let fn_var = variable;

        // This is a top-level declaration, which will code gen to a 0-arity thunk.
        let result = call_by_name(
            env,
            procs,
            fn_var,
            symbol,
            std::vec::Vec::new(),
            layout_cache,
            assigned,
            hole,
        );

        return result;
    } else if env.is_imported_symbol(symbol) {
        match layout_cache.from_var(env.arena, variable, env.subs) {
            Err(e) => panic!("invalid layout {:?}", e),
            Ok(_) => {
                // this is a 0-arity thunk
                let result = call_by_name(
                    env,
                    procs,
                    variable,
                    symbol,
                    std::vec::Vec::new(),
                    layout_cache,
                    assigned,
                    match hole {
                        Stmt::Jump(id, _) => env
                            .arena
                            .alloc(Stmt::Jump(*id, env.arena.alloc([assigned]))),
                        Stmt::Ret(_) => env.arena.alloc(Stmt::Ret(assigned)),
                        hole => hole,
                    },
                );

                return result;
            }
        }
    }

    let result = match hole {
        Stmt::Jump(id, _) => Stmt::Jump(*id, env.arena.alloc([symbol])),
        _ => Stmt::Ret(symbol),
    };

    // if the symbol is a function symbol, ensure it is properly specialized!
    let original = symbol;

    let opt_fn_var = Some(variable);

    // if this is a function symbol, ensure that it's properly specialized!
    specialize_symbol(
        env,
        procs,
        layout_cache,
        opt_fn_var,
        symbol,
        result,
        original,
    )
}

fn try_make_literal<'a>(can_expr: &roc_can::expr::Expr, layout: Layout<'a>) -> Option<Literal<'a>> {
    use roc_can::expr::Expr::*;

    match can_expr {
        Int(_, _, int_str, int, _bound) => {
            Some(make_num_literal(layout, int_str, IntOrFloatValue::Int(*int)).to_expr_literal())
        }

        Float(_, _, float_str, float, _bound) => Some(
            make_num_literal(layout, float_str, IntOrFloatValue::Float(*float)).to_expr_literal(),
        ),

        // TODO investigate lifetime trouble
        // Str(string) => Some(Literal::Str(env.arena.alloc(string))),
        Num(_, num_str, num, _bound) => {
            Some(make_num_literal(layout, num_str, IntOrFloatValue::Int(*num)).to_expr_literal())
        }
        _ => None,
    }
}

pub fn with_hole<'a>(
    env: &mut Env<'a, '_>,
    can_expr: roc_can::expr::Expr,
    variable: Variable,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    use roc_can::expr::Expr::*;

    let arena = env.arena;

    match can_expr {
        Int(_, _, int_str, int, _bound) => assign_num_literal_expr(
            env,
            layout_cache,
            assigned,
            variable,
            &int_str,
            IntOrFloatValue::Int(int),
            hole,
        ),

        Float(_, _, float_str, float, _bound) => assign_num_literal_expr(
            env,
            layout_cache,
            assigned,
            variable,
            &float_str,
            IntOrFloatValue::Float(float),
            hole,
        ),

        Num(_, num_str, num, _bound) => assign_num_literal_expr(
            env,
            layout_cache,
            assigned,
            variable,
            &num_str,
            IntOrFloatValue::Int(num),
            hole,
        ),

        Str(string) => Stmt::Let(
            assigned,
            Expr::Literal(Literal::Str(arena.alloc(string))),
            Layout::Builtin(Builtin::Str),
            hole,
        ),

        SingleQuote(_, _, character, _) => {
            let layout = layout_cache
                .from_var(env.arena, variable, env.subs)
                .unwrap();

            Stmt::Let(
                assigned,
                Expr::Literal(Literal::Int((character as i128).to_ne_bytes())),
                layout,
                hole,
            )
        }
        LetNonRec(def, cont) => from_can_let(
            env,
            procs,
            layout_cache,
            def,
            cont,
            variable,
            Some((assigned, hole)),
        ),
        LetRec(defs, cont, _cycle_mark) => {
            // because Roc is strict, only functions can be recursive!
            for def in defs.into_iter() {
                if let roc_can::pattern::Pattern::Identifier(symbol) = &def.loc_pattern.value {
                    if let Closure(closure_data) = def.loc_expr.value {
                        register_noncapturing_closure(env, procs, *symbol, closure_data);

                        continue;
                    }
                }
                unreachable!("recursive value does not have Identifier pattern")
            }

            with_hole(
                env,
                cont.value,
                variable,
                procs,
                layout_cache,
                assigned,
                hole,
            )
        }
        Var(mut symbol, _) => {
            // If this symbol is a raw value, find the real name we gave to its specialized usage.
            if let ReuseSymbol::Value(_symbol) = can_reuse_symbol(
                env,
                procs,
                &roc_can::expr::Expr::Var(symbol, variable),
                variable,
            ) {
                let real_symbol =
                    procs.get_or_insert_symbol_specialization(env, layout_cache, symbol, variable);
                symbol = real_symbol;
            }

            specialize_naked_symbol(env, variable, procs, layout_cache, assigned, hole, symbol)
        }
        AbilityMember(member, specialization_id, specialization_var) => {
            let specialization_symbol = late_resolve_ability_specialization(
                env,
                member,
                specialization_id,
                specialization_var,
            );

            specialize_naked_symbol(
                env,
                variable,
                procs,
                layout_cache,
                assigned,
                hole,
                specialization_symbol,
            )
        }
        Tag {
            tag_union_var: variant_var,
            name: tag_name,
            arguments: args,
            ..
        } => {
            let arena = env.arena;

            debug_assert!(!matches!(
                env.subs.get_content_without_compacting(variant_var),
                Content::Structure(FlatType::Func(_, _, _))
            ));
            convert_tag_union(
                env,
                variant_var,
                assigned,
                hole,
                tag_name,
                procs,
                layout_cache,
                args,
                arena,
            )
        }

        ZeroArgumentTag {
            variant_var: _,
            name: tag_name,
            ext_var,
            closure_name,
        } => {
            let arena = env.arena;

            let content = env.subs.get_content_without_compacting(variable);

            if let Content::Structure(FlatType::Func(arg_vars, _, ret_var)) = content {
                let ret_var = *ret_var;
                let arg_vars = *arg_vars;

                tag_union_to_function(
                    env,
                    arg_vars,
                    ret_var,
                    tag_name,
                    closure_name,
                    ext_var,
                    procs,
                    variable,
                    layout_cache,
                    assigned,
                    hole,
                )
            } else {
                convert_tag_union(
                    env,
                    variable,
                    assigned,
                    hole,
                    tag_name,
                    procs,
                    layout_cache,
                    std::vec::Vec::new(),
                    arena,
                )
            }
        }

        OpaqueRef { argument, .. } => {
            let (arg_var, loc_arg_expr) = *argument;

            match can_reuse_symbol(env, procs, &loc_arg_expr.value, arg_var) {
                // Opaques decay to their argument.
                ReuseSymbol::Value(symbol) => {
                    let real_name = procs.get_or_insert_symbol_specialization(
                        env,
                        layout_cache,
                        symbol,
                        arg_var,
                    );
                    let mut result = hole.clone();
                    substitute_in_exprs(arena, &mut result, assigned, real_name);
                    result
                }
                _ => with_hole(
                    env,
                    loc_arg_expr.value,
                    arg_var,
                    procs,
                    layout_cache,
                    assigned,
                    hole,
                ),
            }
        }

        Record {
            record_var,
            mut fields,
            ..
        } => {
            let sorted_fields_result = {
                let mut layout_env = layout::Env::from_components(
                    layout_cache,
                    env.subs,
                    env.arena,
                    env.target_info,
                );
                layout::sort_record_fields(&mut layout_env, record_var)
            };
            let sorted_fields = match sorted_fields_result {
                Ok(fields) => fields,
                Err(_) => return runtime_error(env, "Can't create record with improper layout"),
            };

            let mut field_symbols = Vec::with_capacity_in(fields.len(), env.arena);
            let mut can_fields = Vec::with_capacity_in(fields.len(), env.arena);

            #[allow(clippy::enum_variant_names)]
            enum Field {
                // TODO: rename this since it can handle unspecialized expressions now too
                FunctionOrUnspecialized(Symbol, Variable),
                ValueSymbol,
                Field(roc_can::expr::Field),
            }

            for (label, variable, _) in sorted_fields.into_iter() {
                // TODO how should function pointers be handled here?
                use ReuseSymbol::*;
                match fields.remove(&label) {
                    Some(field) => {
                        match can_reuse_symbol(env, procs, &field.loc_expr.value, field.var) {
                            Imported(symbol)
                            | LocalFunction(symbol)
                            | UnspecializedExpr(symbol) => {
                                field_symbols.push(symbol);
                                can_fields.push(Field::FunctionOrUnspecialized(symbol, variable));
                            }
                            Value(symbol) => {
                                let reusable = procs.get_or_insert_symbol_specialization(
                                    env,
                                    layout_cache,
                                    symbol,
                                    field.var,
                                );
                                field_symbols.push(reusable);
                                can_fields.push(Field::ValueSymbol);
                            }
                            NotASymbol => {
                                field_symbols.push(env.unique_symbol());
                                can_fields.push(Field::Field(field));
                            }
                        }
                    }
                    None => {
                        // this field was optional, but not given
                        continue;
                    }
                }
            }

            // creating a record from the var will unpack it if it's just a single field.
            let layout = match layout_cache.from_var(env.arena, record_var, env.subs) {
                Ok(layout) => layout,
                Err(_) => return runtime_error(env, "Can't create record with improper layout"),
            };

            let field_symbols = field_symbols.into_bump_slice();

            let mut stmt = if let [only_field] = field_symbols {
                let mut hole = hole.clone();
                substitute_in_exprs(env.arena, &mut hole, assigned, *only_field);
                hole
            } else {
                Stmt::Let(assigned, Expr::Struct(field_symbols), layout, hole)
            };

            for (opt_field, symbol) in can_fields.into_iter().rev().zip(field_symbols.iter().rev())
            {
                match opt_field {
                    Field::ValueSymbol => {
                        // this symbol is already defined; nothing to do
                    }
                    Field::FunctionOrUnspecialized(symbol, variable) => {
                        stmt = specialize_symbol(
                            env,
                            procs,
                            layout_cache,
                            Some(variable),
                            symbol,
                            stmt,
                            symbol,
                        );
                    }
                    Field::Field(field) => {
                        stmt = with_hole(
                            env,
                            field.loc_expr.value,
                            field.var,
                            procs,
                            layout_cache,
                            *symbol,
                            env.arena.alloc(stmt),
                        );
                    }
                }
            }

            stmt
        }

        EmptyRecord => let_empty_struct(assigned, hole),

        Expect { .. } => unreachable!("I think this is unreachable"),
        ExpectFx { .. } => unreachable!("I think this is unreachable"),
        Dbg { .. } => unreachable!("I think this is unreachable"),

        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            match (
                layout_cache.from_var(env.arena, branch_var, env.subs),
                layout_cache.from_var(env.arena, cond_var, env.subs),
            ) {
                (Ok(ret_layout), Ok(cond_layout)) => {
                    // if the hole is a return, then we don't need to merge the two
                    // branches together again, we can just immediately return
                    let is_terminated = matches!(hole, Stmt::Ret(_));

                    if is_terminated {
                        let terminator = hole;

                        let mut stmt = with_hole(
                            env,
                            final_else.value,
                            branch_var,
                            procs,
                            layout_cache,
                            assigned,
                            terminator,
                        );

                        for (loc_cond, loc_then) in branches.into_iter().rev() {
                            let branching_symbol = env.unique_symbol();

                            let then = with_hole(
                                env,
                                loc_then.value,
                                branch_var,
                                procs,
                                layout_cache,
                                assigned,
                                terminator,
                            );

                            stmt = cond(env, branching_symbol, cond_layout, then, stmt, ret_layout);

                            // add condition
                            stmt = with_hole(
                                env,
                                loc_cond.value,
                                cond_var,
                                procs,
                                layout_cache,
                                branching_symbol,
                                env.arena.alloc(stmt),
                            );
                        }
                        stmt
                    } else {
                        let assigned_in_jump = env.unique_symbol();
                        let id = JoinPointId(env.unique_symbol());

                        let terminator = env
                            .arena
                            .alloc(Stmt::Jump(id, env.arena.alloc([assigned_in_jump])));

                        let mut stmt = with_hole(
                            env,
                            final_else.value,
                            branch_var,
                            procs,
                            layout_cache,
                            assigned_in_jump,
                            terminator,
                        );

                        for (loc_cond, loc_then) in branches.into_iter().rev() {
                            let branching_symbol = possible_reuse_symbol_or_specialize(
                                env,
                                procs,
                                layout_cache,
                                &loc_cond.value,
                                cond_var,
                            );

                            let then = with_hole(
                                env,
                                loc_then.value,
                                branch_var,
                                procs,
                                layout_cache,
                                assigned_in_jump,
                                terminator,
                            );

                            stmt = cond(env, branching_symbol, cond_layout, then, stmt, ret_layout);

                            // add condition
                            stmt = assign_to_symbol(
                                env,
                                procs,
                                layout_cache,
                                cond_var,
                                loc_cond,
                                branching_symbol,
                                stmt,
                            );
                        }

                        let layout = layout_cache
                            .from_var(env.arena, branch_var, env.subs)
                            .unwrap_or_else(|err| {
                                panic!("TODO turn fn_var into a RuntimeError {:?}", err)
                            });

                        let param = Param {
                            symbol: assigned,
                            layout,
                            borrow: false,
                        };

                        Stmt::Join {
                            id,
                            parameters: env.arena.alloc([param]),
                            remainder: env.arena.alloc(stmt),
                            body: hole,
                        }
                    }
                }
                (Err(_), _) => runtime_error(env, "invalid ret_layout"),
                (_, Err(_)) => runtime_error(env, "invalid cond_layout"),
            }
        }

        When {
            cond_var,
            expr_var,
            region: _,
            loc_cond,
            branches,
            branches_cond_var: _,
            exhaustive,
        } => {
            let cond_symbol = possible_reuse_symbol_or_specialize(
                env,
                procs,
                layout_cache,
                &loc_cond.value,
                cond_var,
            );

            let id = JoinPointId(env.unique_symbol());

            let mut stmt = from_can_when(
                env,
                cond_var,
                expr_var,
                cond_symbol,
                branches,
                exhaustive,
                layout_cache,
                procs,
                Some(id),
            );

            // define the `when` condition
            stmt = assign_to_symbol(
                env,
                procs,
                layout_cache,
                cond_var,
                *loc_cond,
                cond_symbol,
                stmt,
            );

            let layout = layout_cache
                .from_var(env.arena, expr_var, env.subs)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

            let param = Param {
                symbol: assigned,
                layout,
                borrow: false,
            };

            Stmt::Join {
                id,
                parameters: env.arena.alloc([param]),
                remainder: env.arena.alloc(stmt),
                body: env.arena.alloc(hole),
            }
        }

        List {
            loc_elems,
            elem_var,
            ..
        } if loc_elems.is_empty() => {
            // because an empty list has an unknown element type, it is handled differently
            let opt_elem_layout = layout_cache.from_var(env.arena, elem_var, env.subs);

            match opt_elem_layout {
                Ok(elem_layout) => {
                    let expr = Expr::EmptyArray;
                    Stmt::Let(
                        assigned,
                        expr,
                        Layout::Builtin(Builtin::List(env.arena.alloc(elem_layout))),
                        hole,
                    )
                }
                Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                    let expr = Expr::EmptyArray;
                    Stmt::Let(
                        assigned,
                        expr,
                        Layout::Builtin(Builtin::List(&Layout::VOID)),
                        hole,
                    )
                }
                Err(LayoutProblem::Erroneous) => panic!("list element is error type"),
            }
        }

        List {
            elem_var,
            loc_elems,
        } => {
            let mut arg_symbols = Vec::with_capacity_in(loc_elems.len(), env.arena);
            let mut elements = Vec::with_capacity_in(loc_elems.len(), env.arena);

            let mut symbol_exprs = Vec::with_capacity_in(loc_elems.len(), env.arena);

            let elem_layout = layout_cache
                .from_var(env.arena, elem_var, env.subs)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

            for arg_expr in loc_elems.into_iter() {
                if let Some(literal) = try_make_literal(&arg_expr.value, elem_layout) {
                    elements.push(ListLiteralElement::Literal(literal));
                } else {
                    let symbol = possible_reuse_symbol_or_specialize(
                        env,
                        procs,
                        layout_cache,
                        &arg_expr.value,
                        elem_var,
                    );

                    elements.push(ListLiteralElement::Symbol(symbol));
                    arg_symbols.push(symbol);
                    symbol_exprs.push(arg_expr);
                }
            }
            let arg_symbols = arg_symbols.into_bump_slice();

            let expr = Expr::Array {
                elem_layout,
                elems: elements.into_bump_slice(),
            };

            let stmt = Stmt::Let(
                assigned,
                expr,
                Layout::Builtin(Builtin::List(env.arena.alloc(elem_layout))),
                hole,
            );

            let iter = symbol_exprs
                .into_iter()
                .rev()
                .map(|e| (elem_var, e))
                .zip(arg_symbols.iter().rev());

            assign_to_symbols(env, procs, layout_cache, iter, stmt)
        }

        Access {
            record_var,
            field_var,
            field,
            loc_expr,
            ..
        } => {
            let sorted_fields_result = {
                let mut layout_env = layout::Env::from_components(
                    layout_cache,
                    env.subs,
                    env.arena,
                    env.target_info,
                );
                layout::sort_record_fields(&mut layout_env, record_var)
            };
            let sorted_fields = match sorted_fields_result {
                Ok(fields) => fields,
                Err(_) => return runtime_error(env, "Can't access record with improper layout"),
            };

            let mut index = None;
            let mut field_layouts = Vec::with_capacity_in(sorted_fields.len(), env.arena);

            let mut current = 0;
            for (label, _, opt_field_layout) in sorted_fields.into_iter() {
                match opt_field_layout {
                    Err(_) => {
                        // this was an optional field, and now does not exist!
                        // do not increment `current`!
                    }
                    Ok(field_layout) => {
                        field_layouts.push(field_layout);

                        if label == field {
                            index = Some(current);
                        }

                        current += 1;
                    }
                }
            }

            let record_symbol = possible_reuse_symbol_or_specialize(
                env,
                procs,
                layout_cache,
                &loc_expr.value,
                record_var,
            );

            let mut stmt = match field_layouts.as_slice() {
                [_] => {
                    let mut hole = hole.clone();
                    substitute_in_exprs(env.arena, &mut hole, assigned, record_symbol);

                    hole
                }
                _ => {
                    let expr = Expr::StructAtIndex {
                        index: index.expect("field not in its own type") as u64,
                        field_layouts: field_layouts.into_bump_slice(),
                        structure: record_symbol,
                    };

                    let layout = layout_cache
                        .from_var(env.arena, field_var, env.subs)
                        .unwrap_or_else(|err| {
                            panic!("TODO turn fn_var into a RuntimeError {:?}", err)
                        });

                    Stmt::Let(assigned, expr, layout, hole)
                }
            };

            stmt = assign_to_symbol(
                env,
                procs,
                layout_cache,
                record_var,
                *loc_expr,
                record_symbol,
                stmt,
            );

            stmt
        }

        Accessor(accessor_data) => {
            let field_var = accessor_data.field_var;
            let fresh_record_symbol = env.unique_symbol();

            let ClosureData {
                name,
                function_type,
                arguments,
                loc_body,
                ..
            } = accessor_data.to_closure_data(fresh_record_symbol);

            match procs.insert_anonymous(
                env,
                LambdaName::no_niche(name),
                function_type,
                arguments,
                *loc_body,
                CapturedSymbols::None,
                field_var,
                layout_cache,
            ) {
                Ok(_) => {
                    let raw_layout = return_on_layout_error!(
                        env,
                        layout_cache.raw_from_var(env.arena, function_type, env.subs),
                        "Expr::Accessor"
                    );

                    match raw_layout {
                        RawFunctionLayout::Function(_, lambda_set, _) => {
                            let lambda_name =
                                find_lambda_name(env, layout_cache, lambda_set, name, &[]);
                            construct_closure_data(
                                env,
                                procs,
                                layout_cache,
                                lambda_set,
                                lambda_name,
                                &[],
                                assigned,
                                hole,
                            )
                        }
                        RawFunctionLayout::ZeroArgumentThunk(_) => unreachable!(),
                    }
                }

                Err(_error) => runtime_error(
                    env,
                    "TODO convert anonymous function error to a RuntimeError string",
                ),
            }
        }

        OpaqueWrapFunction(wrap_fn_data) => {
            let opaque_var = wrap_fn_data.opaque_var;
            let arg_symbol = env.unique_symbol();

            let ClosureData {
                name,
                function_type,
                arguments,
                loc_body,
                ..
            } = wrap_fn_data.to_closure_data(arg_symbol);

            match procs.insert_anonymous(
                env,
                LambdaName::no_niche(name),
                function_type,
                arguments,
                *loc_body,
                CapturedSymbols::None,
                opaque_var,
                layout_cache,
            ) {
                Ok(_) => {
                    let raw_layout = return_on_layout_error!(
                        env,
                        layout_cache.raw_from_var(env.arena, function_type, env.subs),
                        "Expr::OpaqueWrapFunction"
                    );

                    match raw_layout {
                        RawFunctionLayout::Function(_, lambda_set, _) => {
                            let lambda_name =
                                find_lambda_name(env, layout_cache, lambda_set, name, &[]);
                            construct_closure_data(
                                env,
                                procs,
                                layout_cache,
                                lambda_set,
                                lambda_name,
                                &[],
                                assigned,
                                hole,
                            )
                        }
                        RawFunctionLayout::ZeroArgumentThunk(_) => {
                            internal_error!("should not be a thunk!")
                        }
                    }
                }

                Err(_error) => runtime_error(
                    env,
                    "TODO convert anonymous function error to a RuntimeError string",
                ),
            }
        }

        Update {
            record_var,
            symbol: structure,
            updates,
            ..
        } => {
            use FieldType::*;

            enum FieldType<'a> {
                CopyExisting(u64),
                UpdateExisting(&'a roc_can::expr::Field),
            }

            // Strategy: turn a record update into the creation of a new record.
            // This has the benefit that we don't need to do anything special for reference
            // counting

            let sorted_fields_result = {
                let mut layout_env = layout::Env::from_components(
                    layout_cache,
                    env.subs,
                    env.arena,
                    env.target_info,
                );
                layout::sort_record_fields(&mut layout_env, record_var)
            };

            let sorted_fields = match sorted_fields_result {
                Ok(fields) => fields,
                Err(_) => return runtime_error(env, "Can't update record with improper layout"),
            };

            let mut field_layouts = Vec::with_capacity_in(sorted_fields.len(), env.arena);

            let mut symbols = Vec::with_capacity_in(sorted_fields.len(), env.arena);
            let mut fields = Vec::with_capacity_in(sorted_fields.len(), env.arena);

            let mut current = 0;
            for (label, _, opt_field_layout) in sorted_fields.into_iter() {
                match opt_field_layout {
                    Err(_) => {
                        debug_assert!(!updates.contains_key(&label));
                        // this was an optional field, and now does not exist!
                        // do not increment `current`!
                    }
                    Ok(field_layout) => {
                        field_layouts.push(field_layout);

                        if let Some(field) = updates.get(&label) {
                            let field_symbol = possible_reuse_symbol_or_specialize(
                                env,
                                procs,
                                layout_cache,
                                &field.loc_expr.value,
                                field.var,
                            );

                            fields.push(UpdateExisting(field));
                            symbols.push(field_symbol);
                        } else {
                            fields.push(CopyExisting(current));
                            symbols.push(env.unique_symbol());
                        }

                        current += 1;
                    }
                }
            }
            let symbols = symbols.into_bump_slice();

            let record_layout = layout_cache
                .from_var(env.arena, record_var, env.subs)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

            let field_layouts = match &record_layout {
                Layout::Struct { field_layouts, .. } => *field_layouts,
                other => arena.alloc([*other]),
            };

            debug_assert_eq!(field_layouts.len(), symbols.len());
            debug_assert_eq!(fields.len(), symbols.len());

            if symbols.len() == 1 {
                // TODO we can probably special-case this more, skippiing the generation of
                // UpdateExisting
                let mut stmt = hole.clone();

                let what_to_do = &fields[0];

                match what_to_do {
                    UpdateExisting(field) => {
                        substitute_in_exprs(env.arena, &mut stmt, assigned, symbols[0]);

                        stmt = assign_to_symbol(
                            env,
                            procs,
                            layout_cache,
                            field.var,
                            *field.loc_expr.clone(),
                            symbols[0],
                            stmt,
                        );
                    }
                    CopyExisting(_) => {
                        unreachable!(
                            r"when a record has just one field and is updated, it must update that one field"
                        );
                    }
                }

                stmt
            } else {
                let expr = Expr::Struct(symbols);
                let mut stmt = Stmt::Let(assigned, expr, record_layout, hole);

                let it = field_layouts.iter().zip(symbols.iter()).zip(fields);

                for ((field_layout, symbol), what_to_do) in it {
                    match what_to_do {
                        UpdateExisting(field) => {
                            stmt = assign_to_symbol(
                                env,
                                procs,
                                layout_cache,
                                field.var,
                                *field.loc_expr.clone(),
                                *symbol,
                                stmt,
                            );
                        }
                        CopyExisting(index) => {
                            let record_needs_specialization =
                                procs.ability_member_aliases.get(structure).is_some();
                            let specialized_structure_sym = if record_needs_specialization {
                                // We need to specialize the record now; create a new one for it.
                                // TODO: reuse this symbol for all updates
                                env.unique_symbol()
                            } else {
                                // The record is already good.
                                structure
                            };

                            let access_expr = Expr::StructAtIndex {
                                structure: specialized_structure_sym,
                                index,
                                field_layouts,
                            };
                            stmt =
                                Stmt::Let(*symbol, access_expr, *field_layout, arena.alloc(stmt));

                            // If the records needs specialization or it's a thunk, we need to
                            // create the specialized definition or force the thunk, respectively.
                            // Both cases are handled below.
                            if record_needs_specialization || procs.is_module_thunk(structure) {
                                stmt = specialize_symbol(
                                    env,
                                    procs,
                                    layout_cache,
                                    Some(record_var),
                                    specialized_structure_sym,
                                    stmt,
                                    structure,
                                );
                            }
                        }
                    }
                }
                stmt
            }
        }

        Closure(ClosureData {
            function_type,
            return_type,
            name,
            arguments,
            captured_symbols,
            loc_body: boxed_body,
            ..
        }) => {
            let loc_body = *boxed_body;

            let raw = layout_cache.raw_from_var(env.arena, function_type, env.subs);

            match return_on_layout_error!(env, raw, "Expr::Closure") {
                RawFunctionLayout::ZeroArgumentThunk(_) => {
                    unreachable!("a closure syntactically always must have at least one argument")
                }
                RawFunctionLayout::Function(_argument_layouts, lambda_set, _ret_layout) => {
                    let mut captured_symbols = Vec::from_iter_in(captured_symbols, env.arena);
                    captured_symbols.sort();
                    let captured_symbols = captured_symbols.into_bump_slice();

                    let symbols =
                        Vec::from_iter_in(captured_symbols.iter(), env.arena).into_bump_slice();

                    let lambda_name = find_lambda_name(
                        env,
                        layout_cache,
                        lambda_set,
                        name,
                        symbols.iter().copied(),
                    );

                    let inserted = procs.insert_anonymous(
                        env,
                        lambda_name,
                        function_type,
                        arguments,
                        loc_body,
                        CapturedSymbols::Captured(captured_symbols),
                        return_type,
                        layout_cache,
                    );

                    if let Err(e) = inserted {
                        return runtime_error(
                            env,
                            env.arena.alloc(format!("RuntimeError: {:?}", e,)),
                        );
                    } else {
                        drop(inserted);
                    }

                    // define the closure data

                    construct_closure_data(
                        env,
                        procs,
                        layout_cache,
                        lambda_set,
                        lambda_name,
                        symbols.iter().copied(),
                        assigned,
                        hole,
                    )
                }
            }
        }

        Call(boxed, loc_args, _) => {
            let (fn_var, loc_expr, _lambda_set_var, _ret_var) = *boxed;

            // even if a call looks like it's by name, it may in fact be by-pointer.
            // E.g. in `(\f, x -> f x)` the call is in fact by pointer.
            // So we check the function name against the list of partial procedures,
            // the procedures that we have lifted to the top-level and can call by name
            // if it's in there, it's a call by name, otherwise it's a call by pointer
            let is_known = |key| {
                // a proc in this module, or an imported symbol
                procs.partial_procs.contains_key(key)
                    || (env.is_imported_symbol(key) && !procs.is_imported_module_thunk(key))
            };

            match loc_expr.value {
                roc_can::expr::Expr::Var(proc_name, _) if is_known(proc_name) => {
                    // a call by a known name
                    call_by_name(
                        env,
                        procs,
                        fn_var,
                        proc_name,
                        loc_args,
                        layout_cache,
                        assigned,
                        hole,
                    )
                }
                roc_can::expr::Expr::AbilityMember(member, specialization_id, _) => {
                    let specialization_proc_name =
                        late_resolve_ability_specialization(env, member, specialization_id, fn_var);

                    call_by_name(
                        env,
                        procs,
                        fn_var,
                        specialization_proc_name,
                        loc_args,
                        layout_cache,
                        assigned,
                        hole,
                    )
                }
                _ => {
                    // Call by pointer - the closure was anonymous, e.g.
                    //
                    // ((\a -> a) 5)
                    //
                    // It might even be the anonymous result of a conditional:
                    //
                    // ((if x > 0 then \a -> a else \_ -> 0) 5)
                    //
                    // It could be named too:
                    //
                    // ((if x > 0 then foo else bar) 5)
                    //
                    // also this occurs for functions passed in as arguments, e.g.
                    //
                    // (\f, x -> f x)

                    let arg_symbols = Vec::from_iter_in(
                        loc_args.iter().map(|(var, arg_expr)| {
                            possible_reuse_symbol_or_specialize(
                                env,
                                procs,
                                layout_cache,
                                &arg_expr.value,
                                *var,
                            )
                        }),
                        arena,
                    )
                    .into_bump_slice();

                    let full_layout = return_on_layout_error!(
                        env,
                        layout_cache.raw_from_var(env.arena, fn_var, env.subs),
                        "Expr::Call"
                    );

                    // if the function expression (loc_expr) is already a symbol,
                    // re-use that symbol, and don't define its value again
                    let mut result;
                    use ReuseSymbol::*;
                    match can_reuse_symbol(env, procs, &loc_expr.value, fn_var) {
                        LocalFunction(_) => {
                            unreachable!("if this was known to be a function, we would not be here")
                        }
                        Imported(thunk_name) => {
                            debug_assert!(procs.is_imported_module_thunk(thunk_name));

                            add_needed_external(
                                procs,
                                env,
                                fn_var,
                                LambdaName::no_niche(thunk_name),
                            );

                            let function_symbol = env.unique_symbol();

                            match full_layout {
                                RawFunctionLayout::Function(
                                    arg_layouts,
                                    lambda_set,
                                    ret_layout,
                                ) => {
                                    let closure_data_symbol = function_symbol;

                                    result = match_on_lambda_set(
                                        env,
                                        layout_cache,
                                        procs,
                                        lambda_set,
                                        closure_data_symbol,
                                        arg_symbols,
                                        arg_layouts,
                                        ret_layout,
                                        assigned,
                                        hole,
                                    );

                                    result = force_thunk(
                                        env,
                                        thunk_name,
                                        Layout::LambdaSet(lambda_set),
                                        function_symbol,
                                        env.arena.alloc(result),
                                    );
                                }
                                RawFunctionLayout::ZeroArgumentThunk(_) => {
                                    unreachable!("calling a non-closure layout")
                                }
                            }
                        }
                        Value(function_symbol) => {
                            let function_symbol = procs.get_or_insert_symbol_specialization(
                                env,
                                layout_cache,
                                function_symbol,
                                fn_var,
                            );

                            match full_layout {
                                RawFunctionLayout::Function(
                                    arg_layouts,
                                    lambda_set,
                                    ret_layout,
                                ) => {
                                    let closure_data_symbol = function_symbol;

                                    result = match_on_lambda_set(
                                        env,
                                        layout_cache,
                                        procs,
                                        lambda_set,
                                        closure_data_symbol,
                                        arg_symbols,
                                        arg_layouts,
                                        ret_layout,
                                        assigned,
                                        hole,
                                    );
                                }
                                RawFunctionLayout::ZeroArgumentThunk(_) => {
                                    unreachable!("calling a non-closure layout")
                                }
                            }
                        }
                        UnspecializedExpr(symbol) => {
                            match procs.ability_member_aliases.get(symbol).unwrap() {
                                &self::AbilityMember(member) => {
                                    let resolved_proc = resolve_ability_specialization(env.home, env.subs, &env.abilities, member, fn_var)
                                            .expect("Recorded as an ability member, but it doesn't have a specialization");

                                    let resolved_proc = match resolved_proc {
                                        Resolved::Specialization(symbol) => symbol,
                                        Resolved::NeedsGenerated(_) => {
                                            todo_abilities!("Generate impls for structural types")
                                        }
                                    };

                                    // a call by a known name
                                    return call_by_name(
                                        env,
                                        procs,
                                        fn_var,
                                        resolved_proc,
                                        loc_args,
                                        layout_cache,
                                        assigned,
                                        hole,
                                    );
                                }
                            }
                        }
                        NotASymbol => {
                            // the expression is not a symbol. That means it's an expression
                            // evaluating to a function value.

                            match full_layout {
                                RawFunctionLayout::Function(
                                    arg_layouts,
                                    lambda_set,
                                    ret_layout,
                                ) => {
                                    let closure_data_symbol = env.unique_symbol();

                                    result = match_on_lambda_set(
                                        env,
                                        layout_cache,
                                        procs,
                                        lambda_set,
                                        closure_data_symbol,
                                        arg_symbols,
                                        arg_layouts,
                                        ret_layout,
                                        assigned,
                                        hole,
                                    );

                                    result = with_hole(
                                        env,
                                        loc_expr.value,
                                        fn_var,
                                        procs,
                                        layout_cache,
                                        closure_data_symbol,
                                        env.arena.alloc(result),
                                    );
                                }
                                RawFunctionLayout::ZeroArgumentThunk(_) => {
                                    unreachable!(
                                        "{:?} cannot be called in the source language",
                                        full_layout
                                    )
                                }
                            }
                        }
                    }
                    let iter = loc_args.into_iter().rev().zip(arg_symbols.iter().rev());
                    assign_to_symbols(env, procs, layout_cache, iter, result)
                }
            }
        }

        ForeignCall {
            foreign_symbol,
            args,
            ret_var,
        } => {
            let mut arg_symbols = Vec::with_capacity_in(args.len(), env.arena);

            for (var, arg_expr) in args.iter() {
                arg_symbols.push(possible_reuse_symbol_or_specialize(
                    env,
                    procs,
                    layout_cache,
                    arg_expr,
                    *var,
                ));
            }
            let arg_symbols = arg_symbols.into_bump_slice();

            // layout of the return type
            let layout = return_on_layout_error!(
                env,
                layout_cache.from_var(env.arena, ret_var, env.subs),
                "ForeignCall"
            );

            let call = self::Call {
                call_type: CallType::Foreign {
                    foreign_symbol,
                    ret_layout: env.arena.alloc(layout),
                },
                arguments: arg_symbols,
            };

            let result = build_call(env, call, assigned, layout, hole);

            let iter = args
                .into_iter()
                .rev()
                .map(|(a, b)| (a, Loc::at_zero(b)))
                .zip(arg_symbols.iter().rev());
            assign_to_symbols(env, procs, layout_cache, iter, result)
        }

        RunLowLevel { op, args, ret_var } => {
            let mut arg_symbols = Vec::with_capacity_in(args.len(), env.arena);

            for (var, arg_expr) in args.iter() {
                arg_symbols.push(possible_reuse_symbol_or_specialize(
                    env,
                    procs,
                    layout_cache,
                    arg_expr,
                    *var,
                ));
            }
            let arg_symbols = arg_symbols.into_bump_slice();

            // layout of the return type
            let layout = return_on_layout_error!(
                env,
                layout_cache.from_var(env.arena, ret_var, env.subs),
                "RunLowLevel"
            );

            macro_rules! match_on_closure_argument {
                ( $ho:ident, [$($x:ident),* $(,)?]) => {{
                    let closure_index = op.function_argument_position();
                    let closure_data_symbol = arg_symbols[closure_index];
                    let closure_data_var = args[closure_index].0;

                    let closure_data_layout = return_on_layout_error!(
                        env,
                        layout_cache.raw_from_var(env.arena, closure_data_var, env.subs),
                        "match_on_closure_argument"
                    );

                    // NB: I don't think the top_level here can have a captures niche?
                    let top_level_capture_niche = CapturesNiche::no_niche();
                    let top_level = ProcLayout::from_raw(env.arena, &layout_cache.interner, closure_data_layout, top_level_capture_niche);

                    let arena = env.arena;

                    let arg_layouts = top_level.arguments;
                    let ret_layout = top_level.result;

                    match closure_data_layout {
                        RawFunctionLayout::Function(_, lambda_set, _) =>  {
                            lowlevel_match_on_lambda_set(
                                env,
                                layout_cache,
                                lambda_set,
                                op,
                                closure_data_symbol,
                                |(top_level_function, closure_data, closure_env_layout,  specialization_id, update_mode)| {
                                    let passed_function = PassedFunction {
                                        name: top_level_function,
                                        captured_environment: closure_data_symbol,
                                        owns_captured_environment: false,
                                        specialization_id,
                                        argument_layouts: arg_layouts,
                                        return_layout: ret_layout,
                                    };


                                    let higher_order = HigherOrderLowLevel {
                                        op: crate::low_level::HigherOrder::$ho { $($x,)* },
                                        closure_env_layout,
                                        update_mode,
                                        passed_function,
                                    };

                                    self::Call {
                                        call_type: CallType::HigherOrder(arena.alloc(higher_order)),
                                        arguments: arena.alloc([$($x,)* top_level_function.name(), closure_data]),
                                    }
                                },
                                layout,
                                assigned,
                                hole,
                            )
                        }
                        RawFunctionLayout::ZeroArgumentThunk(_) => unreachable!("match_on_closure_argument received a zero-argument thunk"),
                    }
                }};
            }

            use LowLevel::*;
            match op {
                ListMap => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListMap, [xs])
                }
                ListSortWith => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListSortWith, [xs])
                }
                ListMap2 => {
                    debug_assert_eq!(arg_symbols.len(), 3);

                    let xs = arg_symbols[0];
                    let ys = arg_symbols[1];

                    match_on_closure_argument!(ListMap2, [xs, ys])
                }
                ListMap3 => {
                    debug_assert_eq!(arg_symbols.len(), 4);

                    let xs = arg_symbols[0];
                    let ys = arg_symbols[1];
                    let zs = arg_symbols[2];

                    match_on_closure_argument!(ListMap3, [xs, ys, zs])
                }
                ListMap4 => {
                    debug_assert_eq!(arg_symbols.len(), 5);

                    let xs = arg_symbols[0];
                    let ys = arg_symbols[1];
                    let zs = arg_symbols[2];
                    let ws = arg_symbols[3];

                    match_on_closure_argument!(ListMap4, [xs, ys, zs, ws])
                }
                BoxExpr => {
                    debug_assert_eq!(arg_symbols.len(), 1);
                    let x = arg_symbols[0];

                    Stmt::Let(assigned, Expr::ExprBox { symbol: x }, layout, hole)
                }
                UnboxExpr => {
                    debug_assert_eq!(arg_symbols.len(), 1);
                    let x = arg_symbols[0];

                    Stmt::Let(assigned, Expr::ExprUnbox { symbol: x }, layout, hole)
                }
                _ => {
                    let call = self::Call {
                        call_type: CallType::LowLevel {
                            op,
                            update_mode: env.next_update_mode_id(),
                        },
                        arguments: arg_symbols,
                    };

                    let result = build_call(env, call, assigned, layout, hole);

                    let iter = args
                        .into_iter()
                        .rev()
                        .map(|(a, b)| (a, Loc::at_zero(b)))
                        .zip(arg_symbols.iter().rev());
                    assign_to_symbols(env, procs, layout_cache, iter, result)
                }
            }
        }
        TypedHole(_) => runtime_error(env, "Hit a blank"),
        RuntimeError(e) => runtime_error(env, env.arena.alloc(e.runtime_message())),
        Crash { msg, ret_var: _ } => {
            let msg_sym = possible_reuse_symbol_or_specialize(
                env,
                procs,
                layout_cache,
                &msg.value,
                Variable::STR,
            );
            let stmt = Stmt::Crash(msg_sym, CrashTag::User);

            assign_to_symbol(env, procs, layout_cache, Variable::STR, *msg, msg_sym, stmt)
        }
    }
}

#[inline(always)]
fn late_resolve_ability_specialization<'a>(
    env: &mut Env<'a, '_>,
    member: Symbol,
    specialization_id: Option<SpecializationId>,
    specialization_var: Variable,
) -> Symbol {
    let opt_resolved = specialization_id.and_then(|id| {
        env.abilities
            .with_module_abilities_store(env.home, |store| store.get_resolved(id))
    });

    if let Some(spec_symbol) = opt_resolved {
        // Fast path: specialization is monomorphic, was found during solving.
        spec_symbol
    } else if let Content::Structure(FlatType::Func(_, lambda_set, _)) =
        env.subs.get_content_without_compacting(specialization_var)
    {
        // Fast path: the member is a function, so the lambda set will tell us the
        // specialization.
        use roc_types::subs::LambdaSet;
        let LambdaSet {
            solved,
            unspecialized,
            recursion_var: _,
            ambient_function: _,
        } = env.subs.get_lambda_set(*lambda_set);

        debug_assert!(unspecialized.is_empty());
        let mut iter_lambda_set = solved.iter_all();
        debug_assert_eq!(iter_lambda_set.len(), 1);
        let spec_symbol_index = iter_lambda_set.next().unwrap().0;
        env.subs[spec_symbol_index]
    } else {
        // Otherwise, resolve by checking the able var.
        let specialization = resolve_ability_specialization(
            env.home,
            env.subs,
            &env.abilities,
            member,
            specialization_var,
        )
        .expect("Ability specialization is unknown - code generation cannot proceed!");

        match specialization {
            Resolved::Specialization(symbol) => symbol,
            Resolved::NeedsGenerated(var) => {
                let derive_key = roc_derive_key::Derived::builtin(
                    member.try_into().expect("derived symbols must be builtins"),
                    env.subs,
                    var,
                )
                .expect("specialization var not derivable!");

                match derive_key {
                    roc_derive_key::Derived::Immediate(imm)
                    | roc_derive_key::Derived::SingleLambdaSetImmediate(imm) => {
                        // The immediate may be an ability member itself, so it must be resolved!
                        late_resolve_ability_specialization(env, imm, None, specialization_var)
                    }
                    roc_derive_key::Derived::Key(derive_key) => {
                        let mut derived_module = env
                            .derived_module
                            .lock()
                            .expect("derived module unavailable");

                        derived_module
                            .get_or_insert(env.exposed_by_module, derive_key)
                            .0
                    }
                }
            }
        }
    }
}

fn find_lambda_name<'a, I>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    lambda_set: LambdaSet<'a>,
    function_name: Symbol,
    captures: I,
) -> LambdaName<'a>
where
    I: IntoIterator<Item = &'a (Symbol, Variable)>,
{
    let this_function_captures_layouts = captures
        .into_iter()
        .map(|(_, var)| {
            layout_cache
                .from_var(env.arena, *var, env.subs)
                .expect("layout problem for capture")
        })
        .collect_in::<Vec<_>>(env.arena);
    lambda_set.find_lambda_name(
        &layout_cache.interner,
        function_name,
        &this_function_captures_layouts,
    )
}

#[allow(clippy::too_many_arguments)]
fn construct_closure_data<'a, I>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    lambda_set: LambdaSet<'a>,
    name: LambdaName<'a>,
    symbols: I,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a>
where
    I: IntoIterator<Item = &'a (Symbol, Variable)>,
    I::IntoIter: ExactSizeIterator,
{
    let lambda_set_layout = Layout::LambdaSet(lambda_set);
    let symbols = symbols.into_iter();

    let result = match lambda_set.layout_for_member_with_lambda_name(&layout_cache.interner, name) {
        ClosureRepresentation::Union {
            tag_id,
            alphabetic_order_fields: field_layouts,
            union_layout,
            closure_name: _,
        } => {
            // captured variables are in symbol-alphabetic order, but now we want
            // them ordered by their alignment requirements
            let mut combined = Vec::with_capacity_in(symbols.len(), env.arena);
            for ((symbol, _variable), layout) in symbols.zip(field_layouts.iter()) {
                combined.push((*symbol, layout))
            }

            let ptr_bytes = env.target_info;

            combined.sort_by(|(_, layout1), (_, layout2)| {
                let size1 = layout1.alignment_bytes(&layout_cache.interner, ptr_bytes);
                let size2 = layout2.alignment_bytes(&layout_cache.interner, ptr_bytes);

                size2.cmp(&size1)
            });

            let symbols =
                Vec::from_iter_in(combined.iter().map(|(a, _)| *a), env.arena).into_bump_slice();

            let expr = Expr::Tag {
                tag_id,
                tag_layout: union_layout,
                arguments: symbols,
            };

            Stmt::Let(assigned, expr, lambda_set_layout, env.arena.alloc(hole))
        }
        ClosureRepresentation::AlphabeticOrderStruct(field_layouts) => {
            debug_assert_eq!(field_layouts.len(), symbols.len());

            // captured variables are in symbol-alphabetic order, but now we want
            // them ordered by their alignment requirements
            let mut combined = Vec::with_capacity_in(symbols.len(), env.arena);
            for ((symbol, _variable), layout) in symbols.zip(field_layouts.iter()) {
                combined.push((*symbol, layout))
            }

            let ptr_bytes = env.target_info;

            combined.sort_by(|(_, layout1), (_, layout2)| {
                let size1 = layout1.alignment_bytes(&layout_cache.interner, ptr_bytes);
                let size2 = layout2.alignment_bytes(&layout_cache.interner, ptr_bytes);

                size2.cmp(&size1)
            });

            let symbols =
                Vec::from_iter_in(combined.iter().map(|(a, _)| *a), env.arena).into_bump_slice();
            let field_layouts =
                Vec::from_iter_in(combined.iter().map(|(_, b)| **b), env.arena).into_bump_slice();

            debug_assert_eq!(
                Layout::struct_no_name_order(field_layouts),
                lambda_set.runtime_representation(&layout_cache.interner)
            );

            let expr = Expr::Struct(symbols);

            Stmt::Let(assigned, expr, lambda_set_layout, hole)
        }
        ClosureRepresentation::UnwrappedCapture(_layout) => {
            debug_assert_eq!(symbols.len(), 1);

            let mut symbols = symbols;
            let (captured_symbol, captured_var) = symbols.next().unwrap();

            let captured_symbol = procs.get_or_insert_symbol_specialization(
                env,
                layout_cache,
                *captured_symbol,
                *captured_var,
            );

            // The capture set is unwrapped, so just replaced the assigned capture symbol with the
            // only capture.
            let mut hole = hole.clone();
            substitute_in_exprs(env.arena, &mut hole, assigned, captured_symbol);
            hole
        }
        ClosureRepresentation::EnumDispatch(repr) => match repr {
            EnumDispatch::Bool => {
                debug_assert_eq!(symbols.len(), 0);

                debug_assert_eq!(lambda_set.len(), 2);
                let tag_id = name.name() != lambda_set.iter_set().next().unwrap().name();
                let expr = Expr::Literal(Literal::Bool(tag_id));

                Stmt::Let(assigned, expr, lambda_set_layout, hole)
            }
            EnumDispatch::U8 => {
                debug_assert_eq!(symbols.len(), 0);

                debug_assert!(lambda_set.len() > 2);
                let tag_id = lambda_set
                    .iter_set()
                    .position(|s| s.name() == name.name())
                    .unwrap() as u8;

                let expr = Expr::Literal(Literal::Byte(tag_id));

                Stmt::Let(assigned, expr, lambda_set_layout, hole)
            }
        },
    };

    result
}

#[allow(clippy::too_many_arguments)]
fn convert_tag_union<'a>(
    env: &mut Env<'a, '_>,
    variant_var: Variable,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
    tag_name: TagName,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    args: std::vec::Vec<(Variable, Loc<roc_can::expr::Expr>)>,
    arena: &'a Bump,
) -> Stmt<'a> {
    use crate::layout::UnionVariant::*;
    let res_variant = {
        let mut layout_env =
            layout::Env::from_components(layout_cache, env.subs, env.arena, env.target_info);
        crate::layout::union_sorted_tags(&mut layout_env, variant_var)
    };
    let variant = match res_variant {
        Ok(cached) => cached,
        Err(LayoutProblem::UnresolvedTypeVar(_)) => {
            return runtime_error(
                env,
                env.arena.alloc(format!(
                    "Unresolved type variable for tag {}",
                    tag_name.0.as_str()
                )),
            )
        }
        Err(LayoutProblem::Erroneous) => {
            return runtime_error(
                env,
                env.arena.alloc(format!(
                    "Tag {} was part of a type error!",
                    tag_name.0.as_str()
                )),
            );
        }
    };

    match variant {
        Never => unreachable!(
            "The `[]` type has no constructors, source var {:?}",
            variant_var
        ),
        Unit => Stmt::Let(assigned, Expr::Struct(&[]), Layout::UNIT, hole),
        BoolUnion { ttrue, .. } => Stmt::Let(
            assigned,
            Expr::Literal(Literal::Bool(&tag_name == ttrue.expect_tag_ref())),
            Layout::Builtin(Builtin::Bool),
            hole,
        ),
        ByteUnion(tag_names) => {
            let opt_tag_id = tag_names
                .iter()
                .position(|key| key.expect_tag_ref() == &tag_name);

            match opt_tag_id {
                Some(tag_id) => Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Byte(tag_id as u8)),
                    Layout::Builtin(Builtin::Int(IntWidth::U8)),
                    hole,
                ),
                None => runtime_error(env, "tag must be in its own type"),
            }
        }

        Newtype {
            arguments: field_layouts,
            ..
        } => {
            let field_symbols_temp = sorted_field_symbols(env, procs, layout_cache, args);

            let mut field_symbols = Vec::with_capacity_in(field_layouts.len(), env.arena);
            field_symbols.extend(field_symbols_temp.iter().map(|r| r.1));
            let field_symbols = field_symbols.into_bump_slice();

            // Layout will unpack this unwrapped tack if it only has one (non-zero-sized) field
            let layout = layout_cache
                .from_var(env.arena, variant_var, env.subs)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

            // even though this was originally a Tag, we treat it as a Struct from now on
            let stmt = if let [only_field] = field_symbols {
                let mut hole = hole.clone();
                substitute_in_exprs(env.arena, &mut hole, assigned, *only_field);
                hole
            } else {
                Stmt::Let(assigned, Expr::Struct(field_symbols), layout, hole)
            };

            let iter = field_symbols_temp.into_iter().map(|(_, _, data)| data);
            assign_to_symbols(env, procs, layout_cache, iter, stmt)
        }
        NewtypeByVoid {
            data_tag_arguments: field_layouts,
            data_tag_name,
            ..
        } => {
            let dataful_tag = data_tag_name.expect_tag();

            if dataful_tag != tag_name {
                // this tag is not represented, and hence will never be reached, at runtime.
                runtime_error(env, "voided tag constructor is unreachable")
            } else {
                let field_symbols_temp = sorted_field_symbols(env, procs, layout_cache, args);

                let mut field_symbols = Vec::with_capacity_in(field_layouts.len(), env.arena);
                field_symbols.extend(field_symbols_temp.iter().map(|r| r.1));
                let field_symbols = field_symbols.into_bump_slice();

                // Layout will unpack this unwrapped tack if it only has one (non-zero-sized) field
                let layout = layout_cache
                    .from_var(env.arena, variant_var, env.subs)
                    .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

                // even though this was originally a Tag, we treat it as a Struct from now on
                let stmt = if let [only_field] = field_symbols {
                    let mut hole = hole.clone();
                    substitute_in_exprs(env.arena, &mut hole, assigned, *only_field);
                    hole
                } else {
                    Stmt::Let(assigned, Expr::Struct(field_symbols), layout, hole)
                };

                let iter = field_symbols_temp.into_iter().map(|(_, _, data)| data);
                assign_to_symbols(env, procs, layout_cache, iter, stmt)
            }
        }
        Wrapped(variant) => {
            let (tag_id, _) = variant.tag_name_to_id(&tag_name);

            let field_symbols_temp = sorted_field_symbols(env, procs, layout_cache, args);

            let field_symbols;

            // we must derive the union layout from the whole_var, building it up
            // from `layouts` would unroll recursive tag unions, and that leads to
            // problems down the line because we hash layouts and an unrolled
            // version is not the same as the minimal version.
            let union_layout = match return_on_layout_error!(
                env,
                layout_cache.from_var(env.arena, variant_var, env.subs),
                "Wrapped"
            ) {
                Layout::Union(ul) => ul,
                other => internal_error!(
                    "unexpected layout {:?} for {:?}",
                    other,
                    roc_types::subs::SubsFmtContent(
                        env.subs.get_content_without_compacting(variant_var),
                        env.subs
                    )
                ),
            };

            use WrappedVariant::*;
            let (tag, union_layout) = match variant {
                Recursive { sorted_tag_layouts } => {
                    debug_assert!(sorted_tag_layouts.len() > 1);

                    field_symbols = {
                        let mut temp = Vec::with_capacity_in(field_symbols_temp.len() + 1, arena);

                        temp.extend(field_symbols_temp.iter().map(|r| r.1));

                        temp.into_bump_slice()
                    };

                    let mut layouts: Vec<&'a [Layout<'a>]> =
                        Vec::with_capacity_in(sorted_tag_layouts.len(), env.arena);

                    for (_, arg_layouts) in sorted_tag_layouts.into_iter() {
                        layouts.push(arg_layouts);
                    }

                    let tag = Expr::Tag {
                        tag_layout: union_layout,
                        tag_id: tag_id as _,
                        arguments: field_symbols,
                    };

                    (tag, union_layout)
                }
                NonNullableUnwrapped {
                    tag_name: wrapped_tag_name,
                    ..
                } => {
                    debug_assert_eq!(wrapped_tag_name.expect_tag(), tag_name);

                    field_symbols = {
                        let mut temp = Vec::with_capacity_in(field_symbols_temp.len(), arena);

                        temp.extend(field_symbols_temp.iter().map(|r| r.1));

                        temp.into_bump_slice()
                    };

                    let tag = Expr::Tag {
                        tag_layout: union_layout,
                        tag_id: tag_id as _,
                        arguments: field_symbols,
                    };

                    (tag, union_layout)
                }
                NonRecursive { sorted_tag_layouts } => {
                    field_symbols = {
                        let mut temp = Vec::with_capacity_in(field_symbols_temp.len(), arena);

                        temp.extend(field_symbols_temp.iter().map(|r| r.1));

                        temp.into_bump_slice()
                    };

                    let mut layouts: Vec<&'a [Layout<'a>]> =
                        Vec::with_capacity_in(sorted_tag_layouts.len(), env.arena);

                    for (_, arg_layouts) in sorted_tag_layouts.into_iter() {
                        layouts.push(arg_layouts);
                    }

                    let tag = Expr::Tag {
                        tag_layout: union_layout,
                        tag_id: tag_id as _,
                        arguments: field_symbols,
                    };

                    (tag, union_layout)
                }
                NullableWrapped {
                    sorted_tag_layouts, ..
                } => {
                    field_symbols = {
                        let mut temp = Vec::with_capacity_in(field_symbols_temp.len() + 1, arena);

                        temp.extend(field_symbols_temp.iter().map(|r| r.1));

                        temp.into_bump_slice()
                    };

                    let mut layouts: Vec<&'a [Layout<'a>]> =
                        Vec::with_capacity_in(sorted_tag_layouts.len(), env.arena);

                    for (_, arg_layouts) in sorted_tag_layouts.into_iter() {
                        layouts.push(arg_layouts);
                    }

                    let tag = Expr::Tag {
                        tag_layout: union_layout,
                        tag_id: tag_id as _,
                        arguments: field_symbols,
                    };

                    (tag, union_layout)
                }
                NullableUnwrapped { .. } => {
                    field_symbols = {
                        let mut temp = Vec::with_capacity_in(field_symbols_temp.len() + 1, arena);

                        temp.extend(field_symbols_temp.iter().map(|r| r.1));

                        temp.into_bump_slice()
                    };

                    let tag = Expr::Tag {
                        tag_layout: union_layout,
                        tag_id: tag_id as _,
                        arguments: field_symbols,
                    };

                    (tag, union_layout)
                }
            };

            let stmt = Stmt::Let(assigned, tag, Layout::Union(union_layout), hole);
            let iter = field_symbols_temp
                .into_iter()
                .map(|x| x.2 .0)
                .rev()
                .zip(field_symbols.iter().rev());

            assign_to_symbols(env, procs, layout_cache, iter, stmt)
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn tag_union_to_function<'a>(
    env: &mut Env<'a, '_>,
    argument_variables: VariableSubsSlice,
    return_variable: Variable,
    tag_name: TagName,
    proc_symbol: Symbol,
    ext_var: Variable,
    procs: &mut Procs<'a>,
    whole_var: Variable,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let mut loc_pattern_args = vec![];
    let mut loc_expr_args = vec![];

    for index in argument_variables {
        let arg_var = env.subs[index];

        let arg_symbol = env.unique_symbol();

        let loc_pattern = Loc::at_zero(roc_can::pattern::Pattern::Identifier(arg_symbol));

        let loc_expr = Loc::at_zero(roc_can::expr::Expr::Var(arg_symbol, arg_var));

        loc_pattern_args.push((arg_var, AnnotatedMark::known_exhaustive(), loc_pattern));
        loc_expr_args.push((arg_var, loc_expr));
    }

    let loc_body = Loc::at_zero(roc_can::expr::Expr::Tag {
        tag_union_var: return_variable,
        name: tag_name,
        arguments: loc_expr_args,
        ext_var,
    });

    // Lambda does not capture anything, can't have a captures niche
    let lambda_name = LambdaName::no_niche(proc_symbol);

    let inserted = procs.insert_anonymous(
        env,
        lambda_name,
        whole_var,
        loc_pattern_args,
        loc_body,
        CapturedSymbols::None,
        return_variable,
        layout_cache,
    );

    match inserted {
        Ok(_layout) => {
            // only need to construct closure data
            let raw_layout = return_on_layout_error!(
                env,
                layout_cache.raw_from_var(env.arena, whole_var, env.subs),
                "tag_union_to_function"
            );

            match raw_layout {
                RawFunctionLayout::Function(_, lambda_set, _) => {
                    let lambda_name =
                        find_lambda_name(env, layout_cache, lambda_set, proc_symbol, &[]);
                    debug_assert!(lambda_name.no_captures());
                    construct_closure_data(
                        env,
                        procs,
                        layout_cache,
                        lambda_set,
                        lambda_name,
                        &[],
                        assigned,
                        hole,
                    )
                }
                RawFunctionLayout::ZeroArgumentThunk(_) => unreachable!(),
            }
        }

        Err(e) => runtime_error(
            env,
            env.arena.alloc(format!(
                "Could not produce tag function due to a runtime error: {:?}",
                e,
            )),
        ),
    }
}

#[allow(clippy::type_complexity)]
fn sorted_field_symbols<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    mut args: std::vec::Vec<(Variable, Loc<roc_can::expr::Expr>)>,
) -> Vec<
    'a,
    (
        u32,
        Symbol,
        ((Variable, Loc<roc_can::expr::Expr>), &'a Symbol),
    ),
> {
    let mut field_symbols_temp = Vec::with_capacity_in(args.len(), env.arena);

    for (var, mut arg) in args.drain(..) {
        // Layout will unpack this unwrapped tag if it only has one (non-zero-sized) field
        let layout = match layout_cache.from_var(env.arena, var, env.subs) {
            Ok(cached) => cached,
            Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                // this argument has type `forall a. a`, which is isomorphic to
                // the empty type (Void, Never, the empty tag union `[]`)
                // Note it does not catch the use of `[]` currently.
                use roc_can::expr::Expr;
                arg.value = Expr::RuntimeError(RuntimeError::VoidValue);
                Layout::UNIT
            }
            Err(LayoutProblem::Erroneous) => {
                // something went very wrong
                panic!("TODO turn fn_var into a RuntimeError")
            }
        };

        let alignment = layout.alignment_bytes(&layout_cache.interner, env.target_info);

        let symbol = possible_reuse_symbol_or_specialize(env, procs, layout_cache, &arg.value, var);
        field_symbols_temp.push((alignment, symbol, ((var, arg), &*env.arena.alloc(symbol))));
    }
    field_symbols_temp.sort_by(|a, b| b.0.cmp(&a.0));

    field_symbols_temp
}

/// Insert a closure that does capture symbols (because it is top-level) to the list of partial procs
fn register_noncapturing_closure<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    closure_name: Symbol,
    closure_data: ClosureData,
) {
    let ClosureData {
        function_type,
        return_type,
        recursive,
        arguments,
        loc_body: boxed_body,
        captured_symbols,
        ..
    } = closure_data;

    // Extract Procs, but discard the resulting Expr::Load.
    // That Load looks up the pointer, which we won't use here!

    let loc_body = *boxed_body;

    let is_self_recursive = !matches!(recursive, roc_can::expr::Recursive::NotRecursive);

    // this should be a top-level declaration, and hence have no captured symbols
    // if we ever do hit this (and it's not a bug), we should make sure to put the
    // captured symbols into a CapturedSymbols and give it to PartialProc::from_named_function
    debug_assert!(captured_symbols.is_empty());

    let partial_proc = PartialProc::from_named_function(
        env,
        function_type,
        arguments,
        loc_body,
        CapturedSymbols::None,
        is_self_recursive,
        return_type,
    );

    procs.partial_procs.insert(closure_name, partial_proc);
}

/// Insert a closure that may capture symbols to the list of partial procs
fn register_capturing_closure<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    closure_name: Symbol,
    closure_data: ClosureData,
) {
    // the function surrounding the closure definition may be specialized multiple times,
    // hence in theory this partial proc may be added multiple times. That would be wasteful
    // so we check whether this partial proc is already there.
    //
    // (the `gen_primitives::task_always_twice` test has this behavior)
    if !procs.partial_procs.contains_key(closure_name) {
        let ClosureData {
            function_type,
            return_type,
            closure_type,
            recursive,
            arguments,
            loc_body: boxed_body,
            captured_symbols,
            ..
        } = closure_data;
        let loc_body = *boxed_body;

        let is_self_recursive = !matches!(recursive, roc_can::expr::Recursive::NotRecursive);

        let captured_symbols = match *env.subs.get_content_without_compacting(function_type) {
            Content::Structure(FlatType::Func(_, closure_var, _)) => {
                let lambda_set_layout = {
                    LambdaSet::from_var_pub(
                        layout_cache,
                        env.arena,
                        env.subs,
                        closure_var,
                        env.target_info,
                    )
                };

                match lambda_set_layout {
                    Ok(lambda_set) => {
                        if lambda_set.is_represented(&layout_cache.interner).is_none() {
                            CapturedSymbols::None
                        } else {
                            let mut temp = Vec::from_iter_in(captured_symbols, env.arena);
                            temp.sort();
                            CapturedSymbols::Captured(temp.into_bump_slice())
                        }
                    }
                    Err(_) => {
                        // just allow this. see https://github.com/roc-lang/roc/issues/1585
                        if captured_symbols.is_empty() {
                            CapturedSymbols::None
                        } else {
                            let mut temp = Vec::from_iter_in(captured_symbols, env.arena);
                            temp.sort();
                            CapturedSymbols::Captured(temp.into_bump_slice())
                        }
                    }
                }
            }
            _ => {
                // This is a value (zero-argument thunk); it cannot capture any variables.
                debug_assert!(
                    captured_symbols.is_empty(),
                    "{:?} with layout {:?} {:?} {:?}",
                    &captured_symbols,
                    layout_cache.raw_from_var(env.arena, function_type, env.subs,),
                    env.subs,
                    (function_type, closure_type),
                );
                CapturedSymbols::None
            }
        };

        let partial_proc = PartialProc::from_named_function(
            env,
            function_type,
            arguments,
            loc_body,
            captured_symbols,
            is_self_recursive,
            return_type,
        );

        procs.partial_procs.insert(closure_name, partial_proc);
    }
}

pub fn from_can<'a>(
    env: &mut Env<'a, '_>,
    variable: Variable,
    can_expr: roc_can::expr::Expr,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
) -> Stmt<'a> {
    use roc_can::expr::Expr::*;

    match can_expr {
        When {
            cond_var,
            expr_var,
            region: _,
            loc_cond,
            branches,
            branches_cond_var: _,
            exhaustive,
        } => {
            let cond_symbol = possible_reuse_symbol_or_specialize(
                env,
                procs,
                layout_cache,
                &loc_cond.value,
                cond_var,
            );

            let stmt = from_can_when(
                env,
                cond_var,
                expr_var,
                cond_symbol,
                branches,
                exhaustive,
                layout_cache,
                procs,
                None,
            );

            // define the `when` condition
            assign_to_symbol(
                env,
                procs,
                layout_cache,
                cond_var,
                *loc_cond,
                cond_symbol,
                stmt,
            )
        }
        If {
            cond_var,
            branch_var,
            branches,
            final_else,
        } => {
            let ret_layout = layout_cache
                .from_var(env.arena, branch_var, env.subs)
                .expect("invalid ret_layout");
            let cond_layout = layout_cache
                .from_var(env.arena, cond_var, env.subs)
                .expect("invalid cond_layout");

            let mut stmt = from_can(env, branch_var, final_else.value, procs, layout_cache);

            for (loc_cond, loc_then) in branches.into_iter().rev() {
                let branching_symbol = possible_reuse_symbol_or_specialize(
                    env,
                    procs,
                    layout_cache,
                    &loc_cond.value,
                    cond_var,
                );
                let then = from_can(env, branch_var, loc_then.value, procs, layout_cache);

                stmt = cond(env, branching_symbol, cond_layout, then, stmt, ret_layout);

                stmt = assign_to_symbol(
                    env,
                    procs,
                    layout_cache,
                    cond_var,
                    loc_cond,
                    branching_symbol,
                    stmt,
                );
            }

            stmt
        }

        Expect {
            loc_condition,
            loc_continuation,
            lookups_in_cond,
        } => {
            let rest = from_can(env, variable, loc_continuation.value, procs, layout_cache);
            let cond_symbol = env.unique_symbol();

            let mut lookups = Vec::with_capacity_in(lookups_in_cond.len(), env.arena);
            let mut layouts = Vec::with_capacity_in(lookups_in_cond.len(), env.arena);

            for ExpectLookup {
                symbol,
                var,
                ability_info,
            } in lookups_in_cond
            {
                let symbol = match ability_info {
                    Some(specialization_id) => late_resolve_ability_specialization(
                        env,
                        symbol,
                        Some(specialization_id),
                        var,
                    ),
                    None => symbol,
                };
                let res_layout = layout_cache.from_var(env.arena, var, env.subs);
                let layout = return_on_layout_error!(env, res_layout, "Expect");
                if !matches!(layout, Layout::LambdaSet(..)) {
                    // Exclude functions from lookups
                    lookups.push(symbol);
                    layouts.push(layout);
                }
            }

            let mut stmt = Stmt::Expect {
                condition: cond_symbol,
                region: loc_condition.region,
                lookups: lookups.into_bump_slice(),
                layouts: layouts.into_bump_slice(),
                remainder: env.arena.alloc(rest),
            };

            stmt = with_hole(
                env,
                loc_condition.value,
                Variable::BOOL,
                procs,
                layout_cache,
                cond_symbol,
                env.arena.alloc(stmt),
            );

            stmt
        }

        ExpectFx {
            loc_condition,
            loc_continuation,
            lookups_in_cond,
        } => {
            let rest = from_can(env, variable, loc_continuation.value, procs, layout_cache);
            let cond_symbol = env.unique_symbol();

            let mut lookups = Vec::with_capacity_in(lookups_in_cond.len(), env.arena);
            let mut layouts = Vec::with_capacity_in(lookups_in_cond.len(), env.arena);

            for ExpectLookup {
                symbol,
                var,
                ability_info,
            } in lookups_in_cond
            {
                let symbol = match ability_info {
                    Some(specialization_id) => late_resolve_ability_specialization(
                        env,
                        symbol,
                        Some(specialization_id),
                        var,
                    ),
                    None => symbol,
                };
                let res_layout = layout_cache.from_var(env.arena, var, env.subs);
                let layout = return_on_layout_error!(env, res_layout, "Expect");
                if !matches!(layout, Layout::LambdaSet(..)) {
                    // Exclude functions from lookups
                    lookups.push(symbol);
                    layouts.push(layout);
                }
            }

            let mut stmt = Stmt::ExpectFx {
                condition: cond_symbol,
                region: loc_condition.region,
                lookups: lookups.into_bump_slice(),
                layouts: layouts.into_bump_slice(),
                remainder: env.arena.alloc(rest),
            };

            stmt = with_hole(
                env,
                loc_condition.value,
                Variable::BOOL,
                procs,
                layout_cache,
                cond_symbol,
                env.arena.alloc(stmt),
            );

            stmt
        }

        Dbg {
            loc_condition,
            loc_continuation,
            variable,
            symbol: dbg_symbol,
        } => {
            let rest = from_can(env, variable, loc_continuation.value, procs, layout_cache);

            let call = crate::ir::Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::Dbg,
                    update_mode: env.next_update_mode_id(),
                },
                arguments: env.arena.alloc([dbg_symbol]),
            };

            let dbg_layout = layout_cache
                .from_var(env.arena, variable, env.subs)
                .expect("invalid dbg_layout");

            let expr = Expr::Call(call);
            let mut stmt = Stmt::Let(dbg_symbol, expr, dbg_layout, env.arena.alloc(rest));

            let symbol_is_reused = matches!(
                can_reuse_symbol(env, procs, &loc_condition.value, variable),
                ReuseSymbol::Value(_)
            );

            // skip evaluating the condition if it's just a symbol
            if !symbol_is_reused {
                stmt = with_hole(
                    env,
                    loc_condition.value,
                    variable,
                    procs,
                    layout_cache,
                    dbg_symbol,
                    env.arena.alloc(stmt),
                );
            }

            stmt
        }

        LetRec(defs, cont, _cycle_mark) => {
            // because Roc is strict, only functions can be recursive!
            for def in defs.into_iter() {
                if let roc_can::pattern::Pattern::Identifier(symbol) = &def.loc_pattern.value {
                    // Now that we know for sure it's a closure, get an owned
                    // version of these variant args so we can use them properly.
                    match def.loc_expr.value {
                        Closure(closure_data) => {
                            register_capturing_closure(
                                env,
                                procs,
                                layout_cache,
                                *symbol,
                                closure_data,
                            );

                            continue;
                        }
                        _ => unreachable!("recursive value is not a function"),
                    }
                }
                unreachable!("recursive value does not have Identifier pattern")
            }

            from_can(env, variable, cont.value, procs, layout_cache)
        }
        LetNonRec(def, cont) => from_can_let(env, procs, layout_cache, def, cont, variable, None),
        _ => {
            let symbol = env.unique_symbol();
            let hole = env.arena.alloc(Stmt::Ret(symbol));
            with_hole(env, can_expr, variable, procs, layout_cache, symbol, hole)
        }
    }
}

fn to_opt_branches<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    branches: std::vec::Vec<roc_can::expr::WhenBranch>,
    exhaustive_mark: ExhaustiveMark,
    layout_cache: &mut LayoutCache<'a>,
) -> std::vec::Vec<(
    Pattern<'a>,
    Option<Loc<roc_can::expr::Expr>>,
    roc_can::expr::Expr,
)> {
    debug_assert!(!branches.is_empty());

    let mut opt_branches = std::vec::Vec::new();

    for when_branch in branches {
        if when_branch.redundant.is_redundant(env.subs) {
            // Don't codegen this branch since it's redundant.
            continue;
        }

        for loc_pattern in when_branch.patterns {
            match from_can_pattern(env, procs, layout_cache, &loc_pattern.pattern.value) {
                Ok((mono_pattern, assignments)) => {
                    let loc_expr = if !loc_pattern.degenerate {
                        let mut loc_expr = when_branch.value.clone();

                        let region = loc_pattern.pattern.region;
                        for (symbol, variable, expr) in assignments.into_iter().rev() {
                            let def = roc_can::def::Def {
                                annotation: None,
                                expr_var: variable,
                                loc_expr: Loc::at(region, expr),
                                loc_pattern: Loc::at(
                                    region,
                                    roc_can::pattern::Pattern::Identifier(symbol),
                                ),
                                pattern_vars: std::iter::once((symbol, variable)).collect(),
                            };
                            let new_expr =
                                roc_can::expr::Expr::LetNonRec(Box::new(def), Box::new(loc_expr));
                            loc_expr = Loc::at(region, new_expr);
                        }

                        loc_expr
                    } else {
                        // This pattern is degenerate; when it's reached we must emit a runtime
                        // error.
                        Loc::at_zero(roc_can::expr::Expr::RuntimeError(
                            RuntimeError::DegenerateBranch(loc_pattern.pattern.region),
                        ))
                    };

                    // TODO remove clone?
                    opt_branches.push((mono_pattern, when_branch.guard.clone(), loc_expr.value));
                }
                Err(runtime_error) => {
                    // TODO remove clone?
                    opt_branches.push((
                        Pattern::Underscore,
                        when_branch.guard.clone(),
                        roc_can::expr::Expr::RuntimeError(runtime_error),
                    ));
                }
            }
        }
    }

    if exhaustive_mark.is_non_exhaustive(env.subs) {
        // In contrast to elm (currently), we still do codegen even if a pattern is non-exhaustive.
        // So we not only report exhaustiveness errors, but also correct them
        opt_branches.push((
            Pattern::Underscore,
            None,
            roc_can::expr::Expr::RuntimeError(roc_problem::can::RuntimeError::NonExhaustivePattern),
        ));
    }

    opt_branches
}

#[allow(clippy::too_many_arguments)]
fn from_can_when<'a>(
    env: &mut Env<'a, '_>,
    cond_var: Variable,
    expr_var: Variable,
    cond_symbol: Symbol,
    branches: std::vec::Vec<roc_can::expr::WhenBranch>,
    exhaustive_mark: ExhaustiveMark,
    layout_cache: &mut LayoutCache<'a>,
    procs: &mut Procs<'a>,
    join_point: Option<JoinPointId>,
) -> Stmt<'a> {
    if branches.is_empty() {
        // A when-expression with no branches is a runtime error.
        // We can't know what to return!
        return runtime_error(env, "Hit a 0-branch when expression");
    }
    let opt_branches = to_opt_branches(env, procs, branches, exhaustive_mark, layout_cache);

    let cond_layout = return_on_layout_error!(
        env,
        layout_cache.from_var(env.arena, cond_var, env.subs),
        "from_can_when cond_layout"
    );

    let ret_layout = return_on_layout_error!(
        env,
        layout_cache.from_var(env.arena, expr_var, env.subs),
        "from_can_when ret_layout"
    );

    let arena = env.arena;
    let it = opt_branches
        .into_iter()
        .filter_map(|(pattern, opt_guard, can_expr)| {
            // If the pattern has a void layout we can drop it; however, we must still perform the
            // work of building the body, because that may contain specializations we must
            // discover for use elsewhere. See
            //   `unreachable_branch_is_eliminated_but_produces_lambda_specializations` in test_mono
            // for an example.
            let should_eliminate_branch = pattern.is_voided();

            // If we're going to eliminate the branch, we need to take a snapshot of the symbol
            // specializations before we enter the branch, because any new specializations that
            // will be added in the branch body will never need to be resolved!
            let specialization_symbol_snapshot = if should_eliminate_branch {
                Some(std::mem::take(&mut procs.symbol_specializations))
            } else {
                None
            };

            let branch_stmt = match join_point {
                None => from_can(env, expr_var, can_expr, procs, layout_cache),
                Some(id) => {
                    let symbol = env.unique_symbol();
                    let arguments = bumpalo::vec![in env.arena; symbol].into_bump_slice();
                    let jump = env.arena.alloc(Stmt::Jump(id, arguments));

                    with_hole(env, can_expr, expr_var, procs, layout_cache, symbol, jump)
                }
            };

            use crate::decision_tree::Guard;
            let result = if let Some(loc_expr) = opt_guard {
                let id = JoinPointId(env.unique_symbol());
                let symbol = env.unique_symbol();
                let jump = env.arena.alloc(Stmt::Jump(id, env.arena.alloc([symbol])));

                let guard_stmt = with_hole(
                    env,
                    loc_expr.value,
                    Variable::BOOL,
                    procs,
                    layout_cache,
                    symbol,
                    jump,
                );

                (
                    pattern.clone(),
                    Guard::Guard {
                        id,
                        pattern,
                        stmt: guard_stmt,
                    },
                    branch_stmt,
                )
            } else {
                (pattern, Guard::NoGuard, branch_stmt)
            };

            if should_eliminate_branch {
                procs.symbol_specializations = specialization_symbol_snapshot.unwrap();
                None
            } else {
                Some(result)
            }
        });
    let mono_branches = Vec::from_iter_in(it, arena);

    crate::decision_tree::optimize_when(
        env,
        procs,
        layout_cache,
        cond_symbol,
        cond_layout,
        ret_layout,
        mono_branches,
    )
}

fn substitute(substitutions: &BumpMap<Symbol, Symbol>, s: Symbol) -> Option<Symbol> {
    match substitutions.get(&s) {
        Some(new) => {
            debug_assert!(!substitutions.contains_key(new));
            Some(*new)
        }
        None => None,
    }
}

fn substitute_in_exprs<'a>(arena: &'a Bump, stmt: &mut Stmt<'a>, from: Symbol, to: Symbol) {
    let mut subs = BumpMap::with_capacity_in(1, arena);
    subs.insert(from, to);

    // TODO clean this up
    let ref_stmt = arena.alloc(stmt.clone());
    if let Some(new) = substitute_in_stmt_help(arena, ref_stmt, &subs) {
        *stmt = new.clone();
    }
}

fn substitute_in_stmt_help<'a>(
    arena: &'a Bump,
    stmt: &'a Stmt<'a>,
    subs: &BumpMap<Symbol, Symbol>,
) -> Option<&'a Stmt<'a>> {
    use Stmt::*;

    match stmt {
        Let(symbol, expr, layout, cont) => {
            let opt_cont = substitute_in_stmt_help(arena, cont, subs);
            let opt_expr = substitute_in_expr(arena, expr, subs);

            if opt_expr.is_some() || opt_cont.is_some() {
                let cont = opt_cont.unwrap_or(cont);
                let expr = opt_expr.unwrap_or_else(|| expr.clone());

                Some(arena.alloc(Let(*symbol, expr, *layout, cont)))
            } else {
                None
            }
        }
        Join {
            id,
            parameters,
            remainder,
            body: continuation,
        } => {
            let opt_remainder = substitute_in_stmt_help(arena, remainder, subs);
            let opt_continuation = substitute_in_stmt_help(arena, continuation, subs);

            if opt_remainder.is_some() || opt_continuation.is_some() {
                let remainder = opt_remainder.unwrap_or(remainder);
                let continuation = opt_continuation.unwrap_or(*continuation);

                Some(arena.alloc(Join {
                    id: *id,
                    parameters,
                    remainder,
                    body: continuation,
                }))
            } else {
                None
            }
        }
        Switch {
            cond_symbol,
            cond_layout,
            branches,
            default_branch,
            ret_layout,
        } => {
            let opt_default = substitute_in_stmt_help(arena, default_branch.1, subs);

            let mut did_change = false;

            let opt_branches = Vec::from_iter_in(
                branches.iter().map(|(label, info, branch)| {
                    match substitute_in_stmt_help(arena, branch, subs) {
                        None => None,
                        Some(branch) => {
                            did_change = true;
                            Some((*label, info.clone(), branch.clone()))
                        }
                    }
                }),
                arena,
            );

            if opt_default.is_some() || did_change {
                let default_branch = (
                    default_branch.0.clone(),
                    opt_default.unwrap_or(default_branch.1),
                );

                let branches = if did_change {
                    let new = Vec::from_iter_in(
                        opt_branches.into_iter().zip(branches.iter()).map(
                            |(opt_branch, branch)| match opt_branch {
                                None => branch.clone(),
                                Some(new_branch) => new_branch,
                            },
                        ),
                        arena,
                    );

                    new.into_bump_slice()
                } else {
                    branches
                };

                Some(arena.alloc(Switch {
                    cond_symbol: *cond_symbol,
                    cond_layout: *cond_layout,
                    default_branch,
                    branches,
                    ret_layout: *ret_layout,
                }))
            } else {
                None
            }
        }
        Ret(s) => match substitute(subs, *s) {
            Some(s) => Some(arena.alloc(Ret(s))),
            None => None,
        },
        Refcounting(modify, cont) => {
            // TODO should we substitute in the ModifyRc?
            match substitute_in_stmt_help(arena, cont, subs) {
                Some(cont) => Some(arena.alloc(Refcounting(*modify, cont))),
                None => None,
            }
        }

        Expect {
            condition,
            region,
            lookups,
            layouts,
            remainder,
        } => {
            let new_remainder =
                substitute_in_stmt_help(arena, remainder, subs).unwrap_or(remainder);

            let new_lookups = Vec::from_iter_in(
                lookups.iter().map(|s| substitute(subs, *s).unwrap_or(*s)),
                arena,
            );

            let expect = Expect {
                condition: substitute(subs, *condition).unwrap_or(*condition),
                region: *region,
                lookups: new_lookups.into_bump_slice(),
                layouts,
                remainder: new_remainder,
            };

            Some(arena.alloc(expect))
        }

        ExpectFx {
            condition,
            region,
            lookups,
            layouts,
            remainder,
        } => {
            let new_remainder =
                substitute_in_stmt_help(arena, remainder, subs).unwrap_or(remainder);

            let new_lookups = Vec::from_iter_in(
                lookups.iter().map(|s| substitute(subs, *s).unwrap_or(*s)),
                arena,
            );

            let expect = ExpectFx {
                condition: substitute(subs, *condition).unwrap_or(*condition),
                region: *region,
                lookups: new_lookups.into_bump_slice(),
                layouts,
                remainder: new_remainder,
            };

            Some(arena.alloc(expect))
        }

        Jump(id, args) => {
            let mut did_change = false;
            let new_args = Vec::from_iter_in(
                args.iter().map(|s| match substitute(subs, *s) {
                    None => *s,
                    Some(s) => {
                        did_change = true;
                        s
                    }
                }),
                arena,
            );

            if did_change {
                let args = new_args.into_bump_slice();

                Some(arena.alloc(Jump(*id, args)))
            } else {
                None
            }
        }
        Crash(msg, tag) => substitute(subs, *msg).map(|new| &*arena.alloc(Crash(new, *tag))),
    }
}

fn substitute_in_call<'a>(
    arena: &'a Bump,
    call: &'a Call<'a>,
    subs: &BumpMap<Symbol, Symbol>,
) -> Option<Call<'a>> {
    let Call {
        call_type,
        arguments,
    } = call;

    let opt_call_type = match call_type {
        CallType::ByName {
            name,
            arg_layouts,
            ret_layout,
            specialization_id,
        } => substitute(subs, name.name()).map(|new| CallType::ByName {
            name: name.replace_name(new),
            arg_layouts,
            ret_layout,
            specialization_id: *specialization_id,
        }),
        CallType::Foreign { .. } => None,
        CallType::LowLevel { .. } => None,
        CallType::HigherOrder { .. } => None,
    };

    let mut did_change = false;
    let new_args = Vec::from_iter_in(
        arguments.iter().map(|s| match substitute(subs, *s) {
            None => *s,
            Some(s) => {
                did_change = true;
                s
            }
        }),
        arena,
    );

    if did_change || opt_call_type.is_some() {
        let call_type = opt_call_type.unwrap_or_else(|| call_type.clone());

        let arguments = new_args.into_bump_slice();

        Some(self::Call {
            call_type,
            arguments,
        })
    } else {
        None
    }
}

fn substitute_in_expr<'a>(
    arena: &'a Bump,
    expr: &'a Expr<'a>,
    subs: &BumpMap<Symbol, Symbol>,
) -> Option<Expr<'a>> {
    use Expr::*;

    match expr {
        Literal(_) | EmptyArray | RuntimeErrorFunction(_) => None,

        Call(call) => substitute_in_call(arena, call, subs).map(Expr::Call),

        Tag {
            tag_layout,
            tag_id,
            arguments: args,
        } => {
            let mut did_change = false;
            let new_args = Vec::from_iter_in(
                args.iter().map(|s| match substitute(subs, *s) {
                    None => *s,
                    Some(s) => {
                        did_change = true;
                        s
                    }
                }),
                arena,
            );

            if did_change {
                let arguments = new_args.into_bump_slice();

                Some(Tag {
                    tag_layout: *tag_layout,
                    tag_id: *tag_id,
                    arguments,
                })
            } else {
                None
            }
        }

        Reuse { .. } | Reset { .. } => unreachable!("reset/reuse have not been introduced yet"),

        Struct(args) => {
            let mut did_change = false;
            let new_args = Vec::from_iter_in(
                args.iter().map(|s| match substitute(subs, *s) {
                    None => *s,
                    Some(s) => {
                        did_change = true;
                        s
                    }
                }),
                arena,
            );

            if did_change {
                let args = new_args.into_bump_slice();

                Some(Struct(args))
            } else {
                None
            }
        }

        Array {
            elems: args,
            elem_layout,
        } => {
            let mut did_change = false;
            let new_args = Vec::from_iter_in(
                args.iter().map(|e| {
                    if let ListLiteralElement::Symbol(s) = e {
                        match substitute(subs, *s) {
                            None => ListLiteralElement::Symbol(*s),
                            Some(s) => {
                                did_change = true;
                                ListLiteralElement::Symbol(s)
                            }
                        }
                    } else {
                        *e
                    }
                }),
                arena,
            );

            if did_change {
                let args = new_args.into_bump_slice();

                Some(Array {
                    elem_layout: *elem_layout,
                    elems: args,
                })
            } else {
                None
            }
        }

        ExprBox { symbol } => {
            substitute(subs, *symbol).map(|new_symbol| ExprBox { symbol: new_symbol })
        }

        ExprUnbox { symbol } => {
            substitute(subs, *symbol).map(|new_symbol| ExprUnbox { symbol: new_symbol })
        }

        StructAtIndex {
            index,
            structure,
            field_layouts,
        } => match substitute(subs, *structure) {
            Some(structure) => Some(StructAtIndex {
                index: *index,
                field_layouts,
                structure,
            }),
            None => None,
        },

        GetTagId {
            structure,
            union_layout,
        } => match substitute(subs, *structure) {
            Some(structure) => Some(GetTagId {
                structure,
                union_layout: *union_layout,
            }),
            None => None,
        },

        UnionAtIndex {
            structure,
            tag_id,
            index,
            union_layout,
        } => match substitute(subs, *structure) {
            Some(structure) => Some(UnionAtIndex {
                structure,
                tag_id: *tag_id,
                index: *index,
                union_layout: *union_layout,
            }),
            None => None,
        },
    }
}

#[allow(clippy::too_many_arguments)]
pub fn store_pattern<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_pat: &Pattern<'a>,
    outer_symbol: Symbol,
    stmt: Stmt<'a>,
) -> Stmt<'a> {
    match store_pattern_help(env, procs, layout_cache, can_pat, outer_symbol, stmt) {
        StorePattern::Productive(new) => new,
        StorePattern::NotProductive(new) => new,
    }
}

enum StorePattern<'a> {
    /// we bound new symbols
    Productive(Stmt<'a>),
    /// no new symbols were bound in this pattern
    NotProductive(Stmt<'a>),
}

/// It is crucial for correct RC insertion that we don't create dead variables!
#[allow(clippy::too_many_arguments)]
fn store_pattern_help<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_pat: &Pattern<'a>,
    outer_symbol: Symbol,
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    match can_pat {
        Identifier(symbol) => {
            // An identifier in a pattern can define at most one specialization!
            // Remove any requested specializations for this name now, since this is the definition site.
            let specialization_symbol = procs
                .symbol_specializations
                .remove_single(*symbol)
                // Can happen when the symbol was never used under this body, and hence has no
                // requested specialization.
                .unwrap_or(*symbol);

            substitute_in_exprs(env.arena, &mut stmt, specialization_symbol, outer_symbol);
        }
        Underscore => {
            // do nothing
            return StorePattern::NotProductive(stmt);
        }
        IntLiteral(_, _)
        | FloatLiteral(_, _)
        | DecimalLiteral(_)
        | EnumLiteral { .. }
        | BitLiteral { .. }
        | StrLiteral(_) => {
            return StorePattern::NotProductive(stmt);
        }
        NewtypeDestructure { arguments, .. } => match arguments.as_slice() {
            [(pattern, _layout)] => {
                return store_pattern_help(env, procs, layout_cache, pattern, outer_symbol, stmt);
            }
            _ => {
                let mut fields = Vec::with_capacity_in(arguments.len(), env.arena);
                fields.extend(arguments.iter().map(|x| x.1));

                let layout = Layout::struct_no_name_order(fields.into_bump_slice());

                return store_newtype_pattern(
                    env,
                    procs,
                    layout_cache,
                    outer_symbol,
                    &layout,
                    arguments,
                    stmt,
                );
            }
        },
        AppliedTag {
            arguments,
            layout,
            tag_id,
            ..
        } => {
            return store_tag_pattern(
                env,
                procs,
                layout_cache,
                outer_symbol,
                *layout,
                arguments,
                *tag_id,
                stmt,
            );
        }

        List {
            arity,
            element_layout,
            elements,
        } => {
            return store_list_pattern(
                env,
                procs,
                layout_cache,
                outer_symbol,
                *arity,
                *element_layout,
                elements,
                stmt,
            )
        }

        Voided { .. } => {
            return StorePattern::NotProductive(stmt);
        }

        OpaqueUnwrap { argument, .. } => {
            let (pattern, _layout) = &**argument;
            return store_pattern_help(env, procs, layout_cache, pattern, outer_symbol, stmt);
        }

        RecordDestructure(destructs, [_single_field]) => {
            for destruct in destructs {
                match &destruct.typ {
                    DestructType::Required(symbol) => {
                        let specialization_symbol = procs
                            .symbol_specializations
                            .remove_single(*symbol)
                            // Can happen when the symbol was never used under this body, and hence has no
                            // requested specialization.
                            .unwrap_or(*symbol);

                        substitute_in_exprs(
                            env.arena,
                            &mut stmt,
                            specialization_symbol,
                            outer_symbol,
                        );
                    }
                    DestructType::Guard(guard_pattern) => {
                        return store_pattern_help(
                            env,
                            procs,
                            layout_cache,
                            guard_pattern,
                            outer_symbol,
                            stmt,
                        );
                    }
                }
            }
        }
        RecordDestructure(destructs, sorted_fields) => {
            let mut is_productive = false;
            for (index, destruct) in destructs.iter().enumerate().rev() {
                match store_record_destruct(
                    env,
                    procs,
                    layout_cache,
                    destruct,
                    index as u64,
                    outer_symbol,
                    sorted_fields,
                    stmt,
                ) {
                    StorePattern::Productive(new) => {
                        is_productive = true;
                        stmt = new;
                    }
                    StorePattern::NotProductive(new) => {
                        stmt = new;
                    }
                }
            }

            if !is_productive {
                return StorePattern::NotProductive(stmt);
            }
        }
    }

    StorePattern::Productive(stmt)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ListIndex(
    /// Positive if we should index from the head, negative if we should index from the tail
    /// 0 is lst[0]
    /// -1 is lst[List.len lst - 1]
    i64,
);

impl ListIndex {
    pub fn from_pattern_index(index: usize, arity: ListArity) -> Self {
        match arity {
            ListArity::Exact(_) => Self(index as _),
            ListArity::Slice(head, tail) => {
                if index < head {
                    Self(index as _)
                } else {
                    // Slice(head=2, tail=5)
                    //
                    // s t ... w y z x q
                    // 0 1     2 3 4 5 6 index
                    //         0 1 2 3 4 (index - head)
                    //         5 4 3 2 1 (tail - (index - head))
                    Self(-((tail - (index - head)) as i64))
                }
            }
        }
    }
}

pub(crate) type Store<'a> = (Symbol, Layout<'a>, Expr<'a>);

/// Builds the list index we should index into
#[must_use]
pub(crate) fn build_list_index_probe<'a>(
    env: &mut Env<'a, '_>,
    list_sym: Symbol,
    list_index: &ListIndex,
) -> (Symbol, impl DoubleEndedIterator<Item = Store<'a>>) {
    let usize_layout = Layout::usize(env.target_info);

    let list_index = list_index.0;
    let index_sym = env.unique_symbol();

    let (opt_len_store, opt_offset_store, index_store) = if list_index >= 0 {
        let index_expr = Expr::Literal(Literal::Int((list_index as i128).to_ne_bytes()));

        let index_store = (index_sym, usize_layout, index_expr);

        (None, None, index_store)
    } else {
        let len_sym = env.unique_symbol();
        let len_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::ListLen,
                update_mode: env.next_update_mode_id(),
            },
            arguments: env.arena.alloc([list_sym]),
        });

        let offset = list_index.abs();
        let offset_sym = env.unique_symbol();
        let offset_expr = Expr::Literal(Literal::Int((offset as i128).to_ne_bytes()));

        let index_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::NumSub,
                update_mode: env.next_update_mode_id(),
            },
            arguments: env.arena.alloc([len_sym, offset_sym]),
        });

        let len_store = (len_sym, usize_layout, len_expr);
        let offset_store = (offset_sym, usize_layout, offset_expr);
        let index_store = (index_sym, usize_layout, index_expr);

        (Some(len_store), Some(offset_store), index_store)
    };

    let stores = (opt_len_store.into_iter())
        .chain(opt_offset_store)
        .chain([index_store]);

    (index_sym, stores)
}

#[allow(clippy::too_many_arguments)]
fn store_list_pattern<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    list_sym: Symbol,
    list_arity: ListArity,
    element_layout: Layout<'a>,
    elements: &[Pattern<'a>],
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    let mut is_productive = false;

    for (index, element) in elements.iter().enumerate().rev() {
        let compute_element_load = |env: &mut Env<'a, '_>| {
            let list_index = ListIndex::from_pattern_index(index, list_arity);

            let (index_sym, needed_stores) = build_list_index_probe(env, list_sym, &list_index);

            let load = Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::ListGetUnsafe,
                    update_mode: env.next_update_mode_id(),
                },
                arguments: env.arena.alloc([list_sym, index_sym]),
            });

            (load, needed_stores)
        };

        let (store_loaded, needed_stores) = match element {
            Identifier(symbol) => {
                let (load, needed_stores) = compute_element_load(env);

                // Pattern can define only one specialization
                let symbol = procs
                    .symbol_specializations
                    .remove_single(*symbol)
                    .unwrap_or(*symbol);

                // store immediately in the given symbol
                (
                    Stmt::Let(symbol, load, element_layout, env.arena.alloc(stmt)),
                    needed_stores,
                )
            }
            Underscore
            | IntLiteral(_, _)
            | FloatLiteral(_, _)
            | DecimalLiteral(_)
            | EnumLiteral { .. }
            | BitLiteral { .. }
            | StrLiteral(_) => {
                // ignore
                continue;
            }
            _ => {
                // store the field in a symbol, and continue matching on it
                let symbol = env.unique_symbol();

                // first recurse, continuing to unpack symbol
                match store_pattern_help(env, procs, layout_cache, element, symbol, stmt) {
                    StorePattern::Productive(new) => {
                        stmt = new;
                        let (load, needed_stores) = compute_element_load(env);

                        // only if we bind one of its (sub)fields to a used name should we
                        // extract the field
                        (
                            Stmt::Let(symbol, load, element_layout, env.arena.alloc(stmt)),
                            needed_stores,
                        )
                    }
                    StorePattern::NotProductive(new) => {
                        // do nothing
                        stmt = new;
                        continue;
                    }
                }
            }
        };

        is_productive = true;

        stmt = store_loaded;
        for (sym, lay, expr) in needed_stores.rev() {
            stmt = Stmt::Let(sym, expr, lay, env.arena.alloc(stmt));
        }
    }

    if is_productive {
        StorePattern::Productive(stmt)
    } else {
        StorePattern::NotProductive(stmt)
    }
}

#[allow(clippy::too_many_arguments)]
fn store_tag_pattern<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    structure: Symbol,
    union_layout: UnionLayout<'a>,
    arguments: &[(Pattern<'a>, Layout<'a>)],
    tag_id: TagIdIntType,
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    let mut is_productive = false;

    for (index, (argument, arg_layout)) in arguments.iter().enumerate().rev() {
        let mut arg_layout = *arg_layout;

        if let Layout::RecursivePointer = arg_layout {
            arg_layout = Layout::Union(union_layout);
        }

        let load = Expr::UnionAtIndex {
            index: index as u64,
            structure,
            tag_id,
            union_layout,
        };

        match argument {
            Identifier(symbol) => {
                // Pattern can define only one specialization
                let symbol = procs
                    .symbol_specializations
                    .remove_single(*symbol)
                    .unwrap_or(*symbol);

                // store immediately in the given symbol
                stmt = Stmt::Let(symbol, load, arg_layout, env.arena.alloc(stmt));
                is_productive = true;
            }
            Underscore => {
                // ignore
            }
            IntLiteral(_, _)
            | FloatLiteral(_, _)
            | DecimalLiteral(_)
            | EnumLiteral { .. }
            | BitLiteral { .. }
            | StrLiteral(_) => {}
            _ => {
                // store the field in a symbol, and continue matching on it
                let symbol = env.unique_symbol();

                // first recurse, continuing to unpack symbol
                match store_pattern_help(env, procs, layout_cache, argument, symbol, stmt) {
                    StorePattern::Productive(new) => {
                        is_productive = true;
                        stmt = new;
                        // only if we bind one of its (sub)fields to a used name should we
                        // extract the field
                        stmt = Stmt::Let(symbol, load, arg_layout, env.arena.alloc(stmt));
                    }
                    StorePattern::NotProductive(new) => {
                        // do nothing
                        stmt = new;
                    }
                }
            }
        }
    }

    if is_productive {
        StorePattern::Productive(stmt)
    } else {
        StorePattern::NotProductive(stmt)
    }
}

#[allow(clippy::too_many_arguments)]
fn store_newtype_pattern<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    structure: Symbol,
    layout: &Layout<'a>,
    arguments: &[(Pattern<'a>, Layout<'a>)],
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    let mut arg_layouts = Vec::with_capacity_in(arguments.len(), env.arena);
    let mut is_productive = false;

    for (_, layout) in arguments {
        arg_layouts.push(*layout);
    }

    for (index, (argument, arg_layout)) in arguments.iter().enumerate().rev() {
        let mut arg_layout = *arg_layout;

        if let Layout::RecursivePointer = arg_layout {
            arg_layout = *layout;
        }

        let load = Expr::StructAtIndex {
            index: index as u64,
            field_layouts: arg_layouts.clone().into_bump_slice(),
            structure,
        };

        match argument {
            Identifier(symbol) => {
                // store immediately in the given symbol, removing it specialization if it had any
                let specialization_symbol = procs
                    .symbol_specializations
                    .remove_single(*symbol)
                    // Can happen when the symbol was never used under this body, and hence has no
                    // requested specialization.
                    .unwrap_or(*symbol);

                stmt = Stmt::Let(
                    specialization_symbol,
                    load,
                    arg_layout,
                    env.arena.alloc(stmt),
                );
                is_productive = true;
            }
            Underscore => {
                // ignore
            }
            IntLiteral(_, _)
            | FloatLiteral(_, _)
            | DecimalLiteral(_)
            | EnumLiteral { .. }
            | BitLiteral { .. }
            | StrLiteral(_) => {}
            _ => {
                // store the field in a symbol, and continue matching on it
                let symbol = env.unique_symbol();

                // first recurse, continuing to unpack symbol
                match store_pattern_help(env, procs, layout_cache, argument, symbol, stmt) {
                    StorePattern::Productive(new) => {
                        is_productive = true;
                        stmt = new;
                        // only if we bind one of its (sub)fields to a used name should we
                        // extract the field
                        stmt = Stmt::Let(symbol, load, arg_layout, env.arena.alloc(stmt));
                    }
                    StorePattern::NotProductive(new) => {
                        // do nothing
                        stmt = new;
                    }
                }
            }
        }
    }

    if is_productive {
        StorePattern::Productive(stmt)
    } else {
        StorePattern::NotProductive(stmt)
    }
}

#[allow(clippy::too_many_arguments)]
fn store_record_destruct<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    destruct: &RecordDestruct<'a>,
    index: u64,
    outer_symbol: Symbol,
    sorted_fields: &'a [Layout<'a>],
    mut stmt: Stmt<'a>,
) -> StorePattern<'a> {
    use Pattern::*;

    let load = Expr::StructAtIndex {
        index,
        field_layouts: sorted_fields,
        structure: outer_symbol,
    };

    match &destruct.typ {
        DestructType::Required(symbol) => {
            // A destructure can define at most one specialization!
            // Remove any requested specializations for this name now, since this is the definition site.
            let specialization_symbol = procs
                .symbol_specializations
                .remove_single(*symbol)
                // Can happen when the symbol was never used under this body, and hence has no
                // requested specialization.
                .unwrap_or(*symbol);

            stmt = Stmt::Let(
                specialization_symbol,
                load,
                destruct.layout,
                env.arena.alloc(stmt),
            );
        }
        DestructType::Guard(guard_pattern) => match &guard_pattern {
            Identifier(symbol) => {
                let specialization_symbol = procs
                    .symbol_specializations
                    .remove_single(*symbol)
                    // Can happen when the symbol was never used under this body, and hence has no
                    // requested specialization.
                    .unwrap_or(*symbol);

                stmt = Stmt::Let(
                    specialization_symbol,
                    load,
                    destruct.layout,
                    env.arena.alloc(stmt),
                );
            }
            Underscore => {
                // important that this is special-cased to do nothing: mono record patterns will extract all the
                // fields, but those not bound in the source code are guarded with the underscore
                // pattern. So given some record `{ x : a, y : b }`, a match
                //
                // { x } -> ...
                //
                // is actually
                //
                // { x, y: _ } -> ...
                //
                // internally. But `y` is never used, so we must make sure it't not stored/loaded.
                return StorePattern::NotProductive(stmt);
            }
            IntLiteral(_, _)
            | FloatLiteral(_, _)
            | DecimalLiteral(_)
            | EnumLiteral { .. }
            | BitLiteral { .. }
            | StrLiteral(_) => {
                return StorePattern::NotProductive(stmt);
            }

            _ => {
                let symbol = env.unique_symbol();

                match store_pattern_help(env, procs, layout_cache, guard_pattern, symbol, stmt) {
                    StorePattern::Productive(new) => {
                        stmt = new;
                        stmt = Stmt::Let(symbol, load, destruct.layout, env.arena.alloc(stmt));
                    }
                    StorePattern::NotProductive(stmt) => return StorePattern::NotProductive(stmt),
                }
            }
        },
    }

    StorePattern::Productive(stmt)
}

/// We want to re-use symbols that are not function symbols
/// for any other expression, we create a new symbol, and will
/// later make sure it gets assigned the correct value.

#[derive(Debug)]
enum ReuseSymbol {
    Imported(Symbol),
    LocalFunction(Symbol),
    Value(Symbol),
    UnspecializedExpr(Symbol),
    NotASymbol,
}

fn can_reuse_symbol<'a>(
    env: &mut Env<'a, '_>,
    procs: &Procs<'a>,
    expr: &roc_can::expr::Expr,
    expr_var: Variable,
) -> ReuseSymbol {
    use roc_can::expr::Expr::*;
    use ReuseSymbol::*;

    let symbol = match expr {
        AbilityMember(member, specialization_id, _) => {
            late_resolve_ability_specialization(env, *member, *specialization_id, expr_var)
        }
        Var(symbol, _) => *symbol,
        _ => return NotASymbol,
    };

    let arguments = [
        Symbol::ARG_1,
        Symbol::ARG_2,
        Symbol::ARG_3,
        Symbol::ARG_4,
        Symbol::ARG_5,
        Symbol::ARG_6,
        Symbol::ARG_7,
    ];

    if arguments.contains(&symbol) {
        Value(symbol)
    } else if env.is_imported_symbol(symbol) || env.is_unloaded_derived_symbol(symbol, procs) {
        Imported(symbol)
    } else if procs.partial_procs.contains_key(symbol) {
        LocalFunction(symbol)
    } else if procs.ability_member_aliases.get(symbol).is_some() {
        UnspecializedExpr(symbol)
    } else {
        Value(symbol)
    }
}

fn possible_reuse_symbol_or_specialize<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    expr: &roc_can::expr::Expr,
    var: Variable,
) -> Symbol {
    match can_reuse_symbol(env, procs, expr, var) {
        ReuseSymbol::Value(symbol) => {
            procs.get_or_insert_symbol_specialization(env, layout_cache, symbol, var)
        }
        _ => env.unique_symbol(),
    }
}

fn handle_variable_aliasing<'a, BuildRest>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    variable: Variable,
    left: Symbol,
    right: Symbol,
    build_rest: BuildRest,
) -> Stmt<'a>
where
    BuildRest: FnOnce(&mut Env<'a, '_>, &mut Procs<'a>, &mut LayoutCache<'a>) -> Stmt<'a>,
{
    // 1. Handle references to ability members - we could be aliasing an ability member, or another
    //    alias to an ability member.
    {
        let is_ability_member = env
            .abilities
            .with_module_abilities_store(env.home, |store| store.is_ability_member_name(right));

        if is_ability_member {
            procs
                .ability_member_aliases
                .insert(left, AbilityMember(right));
            return build_rest(env, procs, layout_cache);
        }
        if let Some(&ability_member) = procs.ability_member_aliases.get(right) {
            procs.ability_member_aliases.insert(left, ability_member);
            return build_rest(env, procs, layout_cache);
        }
    }

    // 2. Handle references to a known proc - again, we may be either aliasing the proc, or another
    //    alias to a proc.
    if procs.partial_procs.contains_key(right) {
        // This is an alias to a function defined in this module.
        // Attach the alias, then build the rest of the module, so that we reference and specialize
        // the correct proc.
        procs.partial_procs.insert_alias(left, right);
        return build_rest(env, procs, layout_cache);
    }

    // Otherwise we're dealing with an alias whose usages will tell us what specializations we
    // need. So let's figure those out first.
    let result = build_rest(env, procs, layout_cache);

    // The specializations we wanted of the symbol on the LHS of this alias.
    let needed_specializations_of_left = procs.symbol_specializations.remove(left);

    if procs.is_imported_module_thunk(right) {
        // if this is an imported symbol, then we must make sure it is
        // specialized, and wrap the original in a function pointer.
        let mut result = result;

        let no_specializations_needed = needed_specializations_of_left.len() == 0;
        let needed_specializations_of_left = needed_specializations_of_left
            .map(|(_, spec)| Some(spec))
            // HACK: sometimes specializations can be lost, for example for `x` in
            //   x = Bool.true
            //   p = \_ -> x == 1
            // that's because when specializing `p`, we collect specializations for `x`, but then
            // drop all of them when leaving the body of `p`, because `x` is an argument of `p` in
            // such a case.
            // So, if we have no recorded specializations, suppose we are in a case like this, and
            // generate the default implementation.
            //
            // TODO: we should fix this properly. I think the way to do it is to only have proc
            // specialization only drop specializations of non-captured symbols. That's because
            // captured symbols can only ever be specialized outside the closure.
            // After that is done, remove this hack.
            .chain(if no_specializations_needed {
                [Some((
                    variable,
                    left,
                    procs.specialization_stack.current_use_depth(),
                ))]
            } else {
                [None]
            })
            .flatten();

        for (variable, left, _deepest_use) in needed_specializations_of_left {
            add_needed_external(procs, env, variable, LambdaName::no_niche(right));

            let res_layout = layout_cache.from_var(env.arena, variable, env.subs);
            let layout = return_on_layout_error!(env, res_layout, "handle_variable_aliasing");

            result = force_thunk(env, right, layout, left, env.arena.alloc(result));
        }
        result
    } else if env.is_imported_symbol(right) {
        // if this is an imported symbol, then we must make sure it is
        // specialized, and wrap the original in a function pointer.
        add_needed_external(procs, env, variable, LambdaName::no_niche(right));

        // then we must construct its closure; since imported symbols have no closure, we use the empty struct
        let_empty_struct(left, env.arena.alloc(result))
    } else {
        // Otherwise, we are referencing a non-proc value.

        // We need to lift all specializations of "left" to be specializations of "right".
        let mut scratchpad_update_specializations = std::vec::Vec::new();

        let left_had_specialization_symbols = needed_specializations_of_left.len() > 0;

        for (specialization_mark, (specialized_var, specialized_sym, deepest_use)) in
            needed_specializations_of_left
        {
            let old_specialized_sym = procs.symbol_specializations.get_or_insert_known(
                right,
                specialization_mark,
                specialized_var,
                specialized_sym,
                deepest_use,
            );

            if let Some((_, old_specialized_sym, _)) = old_specialized_sym {
                scratchpad_update_specializations.push((old_specialized_sym, specialized_sym));
            }
        }

        let mut result = result;
        if left_had_specialization_symbols {
            // If the symbol is specialized, only the specializations need to be updated.
            for (old_specialized_sym, specialized_sym) in
                scratchpad_update_specializations.into_iter()
            {
                substitute_in_exprs(env.arena, &mut result, old_specialized_sym, specialized_sym);
            }
        } else {
            substitute_in_exprs(env.arena, &mut result, left, right);
        }

        result
    }
}

fn force_thunk<'a>(
    env: &mut Env<'a, '_>,
    thunk_name: Symbol,
    layout: Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let call = self::Call {
        call_type: CallType::ByName {
            name: LambdaName::no_niche(thunk_name),
            ret_layout: env.arena.alloc(layout),
            arg_layouts: &[],
            specialization_id: env.next_call_specialization_id(),
        },
        arguments: &[],
    };

    build_call(env, call, assigned, layout, env.arena.alloc(hole))
}

fn let_empty_struct<'a>(assigned: Symbol, hole: &'a Stmt<'a>) -> Stmt<'a> {
    Stmt::Let(assigned, Expr::Struct(&[]), Layout::UNIT, hole)
}

/// If the symbol is a function or polymorphic value, make sure it is properly specialized
fn specialize_symbol<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    arg_var: Option<Variable>,
    symbol: Symbol,
    result: Stmt<'a>,
    original: Symbol,
) -> Stmt<'a> {
    match procs.get_partial_proc(original) {
        None => {
            match arg_var {
                Some(arg_var)
                    if env.is_imported_symbol(original)
                        || env.is_unloaded_derived_symbol(original, procs) =>
                {
                    let raw = match layout_cache.raw_from_var(env.arena, arg_var, env.subs) {
                        Ok(v) => v,
                        Err(e) => return_on_layout_error_help!(env, e, "specialize_symbol"),
                    };

                    if procs.is_imported_module_thunk(original) {
                        let layout = match raw {
                            RawFunctionLayout::ZeroArgumentThunk(layout) => layout,
                            RawFunctionLayout::Function(_, lambda_set, _) => {
                                Layout::LambdaSet(lambda_set)
                            }
                        };

                        let raw = RawFunctionLayout::ZeroArgumentThunk(layout);
                        let top_level = ProcLayout::from_raw(
                            env.arena,
                            &layout_cache.interner,
                            raw,
                            CapturesNiche::no_niche(),
                        );

                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            LambdaName::no_niche(original),
                            top_level,
                            layout_cache,
                        );

                        force_thunk(env, original, layout, symbol, env.arena.alloc(result))
                    } else {
                        // Imported symbol, so it must have no captures niche (since
                        // top-levels can't capture)
                        let top_level = ProcLayout::from_raw(
                            env.arena,
                            &layout_cache.interner,
                            raw,
                            CapturesNiche::no_niche(),
                        );
                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            LambdaName::no_niche(original),
                            top_level,
                            layout_cache,
                        );

                        let_empty_struct(symbol, env.arena.alloc(result))
                    }
                }

                _ => {
                    // danger: a foreign symbol may not be specialized!
                    debug_assert!(
                        !env.is_imported_symbol(original),
                        "symbol {:?} while processing module {:?}",
                        original,
                        (env.home, &arg_var),
                    );
                    result
                }
            }
        }

        Some(partial_proc) => {
            let arg_var = arg_var.unwrap_or(partial_proc.annotation);
            // this symbol is a function, that is used by-name (e.g. as an argument to another
            // function). Register it with the current variable, then create a function pointer
            // to it in the IR.
            let res_layout = return_on_layout_error!(
                env,
                layout_cache.raw_from_var(env.arena, arg_var, env.subs),
                "specialize_symbol res_layout"
            );

            // we have three kinds of functions really. Plain functions, closures by capture,
            // and closures by unification. Here we record whether this function captures
            // anything.
            let captures = partial_proc.captured_symbols.captures();
            let captured = partial_proc.captured_symbols;

            match res_layout {
                RawFunctionLayout::Function(_, lambda_set, _) => {
                    if captures {
                        let symbols = match captured {
                            CapturedSymbols::Captured(captured_symbols) => {
                                Vec::from_iter_in(captured_symbols.iter(), env.arena)
                                    .into_bump_slice()
                            }
                            CapturedSymbols::None => unreachable!(),
                        };

                        let lambda_name = find_lambda_name(
                            env,
                            layout_cache,
                            lambda_set,
                            original,
                            symbols.iter().copied(),
                        );

                        // define the function pointer
                        let function_ptr_layout = ProcLayout::from_raw(
                            env.arena,
                            &layout_cache.interner,
                            res_layout,
                            lambda_name.captures_niche(),
                        );

                        // this is a closure by capture, meaning it itself captures local variables.
                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            lambda_name,
                            function_ptr_layout,
                            layout_cache,
                        );

                        let closure_data = symbol;

                        construct_closure_data(
                            env,
                            procs,
                            layout_cache,
                            lambda_set,
                            lambda_name,
                            symbols.iter().copied(),
                            closure_data,
                            env.arena.alloc(result),
                        )
                    } else if procs.is_module_thunk(original) {
                        // this is a 0-argument thunk

                        // TODO suspicious
                        // let layout = Layout::Closure(argument_layouts, lambda_set, ret_layout);
                        // panic!("suspicious");
                        let layout = Layout::LambdaSet(lambda_set);
                        let top_level =
                            ProcLayout::new(env.arena, &[], CapturesNiche::no_niche(), layout);
                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            LambdaName::no_niche(original),
                            top_level,
                            layout_cache,
                        );

                        force_thunk(env, original, layout, symbol, env.arena.alloc(result))
                    } else {
                        // even though this function may not itself capture,
                        // unification may still cause it to have an extra argument
                        let lambda_name =
                            find_lambda_name(env, layout_cache, lambda_set, original, &[]);

                        debug_assert!(lambda_name.no_captures());

                        // define the function pointer
                        let function_ptr_layout = ProcLayout::from_raw(
                            env.arena,
                            &layout_cache.interner,
                            res_layout,
                            lambda_name.captures_niche(),
                        );

                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            lambda_name,
                            function_ptr_layout,
                            layout_cache,
                        );

                        construct_closure_data(
                            env,
                            procs,
                            layout_cache,
                            lambda_set,
                            lambda_name,
                            &[],
                            symbol,
                            env.arena.alloc(result),
                        )
                    }
                }
                RawFunctionLayout::ZeroArgumentThunk(ret_layout) => {
                    // this is a 0-argument thunk
                    let top_level =
                        ProcLayout::new(env.arena, &[], CapturesNiche::no_niche(), ret_layout);
                    procs.insert_passed_by_name(
                        env,
                        arg_var,
                        LambdaName::no_niche(original),
                        top_level,
                        layout_cache,
                    );

                    force_thunk(env, original, ret_layout, symbol, env.arena.alloc(result))
                }
            }
        }
    }
}

fn assign_to_symbol<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    arg_var: Variable,
    loc_arg: Loc<roc_can::expr::Expr>,
    symbol: Symbol,
    result: Stmt<'a>,
) -> Stmt<'a> {
    use ReuseSymbol::*;
    match can_reuse_symbol(env, procs, &loc_arg.value, arg_var) {
        Imported(original) | LocalFunction(original) | UnspecializedExpr(original) => {
            // for functions we must make sure they are specialized correctly
            specialize_symbol(
                env,
                procs,
                layout_cache,
                Some(arg_var),
                symbol,
                result,
                original,
            )
        }
        Value(_symbol) => result,
        NotASymbol => with_hole(
            env,
            loc_arg.value,
            arg_var,
            procs,
            layout_cache,
            symbol,
            env.arena.alloc(result),
        ),
    }
}

fn assign_to_symbols<'a, I>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    iter: I,
    mut result: Stmt<'a>,
) -> Stmt<'a>
where
    I: Iterator<Item = ((Variable, Loc<roc_can::expr::Expr>), &'a Symbol)>,
{
    for ((arg_var, loc_arg), symbol) in iter {
        result = assign_to_symbol(env, procs, layout_cache, arg_var, loc_arg, *symbol, result);
    }

    result
}

fn add_needed_external<'a>(
    procs: &mut Procs<'a>,
    env: &mut Env<'a, '_>,
    fn_var: Variable,
    name: LambdaName<'a>,
) {
    // call of a function that is not in this module
    use hashbrown::hash_map::Entry::{Occupied, Vacant};

    let existing = match procs.externals_we_need.entry(name.name().module_id()) {
        Vacant(entry) => entry.insert(ExternalSpecializations::new()),
        Occupied(entry) => entry.into_mut(),
    };

    roc_tracing::debug!(proc_name = ?name, ?fn_var, fn_content = ?roc_types::subs::SubsFmtContent(env.subs.get_content_without_compacting(fn_var), env.subs), "needed external");

    existing.insert_external(name, env.subs, fn_var);
}

fn build_call<'a>(
    _env: &mut Env<'a, '_>,
    call: Call<'a>,
    assigned: Symbol,
    return_layout: Layout<'a>,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    Stmt::Let(assigned, Expr::Call(call), return_layout, hole)
}

/// See https://github.com/roc-lang/roc/issues/1549
///
/// What happened is that a function has a type error, but the arguments are not processed.
/// That means specializations were missing. Normally that is not a problem, but because
/// of our closure strategy, internal functions can "leak". That's what happened here.
///
/// The solution is to evaluate the arguments as normal, and only when calling the function give an error
fn evaluate_arguments_then_runtime_error<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    msg: String,
    loc_args: std::vec::Vec<(Variable, Loc<roc_can::expr::Expr>)>,
) -> Stmt<'a> {
    let arena = env.arena;

    // eventually we will throw this runtime error
    let result = runtime_error(env, env.arena.alloc(msg));

    // but, we also still evaluate and specialize the arguments to give better error messages
    let arg_symbols = Vec::from_iter_in(
        loc_args.iter().map(|(var, arg_expr)| {
            possible_reuse_symbol_or_specialize(env, procs, layout_cache, &arg_expr.value, *var)
        }),
        arena,
    )
    .into_bump_slice();

    let iter = loc_args.into_iter().rev().zip(arg_symbols.iter().rev());
    assign_to_symbols(env, procs, layout_cache, iter, result)
}

#[allow(clippy::too_many_arguments)]
fn call_by_name<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    fn_var: Variable,
    proc_name: Symbol,
    loc_args: std::vec::Vec<(Variable, Loc<roc_can::expr::Expr>)>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    // Register a pending_specialization for this function
    match layout_cache.raw_from_var(env.arena, fn_var, env.subs) {
        Err(LayoutProblem::UnresolvedTypeVar(var)) => {
            let msg = format!(
                "Hit an unresolved type variable {:?} when creating a layout for {:?} (var {:?})",
                var, proc_name, fn_var
            );

            evaluate_arguments_then_runtime_error(env, procs, layout_cache, msg, loc_args)
        }
        Err(LayoutProblem::Erroneous) => {
            let msg = format!(
                "Hit an erroneous type when creating a layout for {:?}",
                proc_name
            );

            evaluate_arguments_then_runtime_error(env, procs, layout_cache, msg, loc_args)
        }
        Ok(RawFunctionLayout::Function(arg_layouts, lambda_set, ret_layout)) => {
            if procs.is_module_thunk(proc_name) {
                if loc_args.is_empty() {
                    call_by_name_module_thunk(
                        env,
                        procs,
                        fn_var,
                        proc_name,
                        env.arena.alloc(Layout::LambdaSet(lambda_set)),
                        layout_cache,
                        assigned,
                        hole,
                    )
                } else {
                    // here we turn a call to a module thunk into forcing of that thunk
                    // the thunk represents the closure environment for the body, so we then match
                    // on the closure environment to perform the call that the body represents.
                    //
                    // Example:
                    //
                    // > main = parseA  "foo" "bar"
                    // > parseA = Str.concat

                    let closure_data_symbol = env.unique_symbol();

                    let arena = env.arena;
                    let arg_symbols = Vec::from_iter_in(
                        loc_args.iter().map(|(arg_var, arg_expr)| {
                            possible_reuse_symbol_or_specialize(
                                env,
                                procs,
                                layout_cache,
                                &arg_expr.value,
                                *arg_var,
                            )
                        }),
                        arena,
                    )
                    .into_bump_slice();

                    debug_assert_eq!(arg_symbols.len(), arg_layouts.len());

                    let result = match_on_lambda_set(
                        env,
                        layout_cache,
                        procs,
                        lambda_set,
                        closure_data_symbol,
                        arg_symbols,
                        arg_layouts,
                        ret_layout,
                        assigned,
                        hole,
                    );

                    let result = call_by_name_module_thunk(
                        env,
                        procs,
                        fn_var,
                        proc_name,
                        env.arena.alloc(Layout::LambdaSet(lambda_set)),
                        layout_cache,
                        closure_data_symbol,
                        env.arena.alloc(result),
                    );

                    let iter = loc_args.into_iter().rev().zip(arg_symbols.iter().rev());
                    assign_to_symbols(env, procs, layout_cache, iter, result)
                }
            } else {
                call_by_name_help(
                    env,
                    procs,
                    fn_var,
                    proc_name,
                    loc_args,
                    lambda_set,
                    arg_layouts,
                    ret_layout,
                    layout_cache,
                    assigned,
                    hole,
                )
            }
        }
        Ok(RawFunctionLayout::ZeroArgumentThunk(ret_layout)) => {
            if procs.is_module_thunk(proc_name) {
                // here we turn a call to a module thunk into  forcing of that thunk
                call_by_name_module_thunk(
                    env,
                    procs,
                    fn_var,
                    proc_name,
                    env.arena.alloc(ret_layout),
                    layout_cache,
                    assigned,
                    hole,
                )
            } else if env.is_imported_symbol(proc_name) {
                add_needed_external(procs, env, fn_var, LambdaName::no_niche(proc_name));
                force_thunk(env, proc_name, ret_layout, assigned, hole)
            } else {
                panic!("most likely we're trying to call something that is not a function");
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn call_by_name_help<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    fn_var: Variable,
    proc_name: Symbol,
    loc_args: std::vec::Vec<(Variable, Loc<roc_can::expr::Expr>)>,
    lambda_set: LambdaSet<'a>,
    argument_layouts: &'a [Layout<'a>],
    ret_layout: &'a Layout<'a>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let original_fn_var = fn_var;
    let arena = env.arena;

    // the arguments given to the function, stored in symbols
    let mut field_symbols = Vec::with_capacity_in(loc_args.len(), arena);
    field_symbols.extend(loc_args.iter().map(|(arg_var, arg_expr)| {
        possible_reuse_symbol_or_specialize(env, procs, layout_cache, &arg_expr.value, *arg_var)
    }));

    // THEORY: with a call by name, there are three options:
    //   - this is actually a thunk, and the lambda set is empty
    //   - the name references a function directly, like `main = \x -> ...`. In this case the
    //     lambda set includes only the function itself, and hence there is exactly one captures
    //     niche for the function.
    //   - the name references a value that yields a function, like
    //     `main = if b then \x -> .. else \y -> ..`. In that case the name being called never
    //     actually appears in the lambda set, and in fact has no capture set, and hence no
    //     captures niche.
    // So, if this function has any captures niche, it will be the first one.
    let mut iter_lambda_names = lambda_set
        .iter_set()
        .filter(|lam_name| lam_name.name() == proc_name);
    let proc_name = match iter_lambda_names.next() {
        Some(name) => {
            debug_assert!(
                iter_lambda_names.next().is_none(),
                "Somehow, call by name for {:?} has multiple capture niches: {:?}",
                proc_name,
                lambda_set
            );
            name
        }
        None => LambdaName::no_niche(proc_name),
    };

    // If required, add an extra argument to the layout that is the captured environment
    // afterwards, we MUST make sure the number of arguments in the layout matches the
    // number of arguments actually passed.
    let top_level_layout = {
        let argument_layouts =
            lambda_set.extend_argument_list(env.arena, &layout_cache.interner, argument_layouts);
        ProcLayout::new(
            env.arena,
            argument_layouts,
            proc_name.captures_niche(),
            *ret_layout,
        )
    };

    // the variables of the given arguments
    let mut pattern_vars = Vec::with_capacity_in(loc_args.len(), arena);
    for (var, _) in &loc_args {
        match layout_cache.from_var(env.arena, *var, env.subs) {
            Ok(_) => {
                pattern_vars.push(*var);
            }
            Err(_) => {
                // One of this function's arguments code gens to a runtime error,
                // so attempting to call it will immediately crash.
                return runtime_error(env, "TODO runtime error for invalid layout");
            }
        }
    }

    // If we've already specialized this one, no further work is needed.
    if procs
        .specialized
        .is_specialized(proc_name.name(), &top_level_layout)
    {
        debug_assert_eq!(
            argument_layouts.len(),
            field_symbols.len(),
            "see call_by_name for background (scroll down a bit), function is {:?}",
            proc_name,
        );
        call_specialized_proc(
            env,
            procs,
            proc_name,
            lambda_set,
            RawFunctionLayout::Function(argument_layouts, lambda_set, ret_layout),
            top_level_layout,
            field_symbols.into_bump_slice(),
            loc_args,
            layout_cache,
            assigned,
            hole,
        )
    } else if env.is_imported_symbol(proc_name.name())
        || env.is_unloaded_derived_symbol(proc_name.name(), procs)
    {
        add_needed_external(procs, env, original_fn_var, proc_name);

        debug_assert_ne!(proc_name.name().module_id(), ModuleId::ATTR);

        if procs.is_imported_module_thunk(proc_name.name()) {
            force_thunk(
                env,
                proc_name.name(),
                Layout::LambdaSet(lambda_set),
                assigned,
                hole,
            )
        } else if field_symbols.is_empty() {
            // this is a case like `Str.concat`, an imported standard function, applied to zero arguments

            // imported symbols cannot capture anything
            let captured = &[];
            debug_assert!(proc_name.no_captures());

            construct_closure_data(
                env,
                procs,
                layout_cache,
                lambda_set,
                proc_name,
                captured,
                assigned,
                hole,
            )
        } else {
            debug_assert_eq!(
                argument_layouts.len(),
                field_symbols.len(),
                "see call_by_name for background (scroll down a bit), function is {:?}",
                proc_name,
            );

            let field_symbols = field_symbols.into_bump_slice();

            let call = self::Call {
                call_type: CallType::ByName {
                    name: proc_name,
                    ret_layout,
                    arg_layouts: argument_layouts,
                    specialization_id: env.next_call_specialization_id(),
                },
                arguments: field_symbols,
            };

            let result = build_call(env, call, assigned, *ret_layout, hole);

            let iter = loc_args.into_iter().rev().zip(field_symbols.iter().rev());
            assign_to_symbols(env, procs, layout_cache, iter, result)
        }
    } else {
        // When requested (that is, when procs.pending_specializations is `Some`),
        // store a pending specialization rather than specializing immediately.
        //
        // We do this so that we can do specialization in two passes: first,
        // build the mono_expr with all the specialized calls in place (but
        // no specializations performed yet), and then second, *after*
        // de-duplicating requested specializations (since multiple modules
        // which could be getting monomorphized in parallel might request
        // the same specialization independently), we work through the
        // queue of pending specializations to complete each specialization
        // exactly once.
        if procs.is_module_thunk(proc_name.name()) {
            debug_assert!(top_level_layout.arguments.is_empty());
        }

        let needs_suspended_specialization =
            procs.symbol_needs_suspended_specialization(proc_name.name());
        match (
            &mut procs.pending_specializations,
            needs_suspended_specialization,
        ) {
            (PendingSpecializations::Finding(suspended), _)
            | (PendingSpecializations::Making(suspended), true) => {
                debug_assert!(!env.is_imported_symbol(proc_name.name()));

                // register the pending specialization, so this gets code genned later
                suspended.specialization(env.subs, proc_name, top_level_layout, fn_var);

                debug_assert_eq!(
                    argument_layouts.len(),
                    field_symbols.len(),
                    "see call_by_name for background (scroll down a bit), function is {:?}",
                    proc_name,
                );

                let field_symbols = field_symbols.into_bump_slice();

                call_specialized_proc(
                    env,
                    procs,
                    proc_name,
                    lambda_set,
                    RawFunctionLayout::Function(argument_layouts, lambda_set, ret_layout),
                    top_level_layout,
                    field_symbols,
                    loc_args,
                    layout_cache,
                    assigned,
                    hole,
                )
            }
            (PendingSpecializations::Making(_), false) => {
                let opt_partial_proc = procs.partial_procs.symbol_to_id(proc_name.name());

                let field_symbols = field_symbols.into_bump_slice();

                match opt_partial_proc {
                    Some(partial_proc) => {
                        // Mark this proc as in-progress, so if we're dealing with
                        // mutually recursive functions, we don't loop forever.
                        // (We had a bug around this before this system existed!)
                        procs
                            .specialized
                            .mark_in_progress(proc_name.name(), top_level_layout);

                        match specialize_variable(
                            env,
                            procs,
                            proc_name,
                            layout_cache,
                            fn_var,
                            &[],
                            partial_proc,
                        ) {
                            Ok((proc, layout)) => {
                                let proc_name = proc.name;
                                let function_layout = ProcLayout::from_raw(
                                    env.arena,
                                    &layout_cache.interner,
                                    layout,
                                    proc_name.captures_niche(),
                                );
                                procs.specialized.insert_specialized(
                                    proc_name.name(),
                                    function_layout,
                                    proc,
                                );

                                // now we just call our freshly-specialized function
                                call_specialized_proc(
                                    env,
                                    procs,
                                    proc_name,
                                    lambda_set,
                                    layout,
                                    function_layout,
                                    field_symbols,
                                    loc_args,
                                    layout_cache,
                                    assigned,
                                    hole,
                                )
                            }
                            Err(SpecializeFailure { attempted_layout }) => {
                                let proc = generate_runtime_error_function(
                                    env,
                                    layout_cache,
                                    proc_name.name(),
                                    attempted_layout,
                                );

                                let proc_name = proc.name;
                                let function_layout = ProcLayout::from_raw(
                                    env.arena,
                                    &layout_cache.interner,
                                    attempted_layout,
                                    proc_name.captures_niche(),
                                );
                                procs.specialized.insert_specialized(
                                    proc_name.name(),
                                    function_layout,
                                    proc,
                                );

                                call_specialized_proc(
                                    env,
                                    procs,
                                    proc_name,
                                    lambda_set,
                                    attempted_layout,
                                    function_layout,
                                    field_symbols,
                                    loc_args,
                                    layout_cache,
                                    assigned,
                                    hole,
                                )
                            }
                        }
                    }

                    None => {
                        unreachable!("Proc name {:?} is invalid", proc_name)
                    }
                }
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn call_by_name_module_thunk<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    fn_var: Variable,
    proc_name: Symbol,
    ret_layout: &'a Layout<'a>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let top_level_layout = ProcLayout::new(env.arena, &[], CapturesNiche::no_niche(), *ret_layout);

    let inner_layout = *ret_layout;

    // If we've already specialized this one, no further work is needed.
    let already_specialized = procs
        .specialized
        .is_specialized(proc_name, &top_level_layout);

    if already_specialized {
        force_thunk(env, proc_name, inner_layout, assigned, hole)
    } else {
        // When requested (that is, when procs.pending_specializations is `Some`),
        // store a pending specialization rather than specializing immediately.
        //
        // We do this so that we can do specialization in two passes: first,
        // build the mono_expr with all the specialized calls in place (but
        // no specializations performed yet), and then second, *after*
        // de-duplicating requested specializations (since multiple modules
        // which could be getting monomorphized in parallel might request
        // the same specialization independently), we work through the
        // queue of pending specializations to complete each specialization
        // exactly once.
        if procs.is_module_thunk(proc_name) {
            debug_assert!(top_level_layout.arguments.is_empty());
        }

        let needs_suspended_specialization = procs.symbol_needs_suspended_specialization(proc_name);
        match (
            &mut procs.pending_specializations,
            needs_suspended_specialization,
        ) {
            (PendingSpecializations::Finding(suspended), _)
            | (PendingSpecializations::Making(suspended), true) => {
                debug_assert!(!env.is_imported_symbol(proc_name));

                // register the pending specialization, so this gets code genned later
                suspended.specialization(
                    env.subs,
                    LambdaName::no_niche(proc_name),
                    top_level_layout,
                    fn_var,
                );

                force_thunk(env, proc_name, inner_layout, assigned, hole)
            }
            (PendingSpecializations::Making(_), false) => {
                let opt_partial_proc = procs.partial_procs.symbol_to_id(proc_name);

                match opt_partial_proc {
                    Some(partial_proc) => {
                        // Mark this proc as in-progress, so if we're dealing with
                        // mutually recursive functions, we don't loop forever.
                        // (We had a bug around this before this system existed!)
                        procs
                            .specialized
                            .mark_in_progress(proc_name, top_level_layout);

                        match specialize_variable(
                            env,
                            procs,
                            LambdaName::no_niche(proc_name),
                            layout_cache,
                            fn_var,
                            &[],
                            partial_proc,
                        ) {
                            Ok((proc, raw_layout)) => {
                                debug_assert!(
                                    raw_layout.is_zero_argument_thunk(),
                                    "but actually {:?}",
                                    raw_layout
                                );

                                let was_present = procs
                                    .specialized
                                    .remove_specialized(proc_name, &top_level_layout);
                                debug_assert!(was_present);

                                procs.specialized.insert_specialized(
                                    proc_name,
                                    top_level_layout,
                                    proc,
                                );

                                force_thunk(env, proc_name, inner_layout, assigned, hole)
                            }
                            Err(SpecializeFailure { attempted_layout }) => {
                                let proc = generate_runtime_error_function(
                                    env,
                                    layout_cache,
                                    proc_name,
                                    attempted_layout,
                                );

                                let was_present = procs
                                    .specialized
                                    .remove_specialized(proc_name, &top_level_layout);
                                debug_assert!(was_present);

                                procs.specialized.insert_specialized(
                                    proc_name,
                                    top_level_layout,
                                    proc,
                                );

                                force_thunk(env, proc_name, inner_layout, assigned, hole)
                            }
                        }
                    }

                    None => {
                        unreachable!("Proc name {:?} is invalid", proc_name)
                    }
                }
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn call_specialized_proc<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    proc_name: LambdaName<'a>,
    lambda_set: LambdaSet<'a>,
    layout: RawFunctionLayout<'a>,
    function_layout: ProcLayout<'a>,
    field_symbols: &'a [Symbol],
    loc_args: std::vec::Vec<(Variable, Loc<roc_can::expr::Expr>)>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    if field_symbols.is_empty() {
        debug_assert!(loc_args.is_empty());

        // This happens when we return a function, e.g.
        //
        // foo = Num.add
        //
        // Even though the layout (and type) are functions,
        // there are no arguments. This confuses our IR,
        // and we have to fix it here.
        match layout {
            RawFunctionLayout::Function(_, lambda_set, _) => {
                // when the body is a closure, the function will return the closure environment
                let call = self::Call {
                    call_type: CallType::ByName {
                        name: proc_name,
                        ret_layout: env.arena.alloc(function_layout.result),
                        arg_layouts: function_layout.arguments,
                        specialization_id: env.next_call_specialization_id(),
                    },
                    arguments: field_symbols,
                };

                // the closure argument is already added here (to get the right specialization)
                // but now we need to remove it because the `match_on_lambda_set` will add it again
                build_call(env, call, assigned, Layout::LambdaSet(lambda_set), hole)
            }
            RawFunctionLayout::ZeroArgumentThunk(_) => {
                unreachable!()
            }
        }
    } else {
        let iter = loc_args.into_iter().rev().zip(field_symbols.iter().rev());

        match procs
            .partial_procs
            .get_symbol(proc_name.name())
            .map(|pp| &pp.captured_symbols)
        {
            Some(&CapturedSymbols::Captured(captured_symbols)) => {
                let symbols =
                    Vec::from_iter_in(captured_symbols.iter(), env.arena).into_bump_slice();

                let closure_data_symbol = env.unique_symbol();

                // the closure argument is already added here (to get the right specialization)
                // but now we need to remove it because the `match_on_lambda_set` will add it again
                let mut argument_layouts =
                    Vec::from_iter_in(function_layout.arguments.iter().copied(), env.arena);
                argument_layouts.pop().unwrap();

                debug_assert_eq!(argument_layouts.len(), field_symbols.len(),);

                let new_hole = match_on_lambda_set(
                    env,
                    layout_cache,
                    procs,
                    lambda_set,
                    closure_data_symbol,
                    field_symbols,
                    argument_layouts.into_bump_slice(),
                    env.arena.alloc(function_layout.result),
                    assigned,
                    hole,
                );

                let result = construct_closure_data(
                    env,
                    procs,
                    layout_cache,
                    lambda_set,
                    proc_name,
                    symbols.iter().copied(),
                    closure_data_symbol,
                    env.arena.alloc(new_hole),
                );

                assign_to_symbols(env, procs, layout_cache, iter, result)
            }
            _ => {
                debug_assert_eq!(
                    function_layout.arguments.len(),
                    field_symbols.len(),
                    "function {:?} with layout {:#?} expects {:?} arguments, but is applied to {:?}",
                    proc_name,
                    function_layout,
                    function_layout.arguments.len(),
                    field_symbols.len(),
                );

                let call = self::Call {
                    call_type: CallType::ByName {
                        name: proc_name,
                        ret_layout: env.arena.alloc(function_layout.result),
                        arg_layouts: function_layout.arguments,
                        specialization_id: env.next_call_specialization_id(),
                    },
                    arguments: field_symbols,
                };

                let result = build_call(env, call, assigned, function_layout.result, hole);

                assign_to_symbols(env, procs, layout_cache, iter, result)
            }
        }
    }
}

/// A pattern, including possible problems (e.g. shadowing) so that
/// codegen can generate a runtime error if this pattern is reached.
#[derive(Clone, Debug, PartialEq)]
pub enum Pattern<'a> {
    Identifier(Symbol),
    Underscore,
    IntLiteral([u8; 16], IntWidth),
    FloatLiteral(u64, FloatWidth),
    DecimalLiteral([u8; 16]),
    BitLiteral {
        value: bool,
        tag_name: TagName,
        union: roc_exhaustive::Union,
    },
    EnumLiteral {
        tag_id: u8,
        tag_name: TagName,
        union: roc_exhaustive::Union,
    },
    StrLiteral(Box<str>),

    RecordDestructure(Vec<'a, RecordDestruct<'a>>, &'a [Layout<'a>]),
    NewtypeDestructure {
        tag_name: TagName,
        arguments: Vec<'a, (Pattern<'a>, Layout<'a>)>,
    },
    AppliedTag {
        tag_name: TagName,
        tag_id: TagIdIntType,
        arguments: Vec<'a, (Pattern<'a>, Layout<'a>)>,
        layout: UnionLayout<'a>,
        union: roc_exhaustive::Union,
    },
    Voided {
        tag_name: TagName,
    },
    OpaqueUnwrap {
        opaque: Symbol,
        argument: Box<(Pattern<'a>, Layout<'a>)>,
    },
    List {
        arity: ListArity,
        element_layout: Layout<'a>,
        elements: Vec<'a, Pattern<'a>>,
    },
}

impl<'a> Pattern<'a> {
    /// This pattern contains a pattern match on Void (i.e. [], the empty tag union)
    /// such branches are not reachable at runtime
    pub fn is_voided(&self) -> bool {
        let mut stack: std::vec::Vec<&Pattern> = vec![self];

        while let Some(pattern) = stack.pop() {
            match pattern {
                Pattern::Identifier(_)
                | Pattern::Underscore
                | Pattern::IntLiteral(_, _)
                | Pattern::FloatLiteral(_, _)
                | Pattern::DecimalLiteral(_)
                | Pattern::BitLiteral { .. }
                | Pattern::EnumLiteral { .. }
                | Pattern::StrLiteral(_) => { /* terminal */ }
                Pattern::RecordDestructure(destructs, _) => {
                    for destruct in destructs {
                        match &destruct.typ {
                            DestructType::Required(_) => { /* do nothing */ }
                            DestructType::Guard(pattern) => {
                                stack.push(pattern);
                            }
                        }
                    }
                }
                Pattern::NewtypeDestructure { arguments, .. } => {
                    stack.extend(arguments.iter().map(|(t, _)| t))
                }
                Pattern::Voided { .. } => return true,
                Pattern::AppliedTag { arguments, .. } => {
                    stack.extend(arguments.iter().map(|(t, _)| t))
                }
                Pattern::OpaqueUnwrap { argument, .. } => stack.push(&argument.0),
                Pattern::List { elements, .. } => stack.extend(elements),
            }
        }

        false
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RecordDestruct<'a> {
    pub label: Lowercase,
    pub variable: Variable,
    pub layout: Layout<'a>,
    pub typ: DestructType<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DestructType<'a> {
    Required(Symbol),
    Guard(Pattern<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhenBranch<'a> {
    pub patterns: Vec<'a, Pattern<'a>>,
    pub value: Expr<'a>,
    pub guard: Option<Stmt<'a>>,
}

#[allow(clippy::type_complexity)]
fn from_can_pattern<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_pattern: &roc_can::pattern::Pattern,
) -> Result<
    (
        Pattern<'a>,
        Vec<'a, (Symbol, Variable, roc_can::expr::Expr)>,
    ),
    RuntimeError,
> {
    let mut assignments = Vec::new_in(env.arena);
    let pattern = from_can_pattern_help(env, procs, layout_cache, can_pattern, &mut assignments)?;

    Ok((pattern, assignments))
}

fn from_can_pattern_help<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_pattern: &roc_can::pattern::Pattern,
    assignments: &mut Vec<'a, (Symbol, Variable, roc_can::expr::Expr)>,
) -> Result<Pattern<'a>, RuntimeError> {
    use roc_can::pattern::Pattern::*;

    match can_pattern {
        Underscore => Ok(Pattern::Underscore),
        Identifier(symbol) => Ok(Pattern::Identifier(*symbol)),
        AbilityMemberSpecialization { ident, .. } => Ok(Pattern::Identifier(*ident)),
        IntLiteral(var, _, int_str, int, _bound) => Ok(make_num_literal_pattern(
            env,
            layout_cache,
            *var,
            int_str,
            IntOrFloatValue::Int(*int),
        )),
        FloatLiteral(var, _, float_str, float, _bound) => Ok(make_num_literal_pattern(
            env,
            layout_cache,
            *var,
            float_str,
            IntOrFloatValue::Float(*float),
        )),
        StrLiteral(v) => Ok(Pattern::StrLiteral(v.clone())),
        SingleQuote(var, _, c, _) => match layout_cache.from_var(env.arena, *var, env.subs) {
            Ok(Layout::Builtin(Builtin::Int(width))) => {
                Ok(Pattern::IntLiteral((*c as i128).to_ne_bytes(), width))
            }
            o => internal_error!("an integer width was expected, but we found {:?}", o),
        },
        Shadowed(region, ident, _new_symbol) => Err(RuntimeError::Shadowing {
            original_region: *region,
            shadow: ident.clone(),
            kind: ShadowKind::Variable,
        }),
        UnsupportedPattern(region) => Err(RuntimeError::UnsupportedPattern(*region)),
        MalformedPattern(_problem, region) => {
            // TODO preserve malformed problem information here?
            Err(RuntimeError::UnsupportedPattern(*region))
        }
        OpaqueNotInScope(loc_ident) => {
            // TODO(opaques) should be `RuntimeError::OpaqueNotDefined`
            Err(RuntimeError::UnsupportedPattern(loc_ident.region))
        }
        NumLiteral(var, num_str, num, _bound) => Ok(make_num_literal_pattern(
            env,
            layout_cache,
            *var,
            num_str,
            IntOrFloatValue::Int(*num),
        )),

        AppliedTag {
            whole_var,
            tag_name,
            arguments,
            ..
        } => {
            use crate::layout::UnionVariant::*;
            use roc_exhaustive::Union;

            let res_variant = {
                let mut layout_env = layout::Env::from_components(
                    layout_cache,
                    env.subs,
                    env.arena,
                    env.target_info,
                );
                crate::layout::union_sorted_tags(&mut layout_env, *whole_var).map_err(Into::into)
            };

            let variant = match res_variant {
                Ok(cached) => cached,
                Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                    return Err(RuntimeError::UnresolvedTypeVar)
                }
                Err(LayoutProblem::Erroneous) => return Err(RuntimeError::ErroneousType),
            };

            let result = match variant {
                Never => unreachable!(
                    "there is no pattern of type `[]`, union var {:?}",
                    *whole_var
                ),
                Unit => Pattern::EnumLiteral {
                    tag_id: 0,
                    tag_name: tag_name.clone(),
                    union: Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![Ctor {
                            tag_id: TagId(0),
                            name: CtorName::Tag(tag_name.clone()),
                            arity: 0,
                        }],
                    },
                },
                BoolUnion { ttrue, ffalse } => {
                    let (ttrue, ffalse) = (ttrue.expect_tag(), ffalse.expect_tag());
                    Pattern::BitLiteral {
                        value: tag_name == &ttrue,
                        tag_name: tag_name.clone(),
                        union: Union {
                            render_as: RenderAs::Tag,
                            alternatives: vec![
                                Ctor {
                                    tag_id: TagId(0),
                                    name: CtorName::Tag(ffalse),
                                    arity: 0,
                                },
                                Ctor {
                                    tag_id: TagId(1),
                                    name: CtorName::Tag(ttrue),
                                    arity: 0,
                                },
                            ],
                        },
                    }
                }
                ByteUnion(tag_names) => {
                    let tag_id = tag_names
                        .iter()
                        .position(|key| tag_name == key.expect_tag_ref())
                        .expect("tag must be in its own type");

                    let mut ctors = std::vec::Vec::with_capacity(tag_names.len());
                    for (i, tag_name) in tag_names.into_iter().enumerate() {
                        ctors.push(Ctor {
                            tag_id: TagId(i as _),
                            name: CtorName::Tag(tag_name.expect_tag()),
                            arity: 0,
                        })
                    }

                    let union = roc_exhaustive::Union {
                        render_as: RenderAs::Tag,
                        alternatives: ctors,
                    };

                    Pattern::EnumLiteral {
                        tag_id: tag_id as u8,
                        tag_name: tag_name.clone(),
                        union,
                    }
                }
                Newtype {
                    arguments: field_layouts,
                    ..
                } => {
                    let mut arguments = arguments.clone();

                    arguments.sort_by(|arg1, arg2| {
                        let size1 = layout_cache
                            .from_var(env.arena, arg1.0, env.subs)
                            .map(|x| x.alignment_bytes(&layout_cache.interner, env.target_info))
                            .unwrap_or(0);

                        let size2 = layout_cache
                            .from_var(env.arena, arg2.0, env.subs)
                            .map(|x| x.alignment_bytes(&layout_cache.interner, env.target_info))
                            .unwrap_or(0);

                        size2.cmp(&size1)
                    });

                    let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);
                    for ((_, loc_pat), layout) in arguments.iter().zip(field_layouts.iter()) {
                        mono_args.push((
                            from_can_pattern_help(
                                env,
                                procs,
                                layout_cache,
                                &loc_pat.value,
                                assignments,
                            )?,
                            *layout,
                        ));
                    }

                    Pattern::NewtypeDestructure {
                        tag_name: tag_name.clone(),
                        arguments: mono_args,
                    }
                }
                NewtypeByVoid {
                    data_tag_arguments,
                    data_tag_name,
                    ..
                } => {
                    let data_tag_name = data_tag_name.expect_tag();

                    if tag_name != &data_tag_name {
                        // this tag is not represented at runtime
                        Pattern::Voided {
                            tag_name: tag_name.clone(),
                        }
                    } else {
                        let mut arguments = arguments.clone();

                        arguments.sort_by(|arg1, arg2| {
                            let size1 = layout_cache
                                .from_var(env.arena, arg1.0, env.subs)
                                .map(|x| x.alignment_bytes(&layout_cache.interner, env.target_info))
                                .unwrap_or(0);

                            let size2 = layout_cache
                                .from_var(env.arena, arg2.0, env.subs)
                                .map(|x| x.alignment_bytes(&layout_cache.interner, env.target_info))
                                .unwrap_or(0);

                            size2.cmp(&size1)
                        });

                        let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);
                        let it = arguments.iter().zip(data_tag_arguments.iter());
                        for ((_, loc_pat), layout) in it {
                            mono_args.push((
                                from_can_pattern_help(
                                    env,
                                    procs,
                                    layout_cache,
                                    &loc_pat.value,
                                    assignments,
                                )?,
                                *layout,
                            ));
                        }

                        Pattern::NewtypeDestructure {
                            tag_name: tag_name.clone(),
                            arguments: mono_args,
                        }
                    }
                }

                Wrapped(variant) => {
                    let (tag_id, argument_layouts) = variant.tag_name_to_id(tag_name);
                    let number_of_tags = variant.number_of_tags();
                    let mut ctors = std::vec::Vec::with_capacity(number_of_tags);

                    let arguments = {
                        let mut temp = arguments.clone();

                        temp.sort_by(|arg1, arg2| {
                            let layout1 =
                                layout_cache.from_var(env.arena, arg1.0, env.subs).unwrap();
                            let layout2 =
                                layout_cache.from_var(env.arena, arg2.0, env.subs).unwrap();

                            let size1 =
                                layout1.alignment_bytes(&layout_cache.interner, env.target_info);
                            let size2 =
                                layout2.alignment_bytes(&layout_cache.interner, env.target_info);

                            size2.cmp(&size1)
                        });

                        temp
                    };

                    // we must derive the union layout from the whole_var, building it up
                    // from `layouts` would unroll recursive tag unions, and that leads to
                    // problems down the line because we hash layouts and an unrolled
                    // version is not the same as the minimal version.
                    let layout = match layout_cache.from_var(env.arena, *whole_var, env.subs) {
                        Ok(Layout::Union(ul)) => ul,
                        _ => unreachable!(),
                    };

                    use WrappedVariant::*;
                    match variant {
                        NonRecursive {
                            sorted_tag_layouts: ref tags,
                        } => {
                            debug_assert!(tags.len() > 1);

                            for (i, (tag_name, args)) in tags.iter().enumerate() {
                                ctors.push(Ctor {
                                    tag_id: TagId(i as _),
                                    name: CtorName::Tag(tag_name.expect_tag_ref().clone()),
                                    arity: args.len(),
                                })
                            }

                            let union = roc_exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            debug_assert_eq!(
                                arguments.len(),
                                argument_layouts.len(),
                                "The {:?} tag got {} arguments, but its layout expects {}!",
                                tag_name,
                                arguments.len(),
                                argument_layouts.len(),
                            );
                            let it = argument_layouts.iter();

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
                                        procs,
                                        layout_cache,
                                        &loc_pat.value,
                                        assignments,
                                    )?,
                                    *layout,
                                ));
                            }

                            Pattern::AppliedTag {
                                tag_name: tag_name.clone(),
                                tag_id: tag_id as _,
                                arguments: mono_args,
                                union,
                                layout,
                            }
                        }

                        Recursive {
                            sorted_tag_layouts: ref tags,
                        } => {
                            debug_assert!(tags.len() > 1);

                            for (i, (tag_name, args)) in tags.iter().enumerate() {
                                ctors.push(Ctor {
                                    tag_id: TagId(i as _),
                                    name: CtorName::Tag(tag_name.expect_tag_ref().clone()),
                                    // don't include tag discriminant in arity
                                    arity: args.len() - 1,
                                })
                            }

                            let union = roc_exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            debug_assert_eq!(arguments.len(), argument_layouts.len());
                            let it = argument_layouts.iter();

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
                                        procs,
                                        layout_cache,
                                        &loc_pat.value,
                                        assignments,
                                    )?,
                                    *layout,
                                ));
                            }

                            Pattern::AppliedTag {
                                tag_name: tag_name.clone(),
                                tag_id: tag_id as _,
                                arguments: mono_args,
                                union,
                                layout,
                            }
                        }

                        NonNullableUnwrapped {
                            tag_name: w_tag_name,
                            fields,
                        } => {
                            debug_assert_eq!(w_tag_name.expect_tag_ref(), tag_name);

                            ctors.push(Ctor {
                                tag_id: TagId(0),
                                name: CtorName::Tag(tag_name.clone()),
                                arity: fields.len(),
                            });

                            let union = roc_exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            debug_assert_eq!(arguments.len(), argument_layouts.len());
                            let it = argument_layouts.iter();

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
                                        procs,
                                        layout_cache,
                                        &loc_pat.value,
                                        assignments,
                                    )?,
                                    *layout,
                                ));
                            }

                            Pattern::AppliedTag {
                                tag_name: tag_name.clone(),
                                tag_id: tag_id as _,
                                arguments: mono_args,
                                union,
                                layout,
                            }
                        }

                        NullableWrapped {
                            sorted_tag_layouts: ref tags,
                            nullable_id,
                            nullable_name,
                        } => {
                            debug_assert!(!tags.is_empty());

                            let mut i = 0;
                            for (tag_name, args) in tags.iter() {
                                if i == nullable_id as usize {
                                    ctors.push(Ctor {
                                        tag_id: TagId(i as _),
                                        name: CtorName::Tag(nullable_name.expect_tag_ref().clone()),
                                        // don't include tag discriminant in arity
                                        arity: 0,
                                    });

                                    i += 1;
                                }

                                ctors.push(Ctor {
                                    tag_id: TagId(i as _),
                                    name: CtorName::Tag(tag_name.expect_tag_ref().clone()),
                                    // don't include tag discriminant in arity
                                    arity: args.len() - 1,
                                });

                                i += 1;
                            }

                            if i == nullable_id as usize {
                                ctors.push(Ctor {
                                    tag_id: TagId(i as _),
                                    name: CtorName::Tag(nullable_name.expect_tag_ref().clone()),
                                    // don't include tag discriminant in arity
                                    arity: 0,
                                });
                            }

                            let union = roc_exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            let it = if tag_name == nullable_name.expect_tag_ref() {
                                [].iter()
                            } else {
                                argument_layouts.iter()
                            };

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
                                        procs,
                                        layout_cache,
                                        &loc_pat.value,
                                        assignments,
                                    )?,
                                    *layout,
                                ));
                            }

                            Pattern::AppliedTag {
                                tag_name: tag_name.clone(),
                                tag_id: tag_id as _,
                                arguments: mono_args,
                                union,
                                layout,
                            }
                        }

                        NullableUnwrapped {
                            other_fields,
                            nullable_id,
                            nullable_name,
                            other_name: _,
                        } => {
                            debug_assert!(!other_fields.is_empty());

                            ctors.push(Ctor {
                                tag_id: TagId(nullable_id as _),
                                name: CtorName::Tag(nullable_name.expect_tag_ref().clone()),
                                arity: 0,
                            });

                            ctors.push(Ctor {
                                tag_id: TagId(!nullable_id as _),
                                name: CtorName::Tag(nullable_name.expect_tag_ref().clone()),
                                // FIXME drop tag
                                arity: other_fields.len() - 1,
                            });

                            let union = roc_exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            let it = if tag_name == nullable_name.expect_tag_ref() {
                                [].iter()
                            } else {
                                // FIXME drop tag
                                argument_layouts.iter()
                            };

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
                                        procs,
                                        layout_cache,
                                        &loc_pat.value,
                                        assignments,
                                    )?,
                                    *layout,
                                ));
                            }

                            Pattern::AppliedTag {
                                tag_name: tag_name.clone(),
                                tag_id: tag_id as _,
                                arguments: mono_args,
                                union,
                                layout,
                            }
                        }
                    }
                }
            };

            Ok(result)
        }

        UnwrappedOpaque {
            opaque, argument, ..
        } => {
            let (arg_var, loc_arg_pattern) = &(**argument);
            let arg_layout = layout_cache
                .from_var(env.arena, *arg_var, env.subs)
                .unwrap();
            let mono_arg_pattern = from_can_pattern_help(
                env,
                procs,
                layout_cache,
                &loc_arg_pattern.value,
                assignments,
            )?;
            Ok(Pattern::OpaqueUnwrap {
                opaque: *opaque,
                argument: Box::new((mono_arg_pattern, arg_layout)),
            })
        }

        RecordDestructure {
            whole_var,
            destructs,
            ..
        } => {
            // sorted fields based on the type
            let sorted_fields = {
                let mut layout_env = layout::Env::from_components(
                    layout_cache,
                    env.subs,
                    env.arena,
                    env.target_info,
                );
                crate::layout::sort_record_fields(&mut layout_env, *whole_var)
                    .map_err(RuntimeError::from)?
            };

            // sorted fields based on the destruct
            let mut mono_destructs = Vec::with_capacity_in(destructs.len(), env.arena);
            let mut destructs_by_label = BumpMap::with_capacity_in(destructs.len(), env.arena);
            destructs_by_label.extend(destructs.iter().map(|x| (&x.value.label, x)));

            let mut field_layouts = Vec::with_capacity_in(sorted_fields.len(), env.arena);

            // next we step through both sequences of fields. The outer loop is the sequence based
            // on the type, since not all fields need to actually be destructured in the source
            // language.
            //
            // However in mono patterns, we do destruct all patterns (but use Underscore) when
            // in the source the field is not matche in the source language.
            //
            // Optional fields somewhat complicate the matter here

            for (label, variable, res_layout) in sorted_fields.into_iter() {
                match res_layout {
                    Ok(field_layout) => {
                        // the field is non-optional according to the type

                        match destructs_by_label.remove(&label) {
                            Some(destruct) => {
                                // this field is destructured by the pattern
                                mono_destructs.push(from_can_record_destruct(
                                    env,
                                    procs,
                                    layout_cache,
                                    &destruct.value,
                                    field_layout,
                                    assignments,
                                )?);
                            }
                            None => {
                                // this field is not destructured by the pattern
                                // put in an underscore
                                mono_destructs.push(RecordDestruct {
                                    label: label.clone(),
                                    variable,
                                    layout: field_layout,
                                    typ: DestructType::Guard(Pattern::Underscore),
                                });
                            }
                        }

                        // the layout of this field is part of the layout of the record
                        field_layouts.push(field_layout);
                    }
                    Err(field_layout) => {
                        // the field is optional according to the type
                        match destructs_by_label.remove(&label) {
                            Some(destruct) => {
                                // this field is destructured by the pattern
                                match &destruct.value.typ {
                                    roc_can::pattern::DestructType::Optional(_, loc_expr) => {
                                        // if we reach this stage, the optional field is not present
                                        // so we push the default assignment into the branch
                                        assignments.push((
                                            destruct.value.symbol,
                                            variable,
                                            loc_expr.value.clone(),
                                        ));
                                    }
                                    _ => unreachable!(
                                        "only optional destructs can be optional fields"
                                    ),
                                };
                            }
                            None => {
                                // this field is not destructured by the pattern
                                // put in an underscore
                                mono_destructs.push(RecordDestruct {
                                    label: label.clone(),
                                    variable,
                                    layout: field_layout,
                                    typ: DestructType::Guard(Pattern::Underscore),
                                });
                            }
                        }
                    }
                }
            }

            for (_, destruct) in destructs_by_label.drain() {
                // this destruct is not in the type, but is in the pattern
                // it must be an optional field, and we will use the default
                match &destruct.value.typ {
                    roc_can::pattern::DestructType::Optional(field_var, loc_expr) => {
                        // TODO these don't match up in the uniqueness inference; when we remove
                        // that, reinstate this assert!
                        //
                        // dbg!(&env.subs.get_content_without_compacting(*field_var));
                        // dbg!(&env.subs.get_content_without_compacting(destruct.var).content);
                        // debug_assert_eq!(
                        //     env.subs.get_root_key_without_compacting(*field_var),
                        //     env.subs.get_root_key_without_compacting(destruct.value.var)
                        // );
                        assignments.push((
                            destruct.value.symbol,
                            // destruct.value.var,
                            *field_var,
                            loc_expr.value.clone(),
                        ));
                    }
                    _ => unreachable!("only optional destructs can be optional fields"),
                }
            }

            Ok(Pattern::RecordDestructure(
                mono_destructs,
                field_layouts.into_bump_slice(),
            ))
        }

        List {
            list_var: _,
            elem_var,
            patterns,
        } => {
            let element_layout = match layout_cache.from_var(env.arena, *elem_var, env.subs) {
                Ok(lay) => lay,
                Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                    return Err(RuntimeError::UnresolvedTypeVar)
                }
                Err(LayoutProblem::Erroneous) => return Err(RuntimeError::ErroneousType),
            };

            let arity = patterns.arity();

            let mut mono_patterns = Vec::with_capacity_in(patterns.patterns.len(), env.arena);
            for loc_pat in patterns.patterns.iter() {
                let mono_pat =
                    from_can_pattern_help(env, procs, layout_cache, &loc_pat.value, assignments)?;
                mono_patterns.push(mono_pat);
            }

            Ok(Pattern::List {
                arity,
                element_layout,
                elements: mono_patterns,
            })
        }
    }
}

fn from_can_record_destruct<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    can_rd: &roc_can::pattern::RecordDestruct,
    field_layout: Layout<'a>,
    assignments: &mut Vec<'a, (Symbol, Variable, roc_can::expr::Expr)>,
) -> Result<RecordDestruct<'a>, RuntimeError> {
    Ok(RecordDestruct {
        label: can_rd.label.clone(),
        variable: can_rd.var,
        layout: field_layout,
        typ: match &can_rd.typ {
            roc_can::pattern::DestructType::Required => DestructType::Required(can_rd.symbol),
            roc_can::pattern::DestructType::Optional(_, _) => {
                // if we reach this stage, the optional field is present
                DestructType::Required(can_rd.symbol)
            }
            roc_can::pattern::DestructType::Guard(_, loc_pattern) => DestructType::Guard(
                from_can_pattern_help(env, procs, layout_cache, &loc_pattern.value, assignments)?,
            ),
        },
    })
}

enum IntOrFloatValue {
    Int(IntValue),
    Float(f64),
}

enum NumLiteral {
    Int([u8; 16], IntWidth),
    U128([u8; 16]),
    Float(f64, FloatWidth),
    Decimal([u8; 16]),
}

impl NumLiteral {
    fn to_expr_literal(&self) -> Literal<'static> {
        match *self {
            NumLiteral::Int(n, _) => Literal::Int(n),
            NumLiteral::U128(n) => Literal::U128(n),
            NumLiteral::Float(n, _) => Literal::Float(n),
            NumLiteral::Decimal(n) => Literal::Decimal(n),
        }
    }
    fn to_pattern(&self) -> Pattern<'static> {
        match *self {
            NumLiteral::Int(n, w) => Pattern::IntLiteral(n, w),
            NumLiteral::U128(n) => Pattern::IntLiteral(n, IntWidth::U128),
            NumLiteral::Float(n, w) => Pattern::FloatLiteral(f64::to_bits(n), w),
            NumLiteral::Decimal(n) => Pattern::DecimalLiteral(n),
        }
    }
}

fn make_num_literal(layout: Layout<'_>, num_str: &str, num_value: IntOrFloatValue) -> NumLiteral {
    match layout {
        Layout::Builtin(Builtin::Int(width)) => match num_value {
            IntOrFloatValue::Int(IntValue::I128(n)) => NumLiteral::Int(n, width),
            IntOrFloatValue::Int(IntValue::U128(n)) => NumLiteral::U128(n),
            IntOrFloatValue::Float(..) => {
                internal_error!("Float value where int was expected, should have been a type error")
            }
        },
        Layout::Builtin(Builtin::Float(width)) => match num_value {
            IntOrFloatValue::Float(n) => NumLiteral::Float(n, width),
            IntOrFloatValue::Int(int_value) => match int_value {
                IntValue::I128(n) => NumLiteral::Float(i128::from_ne_bytes(n) as f64, width),
                IntValue::U128(n) => NumLiteral::Float(u128::from_ne_bytes(n) as f64, width),
            },
        },
        Layout::Builtin(Builtin::Decimal) => {
            let dec = match RocDec::from_str(num_str) {
                Some(d) => d,
                None => internal_error!(
                    "Invalid decimal for float literal = {}. This should be a type error!",
                    num_str
                ),
            };
            NumLiteral::Decimal(dec.to_ne_bytes())
        }
        layout => internal_error!(
            "Found a non-num layout where a number was expected: {:?}",
            layout
        ),
    }
}

fn assign_num_literal_expr<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    variable: Variable,
    num_str: &str,
    num_value: IntOrFloatValue,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let layout = layout_cache
        .from_var(env.arena, variable, env.subs)
        .unwrap();
    let literal = make_num_literal(layout, num_str, num_value).to_expr_literal();

    Stmt::Let(assigned, Expr::Literal(literal), layout, hole)
}

fn make_num_literal_pattern<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    variable: Variable,
    num_str: &str,
    num_value: IntOrFloatValue,
) -> Pattern<'a> {
    let layout = layout_cache
        .from_var(env.arena, variable, env.subs)
        .unwrap();
    let literal = make_num_literal(layout, num_str, num_value);
    literal.to_pattern()
}

type ToLowLevelCallArguments<'a> = (
    LambdaName<'a>,
    Symbol,
    Option<Layout<'a>>,
    CallSpecId,
    UpdateModeId,
);

/// Use the lambda set to figure out how to make a lowlevel call
#[allow(clippy::too_many_arguments)]
fn lowlevel_match_on_lambda_set<'a, ToLowLevelCall>(
    env: &mut Env<'a, '_>,
    layout_cache: &LayoutCache<'a>,
    lambda_set: LambdaSet<'a>,
    op: LowLevel,
    closure_data_symbol: Symbol,
    to_lowlevel_call: ToLowLevelCall,
    return_layout: Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a>
where
    ToLowLevelCall: Fn(ToLowLevelCallArguments<'a>) -> Call<'a> + Copy,
{
    match lambda_set.call_by_name_options(&layout_cache.interner) {
        ClosureCallOptions::Void => empty_lambda_set_error(env),
        ClosureCallOptions::Union(union_layout) => {
            let closure_tag_id_symbol = env.unique_symbol();

            let result = lowlevel_union_lambda_set_to_switch(
                env,
                lambda_set.iter_set(),
                closure_tag_id_symbol,
                union_layout.tag_id_layout(),
                closure_data_symbol,
                lambda_set.is_represented(&layout_cache.interner),
                to_lowlevel_call,
                return_layout,
                assigned,
                hole,
            );

            // extract & assign the closure_tag_id_symbol
            let expr = Expr::GetTagId {
                structure: closure_data_symbol,
                union_layout,
            };

            Stmt::Let(
                closure_tag_id_symbol,
                expr,
                union_layout.tag_id_layout(),
                env.arena.alloc(result),
            )
        }
        ClosureCallOptions::Struct { .. } => match lambda_set.iter_set().next() {
            Some(lambda_name) => {
                let call_spec_id = env.next_call_specialization_id();
                let update_mode = env.next_update_mode_id();
                let call = to_lowlevel_call((
                    lambda_name,
                    closure_data_symbol,
                    lambda_set.is_represented(&layout_cache.interner),
                    call_spec_id,
                    update_mode,
                ));

                build_call(env, call, assigned, return_layout, env.arena.alloc(hole))
            }
            None => {
                eprintln!(
                    "a function passed to `{:?}` LowLevel call has an empty lambda set!
                     The most likely reason is that some symbol you use is not in scope.
                    ",
                    op
                );

                hole.clone()
            }
        },
        ClosureCallOptions::UnwrappedCapture(_) => {
            let lambda_name = lambda_set
                .iter_set()
                .next()
                .expect("no function in lambda set");

            let call_spec_id = env.next_call_specialization_id();
            let update_mode = env.next_update_mode_id();
            let call = to_lowlevel_call((
                lambda_name,
                closure_data_symbol,
                lambda_set.is_represented(&layout_cache.interner),
                call_spec_id,
                update_mode,
            ));

            build_call(env, call, assigned, return_layout, env.arena.alloc(hole))
        }
        ClosureCallOptions::EnumDispatch(repr) => match repr {
            EnumDispatch::Bool => {
                let closure_tag_id_symbol = closure_data_symbol;

                lowlevel_enum_lambda_set_to_switch(
                    env,
                    lambda_set.iter_set(),
                    closure_tag_id_symbol,
                    Layout::Builtin(Builtin::Bool),
                    closure_data_symbol,
                    lambda_set.is_represented(&layout_cache.interner),
                    to_lowlevel_call,
                    return_layout,
                    assigned,
                    hole,
                )
            }
            EnumDispatch::U8 => {
                let closure_tag_id_symbol = closure_data_symbol;

                lowlevel_enum_lambda_set_to_switch(
                    env,
                    lambda_set.iter_set(),
                    closure_tag_id_symbol,
                    Layout::Builtin(Builtin::Int(IntWidth::U8)),
                    closure_data_symbol,
                    lambda_set.is_represented(&layout_cache.interner),
                    to_lowlevel_call,
                    return_layout,
                    assigned,
                    hole,
                )
            }
        },
    }
}

#[allow(clippy::too_many_arguments)]
fn lowlevel_union_lambda_set_to_switch<'a, ToLowLevelCall>(
    env: &mut Env<'a, '_>,
    lambda_set: impl ExactSizeIterator<Item = LambdaName<'a>> + 'a,
    closure_tag_id_symbol: Symbol,
    closure_tag_id_layout: Layout<'a>,
    closure_data_symbol: Symbol,
    closure_env_layout: Option<Layout<'a>>,
    to_lowlevel_call: ToLowLevelCall,
    return_layout: Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a>
where
    ToLowLevelCall: Fn(ToLowLevelCallArguments<'a>) -> Call<'a> + Copy,
{
    debug_assert_ne!(lambda_set.len(), 0);

    let join_point_id = JoinPointId(env.unique_symbol());

    let mut branches = Vec::with_capacity_in(lambda_set.len(), env.arena);

    for (i, lambda_name) in lambda_set.into_iter().enumerate() {
        let assigned = env.unique_symbol();

        let hole = Stmt::Jump(join_point_id, env.arena.alloc([assigned]));

        let call_spec_id = env.next_call_specialization_id();
        let update_mode = env.next_update_mode_id();
        let call = to_lowlevel_call((
            lambda_name,
            closure_data_symbol,
            closure_env_layout,
            call_spec_id,
            update_mode,
        ));
        let stmt = build_call(env, call, assigned, return_layout, env.arena.alloc(hole));

        branches.push((i as u64, BranchInfo::None, stmt));
    }

    let default_branch = {
        let (_, info, stmt) = branches.pop().unwrap();

        (info, &*env.arena.alloc(stmt))
    };

    let switch = Stmt::Switch {
        cond_symbol: closure_tag_id_symbol,
        cond_layout: closure_tag_id_layout,
        branches: branches.into_bump_slice(),
        default_branch,
        ret_layout: return_layout,
    };

    let param = Param {
        symbol: assigned,
        layout: return_layout,
        borrow: false,
    };

    Stmt::Join {
        id: join_point_id,
        parameters: &*env.arena.alloc([param]),
        body: hole,
        remainder: env.arena.alloc(switch),
    }
}

fn empty_lambda_set_error<'a>(env: &mut Env<'a, '_>) -> Stmt<'a> {
    let msg = "a Lambda Set is empty. Most likely there is a type error in your program.";
    runtime_error(env, msg)
}

/// Use the lambda set to figure out how to make a call-by-name
#[allow(clippy::too_many_arguments)]
fn match_on_lambda_set<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &LayoutCache<'a>,
    procs: &mut Procs<'a>,
    lambda_set: LambdaSet<'a>,
    closure_data_symbol: Symbol,
    argument_symbols: &'a [Symbol],
    argument_layouts: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    match lambda_set.call_by_name_options(&layout_cache.interner) {
        ClosureCallOptions::Void => empty_lambda_set_error(env),
        ClosureCallOptions::Union(union_layout) => {
            let closure_tag_id_symbol = env.unique_symbol();

            let result = union_lambda_set_to_switch(
                env,
                layout_cache,
                lambda_set,
                closure_tag_id_symbol,
                union_layout.tag_id_layout(),
                closure_data_symbol,
                argument_symbols,
                argument_layouts,
                return_layout,
                assigned,
                hole,
            );

            // extract & assign the closure_tag_id_symbol
            let expr = Expr::GetTagId {
                structure: closure_data_symbol,
                union_layout,
            };

            Stmt::Let(
                closure_tag_id_symbol,
                expr,
                union_layout.tag_id_layout(),
                env.arena.alloc(result),
            )
        }
        ClosureCallOptions::Struct {
            field_layouts,
            field_order_hash: _,
        } => {
            let function_symbol = match lambda_set.iter_set().next() {
                Some(function_symbol) => function_symbol,
                None => {
                    // Lambda set is empty, so this function is never called; synthesize a function
                    // that always yields a runtime error.
                    let name = env.unique_symbol();
                    let function_layout =
                        RawFunctionLayout::Function(argument_layouts, lambda_set, return_layout);
                    let proc =
                        generate_runtime_error_function(env, layout_cache, name, function_layout);
                    let top_level = ProcLayout::from_raw(
                        env.arena,
                        &layout_cache.interner,
                        function_layout,
                        CapturesNiche::no_niche(),
                    );

                    procs.specialized.insert_specialized(name, top_level, proc);
                    LambdaName::no_niche(name)
                }
            };

            let closure_info = match field_layouts {
                [] => ClosureInfo::DoesNotCapture,
                _ => ClosureInfo::Captures {
                    lambda_set,
                    closure_data_symbol,
                },
            };

            union_lambda_set_branch_help(
                env,
                layout_cache,
                function_symbol,
                closure_info,
                argument_symbols,
                argument_layouts,
                return_layout,
                assigned,
                hole,
            )
        }
        ClosureCallOptions::UnwrappedCapture(_) => {
            let function_symbol = lambda_set
                .iter_set()
                .next()
                .expect("no function in lambda set");

            let closure_info = ClosureInfo::Captures {
                lambda_set,
                closure_data_symbol,
            };

            union_lambda_set_branch_help(
                env,
                layout_cache,
                function_symbol,
                closure_info,
                argument_symbols,
                argument_layouts,
                return_layout,
                assigned,
                hole,
            )
        }
        ClosureCallOptions::EnumDispatch(repr) => match repr {
            EnumDispatch::Bool => {
                let closure_tag_id_symbol = closure_data_symbol;

                enum_lambda_set_to_switch(
                    env,
                    lambda_set.iter_set(),
                    closure_tag_id_symbol,
                    Layout::Builtin(Builtin::Bool),
                    argument_symbols,
                    argument_layouts,
                    return_layout,
                    assigned,
                    hole,
                )
            }
            EnumDispatch::U8 => {
                let closure_tag_id_symbol = closure_data_symbol;

                enum_lambda_set_to_switch(
                    env,
                    lambda_set.iter_set(),
                    closure_tag_id_symbol,
                    Layout::Builtin(Builtin::Int(IntWidth::U8)),
                    argument_symbols,
                    argument_layouts,
                    return_layout,
                    assigned,
                    hole,
                )
            }
        },
    }
}

#[allow(clippy::too_many_arguments)]
fn union_lambda_set_to_switch<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &LayoutCache<'a>,
    lambda_set: LambdaSet<'a>,
    closure_tag_id_symbol: Symbol,
    closure_tag_id_layout: Layout<'a>,
    closure_data_symbol: Symbol,
    argument_symbols: &'a [Symbol],
    argument_layouts: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    if lambda_set.is_empty() {
        // NOTE this can happen if there is a type error somewhere. Since the lambda set is empty,
        // there is really nothing we can do here. We generate a runtime error here which allows
        // code gen to proceed. We then assume that we hit another (more descriptive) error before
        // hitting this one
        return empty_lambda_set_error(env);
    }

    let join_point_id = JoinPointId(env.unique_symbol());

    let mut branches = Vec::with_capacity_in(lambda_set.len(), env.arena);

    for (i, lambda_name) in lambda_set.iter_set().enumerate() {
        let closure_info = if lambda_name.no_captures() {
            ClosureInfo::DoesNotCapture
        } else {
            ClosureInfo::Captures {
                lambda_set,
                closure_data_symbol,
            }
        };

        let stmt = union_lambda_set_branch(
            env,
            layout_cache,
            join_point_id,
            lambda_name,
            closure_info,
            argument_symbols,
            argument_layouts,
            return_layout,
        );
        branches.push((i as u64, BranchInfo::None, stmt));
    }

    let default_branch = {
        let (_, info, stmt) = branches.pop().unwrap();

        (info, &*env.arena.alloc(stmt))
    };

    let switch = Stmt::Switch {
        cond_symbol: closure_tag_id_symbol,
        cond_layout: closure_tag_id_layout,
        branches: branches.into_bump_slice(),
        default_branch,
        ret_layout: *return_layout,
    };

    let param = Param {
        symbol: assigned,
        layout: *return_layout,
        borrow: false,
    };

    Stmt::Join {
        id: join_point_id,
        parameters: &*env.arena.alloc([param]),
        body: hole,
        remainder: env.arena.alloc(switch),
    }
}

#[allow(clippy::too_many_arguments)]
fn union_lambda_set_branch<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &LayoutCache<'a>,
    join_point_id: JoinPointId,
    lambda_name: LambdaName<'a>,
    closure_info: ClosureInfo<'a>,
    argument_symbols_slice: &'a [Symbol],
    argument_layouts_slice: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
) -> Stmt<'a> {
    let result_symbol = env.unique_symbol();

    let hole = Stmt::Jump(join_point_id, env.arena.alloc([result_symbol]));

    union_lambda_set_branch_help(
        env,
        layout_cache,
        lambda_name,
        closure_info,
        argument_symbols_slice,
        argument_layouts_slice,
        return_layout,
        result_symbol,
        env.arena.alloc(hole),
    )
}

#[derive(Clone, Copy)]
enum ClosureInfo<'a> {
    Captures {
        closure_data_symbol: Symbol,
        /// The whole lambda set representation this closure is a variant of
        lambda_set: LambdaSet<'a>,
    },
    DoesNotCapture,
}

#[allow(clippy::too_many_arguments)]
fn union_lambda_set_branch_help<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &LayoutCache<'a>,
    lambda_name: LambdaName<'a>,
    closure_info: ClosureInfo<'a>,
    argument_symbols_slice: &'a [Symbol],
    argument_layouts_slice: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let (argument_layouts, argument_symbols) = match closure_info {
        ClosureInfo::Captures {
            lambda_set,
            closure_data_symbol,
        } => {
            let argument_layouts = lambda_set.extend_argument_list(
                env.arena,
                &layout_cache.interner,
                argument_layouts_slice,
            );
            let argument_symbols = if argument_layouts.len() > argument_layouts_slice.len() {
                // extend symbols with the symbol of the closure environment
                let mut argument_symbols =
                    Vec::with_capacity_in(argument_symbols_slice.len() + 1, env.arena);
                argument_symbols.extend(argument_symbols_slice);
                argument_symbols.push(closure_data_symbol);
                argument_symbols.into_bump_slice()
            } else {
                argument_symbols_slice
            };
            (argument_layouts, argument_symbols)
        }
        ClosureInfo::DoesNotCapture => {
            // sometimes unification causes a function that does not itself capture anything
            // to still get a lambda set that does store information. We must not pass a closure
            // argument in this case

            (argument_layouts_slice, argument_symbols_slice)
        }
    };

    // build the call
    let call = self::Call {
        call_type: CallType::ByName {
            name: lambda_name,
            ret_layout: return_layout,
            arg_layouts: argument_layouts,
            specialization_id: env.next_call_specialization_id(),
        },
        arguments: argument_symbols,
    };

    build_call(env, call, assigned, *return_layout, hole)
}

/// Switches over a enum lambda set, which may dispatch to different functions, none of which
/// capture.
#[allow(clippy::too_many_arguments)]
fn enum_lambda_set_to_switch<'a>(
    env: &mut Env<'a, '_>,
    lambda_set: impl ExactSizeIterator<Item = LambdaName<'a>>,
    closure_tag_id_symbol: Symbol,
    closure_tag_id_layout: Layout<'a>,
    argument_symbols: &'a [Symbol],
    argument_layouts: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    debug_assert_ne!(lambda_set.len(), 0);

    let join_point_id = JoinPointId(env.unique_symbol());

    let mut branches = Vec::with_capacity_in(lambda_set.len(), env.arena);

    for (i, lambda_name) in lambda_set.into_iter().enumerate() {
        let stmt = enum_lambda_set_branch(
            env,
            join_point_id,
            lambda_name,
            argument_symbols,
            argument_layouts,
            return_layout,
        );
        branches.push((i as u64, BranchInfo::None, stmt));
    }

    let default_branch = {
        let (_, info, stmt) = branches.pop().unwrap();

        (info, &*env.arena.alloc(stmt))
    };

    let switch = Stmt::Switch {
        cond_symbol: closure_tag_id_symbol,
        cond_layout: closure_tag_id_layout,
        branches: branches.into_bump_slice(),
        default_branch,
        ret_layout: *return_layout,
    };

    let param = Param {
        symbol: assigned,
        layout: *return_layout,
        borrow: false,
    };

    Stmt::Join {
        id: join_point_id,
        parameters: &*env.arena.alloc([param]),
        body: hole,
        remainder: env.arena.alloc(switch),
    }
}

/// A branch for an enum lambda set branch dispatch, which never capture!
#[allow(clippy::too_many_arguments)]
fn enum_lambda_set_branch<'a>(
    env: &mut Env<'a, '_>,
    join_point_id: JoinPointId,
    lambda_name: LambdaName<'a>,
    argument_symbols: &'a [Symbol],
    argument_layouts: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
) -> Stmt<'a> {
    let result_symbol = env.unique_symbol();

    let hole = Stmt::Jump(join_point_id, env.arena.alloc([result_symbol]));

    let assigned = result_symbol;

    let call = self::Call {
        call_type: CallType::ByName {
            name: lambda_name,
            ret_layout: return_layout,
            arg_layouts: argument_layouts,
            specialization_id: env.next_call_specialization_id(),
        },
        arguments: argument_symbols,
    };
    build_call(env, call, assigned, *return_layout, env.arena.alloc(hole))
}

#[allow(clippy::too_many_arguments)]
fn lowlevel_enum_lambda_set_to_switch<'a, ToLowLevelCall>(
    env: &mut Env<'a, '_>,
    lambda_set: impl ExactSizeIterator<Item = LambdaName<'a>>,
    closure_tag_id_symbol: Symbol,
    closure_tag_id_layout: Layout<'a>,
    closure_data_symbol: Symbol,
    closure_env_layout: Option<Layout<'a>>,
    to_lowlevel_call: ToLowLevelCall,
    return_layout: Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a>
where
    ToLowLevelCall: Fn(ToLowLevelCallArguments<'a>) -> Call<'a> + Copy,
{
    debug_assert_ne!(lambda_set.len(), 0);

    let join_point_id = JoinPointId(env.unique_symbol());

    let mut branches = Vec::with_capacity_in(lambda_set.len(), env.arena);

    for (i, function_symbol) in lambda_set.into_iter().enumerate() {
        let result_symbol = env.unique_symbol();

        let hole = Stmt::Jump(join_point_id, env.arena.alloc([result_symbol]));

        let call_spec_id = env.next_call_specialization_id();
        let update_mode = env.next_update_mode_id();
        let call = to_lowlevel_call((
            function_symbol,
            closure_data_symbol,
            closure_env_layout,
            call_spec_id,
            update_mode,
        ));
        let stmt = build_call(
            env,
            call,
            result_symbol,
            return_layout,
            env.arena.alloc(hole),
        );

        branches.push((i as u64, BranchInfo::None, stmt));
    }

    let default_branch = {
        let (_, info, stmt) = branches.pop().unwrap();

        (info, &*env.arena.alloc(stmt))
    };

    let switch = Stmt::Switch {
        cond_symbol: closure_tag_id_symbol,
        cond_layout: closure_tag_id_layout,
        branches: branches.into_bump_slice(),
        default_branch,
        ret_layout: return_layout,
    };

    let param = Param {
        symbol: assigned,
        layout: return_layout,
        borrow: false,
    };

    Stmt::Join {
        id: join_point_id,
        parameters: &*env.arena.alloc([param]),
        body: hole,
        remainder: env.arena.alloc(switch),
    }
}
