#![allow(clippy::manual_map)]

use crate::ir::erased::{build_erased_function, ResolvedErasedLambda};
use crate::ir::literal::{make_num_literal, IntOrFloatValue};
use crate::layout::{
    self, Builtin, ClosureCallOptions, ClosureDataKind, ClosureRepresentation, EnumDispatch,
    InLayout, LambdaName, LambdaSet, Layout, LayoutCache, LayoutInterner, LayoutProblem,
    LayoutRepr, Niche, RawFunctionLayout, TLLayoutInterner, TagIdIntType, UnionLayout,
    WrappedVariant,
};
use bumpalo::collections::{CollectIn, Vec};
use bumpalo::Bump;
use roc_can::abilities::SpecializationId;
use roc_can::expr::{AnnotatedMark, ClosureData, ExpectLookup, WhenBranch, WhenBranchPattern};
use roc_can::module::ExposedByModule;
use roc_collections::all::{default_hasher, BumpMap, BumpMapDefault, MutMap};
use roc_collections::VecMap;
use roc_debug_flags::dbg_do;
#[cfg(debug_assertions)]
use roc_debug_flags::{
    ROC_PRINT_IR_AFTER_DROP_SPECIALIZATION, ROC_PRINT_IR_AFTER_REFCOUNT,
    ROC_PRINT_IR_AFTER_RESET_REUSE, ROC_PRINT_IR_AFTER_SPECIALIZATION, ROC_PRINT_RUNTIME_ERROR_GEN,
};
use roc_derive::SharedDerivedModule;
use roc_error_macros::{internal_error, todo_abilities, todo_lambda_erasure};
use roc_late_solve::storage::{ExternalModuleStorage, ExternalModuleStorageSnapshot};
use roc_late_solve::{resolve_ability_specialization, AbilitiesView, Resolved, UnificationFailed};
use roc_module::ident::{ForeignSymbol, Lowercase, TagName};
use roc_module::low_level::{LowLevel, LowLevelWrapperType};
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_problem::can::{RuntimeError, ShadowKind};
use roc_region::all::{Loc, Region};
use roc_std::RocDec;
use roc_target::Target;
use roc_types::subs::{
    instantiate_rigids, storage_copy_var_to, Content, ExhaustiveMark, FlatType, RedundantMark,
    StorageSubs, Subs, Variable, VariableSubsSlice,
};
use std::collections::HashMap;
use ven_pretty::{text, BoxAllocator, DocAllocator, DocBuilder};

use pattern::{from_can_pattern, store_pattern, Pattern};

pub use literal::{ListLiteralElement, Literal};

mod boxed;
mod decision_tree;
mod erased;
mod literal;
mod pattern;

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
    dbg_do!(ROC_PRINT_IR_AFTER_DROP_SPECIALIZATION, {
        return true;
    });
    false
}

// if your changes cause this number to go down, great!
// please change it to the lower number.
// if it went up, maybe check that the change is really required
roc_error_macros::assert_sizeof_wasm!(Literal, 24);
roc_error_macros::assert_sizeof_wasm!(Expr, 48);
roc_error_macros::assert_sizeof_wasm!(Stmt, 64);
roc_error_macros::assert_sizeof_wasm!(ProcLayout, 20);
roc_error_macros::assert_sizeof_wasm!(Call, 44);
roc_error_macros::assert_sizeof_wasm!(CallType, 36);

roc_error_macros::assert_sizeof_non_wasm!(Literal, 3 * 8);
roc_error_macros::assert_sizeof_non_wasm!(Expr, 9 * 8);
roc_error_macros::assert_sizeof_non_wasm!(Stmt, 12 * 8);
roc_error_macros::assert_sizeof_non_wasm!(ProcLayout, 5 * 8);
roc_error_macros::assert_sizeof_non_wasm!(Call, 9 * 8);
roc_error_macros::assert_sizeof_non_wasm!(CallType, 7 * 8);

fn runtime_error<'a>(env: &mut Env<'a, '_>, msg: &'a str) -> Stmt<'a> {
    let sym = env.unique_symbol();
    Stmt::Let(
        sym,
        Expr::Literal(Literal::Str(msg)),
        Layout::STR,
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
pub struct SingleEntryPoint<'a> {
    pub name: &'a str,
    pub symbol: Symbol,
    pub layout: ProcLayout<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum EntryPoint<'a> {
    Program(&'a [SingleEntryPoint<'a>]),
    Expects { symbols: &'a [Symbol] },
}

#[derive(Clone, Copy, Debug)]
pub struct PartialProcId(usize);

#[derive(Clone, Debug)]
pub struct PartialProcs<'a> {
    /// maps a function name (symbol) to an index
    symbols: Vec<'a, Symbol>,

    partial_procs: Vec<'a, PartialProc<'a>>,
}

impl<'a> PartialProcs<'a> {
    fn new_in(arena: &'a Bump) -> Self {
        Self {
            symbols: Vec::new_in(arena),
            partial_procs: Vec::new_in(arena),
        }
    }
    fn contains_key(&self, symbol: Symbol) -> bool {
        self.symbol_to_id(symbol).is_some()
    }

    fn symbol_to_id(&self, symbol: Symbol) -> Option<PartialProcId> {
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
            "The {symbol:?} is inserted as a partial proc twice: that's a bug!",
        );

        let id = PartialProcId(self.symbols.len());

        self.symbols.push(symbol);
        self.partial_procs.push(partial_proc);

        id
    }

    pub fn drain(self) -> impl Iterator<Item = (Symbol, PartialProc<'a>)> {
        debug_assert_eq!(self.symbols.len(), self.partial_procs.len());

        self.symbols.into_iter().zip(self.partial_procs)
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum CapturedSymbols<'a> {
    #[default]
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

#[derive(Clone, Debug, PartialEq)]
pub struct Proc<'a> {
    pub name: LambdaName<'a>,
    pub args: &'a [(InLayout<'a>, Symbol)],
    pub body: Stmt<'a>,
    pub closure_data_layout: Option<InLayout<'a>>,
    pub ret_layout: InLayout<'a>,
    pub is_self_recursive: SelfRecursive,
    pub is_erased: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct HostExposedLambdaSet<'a> {
    pub id: LambdaSetId,
    /// Symbol of the exposed function
    pub symbol: Symbol,
    pub proc_layout: ProcLayout<'a>,
    pub raw_function_layout: RawFunctionLayout<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
        pretty: bool,
        _parens: Parens,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: LayoutInterner<'a>,
    {
        let args_doc = self.args.iter().map(|(layout, symbol)| {
            let arg_doc = symbol_to_doc(alloc, *symbol, pretty);
            if pretty_print_ir_symbols() {
                arg_doc
                    .append(alloc.reflow(": "))
                    .append(interner.to_doc_top(*layout, alloc))
            } else {
                arg_doc
            }
        });

        if pretty_print_ir_symbols() {
            alloc
                .text("procedure : ")
                .append(symbol_to_doc(alloc, self.name.name(), pretty))
                .append(" ")
                .append(interner.to_doc_top(self.ret_layout, alloc))
                .append(alloc.hardline())
                .append(alloc.text("procedure = "))
                .append(symbol_to_doc(alloc, self.name.name(), pretty))
                .append(" (")
                .append(alloc.intersperse(args_doc, ", "))
                .append("):")
                .append(alloc.hardline())
                .append(self.body.to_doc(alloc, interner, pretty).indent(4))
        } else {
            alloc
                .text("procedure ")
                .append(symbol_to_doc(alloc, self.name.name(), pretty))
                .append(" (")
                .append(alloc.intersperse(args_doc, ", "))
                .append("):")
                .append(alloc.hardline())
                .append(self.body.to_doc(alloc, interner, pretty).indent(4))
        }
    }

    pub fn to_pretty<I>(&self, interner: &I, width: usize, pretty: bool) -> String
    where
        I: LayoutInterner<'a>,
    {
        let allocator = BoxAllocator;
        let mut w = std::vec::Vec::new();
        self.to_doc::<_, (), _>(&allocator, interner, pretty, Parens::NotNeeded)
            .1
            .render(width, &mut w)
            .unwrap();
        w.push(b'\n');
        String::from_utf8(w).unwrap()
    }

    pub fn proc_layout(&self, arena: &'a Bump) -> ProcLayout<'a> {
        let args = Vec::from_iter_in(self.args.iter().map(|(a, _)| *a), arena);

        ProcLayout {
            arguments: args.into_bump_slice(),
            result: self.ret_layout,
            niche: Niche::NONE,
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
    /// For each symbol, a variable that stores the unsolved (!) annotation
    annotations: std::vec::Vec<Option<Variable>>,
    /// For each symbol, what types to specialize it for, points into the storage_subs
    types_to_specialize: std::vec::Vec<Variable>,
    storage_subs: StorageSubs,
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
            annotations: std::vec::Vec::new(),
            storage_subs: StorageSubs::new(Subs::default()),
            types_to_specialize: std::vec::Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.symbol_or_lambdas.is_empty()
    }

    pub fn insert_host_exposed(
        &mut self,
        env_subs: &mut Subs,
        symbol_or_lambda: LambdaName<'a>,
        annotation: Option<Variable>,
        variable: Variable,
    ) {
        let variable = self.storage_subs.extend_with_variable(env_subs, variable);

        match self
            .symbol_or_lambdas
            .iter()
            .position(|s| *s == symbol_or_lambda)
        {
            None => {
                self.symbol_or_lambdas.push(symbol_or_lambda);
                self.types_to_specialize.push(variable);
                self.annotations.push(annotation);
            }
            Some(_) => {
                // we assume that only one specialization of a function is directly exposed to the
                // host. Other host-exposed symbols may (transitively) specialize this symbol,
                // but then the existing specialization mechanism will find those specializations
                panic!("A host-exposed symbol can only be exposed once");
            }
        }
    }

    fn decompose(
        self,
    ) -> (
        StorageSubs,
        impl Iterator<Item = (LambdaName<'a>, Variable, Option<Variable>)>,
    ) {
        let it1 = self.symbol_or_lambdas.into_iter();

        let it2 = self.types_to_specialize.into_iter();
        let it3 = self.annotations.into_iter();

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
                .zip(self.types_to_specialize),
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
            .zip(self.proc_layouts)
            .zip(self.procedures)
            .filter_map(|((s, l), in_progress)| {
                if let Symbol::REMOVED_SPECIALIZATION = s {
                    None
                } else {
                    match in_progress {
                        InProgressProc::InProgress => {
                            panic!("Function {s:?} ({l:?}) is not done specializing")
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

    fn insert_specialized(
        &mut self,
        symbol: Symbol,
        layout: ProcLayout<'a>,
        proc: Proc<'a>,
    ) -> SpecializedIndex {
        for (i, s) in self.symbols.iter().enumerate() {
            if *s == symbol && self.proc_layouts[i] == layout {
                match &self.procedures[i] {
                    InProgressProc::InProgress => {
                        self.procedures[i] = InProgressProc::Done(proc);
                        return SpecializedIndex(i);
                    }
                    InProgressProc::Done(_) => {
                        // overwrite existing! this is important in practice
                        // TODO investigate why we generate the wrong proc in some cases and then
                        // correct later
                        self.procedures[i] = InProgressProc::Done(proc);
                        return SpecializedIndex(i);
                    }
                }
            }
        }

        // the key/layout combo was not found; insert it
        let i = self.symbols.len();

        self.symbols.push(symbol);
        self.proc_layouts.push(layout);
        self.procedures.push(InProgressProc::Done(proc));

        SpecializedIndex(i)
    }
}

struct SpecializedIndex(usize);

/// Uniquely determines the specialization of a polymorphic (non-proc) value symbol.
/// Two specializations are equivalent if their [`SpecializationMark`]s are equal.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
struct SpecializationMark<'a> {
    /// The layout of the symbol itself.
    layout: InLayout<'a>,

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

type NumberSpecializations<'a> = VecMap<InLayout<'a>, (Symbol, UseDepth)>;

/// When walking a function body, we may encounter specialized usages of polymorphic number symbols.
/// For example
///
///  n = 1
///  use1 : U8
///  use1 = 1
///  use2 : Dec
///  use2 = 2
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
    VecMap<Symbol, NumberSpecializations<'a>>,
);

impl<'a> SymbolSpecializations<'a> {
    /// Mark a let-generalized symbol eligible for specialization.
    /// Only those bound to number literals can be compiled polymorphically.
    fn mark_eligible(&mut self, symbol: Symbol) {
        let _old = self.0.insert(symbol, VecMap::with_capacity(1));
        debug_assert!(_old.is_none(), "overwriting specializations for {symbol:?}");
    }

    /// Removes all specializations for a symbol, returning the type and symbol of each specialization.
    fn remove(&mut self, symbol: Symbol) -> Option<NumberSpecializations<'a>> {
        self.0
            .remove(&symbol)
            .map(|(_, specializations)| specializations)
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn maybe_get_specialized(&self, symbol: Symbol, layout: InLayout) -> Symbol {
        self.0
            .get(&symbol)
            .and_then(|m| m.get(&layout))
            .map(|x| x.0)
            .unwrap_or(symbol)
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

impl<'a> ProcsBase<'a> {
    pub fn get_host_exposed_symbols(&self) -> impl Iterator<Item = Symbol> + '_ {
        self.host_specializations
            .symbol_or_lambdas
            .iter()
            .copied()
            .map(|n| n.name())
    }
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

pub type HostExposedLambdaSets<'a> =
    std::vec::Vec<(LambdaName<'a>, Symbol, HostExposedLambdaSet<'a>)>;

#[derive(Clone, Debug)]
pub struct Procs<'a> {
    pub partial_procs: PartialProcs<'a>,
    ability_member_aliases: AbilityAliases,
    pending_specializations: PendingSpecializations<'a>,
    specialized: Specialized<'a>,
    host_exposed_lambda_sets: HostExposedLambdaSets<'a>,
    pub runtime_errors: BumpMap<Symbol, &'a str>,
    pub externals_we_need: BumpMap<ModuleId, ExternalSpecializations<'a>>,
    symbol_specializations: SymbolSpecializations<'a>,
    specialization_stack: SpecializationStack<'a>,

    pub imported_module_thunks: &'a [Symbol],
    pub module_thunks: &'a [Symbol],
    pub host_exposed_symbols: &'a [Symbol],
}

impl<'a> Procs<'a> {
    pub fn new_in(arena: &'a Bump) -> Self {
        Self {
            partial_procs: PartialProcs::new_in(arena),
            ability_member_aliases: AbilityAliases::new_in(arena),
            pending_specializations: PendingSpecializations::Finding(Suspended::new_in(arena)),
            specialized: Specialized::default(),
            runtime_errors: BumpMap::new_in(arena),
            externals_we_need: BumpMap::new_in(arena),
            host_exposed_lambda_sets: std::vec::Vec::new(),
            symbol_specializations: Default::default(),
            specialization_stack: SpecializationStack(Vec::with_capacity_in(16, arena)),

            imported_module_thunks: &[],
            module_thunks: &[],
            host_exposed_symbols: &[],
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
            "incorrect popped specialization: passed {specialization:?}, but was {popped:?}"
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
    ) -> (
        MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
        HostExposedLambdaSets<'a>,
        ProcsBase<'a>,
    ) {
        let mut specialized_procs =
            MutMap::with_capacity_and_hasher(self.specialized.len(), default_hasher());

        for (symbol, layout, proc) in self.specialized.into_iter_assert_done() {
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

        (
            specialized_procs,
            self.host_exposed_lambda_sets,
            restored_procs_base,
        )
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
        let raw_layout = layout_cache.raw_from_var(env.arena, annotation, env.subs)?;

        let top_level = ProcLayout::from_raw_named(env.arena, name, raw_layout);

        // anonymous functions cannot reference themselves, therefore cannot be tail-recursive
        // EXCEPT when the closure conversion makes it tail-recursive.
        let is_self_recursive = match top_level
            .arguments
            .last()
            .map(|l| layout_cache.get_repr(*l))
        {
            Some(LayoutRepr::LambdaSet(lambda_set)) => lambda_set.contains(name.name()),
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
                        debug_assert!(layout.arguments.is_empty(), "{name:?}");
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
                                // NOTE we can't skip extra work! We still need to make the specialization for this
                                // invocation.
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
                                partial_proc_id,
                            ) {
                                Ok((proc, layout)) => {
                                    let proc_name = proc.name;
                                    let function_layout =
                                        ProcLayout::from_raw_named(env.arena, proc_name, layout);
                                    self.specialized.insert_specialized(
                                        proc_name.name(),
                                        function_layout,
                                        proc,
                                    );
                                }
                                Err(error) => {
                                    panic!("TODO generate a RuntimeError message for {error:?}");
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
                let proc_name = name;

                let partial_proc_id = match self.partial_procs.symbol_to_id(proc_name.name()) {
                    Some(p) => p,
                    None => panic!(
                        "no partial_proc for {:?} in module {:?}",
                        proc_name, env.home
                    ),
                };

                // Mark this proc as in-progress, so if we're dealing with
                // mutually recursive functions, we don't loop forever.
                // (We had a bug around this before this system existed!)
                self.specialized.mark_in_progress(proc_name.name(), layout);

                // See https://github.com/roc-lang/roc/issues/1600
                //
                // The annotation variable is the generic/lifted/top-level annotation.
                // It is connected to the variables of the function's body
                //
                // fn_var is the variable representing the type that we actually need for the
                // function right here.
                match specialize_variable(
                    env,
                    self,
                    proc_name,
                    layout_cache,
                    fn_var,
                    partial_proc_id,
                ) {
                    Ok((proc, raw_layout)) => {
                        let proc_layout =
                            ProcLayout::from_raw_named(env.arena, proc_name, raw_layout);

                        self.specialized
                            .insert_specialized(proc_name.name(), proc_layout, proc);
                    }
                    Err(error) => {
                        panic!("TODO generate a RuntimeError message for {error:?}");
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
        let symbol_specializations = match self.symbol_specializations.0.get_mut(&symbol) {
            Some(m) => m,
            None => {
                // Not eligible for multiple specializations
                return symbol;
            }
        };

        let arena = env.arena;
        let subs: &Subs = env.subs;

        let layout = match layout_cache.from_var(arena, specialization_var, subs) {
            Ok(layout) => layout,
            // This can happen when the def symbol has a type error. In such cases just use the
            // def symbol, which is erroring.
            Err(_) => return symbol,
        };

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
        let (specialized_symbol, deepest_use) = symbol_specializations
            .get_or_insert(layout, || (make_specialized_symbol(), current_use));

        if deepest_use.is_nested_use_in(&current_use) {
            *deepest_use = current_use;
        }

        *specialized_symbol
    }

    /// Get the symbol specializations used in the active specialization's body.
    pub fn get_symbol_specializations_used_in_body(
        &self,
        symbol: Symbol,
    ) -> Option<impl Iterator<Item = Symbol> + '_> {
        let this_use = self.specialization_stack.current_use_depth();
        self.symbol_specializations.0.get(&symbol).map(move |l| {
            l.iter().filter_map(move |(_, (sym, deepest_use))| {
                if deepest_use.is_nested_use_in(&this_use) {
                    Some(*sym)
                } else {
                    None
                }
            })
        })
    }
}

#[derive(Default)]
pub struct Specializations<'a> {
    by_symbol: MutMap<Symbol, MutMap<InLayout<'a>, Proc<'a>>>,
}

impl<'a> Specializations<'a> {
    pub fn insert(&mut self, symbol: Symbol, layout: InLayout<'a>, proc: Proc<'a>) {
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
    /// [Subs] to write specialized variables of lookups in expects.
    /// [None] if this module doesn't produce any expects.
    pub expectation_subs: Option<&'i mut Subs>,
    pub home: ModuleId,
    pub ident_ids: &'i mut IdentIds,
    pub target: Target,
    pub update_mode_ids: &'i mut UpdateModeIds,
    pub call_specialization_counter: u32,
    // TODO: WorldAbilities and exposed_by_module share things, think about how to combine them
    pub abilities: AbilitiesView<'i>,
    pub exposed_by_module: &'i ExposedByModule,
    pub derived_module: &'i SharedDerivedModule,
    pub struct_indexing: UsageTrackingMap<(Symbol, u64), Symbol>,
}

impl<'a, 'i> Env<'a, 'i> {
    pub fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }

    pub fn named_unique_symbol(&mut self, name: &str) -> Symbol {
        let ident_id = self.ident_ids.add_str(name);
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

        layout_cache.invalidate(self.subs, changed_variables.iter().copied());
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
    pub layout: InLayout<'a>,
}

impl<'a> Param<'a> {
    pub const EMPTY: Self = Param {
        symbol: Symbol::EMPTY_PARAM,
        layout: Layout::UNIT,
    };
}

pub fn cond<'a>(
    env: &mut Env<'a, '_>,
    cond_symbol: Symbol,
    cond_layout: InLayout<'a>,
    pass: Stmt<'a>,
    fail: Stmt<'a>,
    ret_layout: InLayout<'a>,
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

/// The specialized type of a lookup. Represented as a type-variable.
pub type LookupType = Variable;

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<'a> {
    Let(Symbol, Expr<'a>, InLayout<'a>, &'a Stmt<'a>),
    Switch {
        /// This *must* stand for an integer, because Switch potentially compiles to a jump table.
        cond_symbol: Symbol,
        cond_layout: InLayout<'a>,
        /// The u64 in the tuple will be compared directly to the condition Expr.
        /// If they are equal, this branch will be taken.
        branches: &'a [(u64, BranchInfo<'a>, Stmt<'a>)],
        /// If no other branches pass, this default branch will be taken.
        default_branch: (BranchInfo<'a>, &'a Stmt<'a>),
        /// Each branch must return a value of this type.
        ret_layout: InLayout<'a>,
    },
    Ret(Symbol),
    Refcounting(ModifyRc, &'a Stmt<'a>),
    Expect {
        condition: Symbol,
        region: Region,
        lookups: &'a [Symbol],
        variables: &'a [LookupType],
        /// what happens after the expect
        remainder: &'a Stmt<'a>,
    },
    Dbg {
        /// The location this dbg is in source as a printable string.
        source_location: &'a str,
        /// The source code of the expression being debugged.
        source: &'a str,
        /// The expression we're displaying
        symbol: Symbol,
        /// The specialized variable of the expression
        variable: Variable,
        /// What happens after the dbg
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
        layout: InLayout<'a>,
        tag_id: TagIdIntType,
    },
    List {
        scrutinee: Symbol,
        len: u64,
    },
    Unique {
        scrutinee: Symbol,
        unique: bool,
    },
}

impl<'a> BranchInfo<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D, _pretty: bool) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        alloc.text("")
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
    /// Unconditionally deallocate the memory. For tag union that do pointer tagging (store the tag
    /// id in the pointer) the backend has to clear the tag id!
    Free(Symbol),
}

impl ModifyRc {
    pub fn to_doc<'a, D, A>(self, alloc: &'a D, pretty: bool) -> DocBuilder<'a, D, A>
    where
        D: DocAllocator<'a, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use ModifyRc::*;

        match self {
            Inc(symbol, 1) => alloc
                .text("inc ")
                .append(symbol_to_doc(alloc, symbol, pretty))
                .append(";"),
            Inc(symbol, n) => alloc
                .text("inc ")
                .append(text!(alloc, "{} ", n))
                .append(symbol_to_doc(alloc, symbol, pretty))
                .append(";"),
            Dec(symbol) => alloc
                .text("dec ")
                .append(symbol_to_doc(alloc, symbol, pretty))
                .append(";"),
            DecRef(symbol) => alloc
                .text("decref ")
                .append(symbol_to_doc(alloc, symbol, pretty))
                .append(";"),
            Free(symbol) => alloc
                .text("free ")
                .append(symbol_to_doc(alloc, symbol, pretty))
                .append(";"),
        }
    }

    pub fn get_symbol(&self) -> Symbol {
        use ModifyRc::*;

        match self {
            Inc(symbol, _) => *symbol,
            Dec(symbol) => *symbol,
            DecRef(symbol) => *symbol,
            Free(symbol) => *symbol,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Call<'a> {
    pub call_type: CallType<'a>,
    pub arguments: &'a [Symbol],
}

impl<'a> Call<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D, pretty: bool) -> DocBuilder<'b, D, A>
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
                    .map(|s| symbol_to_doc(alloc, s, pretty));

                alloc.text("CallByName ").append(alloc.intersperse(it, " "))
            }
            CallType::ByPointer { pointer, .. } => {
                let it = std::iter::once(pointer)
                    .chain(arguments.iter().copied())
                    .map(|s| symbol_to_doc(alloc, s, pretty));

                alloc.text("CallByPtr ").append(alloc.intersperse(it, " "))
            }
            LowLevel { op: lowlevel, .. } => {
                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s, pretty));

                text!(alloc, "lowlevel {:?} ", lowlevel).append(alloc.intersperse(it, " "))
            }
            HigherOrder(higher_order) => {
                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s, pretty));

                text!(alloc, "lowlevel {:?} ", higher_order.op).append(alloc.intersperse(it, " "))
            }
            Foreign {
                ref foreign_symbol, ..
            } => {
                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s, pretty));

                text!(alloc, "foreign {:?} ", foreign_symbol.as_str())
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
        ret_layout: InLayout<'a>,
        arg_layouts: &'a [InLayout<'a>],
        specialization_id: CallSpecId,
    },
    ByPointer {
        pointer: Symbol,
        ret_layout: InLayout<'a>,
        arg_layouts: &'a [InLayout<'a>],
    },
    Foreign {
        foreign_symbol: ForeignSymbol,
        ret_layout: InLayout<'a>,
    },
    LowLevel {
        op: LowLevel,
        update_mode: UpdateModeId,
    },
    HigherOrder(&'a HigherOrderLowLevel<'a>),
}

impl<'a> CallType<'a> {
    /**
    Replace calls to wrappers of lowlevel functions with the lowlevel function itself
    */
    pub fn replace_lowlevel_wrapper(self) -> Self {
        match self {
            CallType::ByName { name, .. } => match LowLevelWrapperType::from_symbol(name.name()) {
                LowLevelWrapperType::CanBeReplacedBy(lowlevel) => CallType::LowLevel {
                    op: lowlevel,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                LowLevelWrapperType::NotALowLevelWrapper => self,
            },
            _ => self,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct PassedFunction<'a> {
    /// name of the top-level function that is passed as an argument
    /// e.g. in `List.map xs Num.abs` this would be `Num.abs`
    pub name: LambdaName<'a>,

    pub argument_layouts: &'a [InLayout<'a>],
    pub return_layout: InLayout<'a>,

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
    pub closure_env_layout: Option<InLayout<'a>>,

    /// update mode of the higher order lowlevel itself
    pub update_mode: UpdateModeId,

    pub passed_function: PassedFunction<'a>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReuseToken {
    pub symbol: Symbol,
    pub update_tag_id: bool,
    pub update_mode: UpdateModeId,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ErasedField {
    /// Load a dereferenceable pointer to the value.
    Value,
    /// Load a non-dereferenceable pointer to the value.
    ValuePtr,
    Callee,
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
        reuse: Option<ReuseToken>,
    },
    Struct(&'a [Symbol]),
    NullPointer,

    StructAtIndex {
        index: u64,
        field_layouts: &'a [InLayout<'a>],
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
    GetElementPointer {
        structure: Symbol,
        union_layout: UnionLayout<'a>,
        indices: &'a [u64],
    },

    Array {
        elem_layout: InLayout<'a>,
        elems: &'a [ListLiteralElement<'a>],
    },
    EmptyArray,

    /// Creates a type-erased value.
    ErasedMake {
        /// The erased value. If this is an erased function, the value are the function captures,
        /// or `None` if the function is not a closure.
        value: Option<Symbol>,
        /// The function pointer of the erased value, if it's an erased function.
        callee: Symbol,
    },

    /// Loads a field from a type-erased value.
    ErasedLoad {
        /// The erased symbol.
        symbol: Symbol,
        /// The field to load.
        field: ErasedField,
    },

    /// Returns a pointer to the given function.
    FunctionPointer {
        lambda_name: LambdaName<'a>,
    },

    Alloca {
        element_layout: InLayout<'a>,
        initializer: Option<Symbol>,
    },

    Reset {
        symbol: Symbol,
        update_mode: UpdateModeId,
    },

    // Just like Reset, but does not recursively decrement the children.
    // Used in reuse analysis to replace a decref with a resetRef to avoid decrementing when the dec ref didn't.
    ResetRef {
        symbol: Symbol,
        update_mode: UpdateModeId,
    },
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
            Int(bytes) => text!(alloc, "{}i64", i128::from_ne_bytes(*bytes)),
            U128(bytes) => text!(alloc, "{}u128", u128::from_ne_bytes(*bytes)),
            Float(lit) => text!(alloc, "{}f64", lit),
            Decimal(bytes) => text!(alloc, "{}dec", RocDec::from_ne_bytes(*bytes)),
            Bool(lit) => text!(alloc, "{}", lit),
            Byte(lit) => text!(alloc, "{}u8", lit),
            Str(lit) => text!(alloc, "{:?}", lit),
        }
    }
}

pub(crate) fn symbol_to_doc_string(symbol: Symbol, force_pretty: bool) -> String {
    use roc_module::ident::ModuleName;

    if pretty_print_ir_symbols() || force_pretty {
        format!("{symbol:?}")
    } else {
        let text = format!("{symbol}");

        if text.starts_with(ModuleName::APP) {
            let name: String = text.trim_start_matches(ModuleName::APP).into();
            format!("Test{name}")
        } else {
            text
        }
    }
}

fn symbol_to_doc<'b, D, A>(alloc: &'b D, symbol: Symbol, force_pretty: bool) -> DocBuilder<'b, D, A>
where
    D: DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    alloc.text(symbol_to_doc_string(symbol, force_pretty))
}

fn join_point_to_doc<'b, D, A>(
    alloc: &'b D,
    symbol: JoinPointId,
    pretty: bool,
) -> DocBuilder<'b, D, A>
where
    D: DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    symbol_to_doc(alloc, symbol.0, pretty)
}

impl<'a> Expr<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D, pretty: bool) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use Expr::*;

        match self {
            Literal(lit) => lit.to_doc(alloc),

            Call(call) => call.to_doc(alloc, pretty),

            Tag {
                tag_id,
                arguments,
                reuse: None,
                ..
            } => {
                let doc_tag = alloc
                    .text("TagId(")
                    .append(alloc.text(tag_id.to_string()))
                    .append(")");

                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s, pretty));

                doc_tag
                    .append(alloc.space())
                    .append(alloc.intersperse(it, " "))
            }

            Tag {
                tag_id,
                arguments,
                reuse: Some(reuse_token),
                ..
            } => {
                let doc_tag = alloc
                    .text("TagId(")
                    .append(alloc.text(tag_id.to_string()))
                    .append(")");

                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s, pretty));

                alloc
                    .text("Reuse ")
                    .append(symbol_to_doc(alloc, reuse_token.symbol, pretty))
                    .append(alloc.space())
                    .append(format!("{:?}", reuse_token.update_mode))
                    .append(alloc.space())
                    .append(doc_tag)
                    .append(alloc.space())
                    .append(alloc.intersperse(it, " "))
            }
            NullPointer => alloc.text("NullPointer"),
            Reset {
                symbol,
                update_mode,
            } => alloc
                .text("Reset { symbol: ")
                .append(symbol_to_doc(alloc, *symbol, pretty))
                .append(", id: ")
                .append(format!("{update_mode:?}"))
                .append(" }"),
            ResetRef {
                symbol,
                update_mode,
            } => alloc
                .text("ResetRef { symbol: ")
                .append(symbol_to_doc(alloc, *symbol, pretty))
                .append(", id: ")
                .append(format!("{update_mode:?}"))
                .append(" }"),
            Struct(args) => {
                let it = args.iter().map(|s| symbol_to_doc(alloc, *s, pretty));

                alloc
                    .text("Struct {")
                    .append(alloc.intersperse(it, ", "))
                    .append(alloc.text("}"))
            }
            Array { elems, .. } => {
                let it = elems.iter().map(|e| match e {
                    ListLiteralElement::Literal(l) => l.to_doc(alloc),
                    ListLiteralElement::Symbol(s) => symbol_to_doc(alloc, *s, pretty),
                });

                alloc
                    .text("Array [")
                    .append(alloc.intersperse(it, ", "))
                    .append(alloc.text("]"))
            }
            EmptyArray => alloc.text("Array []"),

            StructAtIndex {
                index, structure, ..
            } => text!(alloc, "StructAtIndex {} ", index)
                .append(symbol_to_doc(alloc, *structure, pretty)),

            GetTagId { structure, .. } => alloc
                .text("GetTagId ")
                .append(symbol_to_doc(alloc, *structure, pretty)),

            ErasedMake { value, callee } => {
                let value = match value {
                    Some(v) => symbol_to_doc(alloc, *v, pretty),
                    None => alloc.text("<null>"),
                };
                let callee = symbol_to_doc(alloc, *callee, pretty);
                alloc
                    .text("ErasedMake { value: ")
                    .append(value)
                    .append(", callee: ")
                    .append(callee)
                    .append(" }")
            }

            ErasedLoad { symbol, field } => {
                let field = match field {
                    ErasedField::Value => ".Value",
                    ErasedField::ValuePtr => ".ValuePtr",
                    ErasedField::Callee => ".Callee",
                };

                alloc
                    .text("ErasedLoad ")
                    .append(symbol_to_doc(alloc, *symbol, pretty))
                    .append(alloc.text(" "))
                    .append(field)
            }

            FunctionPointer { lambda_name } => alloc
                .text("FunctionPointer ")
                .append(symbol_to_doc(alloc, lambda_name.name(), pretty)),

            UnionAtIndex {
                tag_id,
                structure,
                index,
                ..
            } => text!(alloc, "UnionAtIndex (Id {tag_id}) (Index {index}) ")
                .append(symbol_to_doc(alloc, *structure, pretty)),

            GetElementPointer {
                structure, indices, ..
            } => {
                let it = indices.iter().map(|num| alloc.as_string(num));
                let it = alloc.intersperse(it, ", ");
                text!(alloc, "GetElementPointer (Indices [",)
                    .append(it)
                    .append(alloc.text("]) "))
                    .append(symbol_to_doc(alloc, *structure, pretty))
            }
            // .append(alloc.intersperse(index.iter(), ", "))},
            Alloca { initializer, .. } => match initializer {
                Some(initializer) => {
                    text!(alloc, "Alloca ").append(symbol_to_doc(alloc, *initializer, pretty))
                }
                None => text!(alloc, "Alloca <uninitialized>"),
            },
        }
    }

    pub fn to_pretty(&self, width: usize, pretty: bool) -> String {
        let allocator = BoxAllocator;
        let mut w = std::vec::Vec::new();
        self.to_doc::<_, ()>(&allocator, pretty)
            .1
            .render(width, &mut w)
            .unwrap();
        w.push(b'\n');
        String::from_utf8(w).unwrap()
    }

    pub(crate) fn ptr_load(symbol: &'a Symbol) -> Expr<'a> {
        Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::PtrLoad,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: std::slice::from_ref(symbol),
        })
    }

    pub(crate) fn ptr_store(arguments: &'a [Symbol]) -> Expr<'a> {
        Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::PtrStore,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments,
        })
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

    pub fn to_doc<'b, D, A, I>(
        &'b self,
        alloc: &'b D,
        interner: &I,
        pretty: bool,
    ) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
        I: LayoutInterner<'a>,
    {
        use Stmt::*;

        match self {
            Let(symbol, expr, layout, cont) => alloc
                .text("let ")
                .append(symbol_to_doc(alloc, *symbol, pretty))
                .append(" : ")
                .append(interner.to_doc_top(*layout, alloc))
                .append(" = ")
                .append(expr.to_doc(alloc, pretty))
                .append(";")
                .append(alloc.hardline())
                .append(cont.to_doc(alloc, interner, pretty)),

            Refcounting(modify, cont) => modify
                .to_doc(alloc, pretty)
                .append(alloc.hardline())
                .append(cont.to_doc(alloc, interner, pretty)),

            Dbg {
                symbol, remainder, ..
            } => alloc
                .text("dbg ")
                .append(symbol_to_doc(alloc, *symbol, pretty))
                .append(";")
                .append(alloc.hardline())
                .append(remainder.to_doc(alloc, interner, pretty)),

            Expect {
                condition,
                remainder,
                ..
            } => alloc
                .text("expect ")
                .append(symbol_to_doc(alloc, *condition, pretty))
                .append(";")
                .append(alloc.hardline())
                .append(remainder.to_doc(alloc, interner, pretty)),

            Ret(symbol) => alloc
                .text("ret ")
                .append(symbol_to_doc(alloc, *symbol, pretty))
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
                            .append(symbol_to_doc(alloc, *cond_symbol, pretty))
                            .append(" then")
                            .append(info.to_doc(alloc, pretty))
                            .append(alloc.hardline())
                            .append(pass.to_doc(alloc, interner, pretty).indent(4))
                            .append(alloc.hardline())
                            .append(alloc.text("else"))
                            .append(default_branch.0.to_doc(alloc, pretty))
                            .append(alloc.hardline())
                            .append(fail.to_doc(alloc, interner, pretty).indent(4))
                    }

                    _ => {
                        let default_doc = alloc
                            .text("default:")
                            .append(alloc.hardline())
                            .append(default_branch.1.to_doc(alloc, interner, pretty).indent(4))
                            .indent(4);

                        let branches_docs = branches
                            .iter()
                            .map(|(tag, _info, expr)| {
                                text!(alloc, "case {}:", tag)
                                    .append(alloc.hardline())
                                    .append(expr.to_doc(alloc, interner, pretty).indent(4))
                                    .indent(4)
                            })
                            .chain(std::iter::once(default_doc));
                        //
                        alloc
                            .text("switch ")
                            .append(symbol_to_doc(alloc, *cond_symbol, pretty))
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

            Crash(s, _src) => alloc
                .text("Crash ")
                .append(symbol_to_doc(alloc, *s, pretty)),

            Join {
                id,
                parameters,
                body: continuation,
                remainder,
            } => {
                let it = parameters
                    .iter()
                    .map(|p| symbol_to_doc(alloc, p.symbol, pretty));

                alloc.intersperse(
                    vec![
                        alloc
                            .text("joinpoint ")
                            .append(join_point_to_doc(alloc, *id, pretty))
                            .append(" ".repeat(parameters.len().min(1)))
                            .append(alloc.intersperse(it, alloc.space()))
                            .append(":"),
                        continuation.to_doc(alloc, interner, pretty).indent(4),
                        alloc.text("in"),
                        remainder.to_doc(alloc, interner, pretty),
                    ],
                    alloc.hardline(),
                )
            }
            Jump(id, arguments) => {
                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s, pretty));

                alloc
                    .text("jump ")
                    .append(join_point_to_doc(alloc, *id, pretty))
                    .append(" ".repeat(arguments.len().min(1)))
                    .append(alloc.intersperse(it, alloc.space()))
                    .append(";")
            }
        }
    }

    pub fn to_pretty<I>(&self, interner: &I, width: usize, pretty: bool) -> String
    where
        I: LayoutInterner<'a>,
    {
        let allocator = BoxAllocator;
        let mut w = std::vec::Vec::new();
        self.to_doc::<_, (), _>(&allocator, interner, pretty)
            .1
            .render(width, &mut w)
            .unwrap();
        w.push(b'\n');
        String::from_utf8(w).unwrap()
    }

    pub fn if_then_else(
        arena: &'a Bump,
        condition_symbol: Symbol,
        return_layout: InLayout<'a>,
        then_branch_stmt: Stmt<'a>,
        else_branch_stmt: &'a Stmt<'a>,
    ) -> Self {
        let then_branch_info = BranchInfo::Constructor {
            scrutinee: condition_symbol,
            layout: Layout::BOOL,
            tag_id: 1,
        };
        let then_branch = (1u64, then_branch_info, then_branch_stmt);

        let else_branch_info = BranchInfo::Constructor {
            scrutinee: condition_symbol,
            layout: Layout::BOOL,
            tag_id: 0,
        };
        let else_branch = (else_branch_info, else_branch_stmt);

        Stmt::Switch {
            cond_symbol: condition_symbol,
            cond_layout: Layout::BOOL,
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
            RecordAccessor(accessor_data) => {
                let fresh_record_symbol = env.unique_symbol();
                let closure_data = accessor_data.to_closure_data(fresh_record_symbol);
                debug_assert_eq!(*symbol, closure_data.name);
                register_noncapturing_closure(env, procs, *symbol, closure_data);

                lower_rest!(variable, cont.value)
            }
            ImportParams(_, _, None) => {
                lower_rest!(variable, cont.value)
            }
            Var(original, _) | AbilityMember(original, _, _)
                if procs.get_partial_proc(original).is_none() =>
            {
                // a variable is aliased, e.g.
                //
                //  foo = bar
                //
                // We need to generate an IR that is free of local lvalue aliasing, as this aids in
                // refcounting. As such, variable aliasing usually involves renaming the LHS in the
                // rest of the program with the RHS (i.e. [foo->bar]); see `handle_variable_aliasing`
                // below for the exact algorithm.
                //
                // However, do not attempt to eliminate aliasing to procedures
                // (either a function pointer or a thunk) here. Doing so is not necessary - if we
                // have `var = f` where `f` is either a proc or thunk, in either case, `f` will be
                // resolved to an rvalue, not an lvalue:
                //
                // - If `f` is a proc, we assign to `var` its closure data (even if the lambda set
                //   of `f` is unary with no captures, we leave behind the empty closure data)
                //
                // - If `f` is a thunk, we force the thunk and assign to `var` its value.
                //
                // With this in mind, when `f` is a thunk or proper function, we are free to follow
                // the usual (non-lvalue-aliasing) branch of assignment, and end up with correct
                // code.
                //
                // ===
                //
                // Recording that an lvalue references a procedure or a thunk may open up
                // opportunities for optimization - and indeed, recording this information may
                // sometimes eliminate such unused lvalues, or inline closure data. However, in
                // general, making sure this kind of aliasing works correctly is very difficult. As
                // illustration, consider
                //
                //     getNum1 = \{} -> 1u64
                //     getNum2 = \{} -> 2u64
                //
                //     dispatch = \fun -> fun {}
                //
                //     main =
                //         myFun1 = getNum1
                //         myFun2 = getNum2
                //         dispatch (if Bool.true then myFun1 else myFun2)
                //
                // Suppose we leave nothing behind for the assignments `myFun* = getNum*`, and
                // instead simply associate that they reference procs. In the if-then-else
                // expression, we then need to construct the closure data for both getNum1 and
                // getNum2 - but we do not know what lambdas they represent, as we only have access
                // to the symbols `myFun1` and `myFun2`.
                //
                // While associations of `myFun1 -> getNum1` could be propogated, the story gets
                // more complicated when the referenced proc itself resolves a lambda set with
                // indirection; for example consider the amendment
                //
                //     getNum1 = @Dispatcher \{} -> 1u64
                //     getNum2 = @Dispatcher \{} -> 2u64
                //
                // Now, even the association of `myFun1 -> getNum1` is not enough, as the lambda
                // set of (if Bool.true then myFun1 else myFun2) would not be { getNum1, getNum2 }
                // - it would be the binary lambda set of the anonymous closures created under the
                // `@Dispatcher` wrappers.
                //
                // Trying to keep all this information in line has been error prone, and is not
                // attempted.

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

                use roc_can::{def::Def, expr::Expr, pattern::Pattern};

                let new_outer = match &nested_cont.value {
                    &Expr::Closure(ClosureData {
                        name: anon_name, ..
                    }) => {
                        // A wrinkle:
                        //
                        //   let f =
                        //      let n = 1 in
                        //      \{} -[#lam]-> n
                        //
                        // must become
                        //
                        //   let n = 1 in
                        //   let #lam = \{} -[#lam]-> n in
                        //   let f = #lam

                        debug_assert_ne!(*symbol, anon_name);

                        // #lam = \...
                        let def_anon_closure = Box::new(Def {
                            loc_pattern: Loc::at_zero(Pattern::Identifier(anon_name)),
                            loc_expr: *nested_cont,
                            expr_var: def.expr_var,
                            pattern_vars: std::iter::once((anon_name, def.expr_var)).collect(),
                            annotation: None,
                            kind: def.kind,
                        });

                        // f = #lam
                        let new_def = Box::new(Def {
                            loc_pattern: def.loc_pattern,
                            loc_expr: Loc::at_zero(Expr::Var(anon_name, def.expr_var)),
                            expr_var: def.expr_var,
                            pattern_vars: def.pattern_vars,
                            annotation: def.annotation,
                            kind: def.kind,
                        });

                        let new_inner = LetNonRec(new_def, cont);

                        LetNonRec(
                            nested_def,
                            Box::new(Loc::at_zero(LetNonRec(
                                def_anon_closure,
                                Box::new(Loc::at_zero(new_inner)),
                            ))),
                        )
                    }
                    _ => {
                        let new_def = Def {
                            loc_pattern: def.loc_pattern,
                            loc_expr: *nested_cont,
                            pattern_vars: def.pattern_vars,
                            annotation: def.annotation,
                            expr_var: def.expr_var,
                            kind: def.kind,
                        };

                        let new_inner = LetNonRec(Box::new(new_def), cont);

                        LetNonRec(nested_def, Box::new(Loc::at_zero(new_inner)))
                    }
                };

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
                    kind: def.kind,
                };

                let new_inner = LetNonRec(Box::new(new_def), cont);

                let new_outer = LetRec(nested_defs, Box::new(Loc::at_zero(new_inner)), cycle_mark);

                lower_rest!(variable, new_outer)
            }
            e @ (Int(..) | Float(..) | Num(..)) => {
                let (str, val): (Box<str>, IntOrFloatValue) = match e {
                    Int(_, _, str, val, _) => (str, IntOrFloatValue::Int(val)),
                    Float(_, _, str, val, _) => (str, IntOrFloatValue::Float(val)),
                    Num(_, str, val, _) => (str, IntOrFloatValue::Int(val)),
                    _ => unreachable!(),
                };
                procs.symbol_specializations.mark_eligible(*symbol);

                let mut stmt = lower_rest!(variable, cont.value);

                let needed_specializations = procs.symbol_specializations.remove(*symbol).unwrap();
                let zero_specialization = if needed_specializations.is_empty() {
                    let layout = layout_cache
                        .from_var(env.arena, def.expr_var, env.subs)
                        .unwrap();
                    Some((layout, *symbol))
                } else {
                    None
                };

                // Layer on the specialized numbers
                for (layout, sym) in needed_specializations
                    .into_iter()
                    .map(|(lay, (sym, _))| (lay, sym))
                    .chain(zero_specialization)
                {
                    let literal = make_num_literal(&layout_cache.interner, layout, &str, val);
                    stmt = Stmt::Let(
                        sym,
                        Expr::Literal(literal.to_expr_literal()),
                        layout,
                        env.arena.alloc(stmt),
                    );
                }

                stmt
            }
            _ => {
                let rest = lower_rest!(variable, cont.value);

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
        };
    }

    // this may be a destructure pattern
    let (mono_pattern, assignments) =
        match from_can_pattern(env, procs, layout_cache, &def.loc_pattern.value) {
            Ok(v) => v,
            Err(_) => {
                eprintln!(indoc::indoc! {"
                    Error:
                        This can happen if you redefine a variable in the repl, for example:

                            x = 1
                            x = 2

                        Roc does not allow this yet.
                "});
                std::process::exit(1);
            }
        };

    // convert the continuation
    let mut stmt = lower_rest!(variable, cont.value);

    // layer on any default record fields
    for (symbol, variable, expr) in assignments {
        let hole = env.arena.alloc(stmt);
        stmt = with_hole(env, expr, variable, procs, layout_cache, symbol, hole);
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
fn pattern_to_when(
    env: &mut Env<'_, '_>,
    pattern_var: Variable,
    pattern: Loc<roc_can::pattern::Pattern>,
    body_var: Variable,
    body: Loc<roc_can::expr::Expr>,
) -> (Symbol, Loc<roc_can::expr::Expr>) {
    use roc_can::expr::Expr::*;
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

        As(pattern, symbol) => pattern_to_when_help(pattern_var, pattern, body_var, body, *symbol),

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

        AppliedTag { .. }
        | RecordDestructure { .. }
        | TupleDestructure { .. }
        | UnwrappedOpaque { .. } => {
            let symbol = env.unique_symbol();
            pattern_to_when_help(pattern_var, &pattern, body_var, body, symbol)
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

fn pattern_to_when_help(
    pattern_var: Variable,
    pattern: &Loc<roc_can::pattern::Pattern>,
    body_var: Variable,
    body: Loc<roc_can::expr::Expr>,
    symbol: Symbol,
) -> (Symbol, Loc<roc_can::expr::Expr>) {
    use roc_can::expr::Expr;

    let wrapped_body = Expr::When {
        cond_var: pattern_var,
        expr_var: body_var,
        region: Region::zero(),
        loc_cond: Box::new(Loc::at_zero(Expr::Var(symbol, pattern_var))),
        branches: vec![WhenBranch {
            patterns: vec![WhenBranchPattern {
                pattern: pattern.to_owned(),
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

        match specialize_variable(env, procs, name, layout_cache, var, partial_proc) {
            Ok((proc, raw_layout)) => {
                let proc_layout = ProcLayout::from_raw_named(env.arena, name, raw_layout);
                procs
                    .specialized
                    .insert_specialized(name.name(), proc_layout, proc);
            }
            Err(SpecializeFailure {
                attempted_layout, ..
            }) => {
                let proc = generate_runtime_error_function(env, name, attempted_layout);

                let top_level = ProcLayout::from_raw_named(env.arena, name, attempted_layout);

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

    for (lambda_name, from_app, opt_from_platform) in it {
        let from_app = offset_variable(from_app);
        let index = specialize_external_help(env, procs, layout_cache, lambda_name, from_app);

        let Some(from_platform) = opt_from_platform else {
            continue;
        };

        // now run the lambda set numbering scheme
        let hels = find_lambda_sets(env.arena, env.subs, from_platform);

        // now unify
        let mut unify_env = roc_unify::Env::new(
            env.subs,
            #[cfg(debug_assertions)]
            None,
        );

        let unified = roc_unify::unify::unify(
            &mut unify_env,
            from_platform,
            from_app,
            roc_solve_schema::UnificationMode::EQ,
            roc_types::types::Polarity::Pos,
        );

        {
            use roc_unify::unify::Unified::*;

            match unified {
                Success { .. } => { /* great */ }
                Failure(..) => internal_error!("unification here should never fail"),
            }
        }

        for (var, id) in hels {
            let symbol = env.unique_symbol();
            let lambda_name = LambdaName::no_niche(symbol);

            let mut layout_env = layout::Env::from_components(layout_cache, env.subs, env.arena);
            let lambda_set = env.subs.get_lambda_set(var);
            let raw_function_layout =
                RawFunctionLayout::from_var(&mut layout_env, lambda_set.ambient_function)
                    .value()
                    .unwrap();

            let (key, (top_level, proc)) = generate_host_exposed_function(
                env,
                procs,
                layout_cache,
                lambda_name,
                raw_function_layout,
            );

            procs
                .specialized
                .insert_specialized(symbol, top_level, proc);

            let hels = HostExposedLambdaSet {
                id,
                symbol,
                proc_layout: top_level,
                raw_function_layout,
            };

            let in_progress = &mut procs.specialized.procedures[index.0];
            let InProgressProc::Done(proc) = in_progress else {
                unreachable!()
            };

            procs.host_exposed_lambda_sets.push((proc.name, key, hels));
        }
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

            specialize_external_help(env, procs, layout_cache, symbol, imported_variable);
        }
    }
}

fn specialize_external_help<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    name: LambdaName<'a>,
    variable: Variable,
) -> SpecializedIndex {
    let partial_proc_id = match procs.partial_procs.symbol_to_id(name.name()) {
        Some(v) => v,
        None => {
            panic!("Cannot find a partial proc for {name:?}");
        }
    };

    let specialization_result =
        specialize_variable(env, procs, name, layout_cache, variable, partial_proc_id);

    match specialization_result {
        Ok((proc, layout)) => {
            let top_level = ProcLayout::from_raw_named(env.arena, name, layout);

            if procs.is_module_thunk(name.name()) {
                debug_assert!(top_level.arguments.is_empty());
            }

            if procs.host_exposed_symbols.contains(&proc.name.name()) {
                // layouts that are (transitively) used in the type of `main_for_host`.
                let mut host_exposed_layouts: Vec<_> = top_level
                    .arguments
                    .iter()
                    .copied()
                    .chain([top_level.result])
                    .collect_in(env.arena);

                // it is very likely we see the same types across functions, or in multiple arguments
                host_exposed_layouts.sort();
                host_exposed_layouts.dedup();

                // Computer the getter procs for every host-exposed layout.
                for in_layout in host_exposed_layouts {
                    let layout = layout_cache.interner.get(in_layout);

                    let all_glue_procs = generate_glue_procs(
                        env.home,
                        env.ident_ids,
                        env.arena,
                        &mut layout_cache.interner,
                        env.arena.alloc(layout),
                    );

                    let GlueProcs {
                        getters,
                        legacy_layout_based_extern_names: _,
                    } = all_glue_procs;

                    for (_layout, glue_procs) in getters {
                        for glue_proc in glue_procs {
                            procs.specialized.insert_specialized(
                                glue_proc.proc.name.name(),
                                glue_proc.proc_layout,
                                glue_proc.proc,
                            );
                        }
                    }
                }
            }

            procs
                .specialized
                .insert_specialized(name.name(), top_level, proc)
        }
        Err(SpecializeFailure { attempted_layout }) => {
            let proc = generate_runtime_error_function(env, name, attempted_layout);

            let top_level = ProcLayout::from_raw_named(env.arena, name, attempted_layout);

            procs
                .specialized
                .insert_specialized(name.name(), top_level, proc)
        }
    }
}

fn generate_runtime_error_function<'a>(
    env: &mut Env<'a, '_>,
    lambda_name: LambdaName<'a>,
    layout: RawFunctionLayout<'a>,
) -> Proc<'a> {
    let mut msg = bumpalo::collections::string::String::with_capacity_in(80, env.arena);
    use std::fmt::Write;
    write!(
        &mut msg,
        "The {:?} function could not be generated, likely due to a type error.",
        lambda_name.name(),
    )
    .unwrap();

    dbg_do!(ROC_PRINT_RUNTIME_ERROR_GEN, {
        eprintln!(
            "emitted runtime error function {:?} for layout {:?}",
            &msg, layout
        );
    });

    let runtime_error = runtime_error(env, msg.into_bump_str());

    let is_erased = layout.is_erased_function();
    let (args, ret_layout) = match layout {
        RawFunctionLayout::Function(arg_layouts, lambda_set, ret_layout) => {
            let real_arg_layouts =
                lambda_set.extend_argument_list_for_named(env.arena, lambda_name, arg_layouts);
            let mut args = Vec::with_capacity_in(real_arg_layouts.len(), env.arena);

            for arg in arg_layouts {
                args.push((*arg, env.unique_symbol()));
            }
            if real_arg_layouts.len() != arg_layouts.len() {
                let lambda_set_layout = lambda_set.full_layout;
                args.push((lambda_set_layout, Symbol::ARG_CLOSURE));
            }

            (args.into_bump_slice(), ret_layout)
        }
        RawFunctionLayout::ErasedFunction(..) => {
            todo_lambda_erasure!()
        }
        RawFunctionLayout::ZeroArgumentThunk(ret_layout) => (&[] as &[_], ret_layout),
    };

    Proc {
        name: lambda_name,
        args,
        body: runtime_error,
        closure_data_layout: None,
        ret_layout,
        is_self_recursive: SelfRecursive::NotSelfRecursive,
        is_erased,
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

fn generate_host_exposed_function<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    lambda_name: LambdaName<'a>,
    layout: RawFunctionLayout<'a>,
) -> (Symbol, (ProcLayout<'a>, Proc<'a>)) {
    let function_name = lambda_name.name();

    match layout {
        RawFunctionLayout::Function(_, lambda_set, _) => {
            let (proc, top_level) = generate_host_exposed_lambda_set(
                env,
                procs,
                layout_cache,
                function_name,
                lambda_set,
            );

            (function_name, (top_level, proc))
        }
        RawFunctionLayout::ErasedFunction(..) => {
            todo_lambda_erasure!()
        }
        RawFunctionLayout::ZeroArgumentThunk(result) => {
            let assigned = env.unique_symbol();
            let hole = env.arena.alloc(Stmt::Ret(assigned));
            let forced = force_thunk(env, function_name, result, assigned, hole);

            let lambda_name = LambdaName::no_niche(function_name);
            let proc = Proc {
                name: lambda_name,
                args: &[],
                body: forced,
                closure_data_layout: None,
                ret_layout: result,
                is_self_recursive: SelfRecursive::NotSelfRecursive,
                is_erased: false,
            };

            let top_level = ProcLayout::from_raw_named(env.arena, lambda_name, layout);

            (function_name, (top_level, proc))
        }
    }
}

fn generate_host_exposed_lambda_set<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    name: Symbol,
    lambda_set: LambdaSet<'a>,
) -> (Proc<'a>, ProcLayout<'a>) {
    let assigned = env.unique_symbol();

    let argument_layouts = *lambda_set.args;
    let return_layout = lambda_set.ret;

    let mut argument_symbols = Vec::with_capacity_in(argument_layouts.len(), env.arena);
    let mut proc_arguments = Vec::with_capacity_in(argument_layouts.len() + 1, env.arena);
    let mut top_level_arguments = Vec::with_capacity_in(argument_layouts.len() + 1, env.arena);

    for layout in *lambda_set.args {
        let symbol = env.unique_symbol();

        proc_arguments.push((*layout, symbol));

        argument_symbols.push(symbol);
        top_level_arguments.push(*layout);
    }

    // the proc needs to take an extra closure argument
    let lambda_set_layout = lambda_set.full_layout;
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
        ret_layout: return_layout,
        is_self_recursive: SelfRecursive::NotSelfRecursive,
        is_erased: false,
    };

    let top_level = ProcLayout::new(
        env.arena,
        top_level_arguments.into_bump_slice(),
        Niche::NONE,
        return_layout,
    );

    (proc, top_level)
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

    let mut specialized_body = from_can(env, body_var, body, procs, layout_cache);

    //dbg!(&specialized_body);

    let specialized_proc = match specialized {
        SpecializedLayout::FunctionPointerBody {
            ret_layout,
            closure: opt_closure_layout,
            is_erased,
        } => {
            // this is a function body like
            //
            //      foo = Num.add
            //
            // we need to expand this to
            //
            //      foo = \x,y -> Num.add x y

            let closure_data_layout = match opt_closure_layout {
                Some(lambda_set) => lambda_set.full_layout,
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
                is_erased,
            }
        }
        SpecializedLayout::FunctionBody {
            arguments: proc_args,
            closure: opt_closure_layout,
            ret_layout,
            is_erased,
        } => {
            let mut proc_args = Vec::from_iter_in(proc_args.iter().copied(), env.arena);

            // unpack the closure symbols, if any
            match (opt_closure_layout, captured_symbols) {
                (
                    Some(ClosureDataKind::LambdaSet(closure_layout)),
                    CapturedSymbols::Captured(captured),
                ) => {
                    // debug_assert!(!captured.is_empty());

                    // An argument from the closure list may have taken on a specialized symbol
                    // name during the evaluation of the def body. If this is the case, load the
                    // specialized name rather than the original captured name!
                    let get_specialized_name = |symbol| {
                        let specs_used_in_body =
                            procs.get_symbol_specializations_used_in_body(symbol);

                        match specs_used_in_body {
                            Some(mut specs) => {
                                let spec_symbol = specs.next().unwrap_or(symbol);
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
                                    | UnionLayout::NullableWrapped { .. }
                            ));
                            debug_assert_eq!(field_layouts.len(), captured.len());

                            // captured variables are in symbol-alphabetic order, but now we want
                            // them ordered by their alignment requirements
                            let mut combined = Vec::from_iter_in(
                                captured.iter().map(|(x, _)| x).zip(field_layouts.iter()),
                                env.arena,
                            );

                            combined.sort_by(|(_, layout1), (_, layout2)| {
                                let size1 = layout_cache
                                    .get_repr(**layout1)
                                    .alignment_bytes(&layout_cache.interner);
                                let size2 = layout_cache
                                    .get_repr(**layout2)
                                    .alignment_bytes(&layout_cache.interner);

                                size2.cmp(&size1)
                            });

                            for (index, (symbol, _)) in combined.iter().enumerate() {
                                let layout = union_layout.layout_at(
                                    &mut layout_cache.interner,
                                    tag_id,
                                    index,
                                );

                                let expr = Expr::UnionAtIndex {
                                    tag_id,
                                    structure: Symbol::ARG_CLOSURE,
                                    index: index as u64,
                                    union_layout,
                                };

                                let symbol = get_specialized_name(**symbol);

                                let fresh_symbol =
                                    env.named_unique_symbol(&format!("{:?}_closure", symbol));

                                specialized_body = Stmt::Let(
                                    fresh_symbol,
                                    expr,
                                    layout,
                                    env.arena.alloc(specialized_body),
                                );

                                // the same symbol may be used where
                                // - the closure is created
                                // - the closure is consumed
                                substitute_in_exprs(
                                    env.arena,
                                    &mut specialized_body,
                                    symbol,
                                    fresh_symbol,
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

                            combined.sort_by(|(_, layout1), (_, layout2)| {
                                let size1 = layout_cache
                                    .get_repr(**layout1)
                                    .alignment_bytes(&layout_cache.interner);
                                let size2 = layout_cache
                                    .get_repr(**layout2)
                                    .alignment_bytes(&layout_cache.interner);

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
                (Some(ClosureDataKind::Erased), CapturedSymbols::Captured(captured)) => {
                    specialized_body = erased::unpack_closure_data(
                        env,
                        layout_cache,
                        Symbol::ARG_CLOSURE,
                        captured,
                        specialized_body,
                    );
                }
                (None, CapturedSymbols::None) | (None, CapturedSymbols::Captured([])) => {}
                _ => unreachable!("to closure or not to closure?"),
            }

            proc_args.iter_mut().for_each(|(layout, symbol)| {
                // Grab the specialization symbol, if it exists.
                *symbol = procs
                    .symbol_specializations
                    .maybe_get_specialized(*symbol, *layout)
            });

            let closure_data_layout = opt_closure_layout.map(|clos| clos.data_layout());

            Proc {
                name: lambda_name,
                args: proc_args.into_bump_slice(),
                body: specialized_body,
                closure_data_layout,
                ret_layout,
                is_self_recursive: recursivity,
                is_erased,
            }
        }
    };

    Ok(specialized_proc)
}

#[derive(Debug)]
enum SpecializedLayout<'a> {
    /// A body like `foo = \a,b,c -> ...`
    FunctionBody {
        arguments: &'a [(InLayout<'a>, Symbol)],
        closure: Option<ClosureDataKind<'a>>,
        ret_layout: InLayout<'a>,
        is_erased: bool,
    },
    /// A body like `foo = Num.add`
    FunctionPointerBody {
        closure: Option<LambdaSet<'a>>,
        ret_layout: InLayout<'a>,
        is_erased: bool,
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
                Some(ClosureDataKind::LambdaSet(closure_layout)),
                ret_layout,
            )
        }
        RawFunctionLayout::ErasedFunction(pattern_layouts, ret_layout) => {
            let mut pattern_layouts_vec = Vec::with_capacity_in(pattern_layouts.len(), env.arena);
            pattern_layouts_vec.extend_from_slice(pattern_layouts);

            build_specialized_proc(
                env.arena,
                lambda_name,
                pattern_symbols,
                pattern_layouts_vec,
                Some(ClosureDataKind::Erased),
                ret_layout,
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
    pattern_layouts: Vec<'a, InLayout<'a>>,
    closure_data: Option<ClosureDataKind<'a>>,
    ret_layout: InLayout<'a>,
) -> Result<SpecializedLayout<'a>, LayoutProblem> {
    use SpecializedLayout::*;

    let mut proc_args = Vec::with_capacity_in(pattern_layouts.len(), arena);

    let pattern_layouts_len = pattern_layouts.len();

    for (arg_layout, arg_name) in pattern_layouts.into_iter().zip(pattern_symbols.iter()) {
        proc_args.push((arg_layout, *arg_name));
    }

    let is_erased = matches!(closure_data, Some(ClosureDataKind::Erased));

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
    match closure_data {
        Some(closure_data) if pattern_symbols.last() == Some(&Symbol::ARG_CLOSURE) => {
            // here we define the lifted (now top-level) f function. Its final argument is `Symbol::ARG_CLOSURE`,
            // it stores the closure structure (just an integer in this case)
            let closure_data_layout = closure_data.data_layout();
            proc_args.push((closure_data_layout, Symbol::ARG_CLOSURE));

            debug_assert_eq!(
                pattern_layouts_len + 1,
                pattern_symbols.len(),
                "Tried to zip two vecs with different lengths in {proc_name:?}!",
            );

            let proc_args = proc_args.into_bump_slice();

            Ok(FunctionBody {
                arguments: proc_args,
                closure: Some(closure_data),
                ret_layout,
                is_erased,
            })
        }
        Some(closure_data) => {
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
                        is_erased,
                    })
                }
                Ordering::Greater => {
                    if pattern_symbols.is_empty() {
                        let ret_layout = closure_data.data_layout();
                        Ok(FunctionPointerBody {
                            closure: None,
                            ret_layout,
                            is_erased,
                        })
                    } else {
                        // so far, the problem when hitting this branch was always somewhere else
                        // I think this branch should not be reachable in a bugfree compiler
                        panic!(
                            "more arguments (according to the layout) than argument symbols for {proc_name:?}"
                        )
                    }
                }
                Ordering::Less => panic!(
                    "more argument symbols than arguments (according to the layout) for {proc_name:?}"
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
                        is_erased,
                    })
                }
                Ordering::Greater => {
                    if pattern_symbols.is_empty() {
                        Ok(FunctionPointerBody {
                            closure: None,
                            ret_layout,
                            is_erased,
                        })
                    } else {
                        // so far, the problem when hitting this branch was always somewhere else
                        // I think this branch should not be reachable in a bugfree compiler
                        panic!(
                            "more arguments (according to the layout) than argument symbols for {proc_name:?}"
                        )
                    }
                }
                Ordering::Less => panic!(
                    "more argument symbols than arguments (according to the layout) for {proc_name:?}. Pattern symbols: {:?}\n\nPattern layouts: {:?}", pattern_symbols, pattern_layouts_len,
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
    partial_proc_id: PartialProcId,
) -> Result<SpecializeSuccess<'a>, SpecializeFailure<'a>> {
    let snapshot = snapshot_typestate(env.subs, procs, layout_cache);

    // for debugging only
    // TODO: can we get rid of raw entirely?
    let raw = layout_cache
        .raw_from_var(env.arena, fn_var, env.subs)
        .unwrap_or_else(|err| panic!("TODO handle invalid function {err:?}"));

    let raw = if procs.is_module_thunk(proc_name.name()) {
        match raw {
            RawFunctionLayout::Function(_, lambda_set, _) => {
                let lambda_set_layout = lambda_set.full_layout;
                RawFunctionLayout::ZeroArgumentThunk(lambda_set_layout)
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

    let specialized =
        specialize_proc_help(env, procs, proc_name, layout_cache, fn_var, partial_proc_id);

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
    pub arguments: &'a [InLayout<'a>],
    pub result: InLayout<'a>,
    pub niche: Niche<'a>,
}

impl<'a> ProcLayout<'a> {
    pub(crate) fn new(
        arena: &'a Bump,
        old_arguments: &'a [InLayout<'a>],
        old_niche: Niche<'a>,
        result: InLayout<'a>,
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
            niche: old_niche,
            result: new_result,
        }
    }

    fn from_raw_named(
        arena: &'a Bump,
        lambda_name: LambdaName<'a>,
        raw: RawFunctionLayout<'a>,
    ) -> Self {
        match raw {
            RawFunctionLayout::Function(arguments, lambda_set, result) => {
                let arguments =
                    lambda_set.extend_argument_list_for_named(arena, lambda_name, arguments);
                ProcLayout::new(arena, arguments, lambda_name.niche(), result)
            }
            RawFunctionLayout::ErasedFunction(arguments, result) => {
                let arguments = if lambda_name.no_captures() {
                    arguments
                } else {
                    let mut extended_args = Vec::with_capacity_in(arguments.len(), arena);
                    extended_args.extend(arguments.iter().chain(&[Layout::ERASED]).copied());
                    extended_args.into_bump_slice()
                };

                ProcLayout::new(arena, arguments, lambda_name.niche(), result)
            }
            RawFunctionLayout::ZeroArgumentThunk(result) => {
                ProcLayout::new(arena, &[], Niche::NONE, result)
            }
        }
    }

    pub fn dbg_deep<'r, I: LayoutInterner<'a>>(&self, interner: &'r I) -> DbgProcLayout<'a, 'r, I> {
        DbgProcLayout {
            layout: *self,
            interner,
        }
    }
}

pub struct DbgProcLayout<'a, 'r, I: LayoutInterner<'a>> {
    layout: ProcLayout<'a>,
    interner: &'r I,
}

impl<'a, 'r, I: LayoutInterner<'a>> std::fmt::Debug for DbgProcLayout<'a, 'r, I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ProcLayout {
            arguments,
            result,
            niche,
        } = self.layout;
        f.debug_struct("ProcLayout")
            .field("arguments", &self.interner.dbg_deep_iter(arguments))
            .field("result", &self.interner.dbg_deep(result))
            .field("niche", &niche.dbg_deep(self.interner))
            .finish()
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
            Err(e) => panic!("invalid layout {e:?}"),
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
                    hole,
                );

                return result;
            }
        }
    }

    // if the symbol is a function symbol, ensure it is properly specialized!
    let original = symbol;

    let opt_fn_var = Some(variable);

    // if this is a function symbol, ensure that it's properly specialized!
    specialize_symbol(
        env,
        procs,
        layout_cache,
        opt_fn_var,
        assigned,
        hole,
        original,
    )
}

fn try_make_literal<'a>(
    interner: &TLLayoutInterner<'a>,
    can_expr: &roc_can::expr::Expr,
    layout: InLayout<'a>,
) -> Option<Literal<'a>> {
    use roc_can::expr::Expr::*;

    match can_expr {
        Int(_, _, int_str, int, _bound) => Some(
            make_num_literal(interner, layout, int_str, IntOrFloatValue::Int(*int))
                .to_expr_literal(),
        ),

        Float(_, _, float_str, float, _bound) => Some(
            make_num_literal(interner, layout, float_str, IntOrFloatValue::Float(*float))
                .to_expr_literal(),
        ),

        // TODO investigate lifetime trouble
        // Str(string) => Some(Literal::Str(env.arena.alloc(string))),
        Num(_, num_str, num, _bound) => Some(
            make_num_literal(interner, layout, num_str, IntOrFloatValue::Int(*num))
                .to_expr_literal(),
        ),
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
        Int(_, _, int_str, int, _bound) => {
            match assign_num_literal_expr(
                env,
                layout_cache,
                assigned,
                variable,
                &int_str,
                IntOrFloatValue::Int(int),
                hole,
            ) {
                Ok(stmt) => stmt,
                Err(_) => hole.clone(),
            }
        }

        Float(_, _, float_str, float, _bound) => {
            match assign_num_literal_expr(
                env,
                layout_cache,
                assigned,
                variable,
                &float_str,
                IntOrFloatValue::Float(float),
                hole,
            ) {
                Ok(stmt) => stmt,
                Err(_) => hole.clone(),
            }
        }

        Num(_, num_str, num, _bound) => {
            match assign_num_literal_expr(
                env,
                layout_cache,
                assigned,
                variable,
                &num_str,
                IntOrFloatValue::Int(num),
                hole,
            ) {
                Ok(stmt) => stmt,
                Err(_) => hole.clone(),
            }
        }

        Str(string) => Stmt::Let(
            assigned,
            Expr::Literal(Literal::Str(arena.alloc(string))),
            Layout::STR,
            hole,
        ),

        IngestedFile(_, bytes, var) => {
            let interned = layout_cache.from_var(env.arena, var, env.subs).unwrap();
            let layout = layout_cache.get_repr(interned);

            match layout {
                LayoutRepr::Builtin(Builtin::List(elem_layout)) if elem_layout == Layout::U8 => {
                    let mut elements = Vec::with_capacity_in(bytes.len(), env.arena);
                    for byte in bytes.iter() {
                        elements.push(ListLiteralElement::Literal(Literal::Byte(*byte)));
                    }
                    let expr = Expr::Array {
                        elem_layout,
                        elems: elements.into_bump_slice(),
                    };

                    Stmt::Let(assigned, expr, interned, hole)
                }
                LayoutRepr::Builtin(Builtin::Str) => Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Str(
                        // This is safe because we ensure the utf8 bytes are valid earlier in the compiler pipeline.
                        arena.alloc(
                            unsafe { std::str::from_utf8_unchecked(bytes.as_ref()) }.to_owned(),
                        ),
                    )),
                    Layout::STR,
                    hole,
                ),
                _ => {
                    // This will not manifest as a real runtime error and is just returned to have a value here.
                    // The actual type error during solve will be fatal.
                    runtime_error(env, "Invalid type for ingested file")
                }
            }
        }
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
                layout_cache,
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
        ParamsVar { .. } => {
            internal_error!("ParamsVar should've been lowered to Var")
        }
        ImportParams(_, _, Some((_, value))) => {
            with_hole(env, *value, variable, procs, layout_cache, assigned, hole)
        }
        ImportParams(_, _, None) => {
            internal_error!("Missing module params should've been dropped by now");
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
                Content::Structure(FlatType::Func(_, _, _, _))
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

            if let Content::Structure(FlatType::Func(arg_vars, _, ret_var, _fx_var)) = content {
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

            match can_reuse_symbol(env, layout_cache, procs, &loc_arg_expr.value, arg_var) {
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

        Tuple {
            tuple_var, elems, ..
        } => {
            let sorted_elems_result = {
                let mut layout_env =
                    layout::Env::from_components(layout_cache, env.subs, env.arena);
                layout::sort_tuple_elems(&mut layout_env, tuple_var)
            };
            let sorted_elems = match sorted_elems_result {
                Ok(elems) => elems,
                Err(_) => return runtime_error(env, "Can't create tuple with improper layout"),
            };

            // Hacky way to let us remove the owned elements from the vector, possibly out-of-order.
            let mut elems = Vec::from_iter_in(elems.into_iter().map(Some), env.arena);
            let take_elem_expr = move |index: usize| elems[index].take();

            compile_struct_like(
                env,
                procs,
                layout_cache,
                sorted_elems,
                take_elem_expr,
                tuple_var,
                hole,
                assigned,
            )
        }

        Record {
            record_var,
            mut fields,
            ..
        } => {
            let sorted_fields_result = {
                let mut layout_env =
                    layout::Env::from_components(layout_cache, env.subs, env.arena);
                layout::sort_record_fields(&mut layout_env, record_var)
            };
            let sorted_fields = match sorted_fields_result {
                Ok(fields) => fields,
                Err(_) => return runtime_error(env, "Can't create record with improper layout"),
            };

            let take_field_expr =
                move |field: Lowercase| fields.remove(&field).map(|f| (f.var, f.loc_expr));

            compile_struct_like(
                env,
                procs,
                layout_cache,
                sorted_fields,
                take_field_expr,
                record_var,
                hole,
                assigned,
            )
        }

        EmptyRecord => let_empty_struct(assigned, hole),

        Expect { .. } => unreachable!("I think this is unreachable"),
        Dbg {
            source_location,
            source,
            loc_message,
            loc_continuation,
            variable: cond_variable,
            symbol: dbg_symbol,
        } => {
            let rest = with_hole(
                env,
                loc_continuation.value,
                variable,
                procs,
                layout_cache,
                assigned,
                hole,
            );

            compile_dbg(
                env,
                procs,
                layout_cache,
                &*arena.alloc(source_location),
                &*arena.alloc(source),
                dbg_symbol,
                *loc_message,
                cond_variable,
                rest,
            )
        }

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
                                panic!("TODO turn fn_var into a RuntimeError {err:?}")
                            });

                        let param = Param {
                            symbol: assigned,
                            layout,
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
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {err:?}"));

            let param = Param {
                symbol: assigned,
                layout,
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
                    let list_layout = layout_cache
                        .put_in_direct_no_semantic(LayoutRepr::Builtin(Builtin::List(elem_layout)));
                    Stmt::Let(assigned, expr, list_layout, hole)
                }
                Err(LayoutProblem::UnresolvedTypeVar(_)) => {
                    let expr = Expr::EmptyArray;
                    let list_layout = layout_cache.put_in_direct_no_semantic(LayoutRepr::Builtin(
                        Builtin::List(Layout::VOID),
                    ));
                    Stmt::Let(assigned, expr, list_layout, hole)
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

            let elem_layout = match layout_cache.from_var(env.arena, elem_var, env.subs) {
                Ok(elem_layout) => elem_layout,
                Err(_) => return runtime_error(env, "invalid list element type"),
            };

            for arg_expr in loc_elems.into_iter() {
                if let Some(literal) =
                    try_make_literal(&layout_cache.interner, &arg_expr.value, elem_layout)
                {
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

            let list_layout = layout_cache
                .put_in_direct_no_semantic(LayoutRepr::Builtin(Builtin::List(elem_layout)));

            let stmt = Stmt::Let(assigned, expr, list_layout, hole);

            let iter = symbol_exprs
                .into_iter()
                .rev()
                .map(|e| (elem_var, e))
                .zip(arg_symbols.iter().rev());

            assign_to_symbols(env, procs, layout_cache, iter, stmt)
        }

        RecordAccess {
            record_var,
            field_var,
            field,
            loc_expr,
            ..
        } => {
            let sorted_fields_result = {
                let mut layout_env =
                    layout::Env::from_components(layout_cache, env.subs, env.arena);
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

            let struct_index = match index {
                Some(index) => index,
                None => return runtime_error(env, "No such field in record"),
            };

            compile_struct_like_access(
                env,
                procs,
                layout_cache,
                field_layouts,
                struct_index,
                *loc_expr,
                record_var,
                hole,
                assigned,
                field_var,
            )
        }

        RecordAccessor(accessor_data) => {
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
                        RawFunctionLayout::ErasedFunction(_, _) => todo_lambda_erasure!(),
                        RawFunctionLayout::ZeroArgumentThunk(_) => unreachable!(),
                    }
                }

                Err(_error) => runtime_error(
                    env,
                    "TODO convert anonymous function error to a RuntimeError string",
                ),
            }
        }

        TupleAccess {
            tuple_var,
            elem_var,
            index: accessed_index,
            loc_expr,
            ..
        } => {
            let sorted_elems_result = {
                let mut layout_env =
                    layout::Env::from_components(layout_cache, env.subs, env.arena);
                layout::sort_tuple_elems(&mut layout_env, tuple_var)
            };
            let sorted_elems = match sorted_elems_result {
                Ok(fields) => fields,
                Err(_) => return runtime_error(env, "Can't access tuple with improper layout"),
            };
            let mut field_layouts = Vec::with_capacity_in(sorted_elems.len(), env.arena);

            let mut final_index = None;

            for (current, (index, _, elem_layout)) in sorted_elems.into_iter().enumerate() {
                field_layouts.push(elem_layout);

                if index == accessed_index {
                    final_index = Some(current);
                }
            }

            let tuple_index = match final_index {
                Some(index) => index as u64,
                None => return runtime_error(env, "No such index in tuple"),
            };

            compile_struct_like_access(
                env,
                procs,
                layout_cache,
                field_layouts,
                tuple_index,
                *loc_expr,
                tuple_var,
                hole,
                assigned,
                elem_var,
            )
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
                        RawFunctionLayout::ErasedFunction(..) => todo_lambda_erasure!(),
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

        RecordUpdate {
            record_var,
            symbol: structure,
            ref updates,
            ..
        } => {
            use FieldType::*;

            enum FieldType<'a> {
                CopyExisting,
                UpdateExisting(&'a roc_can::expr::Field),
            }

            // Strategy: turn a record update into the creation of a new record.
            // This has the benefit that we don't need to do anything special for reference
            // counting
            let sorted_fields_result = {
                let mut layout_env =
                    layout::Env::from_components(layout_cache, env.subs, env.arena);
                layout::sort_record_fields(&mut layout_env, record_var)
            };

            let sorted_fields = match sorted_fields_result {
                Ok(fields) => fields,
                Err(_) => return runtime_error(env, "Can't update record with improper layout"),
            };

            let sorted_fields_filtered =
                sorted_fields
                    .iter()
                    .filter_map(|(label, _, opt_field_layout)| {
                        match opt_field_layout {
                            Ok(_) => Some(label),
                            Err(_) => {
                                debug_assert!(!updates.contains_key(label));
                                // this was an optional field, and now does not exist!
                                None
                            }
                        }
                    });
            let sorted_fields = Vec::from_iter_in(sorted_fields_filtered, env.arena);

            let single_field_struct = sorted_fields.len() == 1;

            // The struct indexing generated by the current context
            let mut current_struct_indexing = Vec::with_capacity_in(sorted_fields.len(), env.arena);
            // The symbols that are used to create the new struct
            let mut new_struct_symbols = Vec::with_capacity_in(sorted_fields.len(), env.arena);
            // Information about the fields that are being updated
            let mut fields = Vec::with_capacity_in(sorted_fields.len(), env.arena);

            // Create a symbol for each of the fields as they might be referenced later.
            // The struct with a single field is optimized in such a way that replacing later indexing will cause an incorrect IR.
            // Thus, only insert these struct_indices if there is more than one field in the struct.
            if !single_field_struct {
                for index in 0..sorted_fields.len() {
                    let record_index = (structure, index as u64);

                    current_struct_indexing.push(record_index);

                    let original_struct_symbol = env.unique_symbol();
                    env.struct_indexing
                        .insert(record_index, original_struct_symbol);
                }
            }

            for (index, label) in sorted_fields.iter().enumerate() {
                let record_index = (structure, index as u64);

                if let Some(field) = updates.get(label) {
                    let new_struct_symbol = possible_reuse_symbol_or_specialize(
                        env,
                        procs,
                        layout_cache,
                        &field.loc_expr.value,
                        field.var,
                    );
                    new_struct_symbols.push(new_struct_symbol);
                    fields.push(UpdateExisting(field));
                } else {
                    new_struct_symbols.push(*env.struct_indexing.get(record_index).unwrap());
                    fields.push(CopyExisting);
                }
            }

            let new_struct_symbols = new_struct_symbols.into_bump_slice();

            let record_layout = layout_cache
                .from_var(env.arena, record_var, env.subs)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {err:?}"));

            let field_layouts = match layout_cache.get_repr(record_layout) {
                LayoutRepr::Struct(field_layouts) => field_layouts,
                _ => arena.alloc([record_layout]),
            };

            if single_field_struct {
                // TODO we can probably special-case this more, skipping the generation of
                // UpdateExisting
                let mut stmt = hole.clone();

                let what_to_do = &fields[0];

                match what_to_do {
                    UpdateExisting(field) => {
                        substitute_in_exprs(env.arena, &mut stmt, assigned, new_struct_symbols[0]);

                        stmt = assign_to_symbol(
                            env,
                            procs,
                            layout_cache,
                            field.var,
                            *field.loc_expr.clone(),
                            new_struct_symbols[0],
                            stmt,
                        );
                    }
                    CopyExisting => {
                        unreachable!(
                            r"when a record has just one field and is updated, it must update that one field"
                        );
                    }
                }

                stmt
            } else {
                let expr = Expr::Struct(new_struct_symbols);
                let mut stmt = Stmt::Let(assigned, expr, record_layout, hole);

                for (new_struct_symbol, what_to_do) in new_struct_symbols.iter().zip(fields) {
                    match what_to_do {
                        UpdateExisting(field) => {
                            stmt = assign_to_symbol(
                                env,
                                procs,
                                layout_cache,
                                field.var,
                                *field.loc_expr.clone(),
                                *new_struct_symbol,
                                stmt,
                            );
                        }
                        CopyExisting => {
                            // When a field is copied, the indexing symbol is already placed in new_struct_symbols
                            // Thus, we don't need additional logic here.
                        }
                    }
                }

                let structure_needs_specialization =
                    procs.ability_member_aliases.get(structure).is_some()
                        || procs.is_module_thunk(structure)
                        || procs.is_imported_module_thunk(structure);

                let specialized_structure_sym = if structure_needs_specialization {
                    // We need to specialize the record now; create a new one for it.
                    env.unique_symbol()
                } else {
                    // The record is already good.
                    structure
                };

                for record_index in current_struct_indexing.into_iter().rev() {
                    if let Some(symbol) = env.struct_indexing.get_used(&record_index) {
                        let layout = field_layouts[record_index.1 as usize];
                        let access_expr = Expr::StructAtIndex {
                            structure: specialized_structure_sym,
                            index: record_index.1,
                            field_layouts,
                        };
                        stmt = Stmt::Let(symbol, access_expr, layout, arena.alloc(stmt));
                    };
                }

                if structure_needs_specialization {
                    stmt = specialize_symbol(
                        env,
                        procs,
                        layout_cache,
                        Some(record_var),
                        specialized_structure_sym,
                        env.arena.alloc(stmt),
                        structure,
                    );
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
                RawFunctionLayout::ErasedFunction(argument_layouts, ret_layout) => {
                    let captured_symbols = if captured_symbols.is_empty() {
                        CapturedSymbols::None
                    } else {
                        let captured_symbols = Vec::from_iter_in(captured_symbols, env.arena);
                        let captured_symbols = captured_symbols.into_bump_slice();
                        CapturedSymbols::Captured(captured_symbols)
                    };
                    let resolved_erased_lambda = ResolvedErasedLambda::new(
                        env,
                        layout_cache,
                        name,
                        captured_symbols,
                        argument_layouts,
                        ret_layout,
                    );

                    let inserted = procs.insert_anonymous(
                        env,
                        resolved_erased_lambda.lambda_name(),
                        function_type,
                        arguments,
                        loc_body,
                        captured_symbols,
                        return_type,
                        layout_cache,
                    );

                    if let Err(e) = inserted {
                        return runtime_error(env, env.arena.alloc(format!("RuntimeError: {e:?}")));
                    }
                    drop(inserted);

                    build_erased_function(env, layout_cache, resolved_erased_lambda, assigned, hole)
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
                            env.arena.alloc(format!("RuntimeError: {e:?}",)),
                        );
                    }
                    drop(inserted);

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
            let (fn_var, loc_expr, _lambda_set_var, _ret_var, _fx_var) = *boxed;

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
                    match can_reuse_symbol(env, layout_cache, procs, &loc_expr.value, fn_var) {
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

                                    let lambda_set_layout = lambda_set.full_layout;

                                    result = force_thunk(
                                        env,
                                        thunk_name,
                                        lambda_set_layout,
                                        function_symbol,
                                        env.arena.alloc(result),
                                    );
                                }
                                RawFunctionLayout::ErasedFunction(..) => todo_lambda_erasure!(),
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
                                RawFunctionLayout::ErasedFunction(..) => todo_lambda_erasure!(),
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
                                        Resolved::Derive(_) => {
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
                                RawFunctionLayout::ErasedFunction(arg_layouts, ret_layout) => {
                                    let hole_layout =
                                        layout_cache.from_var(env.arena, fn_var, env.subs).unwrap();
                                    result = erased::call_erased_function(
                                        env,
                                        layout_cache,
                                        procs,
                                        loc_expr.value,
                                        fn_var,
                                        (arg_layouts, ret_layout),
                                        arg_symbols,
                                        assigned,
                                        hole,
                                        hole_layout,
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
                    ret_layout: layout,
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

                    let arena = env.arena;

                    match closure_data_layout {
                        RawFunctionLayout::Function(_, lambda_set, _) =>  {
                            lowlevel_match_on_lambda_set(
                                env,
                                layout_cache,
                                lambda_set,
                                op,
                                closure_data_symbol,
                                |(lambda_name, closure_data, closure_env_layout,  specialization_id, update_mode)| {
                                    // Build a call for a specific lambda in the set
                                    let top_level = ProcLayout::from_raw_named(env.arena, lambda_name, closure_data_layout);
                                    let arg_layouts = top_level.arguments;
                                    let ret_layout = top_level.result;

                                    let passed_function = PassedFunction {
                                        name: lambda_name,
                                        captured_environment: closure_data_symbol,
                                        owns_captured_environment: true,
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
                                        arguments: arena.alloc([$($x,)* lambda_name.name(), closure_data]),
                                    }
                                },
                                layout,
                                assigned,
                                hole,
                            )
                        }
                        RawFunctionLayout::ErasedFunction(..) => todo_lambda_erasure!(),
                        RawFunctionLayout::ZeroArgumentThunk(_) => unreachable!("match_on_closure_argument received a zero-argument thunk"),
                    }
                }};
            }

            use LowLevel::*;
            match op {
                ListSortWith => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListSortWith, [xs])
                }
                BoxExpr => {
                    debug_assert_eq!(arg_symbols.len(), 1);
                    let x = arg_symbols[0];

                    let element_layout = match layout_cache.interner.get_repr(layout) {
                        LayoutRepr::Union(UnionLayout::NonNullableUnwrapped([l])) => l,
                        _ => unreachable!("invalid layout for a box expression"),
                    };

                    let expr = boxed::box_(arena.alloc(x), element_layout);

                    Stmt::Let(assigned, expr, layout, hole)
                }
                UnboxExpr => {
                    debug_assert_eq!(arg_symbols.len(), 1);
                    let x = arg_symbols[0];

                    let expr = boxed::unbox(x, arena.alloc(layout));

                    Stmt::Let(assigned, expr, layout, hole)
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
        Try {
            result_expr,
            result_var,
            return_var,
            ok_payload_var,
            err_payload_var,
            err_ext_var,
            kind: _,
        } => {
            let ok_symbol = env.unique_symbol();
            let err_symbol = env.unique_symbol();

            let ok_branch = WhenBranch {
                patterns: vec![WhenBranchPattern {
                    pattern: Loc::at_zero(roc_can::pattern::Pattern::AppliedTag {
                        whole_var: result_var,
                        ext_var: Variable::EMPTY_TAG_UNION,
                        tag_name: "Ok".into(),
                        arguments: vec![(
                            ok_payload_var,
                            Loc::at_zero(roc_can::pattern::Pattern::Identifier(ok_symbol)),
                        )],
                    }),
                    degenerate: false,
                }],
                value: Loc::at_zero(Var(ok_symbol, ok_payload_var)),
                guard: None,
                redundant: RedundantMark::known_non_redundant(),
            };

            let err_branch = WhenBranch {
                patterns: vec![WhenBranchPattern {
                    pattern: Loc::at_zero(roc_can::pattern::Pattern::AppliedTag {
                        whole_var: result_var,
                        ext_var: err_ext_var,
                        tag_name: "Err".into(),
                        arguments: vec![(
                            err_payload_var,
                            Loc::at_zero(roc_can::pattern::Pattern::Identifier(err_symbol)),
                        )],
                    }),
                    degenerate: false,
                }],
                value: Loc::at_zero(Return {
                    return_var,
                    return_value: Box::new(Loc::at_zero(Tag {
                        tag_union_var: return_var,
                        ext_var: err_ext_var,
                        name: "Err".into(),
                        arguments: vec![(
                            err_payload_var,
                            Loc::at_zero(Var(err_symbol, err_payload_var)),
                        )],
                    })),
                }),
                guard: None,
                redundant: RedundantMark::known_non_redundant(),
            };

            let result_region = result_expr.region;
            let when_expr = When {
                loc_cond: result_expr,
                cond_var: result_var,
                expr_var: ok_payload_var,
                region: result_region,
                branches: vec![ok_branch, err_branch],
                branches_cond_var: result_var,
                exhaustive: ExhaustiveMark::known_exhaustive(),
            };

            with_hole(
                env,
                when_expr,
                variable,
                procs,
                layout_cache,
                assigned,
                hole,
            )
        }
        Return {
            return_value,
            return_var,
        } => {
            let return_symbol = possible_reuse_symbol_or_specialize(
                env,
                procs,
                layout_cache,
                &return_value.value,
                return_var,
            );

            assign_to_symbol(
                env,
                procs,
                layout_cache,
                return_var,
                *return_value,
                return_symbol,
                Stmt::Ret(return_symbol),
            )
        }
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

/// Compiles a `dbg` expression.
fn compile_dbg<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    source_location: &'a str,
    source: &'a str,
    dbg_symbol: Symbol,
    loc_message: Loc<roc_can::expr::Expr>,
    variable: Variable,
    continuation: Stmt<'a>,
) -> Stmt<'a> {
    let spec_var = env
        .expectation_subs
        .as_mut()
        .unwrap()
        .fresh_unnamed_flex_var();

    let dbg_stmt = Stmt::Dbg {
        source_location,
        source,
        symbol: dbg_symbol,
        variable: spec_var,
        remainder: env.arena.alloc(continuation),
    };

    // Now that the dbg value has been specialized, export its specialized type into the
    // expectations subs.
    store_specialized_expectation_lookups(env, [variable], &[spec_var]);

    let symbol_is_reused = matches!(
        can_reuse_symbol(env, layout_cache, procs, &loc_message.value, variable),
        ReuseSymbol::Value(_)
    );

    // skip evaluating the message if it's just a symbol
    if symbol_is_reused {
        dbg_stmt
    } else {
        with_hole(
            env,
            loc_message.value,
            variable,
            procs,
            layout_cache,
            dbg_symbol,
            env.arena.alloc(dbg_stmt),
        )
    }
}

/// Compiles an access into a tuple or record.
fn compile_struct_like_access<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    field_layouts: Vec<'a, InLayout<'a>>,
    index: u64,
    loc_expr: Loc<roc_can::expr::Expr>,
    struct_like_var: Variable,
    hole: &'a Stmt<'a>,
    assigned: Symbol,
    elem_var: Variable,
) -> Stmt<'a> {
    let struct_symbol = possible_reuse_symbol_or_specialize(
        env,
        procs,
        layout_cache,
        &loc_expr.value,
        struct_like_var,
    );

    let mut stmt = match field_layouts.as_slice() {
        [_] => {
            let mut hole = hole.clone();
            substitute_in_exprs(env.arena, &mut hole, assigned, struct_symbol);

            hole
        }
        _ => {
            let expr = Expr::StructAtIndex {
                index,
                field_layouts: field_layouts.into_bump_slice(),
                structure: struct_symbol,
            };

            let layout = layout_cache
                .from_var(env.arena, elem_var, env.subs)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {err:?}"));

            Stmt::Let(assigned, expr, layout, hole)
        }
    };

    stmt = assign_to_symbol(
        env,
        procs,
        layout_cache,
        struct_like_var,
        loc_expr,
        struct_symbol,
        stmt,
    );

    stmt
}

/// Compiles a record or a tuple.
// TODO: UnusedLayout is because `sort_record_fields` currently returns a three-tuple, but is, in
// fact, unneeded for the compilation.
fn compile_struct_like<'a, L, UnusedLayout>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    sorted_elems: Vec<(L, Variable, UnusedLayout)>,
    mut take_elem_expr: impl FnMut(L) -> Option<(Variable, Box<Loc<roc_can::expr::Expr>>)>,
    struct_like_var: Variable,
    hole: &'a Stmt<'a>,
    assigned: Symbol,
) -> Stmt<'a> {
    let mut elem_symbols = Vec::with_capacity_in(sorted_elems.len(), env.arena);
    let mut can_elems = Vec::with_capacity_in(sorted_elems.len(), env.arena);

    #[allow(clippy::enum_variant_names)]
    enum Field {
        // TODO: rename this since it can handle unspecialized expressions now too
        FunctionOrUnspecialized(Symbol, Variable),
        ValueSymbol,
        Field(Variable, Loc<roc_can::expr::Expr>),
    }

    for (index, variable, _) in sorted_elems.into_iter() {
        // TODO how should function pointers be handled here?
        use ReuseSymbol::*;
        match take_elem_expr(index) {
            Some((var, loc_expr)) => {
                match can_reuse_symbol(env, layout_cache, procs, &loc_expr.value, var) {
                    Imported(symbol) => {
                        // we cannot re-use the symbol in this case; it is used as a value, but defined as a thunk
                        elem_symbols.push(env.unique_symbol());
                        can_elems.push(Field::FunctionOrUnspecialized(symbol, variable));
                    }
                    LocalFunction(symbol) | UnspecializedExpr(symbol) => {
                        elem_symbols.push(symbol);
                        can_elems.push(Field::FunctionOrUnspecialized(symbol, variable));
                    }
                    Value(symbol) => {
                        let reusable = procs.get_or_insert_symbol_specialization(
                            env,
                            layout_cache,
                            symbol,
                            var,
                        );
                        elem_symbols.push(reusable);
                        can_elems.push(Field::ValueSymbol);
                    }
                    NotASymbol => {
                        elem_symbols.push(env.unique_symbol());
                        can_elems.push(Field::Field(var, *loc_expr));
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
    let layout = match layout_cache.from_var(env.arena, struct_like_var, env.subs) {
        Ok(layout) => layout,
        Err(_) => return runtime_error(env, "Can't create record with improper layout"),
    };

    let elem_symbols = elem_symbols.into_bump_slice();

    let mut stmt = if let [only_field] = elem_symbols {
        let mut hole = hole.clone();
        substitute_in_exprs(env.arena, &mut hole, assigned, *only_field);
        hole
    } else {
        Stmt::Let(assigned, Expr::Struct(elem_symbols), layout, hole)
    };

    for (opt_field, symbol) in can_elems.into_iter().rev().zip(elem_symbols.iter().rev()) {
        match opt_field {
            Field::ValueSymbol => {
                // this symbol is already defined; nothing to do
            }
            Field::FunctionOrUnspecialized(can_symbol, variable) => {
                stmt = specialize_symbol(
                    env,
                    procs,
                    layout_cache,
                    Some(variable),
                    *symbol,
                    env.arena.alloc(stmt),
                    can_symbol,
                );
            }
            Field::Field(var, loc_expr) => {
                stmt = with_hole(
                    env,
                    loc_expr.value,
                    var,
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

#[inline(always)]
fn late_resolve_ability_specialization(
    env: &mut Env<'_, '_>,
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
    } else if let Content::Structure(FlatType::Func(_, lambda_set, _, _fx_var)) =
        env.subs.get_content_without_compacting(specialization_var)
    {
        // Fast path: the member is a function, so the lambda set will tell us the
        // specialization.
        use roc_types::subs::LambdaSet;
        let LambdaSet {
            solved,
            unspecialized,
            recursion_var: _,
            ambient_function,
        } = env.subs.get_lambda_set(*lambda_set);

        debug_assert!(unspecialized.is_empty());
        let mut iter_lambda_set = solved.iter_all();
        debug_assert_eq!(
            iter_lambda_set.len(),
            1,
            "{:?}",
            (env.subs.dbg(*lambda_set), env.subs.dbg(ambient_function))
        );
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
        .expect("Ability specialization is unknown. Tip: check out <https://roc.zulipchat.com/#narrow/stream/231634-beginners/topic/Non-Functions.20in.20Abilities/near/456068617>");

        match specialization {
            Resolved::Specialization(symbol) => symbol,
            Resolved::Derive(derive_key) => {
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
    let lambda_set_layout = lambda_set.full_layout;
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

            combined.sort_by(|(_, layout1), (_, layout2)| {
                let size1 = layout_cache
                    .get_repr(**layout1)
                    .alignment_bytes(&layout_cache.interner);
                let size2 = layout_cache
                    .get_repr(**layout2)
                    .alignment_bytes(&layout_cache.interner);

                size2.cmp(&size1)
            });

            let symbols =
                Vec::from_iter_in(combined.iter().map(|(a, _)| *a), env.arena).into_bump_slice();

            let expr = Expr::Tag {
                tag_id,
                tag_layout: union_layout,
                arguments: symbols,
                reuse: None,
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

            combined.sort_by(|(_, layout1), (_, layout2)| {
                let size1 = layout_cache
                    .get_repr(**layout1)
                    .alignment_bytes(&layout_cache.interner);
                let size2 = layout_cache
                    .get_repr(**layout2)
                    .alignment_bytes(&layout_cache.interner);

                size2.cmp(&size1)
            });

            let symbols =
                Vec::from_iter_in(combined.iter().map(|(a, _)| *a), env.arena).into_bump_slice();
            let field_layouts =
                Vec::from_iter_in(combined.iter().map(|(_, b)| **b), env.arena).into_bump_slice();

            debug_assert_eq!(
                LayoutRepr::struct_(field_layouts),
                layout_cache.get_repr(lambda_set.runtime_representation())
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
        let mut layout_env = layout::Env::from_components(layout_cache, env.subs, env.arena);
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
            Layout::BOOL,
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
                    Layout::U8,
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
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {err:?}"));

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
                    .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {err:?}"));

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
            let variant_layout = return_on_layout_error!(
                env,
                layout_cache.from_var(env.arena, variant_var, env.subs),
                "Wrapped"
            );
            let union_layout = match layout_cache.interner.chase_recursive(variant_layout) {
                LayoutRepr::Union(ul) => ul,
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

                    let mut layouts: Vec<&'a [InLayout<'a>]> =
                        Vec::with_capacity_in(sorted_tag_layouts.len(), env.arena);

                    for (_, arg_layouts) in sorted_tag_layouts.into_iter() {
                        layouts.push(arg_layouts);
                    }

                    let tag = Expr::Tag {
                        tag_layout: union_layout,
                        tag_id: tag_id as _,
                        arguments: field_symbols,
                        reuse: None,
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
                        reuse: None,
                    };

                    (tag, union_layout)
                }
                NonRecursive { sorted_tag_layouts } => {
                    field_symbols = {
                        let mut temp = Vec::with_capacity_in(field_symbols_temp.len(), arena);

                        temp.extend(field_symbols_temp.iter().map(|r| r.1));

                        temp.into_bump_slice()
                    };

                    let mut layouts: Vec<&'a [InLayout<'a>]> =
                        Vec::with_capacity_in(sorted_tag_layouts.len(), env.arena);

                    for (_, arg_layouts) in sorted_tag_layouts.into_iter() {
                        layouts.push(arg_layouts);
                    }

                    let tag = Expr::Tag {
                        tag_layout: union_layout,
                        tag_id: tag_id as _,
                        arguments: field_symbols,
                        reuse: None,
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

                    let mut layouts: Vec<&'a [InLayout<'a>]> =
                        Vec::with_capacity_in(sorted_tag_layouts.len(), env.arena);

                    for (_, arg_layouts) in sorted_tag_layouts.into_iter() {
                        layouts.push(arg_layouts);
                    }

                    let tag = Expr::Tag {
                        tag_layout: union_layout,
                        tag_id: tag_id as _,
                        arguments: field_symbols,
                        reuse: None,
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
                        reuse: None,
                    };

                    (tag, union_layout)
                }
            };

            let union_layout =
                layout_cache.put_in_direct_no_semantic(LayoutRepr::Union(union_layout));

            let stmt = Stmt::Let(assigned, tag, union_layout, hole);
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
                RawFunctionLayout::ErasedFunction(..) => todo_lambda_erasure!(),
                RawFunctionLayout::ZeroArgumentThunk(_) => unreachable!(),
            }
        }

        Err(e) => runtime_error(
            env,
            env.arena.alloc(format!(
                "Could not produce tag function due to a runtime error: {e:?}",
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

        let alignment = layout_cache
            .get_repr(layout)
            .alignment_bytes(&layout_cache.interner);

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
            Content::Structure(FlatType::Func(args, closure_var, ret, _fx_var)) => {
                let lambda_set_layout = {
                    LambdaSet::from_var_pub(
                        layout_cache,
                        env.arena,
                        env.subs,
                        args,
                        closure_var,
                        ret,
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
            let ret_layout = return_on_layout_error!(
                env,
                layout_cache.from_var(env.arena, branch_var, env.subs),
                "invalid return type in if expression"
            );
            let cond_layout = return_on_layout_error!(
                env,
                layout_cache.from_var(env.arena, cond_var, env.subs),
                "invalid condition type in if expression"
            );

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
            let mut lookup_variables = Vec::with_capacity_in(lookups_in_cond.len(), env.arena);
            let mut specialized_variables = Vec::with_capacity_in(lookups_in_cond.len(), env.arena);

            for ExpectLookup {
                symbol,
                var,
                ability_info,
            } in lookups_in_cond.iter().copied()
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

                let expectation_subs = env
                    .expectation_subs
                    .as_deref_mut()
                    .expect("if expects are compiled, their subs should be available");
                let spec_var = expectation_subs.fresh_unnamed_flex_var();

                if !env.subs.is_function(var) {
                    // Exclude functions from lookups
                    lookups.push(symbol);
                    lookup_variables.push(var);
                    specialized_variables.push(spec_var);
                }
            }

            let specialized_variables = specialized_variables.into_bump_slice();

            let mut stmt = Stmt::Expect {
                condition: cond_symbol,
                region: loc_condition.region,
                lookups: lookups.into_bump_slice(),
                variables: specialized_variables,
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

            // Now that the condition has been specialized, export the specialized types of our
            // lookups into the expectation subs.
            store_specialized_expectation_lookups(env, lookup_variables, specialized_variables);

            stmt
        }

        Dbg {
            source_location,
            source,
            loc_message,
            loc_continuation,
            variable: cond_variable,
            symbol: dbg_symbol,
        } => {
            let rest = from_can(env, variable, loc_continuation.value, procs, layout_cache);

            compile_dbg(
                env,
                procs,
                layout_cache,
                &*env.arena.alloc(source_location),
                &*env.arena.alloc(source),
                dbg_symbol,
                *loc_message,
                cond_variable,
                rest,
            )
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

fn store_specialized_expectation_lookups(
    env: &mut Env,
    lookup_variables: impl IntoIterator<Item = Variable>,
    specialized_variables: &[Variable],
) {
    let subs = &env.subs;
    let expectation_subs = env.expectation_subs.as_deref_mut().unwrap();
    for (lookup_var, stored_var) in lookup_variables.into_iter().zip(specialized_variables) {
        let stored_specialized_var =
            storage_copy_var_to(&mut Default::default(), subs, expectation_subs, lookup_var);
        let stored_specialized_desc = expectation_subs.get(stored_specialized_var);
        expectation_subs.union(*stored_var, stored_specialized_var, stored_specialized_desc);
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
                                kind: roc_can::def::DefKind::Let,
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

            use decision_tree::Guard;
            let result = if let Some(loc_expr) = opt_guard {
                let guard_spec = GuardStmtSpec {
                    guard_expr: loc_expr.value,
                    identity: env.next_call_specialization_id(),
                };

                (
                    pattern.clone(),
                    Guard::Guard {
                        pattern,
                        stmt_spec: guard_spec,
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

    decision_tree::optimize_when(
        env,
        procs,
        layout_cache,
        cond_symbol,
        cond_layout,
        ret_layout,
        mono_branches,
    )
}

/// A functor to generate IR for a guard under a `when` branch.
/// Used in the decision tree compiler, after building a decision tree and converting into IR.
///
/// A guard might appear more than once in various places in the compiled decision tree, so the
/// functor here may be called more than once. As such, it implements clone, which duplicates the
/// guard AST for subsequent IR-regeneration. This is a bit wasteful, but in practice, guard ASTs
/// are quite small. Moreoever, they must be generated on a per-case basis, since the guard may
/// have calls or joins, whose specialization IDs and joinpoint IDs, respectively, must be unique.
#[derive(Debug, Clone)]
pub(crate) struct GuardStmtSpec {
    guard_expr: roc_can::expr::Expr,

    /// Unique id to indentity identical guard statements, even across clones.
    /// Needed so that we can implement [PartialEq] on this type. Re-uses call specialization IDs,
    /// since the identity is kind of irrelevant.
    identity: CallSpecId,
}

impl PartialEq for GuardStmtSpec {
    fn eq(&self, other: &Self) -> bool {
        self.identity == other.identity
    }
}

impl std::hash::Hash for GuardStmtSpec {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.identity.id.hash(state);
    }
}

impl GuardStmtSpec {
    /// Generates IR for the guard, and the joinpoint that the guard will jump to with the
    /// calculated guard boolean value.
    ///
    /// The caller should create a joinpoint with the given joinpoint ID and decide how to branch
    /// after the guard has been evaluated.
    ///
    /// The compiled guard statement expects the pattern before the guard to be destructed before the
    /// returned statement. The caller should layer on the pattern destructuring, as bound from the
    /// `when` condition value.
    pub(crate) fn generate_guard_and_join<'a>(
        self,
        env: &mut Env<'a, '_>,
        procs: &mut Procs<'a>,
        layout_cache: &mut LayoutCache<'a>,
    ) -> CompiledGuardStmt<'a> {
        let Self {
            guard_expr,
            identity: _,
        } = self;

        let join_point_id = JoinPointId(env.unique_symbol());
        let symbol = env.unique_symbol();
        let jump = env
            .arena
            .alloc(Stmt::Jump(join_point_id, env.arena.alloc([symbol])));

        let stmt = with_hole(
            env,
            guard_expr,
            Variable::BOOL,
            procs,
            layout_cache,
            symbol,
            jump,
        );

        CompiledGuardStmt {
            join_point_id,
            stmt,
        }
    }
}

pub(crate) struct CompiledGuardStmt<'a> {
    pub join_point_id: JoinPointId,
    pub stmt: Stmt<'a>,
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

pub(crate) fn substitute_in_exprs_many<'a>(
    arena: &'a Bump,
    stmt: &mut Stmt<'a>,
    subs: BumpMap<Symbol, Symbol>,
) {
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
            let mut did_change = false;

            let cond_symbol = match substitute(subs, *cond_symbol) {
                Some(s) => {
                    did_change = true;
                    s
                }
                None => *cond_symbol,
            };

            let opt_default = substitute_in_stmt_help(arena, default_branch.1, subs);

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
                    cond_symbol,
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

        Dbg {
            source_location,
            source,
            symbol,
            variable,
            remainder,
        } => {
            let new_remainder =
                substitute_in_stmt_help(arena, remainder, subs).unwrap_or(remainder);

            let expect = Dbg {
                source_location,
                source,
                symbol: substitute(subs, *symbol).unwrap_or(*symbol),
                variable: *variable,
                remainder: new_remainder,
            };

            Some(arena.alloc(expect))
        }

        Expect {
            condition,
            region,
            lookups,
            variables,
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
                variables,
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
            ret_layout: *ret_layout,
            specialization_id: *specialization_id,
        }),
        CallType::ByPointer {
            pointer,
            arg_layouts,
            ret_layout,
        } => substitute(subs, *pointer).map(|new| CallType::ByPointer {
            pointer: new,
            arg_layouts,
            ret_layout: *ret_layout,
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
        Literal(_) | EmptyArray => None,

        Call(call) => substitute_in_call(arena, call, subs).map(Expr::Call),

        Tag {
            tag_layout,
            tag_id,
            arguments: args,
            reuse,
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

            let reuse = match *reuse {
                Some(mut ru) => match substitute(subs, ru.symbol) {
                    Some(s) => {
                        did_change = true;
                        ru.symbol = s;
                        Some(ru)
                    }
                    None => Some(ru),
                },
                None => None,
            };

            if did_change {
                let arguments = new_args.into_bump_slice();

                Some(Tag {
                    tag_layout: *tag_layout,
                    tag_id: *tag_id,
                    arguments,
                    reuse,
                })
            } else {
                None
            }
        }

        NullPointer => None,

        Reset { .. } | ResetRef { .. } => {
            unreachable!("reset(ref) has not been introduced yet")
        }

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

        ErasedMake { value, callee } => {
            match (
                value.and_then(|v| substitute(subs, v)),
                substitute(subs, *callee),
            ) {
                (None, None) => None,
                (Some(value), None) => Some(ErasedMake {
                    value: Some(value),
                    callee: *callee,
                }),
                (None, Some(callee)) => Some(ErasedMake {
                    value: *value,
                    callee,
                }),
                (Some(value), Some(callee)) => Some(ErasedMake {
                    value: Some(value),
                    callee,
                }),
            }
        }

        ErasedLoad { symbol, field } => substitute(subs, *symbol).map(|new_symbol| ErasedLoad {
            symbol: new_symbol,
            field: *field,
        }),

        FunctionPointer { .. } => None,

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

        // currently only used for tail recursion modulo cons (TRMC)
        GetElementPointer {
            structure,
            indices,
            union_layout,
        } => match substitute(subs, *structure) {
            Some(structure) => Some(GetElementPointer {
                structure,
                indices,
                union_layout: *union_layout,
            }),
            None => None,
        },

        Alloca {
            element_layout,
            initializer,
        } => match substitute(subs, (*initializer)?) {
            Some(initializer) => Some(Alloca {
                element_layout: *element_layout,
                initializer: Some(initializer),
            }),
            None => None,
        },
    }
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
    layout_cache: &mut LayoutCache<'a>,
    procs: &mut Procs<'a>,
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
        RecordAccess {
            record_var,
            field,
            loc_expr,
            ..
        } => {
            let sorted_fields_result = {
                let mut layout_env =
                    layout::Env::from_components(layout_cache, env.subs, env.arena);
                layout::sort_record_fields(&mut layout_env, *record_var)
            };

            let sorted_fields = match sorted_fields_result {
                Ok(fields) => fields,
                Err(_) => unreachable!("Can't access record with improper layout"),
            };

            let index = sorted_fields
                .into_iter()
                .enumerate()
                .find_map(|(current, (label, _, _))| (label == *field).then_some(current));

            let struct_index = match index {
                Some(index) => index as u64,
                None => return NotASymbol,
            };

            let struct_symbol = possible_reuse_symbol_or_specialize(
                env,
                procs,
                layout_cache,
                &loc_expr.value,
                *record_var,
            );

            match env
                .struct_indexing
                .get((struct_symbol, struct_index as u64))
            {
                Some(symbol) => *symbol,
                None => {
                    return NotASymbol;
                }
            }
        }
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
    match can_reuse_symbol(env, layout_cache, procs, expr, var) {
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

    // We should never reference a partial proc - instead, we want to generate closure data and
    // leave it there, even if the lambda set is unary. That way, we avoid having to try to resolve
    // lambda set of the proc based on the symbol name, which can cause many problems!
    // See my git blame for details.
    debug_assert!(!procs.partial_procs.contains_key(right));

    let result = build_rest(env, procs, layout_cache);

    if procs.is_imported_module_thunk(right) {
        // if this is an imported symbol, then we must make sure it is
        // specialized, and wrap the original in a function pointer.
        add_needed_external(procs, env, variable, LambdaName::no_niche(right));

        let res_layout = layout_cache.from_var(env.arena, variable, env.subs);
        let layout = return_on_layout_error!(env, res_layout, "handle_variable_aliasing");

        force_thunk(env, right, layout, left, env.arena.alloc(result))
    } else if env.is_imported_symbol(right) {
        // if this is an imported symbol, then we must make sure it is
        // specialized, and wrap the original in a function pointer.
        add_needed_external(procs, env, variable, LambdaName::no_niche(right));

        // then we must construct its closure; since imported symbols have no closure, we use the empty struct
        let_empty_struct(left, env.arena.alloc(result))
    } else {
        let mut result = result;
        substitute_in_exprs(env.arena, &mut result, left, right);
        result
    }
}

fn force_thunk<'a>(
    env: &mut Env<'a, '_>,
    thunk_name: Symbol,
    layout: InLayout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let call = self::Call {
        call_type: CallType::ByName {
            name: LambdaName::no_niche(thunk_name),
            ret_layout: layout,
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
    assign_to: Symbol,
    result: &'a Stmt<'a>,
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

                    match raw {
                        RawFunctionLayout::Function(_, lambda_set, _)
                            if !procs.is_imported_module_thunk(original) =>
                        {
                            let lambda_name =
                                find_lambda_name(env, layout_cache, lambda_set, original, &[]);

                            debug_assert!(
                                lambda_name.no_captures(),
                                "imported functions are top-level and should never capture"
                            );

                            let function_ptr_layout =
                                ProcLayout::from_raw_named(env.arena, lambda_name, raw);
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
                                assign_to,
                                env.arena.alloc(result),
                            )
                        }
                        _ => {
                            // This is an imported ZAT that returns either a value, or the closure
                            // data for a lambda set.
                            let layout = match raw {
                                RawFunctionLayout::ZeroArgumentThunk(layout) => layout,
                                RawFunctionLayout::ErasedFunction(..) => todo_lambda_erasure!(),
                                RawFunctionLayout::Function(_, lambda_set, _) => layout_cache
                                    .put_in_direct_no_semantic(LayoutRepr::LambdaSet(lambda_set)),
                            };

                            let raw = RawFunctionLayout::ZeroArgumentThunk(layout);
                            let lambda_name = LambdaName::no_niche(original);
                            let top_level = ProcLayout::from_raw_named(env.arena, lambda_name, raw);

                            procs.insert_passed_by_name(
                                env,
                                arg_var,
                                lambda_name,
                                top_level,
                                layout_cache,
                            );

                            force_thunk(env, original, layout, assign_to, env.arena.alloc(result))
                        }
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

                    // Replaces references of `assign_to` in the rest of the block with `original`,
                    // since we don't actually need to specialize the original symbol to a value.
                    //
                    // This usually means we are using a symbol received from a joinpoint.
                    let mut result = result.clone();
                    substitute_in_exprs(env.arena, &mut result, assign_to, original);
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
                        let function_ptr_layout =
                            ProcLayout::from_raw_named(env.arena, lambda_name, res_layout);

                        // this is a closure by capture, meaning it itself captures local variables.
                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            lambda_name,
                            function_ptr_layout,
                            layout_cache,
                        );

                        let closure_data = assign_to;

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
                        let layout = lambda_set.full_layout;
                        let top_level = ProcLayout::new(env.arena, &[], Niche::NONE, layout);
                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            LambdaName::no_niche(original),
                            top_level,
                            layout_cache,
                        );

                        force_thunk(env, original, layout, assign_to, env.arena.alloc(result))
                    } else {
                        // even though this function may not itself capture,
                        // unification may still cause it to have an extra argument
                        let lambda_name =
                            find_lambda_name(env, layout_cache, lambda_set, original, &[]);

                        debug_assert!(lambda_name.no_captures());

                        // define the function pointer
                        let function_ptr_layout =
                            ProcLayout::from_raw_named(env.arena, lambda_name, res_layout);

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
                            assign_to,
                            env.arena.alloc(result),
                        )
                    }
                }
                RawFunctionLayout::ErasedFunction(argument_layouts, ret_layout) => {
                    let erased_lambda = erased::ResolvedErasedLambda::new(
                        env,
                        layout_cache,
                        original,
                        captured,
                        argument_layouts,
                        ret_layout,
                    );
                    let lambda_name = erased_lambda.lambda_name();

                    let proc_layout =
                        ProcLayout::from_raw_named(env.arena, lambda_name, res_layout);

                    procs.insert_passed_by_name(
                        env,
                        arg_var,
                        lambda_name,
                        proc_layout,
                        layout_cache,
                    );

                    erased::build_erased_function(
                        env,
                        layout_cache,
                        erased_lambda,
                        assign_to,
                        result,
                    )
                }
                RawFunctionLayout::ZeroArgumentThunk(ret_layout) => {
                    // this is a 0-argument thunk
                    let top_level = ProcLayout::new(env.arena, &[], Niche::NONE, ret_layout);
                    procs.insert_passed_by_name(
                        env,
                        arg_var,
                        LambdaName::no_niche(original),
                        top_level,
                        layout_cache,
                    );

                    force_thunk(
                        env,
                        original,
                        ret_layout,
                        assign_to,
                        env.arena.alloc(result),
                    )
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
    match can_reuse_symbol(env, layout_cache, procs, &loc_arg.value, arg_var) {
        Imported(original) | LocalFunction(original) | UnspecializedExpr(original) => {
            // for functions we must make sure they are specialized correctly
            specialize_symbol(
                env,
                procs,
                layout_cache,
                Some(arg_var),
                symbol,
                env.arena.alloc(result),
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
    return_layout: InLayout<'a>,
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
                "Hit an unresolved type variable {var:?} when creating a layout for {proc_name:?} (var {fn_var:?})"
            );

            evaluate_arguments_then_runtime_error(env, procs, layout_cache, msg, loc_args)
        }
        Err(LayoutProblem::Erroneous) => {
            let msg = format!("Hit an erroneous type when creating a layout for {proc_name:?}");

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
                        lambda_set.full_layout,
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
                        lambda_set.full_layout,
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
        Ok(RawFunctionLayout::ErasedFunction(arg_layouts, ret_layout)) => {
            // TODO(erased-lambdas) call-by-name should never apply here
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

            let result = erased::call_erased_function(
                env,
                layout_cache,
                procs,
                roc_can::expr::Expr::Var(proc_name, fn_var),
                fn_var,
                (arg_layouts, ret_layout),
                arg_symbols,
                assigned,
                hole,
                // TODO is this right??
                ret_layout,
            );

            let iter = loc_args.into_iter().rev().zip(arg_symbols.iter().rev());
            assign_to_symbols(env, procs, layout_cache, iter, result)
        }
        Ok(RawFunctionLayout::ZeroArgumentThunk(ret_layout)) => {
            if procs.is_module_thunk(proc_name) {
                // here we turn a call to a module thunk into  forcing of that thunk
                call_by_name_module_thunk(
                    env,
                    procs,
                    fn_var,
                    proc_name,
                    ret_layout,
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
    argument_layouts: &'a [InLayout<'a>],
    ret_layout: InLayout<'a>,
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
                "Somehow, call by name for {proc_name:?} has multiple capture niches: {lambda_set:?}"
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
            lambda_set.extend_argument_list_for_named(env.arena, proc_name, argument_layouts);
        ProcLayout::new(env.arena, argument_layouts, proc_name.niche(), ret_layout)
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
            "see call_by_name for background (scroll down a bit), function is {proc_name:?}",
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
                lambda_set.full_layout,
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
                "see call_by_name for background (scroll down a bit), function is {proc_name:?}",
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

            let result = build_call(env, call, assigned, ret_layout, hole);

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
                    "see call_by_name for background (scroll down a bit), function is {proc_name:?}",
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
                            partial_proc,
                        ) {
                            Ok((proc, layout)) => {
                                let proc_name = proc.name;
                                let function_layout =
                                    ProcLayout::from_raw_named(env.arena, proc_name, layout);
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
                                    proc_name,
                                    attempted_layout,
                                );

                                let proc_name = proc.name;
                                let function_layout = ProcLayout::from_raw_named(
                                    env.arena,
                                    proc_name,
                                    attempted_layout,
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
    ret_layout: InLayout<'a>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let top_level_layout = ProcLayout::new(env.arena, &[], Niche::NONE, ret_layout);

    let inner_layout = ret_layout;

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
                            partial_proc,
                        ) {
                            Ok((proc, raw_layout)) => {
                                debug_assert!(
                                    raw_layout.is_zero_argument_thunk(),
                                    "but actually {raw_layout:?}"
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
                                    LambdaName::no_niche(proc_name),
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
                        ret_layout: function_layout.result,
                        arg_layouts: function_layout.arguments,
                        specialization_id: env.next_call_specialization_id(),
                    },
                    arguments: field_symbols,
                };

                // the closure argument is already added here (to get the right specialization)
                // but now we need to remove it because the `match_on_lambda_set` will add it again
                build_call(env, call, assigned, lambda_set.full_layout, hole)
            }
            RawFunctionLayout::ErasedFunction(..) => todo_lambda_erasure!(),
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
                    function_layout.result,
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
                        ret_layout: function_layout.result,
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

fn assign_num_literal_expr<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    variable: Variable,
    num_str: &str,
    num_value: IntOrFloatValue,
    hole: &'a Stmt<'a>,
) -> Result<Stmt<'a>, RuntimeError> {
    let layout = layout_cache.from_var(env.arena, variable, env.subs)?;
    let literal =
        make_num_literal(&layout_cache.interner, layout, num_str, num_value).to_expr_literal();

    Ok(Stmt::Let(assigned, Expr::Literal(literal), layout, hole))
}

type ToLowLevelCallArguments<'a> = (
    LambdaName<'a>,
    Symbol,
    Option<InLayout<'a>>,
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
    return_layout: InLayout<'a>,
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
                    "a function passed to `{op:?}` LowLevel call has an empty lambda set!
                     The most likely reason is that some symbol you use is not in scope.
                    "
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
                    Layout::BOOL,
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
                    Layout::U8,
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
    closure_tag_id_layout: InLayout<'a>,
    closure_data_symbol: Symbol,
    closure_env_layout: Option<InLayout<'a>>,
    to_lowlevel_call: ToLowLevelCall,
    return_layout: InLayout<'a>,
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
    argument_layouts: &'a [InLayout<'a>],
    return_layout: InLayout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    match lambda_set.call_by_name_options(&layout_cache.interner) {
        ClosureCallOptions::Void => empty_lambda_set_error(env),
        ClosureCallOptions::Union(union_layout) => {
            let closure_tag_id_symbol = env.unique_symbol();

            let result = union_lambda_set_to_switch(
                env,
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
        ClosureCallOptions::Struct(field_layouts) => {
            let function_symbol = match lambda_set.iter_set().next() {
                Some(function_symbol) => function_symbol,
                None => {
                    // Lambda set is empty, so this function is never called; synthesize a function
                    // that always yields a runtime error.
                    let name = env.unique_symbol();
                    let lambda_name = LambdaName::no_niche(name);
                    let function_layout =
                        RawFunctionLayout::Function(argument_layouts, lambda_set, return_layout);
                    let proc = generate_runtime_error_function(env, lambda_name, function_layout);
                    let top_level =
                        ProcLayout::from_raw_named(env.arena, lambda_name, function_layout);

                    procs.specialized.insert_specialized(name, top_level, proc);

                    lambda_name
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
                    Layout::BOOL,
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
                    Layout::U8,
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
    lambda_set: LambdaSet<'a>,
    closure_tag_id_symbol: Symbol,
    closure_tag_id_layout: InLayout<'a>,
    closure_data_symbol: Symbol,
    argument_symbols: &'a [Symbol],
    argument_layouts: &'a [InLayout<'a>],
    return_layout: InLayout<'a>,
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

    let (opt_join, branch_assigned, branch_hole) = match hole {
        Stmt::Ret(_) => {
            // No need to jump to a joinpoint, inline the return in each statement as-is.
            // This makes further analyses, like TCO, easier as well.
            (None, assigned, hole)
        }
        _ => {
            let join_point_id = JoinPointId(env.unique_symbol());
            let assigned = env.unique_symbol();
            let hole = Stmt::Jump(join_point_id, env.arena.alloc([assigned]));

            (Some(join_point_id), assigned, &*env.arena.alloc(hole))
        }
    };

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
            lambda_name,
            closure_info,
            argument_symbols,
            argument_layouts,
            return_layout,
            branch_assigned,
            branch_hole,
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

    match opt_join {
        None => switch,
        Some(join_point_id) => {
            let param = Param {
                symbol: assigned,
                layout: return_layout,
            };

            Stmt::Join {
                id: join_point_id,
                parameters: &*env.arena.alloc([param]),
                body: hole,
                remainder: env.arena.alloc(switch),
            }
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn union_lambda_set_branch<'a>(
    env: &mut Env<'a, '_>,
    lambda_name: LambdaName<'a>,
    closure_info: ClosureInfo<'a>,
    argument_symbols_slice: &'a [Symbol],
    argument_layouts_slice: &'a [InLayout<'a>],
    return_layout: InLayout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    union_lambda_set_branch_help(
        env,
        lambda_name,
        closure_info,
        argument_symbols_slice,
        argument_layouts_slice,
        return_layout,
        assigned,
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
    lambda_name: LambdaName<'a>,
    closure_info: ClosureInfo<'a>,
    argument_symbols_slice: &'a [Symbol],
    argument_layouts_slice: &'a [InLayout<'a>],
    return_layout: InLayout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let (argument_layouts, argument_symbols) = match closure_info {
        ClosureInfo::Captures {
            lambda_set,
            closure_data_symbol,
        } => {
            let argument_layouts = lambda_set.extend_argument_list_for_named(
                env.arena,
                lambda_name,
                argument_layouts_slice,
            );

            // Since this lambda captures, the arguments must have been extended.
            debug_assert!(argument_layouts.len() > argument_layouts_slice.len());
            // Extend symbols with the symbol of the closure environment.
            let mut argument_symbols =
                Vec::with_capacity_in(argument_symbols_slice.len() + 1, env.arena);
            argument_symbols.extend(argument_symbols_slice);
            argument_symbols.push(closure_data_symbol);

            (argument_layouts, argument_symbols.into_bump_slice())
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

    build_call(env, call, assigned, return_layout, hole)
}

/// Switches over a enum lambda set, which may dispatch to different functions, none of which
/// capture.
#[allow(clippy::too_many_arguments)]
fn enum_lambda_set_to_switch<'a>(
    env: &mut Env<'a, '_>,
    lambda_set: impl ExactSizeIterator<Item = LambdaName<'a>>,
    closure_tag_id_symbol: Symbol,
    closure_tag_id_layout: InLayout<'a>,
    argument_symbols: &'a [Symbol],
    argument_layouts: &'a [InLayout<'a>],
    return_layout: InLayout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    debug_assert_ne!(lambda_set.len(), 0);

    let (opt_join, branch_assigned, branch_hole) = match hole {
        Stmt::Ret(_) => {
            // No need to jump to a joinpoint, inline the return in each statement as-is.
            // This makes further analyses, like TCO, easier as well.
            (None, assigned, hole)
        }
        _ => {
            let join_point_id = JoinPointId(env.unique_symbol());
            let assigned = env.unique_symbol();
            let hole = Stmt::Jump(join_point_id, env.arena.alloc([assigned]));

            (Some(join_point_id), assigned, &*env.arena.alloc(hole))
        }
    };

    let mut branches = Vec::with_capacity_in(lambda_set.len(), env.arena);

    for (i, lambda_name) in lambda_set.into_iter().enumerate() {
        let stmt = enum_lambda_set_branch(
            env,
            lambda_name,
            argument_symbols,
            argument_layouts,
            return_layout,
            branch_assigned,
            branch_hole,
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

    match opt_join {
        None => switch,
        Some(join_point_id) => {
            let param = Param {
                symbol: assigned,
                layout: return_layout,
            };

            Stmt::Join {
                id: join_point_id,
                parameters: &*env.arena.alloc([param]),
                body: hole,
                remainder: env.arena.alloc(switch),
            }
        }
    }
}

/// A branch for an enum lambda set branch dispatch, which never capture!
#[allow(clippy::too_many_arguments)]
fn enum_lambda_set_branch<'a>(
    env: &mut Env<'a, '_>,
    lambda_name: LambdaName<'a>,
    argument_symbols: &'a [Symbol],
    argument_layouts: &'a [InLayout<'a>],
    return_layout: InLayout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let call = self::Call {
        call_type: CallType::ByName {
            name: lambda_name,
            ret_layout: return_layout,
            arg_layouts: argument_layouts,
            specialization_id: env.next_call_specialization_id(),
        },
        arguments: argument_symbols,
    };
    build_call(env, call, assigned, return_layout, hole)
}

#[allow(clippy::too_many_arguments)]
fn lowlevel_enum_lambda_set_to_switch<'a, ToLowLevelCall>(
    env: &mut Env<'a, '_>,
    lambda_set: impl ExactSizeIterator<Item = LambdaName<'a>>,
    closure_tag_id_symbol: Symbol,
    closure_tag_id_layout: InLayout<'a>,
    closure_data_symbol: Symbol,
    closure_env_layout: Option<InLayout<'a>>,
    to_lowlevel_call: ToLowLevelCall,
    return_layout: InLayout<'a>,
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
    };

    Stmt::Join {
        id: join_point_id,
        parameters: &*env.arena.alloc([param]),
        body: hole,
        remainder: env.arena.alloc(switch),
    }
}

#[derive(Debug, Default)]
pub struct GlueLayouts<'a> {
    pub getters: std::vec::Vec<(Symbol, ProcLayout<'a>)>,
}

type GlueProcId = u16;

#[derive(Debug)]
pub struct GlueProc<'a> {
    pub name: Symbol,
    pub proc_layout: ProcLayout<'a>,
    pub proc: Proc<'a>,
}

pub struct GlueProcs<'a> {
    pub getters: Vec<'a, (Layout<'a>, Vec<'a, GlueProc<'a>>)>,
    /// Lambda set IDs computed from the layout of the lambda set. Should be replaced by
    /// computation from type variable eventually.
    pub legacy_layout_based_extern_names: Vec<'a, (LambdaSetId, RawFunctionLayout<'a>)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct LambdaSetId(pub u32);

impl LambdaSetId {
    #[must_use]
    pub fn next(self) -> Self {
        debug_assert!(self.0 < u32::MAX);
        Self(self.0 + 1)
    }
}

pub fn find_lambda_sets(
    arena: &Bump,
    subs: &Subs,
    initial: Variable,
) -> MutMap<Variable, LambdaSetId> {
    let mut stack = bumpalo::collections::Vec::new_in(arena);

    // ignore the lambda set of top-level functions
    match subs.get_without_compacting(initial).content {
        Content::Structure(FlatType::Func(arguments, _, result, _fx)) => {
            let arguments = &subs.variables[arguments.indices()];

            stack.extend(arguments.iter().copied());
            stack.push(result);
        }
        _ => {
            stack.push(initial);
        }
    }

    find_lambda_sets_help(subs, stack)
}

fn find_lambda_sets_help(
    subs: &Subs,
    mut stack: Vec<'_, Variable>,
) -> MutMap<Variable, LambdaSetId> {
    use roc_types::subs::GetSubsSlice;

    let mut lambda_set_id = LambdaSetId::default();

    let mut result = MutMap::default();

    while let Some(var) = stack.pop() {
        match subs.get_content_without_compacting(var) {
            Content::RangedNumber(_)
            | Content::Error
            | Content::FlexVar(_)
            | Content::RigidVar(_)
            | Content::FlexAbleVar(_, _)
            | Content::RigidAbleVar(_, _)
            | Content::RecursionVar { .. } => {}
            Content::Structure(flat_type) => match flat_type {
                FlatType::Apply(_, arguments) => {
                    stack.extend(subs.get_subs_slice(*arguments).iter().rev());
                }
                FlatType::Func(arguments, lambda_set_var, ret_var, _fx_var) => {
                    use std::collections::hash_map::Entry;
                    // Only insert a lambda_set_var if we didn't already have a value for this key.
                    if let Entry::Vacant(entry) = result.entry(*lambda_set_var) {
                        entry.insert(lambda_set_id);
                        lambda_set_id = lambda_set_id.next();
                    }

                    let arguments = &subs.variables[arguments.indices()];

                    stack.extend(arguments.iter().copied());
                    stack.push(*lambda_set_var);
                    stack.push(*ret_var);
                }
                FlatType::Record(fields, ext) => {
                    stack.extend(subs.get_subs_slice(fields.variables()).iter().rev());
                    stack.push(*ext);
                }
                FlatType::Tuple(elements, ext) => {
                    stack.extend(subs.get_subs_slice(elements.variables()).iter().rev());
                    stack.push(*ext);
                }
                FlatType::FunctionOrTagUnion(_, _, ext) => {
                    // just the ext
                    match ext {
                        roc_types::subs::TagExt::Openness(var) => stack.push(*var),
                        roc_types::subs::TagExt::Any(_) => { /* ignore */ }
                    }
                }
                FlatType::TagUnion(union_tags, ext)
                | FlatType::RecursiveTagUnion(_, union_tags, ext) => {
                    for tag in union_tags.variables() {
                        stack.extend(
                            subs.get_subs_slice(subs.variable_slices[tag.index()])
                                .iter()
                                .rev(),
                        );
                    }

                    match ext {
                        roc_types::subs::TagExt::Openness(var) => stack.push(*var),
                        roc_types::subs::TagExt::Any(_) => { /* ignore */ }
                    }
                }
                FlatType::EmptyRecord => {}
                FlatType::EmptyTagUnion => {}
                FlatType::EffectfulFunc => {}
            },
            Content::Alias(_, _, actual, _) => {
                stack.push(*actual);
            }
            Content::LambdaSet(lambda_set) => {
                // the lambda set itself should already be caught by Func above, but the
                // capture can itself contain more lambda sets
                for index in lambda_set.solved.variables() {
                    let subs_slice = subs.variable_slices[index.index()];
                    stack.extend(subs.variables[subs_slice.indices()].iter());
                }
            }
            Content::ErasedLambda => {}
            Content::Pure | Content::Effectful => {}
        }
    }

    result
}

pub fn generate_glue_procs<'a, 'i, I>(
    home: ModuleId,
    ident_ids: &mut IdentIds,
    arena: &'a Bump,
    layout_interner: &'i mut I,
    layout: &'a Layout<'a>,
) -> GlueProcs<'a>
where
    I: LayoutInterner<'a>,
{
    let mut answer = GlueProcs {
        getters: Vec::new_in(arena),
        legacy_layout_based_extern_names: Vec::new_in(arena),
    };

    let mut lambda_set_id = LambdaSetId(0);

    let mut stack: Vec<'a, Layout<'a>> = Vec::from_iter_in([*layout], arena);
    let mut next_unique_id = 0;

    macro_rules! handle_tag_field_layouts {
        ($tag_id:expr, $layout:expr, $union_layout:expr, $field_layouts: expr) => {{
            if $field_layouts.iter().any(|l| {
                layout_interner
                    .get_repr(*l)
                    .has_varying_stack_size(layout_interner, arena)
            }) {
                let procs = generate_glue_procs_for_tag_fields(
                    layout_interner,
                    home,
                    &mut next_unique_id,
                    ident_ids,
                    arena,
                    $tag_id,
                    &$layout,
                    $union_layout,
                    $field_layouts,
                );

                answer.getters.push(($layout, procs));
            }

            for in_layout in $field_layouts.iter().rev() {
                stack.push(layout_interner.get(*in_layout));
            }
        }};
    }

    while let Some(layout) = stack.pop() {
        match layout.repr(layout_interner) {
            LayoutRepr::Builtin(builtin) => match builtin {
                Builtin::Int(_)
                | Builtin::Float(_)
                | Builtin::Bool
                | Builtin::Decimal
                | Builtin::Str => { /* do nothing */ }
                Builtin::List(element) => stack.push(layout_interner.get(element)),
            },
            LayoutRepr::Struct(field_layouts) => {
                if field_layouts.iter().any(|l| {
                    layout_interner
                        .get_repr(*l)
                        .has_varying_stack_size(layout_interner, arena)
                }) {
                    let procs = generate_glue_procs_for_struct_fields(
                        layout_interner,
                        home,
                        &mut next_unique_id,
                        ident_ids,
                        arena,
                        &layout,
                        field_layouts,
                    );

                    answer.getters.push((layout, procs));
                }

                for in_layout in field_layouts.iter().rev() {
                    stack.push(layout_interner.get(*in_layout));
                }
            }
            LayoutRepr::Ptr(inner) => {
                stack.push(layout_interner.get(inner));
            }
            LayoutRepr::Union(union_layout) => match union_layout {
                UnionLayout::NonRecursive(tags) => {
                    for in_layout in tags.iter().flat_map(|e| e.iter()) {
                        stack.push(layout_interner.get(*in_layout));
                    }
                }
                UnionLayout::Recursive(tags) => {
                    for in_layout in tags.iter().flat_map(|e| e.iter()) {
                        stack.push(layout_interner.get(*in_layout));
                    }
                }
                UnionLayout::NonNullableUnwrapped(field_layouts) => {
                    handle_tag_field_layouts!(0, layout, union_layout, field_layouts);
                }
                UnionLayout::NullableWrapped {
                    other_tags,
                    nullable_id,
                } => {
                    let tag_ids =
                        (0..nullable_id).chain(nullable_id + 1..other_tags.len() as u16 + 1);
                    for (i, field_layouts) in tag_ids.zip(other_tags) {
                        handle_tag_field_layouts!(i, layout, union_layout, *field_layouts);
                    }
                }
                UnionLayout::NullableUnwrapped { other_fields, .. } => {
                    for in_layout in other_fields.iter().rev() {
                        stack.push(layout_interner.get(*in_layout));
                    }
                }
            },
            LayoutRepr::LambdaSet(lambda_set) => {
                let raw_function_layout =
                    RawFunctionLayout::Function(lambda_set.args, lambda_set, lambda_set.ret);

                let key = (lambda_set_id, raw_function_layout);
                answer.legacy_layout_based_extern_names.push(key);

                // this id is used, increment for the next one
                lambda_set_id = lambda_set_id.next();

                stack.push(layout_interner.get(lambda_set.runtime_representation()));

                // TODO: figure out if we need to look at the other layouts
                // stack.push(layout_interner.get(lambda_set.ret));
            }
            LayoutRepr::RecursivePointer(_) => {
                /* do nothing, we've already generated for this type through the Union(_) */
            }
            LayoutRepr::FunctionPointer(_) => todo_lambda_erasure!(),
            LayoutRepr::Erased(_) => todo_lambda_erasure!(),
        }
    }

    answer
}

fn generate_glue_procs_for_struct_fields<'a, 'i, I>(
    layout_interner: &'i mut I,
    home: ModuleId,
    next_unique_id: &mut GlueProcId,
    ident_ids: &mut IdentIds,
    arena: &'a Bump,
    unboxed_struct_layout: &Layout<'a>,
    field_layouts: &[InLayout<'a>],
) -> Vec<'a, GlueProc<'a>>
where
    I: LayoutInterner<'a>,
{
    let interned_unboxed_struct_layout = layout_interner.insert(*unboxed_struct_layout);
    let union_layout =
        UnionLayout::NonNullableUnwrapped(arena.alloc([interned_unboxed_struct_layout]));
    let boxed_struct_layout = Layout::no_semantic(LayoutRepr::Union(union_layout).direct());
    let boxed_struct_layout = layout_interner.insert(boxed_struct_layout);
    let mut answer = bumpalo::collections::Vec::with_capacity_in(field_layouts.len(), arena);

    let field_layouts = match layout_interner.get_repr(interned_unboxed_struct_layout) {
        LayoutRepr::Struct(field_layouts) => field_layouts,
        other => {
            unreachable!(
                "{:?} {:?}",
                layout_interner.dbg(interned_unboxed_struct_layout),
                other
            )
        }
    };

    for (index, field) in field_layouts.iter().enumerate() {
        let proc_layout = ProcLayout {
            arguments: arena.alloc([boxed_struct_layout]),
            result: *field,
            niche: Niche::NONE,
        };

        let symbol = unique_glue_symbol(arena, next_unique_id, home, ident_ids);

        let argument = Symbol::new(home, ident_ids.gen_unique());
        let unboxed = Symbol::new(home, ident_ids.gen_unique());
        let result = Symbol::new(home, ident_ids.gen_unique());

        home.register_debug_idents(ident_ids);

        let ret_stmt = arena.alloc(Stmt::Ret(result));

        let field_get_expr = Expr::StructAtIndex {
            index: index as u64,
            field_layouts,
            structure: unboxed,
        };

        let field_get_stmt = Stmt::Let(result, field_get_expr, *field, ret_stmt);

        let unbox_expr = boxed::unbox(argument, arena.alloc(interned_unboxed_struct_layout));

        let unbox_stmt = Stmt::Let(
            unboxed,
            unbox_expr,
            interned_unboxed_struct_layout,
            arena.alloc(field_get_stmt),
        );

        let proc = Proc {
            name: LambdaName::no_niche(symbol),
            args: arena.alloc([(boxed_struct_layout, argument)]),
            body: unbox_stmt,
            closure_data_layout: None,
            ret_layout: *field,
            is_self_recursive: SelfRecursive::NotSelfRecursive,
            is_erased: false,
        };

        answer.push(GlueProc {
            name: symbol,
            proc_layout,
            proc,
        });
    }

    answer
}

fn unique_glue_symbol(
    arena: &Bump,
    next_unique_id: &mut GlueProcId,
    home: ModuleId,
    ident_ids: &mut IdentIds,
) -> Symbol {
    let unique_id = *next_unique_id;

    *next_unique_id = unique_id + 1;

    // then name of the platform `main.roc` is the empty string
    let module_name = "";

    // Turn unique_id into a Symbol without doing a heap allocation.
    use std::fmt::Write;
    let mut string = bumpalo::collections::String::with_capacity_in(32, arena);

    let _result = write!(&mut string, "roc__getter_{module_name}_{unique_id}");
    debug_assert_eq!(_result, Ok(())); // This should never fail, but doesn't hurt to debug-check!

    let bump_string = string.into_bump_str();
    let ident_id = ident_ids.get_or_insert(bump_string);

    Symbol::new(home, ident_id)
}

#[allow(clippy::too_many_arguments)]
fn generate_glue_procs_for_tag_fields<'a, 'i, I>(
    layout_interner: &'i mut I,
    home: ModuleId,
    next_unique_id: &mut GlueProcId,
    ident_ids: &mut IdentIds,
    arena: &'a Bump,
    tag_id: TagIdIntType,
    unboxed_struct_layout: &Layout<'a>,
    union_layout: UnionLayout<'a>,
    field_layouts: &'a [InLayout<'a>],
) -> Vec<'a, GlueProc<'a>>
where
    I: LayoutInterner<'a>,
{
    let interned = layout_interner.insert(*unboxed_struct_layout);
    let box_union_layout = UnionLayout::NonNullableUnwrapped(arena.alloc([interned]));
    let boxed_struct_layout = Layout::no_semantic(LayoutRepr::Union(box_union_layout).direct());
    let boxed_struct_layout = layout_interner.insert(boxed_struct_layout);
    let mut answer = bumpalo::collections::Vec::with_capacity_in(field_layouts.len(), arena);

    for (index, field) in field_layouts.iter().enumerate() {
        let proc_layout = ProcLayout {
            arguments: arena.alloc([boxed_struct_layout]),
            result: *field,
            niche: Niche::NONE,
        };
        let symbol = unique_glue_symbol(arena, next_unique_id, home, ident_ids);

        let argument = Symbol::new(home, ident_ids.gen_unique());
        let unboxed = Symbol::new(home, ident_ids.gen_unique());
        let result = Symbol::new(home, ident_ids.gen_unique());

        home.register_debug_idents(ident_ids);

        let ret_stmt = arena.alloc(Stmt::Ret(result));

        let field_get_expr = Expr::UnionAtIndex {
            structure: unboxed,
            tag_id,
            union_layout,
            index: index as u64,
        };

        let field_get_stmt = Stmt::Let(result, field_get_expr, *field, ret_stmt);

        let unbox_expr = boxed::unbox(argument, arena.alloc(interned));
        let unbox_stmt = Stmt::Let(unboxed, unbox_expr, interned, arena.alloc(field_get_stmt));

        let proc = Proc {
            name: LambdaName::no_niche(symbol),
            args: arena.alloc([(boxed_struct_layout, argument)]),
            body: unbox_stmt,
            closure_data_layout: None,
            ret_layout: *field,
            is_self_recursive: SelfRecursive::NotSelfRecursive,
            is_erased: false,
        };

        answer.push(GlueProc {
            name: symbol,
            proc_layout,
            proc,
        });
    }

    answer
}

enum Usage {
    Used,
    Unused,
}

pub struct UsageTrackingMap<K, V> {
    map: MutMap<K, (V, Usage)>,
}

impl<K, V> Default for UsageTrackingMap<K, V> {
    fn default() -> Self {
        Self {
            map: MutMap::default(),
        }
    }
}

impl<K, V> UsageTrackingMap<K, V>
where
    K: std::cmp::Eq + std::hash::Hash,
{
    pub fn insert(&mut self, key: K, value: V) {
        self.map.insert(key, (value, Usage::Unused));
    }

    pub fn get(&mut self, key: K) -> Option<&V> {
        let (value, usage) = self.map.get_mut(&key)?;
        *usage = Usage::Used;
        Some(value)
    }

    fn get_used(&mut self, key: &K) -> Option<V> {
        self.map.remove(key).and_then(|(value, usage)| match usage {
            Usage::Used => Some(value),
            Usage::Unused => None,
        })
    }
}
