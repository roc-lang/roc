#![allow(clippy::manual_map)]

use self::InProgressProc::*;
use crate::exhaustive::{Ctor, Guard, RenderAs, TagId};
use crate::layout::{
    Builtin, ClosureRepresentation, LambdaSet, Layout, LayoutCache, LayoutProblem,
    RawFunctionLayout, TagIdIntType, UnionLayout, WrappedVariant,
};
use bumpalo::collections::Vec;
use bumpalo::Bump;
use hashbrown::hash_map::Entry;
use roc_can::expr::ClosureData;
use roc_collections::all::{default_hasher, BumpMap, BumpMapDefault, MutMap};
use roc_module::ident::{ForeignSymbol, Lowercase, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_problem::can::RuntimeError;
use roc_region::all::{Located, Region};
use roc_std::RocDec;
use roc_types::solved_types::SolvedType;
use roc_types::subs::{Content, FlatType, Subs, Variable, VariableSubsSlice};
use std::collections::HashMap;
use ven_pretty::{BoxAllocator, DocAllocator, DocBuilder};

pub const PRETTY_PRINT_IR_SYMBOLS: bool = false;

// if your changes cause this number to go down, great!
// please change it to the lower number.
// if it went up, maybe check that the change is really required

// i128 alignment is different on arm
#[cfg(target_arch = "aarch64")]
static_assertions::assert_eq_size!([u8; 4 * 8], Literal);
#[cfg(not(target_arch = "aarch64"))]
static_assertions::assert_eq_size!([u8; 3 * 8], Literal);
static_assertions::assert_eq_size!([u8; 10 * 8], Expr);
#[cfg(not(target_arch = "aarch64"))]
static_assertions::assert_eq_size!([u8; 19 * 8], Stmt);
#[cfg(target_arch = "aarch64")]
static_assertions::assert_eq_size!([u8; 20 * 8], Stmt);
static_assertions::assert_eq_size!([u8; 6 * 8], ProcLayout);
static_assertions::assert_eq_size!([u8; 8 * 8], Call);
static_assertions::assert_eq_size!([u8; 6 * 8], CallType);

macro_rules! return_on_layout_error {
    ($env:expr, $layout_result:expr) => {
        match $layout_result {
            Ok(cached) => cached,
            Err(error) => return_on_layout_error_help!($env, error),
        }
    };
}

macro_rules! return_on_layout_error_help {
    ($env:expr, $error:expr) => {
        match $error {
            LayoutProblem::UnresolvedTypeVar(_) => {
                return Stmt::RuntimeError($env.arena.alloc(format!(
                    "UnresolvedTypeVar {} line {}",
                    file!(),
                    line!()
                )));
            }
            LayoutProblem::Erroneous => {
                return Stmt::RuntimeError($env.arena.alloc(format!(
                    "Erroneous {} line {}",
                    file!(),
                    line!()
                )));
            }
        }
    };
}

#[derive(Debug, Clone, Copy)]
pub enum OptLevel {
    Development,
    Normal,
    Optimize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum MonoProblem {
    PatternProblem(crate::exhaustive::Error),
}

#[derive(Debug, Clone, Copy)]
pub struct EntryPoint<'a> {
    pub symbol: Symbol,
    pub layout: ProcLayout<'a>,
}

#[derive(Clone, Copy, Debug)]
pub struct PartialProcId(usize);

#[derive(Clone, Debug, PartialEq)]
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
            "The {:?} is inserted as a partial proc twice: that's a bug!",
            symbol,
        );

        let id = PartialProcId(self.symbols.len());

        self.symbols.push(symbol);
        self.partial_procs.push(partial_proc);

        id
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PartialProc<'a> {
    pub annotation: Variable,
    pub pattern_symbols: &'a [Symbol],
    pub captured_symbols: CapturedSymbols<'a>,
    pub body: roc_can::expr::Expr,
    pub is_self_recursive: bool,
}

impl<'a> PartialProc<'a> {
    #[allow(clippy::too_many_arguments)]
    pub fn from_named_function(
        env: &mut Env<'a, '_>,
        layout_cache: &mut LayoutCache<'a>,
        annotation: Variable,
        loc_args: std::vec::Vec<(Variable, Located<roc_can::pattern::Pattern>)>,
        loc_body: Located<roc_can::expr::Expr>,
        captured_symbols: CapturedSymbols<'a>,
        is_self_recursive: bool,
        ret_var: Variable,
    ) -> PartialProc<'a> {
        let number_of_arguments = loc_args.len();

        match patterns_to_when(env, layout_cache, loc_args, ret_var, loc_body) {
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
                    is_self_recursive: false,
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
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

#[derive(Clone, Debug)]
pub struct PendingSpecialization<'a> {
    solved_type: SolvedType,
    host_exposed_aliases: BumpMap<Symbol, SolvedType>,
    _lifetime: std::marker::PhantomData<&'a u8>,
}

impl<'a> PendingSpecialization<'a> {
    pub fn from_var(arena: &'a Bump, subs: &Subs, var: Variable) -> Self {
        let solved_type = SolvedType::from_var(subs, var);
        PendingSpecialization {
            solved_type,
            host_exposed_aliases: BumpMap::new_in(arena),
            _lifetime: std::marker::PhantomData,
        }
    }

    pub fn from_var_host_exposed(
        arena: &'a Bump,
        subs: &Subs,
        var: Variable,
        exposed: &MutMap<Symbol, Variable>,
    ) -> Self {
        let solved_type = SolvedType::from_var(subs, var);

        let mut host_exposed_aliases = BumpMap::with_capacity_in(exposed.len(), arena);

        host_exposed_aliases.extend(
            exposed
                .iter()
                .map(|(symbol, variable)| (*symbol, SolvedType::from_var(subs, *variable))),
        );

        PendingSpecialization {
            solved_type,
            host_exposed_aliases,
            _lifetime: std::marker::PhantomData,
        }
    }

    /// Add a named function that will be publicly exposed to the host
    pub fn from_exposed_function(
        arena: &'a Bump,
        subs: &Subs,
        opt_annotation: Option<roc_can::def::Annotation>,
        fn_var: Variable,
    ) -> Self {
        match opt_annotation {
            None => PendingSpecialization::from_var(arena, subs, fn_var),
            Some(annotation) => PendingSpecialization::from_var_host_exposed(
                arena,
                subs,
                fn_var,
                &annotation.introduced_variables.host_exposed_aliases,
            ),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Proc<'a> {
    pub name: Symbol,
    pub args: &'a [(Layout<'a>, Symbol)],
    pub body: Stmt<'a>,
    pub closure_data_layout: Option<Layout<'a>>,
    pub ret_layout: Layout<'a>,
    pub is_self_recursive: SelfRecursive,
    pub must_own_arguments: bool,
    pub host_exposed_layouts: HostExposedLayouts<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HostExposedLayouts<'a> {
    NotHostExposed,
    HostExposed {
        rigids: BumpMap<Lowercase, Layout<'a>>,
        aliases: BumpMap<Symbol, (Symbol, ProcLayout<'a>, RawFunctionLayout<'a>)>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum SelfRecursive {
    NotSelfRecursive,
    SelfRecursive(JoinPointId),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Parens {
    NotNeeded,
    InTypeParam,
    InFunction,
}

impl<'a> Proc<'a> {
    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D, _parens: Parens) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        let args_doc = self
            .args
            .iter()
            .map(|(_, symbol)| symbol_to_doc(alloc, *symbol));

        if PRETTY_PRINT_IR_SYMBOLS {
            alloc
                .text("procedure : ")
                .append(symbol_to_doc(alloc, self.name))
                .append(" ")
                .append(self.ret_layout.to_doc(alloc, Parens::NotNeeded))
                .append(alloc.hardline())
                .append(alloc.text("procedure = "))
                .append(symbol_to_doc(alloc, self.name))
                .append(" (")
                .append(alloc.intersperse(args_doc, ", "))
                .append("):")
                .append(alloc.hardline())
                .append(self.body.to_doc(alloc).indent(4))
        } else {
            alloc
                .text("procedure ")
                .append(symbol_to_doc(alloc, self.name))
                .append(" (")
                .append(alloc.intersperse(args_doc, ", "))
                .append("):")
                .append(alloc.hardline())
                .append(self.body.to_doc(alloc).indent(4))
        }
    }

    pub fn to_pretty(&self, width: usize) -> String {
        let allocator = BoxAllocator;
        let mut w = std::vec::Vec::new();
        self.to_doc::<_, ()>(&allocator, Parens::NotNeeded)
            .1
            .render(width, &mut w)
            .unwrap();
        w.push(b'\n');
        String::from_utf8(w).unwrap()
    }

    pub fn insert_refcount_operations(
        arena: &'a Bump,
        procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    ) {
        let borrow_params = arena.alloc(crate::borrow::infer_borrow(arena, procs));

        crate::inc_dec::visit_procs(arena, borrow_params, procs);
    }

    pub fn insert_reset_reuse_operations<'i>(
        arena: &'a Bump,
        home: ModuleId,
        ident_ids: &'i mut IdentIds,
        procs: &mut MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
    ) {
        for (_, proc) in procs.iter_mut() {
            let new_proc =
                crate::reset_reuse::insert_reset_reuse(arena, home, ident_ids, proc.clone());
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
            );

            if let Some(with_tco) = transformed {
                self.body = with_tco;
                self.args = proc_args.into_bump_slice();
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExternalSpecializations<'a> {
    /// Not a bumpalo vec because bumpalo is not thread safe
    pub specs: BumpMap<Symbol, std::vec::Vec<SolvedType>>,
    _lifetime: std::marker::PhantomData<&'a u8>,
}

impl<'a> ExternalSpecializations<'a> {
    pub fn new_in(arena: &'a Bump) -> Self {
        Self {
            specs: BumpMap::new_in(arena),
            _lifetime: std::marker::PhantomData,
        }
    }

    pub fn insert(&mut self, symbol: Symbol, typ: SolvedType) {
        use hashbrown::hash_map::Entry::{Occupied, Vacant};

        let existing = match self.specs.entry(symbol) {
            Vacant(entry) => entry.insert(std::vec::Vec::new()),
            Occupied(entry) => entry.into_mut(),
        };

        existing.push(typ);
    }

    pub fn extend(&mut self, other: Self) {
        use hashbrown::hash_map::Entry::{Occupied, Vacant};

        for (symbol, solved_types) in other.specs {
            let existing = match self.specs.entry(symbol) {
                Vacant(entry) => entry.insert(std::vec::Vec::new()),
                Occupied(entry) => entry.into_mut(),
            };

            existing.extend(solved_types);
        }
    }
}

#[derive(Clone, Debug)]
pub struct Procs<'a> {
    pub partial_procs: PartialProcs<'a>,
    pub imported_module_thunks: &'a [Symbol],
    pub module_thunks: &'a [Symbol],
    pub pending_specializations:
        Option<BumpMap<Symbol, MutMap<ProcLayout<'a>, PendingSpecialization<'a>>>>,
    pub specialized: BumpMap<(Symbol, ProcLayout<'a>), InProgressProc<'a>>,
    pub runtime_errors: BumpMap<Symbol, &'a str>,
    pub call_by_pointer_wrappers: BumpMap<Symbol, Symbol>,
    pub externals_we_need: BumpMap<ModuleId, ExternalSpecializations<'a>>,
}

impl<'a> Procs<'a> {
    pub fn new_in(arena: &'a Bump) -> Self {
        Self {
            partial_procs: PartialProcs::new_in(arena),
            imported_module_thunks: &[],
            module_thunks: &[],
            pending_specializations: Some(BumpMap::new_in(arena)),
            specialized: BumpMap::new_in(arena),
            runtime_errors: BumpMap::new_in(arena),
            call_by_pointer_wrappers: BumpMap::new_in(arena),
            externals_we_need: BumpMap::new_in(arena),
        }
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
    ) -> MutMap<(Symbol, ProcLayout<'a>), Proc<'a>> {
        let mut result = MutMap::with_capacity_and_hasher(self.specialized.len(), default_hasher());

        for (key, in_prog_proc) in self.specialized.into_iter() {
            match in_prog_proc {
                InProgress => {
                    let (symbol, layout) = key;
                    eprintln!(
                        "The procedure {:?} should have be done by now:\n\n    {:?}",
                        symbol, layout
                    );

                    panic!();
                }
                Done(mut proc) => {
                    proc.make_tail_recursive(env);

                    result.insert(key, proc);
                }
            }
        }

        result
    }

    // TODO trim these down
    #[allow(clippy::too_many_arguments)]
    fn insert_anonymous(
        &mut self,
        env: &mut Env<'a, '_>,
        symbol: Symbol,
        annotation: Variable,
        loc_args: std::vec::Vec<(Variable, Located<roc_can::pattern::Pattern>)>,
        loc_body: Located<roc_can::expr::Expr>,
        captured_symbols: CapturedSymbols<'a>,
        ret_var: Variable,
        layout_cache: &mut LayoutCache<'a>,
    ) -> Result<ProcLayout<'a>, RuntimeError> {
        // anonymous functions cannot reference themselves, therefore cannot be tail-recursive
        let is_self_recursive = false;

        let raw_layout = layout_cache
            .raw_from_var(env.arena, annotation, env.subs)
            .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

        let top_level = ProcLayout::from_raw(env.arena, raw_layout);

        match patterns_to_when(env, layout_cache, loc_args, ret_var, loc_body) {
            Ok((_, pattern_symbols, body)) => {
                // an anonymous closure. These will always be specialized already
                // by the surrounding context, so we can add pending specializations
                // for them immediately.

                let already_specialized = self
                    .specialized
                    .keys()
                    .any(|(s, t)| *s == symbol && *t == top_level);

                let layout = top_level;

                // if we've already specialized this one, no further work is needed.
                if !already_specialized {
                    if self.is_module_thunk(symbol) {
                        debug_assert!(layout.arguments.is_empty());
                    }

                    match &mut self.pending_specializations {
                        Some(pending_specializations) => {
                            // register the pending specialization, so this gets code genned later
                            let pending =
                                PendingSpecialization::from_var(env.arena, env.subs, annotation);
                            add_pending(pending_specializations, symbol, layout, pending);

                            match self.partial_procs.symbol_to_id(symbol) {
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
                                        is_self_recursive,
                                    };

                                    self.partial_procs.insert(symbol, partial_proc);
                                }
                            }
                        }
                        None => {
                            // Mark this proc as in-progress, so if we're dealing with
                            // mutually recursive functions, we don't loop forever.
                            // (We had a bug around this before this system existed!)
                            self.specialized.insert((symbol, layout), InProgress);

                            let outside_layout = layout;

                            let partial_proc_id = if let Some(partial_proc_id) =
                                self.partial_procs.symbol_to_id(symbol)
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
                                    is_self_recursive,
                                };

                                self.partial_procs.insert(symbol, partial_proc)
                            };

                            match specialize_variable(
                                env,
                                self,
                                symbol,
                                layout_cache,
                                annotation,
                                BumpMap::new_in(env.arena),
                                partial_proc_id,
                            ) {
                                Ok((proc, layout)) => {
                                    let top_level = ProcLayout::from_raw(env.arena, layout);

                                    debug_assert_eq!(
                                        outside_layout, top_level,
                                        "different raw layouts for {:?}",
                                        proc.name
                                    );

                                    if self.is_module_thunk(proc.name) {
                                        debug_assert!(top_level.arguments.is_empty());
                                    }

                                    self.specialized.insert((symbol, top_level), Done(proc));
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
        name: Symbol,
        layout: ProcLayout<'a>,
        layout_cache: &mut LayoutCache<'a>,
    ) {
        // If we've already specialized this one, no further work is needed.
        if self.specialized.contains_key(&(name, layout)) {
            return;
        }

        // If this is an imported symbol, let its home module make this specialization
        if env.is_imported_symbol(name) {
            add_needed_external(self, env, fn_var, name);
            return;
        }

        // register the pending specialization, so this gets code genned later
        if self.module_thunks.contains(&name) {
            debug_assert!(layout.arguments.is_empty());
        }

        // This should only be called when pending_specializations is Some.
        // Otherwise, it's being called in the wrong pass!
        match &mut self.pending_specializations {
            Some(pending_specializations) => {
                let pending = PendingSpecialization::from_var(env.arena, env.subs, fn_var);

                add_pending(pending_specializations, name, layout, pending)
            }
            None => {
                let symbol = name;

                let partial_proc_id = match self.partial_procs.symbol_to_id(symbol) {
                    Some(p) => p,
                    None => panic!("no partial_proc for {:?} in module {:?}", symbol, env.home),
                };

                // Mark this proc as in-progress, so if we're dealing with
                // mutually recursive functions, we don't loop forever.
                // (We had a bug around this before this system existed!)
                self.specialized.insert((symbol, layout), InProgress);

                // See https://github.com/rtfeldman/roc/issues/1600
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
                        };

                        // NOTE: some function are specialized to have a closure, but don't actually
                        // need any closure argument. Here is where we correct this sort of thing,
                        // by trusting the layout of the Proc, not of what we specialize for
                        self.specialized.remove(&(symbol, layout));
                        self.specialized.insert((symbol, proper_layout), Done(proc));
                    }
                    Err(error) => {
                        panic!("TODO generate a RuntimeError message for {:?}", error);
                    }
                }
            }
        }
    }
}

fn add_pending<'a>(
    pending_specializations: &mut BumpMap<
        Symbol,
        MutMap<ProcLayout<'a>, PendingSpecialization<'a>>,
    >,
    symbol: Symbol,
    layout: ProcLayout<'a>,
    pending: PendingSpecialization<'a>,
) {
    let all_pending = pending_specializations
        .entry(symbol)
        .or_insert_with(|| HashMap::with_capacity_and_hasher(1, default_hasher()));

    all_pending.insert(layout, pending);
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
    pub problems: &'i mut std::vec::Vec<MonoProblem>,
    pub home: ModuleId,
    pub ident_ids: &'i mut IdentIds,
    pub ptr_bytes: u32,
    pub update_mode_counter: u64,
    pub call_specialization_counter: u64,
}

impl<'a, 'i> Env<'a, 'i> {
    pub fn unique_symbol(&mut self) -> Symbol {
        let ident_id = self.ident_ids.gen_unique();

        Symbol::new(self.home, ident_id)
    }

    pub fn next_update_mode_id(&mut self) -> UpdateModeId {
        let id = UpdateModeId {
            id: self.update_mode_counter,
        };

        self.update_mode_counter += 1;

        id
    }

    pub fn next_call_specialization_id(&mut self) -> CallSpecId {
        let id = CallSpecId {
            id: self.call_specialization_counter,
        };

        self.call_specialization_counter += 1;

        id
    }

    pub fn is_imported_symbol(&self, symbol: Symbol) -> bool {
        symbol.module_id() != self.home && !symbol.is_builtin()
    }
}

#[derive(Clone, Debug, PartialEq, Copy, Eq, Hash)]
pub struct JoinPointId(pub Symbol);

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Param<'a> {
    pub symbol: Symbol,
    pub borrow: bool,
    pub layout: Layout<'a>,
}

impl<'a> Param<'a> {
    pub const EMPTY: Self = Param {
        symbol: Symbol::EMPTY_PARAM,
        borrow: false,
        layout: Layout::Struct(&[]),
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
    RuntimeError(&'a str),
}

/// in the block below, symbol `scrutinee` is assumed be be of shape `tag_id`
#[derive(Clone, Debug, PartialEq)]
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
            } if PRETTY_PRINT_IR_SYMBOLS => alloc
                .hardline()
                .append("    BranchInfo: { scrutinee: ")
                .append(symbol_to_doc(alloc, *scrutinee))
                .append(", tag_id: ")
                .append(format!("{}", tag_id))
                .append("} "),

            _ => {
                if PRETTY_PRINT_IR_SYMBOLS {
                    alloc.text(" <no branch info>")
                } else {
                    alloc.text("")
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ModifyRc {
    Inc(Symbol, u64),
    Dec(Symbol),
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
                .append(alloc.text(format!("{}", n)))
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
    Int(i128),
    Float(f64),
    Decimal(RocDec),
    Str(&'a str),
    /// Closed tag unions containing exactly two (0-arity) tags compile to Expr::Bool,
    /// so they can (at least potentially) be emitted as 1-bit machine bools.
    ///
    /// So [ True, False ] compiles to this, and so do [ A, B ] and [ Foo, Bar ].
    /// However, a union like [ True, False, Other Int ] would not.
    Bool(bool),
    /// Closed tag unions containing between 3 and 256 tags (all of 0 arity)
    /// compile to bytes, e.g. [ Blue, Black, Red, Green, White ]
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

#[derive(Clone, Debug, PartialEq)]
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
                let it = std::iter::once(name)
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct CallSpecId {
    id: u64,
}

impl CallSpecId {
    pub fn to_bytes(self) -> [u8; 8] {
        self.id.to_ne_bytes()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct UpdateModeId {
    id: u64,
}

impl UpdateModeId {
    pub fn to_bytes(self) -> [u8; 8] {
        self.id.to_ne_bytes()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum CallType<'a> {
    ByName {
        name: Symbol,
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

#[derive(Clone, Debug, PartialEq)]
pub struct HigherOrderLowLevel<'a> {
    pub op: crate::low_level::HigherOrder,
    /// the layout of the closure argument, if any
    pub closure_env_layout: Option<Layout<'a>>,

    /// name of the top-level function that is passed as an argument
    /// e.g. in `List.map xs Num.abs` this would be `Num.abs`
    pub function_name: Symbol,

    /// Symbol of the environment captured by the function argument
    pub function_env: Symbol,

    /// does the function argument need to own the closure data
    pub function_owns_closure_data: bool,

    /// specialization id of the function argument, used for name generation
    pub specialization_id: CallSpecId,

    /// update mode of the higher order lowlevel itself
    pub update_mode: UpdateModeId,

    /// function layout, used for name generation
    pub arg_layouts: &'a [Layout<'a>],
    pub ret_layout: Layout<'a>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'a> {
    Literal(Literal<'a>),

    // Functions
    Call(Call<'a>),

    Tag {
        tag_layout: UnionLayout<'a>,
        tag_name: TagName,
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

    Reuse {
        symbol: Symbol,
        update_tag_id: bool,
        // normal Tag fields
        tag_layout: UnionLayout<'a>,
        tag_name: TagName,
        tag_id: TagIdIntType,
        arguments: &'a [Symbol],
    },
    Reset(Symbol),

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
            Int(lit) => alloc.text(format!("{}i64", lit)),
            Float(lit) => alloc.text(format!("{}f64", lit)),
            // TODO: Add proper Dec.to_str
            Decimal(lit) => alloc.text(format!("{}Dec", lit.0)),
            Bool(lit) => alloc.text(format!("{}", lit)),
            Byte(lit) => alloc.text(format!("{}u8", lit)),
            Str(lit) => alloc.text(format!("{:?}", lit)),
        }
    }
}

fn symbol_to_doc<'b, D, A>(alloc: &'b D, symbol: Symbol) -> DocBuilder<'b, D, A>
where
    D: DocAllocator<'b, A>,
    D::Doc: Clone,
    A: Clone,
{
    use roc_module::ident::ModuleName;

    if PRETTY_PRINT_IR_SYMBOLS {
        alloc.text(format!("{:?}", symbol))
    } else {
        let text = format!("{}", symbol);

        if text.starts_with(ModuleName::APP) {
            let name: String = text.trim_start_matches(ModuleName::APP).into();
            alloc.text("Test").append(name)
        } else {
            alloc.text(text)
        }
    }
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
                tag_name,
                arguments,
                ..
            } => {
                let doc_tag = match tag_name {
                    TagName::Global(s) => alloc.text(s.as_str()),
                    TagName::Private(s) => symbol_to_doc(alloc, *s),
                    TagName::Closure(s) => alloc
                        .text("ClosureTag(")
                        .append(symbol_to_doc(alloc, *s))
                        .append(")"),
                };

                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s));

                doc_tag
                    .append(alloc.space())
                    .append(alloc.intersperse(it, " "))
            }
            Reuse {
                symbol,
                tag_name,
                arguments,
                ..
            } => {
                let doc_tag = match tag_name {
                    TagName::Global(s) => alloc.text(s.as_str()),
                    TagName::Private(s) => alloc.text(format!("{}", s)),
                    TagName::Closure(s) => alloc
                        .text("ClosureTag(")
                        .append(symbol_to_doc(alloc, *s))
                        .append(")"),
                };

                let it = arguments.iter().map(|s| symbol_to_doc(alloc, *s));

                alloc
                    .text("Reuse ")
                    .append(symbol_to_doc(alloc, *symbol))
                    .append(alloc.space())
                    .append(doc_tag)
                    .append(alloc.space())
                    .append(alloc.intersperse(it, " "))
            }
            Reset(symbol) => alloc.text("Reset ").append(symbol_to_doc(alloc, *symbol)),

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

    pub fn to_doc<'b, D, A>(&'b self, alloc: &'b D) -> DocBuilder<'b, D, A>
    where
        D: DocAllocator<'b, A>,
        D::Doc: Clone,
        A: Clone,
    {
        use Stmt::*;

        match self {
            Let(symbol, expr, _layout, cont) => alloc
                .text("let ")
                .append(symbol_to_doc(alloc, *symbol))
                //.append(" : ")
                //.append(alloc.text(format!("{:?}", _layout)))
                .append(" = ")
                .append(expr.to_doc(alloc))
                .append(";")
                .append(alloc.hardline())
                .append(cont.to_doc(alloc)),

            Refcounting(modify, cont) => modify
                .to_doc(alloc)
                .append(alloc.hardline())
                .append(cont.to_doc(alloc)),

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
                            .append(pass.to_doc(alloc).indent(4))
                            .append(alloc.hardline())
                            .append(alloc.text("else"))
                            .append(default_branch.0.to_doc(alloc))
                            .append(alloc.hardline())
                            .append(fail.to_doc(alloc).indent(4))
                    }

                    _ => {
                        let default_doc = alloc
                            .text("default:")
                            .append(alloc.hardline())
                            .append(default_branch.1.to_doc(alloc).indent(4))
                            .indent(4);

                        let branches_docs = branches
                            .iter()
                            .map(|(tag, _info, expr)| {
                                alloc
                                    .text(format!("case {}:", tag))
                                    .append(alloc.hardline())
                                    .append(expr.to_doc(alloc).indent(4))
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

            RuntimeError(s) => alloc.text(format!("Error {}", s)),

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
                        continuation.to_doc(alloc).indent(4),
                        alloc.text("in"),
                        remainder.to_doc(alloc),
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

    pub fn is_terminal(&self) -> bool {
        use Stmt::*;

        match self {
            Switch { .. } => {
                // TODO is this the reason Lean only looks at the outermost `when`?
                true
            }
            Ret(_) => true,
            Jump(_, _) => true,
            _ => false,
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
    layout_cache: &mut LayoutCache<'a>,
    patterns: std::vec::Vec<(Variable, Located<roc_can::pattern::Pattern>)>,
    body_var: Variable,
    body: Located<roc_can::expr::Expr>,
) -> Result<
    (
        Vec<'a, Variable>,
        Vec<'a, Symbol>,
        Located<roc_can::expr::Expr>,
    ),
    Located<RuntimeError>,
> {
    let mut arg_vars = Vec::with_capacity_in(patterns.len(), env.arena);
    let mut symbols = Vec::with_capacity_in(patterns.len(), env.arena);
    let mut body = Ok(body);

    // patterns that are not yet in a when (e.g. in let or function arguments) must be irrefutable
    // to pass type checking. So the order in which we add them to the body does not matter: there
    // are only stores anyway, no branches.
    //
    // NOTE this fails if the pattern contains rigid variables,
    // see https://github.com/rtfeldman/roc/issues/786
    // this must be fixed when moving exhaustiveness checking to the new canonical AST
    for (pattern_var, pattern) in patterns.into_iter() {
        let context = crate::exhaustive::Context::BadArg;
        let mono_pattern = match from_can_pattern(env, layout_cache, &pattern.value) {
            Ok((pat, assignments)) => {
                for (symbol, variable, expr) in assignments.into_iter().rev() {
                    if let Ok(old_body) = body {
                        let def = roc_can::def::Def {
                            annotation: None,
                            expr_var: variable,
                            loc_expr: Located::at(pattern.region, expr),
                            loc_pattern: Located::at(
                                pattern.region,
                                roc_can::pattern::Pattern::Identifier(symbol),
                            ),
                            pattern_vars: std::iter::once((symbol, variable)).collect(),
                        };
                        let new_expr = roc_can::expr::Expr::LetNonRec(
                            Box::new(def),
                            Box::new(old_body),
                            variable,
                        );
                        let new_body = Located {
                            region: pattern.region,
                            value: new_expr,
                        };

                        body = Ok(new_body);
                    }
                }

                pat
            }
            Err(runtime_error) => {
                // Even if the body was Ok, replace it with this Err.
                // If it was already an Err, leave it at that Err, so the first
                // RuntimeError we encountered remains the first.
                body = body.and({
                    Err(Located {
                        region: pattern.region,
                        value: runtime_error,
                    })
                });

                continue;
            }
        };

        match crate::exhaustive::check(
            pattern.region,
            &[(
                Located::at(pattern.region, mono_pattern),
                crate::exhaustive::Guard::NoGuard,
            )],
            context,
        ) {
            Ok(_) => {
                // Replace the body with a new one, but only if it was Ok.
                if let Ok(unwrapped_body) = body {
                    let (new_symbol, new_body) =
                        pattern_to_when(env, pattern_var, pattern, body_var, unwrapped_body);

                    symbols.push(new_symbol);
                    arg_vars.push(pattern_var);

                    body = Ok(new_body)
                }
            }
            Err(errors) => {
                for error in errors {
                    env.problems.push(MonoProblem::PatternProblem(error))
                }

                let value = RuntimeError::UnsupportedPattern(pattern.region);

                // Even if the body was Ok, replace it with this Err.
                // If it was already an Err, leave it at that Err, so the first
                // RuntimeError we encountered remains the first.
                body = body.and({
                    Err(Located {
                        region: pattern.region,
                        value,
                    })
                });
            }
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
    pattern: Located<roc_can::pattern::Pattern>,
    body_var: Variable,
    body: Located<roc_can::expr::Expr>,
) -> (Symbol, Located<roc_can::expr::Expr>) {
    use roc_can::expr::Expr::*;
    use roc_can::expr::WhenBranch;
    use roc_can::pattern::Pattern::*;

    match &pattern.value {
        Identifier(symbol) => (*symbol, body),
        Underscore => {
            // for underscore we generate a dummy Symbol
            (env.unique_symbol(), body)
        }
        Shadowed(region, loc_ident) => {
            let error = roc_problem::can::RuntimeError::Shadowing {
                original_region: *region,
                shadow: loc_ident.clone(),
            };
            (env.unique_symbol(), Located::at_zero(RuntimeError(error)))
        }

        UnsupportedPattern(region) => {
            // create the runtime error here, instead of delegating to When.
            // UnsupportedPattern should then never occur in When
            let error = roc_problem::can::RuntimeError::UnsupportedPattern(*region);
            (env.unique_symbol(), Located::at_zero(RuntimeError(error)))
        }

        MalformedPattern(problem, region) => {
            // create the runtime error here, instead of delegating to When.
            let error = roc_problem::can::RuntimeError::MalformedPattern(*problem, *region);
            (env.unique_symbol(), Located::at_zero(RuntimeError(error)))
        }

        AppliedTag { .. } | RecordDestructure { .. } => {
            let symbol = env.unique_symbol();

            let wrapped_body = When {
                cond_var: pattern_var,
                expr_var: body_var,
                region: Region::zero(),
                loc_cond: Box::new(Located::at_zero(Var(symbol))),
                branches: vec![WhenBranch {
                    patterns: vec![pattern],
                    value: body,
                    guard: None,
                }],
            };

            (symbol, Located::at_zero(wrapped_body))
        }

        IntLiteral(_, _, _) | NumLiteral(_, _, _) | FloatLiteral(_, _, _) | StrLiteral(_) => {
            // These patters are refutable, and thus should never occur outside a `when` expression
            // They should have been replaced with `UnsupportedPattern` during canonicalization
            unreachable!("refutable pattern {:?} where irrefutable pattern is expected. This should never happen!", pattern.value)
        }
    }
}

pub fn specialize_all<'a>(
    env: &mut Env<'a, '_>,
    mut procs: Procs<'a>,
    externals_others_need: ExternalSpecializations<'a>,
    specializations_for_host: BumpMap<Symbol, MutMap<ProcLayout<'a>, PendingSpecialization<'a>>>,
    layout_cache: &mut LayoutCache<'a>,
) -> Procs<'a> {
    specialize_externals_others_need(env, &mut procs, externals_others_need, layout_cache);

    // When calling from_can, pending_specializations should be unavailable.
    // This must be a single pass, and we must not add any more entries to it!
    let opt_pending_specializations = std::mem::replace(&mut procs.pending_specializations, None);

    let it = specializations_for_host
        .into_iter()
        .chain(opt_pending_specializations.into_iter().flatten());

    for (name, by_layout) in it {
        for (outside_layout, pending) in by_layout.into_iter() {
            // If we've already seen this (Symbol, Layout) combination before,
            // don't try to specialize it again. If we do, we'll loop forever!
            let key = (name, outside_layout);

            let partial_proc = match procs.specialized.entry(key) {
                Entry::Occupied(_) => {
                    // already specialized, just continue
                    continue;
                }
                Entry::Vacant(vacant) => {
                    match procs.partial_procs.symbol_to_id(name) {
                        Some(v) => {
                            // Mark this proc as in-progress, so if we're dealing with
                            // mutually recursive functions, we don't loop forever.
                            // (We had a bug around this before this system existed!)
                            vacant.insert(InProgress);

                            v
                        }
                        None => {
                            // TODO this assumes the specialization is done by another module
                            // make sure this does not become a problem down the road!
                            continue;
                        }
                    }
                }
            };

            match specialize(env, &mut procs, name, layout_cache, pending, partial_proc) {
                Ok((proc, layout)) => {
                    // TODO thiscode is duplicated elsewhere
                    let top_level = ProcLayout::from_raw(env.arena, layout);

                    if procs.is_module_thunk(proc.name) {
                        debug_assert!(
                            top_level.arguments.is_empty(),
                            "{:?} from {:?}",
                            name,
                            layout
                        );
                    }

                    debug_assert_eq!(outside_layout, top_level, " in {:?}", name);
                    procs.specialized.insert((name, top_level), Done(proc));
                }
                Err(SpecializeFailure {
                    attempted_layout, ..
                }) => {
                    let proc = generate_runtime_error_function(env, name, attempted_layout);

                    let top_level = ProcLayout::from_raw(env.arena, attempted_layout);

                    procs.specialized.insert((name, top_level), Done(proc));
                }
            }
        }
    }

    procs
}

fn specialize_externals_others_need<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    externals_others_need: ExternalSpecializations<'a>,
    layout_cache: &mut LayoutCache<'a>,
) {
    for (symbol, solved_types) in externals_others_need.specs.iter() {
        for solved_type in solved_types {
            // historical note: we used to deduplicate with a hash here,
            // but the cost of that hash is very high. So for now we make
            // duplicate specializations, and the insertion into a hash map
            // below will deduplicate them.

            let name = *symbol;

            let partial_proc_id = match procs.partial_procs.symbol_to_id(name) {
                Some(v) => v,
                None => {
                    panic!("Cannot find a partial proc for {:?}", name);
                }
            };

            // TODO I believe this is also duplicated
            match specialize_solved_type(
                env,
                procs,
                name,
                layout_cache,
                solved_type,
                BumpMap::new_in(env.arena),
                partial_proc_id,
            ) {
                Ok((proc, layout)) => {
                    let top_level = ProcLayout::from_raw(env.arena, layout);

                    if procs.is_module_thunk(name) {
                        debug_assert!(top_level.arguments.is_empty());
                    }

                    procs.specialized.insert((name, top_level), Done(proc));
                }
                Err(SpecializeFailure {
                    problem: _,
                    attempted_layout,
                }) => {
                    let proc = generate_runtime_error_function(env, name, attempted_layout);

                    let top_level = ProcLayout::from_raw(env.arena, attempted_layout);

                    procs.specialized.insert((name, top_level), Done(proc));
                }
            }
        }
    }
}

fn generate_runtime_error_function<'a>(
    env: &mut Env<'a, '_>,
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

    eprintln!("emitted runtime error function {:?}", &msg);

    let runtime_error = Stmt::RuntimeError(msg.into_bump_str());

    let (args, ret_layout) = match layout {
        RawFunctionLayout::Function(arg_layouts, lambda_set, ret_layout) => {
            let mut args = Vec::with_capacity_in(arg_layouts.len(), env.arena);

            for arg in arg_layouts {
                args.push((*arg, env.unique_symbol()));
            }

            args.push((Layout::LambdaSet(lambda_set), Symbol::ARG_CLOSURE));

            (args.into_bump_slice(), *ret_layout)
        }
        RawFunctionLayout::ZeroArgumentThunk(ret_layout) => (&[] as &[_], ret_layout),
    };

    Proc {
        name,
        args,
        body: runtime_error,
        closure_data_layout: None,
        ret_layout,
        is_self_recursive: SelfRecursive::NotSelfRecursive,
        must_own_arguments: false,
        host_exposed_layouts: HostExposedLayouts::NotHostExposed,
    }
}

fn specialize_external<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    proc_name: Symbol,
    layout_cache: &mut LayoutCache<'a>,
    fn_var: Variable,
    host_exposed_variables: &[(Symbol, Variable)],
    partial_proc_id: PartialProcId,
) -> Result<Proc<'a>, LayoutProblem> {
    let partial_proc = procs.partial_procs.get_id(partial_proc_id);
    let captured_symbols = partial_proc.captured_symbols;

    // unify the called function with the specialized signature, then specialize the function body
    let snapshot = env.subs.snapshot();
    let cache_snapshot = layout_cache.snapshot();

    let _unified = roc_unify::unify::unify(env.subs, partial_proc.annotation, fn_var);

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
        build_specialized_proc_from_var(env, layout_cache, proc_name, pattern_symbols, fn_var)?;

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
                        lambda_set,
                        Symbol::ARG_CLOSURE,
                        argument_symbols.into_bump_slice(),
                        argument_layouts,
                        return_layout,
                        assigned,
                        hole,
                    );

                    let proc = Proc {
                        name,
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
                        *return_layout,
                    );

                    procs
                        .specialized
                        .insert((name, top_level), InProgressProc::Done(proc));

                    aliases.insert(*symbol, (name, top_level, layout));
                }
                RawFunctionLayout::ZeroArgumentThunk(_) => {
                    unreachable!("so far");
                }
            }
        }

        HostExposedLayouts::HostExposed {
            rigids: BumpMap::new_in(env.arena),
            aliases,
        }
    };

    let recursivity = if partial_proc.is_self_recursive {
        SelfRecursive::SelfRecursive(JoinPointId(env.unique_symbol()))
    } else {
        SelfRecursive::NotSelfRecursive
    };

    let body = partial_proc.body.clone();
    let mut specialized_body = from_can(env, fn_var, body, procs, layout_cache);

    match specialized {
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

            // reset subs, so we don't get type errors when specializing for a different signature
            layout_cache.rollback_to(cache_snapshot);
            env.subs.rollback_to(snapshot);

            let closure_data_layout = match opt_closure_layout {
                Some(lambda_set) => Layout::LambdaSet(lambda_set),
                None => Layout::Struct(&[]),
            };

            // I'm not sure how to handle the closure case, does it ever occur?
            debug_assert!(matches!(captured_symbols, CapturedSymbols::None));

            let proc = Proc {
                name: proc_name,
                args: &[],
                body: specialized_body,
                closure_data_layout: Some(closure_data_layout),
                ret_layout,
                is_self_recursive: recursivity,
                must_own_arguments: false,
                host_exposed_layouts,
            };

            Ok(proc)
        }
        SpecializedLayout::FunctionBody {
            arguments: proc_args,
            closure: opt_closure_layout,
            ret_layout,
        } => {
            // unpack the closure symbols, if any
            match (opt_closure_layout, captured_symbols) {
                (Some(closure_layout), CapturedSymbols::Captured(captured)) => {
                    // debug_assert!(!captured.is_empty());

                    match closure_layout.layout_for_member(proc_name) {
                        ClosureRepresentation::Union {
                            alphabetic_order_fields: field_layouts,
                            union_layout,
                            tag_id,
                            ..
                        } => {
                            debug_assert!(matches!(union_layout, UnionLayout::NonRecursive(_)));
                            debug_assert_eq!(field_layouts.len(), captured.len());

                            // captured variables are in symbol-alphabetic order, but now we want
                            // them ordered by their alignment requirements
                            let mut combined = Vec::from_iter_in(
                                captured.iter().map(|(x, _)| x).zip(field_layouts.iter()),
                                env.arena,
                            );

                            let ptr_bytes = env.ptr_bytes;

                            combined.sort_by(|(_, layout1), (_, layout2)| {
                                let size1 = layout1.alignment_bytes(ptr_bytes);
                                let size2 = layout2.alignment_bytes(ptr_bytes);

                                size2.cmp(&size1)
                            });

                            for (index, (symbol, layout)) in combined.iter().enumerate() {
                                let expr = Expr::UnionAtIndex {
                                    tag_id,
                                    structure: Symbol::ARG_CLOSURE,
                                    index: index as u64,
                                    union_layout,
                                };

                                specialized_body = Stmt::Let(
                                    **symbol,
                                    expr,
                                    **layout,
                                    env.arena.alloc(specialized_body),
                                );
                            }
                        }
                        ClosureRepresentation::AlphabeticOrderStruct(field_layouts) => {
                            // captured variables are in symbol-alphabetic order, but now we want
                            // them ordered by their alignment requirements
                            let mut combined = Vec::from_iter_in(
                                captured.iter().map(|(x, _)| x).zip(field_layouts.iter()),
                                env.arena,
                            );

                            let ptr_bytes = env.ptr_bytes;

                            combined.sort_by(|(_, layout1), (_, layout2)| {
                                let size1 = layout1.alignment_bytes(ptr_bytes);
                                let size2 = layout2.alignment_bytes(ptr_bytes);

                                size2.cmp(&size1)
                            });

                            debug_assert_eq!(
                                captured.len(),
                                field_layouts.len(),
                                "{:?} captures {:?} but has layout {:?}",
                                proc_name,
                                &captured,
                                &field_layouts
                            );

                            for (index, (symbol, layout)) in combined.iter().enumerate() {
                                let expr = Expr::StructAtIndex {
                                    index: index as _,
                                    field_layouts,
                                    structure: Symbol::ARG_CLOSURE,
                                };

                                specialized_body = Stmt::Let(
                                    **symbol,
                                    expr,
                                    **layout,
                                    env.arena.alloc(specialized_body),
                                );
                            }
                            //                                    let symbol = captured[0].0;
                            //
                            //                                    substitute_in_exprs(
                            //                                        env.arena,
                            //                                        &mut specialized_body,
                            //                                        symbol,
                            //                                        Symbol::ARG_CLOSURE,
                            //                                    );
                        }

                        ClosureRepresentation::Other(layout) => match layout {
                            Layout::Builtin(Builtin::Int1) => {
                                // just ignore this value
                                // IDEA don't pass this value in the future
                            }
                            Layout::Builtin(Builtin::Int8) => {
                                // just ignore this value
                                // IDEA don't pass this value in the future
                            }
                            other => {
                                // NOTE other values always should be wrapped in a 1-element record
                                unreachable!(
                                    "{:?} is not a valid closure data representation",
                                    other
                                )
                            }
                        },
                    }
                }
                (None, CapturedSymbols::None) | (None, CapturedSymbols::Captured([])) => {}
                _ => unreachable!("to closure or not to closure?"),
            }

            // reset subs, so we don't get type errors when specializing for a different signature
            layout_cache.rollback_to(cache_snapshot);
            env.subs.rollback_to(snapshot);

            let closure_data_layout = match opt_closure_layout {
                Some(lambda_set) => Some(Layout::LambdaSet(lambda_set)),
                None => None,
            };

            let proc = Proc {
                name: proc_name,
                args: proc_args,
                body: specialized_body,
                closure_data_layout,
                ret_layout,
                is_self_recursive: recursivity,
                must_own_arguments: false,
                host_exposed_layouts,
            };

            Ok(proc)
        }
    }
}

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
    proc_name: Symbol,
    pattern_symbols: &[Symbol],
    fn_var: Variable,
) -> Result<SpecializedLayout<'a>, LayoutProblem> {
    match layout_cache.raw_from_var(env.arena, fn_var, env.subs)? {
        RawFunctionLayout::Function(pattern_layouts, closure_layout, ret_layout) => {
            let mut pattern_layouts_vec = Vec::with_capacity_in(pattern_layouts.len(), env.arena);
            pattern_layouts_vec.extend_from_slice(pattern_layouts);

            build_specialized_proc(
                env.arena,
                proc_name,
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
                proc_name,
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
    proc_name: Symbol,
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
    /// The problem we ran into while creating it
    problem: LayoutProblem,
}

type SpecializeSuccess<'a> = (Proc<'a>, RawFunctionLayout<'a>);

fn specialize<'a, 'b>(
    env: &mut Env<'a, '_>,
    procs: &'b mut Procs<'a>,
    proc_name: Symbol,
    layout_cache: &mut LayoutCache<'a>,
    pending: PendingSpecialization,
    partial_proc_id: PartialProcId,
) -> Result<SpecializeSuccess<'a>, SpecializeFailure<'a>> {
    let PendingSpecialization {
        solved_type,
        host_exposed_aliases,
        ..
    } = pending;

    specialize_solved_type(
        env,
        procs,
        proc_name,
        layout_cache,
        &solved_type,
        host_exposed_aliases,
        partial_proc_id,
    )
}

fn introduce_solved_type_to_subs<'a>(env: &mut Env<'a, '_>, solved_type: &SolvedType) -> Variable {
    use roc_solve::solve::insert_type_into_subs;
    use roc_types::solved_types::{to_type, FreeVars};
    use roc_types::subs::VarStore;
    let mut free_vars = FreeVars::default();
    let mut var_store = VarStore::new_from_subs(env.subs);

    let before = var_store.peek();

    let normal_type = to_type(solved_type, &mut free_vars, &mut var_store);

    let after = var_store.peek();
    let variables_introduced = after - before;

    env.subs.extend_by(variables_introduced as usize);

    insert_type_into_subs(env.subs, &normal_type)
}

fn specialize_solved_type<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    proc_name: Symbol,
    layout_cache: &mut LayoutCache<'a>,
    solved_type: &SolvedType,
    host_exposed_aliases: BumpMap<Symbol, SolvedType>,
    partial_proc_id: PartialProcId,
) -> Result<SpecializeSuccess<'a>, SpecializeFailure<'a>> {
    specialize_variable_help(
        env,
        procs,
        proc_name,
        layout_cache,
        |env| introduce_solved_type_to_subs(env, solved_type),
        host_exposed_aliases,
        partial_proc_id,
    )
}

fn specialize_variable<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    proc_name: Symbol,
    layout_cache: &mut LayoutCache<'a>,
    fn_var: Variable,
    host_exposed_aliases: BumpMap<Symbol, SolvedType>,
    partial_proc_id: PartialProcId,
) -> Result<SpecializeSuccess<'a>, SpecializeFailure<'a>> {
    specialize_variable_help(
        env,
        procs,
        proc_name,
        layout_cache,
        |_| fn_var,
        host_exposed_aliases,
        partial_proc_id,
    )
}

fn specialize_variable_help<'a, F>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    proc_name: Symbol,
    layout_cache: &mut LayoutCache<'a>,
    fn_var_thunk: F,
    host_exposed_aliases: BumpMap<Symbol, SolvedType>,
    partial_proc_id: PartialProcId,
) -> Result<SpecializeSuccess<'a>, SpecializeFailure<'a>>
where
    F: FnOnce(&mut Env<'a, '_>) -> Variable,
{
    // add the specializations that other modules require of us
    use roc_solve::solve::instantiate_rigids;

    let snapshot = env.subs.snapshot();
    let cache_snapshot = layout_cache.snapshot();

    // important: evaluate after the snapshot has been created!
    let fn_var = fn_var_thunk(env);

    // for debugging only
    let raw = layout_cache
        .raw_from_var(env.arena, fn_var, env.subs)
        .unwrap_or_else(|err| panic!("TODO handle invalid function {:?}", err));

    let raw = if procs.is_module_thunk(proc_name) {
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

    let mut host_exposed_variables = Vec::with_capacity_in(host_exposed_aliases.len(), env.arena);

    for (symbol, solved_type) in host_exposed_aliases {
        let alias_var = introduce_solved_type_to_subs(env, &solved_type);

        host_exposed_variables.push((symbol, alias_var));
    }

    let specialized = specialize_external(
        env,
        procs,
        proc_name,
        layout_cache,
        fn_var,
        &host_exposed_variables,
        partial_proc_id,
    );

    match specialized {
        Ok(proc) => {
            // when successful, the layout after unification should be the layout before unification
            //            debug_assert_eq!(
            //                attempted_layout,
            //                layout_cache
            //                    .from_var(env.arena, fn_var, env.subs)
            //                    .unwrap_or_else(|err| panic!("TODO handle invalid function {:?}", err))
            //            );

            env.subs.rollback_to(snapshot);
            layout_cache.rollback_to(cache_snapshot);

            Ok((proc, raw))
        }
        Err(error) => {
            env.subs.rollback_to(snapshot);
            layout_cache.rollback_to(cache_snapshot);

            Err(SpecializeFailure {
                problem: error,
                attempted_layout: raw,
            })
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ProcLayout<'a> {
    pub arguments: &'a [Layout<'a>],
    pub result: Layout<'a>,
}

impl<'a> ProcLayout<'a> {
    pub fn new(arena: &'a Bump, old_arguments: &'a [Layout<'a>], result: Layout<'a>) -> Self {
        let mut arguments = Vec::with_capacity_in(old_arguments.len(), arena);

        for old in old_arguments {
            let other = old;
            arguments.push(*other);
        }

        let other = result;
        let new_result = other;

        ProcLayout {
            arguments: arguments.into_bump_slice(),
            result: new_result,
        }
    }

    pub fn from_raw(arena: &'a Bump, raw: RawFunctionLayout<'a>) -> Self {
        match raw {
            RawFunctionLayout::Function(arguments, lambda_set, result) => {
                let arguments = lambda_set.extend_argument_list(arena, arguments);
                ProcLayout::new(arena, arguments, *result)
            }
            RawFunctionLayout::ZeroArgumentThunk(result) => ProcLayout::new(arena, &[], result),
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
        let partial_proc = procs.get_partial_proc(symbol).unwrap();
        let fn_var = partial_proc.annotation;

        // This is a top-level declaration, which will code gen to a 0-arity thunk.
        let result = call_by_name(
            env,
            procs,
            fn_var,
            symbol,
            std::vec::Vec::new(),
            layout_cache,
            assigned,
            env.arena.alloc(Stmt::Ret(assigned)),
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
                    env.arena.alloc(Stmt::Ret(assigned)),
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
    reuse_function_symbol(
        env,
        procs,
        layout_cache,
        opt_fn_var,
        symbol,
        result,
        original,
    )
}

fn try_make_literal<'a>(
    env: &mut Env<'a, '_>,
    can_expr: &roc_can::expr::Expr,
) -> Option<Literal<'a>> {
    use roc_can::expr::Expr::*;

    match can_expr {
        Int(_, precision, _, int) => {
            match num_argument_to_int_or_float(env.subs, env.ptr_bytes, *precision, false) {
                IntOrFloat::SignedIntType(_) | IntOrFloat::UnsignedIntType(_) => {
                    Some(Literal::Int(*int))
                }
                _ => unreachable!("unexpected float precision for integer"),
            }
        }

        Float(_, precision, float_str, float) => {
            match num_argument_to_int_or_float(env.subs, env.ptr_bytes, *precision, true) {
                IntOrFloat::BinaryFloatType(_) => Some(Literal::Float(*float)),
                IntOrFloat::DecimalFloatType => {
                    let dec = match RocDec::from_str(float_str) {
                            Some(d) => d,
                            None => panic!("Invalid decimal for float literal = {}. TODO: Make this a nice, user-friendly error message", float_str),
                        };

                    Some(Literal::Decimal(dec))
                }
                _ => unreachable!("unexpected float precision for integer"),
            }
        }

        // TODO investigate lifetime trouble
        // Str(string) => Some(Literal::Str(env.arena.alloc(string))),
        Num(var, num_str, num) => {
            // first figure out what kind of number this is
            match num_argument_to_int_or_float(env.subs, env.ptr_bytes, *var, false) {
                IntOrFloat::SignedIntType(_) | IntOrFloat::UnsignedIntType(_) => {
                    Some(Literal::Int((*num).into()))
                }
                IntOrFloat::BinaryFloatType(_) => Some(Literal::Float(*num as f64)),
                IntOrFloat::DecimalFloatType => {
                    let dec = match RocDec::from_str(num_str) {
                        Some(d) => d,
                        None => panic!(
                            r"Invalid decimal for float literal = {}. TODO: Make this a nice, user-friendly error message",
                            num_str
                        ),
                    };

                    Some(Literal::Decimal(dec))
                }
            }
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
        Int(_, precision, _, int) => {
            match num_argument_to_int_or_float(env.subs, env.ptr_bytes, precision, false) {
                IntOrFloat::SignedIntType(precision) => Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Int(int)),
                    precision.as_layout(),
                    hole,
                ),
                IntOrFloat::UnsignedIntType(precision) => Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Int(int)),
                    precision.as_layout(),
                    hole,
                ),
                _ => unreachable!("unexpected float precision for integer"),
            }
        }

        Float(_, precision, float_str, float) => {
            match num_argument_to_int_or_float(env.subs, env.ptr_bytes, precision, true) {
                IntOrFloat::BinaryFloatType(precision) => Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Float(float)),
                    precision.as_layout(),
                    hole,
                ),
                IntOrFloat::DecimalFloatType => {
                    let dec = match RocDec::from_str(&float_str) {
                            Some(d) => d,
                            None => panic!("Invalid decimal for float literal = {}. TODO: Make this a nice, user-friendly error message", float_str),
                        };
                    Stmt::Let(
                        assigned,
                        Expr::Literal(Literal::Decimal(dec)),
                        Layout::Builtin(Builtin::Decimal),
                        hole,
                    )
                }
                _ => unreachable!("unexpected float precision for integer"),
            }
        }

        Str(string) => Stmt::Let(
            assigned,
            Expr::Literal(Literal::Str(arena.alloc(string))),
            Layout::Builtin(Builtin::Str),
            hole,
        ),

        Num(var, num_str, num) => {
            // first figure out what kind of number this is
            match num_argument_to_int_or_float(env.subs, env.ptr_bytes, var, false) {
                IntOrFloat::SignedIntType(precision) => Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Int(num.into())),
                    precision.as_layout(),
                    hole,
                ),
                IntOrFloat::UnsignedIntType(precision) => Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Int(num.into())),
                    precision.as_layout(),
                    hole,
                ),
                IntOrFloat::BinaryFloatType(precision) => Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Float(num as f64)),
                    precision.as_layout(),
                    hole,
                ),
                IntOrFloat::DecimalFloatType => {
                    let dec = match RocDec::from_str(&num_str) {
                            Some(d) => d,
                            None => panic!("Invalid decimal for float literal = {}. TODO: Make this a nice, user-friendly error message", num_str),
                        };
                    Stmt::Let(
                        assigned,
                        Expr::Literal(Literal::Decimal(dec)),
                        Layout::Builtin(Builtin::Decimal),
                        hole,
                    )
                }
            }
        }
        LetNonRec(def, cont, _) => {
            if let roc_can::pattern::Pattern::Identifier(symbol) = def.loc_pattern.value {
                if let Closure(closure_data) = def.loc_expr.value {
                    register_noncapturing_closure(env, procs, layout_cache, symbol, closure_data);

                    return with_hole(
                        env,
                        cont.value,
                        variable,
                        procs,
                        layout_cache,
                        assigned,
                        hole,
                    );
                }
                // special-case the form `let x = E in x`
                // not doing so will drop the `hole`
                match &cont.value {
                    roc_can::expr::Expr::Var(original) if *original == symbol => {
                        return with_hole(
                            env,
                            def.loc_expr.value,
                            def.expr_var,
                            procs,
                            layout_cache,
                            assigned,
                            hole,
                        );
                    }
                    _ => {}
                }

                // continue with the default path
                let mut stmt = with_hole(
                    env,
                    cont.value,
                    variable,
                    procs,
                    layout_cache,
                    assigned,
                    hole,
                );

                // a variable is aliased
                if let roc_can::expr::Expr::Var(original) = def.loc_expr.value {
                    // a variable is aliased, e.g.
                    //
                    //  foo = bar
                    //
                    // or
                    //
                    //  foo = RBTRee.empty

                    stmt = handle_variable_aliasing(
                        env,
                        procs,
                        layout_cache,
                        def.expr_var,
                        symbol,
                        original,
                        stmt,
                    );

                    stmt
                } else {
                    with_hole(
                        env,
                        def.loc_expr.value,
                        def.expr_var,
                        procs,
                        layout_cache,
                        symbol,
                        env.arena.alloc(stmt),
                    )
                }
            } else {
                // this may be a destructure pattern
                let (mono_pattern, assignments) =
                    match from_can_pattern(env, layout_cache, &def.loc_pattern.value) {
                        Ok(v) => v,
                        Err(_runtime_error) => {
                            // todo
                            panic!();
                        }
                    };

                let context = crate::exhaustive::Context::BadDestruct;
                match crate::exhaustive::check(
                    def.loc_pattern.region,
                    &[(
                        Located::at(def.loc_pattern.region, mono_pattern.clone()),
                        crate::exhaustive::Guard::NoGuard,
                    )],
                    context,
                ) {
                    Ok(_) => {}
                    Err(errors) => {
                        for error in errors {
                            env.problems.push(MonoProblem::PatternProblem(error))
                        }
                    } // TODO make all variables bound in the pattern evaluate to a runtime error
                      // return Stmt::RuntimeError("TODO non-exhaustive pattern");
                }

                let mut hole = hole;

                for (symbol, variable, expr) in assignments {
                    let stmt = with_hole(env, expr, variable, procs, layout_cache, symbol, hole);

                    hole = env.arena.alloc(stmt);
                }

                // convert the continuation
                let mut stmt = with_hole(
                    env,
                    cont.value,
                    variable,
                    procs,
                    layout_cache,
                    assigned,
                    hole,
                );

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
        LetRec(defs, cont, _) => {
            // because Roc is strict, only functions can be recursive!
            for def in defs.into_iter() {
                if let roc_can::pattern::Pattern::Identifier(symbol) = &def.loc_pattern.value {
                    if let Closure(closure_data) = def.loc_expr.value {
                        register_noncapturing_closure(
                            env,
                            procs,
                            layout_cache,
                            *symbol,
                            closure_data,
                        );

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
        Var(symbol) => {
            specialize_naked_symbol(env, variable, procs, layout_cache, assigned, hole, symbol)
        }
        Tag {
            variant_var,
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
            variant_var,
            name: tag_name,
            arguments: args,
            ext_var,
            closure_name,
        } => {
            let arena = env.arena;

            let content = env.subs.get_content_without_compacting(variant_var);

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
                    variant_var,
                    layout_cache,
                    assigned,
                    hole,
                )
            } else {
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
        }

        Record {
            record_var,
            mut fields,
            ..
        } => {
            let sorted_fields =
                crate::layout::sort_record_fields(env.arena, record_var, env.subs, env.ptr_bytes);

            let mut field_symbols = Vec::with_capacity_in(fields.len(), env.arena);
            let mut can_fields = Vec::with_capacity_in(fields.len(), env.arena);

            enum Field {
                Function(Symbol, Variable),
                ValueSymbol,
                Field(roc_can::expr::Field),
            }

            for (label, variable, _) in sorted_fields.into_iter() {
                // TODO how should function pointers be handled here?
                use ReuseSymbol::*;
                match fields.remove(&label) {
                    Some(field) => match can_reuse_symbol(env, procs, &field.loc_expr.value) {
                        Imported(symbol) | LocalFunction(symbol) => {
                            field_symbols.push(symbol);
                            can_fields.push(Field::Function(symbol, variable));
                        }
                        Value(reusable) => {
                            field_symbols.push(reusable);
                            can_fields.push(Field::ValueSymbol);
                        }
                        NotASymbol => {
                            field_symbols.push(env.unique_symbol());
                            can_fields.push(Field::Field(field));
                        }
                    },
                    None => {
                        // this field was optional, but not given
                        continue;
                    }
                }
            }

            // creating a record from the var will unpack it if it's just a single field.
            let layout = layout_cache
                .from_var(env.arena, record_var, env.subs)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

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
                    Field::Function(symbol, variable) => {
                        stmt = reuse_function_symbol(
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

        Expect(_, _) => unreachable!("I think this is unreachable"),

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
                            let branching_symbol =
                                possible_reuse_symbol(env, procs, &loc_cond.value);

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
                (Err(_), _) => Stmt::RuntimeError("invalid ret_layout"),
                (_, Err(_)) => Stmt::RuntimeError("invalid cond_layout"),
            }
        }

        When {
            cond_var,
            expr_var,
            region,
            loc_cond,
            branches,
        } => {
            let cond_symbol = possible_reuse_symbol(env, procs, &loc_cond.value);

            let id = JoinPointId(env.unique_symbol());

            let mut stmt = from_can_when(
                env,
                cond_var,
                expr_var,
                region,
                cond_symbol,
                branches,
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
                    Stmt::Let(assigned, expr, Layout::Builtin(Builtin::EmptyList), hole)
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

            for arg_expr in loc_elems.into_iter() {
                if let Some(literal) = try_make_literal(env, &arg_expr.value) {
                    elements.push(ListLiteralElement::Literal(literal));
                } else {
                    let symbol = possible_reuse_symbol(env, procs, &arg_expr.value);

                    elements.push(ListLiteralElement::Symbol(symbol));
                    arg_symbols.push(symbol);
                    symbol_exprs.push(arg_expr);
                }
            }
            let arg_symbols = arg_symbols.into_bump_slice();

            let elem_layout = layout_cache
                .from_var(env.arena, elem_var, env.subs)
                .unwrap_or_else(|err| panic!("TODO turn fn_var into a RuntimeError {:?}", err));

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
            let sorted_fields =
                crate::layout::sort_record_fields(env.arena, record_var, env.subs, env.ptr_bytes);

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

            let record_symbol = possible_reuse_symbol(env, procs, &loc_expr.value);

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

        Accessor {
            name,
            function_var,
            record_var,
            closure_ext_var: _,
            ext_var,
            field_var,
            field,
        } => {
            // IDEA: convert accessor fromt
            //
            // .foo
            //
            // into
            //
            // (\r -> r.foo)
            let record_symbol = env.unique_symbol();
            let body = roc_can::expr::Expr::Access {
                record_var,
                ext_var,
                field_var,
                loc_expr: Box::new(Located::at_zero(roc_can::expr::Expr::Var(record_symbol))),
                field,
            };

            let loc_body = Located::at_zero(body);

            let arguments = vec![(
                record_var,
                Located::at_zero(roc_can::pattern::Pattern::Identifier(record_symbol)),
            )];

            match procs.insert_anonymous(
                env,
                name,
                function_var,
                arguments,
                loc_body,
                CapturedSymbols::None,
                field_var,
                layout_cache,
            ) {
                Ok(_) => {
                    let raw_layout = return_on_layout_error!(
                        env,
                        layout_cache.raw_from_var(env.arena, function_var, env.subs)
                    );

                    match raw_layout {
                        RawFunctionLayout::Function(_, lambda_set, _) => {
                            construct_closure_data(env, lambda_set, name, &[], assigned, hole)
                        }
                        RawFunctionLayout::ZeroArgumentThunk(_) => unreachable!(),
                    }
                }

                Err(_error) => Stmt::RuntimeError(
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

            let sorted_fields =
                crate::layout::sort_record_fields(env.arena, record_var, env.subs, env.ptr_bytes);

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
                            // TODO
                            let field_symbol =
                                possible_reuse_symbol(env, procs, &field.loc_expr.value);

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
                Layout::Struct(layouts) => *layouts,
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
                            let access_expr = Expr::StructAtIndex {
                                structure,
                                index,
                                field_layouts,
                            };
                            stmt =
                                Stmt::Let(*symbol, access_expr, *field_layout, arena.alloc(stmt));
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

            match return_on_layout_error!(env, raw) {
                RawFunctionLayout::ZeroArgumentThunk(_) => {
                    unreachable!("a closure syntactically always must have at least one argument")
                }
                RawFunctionLayout::Function(_argument_layouts, lambda_set, _ret_layout) => {
                    let mut captured_symbols = Vec::from_iter_in(captured_symbols, env.arena);
                    captured_symbols.sort();
                    let captured_symbols = captured_symbols.into_bump_slice();

                    let inserted = procs.insert_anonymous(
                        env,
                        name,
                        function_type,
                        arguments,
                        loc_body,
                        CapturedSymbols::Captured(captured_symbols),
                        return_type,
                        layout_cache,
                    );

                    if let Err(runtime_error) = inserted {
                        return Stmt::RuntimeError(env.arena.alloc(format!(
                            "RuntimeError {} line {} {:?}",
                            file!(),
                            line!(),
                            runtime_error,
                        )));
                    } else {
                        drop(inserted);
                    }

                    // define the closure data

                    let symbols =
                        Vec::from_iter_in(captured_symbols.iter().map(|x| x.0), env.arena)
                            .into_bump_slice();

                    construct_closure_data(env, lambda_set, name, symbols, assigned, hole)
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
                procs.partial_procs.contains_key(key) || env.is_imported_symbol(key)
            };

            match loc_expr.value {
                roc_can::expr::Expr::Var(proc_name) if is_known(proc_name) => {
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
                        loc_args.iter().map(|(_, arg_expr)| {
                            possible_reuse_symbol(env, procs, &arg_expr.value)
                        }),
                        arena,
                    )
                    .into_bump_slice();

                    let full_layout = return_on_layout_error!(
                        env,
                        layout_cache.raw_from_var(env.arena, fn_var, env.subs)
                    );

                    // if the function expression (loc_expr) is already a symbol,
                    // re-use that symbol, and don't define its value again
                    let mut result;
                    use ReuseSymbol::*;
                    match can_reuse_symbol(env, procs, &loc_expr.value) {
                        LocalFunction(_) => {
                            unreachable!("if this was known to be a function, we would not be here")
                        }
                        Imported(_) => {
                            unreachable!("an imported value is never an anonymous function")
                        }
                        Value(function_symbol) => match full_layout {
                            RawFunctionLayout::Function(arg_layouts, lambda_set, ret_layout) => {
                                let closure_data_symbol = function_symbol;

                                result = match_on_lambda_set(
                                    env,
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
                        },
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

            for (_, arg_expr) in args.iter() {
                arg_symbols.push(possible_reuse_symbol(env, procs, arg_expr));
            }
            let arg_symbols = arg_symbols.into_bump_slice();

            // layout of the return type
            let layout =
                return_on_layout_error!(env, layout_cache.from_var(env.arena, ret_var, env.subs));

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
                .map(|(a, b)| (a, Located::at_zero(b)))
                .zip(arg_symbols.iter().rev());
            assign_to_symbols(env, procs, layout_cache, iter, result)
        }

        RunLowLevel { op, args, ret_var } => {
            let mut arg_symbols = Vec::with_capacity_in(args.len(), env.arena);

            for (_, arg_expr) in args.iter() {
                arg_symbols.push(possible_reuse_symbol(env, procs, arg_expr));
            }
            let arg_symbols = arg_symbols.into_bump_slice();

            // layout of the return type
            let layout =
                return_on_layout_error!(env, layout_cache.from_var(env.arena, ret_var, env.subs));

            macro_rules! match_on_closure_argument {
                ( $ho:ident, [$($x:ident),* $(,)?]) => {{
                    let closure_index = op.function_argument_position();
                    let closure_data_symbol = arg_symbols[closure_index];
                    let closure_data_var = args[closure_index].0;

                    let closure_data_layout = return_on_layout_error!(
                        env,
                        layout_cache.raw_from_var(env.arena, closure_data_var, env.subs)
                    );

                    let top_level = ProcLayout::from_raw(env.arena, closure_data_layout);

                    let arena = env.arena;

                    let arg_layouts = top_level.arguments;
                    let ret_layout = top_level.result;

                    match closure_data_layout {
                        RawFunctionLayout::Function(_, lambda_set, _) =>  {
                            lowlevel_match_on_lambda_set(
                                env,
                                lambda_set,
                                op,
                                closure_data_symbol,
                                |(top_level_function, closure_data, closure_env_layout,  specialization_id, update_mode)| {
                                    let higher_order = HigherOrderLowLevel {
                                        op: crate::low_level::HigherOrder::$ho { $($x,)* },
                                        closure_env_layout,
                                        specialization_id,
                                        update_mode,
                                        function_owns_closure_data: false,
                                        function_env: closure_data_symbol,
                                        function_name: top_level_function,
                                        arg_layouts,
                                        ret_layout,
                                    };

                                    self::Call {
                                        call_type: CallType::HigherOrder(arena.alloc(higher_order)),
                                        arguments: arena.alloc([$($x,)* top_level_function, closure_data]),
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

            macro_rules! walk {
                ($oh:ident) => {{
                    debug_assert_eq!(arg_symbols.len(), 3);

                    const LIST_INDEX: usize = 0;
                    const DEFAULT_INDEX: usize = 1;
                    const CLOSURE_INDEX: usize = 2;

                    let xs = arg_symbols[LIST_INDEX];
                    let state = arg_symbols[DEFAULT_INDEX];

                    let stmt = match_on_closure_argument!($oh, [xs, state]);

                    // because of a hack to implement List.product and List.sum, we need to also
                    // assign to symbols here. Normally the arguments to a lowlevel function are
                    // all symbols anyway, but because of this hack the closure symbol can be an
                    // actual closure, and the default is either the number 1 or 0
                    // this can be removed when we define builtin modules as proper modules

                    let stmt = assign_to_symbol(
                        env,
                        procs,
                        layout_cache,
                        args[LIST_INDEX].0,
                        Located::at_zero(args[LIST_INDEX].1.clone()),
                        arg_symbols[LIST_INDEX],
                        stmt,
                    );

                    let stmt = assign_to_symbol(
                        env,
                        procs,
                        layout_cache,
                        args[DEFAULT_INDEX].0,
                        Located::at_zero(args[DEFAULT_INDEX].1.clone()),
                        arg_symbols[DEFAULT_INDEX],
                        stmt,
                    );

                    assign_to_symbol(
                        env,
                        procs,
                        layout_cache,
                        args[CLOSURE_INDEX].0,
                        Located::at_zero(args[CLOSURE_INDEX].1.clone()),
                        arg_symbols[CLOSURE_INDEX],
                        stmt,
                    )
                }};
            }

            use LowLevel::*;
            match op {
                ListMap => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListMap, [xs])
                }

                ListMapWithIndex => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListMapWithIndex, [xs])
                }
                ListKeepIf => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListKeepIf, [xs])
                }
                ListAny => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListAny, [xs])
                }
                ListKeepOks => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListKeepOks, [xs])
                }
                ListKeepErrs => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListKeepErrs, [xs])
                }
                ListSortWith => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListSortWith, [xs])
                }
                ListWalk => walk!(ListWalk),
                ListWalkUntil => walk!(ListWalkUntil),
                ListWalkBackwards => walk!(ListWalkBackwards),
                DictWalk => walk!(DictWalk),
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
                ListFindUnsafe => {
                    debug_assert_eq!(arg_symbols.len(), 2);
                    let xs = arg_symbols[0];
                    match_on_closure_argument!(ListFindUnsafe, [xs])
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
                        .map(|(a, b)| (a, Located::at_zero(b)))
                        .zip(arg_symbols.iter().rev());
                    assign_to_symbols(env, procs, layout_cache, iter, result)
                }
            }
        }
        RuntimeError(e) => {
            eprintln!("emitted runtime error {:?}", &e);

            Stmt::RuntimeError(env.arena.alloc(format!("{:?}", e)))
        }
    }
}

fn construct_closure_data<'a>(
    env: &mut Env<'a, '_>,
    lambda_set: LambdaSet<'a>,
    name: Symbol,
    symbols: &'a [Symbol],
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let lambda_set_layout = Layout::LambdaSet(lambda_set);

    match lambda_set.layout_for_member(name) {
        ClosureRepresentation::Union {
            tag_id,
            alphabetic_order_fields: field_layouts,
            tag_name,
            union_layout,
        } => {
            // captured variables are in symbol-alphabetic order, but now we want
            // them ordered by their alignment requirements
            let mut combined =
                Vec::from_iter_in(symbols.iter().zip(field_layouts.iter()), env.arena);

            let ptr_bytes = env.ptr_bytes;

            combined.sort_by(|(_, layout1), (_, layout2)| {
                let size1 = layout1.alignment_bytes(ptr_bytes);
                let size2 = layout2.alignment_bytes(ptr_bytes);

                size2.cmp(&size1)
            });

            let symbols =
                Vec::from_iter_in(combined.iter().map(|(a, _)| **a), env.arena).into_bump_slice();

            let expr = Expr::Tag {
                tag_id,
                tag_layout: union_layout,
                tag_name,
                arguments: symbols,
            };

            Stmt::Let(assigned, expr, lambda_set_layout, env.arena.alloc(hole))
        }
        ClosureRepresentation::AlphabeticOrderStruct(field_layouts) => {
            debug_assert_eq!(field_layouts.len(), symbols.len());

            // captured variables are in symbol-alphabetic order, but now we want
            // them ordered by their alignment requirements
            let mut combined =
                Vec::from_iter_in(symbols.iter().zip(field_layouts.iter()), env.arena);

            let ptr_bytes = env.ptr_bytes;

            combined.sort_by(|(_, layout1), (_, layout2)| {
                let size1 = layout1.alignment_bytes(ptr_bytes);
                let size2 = layout2.alignment_bytes(ptr_bytes);

                size2.cmp(&size1)
            });

            let symbols =
                Vec::from_iter_in(combined.iter().map(|(a, _)| **a), env.arena).into_bump_slice();
            let field_layouts =
                Vec::from_iter_in(combined.iter().map(|(_, b)| **b), env.arena).into_bump_slice();

            debug_assert_eq!(
                Layout::Struct(field_layouts),
                lambda_set.runtime_representation()
            );

            let expr = Expr::Struct(symbols);

            Stmt::Let(assigned, expr, lambda_set_layout, hole)
        }
        ClosureRepresentation::Other(Layout::Builtin(Builtin::Int1)) => {
            debug_assert_eq!(symbols.len(), 0);

            debug_assert_eq!(lambda_set.set.len(), 2);
            let tag_id = name != lambda_set.set[0].0;
            let expr = Expr::Literal(Literal::Bool(tag_id));

            Stmt::Let(assigned, expr, lambda_set_layout, hole)
        }
        ClosureRepresentation::Other(Layout::Builtin(Builtin::Int8)) => {
            debug_assert_eq!(symbols.len(), 0);

            debug_assert!(lambda_set.set.len() > 2);
            let tag_id = lambda_set.set.iter().position(|(s, _)| *s == name).unwrap() as u8;
            let expr = Expr::Literal(Literal::Byte(tag_id));

            Stmt::Let(assigned, expr, lambda_set_layout, hole)
        }
        _ => unreachable!(),
    }
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
    args: std::vec::Vec<(Variable, Located<roc_can::expr::Expr>)>,
    arena: &'a Bump,
) -> Stmt<'a> {
    use crate::layout::UnionVariant::*;
    let res_variant =
        crate::layout::union_sorted_tags(env.arena, variant_var, env.subs, env.ptr_bytes);
    let variant = match res_variant {
        Ok(cached) => cached,
        Err(LayoutProblem::UnresolvedTypeVar(_)) => {
            return Stmt::RuntimeError(env.arena.alloc(format!(
                "UnresolvedTypeVar {} line {}",
                file!(),
                line!()
            )))
        }
        Err(LayoutProblem::Erroneous) => {
            return Stmt::RuntimeError(env.arena.alloc(format!(
                "Erroneous {} line {}",
                file!(),
                line!()
            )));
        }
    };

    match variant {
        Never => unreachable!(
            "The `[]` type has no constructors, source var {:?}",
            variant_var
        ),
        Unit | UnitWithArguments => {
            Stmt::Let(assigned, Expr::Struct(&[]), Layout::Struct(&[]), hole)
        }
        BoolUnion { ttrue, .. } => Stmt::Let(
            assigned,
            Expr::Literal(Literal::Bool(tag_name == ttrue)),
            Layout::Builtin(Builtin::Int1),
            hole,
        ),
        ByteUnion(tag_names) => {
            let opt_tag_id = tag_names.iter().position(|key| key == &tag_name);

            match opt_tag_id {
                Some(tag_id) => Stmt::Let(
                    assigned,
                    Expr::Literal(Literal::Byte(tag_id as u8)),
                    Layout::Builtin(Builtin::Int8),
                    hole,
                ),
                None => Stmt::RuntimeError("tag must be in its own type"),
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
                layout_cache.from_var(env.arena, variant_var, env.subs)
            ) {
                Layout::Union(ul) => ul,
                _ => unreachable!(),
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
                        tag_name,
                        tag_id: tag_id as _,
                        arguments: field_symbols,
                    };

                    (tag, union_layout)
                }
                NonNullableUnwrapped {
                    tag_name: wrapped_tag_name,
                    ..
                } => {
                    debug_assert_eq!(tag_name, wrapped_tag_name);

                    field_symbols = {
                        let mut temp = Vec::with_capacity_in(field_symbols_temp.len(), arena);

                        temp.extend(field_symbols_temp.iter().map(|r| r.1));

                        temp.into_bump_slice()
                    };

                    let tag = Expr::Tag {
                        tag_layout: union_layout,
                        tag_name,
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
                        tag_name,
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
                        tag_name,
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
                        tag_name,
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

        let loc_pattern = Located::at_zero(roc_can::pattern::Pattern::Identifier(arg_symbol));

        let loc_expr = Located::at_zero(roc_can::expr::Expr::Var(arg_symbol));

        loc_pattern_args.push((arg_var, loc_pattern));
        loc_expr_args.push((arg_var, loc_expr));
    }

    let loc_body = Located::at_zero(roc_can::expr::Expr::Tag {
        variant_var: return_variable,
        name: tag_name,
        arguments: loc_expr_args,
        ext_var,
    });

    let inserted = procs.insert_anonymous(
        env,
        proc_symbol,
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
                layout_cache.raw_from_var(env.arena, whole_var, env.subs)
            );

            match raw_layout {
                RawFunctionLayout::Function(_, lambda_set, _) => {
                    construct_closure_data(env, lambda_set, proc_symbol, &[], assigned, hole)
                }
                RawFunctionLayout::ZeroArgumentThunk(_) => unreachable!(),
            }
        }

        Err(runtime_error) => Stmt::RuntimeError(env.arena.alloc(format!(
            "RuntimeError {} line {} {:?}",
            file!(),
            line!(),
            runtime_error,
        ))),
    }
}

#[allow(clippy::type_complexity)]
fn sorted_field_symbols<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    mut args: std::vec::Vec<(Variable, Located<roc_can::expr::Expr>)>,
) -> Vec<
    'a,
    (
        u32,
        Symbol,
        ((Variable, Located<roc_can::expr::Expr>), &'a Symbol),
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
                Layout::Struct(&[])
            }
            Err(LayoutProblem::Erroneous) => {
                // something went very wrong
                panic!("TODO turn fn_var into a RuntimeError")
            }
        };

        let alignment = layout.alignment_bytes(env.ptr_bytes);

        let symbol = possible_reuse_symbol(env, procs, &arg.value);
        field_symbols_temp.push((alignment, symbol, ((var, arg), &*env.arena.alloc(symbol))));
    }
    field_symbols_temp.sort_by(|a, b| b.0.cmp(&a.0));

    field_symbols_temp
}

/// Insert a closure that does capture symbols (because it is top-level) to the list of partial procs
fn register_noncapturing_closure<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
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
        layout_cache,
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
            closure_ext_var,
            recursive,
            arguments,
            loc_body: boxed_body,
            captured_symbols,
            ..
        } = closure_data;
        let loc_body = *boxed_body;

        let is_self_recursive = !matches!(recursive, roc_can::expr::Recursive::NotRecursive);

        // does this function capture any local values?
        let function_layout = layout_cache.raw_from_var(env.arena, function_type, env.subs);

        let captured_symbols = match function_layout {
            Ok(RawFunctionLayout::Function(_, lambda_set, _)) => {
                if let Layout::Struct(&[]) = lambda_set.runtime_representation() {
                    CapturedSymbols::None
                } else {
                    let mut temp = Vec::from_iter_in(captured_symbols, env.arena);
                    temp.sort();
                    CapturedSymbols::Captured(temp.into_bump_slice())
                }
            }
            Ok(RawFunctionLayout::ZeroArgumentThunk(_)) => {
                // top-level thunks cannot capture any variables
                debug_assert!(
                    captured_symbols.is_empty(),
                    "{:?} with layout {:?} {:?} {:?}",
                    &captured_symbols,
                    function_layout,
                    env.subs,
                    (function_type, closure_type, closure_ext_var),
                );
                CapturedSymbols::None
            }
            Err(_) => {
                // just allow this. see https://github.com/rtfeldman/roc/issues/1585
                if captured_symbols.is_empty() {
                    CapturedSymbols::None
                } else {
                    let mut temp = Vec::from_iter_in(captured_symbols, env.arena);
                    temp.sort();
                    CapturedSymbols::Captured(temp.into_bump_slice())
                }
            }
        };

        let partial_proc = PartialProc::from_named_function(
            env,
            layout_cache,
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
            region,
            loc_cond,
            branches,
        } => {
            let cond_symbol = possible_reuse_symbol(env, procs, &loc_cond.value);

            let stmt = from_can_when(
                env,
                cond_var,
                expr_var,
                region,
                cond_symbol,
                branches,
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
                let branching_symbol = possible_reuse_symbol(env, procs, &loc_cond.value);
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

        Expect(condition, rest) => {
            let rest = from_can(env, variable, rest.value, procs, layout_cache);

            let bool_layout = Layout::Builtin(Builtin::Int1);
            let cond_symbol = env.unique_symbol();

            let op = LowLevel::ExpectTrue;
            let call_type = CallType::LowLevel {
                op,
                update_mode: env.next_update_mode_id(),
            };
            let arguments = env.arena.alloc([cond_symbol]);
            let call = self::Call {
                call_type,
                arguments,
            };

            let rest = Stmt::Let(
                env.unique_symbol(),
                Expr::Call(call),
                bool_layout,
                env.arena.alloc(rest),
            );

            with_hole(
                env,
                condition.value,
                variable,
                procs,
                layout_cache,
                cond_symbol,
                env.arena.alloc(rest),
            )
        }

        LetRec(defs, cont, _) => {
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
        LetNonRec(def, cont, outer_annotation) => {
            if let roc_can::pattern::Pattern::Identifier(symbol) = &def.loc_pattern.value {
                match def.loc_expr.value {
                    roc_can::expr::Expr::Closure(closure_data) => {
                        register_capturing_closure(env, procs, layout_cache, *symbol, closure_data);

                        return from_can(env, variable, cont.value, procs, layout_cache);
                    }
                    roc_can::expr::Expr::Var(original) => {
                        // a variable is aliased, e.g.
                        //
                        //  foo = bar
                        //
                        // or
                        //
                        //  foo = RBTRee.empty
                        let mut rest = from_can(env, def.expr_var, cont.value, procs, layout_cache);

                        rest = handle_variable_aliasing(
                            env,
                            procs,
                            layout_cache,
                            def.expr_var,
                            *symbol,
                            original,
                            rest,
                        );

                        return rest;
                    }
                    roc_can::expr::Expr::LetNonRec(nested_def, nested_cont, nested_annotation) => {
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

                        let new_inner = LetNonRec(Box::new(new_def), cont, outer_annotation);

                        let new_outer = LetNonRec(
                            nested_def,
                            Box::new(Located::at_zero(new_inner)),
                            nested_annotation,
                        );

                        return from_can(env, variable, new_outer, procs, layout_cache);
                    }
                    roc_can::expr::Expr::LetRec(nested_defs, nested_cont, nested_annotation) => {
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

                        let new_inner = LetNonRec(Box::new(new_def), cont, outer_annotation);

                        let new_outer = LetRec(
                            nested_defs,
                            Box::new(Located::at_zero(new_inner)),
                            nested_annotation,
                        );

                        return from_can(env, variable, new_outer, procs, layout_cache);
                    }
                    _ => {
                        let rest = from_can(env, variable, cont.value, procs, layout_cache);
                        return with_hole(
                            env,
                            def.loc_expr.value,
                            def.expr_var,
                            procs,
                            layout_cache,
                            *symbol,
                            env.arena.alloc(rest),
                        );
                    }
                }
            }

            // this may be a destructure pattern
            let (mono_pattern, assignments) =
                match from_can_pattern(env, layout_cache, &def.loc_pattern.value) {
                    Ok(v) => v,
                    Err(_) => todo!(),
                };

            if let Pattern::Identifier(symbol) = mono_pattern {
                let mut hole =
                    env.arena
                        .alloc(from_can(env, variable, cont.value, procs, layout_cache));

                for (symbol, variable, expr) in assignments {
                    let stmt = with_hole(env, expr, variable, procs, layout_cache, symbol, hole);

                    hole = env.arena.alloc(stmt);
                }

                with_hole(
                    env,
                    def.loc_expr.value,
                    def.expr_var,
                    procs,
                    layout_cache,
                    symbol,
                    hole,
                )
            } else {
                let context = crate::exhaustive::Context::BadDestruct;
                match crate::exhaustive::check(
                    def.loc_pattern.region,
                    &[(
                        Located::at(def.loc_pattern.region, mono_pattern.clone()),
                        crate::exhaustive::Guard::NoGuard,
                    )],
                    context,
                ) {
                    Ok(_) => {}
                    Err(errors) => {
                        for error in errors {
                            env.problems.push(MonoProblem::PatternProblem(error))
                        }

                        return Stmt::RuntimeError("TODO non-exhaustive pattern");
                    }
                }

                // convert the continuation
                let mut stmt = from_can(env, variable, cont.value, procs, layout_cache);

                // layer on any default record fields
                for (symbol, variable, expr) in assignments {
                    let hole = env.arena.alloc(stmt);
                    stmt = with_hole(env, expr, variable, procs, layout_cache, symbol, hole);
                }

                if let roc_can::expr::Expr::Var(outer_symbol) = def.loc_expr.value {
                    store_pattern(env, procs, layout_cache, &mono_pattern, outer_symbol, stmt)
                } else {
                    let outer_symbol = env.unique_symbol();
                    stmt =
                        store_pattern(env, procs, layout_cache, &mono_pattern, outer_symbol, stmt);

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

        _ => {
            let symbol = env.unique_symbol();
            let hole = env.arena.alloc(Stmt::Ret(symbol));
            with_hole(env, can_expr, variable, procs, layout_cache, symbol, hole)
        }
    }
}

fn to_opt_branches<'a>(
    env: &mut Env<'a, '_>,
    region: Region,
    branches: std::vec::Vec<roc_can::expr::WhenBranch>,
    layout_cache: &mut LayoutCache<'a>,
) -> std::vec::Vec<(
    Pattern<'a>,
    Option<Located<roc_can::expr::Expr>>,
    roc_can::expr::Expr,
)> {
    debug_assert!(!branches.is_empty());

    let mut loc_branches = std::vec::Vec::new();
    let mut opt_branches = std::vec::Vec::new();

    for when_branch in branches {
        let exhaustive_guard = if when_branch.guard.is_some() {
            Guard::HasGuard
        } else {
            Guard::NoGuard
        };

        for loc_pattern in when_branch.patterns {
            match from_can_pattern(env, layout_cache, &loc_pattern.value) {
                Ok((mono_pattern, assignments)) => {
                    loc_branches.push((
                        Located::at(loc_pattern.region, mono_pattern.clone()),
                        exhaustive_guard.clone(),
                    ));

                    let mut loc_expr = when_branch.value.clone();
                    let region = loc_pattern.region;
                    for (symbol, variable, expr) in assignments.into_iter().rev() {
                        let def = roc_can::def::Def {
                            annotation: None,
                            expr_var: variable,
                            loc_expr: Located::at(region, expr),
                            loc_pattern: Located::at(
                                region,
                                roc_can::pattern::Pattern::Identifier(symbol),
                            ),
                            pattern_vars: std::iter::once((symbol, variable)).collect(),
                        };
                        let new_expr = roc_can::expr::Expr::LetNonRec(
                            Box::new(def),
                            Box::new(loc_expr),
                            variable,
                        );
                        loc_expr = Located::at(region, new_expr);
                    }

                    // TODO remove clone?
                    opt_branches.push((mono_pattern, when_branch.guard.clone(), loc_expr.value));
                }
                Err(runtime_error) => {
                    loc_branches.push((
                        Located::at(loc_pattern.region, Pattern::Underscore),
                        exhaustive_guard.clone(),
                    ));

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

    // NOTE exhaustiveness is checked after the construction of all the branches
    // In contrast to elm (currently), we still do codegen even if a pattern is non-exhaustive.
    // So we not only report exhaustiveness errors, but also correct them
    let context = crate::exhaustive::Context::BadCase;
    match crate::exhaustive::check(region, &loc_branches, context) {
        Ok(_) => {}
        Err(errors) => {
            use crate::exhaustive::Error::*;
            let mut is_not_exhaustive = false;
            let mut overlapping_branches = std::vec::Vec::new();

            for error in errors {
                match &error {
                    Incomplete(_, _, _) => {
                        is_not_exhaustive = true;
                    }
                    Redundant { index, .. } => {
                        overlapping_branches.push(index.to_zero_based());
                    }
                }
                env.problems.push(MonoProblem::PatternProblem(error))
            }

            overlapping_branches.sort_unstable();

            for i in overlapping_branches.into_iter().rev() {
                opt_branches.remove(i);
            }

            if is_not_exhaustive {
                opt_branches.push((
                    Pattern::Underscore,
                    None,
                    roc_can::expr::Expr::RuntimeError(
                        roc_problem::can::RuntimeError::NonExhaustivePattern,
                    ),
                ));
            }
        }
    }

    opt_branches
}

#[allow(clippy::too_many_arguments)]
fn from_can_when<'a>(
    env: &mut Env<'a, '_>,
    cond_var: Variable,
    expr_var: Variable,
    region: Region,
    cond_symbol: Symbol,
    branches: std::vec::Vec<roc_can::expr::WhenBranch>,
    layout_cache: &mut LayoutCache<'a>,
    procs: &mut Procs<'a>,
    join_point: Option<JoinPointId>,
) -> Stmt<'a> {
    if branches.is_empty() {
        // A when-expression with no branches is a runtime error.
        // We can't know what to return!
        return Stmt::RuntimeError("Hit a 0-branch when expression");
    }
    let opt_branches = to_opt_branches(env, region, branches, layout_cache);

    let cond_layout =
        return_on_layout_error!(env, layout_cache.from_var(env.arena, cond_var, env.subs));

    let ret_layout =
        return_on_layout_error!(env, layout_cache.from_var(env.arena, expr_var, env.subs));

    let arena = env.arena;
    let it = opt_branches
        .into_iter()
        .map(|(pattern, opt_guard, can_expr)| {
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
            if let Some(loc_expr) = opt_guard {
                let id = JoinPointId(env.unique_symbol());
                let symbol = env.unique_symbol();
                let jump = env.arena.alloc(Stmt::Jump(id, env.arena.alloc([symbol])));

                let guard_stmt = with_hole(
                    env,
                    loc_expr.value,
                    cond_var,
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
                let continuation = opt_continuation.unwrap_or_else(|| *continuation);

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

        RuntimeError(_) => None,
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
        } => substitute(subs, *name).map(|new| CallType::ByName {
            name: new,
            arg_layouts,
            ret_layout: *ret_layout,
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
            tag_name,
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
                    tag_name: tag_name.clone(),
                    tag_id: *tag_id,
                    arguments,
                })
            } else {
                None
            }
        }

        Reuse { .. } | Reset(_) => unreachable!("reset/reuse have not been introduced yet"),

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

        StructAtIndex {
            index,
            structure,
            field_layouts,
        } => match substitute(subs, *structure) {
            Some(structure) => Some(StructAtIndex {
                index: *index,
                field_layouts: *field_layouts,
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
            substitute_in_exprs(env.arena, &mut stmt, *symbol, outer_symbol);
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
            [single] => {
                return store_pattern_help(env, procs, layout_cache, &single.0, outer_symbol, stmt);
            }
            _ => {
                let mut fields = Vec::with_capacity_in(arguments.len(), env.arena);
                fields.extend(arguments.iter().map(|x| x.1));

                let layout = Layout::Struct(fields.into_bump_slice());

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
        RecordDestructure(destructs, [_single_field]) => {
            for destruct in destructs {
                match &destruct.typ {
                    DestructType::Required(symbol) => {
                        substitute_in_exprs(env.arena, &mut stmt, *symbol, outer_symbol);
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
                // store immediately in the given symbol
                stmt = Stmt::Let(*symbol, load, arg_layout, env.arena.alloc(stmt));
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
                // store immediately in the given symbol
                stmt = Stmt::Let(*symbol, load, arg_layout, env.arena.alloc(stmt));
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
            stmt = Stmt::Let(*symbol, load, destruct.layout, env.arena.alloc(stmt));
        }
        DestructType::Guard(guard_pattern) => match &guard_pattern {
            Identifier(symbol) => {
                stmt = Stmt::Let(*symbol, load, destruct.layout, env.arena.alloc(stmt));
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

enum ReuseSymbol {
    Imported(Symbol),
    LocalFunction(Symbol),
    Value(Symbol),
    NotASymbol,
}

fn can_reuse_symbol<'a>(
    env: &mut Env<'a, '_>,
    procs: &Procs<'a>,
    expr: &roc_can::expr::Expr,
) -> ReuseSymbol {
    use ReuseSymbol::*;

    if let roc_can::expr::Expr::Var(symbol) = expr {
        let symbol = *symbol;

        if env.is_imported_symbol(symbol) {
            Imported(symbol)
        } else if procs.partial_procs.contains_key(symbol) {
            LocalFunction(symbol)
        } else {
            Value(symbol)
        }
    } else {
        NotASymbol
    }
}

fn possible_reuse_symbol<'a>(
    env: &mut Env<'a, '_>,
    procs: &Procs<'a>,
    expr: &roc_can::expr::Expr,
) -> Symbol {
    match can_reuse_symbol(env, procs, expr) {
        ReuseSymbol::Value(s) => s,
        _ => env.unique_symbol(),
    }
}

fn handle_variable_aliasing<'a>(
    env: &mut Env<'a, '_>,
    procs: &mut Procs<'a>,
    layout_cache: &mut LayoutCache<'a>,
    variable: Variable,
    left: Symbol,
    right: Symbol,
    mut result: Stmt<'a>,
) -> Stmt<'a> {
    if env.is_imported_symbol(right) {
        // if this is an imported symbol, then we must make sure it is
        // specialized, and wrap the original in a function pointer.
        add_needed_external(procs, env, variable, right);

        // then we must construct its closure; since imported symbols have no closure, we use the
        // empty struct

        let_empty_struct(left, env.arena.alloc(result))
    } else {
        substitute_in_exprs(env.arena, &mut result, left, right);

        // if the substituted variable is a function, make sure we specialize it
        reuse_function_symbol(
            env,
            procs,
            layout_cache,
            Some(variable),
            right,
            result,
            right,
        )
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
            name: thunk_name,
            ret_layout: env.arena.alloc(layout),
            arg_layouts: &[],
            specialization_id: env.next_call_specialization_id(),
        },
        arguments: &[],
    };

    build_call(env, call, assigned, layout, env.arena.alloc(hole))
}

fn let_empty_struct<'a>(assigned: Symbol, hole: &'a Stmt<'a>) -> Stmt<'a> {
    Stmt::Let(assigned, Expr::Struct(&[]), Layout::Struct(&[]), hole)
}

/// If the symbol is a function, make sure it is properly specialized
fn reuse_function_symbol<'a>(
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
                Some(arg_var) if env.is_imported_symbol(original) => {
                    let raw = layout_cache
                        .raw_from_var(env.arena, arg_var, env.subs)
                        .expect("creating layout does not fail");

                    if procs.is_imported_module_thunk(original) {
                        let layout = match raw {
                            RawFunctionLayout::ZeroArgumentThunk(layout) => layout,
                            RawFunctionLayout::Function(_, lambda_set, _) => {
                                Layout::LambdaSet(lambda_set)
                            }
                        };

                        let raw = RawFunctionLayout::ZeroArgumentThunk(layout);
                        let top_level = ProcLayout::from_raw(env.arena, raw);

                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            original,
                            top_level,
                            layout_cache,
                        );

                        force_thunk(env, original, layout, symbol, env.arena.alloc(result))
                    } else {
                        let top_level = ProcLayout::from_raw(env.arena, raw);
                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            original,
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
                layout_cache.raw_from_var(env.arena, arg_var, env.subs)
            );

            // we have three kinds of functions really. Plain functions, closures by capture,
            // and closures by unification. Here we record whether this function captures
            // anything.
            let captures = partial_proc.captured_symbols.captures();
            let captured = partial_proc.captured_symbols;

            match res_layout {
                RawFunctionLayout::Function(_, lambda_set, _) => {
                    // define the function pointer
                    let function_ptr_layout = ProcLayout::from_raw(env.arena, res_layout);

                    if captures {
                        // this is a closure by capture, meaning it itself captures local variables.
                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            original,
                            function_ptr_layout,
                            layout_cache,
                        );

                        let closure_data = symbol;

                        let symbols = match captured {
                            CapturedSymbols::Captured(captured_symbols) => {
                                Vec::from_iter_in(captured_symbols.iter().map(|x| x.0), env.arena)
                                    .into_bump_slice()
                            }
                            CapturedSymbols::None => unreachable!(),
                        };

                        construct_closure_data(
                            env,
                            lambda_set,
                            original,
                            symbols,
                            closure_data,
                            env.arena.alloc(result),
                        )
                    } else if procs.is_module_thunk(original) {
                        // this is a 0-argument thunk

                        // TODO suspicious
                        // let layout = Layout::Closure(argument_layouts, lambda_set, ret_layout);
                        // panic!("suspicious");
                        let layout = Layout::LambdaSet(lambda_set);
                        let top_level = ProcLayout::new(env.arena, &[], layout);
                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            original,
                            top_level,
                            layout_cache,
                        );

                        force_thunk(env, original, layout, symbol, env.arena.alloc(result))
                    } else {
                        procs.insert_passed_by_name(
                            env,
                            arg_var,
                            original,
                            function_ptr_layout,
                            layout_cache,
                        );

                        // even though this function may not itself capture,
                        // unification may still cause it to have an extra argument
                        construct_closure_data(
                            env,
                            lambda_set,
                            original,
                            &[],
                            symbol,
                            env.arena.alloc(result),
                        )
                    }
                }
                RawFunctionLayout::ZeroArgumentThunk(ret_layout) => {
                    // this is a 0-argument thunk
                    let top_level = ProcLayout::new(env.arena, &[], ret_layout);
                    procs.insert_passed_by_name(env, arg_var, original, top_level, layout_cache);

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
    loc_arg: Located<roc_can::expr::Expr>,
    symbol: Symbol,
    result: Stmt<'a>,
) -> Stmt<'a> {
    use ReuseSymbol::*;
    match can_reuse_symbol(env, procs, &loc_arg.value) {
        Imported(original) | LocalFunction(original) => {
            // for functions we must make sure they are specialized correctly
            reuse_function_symbol(
                env,
                procs,
                layout_cache,
                Some(arg_var),
                symbol,
                result,
                original,
            )
        }
        Value(_) => {
            // symbol is already defined; nothing else to do here
            result
        }
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
    I: Iterator<Item = ((Variable, Located<roc_can::expr::Expr>), &'a Symbol)>,
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
    name: Symbol,
) {
    // call of a function that is not in this module
    use hashbrown::hash_map::Entry::{Occupied, Vacant};

    let existing = match procs.externals_we_need.entry(name.module_id()) {
        Vacant(entry) => entry.insert(ExternalSpecializations::new_in(env.arena)),
        Occupied(entry) => entry.into_mut(),
    };

    let solved_type = SolvedType::from_var(env.subs, fn_var);
    existing.insert(name, solved_type);
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

/// See https://github.com/rtfeldman/roc/issues/1549
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
    loc_args: std::vec::Vec<(Variable, Located<roc_can::expr::Expr>)>,
) -> Stmt<'a> {
    let arena = env.arena;

    // eventually we will throw this runtime error
    let result = Stmt::RuntimeError(env.arena.alloc(msg));

    // but, we also still evaluate and specialize the arguments to give better error messages
    let arg_symbols = Vec::from_iter_in(
        loc_args
            .iter()
            .map(|(_, arg_expr)| possible_reuse_symbol(env, procs, &arg_expr.value)),
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
    loc_args: std::vec::Vec<(Variable, Located<roc_can::expr::Expr>)>,
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
                        loc_args.iter().map(|(_, arg_expr)| {
                            possible_reuse_symbol(env, procs, &arg_expr.value)
                        }),
                        arena,
                    )
                    .into_bump_slice();

                    debug_assert_eq!(arg_symbols.len(), arg_layouts.len());

                    let result = match_on_lambda_set(
                        env,
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
                add_needed_external(procs, env, fn_var, proc_name);
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
    loc_args: std::vec::Vec<(Variable, Located<roc_can::expr::Expr>)>,
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
    field_symbols.extend(
        loc_args
            .iter()
            .map(|(_, arg_expr)| possible_reuse_symbol(env, procs, &arg_expr.value)),
    );

    // If required, add an extra argument to the layout that is the captured environment
    // afterwards, we MUST make sure the number of arguments in the layout matches the
    // number of arguments actually passed.
    let top_level_layout = {
        let argument_layouts = lambda_set.extend_argument_list(env.arena, argument_layouts);
        ProcLayout::new(env.arena, argument_layouts, *ret_layout)
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
                return Stmt::RuntimeError("TODO runtime error for invalid layout");
            }
        }
    }

    // If we've already specialized this one, no further work is needed.
    if procs
        .specialized
        .keys()
        .any(|x| x == &(proc_name, top_level_layout))
    {
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
    } else if env.is_imported_symbol(proc_name) {
        add_needed_external(procs, env, original_fn_var, proc_name);

        debug_assert_ne!(proc_name.module_id(), ModuleId::ATTR);
        if procs.is_imported_module_thunk(proc_name) {
            force_thunk(
                env,
                proc_name,
                Layout::LambdaSet(lambda_set),
                assigned,
                hole,
            )
        } else {
            debug_assert!(
                !field_symbols.is_empty(),
                "should be in the list of imported_module_thunks"
            );

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
        if procs.is_module_thunk(proc_name) {
            debug_assert!(top_level_layout.arguments.is_empty());
        }

        match &mut procs.pending_specializations {
            Some(pending_specializations) => {
                debug_assert!(!env.is_imported_symbol(proc_name));

                // register the pending specialization, so this gets code genned later
                let pending = PendingSpecialization::from_var(env.arena, env.subs, fn_var);
                add_pending(
                    pending_specializations,
                    proc_name,
                    top_level_layout,
                    pending,
                );

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
            None => {
                let opt_partial_proc = procs.partial_procs.symbol_to_id(proc_name);

                let field_symbols = field_symbols.into_bump_slice();

                match opt_partial_proc {
                    Some(partial_proc) => {
                        // Mark this proc as in-progress, so if we're dealing with
                        // mutually recursive functions, we don't loop forever.
                        // (We had a bug around this before this system existed!)
                        procs
                            .specialized
                            .insert((proc_name, top_level_layout), InProgress);

                        match specialize_variable(
                            env,
                            procs,
                            proc_name,
                            layout_cache,
                            fn_var,
                            BumpMap::new_in(env.arena),
                            partial_proc,
                        ) {
                            Ok((proc, layout)) => {
                                // now we just call our freshly-specialized function
                                call_specialized_proc(
                                    env,
                                    procs,
                                    proc_name,
                                    proc,
                                    lambda_set,
                                    layout,
                                    field_symbols,
                                    loc_args,
                                    layout_cache,
                                    assigned,
                                    hole,
                                )
                            }
                            Err(SpecializeFailure {
                                attempted_layout,
                                problem: _,
                            }) => {
                                let proc = generate_runtime_error_function(
                                    env,
                                    proc_name,
                                    attempted_layout,
                                );

                                call_specialized_proc(
                                    env,
                                    procs,
                                    proc_name,
                                    proc,
                                    lambda_set,
                                    attempted_layout,
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
    debug_assert!(!env.is_imported_symbol(proc_name));

    let top_level_layout = ProcLayout::new(env.arena, &[], *ret_layout);

    let inner_layout = *ret_layout;

    // If we've already specialized this one, no further work is needed.
    let already_specialized = procs
        .specialized
        .contains_key(&(proc_name, top_level_layout));

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

        match &mut procs.pending_specializations {
            Some(pending_specializations) => {
                debug_assert!(!env.is_imported_symbol(proc_name));

                // register the pending specialization, so this gets code genned later
                let pending = PendingSpecialization::from_var(env.arena, env.subs, fn_var);
                add_pending(
                    pending_specializations,
                    proc_name,
                    top_level_layout,
                    pending,
                );

                force_thunk(env, proc_name, inner_layout, assigned, hole)
            }
            None => {
                let opt_partial_proc = procs.partial_procs.symbol_to_id(proc_name);

                match opt_partial_proc {
                    Some(partial_proc) => {
                        // Mark this proc as in-progress, so if we're dealing with
                        // mutually recursive functions, we don't loop forever.
                        // (We had a bug around this before this system existed!)
                        procs
                            .specialized
                            .insert((proc_name, top_level_layout), InProgress);

                        match specialize_variable(
                            env,
                            procs,
                            proc_name,
                            layout_cache,
                            fn_var,
                            BumpMap::new_in(env.arena),
                            partial_proc,
                        ) {
                            Ok((proc, raw_layout)) => {
                                debug_assert!(
                                    raw_layout.is_zero_argument_thunk(),
                                    "but actually {:?}",
                                    raw_layout
                                );

                                let was_present =
                                    procs.specialized.remove(&(proc_name, top_level_layout));
                                debug_assert!(was_present.is_some());

                                procs
                                    .specialized
                                    .insert((proc_name, top_level_layout), Done(proc));

                                force_thunk(env, proc_name, inner_layout, assigned, hole)
                            }
                            Err(SpecializeFailure {
                                attempted_layout,
                                problem: _,
                            }) => {
                                let proc = generate_runtime_error_function(
                                    env,
                                    proc_name,
                                    attempted_layout,
                                );

                                let was_present =
                                    procs.specialized.remove(&(proc_name, top_level_layout));
                                debug_assert!(was_present.is_some());

                                procs
                                    .specialized
                                    .insert((proc_name, top_level_layout), Done(proc));

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
    proc_name: Symbol,
    proc: Proc<'a>,
    lambda_set: LambdaSet<'a>,
    layout: RawFunctionLayout<'a>,
    field_symbols: &'a [Symbol],
    loc_args: std::vec::Vec<(Variable, Located<roc_can::expr::Expr>)>,
    layout_cache: &mut LayoutCache<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let function_layout = ProcLayout::from_raw(env.arena, layout);

    procs.specialized.remove(&(proc_name, function_layout));

    procs
        .specialized
        .insert((proc_name, function_layout), Done(proc));

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
            .get_symbol(proc_name)
            .map(|pp| &pp.captured_symbols)
        {
            Some(&CapturedSymbols::Captured(captured_symbols)) => {
                let symbols = Vec::from_iter_in(captured_symbols.iter().map(|x| x.0), env.arena)
                    .into_bump_slice();

                let closure_data_symbol = env.unique_symbol();

                // the closure argument is already added here (to get the right specialization)
                // but now we need to remove it because the `match_on_lambda_set` will add it again
                let mut argument_layouts =
                    Vec::from_iter_in(function_layout.arguments.iter().copied(), env.arena);
                argument_layouts.pop().unwrap();

                debug_assert_eq!(argument_layouts.len(), field_symbols.len(),);

                let new_hole = match_on_lambda_set(
                    env,
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
                    lambda_set,
                    proc_name,
                    symbols,
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
    IntLiteral(i128, IntPrecision),
    FloatLiteral(u64, FloatPrecision),
    DecimalLiteral(RocDec),
    BitLiteral {
        value: bool,
        tag_name: TagName,
        union: crate::exhaustive::Union,
    },
    EnumLiteral {
        tag_id: u8,
        tag_name: TagName,
        union: crate::exhaustive::Union,
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
        union: crate::exhaustive::Union,
    },
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
    let pattern = from_can_pattern_help(env, layout_cache, can_pattern, &mut assignments)?;

    Ok((pattern, assignments))
}

fn from_can_pattern_help<'a>(
    env: &mut Env<'a, '_>,
    layout_cache: &mut LayoutCache<'a>,
    can_pattern: &roc_can::pattern::Pattern,
    assignments: &mut Vec<'a, (Symbol, Variable, roc_can::expr::Expr)>,
) -> Result<Pattern<'a>, RuntimeError> {
    use roc_can::pattern::Pattern::*;

    match can_pattern {
        Underscore => Ok(Pattern::Underscore),
        Identifier(symbol) => Ok(Pattern::Identifier(*symbol)),
        IntLiteral(var, _, int) => {
            match num_argument_to_int_or_float(env.subs, env.ptr_bytes, *var, false) {
                IntOrFloat::SignedIntType(precision) | IntOrFloat::UnsignedIntType(precision) => {
                    Ok(Pattern::IntLiteral(*int as i128, precision))
                }
                other => {
                    panic!(
                        "Invalid precision for int pattern: {:?} has {:?}",
                        can_pattern, other
                    )
                }
            }
        }
        FloatLiteral(var, float_str, float) => {
            // TODO: Can I reuse num_argument_to_int_or_float here if I pass in true?
            match num_argument_to_int_or_float(env.subs, env.ptr_bytes, *var, true) {
                IntOrFloat::SignedIntType(_) | IntOrFloat::UnsignedIntType(_) => {
                    panic!("Invalid precision for float pattern {:?}", var)
                }
                IntOrFloat::BinaryFloatType(precision) => {
                    Ok(Pattern::FloatLiteral(f64::to_bits(*float), precision))
                }
                IntOrFloat::DecimalFloatType => {
                    let dec = match RocDec::from_str(float_str) {
                        Some(d) => d,
                        None => panic!(
                            r"Invalid decimal for float literal = {}. TODO: Make this a nice, user-friendly error message",
                            float_str
                        ),
                    };
                    Ok(Pattern::DecimalLiteral(dec))
                }
            }
        }
        StrLiteral(v) => Ok(Pattern::StrLiteral(v.clone())),
        Shadowed(region, ident) => Err(RuntimeError::Shadowing {
            original_region: *region,
            shadow: ident.clone(),
        }),
        UnsupportedPattern(region) => Err(RuntimeError::UnsupportedPattern(*region)),
        MalformedPattern(_problem, region) => {
            // TODO preserve malformed problem information here?
            Err(RuntimeError::UnsupportedPattern(*region))
        }
        NumLiteral(var, num_str, num) => {
            match num_argument_to_int_or_float(env.subs, env.ptr_bytes, *var, false) {
                IntOrFloat::SignedIntType(precision) => {
                    Ok(Pattern::IntLiteral(*num as i128, precision))
                }
                IntOrFloat::UnsignedIntType(precision) => {
                    Ok(Pattern::IntLiteral(*num as i128, precision))
                }
                IntOrFloat::BinaryFloatType(precision) => {
                    Ok(Pattern::FloatLiteral(*num as u64, precision))
                }
                IntOrFloat::DecimalFloatType => {
                    let dec = match RocDec::from_str(num_str) {
                            Some(d) => d,
                            None => panic!("Invalid decimal for float literal = {}. TODO: Make this a nice, user-friendly error message", num_str),
                        };
                    Ok(Pattern::DecimalLiteral(dec))
                }
            }
        }

        AppliedTag {
            whole_var,
            tag_name,
            arguments,
            ..
        } => {
            use crate::exhaustive::Union;
            use crate::layout::UnionVariant::*;

            let res_variant =
                crate::layout::union_sorted_tags(env.arena, *whole_var, env.subs, env.ptr_bytes);

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
                Unit | UnitWithArguments => Pattern::EnumLiteral {
                    tag_id: 0,
                    tag_name: tag_name.clone(),
                    union: Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![Ctor {
                            tag_id: TagId(0),
                            name: tag_name.clone(),
                            arity: 0,
                        }],
                    },
                },
                BoolUnion { ttrue, ffalse } => Pattern::BitLiteral {
                    value: tag_name == &ttrue,
                    tag_name: tag_name.clone(),
                    union: Union {
                        render_as: RenderAs::Tag,
                        alternatives: vec![
                            Ctor {
                                tag_id: TagId(0),
                                name: ffalse,
                                arity: 0,
                            },
                            Ctor {
                                tag_id: TagId(1),
                                name: ttrue,
                                arity: 0,
                            },
                        ],
                    },
                },
                ByteUnion(tag_names) => {
                    let tag_id = tag_names
                        .iter()
                        .position(|key| key == tag_name)
                        .expect("tag must be in its own type");

                    let mut ctors = std::vec::Vec::with_capacity(tag_names.len());
                    for (i, tag_name) in tag_names.into_iter().enumerate() {
                        ctors.push(Ctor {
                            tag_id: TagId(i as _),
                            name: tag_name,
                            arity: 0,
                        })
                    }

                    let union = crate::exhaustive::Union {
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
                            .map(|x| x.alignment_bytes(env.ptr_bytes))
                            .unwrap_or(0);

                        let size2 = layout_cache
                            .from_var(env.arena, arg2.0, env.subs)
                            .map(|x| x.alignment_bytes(env.ptr_bytes))
                            .unwrap_or(0);

                        size2.cmp(&size1)
                    });

                    let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);
                    for ((_, loc_pat), layout) in arguments.iter().zip(field_layouts.iter()) {
                        mono_args.push((
                            from_can_pattern_help(env, layout_cache, &loc_pat.value, assignments)?,
                            *layout,
                        ));
                    }

                    Pattern::NewtypeDestructure {
                        tag_name: tag_name.clone(),
                        arguments: mono_args,
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

                            let size1 = layout1.alignment_bytes(env.ptr_bytes);
                            let size2 = layout2.alignment_bytes(env.ptr_bytes);

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
                                    name: tag_name.clone(),
                                    arity: args.len(),
                                })
                            }

                            let union = crate::exhaustive::Union {
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
                                    name: tag_name.clone(),
                                    // don't include tag discriminant in arity
                                    arity: args.len() - 1,
                                })
                            }

                            let union = crate::exhaustive::Union {
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
                            debug_assert_eq!(&w_tag_name, tag_name);

                            ctors.push(Ctor {
                                tag_id: TagId(0),
                                name: tag_name.clone(),
                                arity: fields.len(),
                            });

                            let union = crate::exhaustive::Union {
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
                                        name: nullable_name.clone(),
                                        // don't include tag discriminant in arity
                                        arity: 0,
                                    });

                                    i += 1;
                                }

                                ctors.push(Ctor {
                                    tag_id: TagId(i as _),
                                    name: tag_name.clone(),
                                    // don't include tag discriminant in arity
                                    arity: args.len() - 1,
                                });

                                i += 1;
                            }

                            if i == nullable_id as usize {
                                ctors.push(Ctor {
                                    tag_id: TagId(i as _),
                                    name: nullable_name.clone(),
                                    // don't include tag discriminant in arity
                                    arity: 0,
                                });
                            }

                            let union = crate::exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            let it = if tag_name == &nullable_name {
                                [].iter()
                            } else {
                                argument_layouts.iter()
                            };

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
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
                                name: nullable_name.clone(),
                                arity: 0,
                            });

                            ctors.push(Ctor {
                                tag_id: TagId(!nullable_id as _),
                                name: nullable_name.clone(),
                                // FIXME drop tag
                                arity: other_fields.len() - 1,
                            });

                            let union = crate::exhaustive::Union {
                                render_as: RenderAs::Tag,
                                alternatives: ctors,
                            };

                            let mut mono_args = Vec::with_capacity_in(arguments.len(), env.arena);

                            let it = if tag_name == &nullable_name {
                                [].iter()
                            } else {
                                // FIXME drop tag
                                argument_layouts.iter()
                            };

                            for ((_, loc_pat), layout) in arguments.iter().zip(it) {
                                mono_args.push((
                                    from_can_pattern_help(
                                        env,
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

        RecordDestructure {
            whole_var,
            destructs,
            ..
        } => {
            // sorted fields based on the type
            let sorted_fields =
                crate::layout::sort_record_fields(env.arena, *whole_var, env.subs, env.ptr_bytes);

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
    }
}

fn from_can_record_destruct<'a>(
    env: &mut Env<'a, '_>,
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
                from_can_pattern_help(env, layout_cache, &loc_pattern.value, assignments)?,
            ),
        },
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum IntPrecision {
    Usize,
    I128,
    I64,
    I32,
    I16,
    I8,
}

impl IntPrecision {
    pub fn as_layout(&self) -> Layout<'static> {
        Layout::Builtin(self.as_builtin())
    }

    pub fn as_builtin(&self) -> Builtin<'static> {
        use IntPrecision::*;
        match self {
            I128 => Builtin::Int128,
            I64 => Builtin::Int64,
            I32 => Builtin::Int32,
            I16 => Builtin::Int16,
            I8 => Builtin::Int8,
            Usize => Builtin::Usize,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub enum FloatPrecision {
    F64,
    F32,
}

impl FloatPrecision {
    pub fn as_layout(&self) -> Layout<'static> {
        Layout::Builtin(self.as_builtin())
    }

    pub fn as_builtin(&self) -> Builtin<'static> {
        use FloatPrecision::*;
        match self {
            F64 => Builtin::Float64,
            F32 => Builtin::Float32,
        }
    }
}

#[derive(Debug)]
pub enum IntOrFloat {
    SignedIntType(IntPrecision),
    UnsignedIntType(IntPrecision),
    BinaryFloatType(FloatPrecision),
    DecimalFloatType,
}

/// Given the `a` in `Num a`, determines whether it's an int or a float
pub fn num_argument_to_int_or_float(
    subs: &Subs,
    ptr_bytes: u32,
    var: Variable,
    known_to_be_float: bool,
) -> IntOrFloat {
    match subs.get_content_without_compacting(var){
        Content::FlexVar(_) | Content::RigidVar(_) if known_to_be_float => IntOrFloat::BinaryFloatType(FloatPrecision::F64),
        Content::FlexVar(_) | Content::RigidVar(_) => IntOrFloat::SignedIntType(IntPrecision::I64), // We default (Num *) to I64

        Content::Alias(Symbol::NUM_INTEGER, args, _)  => {
            debug_assert!(args.len() == 1);

            // Recurse on the second argument
            let var = subs[args.variables().into_iter().next().unwrap()];
            num_argument_to_int_or_float(subs, ptr_bytes, var, false)
        }

        Content::Alias(Symbol::NUM_I128,  _, _)
        | Content::Alias(Symbol::NUM_SIGNED128, _,  _)
        | Content::Alias(Symbol::NUM_AT_SIGNED128, _, _) => {
            IntOrFloat::SignedIntType(IntPrecision::I128)
        }
        Content::Alias(Symbol::NUM_INT,  _, _)// We default Integer to I64
        | Content::Alias(Symbol::NUM_I64,  _, _)
        | Content::Alias(Symbol::NUM_SIGNED64,  _, _)
        | Content::Alias(Symbol::NUM_AT_SIGNED64, _, _) => {
            IntOrFloat::SignedIntType(IntPrecision::I64)
        }
        Content::Alias(Symbol::NUM_I32, _, _)
        | Content::Alias(Symbol::NUM_SIGNED32, _, _)
        | Content::Alias(Symbol::NUM_AT_SIGNED32, _, _) => {
            IntOrFloat::SignedIntType(IntPrecision::I32)
        }
        Content::Alias(Symbol::NUM_I16, _, _)
        | Content::Alias(Symbol::NUM_SIGNED16, _, _)
        | Content::Alias(Symbol::NUM_AT_SIGNED16, _, _) => {
            IntOrFloat::SignedIntType(IntPrecision::I16)
        }
        Content::Alias(Symbol::NUM_I8, _, _)
        | Content::Alias(Symbol::NUM_SIGNED8, _, _)
        | Content::Alias(Symbol::NUM_AT_SIGNED8, _, _) => {
            IntOrFloat::SignedIntType(IntPrecision::I8)
        }
        Content::Alias(Symbol::NUM_U128, _, _)
        | Content::Alias(Symbol::NUM_UNSIGNED128, _, _)
        | Content::Alias(Symbol::NUM_AT_UNSIGNED128, _, _) => {
            IntOrFloat::UnsignedIntType(IntPrecision::I128)
        }
        Content::Alias(Symbol::NUM_U64, _, _)
        | Content::Alias(Symbol::NUM_UNSIGNED64, _, _)
        | Content::Alias(Symbol::NUM_AT_UNSIGNED64, _, _) => {
            IntOrFloat::UnsignedIntType(IntPrecision::I64)
        }
        Content::Alias(Symbol::NUM_U32, _, _)
        | Content::Alias(Symbol::NUM_UNSIGNED32, _, _)
        | Content::Alias(Symbol::NUM_AT_UNSIGNED32, _, _) => {
            IntOrFloat::UnsignedIntType(IntPrecision::I32)
        }
        Content::Alias(Symbol::NUM_U16, _, _)
        | Content::Alias(Symbol::NUM_UNSIGNED16, _, _)
        | Content::Alias(Symbol::NUM_AT_UNSIGNED16, _, _) => {
            IntOrFloat::UnsignedIntType(IntPrecision::I16)
        }
        Content::Alias(Symbol::NUM_U8, _, _)
        | Content::Alias(Symbol::NUM_UNSIGNED8, _, _)
        | Content::Alias(Symbol::NUM_AT_UNSIGNED8, _, _) => {
            IntOrFloat::UnsignedIntType(IntPrecision::I8)
        }
        Content::Alias(Symbol::NUM_FLOATINGPOINT, args, _)  => {
            debug_assert!(args.len() == 1);

            // Recurse on the second argument
            let var = subs[args.variables().into_iter().next().unwrap()];
            num_argument_to_int_or_float(subs, ptr_bytes, var, true)
        }
        Content::Alias(Symbol::NUM_FLOAT, _, _) // We default FloatingPoint to F64
        | Content::Alias(Symbol::NUM_F64, _, _)
        | Content::Alias(Symbol::NUM_BINARY64, _, _)
        | Content::Alias(Symbol::NUM_AT_BINARY64, _, _) => {
            IntOrFloat::BinaryFloatType(FloatPrecision::F64)
        }
        Content::Alias(Symbol::NUM_DECIMAL, _, _)
        | Content::Alias(Symbol::NUM_AT_DECIMAL, _, _) => {
            IntOrFloat::DecimalFloatType
        }
        Content::Alias(Symbol::NUM_F32, _, _)
        | Content::Alias(Symbol::NUM_BINARY32, _, _)
        | Content::Alias(Symbol::NUM_AT_BINARY32, _, _) => {
            IntOrFloat::BinaryFloatType(FloatPrecision::F32)
        }
        Content::Alias(Symbol::NUM_NAT, _, _)
        | Content::Alias(Symbol::NUM_NATURAL, _, _)
        | Content::Alias(Symbol::NUM_AT_NATURAL, _, _) => {
            IntOrFloat::UnsignedIntType(IntPrecision::Usize)

        }
        other => {
            panic!(
                "Unrecognized Num type argument for var {:?} with Content: {:?}",
                var, other
            );
        }
    }
}

type ToLowLevelCallArguments<'a> = (Symbol, Symbol, Option<Layout<'a>>, CallSpecId, UpdateModeId);

/// Use the lambda set to figure out how to make a lowlevel call
#[allow(clippy::too_many_arguments)]
fn lowlevel_match_on_lambda_set<'a, ToLowLevelCall>(
    env: &mut Env<'a, '_>,
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
    match lambda_set.runtime_representation() {
        Layout::Union(union_layout) => {
            let closure_tag_id_symbol = env.unique_symbol();

            let result = lowlevel_union_lambda_set_to_switch(
                env,
                lambda_set.set,
                closure_tag_id_symbol,
                union_layout.tag_id_layout(),
                closure_data_symbol,
                lambda_set.is_represented(),
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
        Layout::Struct(_) => match lambda_set.set.get(0) {
            Some((function_symbol, _)) => {
                let call_spec_id = env.next_call_specialization_id();
                let update_mode = env.next_update_mode_id();
                let call = to_lowlevel_call((
                    *function_symbol,
                    closure_data_symbol,
                    lambda_set.is_represented(),
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
        Layout::Builtin(Builtin::Int1) => {
            let closure_tag_id_symbol = closure_data_symbol;

            lowlevel_enum_lambda_set_to_switch(
                env,
                lambda_set.set,
                closure_tag_id_symbol,
                Layout::Builtin(Builtin::Int1),
                closure_data_symbol,
                lambda_set.is_represented(),
                to_lowlevel_call,
                return_layout,
                assigned,
                hole,
            )
        }
        Layout::Builtin(Builtin::Int8) => {
            let closure_tag_id_symbol = closure_data_symbol;

            lowlevel_enum_lambda_set_to_switch(
                env,
                lambda_set.set,
                closure_tag_id_symbol,
                Layout::Builtin(Builtin::Int8),
                closure_data_symbol,
                lambda_set.is_represented(),
                to_lowlevel_call,
                return_layout,
                assigned,
                hole,
            )
        }
        other => todo!("{:?}", other),
    }
}

#[allow(clippy::too_many_arguments)]
fn lowlevel_union_lambda_set_to_switch<'a, ToLowLevelCall>(
    env: &mut Env<'a, '_>,
    lambda_set: &'a [(Symbol, &'a [Layout<'a>])],
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
    debug_assert!(!lambda_set.is_empty());

    let join_point_id = JoinPointId(env.unique_symbol());

    let mut branches = Vec::with_capacity_in(lambda_set.len(), env.arena);

    for (i, (function_symbol, _)) in lambda_set.iter().enumerate() {
        let assigned = env.unique_symbol();

        let hole = Stmt::Jump(join_point_id, env.arena.alloc([assigned]));

        let call_spec_id = env.next_call_specialization_id();
        let update_mode = env.next_update_mode_id();
        let call = to_lowlevel_call((
            *function_symbol,
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

/// Use the lambda set to figure out how to make a call-by-name
#[allow(clippy::too_many_arguments)]
fn match_on_lambda_set<'a>(
    env: &mut Env<'a, '_>,
    lambda_set: LambdaSet<'a>,
    closure_data_symbol: Symbol,
    argument_symbols: &'a [Symbol],
    argument_layouts: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    match lambda_set.runtime_representation() {
        Layout::Union(union_layout) => {
            let closure_tag_id_symbol = env.unique_symbol();

            let result = union_lambda_set_to_switch(
                env,
                lambda_set,
                Layout::Union(union_layout),
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
        Layout::Struct(fields) => {
            let function_symbol = lambda_set.set[0].0;

            union_lambda_set_branch_help(
                env,
                function_symbol,
                lambda_set,
                closure_data_symbol,
                Layout::Struct(fields),
                argument_symbols,
                argument_layouts,
                return_layout,
                assigned,
                hole,
            )
        }
        Layout::Builtin(Builtin::Int1) => {
            let closure_tag_id_symbol = closure_data_symbol;

            enum_lambda_set_to_switch(
                env,
                lambda_set.set,
                closure_tag_id_symbol,
                Layout::Builtin(Builtin::Int1),
                closure_data_symbol,
                argument_symbols,
                argument_layouts,
                return_layout,
                assigned,
                hole,
            )
        }
        Layout::Builtin(Builtin::Int8) => {
            let closure_tag_id_symbol = closure_data_symbol;

            enum_lambda_set_to_switch(
                env,
                lambda_set.set,
                closure_tag_id_symbol,
                Layout::Builtin(Builtin::Int8),
                closure_data_symbol,
                argument_symbols,
                argument_layouts,
                return_layout,
                assigned,
                hole,
            )
        }
        other => todo!("{:?}", other),
    }
}

#[allow(clippy::too_many_arguments)]
fn union_lambda_set_to_switch<'a>(
    env: &mut Env<'a, '_>,
    lambda_set: LambdaSet<'a>,
    closure_layout: Layout<'a>,
    closure_tag_id_symbol: Symbol,
    closure_tag_id_layout: Layout<'a>,
    closure_data_symbol: Symbol,
    argument_symbols: &'a [Symbol],
    argument_layouts: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    if lambda_set.set.is_empty() {
        // NOTE this can happen if there is a type error somewhere. Since the lambda set is empty,
        // there is really nothing we can do here. We generate a runtime error here which allows
        // code gen to proceed. We then assume that we hit another (more descriptive) error before
        // hitting this one

        let msg = "a Lambda Set isempty. Most likely there is a type error in your program.";
        return Stmt::RuntimeError(msg);
    }

    let join_point_id = JoinPointId(env.unique_symbol());

    let mut branches = Vec::with_capacity_in(lambda_set.set.len(), env.arena);

    for (i, (function_symbol, _)) in lambda_set.set.iter().enumerate() {
        let stmt = union_lambda_set_branch(
            env,
            lambda_set,
            join_point_id,
            *function_symbol,
            closure_data_symbol,
            closure_layout,
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
    lambda_set: LambdaSet<'a>,
    join_point_id: JoinPointId,
    function_symbol: Symbol,
    closure_data_symbol: Symbol,
    closure_data_layout: Layout<'a>,
    argument_symbols_slice: &'a [Symbol],
    argument_layouts_slice: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
) -> Stmt<'a> {
    let result_symbol = env.unique_symbol();

    let hole = Stmt::Jump(join_point_id, env.arena.alloc([result_symbol]));

    union_lambda_set_branch_help(
        env,
        function_symbol,
        lambda_set,
        closure_data_symbol,
        closure_data_layout,
        argument_symbols_slice,
        argument_layouts_slice,
        return_layout,
        result_symbol,
        env.arena.alloc(hole),
    )
}

#[allow(clippy::too_many_arguments)]
fn union_lambda_set_branch_help<'a>(
    env: &mut Env<'a, '_>,
    function_symbol: Symbol,
    lambda_set: LambdaSet<'a>,
    closure_data_symbol: Symbol,
    closure_data_layout: Layout<'a>,
    argument_symbols_slice: &'a [Symbol],
    argument_layouts_slice: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    let (argument_layouts, argument_symbols) = match closure_data_layout {
        Layout::Struct(&[]) | Layout::Builtin(Builtin::Int1) | Layout::Builtin(Builtin::Int8) => {
            (argument_layouts_slice, argument_symbols_slice)
        }
        _ if lambda_set.member_does_not_need_closure_argument(function_symbol) => {
            // sometimes unification causes a function that does not itself capture anything
            // to still get a lambda set that does store information. We must not pass a closure
            // argument in this case

            (argument_layouts_slice, argument_symbols_slice)
        }
        _ => {
            // extend layouts with the layout of the closure environment
            let mut argument_layouts =
                Vec::with_capacity_in(argument_layouts_slice.len() + 1, env.arena);
            argument_layouts.extend(argument_layouts_slice);
            argument_layouts.push(Layout::LambdaSet(lambda_set));

            // extend symbols with the symbol of the closure environment
            let mut argument_symbols =
                Vec::with_capacity_in(argument_symbols_slice.len() + 1, env.arena);
            argument_symbols.extend(argument_symbols_slice);
            argument_symbols.push(closure_data_symbol);

            (
                argument_layouts.into_bump_slice(),
                argument_symbols.into_bump_slice(),
            )
        }
    };

    // build the call
    let call = self::Call {
        call_type: CallType::ByName {
            name: function_symbol,
            ret_layout: return_layout,
            arg_layouts: argument_layouts,
            specialization_id: env.next_call_specialization_id(),
        },
        arguments: argument_symbols,
    };

    build_call(env, call, assigned, *return_layout, hole)
}

#[allow(clippy::too_many_arguments)]
fn enum_lambda_set_to_switch<'a>(
    env: &mut Env<'a, '_>,
    lambda_set: &'a [(Symbol, &'a [Layout<'a>])],
    closure_tag_id_symbol: Symbol,
    closure_tag_id_layout: Layout<'a>,
    closure_data_symbol: Symbol,
    argument_symbols: &'a [Symbol],
    argument_layouts: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
    assigned: Symbol,
    hole: &'a Stmt<'a>,
) -> Stmt<'a> {
    debug_assert!(!lambda_set.is_empty());

    let join_point_id = JoinPointId(env.unique_symbol());

    let mut branches = Vec::with_capacity_in(lambda_set.len(), env.arena);

    let closure_layout = closure_tag_id_layout;

    for (i, (function_symbol, _)) in lambda_set.iter().enumerate() {
        let stmt = enum_lambda_set_branch(
            env,
            join_point_id,
            *function_symbol,
            closure_data_symbol,
            closure_layout,
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
fn enum_lambda_set_branch<'a>(
    env: &mut Env<'a, '_>,
    join_point_id: JoinPointId,
    function_symbol: Symbol,
    closure_data_symbol: Symbol,
    closure_data_layout: Layout<'a>,
    argument_symbols_slice: &'a [Symbol],
    argument_layouts_slice: &'a [Layout<'a>],
    return_layout: &'a Layout<'a>,
) -> Stmt<'a> {
    let result_symbol = env.unique_symbol();

    let hole = Stmt::Jump(join_point_id, env.arena.alloc([result_symbol]));

    let assigned = result_symbol;

    let (argument_layouts, argument_symbols) = match closure_data_layout {
        Layout::Struct(&[]) | Layout::Builtin(Builtin::Int1) | Layout::Builtin(Builtin::Int8) => {
            (argument_layouts_slice, argument_symbols_slice)
        }
        _ => {
            // extend layouts with the layout of the closure environment
            let mut argument_layouts =
                Vec::with_capacity_in(argument_layouts_slice.len() + 1, env.arena);
            argument_layouts.extend(argument_layouts_slice);
            argument_layouts.push(closure_data_layout);

            // extend symbols with the symbol of the closure environment
            let mut argument_symbols =
                Vec::with_capacity_in(argument_symbols_slice.len() + 1, env.arena);
            argument_symbols.extend(argument_symbols_slice);
            argument_symbols.push(closure_data_symbol);

            (
                argument_layouts.into_bump_slice(),
                argument_symbols.into_bump_slice(),
            )
        }
    };

    let call = self::Call {
        call_type: CallType::ByName {
            name: function_symbol,
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
    lambda_set: &'a [(Symbol, &'a [Layout<'a>])],
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
    debug_assert!(!lambda_set.is_empty());

    let join_point_id = JoinPointId(env.unique_symbol());

    let mut branches = Vec::with_capacity_in(lambda_set.len(), env.arena);

    for (i, (function_symbol, _)) in lambda_set.iter().enumerate() {
        let result_symbol = env.unique_symbol();

        let hole = Stmt::Jump(join_point_id, env.arena.alloc([result_symbol]));

        let call_spec_id = env.next_call_specialization_id();
        let update_mode = env.next_update_mode_id();
        let call = to_lowlevel_call((
            *function_symbol,
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
