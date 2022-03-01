use crate::ir::{Expr, HigherOrderLowLevel, JoinPointId, Param, Proc, ProcLayout, Stmt};
use crate::layout::Layout;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::{default_hasher, MutMap, MutSet};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;

pub const OWNED: bool = false;
pub const BORROWED: bool = true;

fn should_borrow_layout(layout: &Layout) -> bool {
    layout.is_refcounted()
}

pub fn infer_borrow<'a>(
    arena: &'a Bump,
    procs: &MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> ParamMap<'a> {
    // intern the layouts

    let mut param_map = {
        let (declaration_to_index, total_number_of_params) = DeclarationToIndex::new(arena, procs);

        ParamMap {
            declaration_to_index,
            join_points: MutMap::default(),
            declarations: bumpalo::vec![in arena; Param::EMPTY; total_number_of_params],
        }
    };

    for (key, proc) in procs {
        param_map.visit_proc(arena, proc, *key);
    }

    let mut env = BorrowInfState {
        current_proc: Symbol::ATTR_ATTR,
        param_set: MutSet::default(),
        owned: MutMap::default(),
        modified: false,
        arena,
    };

    // next we first partition the functions into strongly connected components, then do a
    // topological sort on these components, finally run the fix-point borrow analysis on each
    // component (in top-sorted order, from primitives (std-lib) to main)

    let successor_map = &make_successor_mapping(arena, procs);
    let successors = move |key: &Symbol| {
        let slice = match successor_map.get(key) {
            None => &[] as &[_],
            Some(s) => s.as_slice(),
        };

        slice.iter().copied()
    };

    let mut symbols = Vec::with_capacity_in(procs.len(), arena);
    symbols.extend(procs.keys().map(|x| x.0));

    let sccs = ven_graph::strongly_connected_components(&symbols, successors);

    let mut symbol_to_component = MutMap::default();
    for (i, symbols) in sccs.iter().enumerate() {
        for symbol in symbols {
            symbol_to_component.insert(*symbol, i);
        }
    }

    let mut component_to_successors = Vec::with_capacity_in(sccs.len(), arena);
    for (i, symbols) in sccs.iter().enumerate() {
        // guess: every function has ~1 successor
        let mut succs = Vec::with_capacity_in(symbols.len(), arena);

        for symbol in symbols {
            for s in successors(symbol) {
                let c = symbol_to_component[&s];

                // don't insert self to prevent cycles
                if c != i {
                    succs.push(c);
                }
            }
        }

        succs.sort_unstable();
        succs.dedup();

        component_to_successors.push(succs);
    }

    let mut components = Vec::with_capacity_in(component_to_successors.len(), arena);
    components.extend(0..component_to_successors.len());

    let mut groups = Vec::new_in(arena);

    let component_to_successors = &component_to_successors;
    match ven_graph::topological_sort_into_groups(&components, |c: &usize| {
        component_to_successors[*c].iter().copied()
    }) {
        Ok(component_groups) => {
            let mut component_to_group = bumpalo::vec![in arena; usize::MAX; components.len()];

            // for each component, store which group it is in
            for (group_index, component_group) in component_groups.iter().enumerate() {
                for component in component_group {
                    component_to_group[*component] = group_index;
                }
            }

            // prepare groups
            groups.reserve(component_groups.len());
            for _ in 0..component_groups.len() {
                groups.push(Vec::new_in(arena));
            }

            for (key, proc) in procs {
                let symbol = key.0;
                let offset = param_map.get_param_offset(key.0, key.1);

                // the component this symbol is a part of
                let component = symbol_to_component[&symbol];

                // now find the group that this component belongs to
                let group = component_to_group[component];

                groups[group].push((proc, offset));
            }
        }
        Err((_groups, _remainder)) => {
            unreachable!("because we find strongly-connected components first");
        }
    }

    for group in groups.into_iter().rev() {
        // This is a fixed-point analysis
        //
        // all functions initiall own all their parameters
        // through a series of checks and heuristics, some arguments are set to borrowed
        // when that doesn't lead to conflicts the change is kept, otherwise it may be reverted
        //
        // when the signatures no longer change, the analysis stops and returns the signatures
        loop {
            for (proc, param_offset) in group.iter() {
                env.collect_proc(&mut param_map, proc, *param_offset);
            }

            if !env.modified {
                // if there were no modifications, we're done
                break;
            } else {
                // otherwise see if there are changes after another iteration
                env.modified = false;
            }
        }
    }

    param_map
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct ParamOffset(usize);

impl From<ParamOffset> for usize {
    fn from(id: ParamOffset) -> Self {
        id.0 as usize
    }
}

#[derive(Debug)]
struct DeclarationToIndex<'a> {
    elements: Vec<'a, ((Symbol, ProcLayout<'a>), ParamOffset)>,
}

impl<'a> DeclarationToIndex<'a> {
    fn new(arena: &'a Bump, procs: &MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>) -> (Self, usize) {
        let mut declaration_to_index = Vec::with_capacity_in(procs.len(), arena);

        let mut i = 0;
        for key in procs.keys().copied() {
            declaration_to_index.push((key, ParamOffset(i)));

            i += key.1.arguments.len();
        }

        declaration_to_index.sort_unstable_by_key(|t| t.0 .0);

        (
            DeclarationToIndex {
                elements: declaration_to_index,
            },
            i,
        )
    }

    fn get_param_offset(
        &self,
        needle_symbol: Symbol,
        needle_layout: ProcLayout<'a>,
    ) -> ParamOffset {
        if let Ok(middle_index) = self
            .elements
            .binary_search_by_key(&needle_symbol, |t| t.0 .0)
        {
            // first, iterate backward until we hit a different symbol
            let backward = self.elements[..middle_index].iter().rev();

            for ((symbol, proc_layout), param_offset) in backward {
                if *symbol != needle_symbol {
                    break;
                } else if *proc_layout == needle_layout {
                    return *param_offset;
                }
            }

            // if not found, iterate forward until we find our combo
            let forward = self.elements[middle_index..].iter();

            for ((symbol, proc_layout), param_offset) in forward {
                if *symbol != needle_symbol {
                    break;
                } else if *proc_layout == needle_layout {
                    return *param_offset;
                }
            }
        }
        unreachable!(
            "symbol/layout {:?} {:?} combo must be in DeclarationToIndex",
            needle_symbol, needle_layout
        )
    }
}

#[derive(Debug)]
pub struct ParamMap<'a> {
    /// Map a (Symbol, ProcLayout) pair to the starting index in the `declarations` array
    declaration_to_index: DeclarationToIndex<'a>,
    /// the parameters of all functions in a single flat array.
    ///
    /// - the map above gives the index of the first parameter for the function
    /// - the length of the ProcLayout's argument field gives the total number of parameters
    ///
    /// These can be read by taking a slice into this array, and can also be updated in-place
    declarations: Vec<'a, Param<'a>>,
    join_points: MutMap<JoinPointId, &'a [Param<'a>]>,
}

impl<'a> ParamMap<'a> {
    pub fn get_param_offset(&self, symbol: Symbol, layout: ProcLayout<'a>) -> ParamOffset {
        self.declaration_to_index.get_param_offset(symbol, layout)
    }

    pub fn get_symbol(&self, symbol: Symbol, layout: ProcLayout<'a>) -> Option<&[Param<'a>]> {
        // let index: usize = self.declaration_to_index[&(symbol, layout)].into();
        let index: usize = self.get_param_offset(symbol, layout).into();

        self.declarations.get(index..index + layout.arguments.len())
    }

    pub fn get_join_point(&self, id: JoinPointId) -> &'a [Param<'a>] {
        match self.join_points.get(&id) {
            Some(slice) => slice,
            None => unreachable!("join point not in param map: {:?}", id),
        }
    }

    pub fn iter_symbols(&'a self) -> impl Iterator<Item = &'a Symbol> {
        self.declaration_to_index.elements.iter().map(|t| &t.0 .0)
    }
}

impl<'a> ParamMap<'a> {
    fn init_borrow_params(arena: &'a Bump, ps: &'a [Param<'a>]) -> &'a [Param<'a>] {
        Vec::from_iter_in(
            ps.iter().map(|p| Param {
                borrow: p.layout.is_refcounted(),
                layout: p.layout,
                symbol: p.symbol,
            }),
            arena,
        )
        .into_bump_slice()
    }

    fn init_borrow_args(arena: &'a Bump, ps: &'a [(Layout<'a>, Symbol)]) -> &'a [Param<'a>] {
        Vec::from_iter_in(
            ps.iter().map(|(layout, symbol)| Param {
                borrow: should_borrow_layout(layout),
                layout: *layout,
                symbol: *symbol,
            }),
            arena,
        )
        .into_bump_slice()
    }

    fn init_borrow_args_always_owned(
        arena: &'a Bump,
        ps: &'a [(Layout<'a>, Symbol)],
    ) -> &'a [Param<'a>] {
        Vec::from_iter_in(
            ps.iter().map(|(layout, symbol)| Param {
                borrow: false,
                layout: *layout,
                symbol: *symbol,
            }),
            arena,
        )
        .into_bump_slice()
    }

    fn visit_proc(&mut self, arena: &'a Bump, proc: &Proc<'a>, key: (Symbol, ProcLayout<'a>)) {
        if proc.must_own_arguments {
            self.visit_proc_always_owned(arena, proc, key);
            return;
        }

        let index: usize = self.get_param_offset(key.0, key.1).into();

        for (i, param) in Self::init_borrow_args(arena, proc.args)
            .iter()
            .copied()
            .enumerate()
        {
            self.declarations[index + i] = param;
        }

        self.visit_stmt(arena, proc.name, &proc.body);
    }

    fn visit_proc_always_owned(
        &mut self,
        arena: &'a Bump,
        proc: &Proc<'a>,
        key: (Symbol, ProcLayout<'a>),
    ) {
        let index: usize = self.get_param_offset(key.0, key.1).into();

        for (i, param) in Self::init_borrow_args_always_owned(arena, proc.args)
            .iter()
            .copied()
            .enumerate()
        {
            self.declarations[index + i] = param;
        }

        self.visit_stmt(arena, proc.name, &proc.body);
    }

    fn visit_stmt(&mut self, arena: &'a Bump, _fnid: Symbol, stmt: &Stmt<'a>) {
        use Stmt::*;

        let mut stack = bumpalo::vec![ in arena; stmt ];

        while let Some(stmt) = stack.pop() {
            match stmt {
                Join {
                    id: j,
                    parameters: xs,
                    remainder: v,
                    body: b,
                } => {
                    self.join_points
                        .insert(*j, Self::init_borrow_params(arena, xs));

                    stack.push(v);
                    stack.push(b);
                }
                Let(_, _, _, cont) => {
                    stack.push(cont);
                }
                Switch {
                    branches,
                    default_branch,
                    ..
                } => {
                    stack.extend(branches.iter().map(|b| &b.2));
                    stack.push(default_branch.1);
                }
                Refcounting(_, _) => unreachable!("these have not been introduced yet"),

                Ret(_) | Jump(_, _) | RuntimeError(_) => {
                    // these are terminal, do nothing
                }
            }
        }
    }
}

// Apply the inferred borrow annotations stored in ParamMap to a block of mutually recursive procs

struct BorrowInfState<'a> {
    current_proc: Symbol,
    param_set: MutSet<Symbol>,
    owned: MutMap<Symbol, MutSet<Symbol>>,
    modified: bool,
    arena: &'a Bump,
}

impl<'a> BorrowInfState<'a> {
    pub fn own_var(&mut self, x: Symbol) {
        let current = self.owned.get_mut(&self.current_proc).unwrap();

        if current.insert(x) {
            // entered if key was not yet present. If so, the set is modified,
            // hence we set this flag
            self.modified = true;
        }
    }

    /// if the extracted value is owned, then the surrounding structure must be too
    fn if_is_owned_then_own(&mut self, extracted: Symbol, structure: Symbol) {
        match self.owned.get_mut(&self.current_proc) {
            None => unreachable!(
                "the current procedure symbol {:?} is not in the owned map",
                self.current_proc
            ),
            Some(set) => {
                if set.contains(&extracted) && set.insert(structure) {
                    // entered if key was not yet present. If so, the set is modified,
                    // hence we set this flag
                    self.modified = true;
                }
            }
        }
    }

    fn is_owned(&self, x: Symbol) -> bool {
        match self.owned.get(&self.current_proc) {
            None => unreachable!(
                "the current procedure symbol {:?} is not in the owned map",
                self.current_proc
            ),
            Some(set) => set.contains(&x),
        }
    }

    fn update_param_map_help(&mut self, ps: &[Param<'a>]) -> &'a [Param<'a>] {
        let mut new_ps = Vec::with_capacity_in(ps.len(), self.arena);
        new_ps.extend(ps.iter().map(|p| {
            if !p.borrow {
                *p
            } else if self.is_owned(p.symbol) {
                self.modified = true;
                let mut p = *p;
                p.borrow = false;

                p
            } else {
                *p
            }
        }));

        new_ps.into_bump_slice()
    }

    fn update_param_map_declaration(
        &mut self,
        param_map: &mut ParamMap<'a>,
        start: ParamOffset,
        length: usize,
    ) {
        let index: usize = start.into();
        let ps = &mut param_map.declarations[index..][..length];

        for p in ps.iter_mut() {
            if !p.borrow {
                // do nothing
            } else if self.is_owned(p.symbol) {
                self.modified = true;
                p.borrow = false;
            } else {
                // do nothing
            }
        }
    }

    fn update_param_map_join_point(&mut self, param_map: &mut ParamMap<'a>, id: JoinPointId) {
        let ps = param_map.join_points[&id];
        let new_ps = self.update_param_map_help(ps);
        param_map.join_points.insert(id, new_ps);
    }

    /// This looks at an application `f x1 x2 x3`
    /// If the parameter (based on the definition of `f`) is owned,
    /// then the argument must also be owned
    fn own_args_using_params(&mut self, xs: &[Symbol], ps: &[Param<'a>]) {
        debug_assert_eq!(xs.len(), ps.len());

        for (x, p) in xs.iter().zip(ps.iter()) {
            if !p.borrow {
                self.own_var(*x);
            }
        }
    }

    /// This looks at an application `f x1 x2 x3`
    /// If the parameter (based on the definition of `f`) is owned,
    /// then the argument must also be owned
    fn own_args_using_bools(&mut self, xs: &[Symbol], ps: &[bool]) {
        debug_assert_eq!(xs.len(), ps.len());

        for (x, borrow) in xs.iter().zip(ps.iter()) {
            if !borrow {
                self.own_var(*x);
            }
        }
    }

    /// For each xs[i], if xs[i] is owned, then mark ps[i] as owned.
    /// We use this action to preserve tail calls. That is, if we have
    /// a tail call `f xs`, if the i-th parameter is borrowed, but `xs[i]` is owned
    /// we would have to insert a `dec xs[i]` after `f xs` and consequently
    /// "break" the tail call.
    fn own_params_using_args(&mut self, xs: &[Symbol], ps: &[Param<'a>]) {
        debug_assert_eq!(xs.len(), ps.len());

        for (x, p) in xs.iter().zip(ps.iter()) {
            if self.is_owned(*x) {
                self.own_var(p.symbol);
            }
        }
    }

    /// Mark `xs[i]` as owned if it is one of the parameters `ps`.
    /// We use this action to mark function parameters that are being "packed" inside constructors.
    /// This is a heuristic, and is not related with the effectiveness of the reset/reuse optimization.
    /// It is useful for code such as
    ///
    /// > def f (x y : obj) :=
    /// > let z := ctor_1 x y;
    /// > ret z
    fn own_args_if_param(&mut self, xs: &[Symbol]) {
        for x in xs.iter() {
            // TODO may also be asking for the index here? see Lean
            if self.param_set.contains(x) {
                self.own_var(*x);
            }
        }
    }

    /// This looks at the assignment
    ///
    /// let z = e in ...
    ///
    /// and determines whether z and which of the symbols used in e
    /// must be taken as owned parameters
    fn collect_call(&mut self, param_map: &mut ParamMap<'a>, z: Symbol, e: &crate::ir::Call<'a>) {
        use crate::ir::CallType::*;

        let crate::ir::Call {
            call_type,
            arguments,
        } = e;

        match call_type {
            ByName {
                name,
                ret_layout,
                arg_layouts,
                ..
            } => {
                let top_level = ProcLayout::new(self.arena, arg_layouts, **ret_layout);

                // get the borrow signature of the applied function
                let ps = param_map
                    .get_symbol(*name, top_level)
                    .expect("function is defined");

                // the return value will be owned
                self.own_var(z);

                // if the function exects an owned argument (ps), the argument must be owned (args)
                debug_assert_eq!(
                    arguments.len(),
                    ps.len(),
                    "{:?} has {} parameters, but was applied to {} arguments",
                    name,
                    ps.len(),
                    arguments.len()
                );

                self.own_args_using_params(arguments, ps);
            }

            LowLevel { op, .. } => {
                debug_assert!(!op.is_higher_order());

                self.own_var(z);

                let ps = lowlevel_borrow_signature(self.arena, *op);

                self.own_args_using_bools(arguments, ps);
            }

            HigherOrder(HigherOrderLowLevel {
                op,
                passed_function,
                ..
            }) => {
                use crate::low_level::HigherOrder::*;

                let closure_layout = ProcLayout {
                    arguments: passed_function.argument_layouts,
                    result: passed_function.return_layout,
                };

                let function_ps = match param_map.get_symbol(passed_function.name, closure_layout) {
                    Some(function_ps) => function_ps,
                    None => unreachable!(),
                };

                match op {
                    ListMap { xs }
                    | ListKeepIf { xs }
                    | ListKeepOks { xs }
                    | ListKeepErrs { xs }
                    | ListAny { xs }
                    | ListAll { xs }
                    | ListFindUnsafe { xs } => {
                        // own the list if the function wants to own the element
                        if !function_ps[0].borrow {
                            self.own_var(*xs);
                        }
                    }
                    ListMapWithIndex { xs } => {
                        // List.mapWithIndex : List before, (before, Nat -> after) -> List after
                        // own the list if the function wants to own the element (before, index 0)
                        if !function_ps[0].borrow {
                            self.own_var(*xs);
                        }
                    }
                    ListMap2 { xs, ys } => {
                        // own the lists if the function wants to own the element
                        if !function_ps[0].borrow {
                            self.own_var(*xs);
                        }

                        if !function_ps[1].borrow {
                            self.own_var(*ys);
                        }
                    }
                    ListMap3 { xs, ys, zs } => {
                        // own the lists if the function wants to own the element
                        if !function_ps[0].borrow {
                            self.own_var(*xs);
                        }
                        if !function_ps[1].borrow {
                            self.own_var(*ys);
                        }
                        if !function_ps[2].borrow {
                            self.own_var(*zs);
                        }
                    }
                    ListMap4 { xs, ys, zs, ws } => {
                        // own the lists if the function wants to own the element
                        if !function_ps[0].borrow {
                            self.own_var(*xs);
                        }
                        if !function_ps[1].borrow {
                            self.own_var(*ys);
                        }
                        if !function_ps[2].borrow {
                            self.own_var(*zs);
                        }
                        if !function_ps[3].borrow {
                            self.own_var(*ws);
                        }
                    }
                    ListSortWith { xs } => {
                        // always own the input list
                        self.own_var(*xs);
                    }
                    ListWalk { xs, state }
                    | ListWalkUntil { xs, state }
                    | ListWalkBackwards { xs, state }
                    | DictWalk { xs, state } => {
                        // own the default value if the function wants to own it
                        if !function_ps[0].borrow {
                            self.own_var(*state);
                        }

                        // own the data structure if the function wants to own the element
                        if !function_ps[1].borrow {
                            self.own_var(*xs);
                        }
                    }
                }

                // own the closure environment if the function needs to own it
                let function_env_position = op.function_arity();
                if let Some(false) = function_ps.get(function_env_position).map(|p| p.borrow) {
                    self.own_var(passed_function.captured_environment);
                }
            }

            Foreign { .. } => {
                // very unsure what demand ForeignCall should place upon its arguments
                self.own_var(z);

                let ps = foreign_borrow_signature(self.arena, arguments.len());

                self.own_args_using_bools(arguments, ps);
            }
        }
    }

    fn collect_expr(&mut self, param_map: &mut ParamMap<'a>, z: Symbol, e: &Expr<'a>) {
        use Expr::*;

        match e {
            Array { elems: xs, .. } => {
                let xs = Vec::from_iter_in(xs.iter().filter_map(|e| e.to_symbol()), self.arena);
                self.own_var(z);

                // if the used symbol is an argument to the current function,
                // the function must take it as an owned parameter
                self.own_args_if_param(&xs);
            }
            Tag { arguments: xs, .. } | Struct(xs) => {
                self.own_var(z);

                // if the used symbol is an argument to the current function,
                // the function must take it as an owned parameter
                self.own_args_if_param(xs);
            }
            Reset { symbol: x, .. } => {
                self.own_var(z);
                self.own_var(*x);
            }
            Reuse {
                symbol: x,
                arguments: ys,
                ..
            } => {
                self.own_var(z);
                self.own_var(*x);
                self.own_args_if_param(ys);
            }
            EmptyArray => {
                self.own_var(z);
            }

            Call(call) => self.collect_call(param_map, z, call),

            Literal(_) | RuntimeErrorFunction(_) => {}

            StructAtIndex { structure: x, .. } => {
                // if the structure (record/tag/array) is owned, the extracted value is
                self.if_is_owned_then_own(*x, z);

                // if the extracted value is owned, the structure must be too
                self.if_is_owned_then_own(z, *x);
            }

            UnionAtIndex { structure: x, .. } => {
                // if the structure (record/tag/array) is owned, the extracted value is
                self.if_is_owned_then_own(*x, z);

                // if the extracted value is owned, the structure must be too
                self.if_is_owned_then_own(z, *x);
            }

            GetTagId { structure: x, .. } => {
                // if the structure (record/tag/array) is owned, the extracted value is
                self.if_is_owned_then_own(*x, z);

                // if the extracted value is owned, the structure must be too
                self.if_is_owned_then_own(z, *x);
            }
        }
    }

    #[allow(clippy::many_single_char_names)]
    fn preserve_tail_call(
        &mut self,
        param_map: &mut ParamMap<'a>,
        x: Symbol,
        v: &Expr<'a>,
        b: &Stmt<'a>,
    ) {
        if let (
            Expr::Call(crate::ir::Call {
                call_type:
                    crate::ir::CallType::ByName {
                        name: g,
                        arg_layouts,
                        ret_layout,
                        ..
                    },
                arguments: ys,
                ..
            }),
            Stmt::Ret(z),
        ) = (v, b)
        {
            let top_level = ProcLayout::new(self.arena, arg_layouts, **ret_layout);

            if self.current_proc == *g && x == *z {
                // anonymous functions (for which the ps may not be known)
                // can never be tail-recursive, so this is fine
                if let Some(ps) = param_map.get_symbol(*g, top_level) {
                    self.own_params_using_args(ys, ps)
                }
            }
        }
    }

    fn update_param_set(&mut self, ps: &[Param<'a>]) {
        for p in ps.iter() {
            self.param_set.insert(p.symbol);
        }
    }

    fn update_param_set_symbols(&mut self, ps: &[Symbol]) {
        for p in ps.iter() {
            self.param_set.insert(*p);
        }
    }

    fn collect_stmt(&mut self, param_map: &mut ParamMap<'a>, stmt: &Stmt<'a>) {
        use Stmt::*;

        match stmt {
            Join {
                id: j,
                parameters: ys,
                remainder: v,
                body: b,
            } => {
                let old = self.param_set.clone();
                self.update_param_set(ys);
                self.collect_stmt(param_map, v);
                self.param_set = old;
                self.update_param_map_join_point(param_map, *j);

                self.collect_stmt(param_map, b);
            }

            Let(x, v, _, mut b) => {
                let mut stack = Vec::new_in(self.arena);

                stack.push((*x, v));

                while let Stmt::Let(symbol, expr, _, tail) = b {
                    b = tail;
                    stack.push((*symbol, expr));
                }

                self.collect_stmt(param_map, b);

                let mut it = stack.into_iter().rev();

                // collect the final expr, and see if we need to preserve a tail call
                let (x, v) = it.next().unwrap();
                self.collect_expr(param_map, x, v);
                self.preserve_tail_call(param_map, x, v, b);

                for (x, v) in it {
                    self.collect_expr(param_map, x, v);
                }
            }

            Jump(j, ys) => {
                let ps = param_map.get_join_point(*j);

                // for making sure the join point can reuse
                self.own_args_using_params(ys, ps);

                // for making sure the tail call is preserved
                self.own_params_using_args(ys, ps);
            }
            Switch {
                branches,
                default_branch,
                ..
            } => {
                for (_, _, b) in branches.iter() {
                    self.collect_stmt(param_map, b);
                }
                self.collect_stmt(param_map, default_branch.1);
            }
            Refcounting(_, _) => unreachable!("these have not been introduced yet"),

            Ret(_) | RuntimeError(_) => {
                // these are terminal, do nothing
            }
        }
    }

    fn collect_proc(
        &mut self,
        param_map: &mut ParamMap<'a>,
        proc: &Proc<'a>,
        param_offset: ParamOffset,
    ) {
        let old = self.param_set.clone();

        let ys = Vec::from_iter_in(proc.args.iter().map(|t| t.1), self.arena).into_bump_slice();
        self.update_param_set_symbols(ys);
        self.current_proc = proc.name;

        // ensure that current_proc is in the owned map
        self.owned.entry(proc.name).or_default();

        self.collect_stmt(param_map, &proc.body);
        self.update_param_map_declaration(param_map, param_offset, proc.args.len());

        self.param_set = old;
    }
}

pub fn foreign_borrow_signature(arena: &Bump, arity: usize) -> &[bool] {
    // NOTE this means that Roc is responsible for cleaning up resources;
    // the host cannot (currently) take ownership
    let all = bumpalo::vec![in arena; BORROWED; arity];
    all.into_bump_slice()
}

pub fn lowlevel_borrow_signature(arena: &Bump, op: LowLevel) -> &[bool] {
    use LowLevel::*;

    // TODO is true or false more efficient for non-refcounted layouts?
    let irrelevant = OWNED;
    let function = irrelevant;
    let closure_data = irrelevant;
    let owned = OWNED;
    let borrowed = BORROWED;

    // Here we define the borrow signature of low-level operations
    //
    // - arguments with non-refcounted layouts (ints, floats) are `irrelevant`
    // - arguments that we may want to update destructively must be Owned
    // - other refcounted arguments are Borrowed
    match op {
        ListLen | StrIsEmpty | StrCountGraphemes => arena.alloc_slice_copy(&[borrowed]),
        ListSet => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),
        ListGetUnsafe => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        ListConcat => arena.alloc_slice_copy(&[owned, owned]),
        StrConcat => arena.alloc_slice_copy(&[owned, borrowed]),
        StrTrim => arena.alloc_slice_copy(&[owned]),
        StrTrimLeft => arena.alloc_slice_copy(&[owned]),
        StrTrimRight => arena.alloc_slice_copy(&[owned]),
        StrSplit => arena.alloc_slice_copy(&[borrowed, borrowed]),
        StrToNum => arena.alloc_slice_copy(&[borrowed]),
        ListSingle => arena.alloc_slice_copy(&[irrelevant]),
        // List.repeat : elem, Nat -> List.elem
        ListRepeat => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        ListReverse => arena.alloc_slice_copy(&[owned]),
        ListPrepend => arena.alloc_slice_copy(&[owned, owned]),
        StrJoinWith => arena.alloc_slice_copy(&[borrowed, borrowed]),
        ListJoin => arena.alloc_slice_copy(&[irrelevant]),
        ListMap | ListMapWithIndex => arena.alloc_slice_copy(&[owned, function, closure_data]),
        ListMap2 => arena.alloc_slice_copy(&[owned, owned, function, closure_data]),
        ListMap3 => arena.alloc_slice_copy(&[owned, owned, owned, function, closure_data]),
        ListMap4 => arena.alloc_slice_copy(&[owned, owned, owned, owned, function, closure_data]),
        ListKeepIf | ListKeepOks | ListKeepErrs | ListAny | ListAll => {
            arena.alloc_slice_copy(&[owned, function, closure_data])
        }
        ListContains => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        ListRange => arena.alloc_slice_copy(&[irrelevant, irrelevant]),
        ListWalk | ListWalkUntil | ListWalkBackwards => {
            arena.alloc_slice_copy(&[owned, owned, function, closure_data])
        }
        ListSortWith => arena.alloc_slice_copy(&[owned, function, closure_data]),
        ListFindUnsafe => arena.alloc_slice_copy(&[owned, function, closure_data]),

        // TODO when we have lists with capacity (if ever)
        // List.append should own its first argument
        ListAppend => arena.alloc_slice_copy(&[owned, owned]),
        ListSublist => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),
        ListDropAt => arena.alloc_slice_copy(&[owned, irrelevant]),
        ListSwap => arena.alloc_slice_copy(&[owned, irrelevant, irrelevant]),

        Eq | NotEq => arena.alloc_slice_copy(&[borrowed, borrowed]),

        And | Or | NumAdd | NumAddWrap | NumAddChecked | NumAddSaturated | NumSub | NumSubWrap
        | NumSubChecked | NumSubSaturated | NumMul | NumMulWrap | NumMulChecked | NumGt
        | NumGte | NumLt | NumLte | NumCompare | NumDivUnchecked | NumDivCeilUnchecked
        | NumRemUnchecked | NumIsMultipleOf | NumPow | NumPowInt | NumBitwiseAnd
        | NumBitwiseXor | NumBitwiseOr | NumShiftLeftBy | NumShiftRightBy | NumShiftRightZfBy => {
            arena.alloc_slice_copy(&[irrelevant, irrelevant])
        }

        NumToStr | NumAbs | NumNeg | NumSin | NumCos | NumSqrtUnchecked | NumLogUnchecked
        | NumRound | NumCeiling | NumFloor | NumToFloat | Not | NumIsFinite | NumAtan | NumAcos
        | NumAsin | NumIntCast | NumToIntChecked => arena.alloc_slice_copy(&[irrelevant]),
        NumBytesToU16 => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        NumBytesToU32 => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        StrStartsWith | StrEndsWith => arena.alloc_slice_copy(&[owned, borrowed]),
        StrStartsWithCodePt => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        StrFromUtf8 => arena.alloc_slice_copy(&[owned]),
        StrFromUtf8Range => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        StrToUtf8 => arena.alloc_slice_copy(&[owned]),
        StrRepeat => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        StrFromInt | StrFromFloat => arena.alloc_slice_copy(&[irrelevant]),
        Hash => arena.alloc_slice_copy(&[borrowed, irrelevant]),
        DictSize => arena.alloc_slice_copy(&[borrowed]),
        DictEmpty => &[],
        DictInsert => arena.alloc_slice_copy(&[owned, owned, owned]),
        DictRemove => arena.alloc_slice_copy(&[owned, borrowed]),
        DictContains => arena.alloc_slice_copy(&[borrowed, borrowed]),
        DictGetUnsafe => arena.alloc_slice_copy(&[borrowed, borrowed]),
        DictKeys | DictValues => arena.alloc_slice_copy(&[borrowed]),
        DictUnion | DictDifference | DictIntersection => arena.alloc_slice_copy(&[owned, borrowed]),

        // borrow function argument so we don't have to worry about RC of the closure
        DictWalk => arena.alloc_slice_copy(&[owned, owned, function, closure_data]),

        SetFromList => arena.alloc_slice_copy(&[owned]),

        ExpectTrue => arena.alloc_slice_copy(&[irrelevant]),

        PtrCast | RefCountInc | RefCountDec => {
            unreachable!("Only inserted *after* borrow checking: {:?}", op);
        }
    }
}

fn make_successor_mapping<'a>(
    arena: &'a Bump,
    procs: &MutMap<(Symbol, ProcLayout<'_>), Proc<'a>>,
) -> MutMap<Symbol, Vec<'a, Symbol>> {
    let mut result = MutMap::with_capacity_and_hasher(procs.len(), default_hasher());

    for (key, proc) in procs {
        let mut call_info = CallInfo {
            keys: Vec::new_in(arena),
        };
        call_info_stmt(arena, &proc.body, &mut call_info);

        let mut keys = call_info.keys;
        keys.sort_unstable();
        keys.dedup();

        result.insert(key.0, keys);
    }

    result
}

struct CallInfo<'a> {
    keys: Vec<'a, Symbol>,
}

fn call_info_call<'a>(call: &crate::ir::Call<'a>, info: &mut CallInfo<'a>) {
    use crate::ir::CallType::*;

    match call.call_type {
        ByName { name, .. } => {
            info.keys.push(name);
        }
        Foreign { .. } => {}
        LowLevel { .. } => {}
        HigherOrder(_) => {}
    }
}

fn call_info_stmt<'a>(arena: &'a Bump, stmt: &Stmt<'a>, info: &mut CallInfo<'a>) {
    use Stmt::*;

    let mut stack = bumpalo::vec![ in arena; stmt ];

    while let Some(stmt) = stack.pop() {
        match stmt {
            Join {
                remainder: v,
                body: b,
                ..
            } => {
                stack.push(v);
                stack.push(b);
            }
            Let(_, expr, _, cont) => {
                if let Expr::Call(call) = expr {
                    call_info_call(call, info);
                }
                stack.push(cont);
            }
            Switch {
                branches,
                default_branch,
                ..
            } => {
                stack.extend(branches.iter().map(|b| &b.2));
                stack.push(default_branch.1);
            }
            Refcounting(_, _) => unreachable!("these have not been introduced yet"),

            Ret(_) | Jump(_, _) | RuntimeError(_) => {
                // these are terminal, do nothing
            }
        }
    }
}
