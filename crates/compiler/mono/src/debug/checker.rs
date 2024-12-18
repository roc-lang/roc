//! Type-checking of the generated [ir][crate::ir::Proc].

use bumpalo::Bump;
use roc_collections::{MutMap, VecMap, VecSet};
use roc_module::symbol::Symbol;

use crate::{
    ir::{
        Call, CallSpecId, CallType, ErasedField, Expr, HigherOrderLowLevel, JoinPointId,
        ListLiteralElement, ModifyRc, Param, Proc, ProcLayout, Stmt,
    },
    layout::{
        Builtin, FunctionPointer, InLayout, Layout, LayoutInterner, LayoutRepr, STLayoutInterner,
        TagIdIntType, UnionLayout,
    },
};

pub enum UseKind {
    Ret,
    TagExpr,
    TagReuse,
    TagPayloadArg,
    ListElemExpr,
    CallArg,
    JumpArg,
    CrashArg,
    SwitchCond,
    ExpectCond,
    ExpectLookup,
    ErasedMake(ErasedField),
    Erased,
    FunctionPointer,
    Alloca,
}

pub enum ProblemKind<'a> {
    RedefinedSymbol {
        symbol: Symbol,
        old_line: usize,
    },
    NoSymbolInScope {
        symbol: Symbol,
    },
    SymbolUseMismatch {
        symbol: Symbol,
        def_layout: InLayout<'a>,
        def_line: usize,
        use_layout: InLayout<'a>,
        use_kind: UseKind,
    },
    SymbolDefMismatch {
        symbol: Symbol,
        def_layout: InLayout<'a>,
        expr_layout: InLayout<'a>,
    },
    BadSwitchConditionLayout {
        found_layout: InLayout<'a>,
    },
    DuplicateSwitchBranch {},
    RedefinedJoinPoint {
        id: JoinPointId,
        old_line: usize,
    },
    NoJoinPoint {
        id: JoinPointId,
    },
    JumpArityMismatch {
        def_line: usize,
        num_needed: usize,
        num_given: usize,
    },
    CallingUndefinedProc {
        symbol: Symbol,
        proc_layout: ProcLayout<'a>,
        similar: Vec<ProcLayout<'a>>,
    },
    PtrToUndefinedProc {
        symbol: Symbol,
    },
    DuplicateCallSpecId {
        old_call_line: usize,
    },
    StructIndexOOB {
        structure: Symbol,
        def_line: usize,
        index: u64,
        size: usize,
    },
    NotAStruct {
        structure: Symbol,
        def_line: usize,
    },
    IndexingTagIdNotInUnion {
        structure: Symbol,
        def_line: usize,
        tag_id: u16,
        union_layout: InLayout<'a>,
    },
    TagUnionStructIndexOOB {
        structure: Symbol,
        def_line: usize,
        tag_id: u16,
        index: u64,
        size: usize,
    },
    IndexIntoNullableTag {
        structure: Symbol,
        def_line: usize,
        tag_id: u16,
        union_layout: InLayout<'a>,
    },
    UnboxNotABox {
        symbol: Symbol,
        def_line: usize,
    },
    CreatingTagIdNotInUnion {
        tag_id: u16,
        union_layout: InLayout<'a>,
    },
    CreateTagPayloadMismatch {
        num_needed: usize,
        num_given: usize,
    },
    ErasedMakeValueNotBoxed {
        symbol: Symbol,
        def_layout: InLayout<'a>,
        def_line: usize,
    },
    ErasedMakeCalleeNotFunctionPointer {
        symbol: Symbol,
        def_layout: InLayout<'a>,
        def_line: usize,
    },
    ErasedLoadValueNotBoxed {
        symbol: Symbol,
        target_layout: InLayout<'a>,
    },
    ErasedLoadCalleeNotFunctionPointer {
        symbol: Symbol,
        target_layout: InLayout<'a>,
    },
}

pub struct Problem<'a> {
    pub proc: &'a Proc<'a>,
    pub proc_layout: ProcLayout<'a>,
    pub line: usize,
    pub kind: ProblemKind<'a>,
}

type Procs<'a> = MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>;
pub struct Problems<'a>(pub(crate) Vec<Problem<'a>>);

impl<'a> Problems<'a> {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

pub fn check_procs<'a>(
    arena: &'a Bump,
    interner: &mut STLayoutInterner<'a>,
    procs: &Procs<'a>,
) -> Problems<'a> {
    let mut problems = Default::default();

    for ((_, proc_layout), proc) in procs.iter() {
        let mut ctx = Ctx {
            arena,
            interner,
            proc,
            proc_layout: *proc_layout,
            ret_layout: proc.ret_layout,
            problems: &mut problems,
            call_spec_ids: Default::default(),
            procs,
            venv: Default::default(),
            joinpoints: Default::default(),
            line: 0,
        };
        ctx.check_proc(proc);
    }

    Problems(problems)
}

type VEnv<'a> = VecMap<Symbol, (usize, InLayout<'a>)>;
type JoinPoints<'a> = VecMap<JoinPointId, (usize, &'a [Param<'a>])>;
type CallSpecIds = VecMap<CallSpecId, usize>;
struct Ctx<'a, 'r> {
    arena: &'a Bump,
    interner: &'r mut STLayoutInterner<'a>,
    problems: &'r mut Vec<Problem<'a>>,
    proc: &'r Proc<'a>,
    proc_layout: ProcLayout<'a>,
    procs: &'r Procs<'a>,
    call_spec_ids: CallSpecIds,
    ret_layout: InLayout<'a>,
    venv: VEnv<'a>,
    joinpoints: JoinPoints<'a>,
    line: usize,
}

impl<'a, 'r> Ctx<'a, 'r> {
    fn problem(&mut self, problem_kind: ProblemKind<'a>) {
        self.problems.push(Problem {
            proc: self.arena.alloc(self.proc.clone()),
            proc_layout: self.proc_layout,
            line: self.line,
            kind: problem_kind,
        })
    }

    fn in_scope<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let old_venv = self.venv.clone();
        let r = f(self);
        self.venv = old_venv;
        r
    }

    fn resolve(&self, mut layout: InLayout<'a>) -> InLayout<'a> {
        // Note that we are more aggressive than the usual `runtime_representation`
        // here because we need strict equality, and so cannot unwrap lambda sets
        // lazily.
        loop {
            layout = self.interner.chase_recursive_in(layout);
            match self.interner.get_repr(layout) {
                LayoutRepr::LambdaSet(ls) => layout = ls.representation,
                _ => return layout,
            }
        }
    }

    fn not_equiv(&mut self, layout1: InLayout<'a>, layout2: InLayout<'a>) -> bool {
        !self
            .interner
            .equiv(self.resolve(layout1), self.resolve(layout2))
    }

    fn insert(&mut self, symbol: Symbol, layout: InLayout<'a>) {
        if let Some((old_line, _)) = self.venv.insert(symbol, (self.line, layout)) {
            self.problem(ProblemKind::RedefinedSymbol { symbol, old_line })
        }
    }

    fn check_sym_exists(&mut self, symbol: Symbol) {
        if !self.venv.contains_key(&symbol) {
            self.problem(ProblemKind::NoSymbolInScope { symbol })
        }
    }

    fn with_sym_layout<T>(
        &mut self,
        symbol: Symbol,
        f: impl FnOnce(&mut Self, usize, InLayout<'a>) -> Option<T>,
    ) -> Option<T> {
        if let Some(&(def_line, layout)) = self.venv.get(&symbol) {
            f(self, def_line, layout)
        } else {
            self.problem(ProblemKind::NoSymbolInScope { symbol });
            None
        }
    }

    fn check_sym_layout(
        &mut self,
        symbol: Symbol,
        expected_layout: InLayout<'a>,
        use_kind: UseKind,
    ) {
        if let Some(&(def_line, layout)) = self.venv.get(&symbol) {
            if self.not_equiv(layout, expected_layout) {
                self.problem(ProblemKind::SymbolUseMismatch {
                    symbol,
                    def_layout: layout,
                    def_line,
                    use_layout: expected_layout,
                    use_kind,
                });
            }
        } else {
            self.problem(ProblemKind::NoSymbolInScope { symbol })
        }
    }

    fn check_proc(&mut self, proc: &Proc<'a>) {
        for (lay, arg) in proc.args.iter() {
            self.insert(*arg, *lay);
        }

        self.check_stmt(&proc.body)
    }

    fn check_stmt(&mut self, body: &Stmt<'a>) {
        self.line += 1;

        match body {
            Stmt::Let(x, e, x_layout, rest) => {
                if let Some(e_layout) = self.check_expr(e, *x_layout) {
                    if self.not_equiv(e_layout, *x_layout) {
                        self.problem(ProblemKind::SymbolDefMismatch {
                            symbol: *x,
                            def_layout: *x_layout,
                            expr_layout: e_layout,
                        })
                    }
                }
                self.insert(*x, *x_layout);
                self.check_stmt(rest);
            }
            Stmt::Switch {
                cond_symbol,
                cond_layout,
                branches,
                default_branch,
                ret_layout: _,
            } => {
                self.check_sym_layout(*cond_symbol, *cond_layout, UseKind::SwitchCond);
                let layout = self.resolve(*cond_layout);
                match self.interner.get_repr(layout) {
                    LayoutRepr::Builtin(Builtin::Int(_)) => {}
                    LayoutRepr::Builtin(Builtin::Bool) => {}
                    _ => self.problem(ProblemKind::BadSwitchConditionLayout {
                        found_layout: *cond_layout,
                    }),
                }

                // TODO: need to adjust line numbers as we step through, and depending on whether
                // the switch is printed as true/false or a proper switch.
                let mut seen_branches = VecSet::with_capacity(branches.len());
                for (match_no, _branch_info, branch) in branches.iter() {
                    if seen_branches.insert(match_no) {
                        self.problem(ProblemKind::DuplicateSwitchBranch {});
                    }
                    self.in_scope(|ctx| ctx.check_stmt(branch));
                }
                let (_branch_info, default_branch) = default_branch;
                self.in_scope(|ctx| ctx.check_stmt(default_branch));
            }
            &Stmt::Ret(sym) => self.check_sym_layout(sym, self.ret_layout, UseKind::Ret),
            &Stmt::Refcounting(rc, rest) => {
                self.check_modify_rc(rc);
                self.check_stmt(rest);
            }
            &Stmt::Dbg { remainder, .. } => {
                self.check_stmt(remainder);
            }
            &Stmt::Expect {
                condition,
                region: _,
                lookups,
                variables: _,
                remainder,
            } => {
                self.check_sym_layout(condition, Layout::BOOL, UseKind::ExpectCond);
                for sym in lookups.iter() {
                    self.check_sym_exists(*sym);
                }
                self.check_stmt(remainder);
            }
            &Stmt::Join {
                id,
                parameters,
                body,
                remainder,
            } => {
                if let Some((old_line, _)) = self.joinpoints.insert(id, (self.line, parameters)) {
                    self.problem(ProblemKind::RedefinedJoinPoint { id, old_line })
                }
                self.in_scope(|ctx| {
                    for Param { symbol, layout } in parameters {
                        ctx.insert(*symbol, *layout);
                    }
                    ctx.check_stmt(body)
                });
                self.line += 1; // `in` line
                self.check_stmt(remainder);
            }
            &Stmt::Jump(id, symbols) => {
                if let Some(&(def_line, parameters)) = self.joinpoints.get(&id) {
                    if symbols.len() != parameters.len() {
                        self.problem(ProblemKind::JumpArityMismatch {
                            def_line,
                            num_needed: parameters.len(),
                            num_given: symbols.len(),
                        });
                    }
                    for (arg, param) in symbols.iter().zip(parameters.iter()) {
                        let Param { symbol: _, layout } = param;
                        self.check_sym_layout(*arg, *layout, UseKind::JumpArg);
                    }
                } else {
                    self.problem(ProblemKind::NoJoinPoint { id });
                }
            }
            &Stmt::Crash(sym, _) => self.check_sym_layout(sym, Layout::STR, UseKind::CrashArg),
        }
    }

    fn check_expr(&mut self, e: &Expr<'a>, target_layout: InLayout<'a>) -> Option<InLayout<'a>> {
        match e {
            Expr::Literal(_) => None,
            Expr::NullPointer => None,
            Expr::Call(call) => self.check_call(call),
            &Expr::Tag {
                tag_layout,
                tag_id,
                arguments,
                reuse,
            } => {
                let interned_layout = self
                    .interner
                    .insert_direct_no_semantic(LayoutRepr::Union(tag_layout));

                if let Some(reuse_token) = reuse {
                    self.check_sym_layout(reuse_token.symbol, interned_layout, UseKind::TagReuse);
                }

                self.check_tag_expr(interned_layout, tag_layout, tag_id, arguments);

                Some(interned_layout)
            }
            Expr::Struct(syms) => {
                for sym in syms.iter() {
                    self.check_sym_exists(*sym);
                }
                // TODO: pass the field order hash down, so we can check this
                None
            }
            &Expr::StructAtIndex {
                index,
                // TODO: pass the field order hash down, so we can check this
                field_layouts: _,
                structure,
            } => self.check_struct_at_index(structure, index),
            Expr::GetTagId {
                structure: _,
                union_layout,
            } => Some(union_layout.tag_id_layout()),
            &Expr::UnionAtIndex {
                structure,
                tag_id,
                union_layout,
                index,
            } => self.with_sym_layout(structure, |ctx, _def_line, layout| {
                ctx.check_union_at_index(structure, layout, union_layout, tag_id, index)
            }),
            &Expr::GetElementPointer {
                structure,
                union_layout,
                indices,
                ..
            } => self.with_sym_layout(structure, |ctx, _def_line, layout| {
                debug_assert!(indices.len() >= 2);

                ctx.check_union_field_ptr_at_index(
                    structure,
                    layout,
                    union_layout,
                    indices[0] as _,
                    indices[1],
                )
            }),
            Expr::Array { elem_layout, elems } => {
                for elem in elems.iter() {
                    match elem {
                        ListLiteralElement::Literal(_) => {}
                        ListLiteralElement::Symbol(sym) => {
                            self.check_sym_layout(*sym, *elem_layout, UseKind::ListElemExpr)
                        }
                    }
                }
                Some(
                    self.interner
                        .insert_direct_no_semantic(LayoutRepr::Builtin(Builtin::List(
                            *elem_layout,
                        ))),
                )
            }
            Expr::EmptyArray => {
                // TODO don't know what the element layout is
                None
            }
            &Expr::ErasedMake { value, callee } => Some(self.check_erased_make(value, callee)),
            &Expr::ErasedLoad { symbol, field } => {
                Some(self.check_erased_load(symbol, field, target_layout))
            }
            &Expr::FunctionPointer { lambda_name } => {
                let lambda_symbol = lambda_name.name();
                let proc = self.procs.iter().find(|((name, proc), _)| {
                    *name == lambda_symbol && proc.niche == lambda_name.niche()
                });
                match proc {
                    None => {
                        self.problem(ProblemKind::PtrToUndefinedProc {
                            symbol: lambda_symbol,
                        });
                        Some(target_layout)
                    }
                    Some(((_, proc_layout), _)) => {
                        let ProcLayout {
                            arguments, result, ..
                        } = proc_layout;

                        let fn_ptr =
                            self.interner
                                .insert_direct_no_semantic(LayoutRepr::FunctionPointer(
                                    FunctionPointer {
                                        args: arguments,
                                        ret: *result,
                                    },
                                ));

                        Some(fn_ptr)
                    }
                }
            }
            &Expr::Reset {
                symbol,
                update_mode: _,
            }
            | &Expr::ResetRef {
                symbol,
                update_mode: _,
            } => {
                self.check_sym_exists(symbol);
                None
            }
            Expr::Alloca {
                initializer,
                element_layout,
            } => {
                if let Some(initializer) = initializer {
                    self.check_sym_exists(*initializer);
                    self.check_sym_layout(*initializer, *element_layout, UseKind::Alloca);
                }

                None
            }
        }
    }

    fn check_struct_at_index(&mut self, structure: Symbol, index: u64) -> Option<InLayout<'a>> {
        self.with_sym_layout(structure, |ctx, def_line, layout| {
            let layout = ctx.resolve(layout);
            match ctx.interner.get_repr(layout) {
                LayoutRepr::Struct(field_layouts) => {
                    if index as usize >= field_layouts.len() {
                        ctx.problem(ProblemKind::StructIndexOOB {
                            structure,
                            def_line,
                            index,
                            size: field_layouts.len(),
                        });
                        None
                    } else {
                        Some(field_layouts[index as usize])
                    }
                }
                _ => {
                    ctx.problem(ProblemKind::NotAStruct {
                        structure,
                        def_line,
                    });
                    None
                }
            }
        })
    }

    fn check_union_at_index(
        &mut self,
        structure: Symbol,
        interned_union_layout: InLayout<'a>,
        union_layout: UnionLayout<'a>,
        tag_id: u16,
        index: u64,
    ) -> Option<InLayout<'a>> {
        let union = self
            .interner
            .insert_direct_no_semantic(LayoutRepr::Union(union_layout));
        self.with_sym_layout(structure, |ctx, def_line, _layout| {
            ctx.check_sym_layout(structure, union, UseKind::TagExpr);

            match get_tag_id_payloads(union_layout, tag_id) {
                TagPayloads::IdNotInUnion => {
                    ctx.problem(ProblemKind::IndexingTagIdNotInUnion {
                        structure,
                        def_line,
                        tag_id,
                        union_layout: interned_union_layout,
                    });
                    None
                }
                TagPayloads::Payloads(payloads) => {
                    if index as usize >= payloads.len() {
                        ctx.problem(ProblemKind::TagUnionStructIndexOOB {
                            structure,
                            def_line,
                            tag_id,
                            index,
                            size: payloads.len(),
                        });
                        return None;
                    }
                    let layout = payloads[index as usize];
                    Some(layout)
                }
            }
        })
    }

    fn check_union_field_ptr_at_index(
        &mut self,
        structure: Symbol,
        interned_union_layout: InLayout<'a>,
        union_layout: UnionLayout<'a>,
        tag_id: u16,
        index: u64,
    ) -> Option<InLayout<'a>> {
        let union = self
            .interner
            .insert_direct_no_semantic(LayoutRepr::Union(union_layout));

        let field_ptr_layout = match get_tag_id_payloads(union_layout, tag_id) {
            TagPayloads::IdNotInUnion => None,
            TagPayloads::Payloads(payloads) => payloads.get(index as usize).map(|field_layout| {
                self.interner
                    .insert_direct_no_semantic(LayoutRepr::Ptr(*field_layout))
            }),
        };

        self.with_sym_layout(structure, |ctx, def_line, _layout| {
            ctx.check_sym_layout(structure, union, UseKind::TagExpr);

            match get_tag_id_payloads(union_layout, tag_id) {
                TagPayloads::IdNotInUnion => {
                    ctx.problem(ProblemKind::IndexingTagIdNotInUnion {
                        structure,
                        def_line,
                        tag_id,
                        union_layout: interned_union_layout,
                    });
                    None
                }
                TagPayloads::Payloads(payloads) => {
                    if field_ptr_layout.is_none() {
                        ctx.problem(ProblemKind::TagUnionStructIndexOOB {
                            structure,
                            def_line,
                            tag_id,
                            index,
                            size: payloads.len(),
                        });

                        None
                    } else {
                        field_ptr_layout
                    }
                }
            }
        })
    }

    fn check_call(&mut self, call: &Call<'a>) -> Option<InLayout<'a>> {
        let Call {
            call_type,
            arguments,
        } = call;

        match call_type {
            CallType::ByName {
                name,
                ret_layout,
                arg_layouts,
                specialization_id,
            } => {
                let proc_layout = ProcLayout {
                    arguments: arg_layouts,
                    result: *ret_layout,
                    niche: name.niche(),
                };
                if !self.procs.contains_key(&(name.name(), proc_layout)) {
                    let similar = self
                        .procs
                        .keys()
                        .filter(|(sym, _)| *sym == name.name())
                        .map(|(_, lay)| *lay)
                        .collect();
                    self.problem(ProblemKind::CallingUndefinedProc {
                        symbol: name.name(),
                        proc_layout,
                        similar,
                    });
                }
                for (arg, wanted_layout) in arguments.iter().zip(arg_layouts.iter()) {
                    self.check_sym_layout(*arg, *wanted_layout, UseKind::CallArg);
                }
                if let Some(old_call_line) =
                    self.call_spec_ids.insert(*specialization_id, self.line)
                {
                    self.problem(ProblemKind::DuplicateCallSpecId { old_call_line });
                }
                Some(*ret_layout)
            }
            CallType::ByPointer {
                pointer,
                ret_layout,
                arg_layouts,
            } => {
                let expected_layout =
                    self.interner
                        .insert_direct_no_semantic(LayoutRepr::FunctionPointer(FunctionPointer {
                            args: arg_layouts,
                            ret: *ret_layout,
                        }));
                self.check_sym_layout(*pointer, expected_layout, UseKind::FunctionPointer);
                for (arg, wanted_layout) in arguments.iter().zip(arg_layouts.iter()) {
                    self.check_sym_layout(*arg, *wanted_layout, UseKind::CallArg);
                }
                Some(*ret_layout)
            }
            CallType::HigherOrder(HigherOrderLowLevel {
                op: _,
                closure_env_layout: _,
                update_mode: _,
                passed_function: _,
            }) => {
                // TODO
                None
            }
            CallType::Foreign {
                foreign_symbol: _,
                ret_layout,
            } => Some(*ret_layout),
            CallType::LowLevel {
                op: _,
                update_mode: _,
            } => None,
        }
    }

    fn check_tag_expr(
        &mut self,
        interned_union_layout: InLayout<'a>,
        union_layout: UnionLayout<'a>,
        tag_id: u16,
        arguments: &[Symbol],
    ) {
        match get_tag_id_payloads(union_layout, tag_id) {
            TagPayloads::IdNotInUnion => {
                self.problem(ProblemKind::CreatingTagIdNotInUnion {
                    tag_id,
                    union_layout: interned_union_layout,
                });
            }
            TagPayloads::Payloads(payloads) => {
                if arguments.len() != payloads.len() {
                    self.problem(ProblemKind::CreateTagPayloadMismatch {
                        num_needed: payloads.len(),
                        num_given: arguments.len(),
                    });
                }
                for (arg, wanted_layout) in arguments.iter().zip(payloads.iter()) {
                    self.check_sym_layout(*arg, *wanted_layout, UseKind::TagPayloadArg);
                }
            }
        }
    }

    fn check_modify_rc(&mut self, rc: ModifyRc) {
        use ModifyRc::*;

        match rc {
            Inc(sym, _) | Dec(sym) | DecRef(sym) | Free(sym) => {
                // TODO: also check that sym layout needs refcounting
                self.check_sym_exists(sym);
            }
        }
    }

    fn check_erased_make(&mut self, value: Option<Symbol>, callee: Symbol) -> InLayout<'a> {
        if let Some(value) = value {
            self.with_sym_layout(value, |this, def_line, layout| {
                let repr = this.interner.get_repr(layout);
                if !matches!(
                    repr,
                    LayoutRepr::Union(UnionLayout::NullableUnwrapped { .. })
                ) {
                    this.problem(ProblemKind::ErasedMakeValueNotBoxed {
                        symbol: value,
                        def_layout: layout,
                        def_line,
                    });
                }

                Option::<()>::None
            });
        }
        self.with_sym_layout(callee, |this, def_line, layout| {
            let repr = this.interner.get_repr(layout);
            if !matches!(repr, LayoutRepr::FunctionPointer(_)) {
                this.problem(ProblemKind::ErasedMakeCalleeNotFunctionPointer {
                    symbol: callee,
                    def_layout: layout,
                    def_line,
                });
            }

            Option::<()>::None
        });

        Layout::ERASED
    }

    fn check_erased_load(
        &mut self,
        symbol: Symbol,
        field: ErasedField,
        target_layout: InLayout<'a>,
    ) -> InLayout<'a> {
        self.check_sym_layout(symbol, Layout::ERASED, UseKind::Erased);

        match field {
            ErasedField::Value => {
                let repr = self.interner.get_repr(target_layout);
                if !matches!(
                    repr,
                    LayoutRepr::Union(UnionLayout::NullableUnwrapped { .. })
                ) {
                    self.problem(ProblemKind::ErasedLoadValueNotBoxed {
                        symbol,
                        target_layout,
                    });
                }
            }
            ErasedField::ValuePtr => {
                let repr = self.interner.get_repr(target_layout);
                if !matches!(repr, LayoutRepr::Ptr(_)) {
                    self.problem(ProblemKind::ErasedLoadValueNotBoxed {
                        symbol,
                        target_layout,
                    });
                }
            }
            ErasedField::Callee => {
                let repr = self.interner.get_repr(target_layout);
                if !matches!(repr, LayoutRepr::FunctionPointer(_)) {
                    self.problem(ProblemKind::ErasedLoadCalleeNotFunctionPointer {
                        symbol,
                        target_layout,
                    });
                }
            }
        }

        target_layout
    }
}

enum TagPayloads<'a> {
    IdNotInUnion,
    Payloads(&'a [InLayout<'a>]),
}

fn get_tag_id_payloads(union_layout: UnionLayout, tag_id: TagIdIntType) -> TagPayloads {
    macro_rules! check_tag_id_oob {
        ($len:expr) => {
            if tag_id as usize >= $len {
                return TagPayloads::IdNotInUnion;
            }
        };
    }

    match union_layout {
        UnionLayout::NonRecursive(union) => {
            check_tag_id_oob!(union.len());
            let payloads = union[tag_id as usize];
            TagPayloads::Payloads(payloads)
        }
        UnionLayout::Recursive(union) => {
            check_tag_id_oob!(union.len());
            let payloads = union[tag_id as usize];
            TagPayloads::Payloads(payloads)
        }
        UnionLayout::NonNullableUnwrapped(payloads) => {
            if tag_id != 0 {
                TagPayloads::Payloads(&[])
            } else {
                TagPayloads::Payloads(payloads)
            }
        }
        UnionLayout::NullableWrapped {
            nullable_id,
            other_tags,
        } => {
            if tag_id == nullable_id {
                TagPayloads::Payloads(&[])
            } else {
                let num_tags = other_tags.len() + 1;
                check_tag_id_oob!(num_tags);

                let tag_id_idx = if tag_id > nullable_id {
                    tag_id - 1
                } else {
                    tag_id
                };
                let payloads = other_tags[tag_id_idx as usize];
                TagPayloads::Payloads(payloads)
            }
        }
        UnionLayout::NullableUnwrapped {
            nullable_id,
            other_fields,
        } => {
            if tag_id == nullable_id as u16 {
                TagPayloads::Payloads(&[])
            } else {
                check_tag_id_oob!(2);
                TagPayloads::Payloads(other_fields)
            }
        }
    }
}
