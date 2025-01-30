use bumpalo::collections::vec::Vec;
use bumpalo::collections::CollectIn;
use bumpalo::Bump;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_target::Target;

use crate::ir::{
    BranchInfo, Call, CallSpecId, CallType, Expr, JoinPointId, Literal, ModifyRc, PassedFunction,
    Proc, ProcLayout, SelfRecursive, Stmt, UpdateModeId,
};
use crate::layout::{
    Builtin, InLayout, LambdaName, Layout, LayoutInterner, LayoutRepr, LayoutWrapper, Niche,
    STLayoutInterner, UnionLayout,
};

mod copy;
mod equality;
mod refcount;

const LAYOUT_BOOL: InLayout = Layout::BOOL;
const LAYOUT_UNIT: InLayout = Layout::UNIT;

const ARG_1: Symbol = Symbol::ARG_1;
const ARG_2: Symbol = Symbol::ARG_2;
const ARG_3: Symbol = Symbol::ARG_3;
const ARG_4: Symbol = Symbol::ARG_4;
const ARG_5: Symbol = Symbol::ARG_5;
const ARG_6: Symbol = Symbol::ARG_6;

/// "Infinite" reference count, for static values
/// Ref counts are encoded as negative numbers where isize::MIN represents 1
pub const REFCOUNT_MAX: usize = 0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HelperOp {
    Inc,
    IncN,
    Dec,
    IndirectInc,
    IndirectIncN,
    IndirectDec,
    DecRef(JoinPointId),
    Reset,
    ResetRef,
    Eq,
    IndirectCopy,
}

impl HelperOp {
    fn is_decref(&self) -> bool {
        matches!(self, Self::DecRef(_))
    }

    fn is_dec(&self) -> bool {
        matches!(self, Self::Dec)
    }

    pub fn is_indirect(&self) -> bool {
        matches!(
            self,
            Self::IndirectInc | Self::IndirectIncN | Self::IndirectDec | Self::IndirectCopy
        )
    }
}

#[derive(Debug)]
struct Specialization<'a> {
    op: HelperOp,
    layout: InLayout<'a>,
    symbol: Symbol,
    proc: Option<Proc<'a>>,
}

#[derive(Debug)]
pub struct Context<'a> {
    new_linker_data: Vec<'a, (Symbol, ProcLayout<'a>)>,
    recursive_union: Option<UnionLayout<'a>>,
    op: HelperOp,
}

/// Generate specialized helper procs for code gen
/// ----------------------------------------------
///
/// Some low level operations need specialized helper procs to traverse data structures at runtime.
/// This includes refcounting, hashing, and equality checks.
///
/// For example, when checking List equality, we need to visit each element and compare them.
/// Depending on the type of the list elements, we may need to recurse deeper into each element.
/// For tag unions, we may need branches for different tag IDs, etc.
///
/// This module creates specialized helper procs for all such operations and types used in the program.
///
/// The backend drives the process, in two steps:
/// 1) When it sees the relevant node, it calls CodeGenHelp to get the replacement IR.
///    CodeGenHelp returns IR for a call to the helper proc, and remembers the specialization.
/// 2) After the backend has generated code for all user procs, it takes the IR for all of the
///    specialized helpers procs, and generates target code for them too.
///
pub struct CodeGenHelp<'a> {
    arena: &'a Bump,
    home: ModuleId,
    target: Target,
    layout_isize: InLayout<'a>,
    specializations: Vec<'a, Specialization<'a>>,
    debug_recursion_depth: usize,
}

impl<'a> CodeGenHelp<'a> {
    pub fn new(arena: &'a Bump, target: Target, home: ModuleId) -> Self {
        let layout_isize = Layout::isize(target);

        CodeGenHelp {
            arena,
            home,
            target,
            layout_isize,
            specializations: Vec::with_capacity_in(16, arena),
            debug_recursion_depth: 0,
        }
    }

    pub fn take_procs(&mut self) -> Vec<'a, Proc<'a>> {
        let procs_iter = self
            .specializations
            .drain(0..)
            .map(|spec| spec.proc.unwrap());
        Vec::from_iter_in(procs_iter, self.arena)
    }

    // ============================================================================
    //
    //              CALL GENERATED PROCS
    //
    // ============================================================================

    /// Expand a `Refcounting` node to a `Let` node that calls a specialized helper proc.
    /// The helper procs themselves are to be generated later with `generate_procs`
    pub fn expand_refcount_stmt(
        &mut self,
        ident_ids: &mut IdentIds,
        layout_interner: &mut STLayoutInterner<'a>,
        layout: InLayout<'a>,
        modify: &ModifyRc,
        following: &'a Stmt<'a>,
    ) -> (&'a Stmt<'a>, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        let op = match modify {
            ModifyRc::Inc(..) => HelperOp::IncN,
            ModifyRc::Dec(_) => HelperOp::Dec,
            ModifyRc::DecRef(_) => {
                let jp_decref = JoinPointId(self.create_symbol(ident_ids, "jp_decref"));
                HelperOp::DecRef(jp_decref)
            }
            ModifyRc::Free(_) => unreachable!("free should be handled by the backend directly"),
        };

        let mut ctx = Context {
            new_linker_data: Vec::new_in(self.arena),
            recursive_union: None,
            op,
        };

        let rc_stmt = refcount::refcount_stmt(
            self,
            ident_ids,
            &mut ctx,
            layout_interner,
            layout,
            modify,
            following,
        );
        (rc_stmt, ctx.new_linker_data)
    }

    pub fn call_reset_refcount(
        &mut self,
        ident_ids: &mut IdentIds,
        layout_interner: &mut STLayoutInterner<'a>,
        layout: InLayout<'a>,
        argument: Symbol,
    ) -> (Expr<'a>, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        self.call_refcount(ident_ids, layout_interner, layout, argument, false)
    }

    /**
    Call a resetref operation. It is similar to reset except it does not recursively decrement it's children when unique.
    */
    pub fn call_resetref_refcount(
        &mut self,
        ident_ids: &mut IdentIds,
        layout_interner: &mut STLayoutInterner<'a>,
        layout: InLayout<'a>,
        argument: Symbol,
    ) -> (Expr<'a>, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        self.call_refcount(ident_ids, layout_interner, layout, argument, true)
    }

    /**
    Call either a reset or a resetref refcount operation.
    */
    fn call_refcount(
        &mut self,
        ident_ids: &mut IdentIds,
        layout_interner: &mut STLayoutInterner<'a>,
        layout: InLayout<'a>,
        argument: Symbol,
        resetref: bool,
    ) -> (Expr<'a>, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        let mut ctx = Context {
            new_linker_data: Vec::new_in(self.arena),
            recursive_union: None,
            op: if resetref {
                HelperOp::ResetRef
            } else {
                HelperOp::Reset
            },
        };

        let proc_name = self.find_or_create_proc(ident_ids, &mut ctx, layout_interner, layout);

        let arguments = self.arena.alloc([argument]);
        let ret_layout = layout;
        let arg_layouts = self.arena.alloc([layout]);
        let expr = Expr::Call(Call {
            call_type: CallType::ByName {
                name: LambdaName::no_niche(proc_name),
                ret_layout,
                arg_layouts,
                specialization_id: CallSpecId::BACKEND_DUMMY,
            },
            arguments,
        });

        (expr, ctx.new_linker_data)
    }

    /// Generate a refcount increment procedure, *without* a Call expression.
    /// *This method should be rarely used* - only when the proc is to be called from Zig.
    /// Otherwise you want to generate the Proc and the Call together, using another method.
    pub fn gen_refcount_proc(
        &mut self,
        ident_ids: &mut IdentIds,
        layout_interner: &mut STLayoutInterner<'a>,
        layout: InLayout<'a>,
        op: HelperOp,
    ) -> (Symbol, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        let mut ctx = Context {
            new_linker_data: Vec::new_in(self.arena),
            recursive_union: None,
            op,
        };

        let proc_name = self.find_or_create_proc(ident_ids, &mut ctx, layout_interner, layout);

        (proc_name, ctx.new_linker_data)
    }

    /// Replace a generic `Lowlevel::Eq` call with a specialized helper proc.
    /// The helper procs themselves are to be generated later with `generate_procs`
    pub fn call_specialized_equals(
        &mut self,
        ident_ids: &mut IdentIds,
        layout_interner: &mut STLayoutInterner<'a>,
        layout: InLayout<'a>,
        arguments: &'a [Symbol],
    ) -> (Expr<'a>, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        let mut ctx = Context {
            new_linker_data: Vec::new_in(self.arena),
            recursive_union: None,
            op: HelperOp::Eq,
        };

        let expr = self
            .call_specialized_op(ident_ids, &mut ctx, layout_interner, layout, arguments)
            .unwrap();

        (expr, ctx.new_linker_data)
    }

    /// Generate a copy procedure, *without* a Call expression.
    /// *This method should be rarely used* - only when the proc is to be called from Zig.
    pub fn gen_copy_proc(
        &mut self,
        ident_ids: &mut IdentIds,
        layout_interner: &mut STLayoutInterner<'a>,
        layout: InLayout<'a>,
    ) -> (Symbol, Vec<'a, (Symbol, ProcLayout<'a>)>) {
        let mut ctx = Context {
            new_linker_data: Vec::new_in(self.arena),
            recursive_union: None,
            op: HelperOp::IndirectCopy,
        };

        let proc_name = self.find_or_create_proc(ident_ids, &mut ctx, layout_interner, layout);

        (proc_name, ctx.new_linker_data)
    }

    // ============================================================================
    //
    //              CALL SPECIALIZED OP
    //
    // ============================================================================

    fn call_specialized_op(
        &mut self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        layout_interner: &mut STLayoutInterner<'a>,
        called_layout: InLayout<'a>,
        arguments: &'a [Symbol],
    ) -> Option<Expr<'a>> {
        use HelperOp::*;

        // debug_assert!(self.debug_recursion_depth < 100);
        self.debug_recursion_depth += 1;

        let layout = if matches!(
            layout_interner.get_repr(called_layout),
            LayoutRepr::RecursivePointer(_)
        ) {
            let union_layout = ctx.recursive_union.unwrap();
            layout_interner.insert_direct_no_semantic(LayoutRepr::Union(union_layout))
        } else {
            called_layout
        };

        if layout_needs_helper_proc(layout_interner, layout, ctx.op) {
            let arena = self.arena;
            let proc_name = self.find_or_create_proc(ident_ids, ctx, layout_interner, layout);

            let (ret_layout, arg_layouts): (InLayout<'a>, &'a [InLayout<'a>]) = {
                let arg = self.replace_rec_ptr(ctx, layout_interner, layout);
                let ptr_arg = layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(arg));

                match ctx.op {
                    Dec | DecRef(_) => (LAYOUT_UNIT, self.arena.alloc([arg])),
                    Reset | ResetRef => (layout, self.arena.alloc([layout])),
                    Inc => (LAYOUT_UNIT, self.arena.alloc([arg])),
                    IncN => (LAYOUT_UNIT, self.arena.alloc([arg, self.layout_isize])),
                    IndirectDec => (LAYOUT_UNIT, arena.alloc([ptr_arg])),
                    IndirectIncN => (LAYOUT_UNIT, arena.alloc([ptr_arg, self.layout_isize])),
                    IndirectInc => (LAYOUT_UNIT, arena.alloc([ptr_arg])),
                    Eq => (LAYOUT_BOOL, self.arena.alloc([arg, arg])),
                    IndirectCopy => (LAYOUT_UNIT, self.arena.alloc([ptr_arg, ptr_arg])),
                }
            };

            Some(Expr::Call(Call {
                call_type: CallType::ByName {
                    name: LambdaName::no_niche(proc_name),
                    ret_layout,
                    arg_layouts,
                    specialization_id: CallSpecId::BACKEND_DUMMY,
                },
                arguments,
            }))
        } else if ctx.op == HelperOp::Eq {
            Some(Expr::Call(Call {
                call_type: CallType::LowLevel {
                    op: LowLevel::Eq,
                    update_mode: UpdateModeId::BACKEND_DUMMY,
                },
                arguments,
            }))
        } else {
            None
        }
    }

    fn find_or_create_proc(
        &mut self,
        ident_ids: &mut IdentIds,
        ctx: &mut Context<'a>,
        layout_interner: &mut STLayoutInterner<'a>,
        orig_layout: InLayout<'a>,
    ) -> Symbol {
        use HelperOp::*;

        let layout = self.replace_rec_ptr(ctx, layout_interner, orig_layout);

        let found = self
            .specializations
            .iter()
            .find(|spec| spec.op == ctx.op && spec.layout == layout);

        if let Some(spec) = found {
            return spec.symbol;
        }

        // Procs can be recursive, so we need to create the symbol before the body is complete
        // But with nested recursion, that means Symbols and Procs can end up in different orders.
        // We want the same order, especially for function indices in Wasm. So create an empty slot and fill it in later.
        let (proc_symbol, proc_layout) =
            self.create_proc_symbol(ident_ids, layout_interner, ctx, layout);
        ctx.new_linker_data.push((proc_symbol, proc_layout));
        let spec_index = self.specializations.len();
        self.specializations.push(Specialization {
            op: ctx.op,
            layout,
            symbol: proc_symbol,
            proc: None,
        });

        // Recursively generate the body of the Proc and sub-procs
        let (ret_layout, body) = match ctx.op {
            Inc | IncN | Dec | DecRef(_) => (
                LAYOUT_UNIT,
                refcount::refcount_generic(
                    self,
                    ident_ids,
                    ctx,
                    layout_interner,
                    layout,
                    Symbol::ARG_1,
                ),
            ),
            IndirectInc | IndirectIncN | IndirectDec => (
                LAYOUT_UNIT,
                refcount::refcount_indirect(
                    self,
                    ident_ids,
                    ctx,
                    layout_interner,
                    layout,
                    Symbol::ARG_1,
                ),
            ),
            Reset => (
                layout,
                refcount::refcount_reset_proc_body(
                    self,
                    ident_ids,
                    ctx,
                    layout_interner,
                    layout,
                    Symbol::ARG_1,
                ),
            ),
            ResetRef => (
                layout,
                refcount::refcount_resetref_proc_body(
                    self,
                    ident_ids,
                    ctx,
                    layout_interner,
                    layout,
                    Symbol::ARG_1,
                ),
            ),
            Eq => (
                LAYOUT_BOOL,
                equality::eq_generic(self, ident_ids, ctx, layout_interner, layout),
            ),
            IndirectCopy => (
                LAYOUT_UNIT,
                copy::copy_indirect(self, ident_ids, ctx, layout_interner, layout),
            ),
        };

        let args: &'a [(InLayout<'a>, Symbol)] = {
            let roc_value = (layout, ARG_1);
            match ctx.op {
                IncN => {
                    let inc_amount = (self.layout_isize, ARG_2);
                    self.arena.alloc([roc_value, inc_amount])
                }
                Inc | Dec | DecRef(_) | Reset | ResetRef => self.arena.alloc([roc_value]),
                IndirectIncN => {
                    let ptr_layout =
                        layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(layout));
                    let inc_amount = (self.layout_isize, ARG_2);
                    self.arena.alloc([(ptr_layout, ARG_1), inc_amount])
                }
                IndirectInc | IndirectDec => {
                    let ptr_layout =
                        layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(layout));
                    self.arena.alloc([(ptr_layout, ARG_1)])
                }
                Eq => self.arena.alloc([roc_value, (layout, ARG_2)]),
                IndirectCopy => {
                    let ptr_layout =
                        layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(layout));
                    self.arena.alloc([(ptr_layout, ARG_1), (ptr_layout, ARG_2)])
                }
            }
        };

        self.specializations[spec_index].proc = Some(Proc {
            name: LambdaName::no_niche(proc_symbol),
            args,
            body,
            closure_data_layout: None,
            ret_layout,
            is_self_recursive: SelfRecursive::NotSelfRecursive,
            is_erased: false,
        });

        proc_symbol
    }

    fn create_proc_symbol(
        &self,
        ident_ids: &mut IdentIds,
        layout_interner: &mut STLayoutInterner<'a>,
        ctx: &mut Context<'a>,
        layout: InLayout<'a>,
    ) -> (Symbol, ProcLayout<'a>) {
        let debug_name = format!(
            "#help{}_{:?}_{:?}",
            self.specializations.len(),
            ctx.op,
            layout
        )
        .replace("Builtin", "");
        let proc_symbol: Symbol = self.create_symbol(ident_ids, &debug_name);

        let proc_layout = match ctx.op {
            HelperOp::IncN => ProcLayout {
                arguments: self.arena.alloc([layout, self.layout_isize]),
                result: LAYOUT_UNIT,
                niche: Niche::NONE,
            },
            HelperOp::Dec | HelperOp::Inc => ProcLayout {
                arguments: self.arena.alloc([layout]),
                result: LAYOUT_UNIT,
                niche: Niche::NONE,
            },
            HelperOp::IndirectIncN => {
                let ptr_layout = layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(layout));

                ProcLayout {
                    arguments: self.arena.alloc([ptr_layout, self.layout_isize]),
                    result: LAYOUT_UNIT,
                    niche: Niche::NONE,
                }
            }
            HelperOp::IndirectDec | HelperOp::IndirectInc => {
                let ptr_layout = layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(layout));

                ProcLayout {
                    arguments: self.arena.alloc([ptr_layout]),
                    result: LAYOUT_UNIT,
                    niche: Niche::NONE,
                }
            }
            HelperOp::Reset | HelperOp::ResetRef => ProcLayout {
                arguments: self.arena.alloc([layout]),
                result: layout,
                niche: Niche::NONE,
            },
            HelperOp::DecRef(_) => unreachable!("No generated Proc for DecRef"),
            HelperOp::Eq => ProcLayout {
                arguments: self.arena.alloc([layout, layout]),
                result: LAYOUT_BOOL,
                niche: Niche::NONE,
            },
            HelperOp::IndirectCopy => {
                let ptr_layout = layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(layout));

                ProcLayout {
                    arguments: self.arena.alloc([ptr_layout, ptr_layout]),
                    result: LAYOUT_UNIT,
                    niche: Niche::NONE,
                }
            }
        };

        (proc_symbol, proc_layout)
    }

    fn create_symbol(&self, ident_ids: &mut IdentIds, debug_name: &str) -> Symbol {
        let ident_id = ident_ids.add_str(debug_name);
        Symbol::new(self.home, ident_id)
    }

    // When creating or looking up Specializations, we need to replace RecursivePointer
    // with the particular Union layout it represents at this point in the tree.
    // For example if a program uses `RoseTree a : [Tree a (List (RoseTree a))]`
    // then it could have both `RoseTree I64` and `RoseTree Str`. In this case it
    // needs *two* specializations for `List(RecursivePointer)`, not just one.
    fn replace_rec_ptr(
        &mut self,
        ctx: &Context<'a>,
        layout_interner: &mut STLayoutInterner<'a>,
        layout: InLayout<'a>,
    ) -> InLayout<'a> {
        let lay = layout_interner.get_repr(layout);
        let semantic = layout_interner.get_semantic(layout);
        let repr = match lay {
            LayoutRepr::Builtin(Builtin::List(v)) => {
                let v = self.replace_rec_ptr(ctx, layout_interner, v);
                LayoutRepr::Builtin(Builtin::List(v))
            }

            LayoutRepr::Builtin(_) => return layout,

            LayoutRepr::Struct(field_layouts) => {
                let mut new_field_layouts = Vec::with_capacity_in(field_layouts.len(), self.arena);
                for f in field_layouts.iter() {
                    new_field_layouts.push(self.replace_rec_ptr(ctx, layout_interner, *f));
                }
                LayoutRepr::Struct(new_field_layouts.into_bump_slice())
            }

            LayoutRepr::Union(UnionLayout::NonRecursive(tags)) => {
                let mut new_tags = Vec::with_capacity_in(tags.len(), self.arena);
                for fields in tags {
                    let mut new_fields = Vec::with_capacity_in(fields.len(), self.arena);
                    for field in fields.iter() {
                        new_fields.push(self.replace_rec_ptr(ctx, layout_interner, *field))
                    }
                    new_tags.push(new_fields.into_bump_slice());
                }
                LayoutRepr::Union(UnionLayout::NonRecursive(new_tags.into_bump_slice()))
            }

            LayoutRepr::Union(_) => {
                // we always fully unroll recursive types. That means tha when we find a
                // recursive tag union we can replace it with the layout
                return layout;
            }

            LayoutRepr::Ptr(inner) => {
                let inner = self.replace_rec_ptr(ctx, layout_interner, inner);
                LayoutRepr::Ptr(inner)
            }

            LayoutRepr::LambdaSet(lambda_set) => {
                return self.replace_rec_ptr(ctx, layout_interner, lambda_set.representation)
            }

            // This line is the whole point of the function
            LayoutRepr::RecursivePointer(_) => LayoutRepr::Union(ctx.recursive_union.unwrap()),

            LayoutRepr::FunctionPointer(_) => return layout,

            LayoutRepr::Erased(_) => return layout,
        };

        layout_interner.insert(Layout::new(LayoutWrapper::Direct(repr), semantic))
    }

    fn union_tail_recursion_fields(
        &self,
        union_in_layout: InLayout<'a>,
        union: UnionLayout<'a>,
    ) -> Option<Vec<'a, Option<usize>>> {
        use UnionLayout::*;
        match union {
            NonRecursive(_) => None,

            Recursive(tags) => self.union_tail_recursion_fields_help(union_in_layout, tags),

            NonNullableUnwrapped(field_layouts) => {
                self.union_tail_recursion_fields_help(union_in_layout, &[field_layouts])
            }

            NullableWrapped {
                other_tags: tags, ..
            } => self.union_tail_recursion_fields_help(union_in_layout, tags),

            NullableUnwrapped { other_fields, .. } => {
                self.union_tail_recursion_fields_help(union_in_layout, &[other_fields])
            }
        }
    }

    fn union_tail_recursion_fields_help(
        &self,
        in_layout: InLayout<'a>,
        tags: &[&'a [InLayout<'a>]],
    ) -> Option<Vec<'a, Option<usize>>> {
        let tailrec_indices = tags
            .iter()
            .map(|fields| fields.iter().position(|f| *f == in_layout))
            .collect_in::<Vec<_>>(self.arena);

        if tailrec_indices.iter().any(|i| i.is_some()) {
            None
        } else {
            Some(tailrec_indices)
        }
    }
}

pub struct CallerProc<'a> {
    pub proc_symbol: Symbol,
    pub proc_layout: ProcLayout<'a>,
    pub proc: Proc<'a>,
}

impl<'a> CallerProc<'a> {
    fn create_symbol(home: ModuleId, ident_ids: &mut IdentIds, debug_name: &str) -> Symbol {
        let ident_id = ident_ids.add_str(debug_name);
        Symbol::new(home, ident_id)
    }

    fn create_caller_proc_symbol(
        home: ModuleId,
        ident_ids: &mut IdentIds,
        operation: &str,
        wrapped_function: Symbol,
    ) -> Symbol {
        let debug_name = format!("#help_{}_{}_{:?}", "caller", operation, wrapped_function,);

        Self::create_symbol(home, ident_ids, &debug_name)
    }

    pub fn new_list_map(
        arena: &'a Bump,
        home: ModuleId,
        ident_ids: &mut IdentIds,
        layout_interner: &mut STLayoutInterner<'a>,
        passed_function: &PassedFunction<'a>,
        capture_layout: Option<InLayout<'a>>,
    ) -> Self {
        assert!(passed_function.argument_layouts.len() <= 4);
        const ARG_SYMBOLS: &[Symbol] = &[ARG_1, ARG_2, ARG_3, ARG_4, ARG_5, ARG_6];

        let argument_layouts = match capture_layout {
            None => passed_function.argument_layouts,
            Some(_capture_layout) => {
                let capture_layout_index = passed_function.argument_layouts.len() - 1;

                #[cfg(debug_assertions)]
                {
                    let passed_capture_layout =
                        passed_function.argument_layouts[capture_layout_index];
                    let repr = layout_interner.get_repr(passed_capture_layout);

                    if let LayoutRepr::LambdaSet(lambda_set) = repr {
                        assert!(layout_interner
                            .equiv(_capture_layout, lambda_set.runtime_representation()));
                    } else {
                        panic!("unexpected layout for capture argument");
                    }
                }

                &passed_function.argument_layouts[..capture_layout_index]
            }
        };

        let capture_symbol = ARG_SYMBOLS[0];
        let return_symbol = ARG_SYMBOLS[1 + argument_layouts.len()];

        let mut ctx = Context {
            new_linker_data: Vec::new_in(arena),
            recursive_union: None,
            op: HelperOp::Eq,
        };

        let ptr_capture_layout = if let Some(capture_layout) = capture_layout {
            layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(capture_layout))
        } else {
            layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(Layout::UNIT))
        };

        let it = argument_layouts
            .iter()
            .map(|l| layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(*l)));
        let ptr_argument_layouts = Vec::from_iter_in(it, arena);

        let ptr_return_layout = layout_interner
            .insert_direct_no_semantic(LayoutRepr::Ptr(passed_function.return_layout));

        let mut arguments = Vec::with_capacity_in(1 + ptr_argument_layouts.len() + 1, arena);
        arguments.push(ptr_capture_layout);
        arguments.extend(ptr_argument_layouts.iter().copied());
        arguments.push(ptr_return_layout);

        let proc_layout = ProcLayout {
            arguments: arguments.into_bump_slice(),
            result: Layout::UNIT,
            niche: Niche::NONE,
        };

        let proc_symbol =
            Self::create_caller_proc_symbol(home, ident_ids, "map", passed_function.name.name());

        ctx.new_linker_data.push((proc_symbol, proc_layout));

        let load_capture = Expr::ptr_load(arena.alloc(capture_symbol));

        let loaded_capture = Self::create_symbol(home, ident_ids, "loaded_capture");
        let call_result = Self::create_symbol(home, ident_ids, "call_result");
        let unit_symbol = Self::create_symbol(home, ident_ids, "unit_symbol");
        let ignored = Self::create_symbol(home, ident_ids, "ignored");

        let loaded_arguments = Vec::from_iter_in(
            (0..argument_layouts.len())
                .map(|i| Self::create_symbol(home, ident_ids, &format!("loaded_argument_{i}"))),
            arena,
        );

        let mut arguments = loaded_arguments.clone();
        if capture_layout.is_some() {
            arguments.push(loaded_capture);
        }

        let call = Expr::Call(Call {
            call_type: CallType::ByName {
                name: passed_function.name,
                ret_layout: passed_function.return_layout,
                arg_layouts: passed_function.argument_layouts,
                specialization_id: passed_function.specialization_id,
            },
            arguments: arguments.into_bump_slice(),
        });

        let ptr_write = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::PtrStore,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: arena.alloc([return_symbol, call_result]),
        });

        let mut body = Stmt::Let(
            call_result,
            call,
            passed_function.return_layout,
            arena.alloc(Stmt::Let(
                ignored,
                ptr_write,
                ptr_return_layout,
                arena.alloc(Stmt::Let(
                    unit_symbol,
                    Expr::Struct(&[]),
                    Layout::UNIT,
                    arena.alloc(Stmt::Ret(unit_symbol)),
                )),
            )),
        );

        let it = loaded_arguments
            .iter()
            .zip(ARG_SYMBOLS.iter().skip(1))
            .zip(argument_layouts.iter())
            .rev();

        for ((loaded_argument, load_argument), argument_layout) in it {
            let load_argument = Expr::ptr_load(arena.alloc(load_argument));

            body = Stmt::Let(
                *loaded_argument,
                load_argument,
                *argument_layout,
                arena.alloc(body),
            );
        }

        if let Some(capture_layout) = capture_layout {
            body = Stmt::Let(
                loaded_capture,
                load_capture,
                capture_layout,
                arena.alloc(body),
            );
        }

        let mut arg_symbols = ARG_SYMBOLS.iter();
        let mut args = Vec::with_capacity_in(1 + ptr_argument_layouts.len() + 1, arena);

        args.push((ptr_capture_layout, *arg_symbols.next().unwrap()));
        for l in &ptr_argument_layouts {
            args.push((*l, *arg_symbols.next().unwrap()));
        }
        args.push((ptr_return_layout, *arg_symbols.next().unwrap()));

        let proc = Proc {
            name: LambdaName::no_niche(proc_symbol),
            args: args.into_bump_slice(),
            body,
            closure_data_layout: None,
            ret_layout: Layout::UNIT,
            is_self_recursive: SelfRecursive::NotSelfRecursive,
            is_erased: false,
        };

        if false {
            home.register_debug_idents(ident_ids);
            println!("{}", proc.to_pretty(layout_interner, 200, true));
        }

        Self {
            proc_symbol,
            proc_layout,
            proc,
        }
    }

    pub fn new_compare(
        arena: &'a Bump,
        home: ModuleId,
        ident_ids: &mut IdentIds,
        layout_interner: &mut STLayoutInterner<'a>,
        passed_function: &PassedFunction<'a>,
        capture_layout: Option<InLayout<'a>>,
    ) -> Self {
        const ARG_SYMBOLS: &[Symbol] = &[ARG_1, ARG_2, ARG_3];

        let argument_layouts = match capture_layout {
            None => passed_function.argument_layouts,
            Some(_) => &passed_function.argument_layouts[1..],
        };

        let capture_symbol = ARG_SYMBOLS[0];

        let mut ctx = Context {
            new_linker_data: Vec::new_in(arena),
            recursive_union: None,
            op: HelperOp::Eq,
        };

        let ptr_capture_layout = if let Some(capture_layout) = capture_layout {
            layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(capture_layout))
        } else {
            layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(Layout::UNIT))
        };

        let it = argument_layouts
            .iter()
            .map(|l| layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(*l)));
        let ptr_argument_layouts = Vec::from_iter_in(it, arena);

        let mut arguments = Vec::with_capacity_in(1 + ptr_argument_layouts.len(), arena);
        arguments.push(ptr_capture_layout);
        arguments.extend(ptr_argument_layouts.iter().copied());

        let proc_layout = ProcLayout {
            arguments: arguments.into_bump_slice(),
            result: Layout::UNIT,
            niche: Niche::NONE,
        };

        let proc_symbol = Self::create_caller_proc_symbol(
            home,
            ident_ids,
            "compare",
            passed_function.name.name(),
        );

        ctx.new_linker_data.push((proc_symbol, proc_layout));

        let load_capture = Expr::ptr_load(arena.alloc(capture_symbol));

        let loaded_capture = Self::create_symbol(home, ident_ids, "loaded_capture");
        let call_result = Self::create_symbol(home, ident_ids, "call_result");

        let loaded_arguments = Vec::from_iter_in(
            (0..argument_layouts.len())
                .map(|i| Self::create_symbol(home, ident_ids, &format!("loaded_argument_{i}"))),
            arena,
        );

        let mut arguments = loaded_arguments.clone();
        if capture_layout.is_some() {
            arguments.push(loaded_capture);
        }

        let call = Expr::Call(Call {
            call_type: CallType::ByName {
                name: passed_function.name,
                ret_layout: passed_function.return_layout,
                arg_layouts: passed_function.argument_layouts,
                specialization_id: passed_function.specialization_id,
            },
            arguments: arguments.into_bump_slice(),
        });

        let mut body = Stmt::Let(
            call_result,
            call,
            passed_function.return_layout,
            arena.alloc(Stmt::Ret(call_result)),
        );

        let it = loaded_arguments
            .iter()
            .zip(ARG_SYMBOLS.iter().skip(1))
            .zip(argument_layouts.iter())
            .rev();

        for ((loaded_argument, load_argument), argument_layout) in it {
            let load_argument = Expr::ptr_load(arena.alloc(load_argument));

            body = Stmt::Let(
                *loaded_argument,
                load_argument,
                *argument_layout,
                arena.alloc(body),
            );
        }

        if let Some(capture_layout) = capture_layout {
            body = Stmt::Let(
                loaded_capture,
                load_capture,
                capture_layout,
                arena.alloc(body),
            );
        }

        let mut arg_symbols = ARG_SYMBOLS.iter();
        let mut args = Vec::with_capacity_in(1 + ptr_argument_layouts.len(), arena);

        args.push((ptr_capture_layout, *arg_symbols.next().unwrap()));
        for l in &ptr_argument_layouts {
            args.push((*l, *arg_symbols.next().unwrap()));
        }

        let proc = Proc {
            name: LambdaName::no_niche(proc_symbol),
            args: args.into_bump_slice(),
            body,
            closure_data_layout: None,
            ret_layout: Layout::BOOL,
            is_self_recursive: SelfRecursive::NotSelfRecursive,
            is_erased: false,
        };

        if false {
            home.register_debug_idents(ident_ids);
            println!("{}", proc.to_pretty(layout_interner, 200, true));
        }

        Self {
            proc_symbol,
            proc_layout,
            proc,
        }
    }
}

fn let_lowlevel<'a>(
    arena: &'a Bump,
    result_layout: InLayout<'a>,
    result: Symbol,
    op: LowLevel,
    arguments: &[Symbol],
    next: &'a Stmt<'a>,
) -> Stmt<'a> {
    Stmt::Let(
        result,
        Expr::Call(Call {
            call_type: CallType::LowLevel {
                op,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: arena.alloc_slice_copy(arguments),
        }),
        result_layout,
        next,
    )
}

fn layout_needs_helper_proc<'a>(
    layout_interner: &STLayoutInterner<'a>,
    layout: InLayout<'a>,
    op: HelperOp,
) -> bool {
    match layout_interner.get_repr(layout) {
        LayoutRepr::Builtin(
            Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal,
        ) => false,
        LayoutRepr::Builtin(Builtin::Str) => {
            // Str type can use either Zig functions or generated IR, since it's not generic.
            // Eq uses a Zig function, refcount uses generated IR.
            // Both are fine, they were just developed at different times.
            matches!(
                op,
                HelperOp::Inc | HelperOp::IncN | HelperOp::Dec | HelperOp::DecRef(_)
            )
        }
        LayoutRepr::Builtin(Builtin::List(_)) => true,
        LayoutRepr::Struct { .. } => true, // note: we do generate a helper for Unit, with just a Stmt::Ret
        LayoutRepr::Union(UnionLayout::NonRecursive(tags)) => !tags.is_empty(),
        LayoutRepr::Union(_) => true,
        LayoutRepr::LambdaSet(_) => true,
        LayoutRepr::RecursivePointer(_) => false,
        LayoutRepr::Ptr(_) => false,
        LayoutRepr::FunctionPointer(_) => false,
        LayoutRepr::Erased(_) => true,
    }
}

pub fn test_helper<'a>(
    env: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    layout_interner: &mut STLayoutInterner<'a>,
    main_proc: &Proc<'a>,
) -> Proc<'a> {
    let name = LambdaName::no_niche(env.create_symbol(ident_ids, "test_main"));

    let it = (0..main_proc.args.len()).map(|i| env.create_symbol(ident_ids, &format!("arg_{i}")));
    let arguments = Vec::from_iter_in(it, env.arena).into_bump_slice();

    let it = arguments
        .iter()
        .zip(main_proc.args.iter())
        .map(|(s, (l, _))| (*l, *s));
    let args = Vec::from_iter_in(it, env.arena).into_bump_slice();

    //    tag: u64,
    //    error_msg: *mut RocStr,
    //    value: MaybeUninit<T>,
    let fields = [Layout::U64, Layout::U64, main_proc.ret_layout];
    let repr = LayoutRepr::Struct(env.arena.alloc(fields));
    let output_layout = layout_interner.insert_direct_no_semantic(repr);
    let body = test_helper_body(
        env,
        ident_ids,
        layout_interner,
        main_proc,
        arguments,
        output_layout,
    );

    Proc {
        name,
        args,
        body,
        closure_data_layout: None,
        ret_layout: output_layout,
        is_self_recursive: main_proc.is_self_recursive,
        is_erased: false,
    }
}

fn test_helper_body<'a>(
    env: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    layout_interner: &mut STLayoutInterner<'a>,
    main_proc: &Proc<'a>,
    arguments: &'a [Symbol],
    output_layout: InLayout<'a>,
) -> Stmt<'a> {
    // let buffer = SetLongJmpBuffer
    let buffer_symbol = env.create_symbol(ident_ids, "buffer");
    let buffer_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::SetLongJmpBuffer,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: &[],
    });
    let buffer_stmt = |next| Stmt::Let(buffer_symbol, buffer_expr, Layout::U64, next);

    let field_layouts = env.arena.alloc([Layout::U64, Layout::U64]);
    let ret_layout = layout_interner.insert_direct_no_semantic(LayoutRepr::Struct(field_layouts));

    let setjmp_symbol = env.create_symbol(ident_ids, "setjmp");
    let setjmp_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::SetJmp,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: env.arena.alloc([buffer_symbol]),
    });
    let setjmp_stmt = |next| Stmt::Let(setjmp_symbol, setjmp_expr, ret_layout, next);

    let is_longjmp_symbol = env.create_symbol(ident_ids, "is_longjmp");
    let is_longjmp_expr = Expr::StructAtIndex {
        index: 0,
        field_layouts,
        structure: setjmp_symbol,
    };
    let is_longjmp_stmt = |next| Stmt::Let(is_longjmp_symbol, is_longjmp_expr, Layout::U64, next);

    let tag_symbol = env.create_symbol(ident_ids, "tag");
    let tag_expr = Expr::StructAtIndex {
        index: 1,
        field_layouts,
        structure: setjmp_symbol,
    };
    let tag_stmt = |next| Stmt::Let(tag_symbol, tag_expr, Layout::U64, next);

    // normal path, no panics
    let if_zero_stmt = {
        let it = main_proc.args.iter().map(|(a, _)| *a);
        let arg_layouts = Vec::from_iter_in(it, env.arena).into_bump_slice();

        let result_symbol = env.create_symbol(ident_ids, "result");
        let result_expr = Expr::Call(Call {
            call_type: CallType::ByName {
                name: main_proc.name,
                ret_layout: main_proc.ret_layout,
                arg_layouts,
                specialization_id: CallSpecId::BACKEND_DUMMY,
            },
            arguments,
        });
        let result = |next| Stmt::Let(result_symbol, result_expr, main_proc.ret_layout, next);

        let ok_tag_symbol = env.create_symbol(ident_ids, "ok_tag");
        let ok_tag_expr = Expr::Literal(Literal::Int((0i128).to_ne_bytes()));
        let ok_tag = |next| Stmt::Let(ok_tag_symbol, ok_tag_expr, Layout::U64, next);

        let msg_ptr_symbol = env.create_symbol(ident_ids, "msg_ptr");
        let msg_ptr_expr = Expr::Literal(Literal::Int((0i128).to_ne_bytes()));
        let msg_ptr = |next| Stmt::Let(msg_ptr_symbol, msg_ptr_expr, Layout::U64, next);

        // construct the record
        let output_symbol = env.create_symbol(ident_ids, "output_ok");
        let fields = [ok_tag_symbol, msg_ptr_symbol, result_symbol];
        let output_expr = Expr::Struct(env.arena.alloc(fields));
        let output = |next| Stmt::Let(output_symbol, output_expr, output_layout, next);

        let arena = env.arena;
        result(arena.alloc(
            //
            ok_tag(arena.alloc(
                //
                msg_ptr(arena.alloc(
                    //
                    output(arena.alloc(
                        //
                        Stmt::Ret(output_symbol),
                    )),
                )),
            )),
        ))
    };

    // a longjmp/panic occurred
    let if_nonzero_stmt = {
        let alloca_symbol = env.create_symbol(ident_ids, "alloca");
        let alloca_expr = Expr::Alloca {
            element_layout: main_proc.ret_layout,
            initializer: None,
        };
        let alloca = |next| Stmt::Let(alloca_symbol, alloca_expr, Layout::U64, next);

        let load_symbol = env.create_symbol(ident_ids, "load");
        let load_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::PtrLoad,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: env.arena.alloc([alloca_symbol]),
        });
        let load = |next| Stmt::Let(load_symbol, load_expr, main_proc.ret_layout, next);

        // construct the record
        let output_symbol = env.create_symbol(ident_ids, "output_err");
        // is_longjmp_symbol is a pointer to the error message
        let fields = [tag_symbol, is_longjmp_symbol, load_symbol];
        let output_expr = Expr::Struct(env.arena.alloc(fields));
        let output = |next| Stmt::Let(output_symbol, output_expr, output_layout, next);

        let arena = env.arena;
        arena.alloc(alloca(arena.alloc(
            //
            load(arena.alloc(
                //
                output(arena.alloc(
                    //
                    Stmt::Ret(output_symbol),
                )),
            )),
        )))
    };

    buffer_stmt(env.arena.alloc(
        //
        setjmp_stmt(env.arena.alloc(
            //
            is_longjmp_stmt(env.arena.alloc(
                //
                tag_stmt(env.arena.alloc(
                    //
                    switch_if_zero_else(
                        env.arena,
                        is_longjmp_symbol,
                        output_layout,
                        if_zero_stmt,
                        if_nonzero_stmt,
                    ),
                )),
            )),
        )),
    ))
}

pub fn repl_helper<'a>(
    env: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    layout_interner: &mut STLayoutInterner<'a>,
    main_proc: &Proc<'a>,
) -> Proc<'a> {
    let name = LambdaName::no_niche(env.create_symbol(ident_ids, "test_main"));

    // NOTE: main_proc's arguments are ignored here. There can be arguments if the input on the
    // repl is a lambda, but then we don't read the output of the evaluation anyway (and just print
    // the function type)

    //    tag: u64,
    //    error_msg: *mut RocStr,
    //    value: MaybeUninit<T>,
    let fields = [Layout::U64, Layout::U64, main_proc.ret_layout];
    let repr = LayoutRepr::Struct(env.arena.alloc(fields));
    let output_layout = layout_interner.insert_direct_no_semantic(repr);

    let argument_layout = layout_interner.insert_direct_no_semantic(LayoutRepr::Ptr(output_layout));
    let argument_symbol = env.create_symbol(ident_ids, "output");
    let argument = (argument_layout, argument_symbol);

    let args = &env.arena.alloc([argument])[..];

    let body = repl_helper_body(
        env,
        ident_ids,
        layout_interner,
        main_proc,
        argument_symbol,
        output_layout,
    );

    Proc {
        name,
        args,
        body,
        closure_data_layout: None,
        ret_layout: Layout::UNIT,
        is_self_recursive: main_proc.is_self_recursive,
        is_erased: false,
    }
}

fn repl_helper_body<'a>(
    env: &CodeGenHelp<'a>,
    ident_ids: &mut IdentIds,
    layout_interner: &mut STLayoutInterner<'a>,
    main_proc: &Proc<'a>,
    output_symbol: Symbol,
    output_layout: InLayout<'a>,
) -> Stmt<'a> {
    // let buffer = SetLongJmpBuffer
    let buffer_symbol = env.create_symbol(ident_ids, "buffer");
    let buffer_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::SetLongJmpBuffer,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: &[],
    });
    let buffer_stmt = |next| Stmt::Let(buffer_symbol, buffer_expr, Layout::U64, next);

    let field_layouts = env.arena.alloc([Layout::U64, Layout::U64]);
    let ret_layout = layout_interner.insert_direct_no_semantic(LayoutRepr::Struct(field_layouts));

    let setjmp_symbol = env.create_symbol(ident_ids, "setjmp");
    let setjmp_expr = Expr::Call(Call {
        call_type: CallType::LowLevel {
            op: LowLevel::SetJmp,
            update_mode: UpdateModeId::BACKEND_DUMMY,
        },
        arguments: env.arena.alloc([buffer_symbol]),
    });
    let setjmp_stmt = |next| Stmt::Let(setjmp_symbol, setjmp_expr, ret_layout, next);

    let is_longjmp_symbol = env.create_symbol(ident_ids, "is_longjmp");
    let is_longjmp_expr = Expr::StructAtIndex {
        index: 0,
        field_layouts,
        structure: setjmp_symbol,
    };
    let is_longjmp_stmt = |next| Stmt::Let(is_longjmp_symbol, is_longjmp_expr, Layout::U64, next);

    let tag_symbol = env.create_symbol(ident_ids, "tag");
    let tag_expr = Expr::StructAtIndex {
        index: 1,
        field_layouts,
        structure: setjmp_symbol,
    };
    let tag_stmt = |next| Stmt::Let(tag_symbol, tag_expr, Layout::U64, next);

    // normal path, no panics
    let if_zero_stmt = {
        let it = main_proc.args.iter().map(|(a, _)| *a);
        let arg_layouts = Vec::from_iter_in(it, env.arena).into_bump_slice();

        let result_symbol = env.create_symbol(ident_ids, "result");
        let result_expr = Expr::Call(Call {
            call_type: CallType::ByName {
                name: main_proc.name,
                ret_layout: main_proc.ret_layout,
                arg_layouts,
                specialization_id: CallSpecId::BACKEND_DUMMY,
            },
            arguments: &[],
        });
        let result = |next| Stmt::Let(result_symbol, result_expr, main_proc.ret_layout, next);

        let ok_tag_symbol = env.create_symbol(ident_ids, "ok_tag");
        let ok_tag_expr = Expr::Literal(Literal::Int((0i128).to_ne_bytes()));
        let ok_tag = |next| Stmt::Let(ok_tag_symbol, ok_tag_expr, Layout::U64, next);

        let msg_ptr_symbol = env.create_symbol(ident_ids, "msg_ptr");
        let msg_ptr_expr = Expr::Literal(Literal::Int((0i128).to_ne_bytes()));
        let msg_ptr = |next| Stmt::Let(msg_ptr_symbol, msg_ptr_expr, Layout::U64, next);

        // construct the record
        let result_symbol1 = env.create_symbol(ident_ids, "output_ok");
        let fields = [ok_tag_symbol, msg_ptr_symbol, result_symbol];
        let result_expr = Expr::Struct(env.arena.alloc(fields));
        let output = |next| Stmt::Let(result_symbol1, result_expr, output_layout, next);

        let unit_symbol = env.create_symbol(ident_ids, "unit");
        let unit_expr = Expr::ptr_store(env.arena.alloc([output_symbol, result_symbol1]));
        let unit = |next| Stmt::Let(unit_symbol, unit_expr, Layout::UNIT, next);

        let arena = env.arena;
        result(arena.alloc(
            //
            ok_tag(arena.alloc(
                //
                msg_ptr(arena.alloc(
                    //
                    output(arena.alloc(
                        //
                        unit(arena.alloc(
                            //
                            Stmt::Ret(unit_symbol),
                        )),
                    )),
                )),
            )),
        ))
    };

    // a longjmp/panic occurred
    let if_nonzero_stmt = {
        let alloca_symbol = env.create_symbol(ident_ids, "alloca");
        let alloca_expr = Expr::Alloca {
            element_layout: main_proc.ret_layout,
            initializer: None,
        };
        let alloca = |next| Stmt::Let(alloca_symbol, alloca_expr, Layout::U64, next);

        let load_symbol = env.create_symbol(ident_ids, "load");
        let load_expr = Expr::Call(Call {
            call_type: CallType::LowLevel {
                op: LowLevel::PtrLoad,
                update_mode: UpdateModeId::BACKEND_DUMMY,
            },
            arguments: env.arena.alloc([alloca_symbol]),
        });
        let load = |next| Stmt::Let(load_symbol, load_expr, main_proc.ret_layout, next);

        // construct the record
        let result_symbol1 = env.create_symbol(ident_ids, "output_err");
        // is_longjmp_symbol is a pointer to the error message
        let fields = [tag_symbol, is_longjmp_symbol, load_symbol];
        let output_expr = Expr::Struct(env.arena.alloc(fields));
        let output = |next| Stmt::Let(result_symbol1, output_expr, output_layout, next);

        let unit_symbol = env.create_symbol(ident_ids, "unit");
        let unit_expr = Expr::ptr_store(env.arena.alloc([output_symbol, result_symbol1]));
        let unit = |next| Stmt::Let(unit_symbol, unit_expr, Layout::UNIT, next);

        let arena = env.arena;
        arena.alloc(alloca(arena.alloc(
            //
            load(arena.alloc(
                //
                output(arena.alloc(
                    //
                    unit(arena.alloc(
                        //
                        Stmt::Ret(unit_symbol),
                    )),
                )),
            )),
        )))
    };

    buffer_stmt(env.arena.alloc(
        //
        setjmp_stmt(env.arena.alloc(
            //
            is_longjmp_stmt(env.arena.alloc(
                //
                tag_stmt(env.arena.alloc(
                    //
                    switch_if_zero_else(
                        env.arena,
                        is_longjmp_symbol,
                        Layout::UNIT,
                        if_zero_stmt,
                        if_nonzero_stmt,
                    ),
                )),
            )),
        )),
    ))
}

fn switch_if_zero_else<'a>(
    arena: &'a Bump,
    condition_symbol: Symbol,
    return_layout: InLayout<'a>,
    then_branch_stmt: Stmt<'a>,
    else_branch_stmt: &'a Stmt<'a>,
) -> Stmt<'a> {
    let then_branch = (0u64, BranchInfo::None, then_branch_stmt);
    let else_branch = (BranchInfo::None, else_branch_stmt);

    Stmt::Switch {
        cond_symbol: condition_symbol,
        cond_layout: Layout::U64,
        branches: &*arena.alloc([then_branch]),
        default_branch: else_branch,
        ret_layout: return_layout,
    }
}
