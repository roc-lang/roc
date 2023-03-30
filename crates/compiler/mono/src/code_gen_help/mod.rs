use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_module::low_level::LowLevel;
use roc_module::symbol::{IdentIds, ModuleId, Symbol};
use roc_target::TargetInfo;

use crate::ir::{
    Call, CallSpecId, CallType, Expr, HostExposedLayouts, JoinPointId, ModifyRc, Proc, ProcLayout,
    SelfRecursive, Stmt, UpdateModeId,
};
use crate::layout::{
    Builtin, InLayout, LambdaName, Layout, LayoutInterner, Niche, STLayoutInterner, UnionLayout,
};

mod equality;
mod refcount;

const LAYOUT_BOOL: InLayout = Layout::BOOL;
const LAYOUT_UNIT: InLayout = Layout::UNIT;

const ARG_1: Symbol = Symbol::ARG_1;
const ARG_2: Symbol = Symbol::ARG_2;

/// "Infinite" reference count, for static values
/// Ref counts are encoded as negative numbers where isize::MIN represents 1
pub const REFCOUNT_MAX: usize = 0;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum HelperOp {
    Inc,
    Dec,
    DecRef(JoinPointId),
    Reset,
    Eq,
}

impl HelperOp {
    fn is_decref(&self) -> bool {
        matches!(self, Self::DecRef(_))
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
    target_info: TargetInfo,
    layout_isize: InLayout<'a>,
    union_refcount: UnionLayout<'a>,
    specializations: Vec<'a, Specialization<'a>>,
    debug_recursion_depth: usize,
}

impl<'a> CodeGenHelp<'a> {
    pub fn new(arena: &'a Bump, target_info: TargetInfo, home: ModuleId) -> Self {
        let layout_isize = Layout::isize(target_info);

        // Refcount is a boxed isize. TODO: use the new Box layout when dev backends support it
        let union_refcount = UnionLayout::NonNullableUnwrapped(arena.alloc([layout_isize]));

        CodeGenHelp {
            arena,
            home,
            target_info,
            layout_isize,
            union_refcount,
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
            ModifyRc::Inc(..) => HelperOp::Inc,
            ModifyRc::Dec(_) => HelperOp::Dec,
            ModifyRc::DecRef(_) => {
                let jp_decref = JoinPointId(self.create_symbol(ident_ids, "jp_decref"));
                HelperOp::DecRef(jp_decref)
            }
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
        let mut ctx = Context {
            new_linker_data: Vec::new_in(self.arena),
            recursive_union: None,
            op: HelperOp::Reset,
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
            layout_interner.get(called_layout),
            Layout::RecursivePointer(_)
        ) {
            let union_layout = ctx.recursive_union.unwrap();
            layout_interner.insert(Layout::Union(union_layout))
        } else {
            called_layout
        };

        if layout_needs_helper_proc(layout_interner, layout, ctx.op) {
            let proc_name = self.find_or_create_proc(ident_ids, ctx, layout_interner, layout);

            let (ret_layout, arg_layouts): (InLayout<'a>, &'a [InLayout<'a>]) = {
                let arg = self.replace_rec_ptr(ctx, layout_interner, layout);
                match ctx.op {
                    Dec | DecRef(_) => (LAYOUT_UNIT, self.arena.alloc([arg])),
                    Reset => (layout, self.arena.alloc([layout])),
                    Inc => (LAYOUT_UNIT, self.arena.alloc([arg, self.layout_isize])),
                    Eq => (LAYOUT_BOOL, self.arena.alloc([arg, arg])),
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
        let (proc_symbol, proc_layout) = self.create_proc_symbol(ident_ids, ctx, layout);
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
            Inc | Dec | DecRef(_) => (
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
            Eq => (
                LAYOUT_BOOL,
                equality::eq_generic(self, ident_ids, ctx, layout_interner, layout),
            ),
        };

        let args: &'a [(InLayout<'a>, Symbol)] = {
            let roc_value = (layout, ARG_1);
            match ctx.op {
                Inc => {
                    let inc_amount = (self.layout_isize, ARG_2);
                    self.arena.alloc([roc_value, inc_amount])
                }
                Dec | DecRef(_) | Reset => self.arena.alloc([roc_value]),
                Eq => self.arena.alloc([roc_value, (layout, ARG_2)]),
            }
        };

        self.specializations[spec_index].proc = Some(Proc {
            name: LambdaName::no_niche(proc_symbol),
            args,
            body,
            closure_data_layout: None,
            ret_layout,
            is_self_recursive: SelfRecursive::NotSelfRecursive,
            // must_own_arguments: false,
            host_exposed_layouts: HostExposedLayouts::NotHostExposed,
        });

        proc_symbol
    }

    fn create_proc_symbol(
        &self,
        ident_ids: &mut IdentIds,
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
            HelperOp::Inc => ProcLayout {
                arguments: self.arena.alloc([layout, self.layout_isize]),
                result: LAYOUT_UNIT,
                niche: Niche::NONE,
            },
            HelperOp::Dec => ProcLayout {
                arguments: self.arena.alloc([layout]),
                result: LAYOUT_UNIT,
                niche: Niche::NONE,
            },
            HelperOp::Reset => ProcLayout {
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
        let layout = match layout_interner.get(layout) {
            Layout::Builtin(Builtin::List(v)) => {
                let v = self.replace_rec_ptr(ctx, layout_interner, v);
                Layout::Builtin(Builtin::List(v))
            }

            Layout::Builtin(_) => return layout,

            Layout::Struct {
                field_layouts,
                field_order_hash,
            } => {
                let mut new_field_layouts = Vec::with_capacity_in(field_layouts.len(), self.arena);
                for f in field_layouts.iter() {
                    new_field_layouts.push(self.replace_rec_ptr(ctx, layout_interner, *f));
                }
                Layout::Struct {
                    field_layouts: new_field_layouts.into_bump_slice(),
                    field_order_hash,
                }
            }

            Layout::Union(UnionLayout::NonRecursive(tags)) => {
                let mut new_tags = Vec::with_capacity_in(tags.len(), self.arena);
                for fields in tags {
                    let mut new_fields = Vec::with_capacity_in(fields.len(), self.arena);
                    for field in fields.iter() {
                        new_fields.push(self.replace_rec_ptr(ctx, layout_interner, *field))
                    }
                    new_tags.push(new_fields.into_bump_slice());
                }
                Layout::Union(UnionLayout::NonRecursive(new_tags.into_bump_slice()))
            }

            Layout::Union(_) => {
                // we always fully unroll recursive types. That means tha when we find a
                // recursive tag union we can replace it with the layout
                return layout;
            }

            Layout::Boxed(inner) => {
                let inner = self.replace_rec_ptr(ctx, layout_interner, inner);
                Layout::Boxed(inner)
            }

            Layout::LambdaSet(lambda_set) => {
                return self.replace_rec_ptr(ctx, layout_interner, lambda_set.representation)
            }

            // This line is the whole point of the function
            Layout::RecursivePointer(_) => Layout::Union(ctx.recursive_union.unwrap()),
        };
        layout_interner.insert(layout)
    }

    fn union_tail_recursion_fields(
        &self,
        layout_interner: &STLayoutInterner<'a>,
        union: UnionLayout<'a>,
    ) -> (bool, Vec<'a, Option<usize>>) {
        use UnionLayout::*;
        match union {
            NonRecursive(_) => (false, bumpalo::vec![in self.arena]),

            Recursive(tags) => self.union_tail_recursion_fields_help(layout_interner, tags),

            NonNullableUnwrapped(field_layouts) => {
                self.union_tail_recursion_fields_help(layout_interner, &[field_layouts])
            }

            NullableWrapped {
                other_tags: tags, ..
            } => self.union_tail_recursion_fields_help(layout_interner, tags),

            NullableUnwrapped { other_fields, .. } => {
                self.union_tail_recursion_fields_help(layout_interner, &[other_fields])
            }
        }
    }

    fn union_tail_recursion_fields_help(
        &self,
        layout_interner: &STLayoutInterner<'a>,
        tags: &[&'a [InLayout<'a>]],
    ) -> (bool, Vec<'a, Option<usize>>) {
        let mut can_use_tailrec = false;
        let mut tailrec_indices = Vec::with_capacity_in(tags.len(), self.arena);

        for fields in tags.iter() {
            let found_index = fields
                .iter()
                .position(|f| matches!(layout_interner.get(*f), Layout::RecursivePointer(_)));
            tailrec_indices.push(found_index);
            can_use_tailrec |= found_index.is_some();
        }

        (can_use_tailrec, tailrec_indices)
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
    match layout_interner.get(layout) {
        Layout::Builtin(Builtin::Int(_) | Builtin::Float(_) | Builtin::Bool | Builtin::Decimal) => {
            false
        }
        Layout::Builtin(Builtin::Str) => {
            // Str type can use either Zig functions or generated IR, since it's not generic.
            // Eq uses a Zig function, refcount uses generated IR.
            // Both are fine, they were just developed at different times.
            matches!(op, HelperOp::Inc | HelperOp::Dec | HelperOp::DecRef(_))
        }
        Layout::Builtin(Builtin::List(_)) => true,
        Layout::Struct { .. } => true, // note: we do generate a helper for Unit, with just a Stmt::Ret
        Layout::Union(UnionLayout::NonRecursive(tags)) => !tags.is_empty(),
        Layout::Union(_) => true,
        Layout::LambdaSet(_) => true,
        Layout::RecursivePointer(_) => false,
        Layout::Boxed(_) => true,
    }
}
