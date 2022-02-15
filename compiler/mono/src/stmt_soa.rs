use crate::ir::{CallSpecId, UpdateModeId};
use crate::layout::TagIdIntType;
use crate::layout_soa::{FunctionLayout, Index, Layout, Layouts, Slice};
use roc_module::ident::{ForeignSymbol, TagName};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;
use roc_std::RocDec;
use roc_target::TargetInfo;

static_assertions::assert_eq_size!(Stmt, [u8; 20]);
static_assertions::assert_eq_size!(Expr, [u8; 40]);

use bumpalo::collections::String;
use bumpalo::collections::Vec;
use bumpalo::Bump;
use roc_collections::all::MutMap;
use roc_types::subs::Subs;

#[derive(Clone, Debug)]
pub struct Proc<'a> {
    pub name: Symbol,
    pub args: &'a [(Index<Layout>, Symbol)],
    pub body: Index<Stmt>,
    /// Is the closure data passed as an argument
    pub closure_data_layout: Option<Index<Layout>>,
    pub ret_layout: Index<Layout>,
    // pub is_self_recursive: SelfRecursive,
    // pub must_own_arguments: bool,
    // pub host_exposed_layouts: HostExposedLayouts<'a>,
}

struct Module<'a> {
    procs: Vec<'a, Proc<'a>>,

    stmts: Vec<'a, Stmt>,
    stmt_symbols: Vec<'a, Symbol>,
    stmt_slices: Vec<'a, Index<Stmt>>,

    exprs: Vec<'a, Expr>,
    expr_symbols: Vec<'a, Symbol>,

    literals: Vec<'a, Literal>,

    layouts: Layouts<'a>,

    symbols: Vec<'a, Symbol>,
    branch_infos: Vec<'a, BranchInfo>,
    branches: Vec<'a, (u64, BranchInfo, Index<Stmt>)>,
    parameters: Vec<'a, Param>,
    // string literals are slices into this string
    strings: String<'a>,
    big_numbers: Vec<'a, i128>,
    // unique things
    call_specialization_counter: CallSpecId,
    update_mode_ids: UpdateModeIds,
}

impl<'a> Module<'a> {
    fn new_in(arena: &'a Bump, target_info: TargetInfo) -> Self {
        Self {
            procs: Vec::new_in(arena),
            stmts: Vec::new_in(arena),
            stmt_symbols: Vec::new_in(arena),
            stmt_slices: Vec::new_in(arena),
            exprs: Vec::new_in(arena),
            expr_symbols: Vec::new_in(arena),
            literals: Vec::new_in(arena),
            layouts: Layouts::new_in(arena, target_info),
            symbols: Vec::new_in(arena),
            branch_infos: Vec::new_in(arena),
            branches: Vec::new_in(arena),
            parameters: Vec::new_in(arena),
            strings: String::new_in(arena),
            big_numbers: Vec::new_in(arena),
            call_specialization_counter: CallSpecId::default(),
            update_mode_ids: UpdateModeIds::new(),
        }
    }

    pub fn next_update_mode_id(&mut self) -> UpdateModeId {
        self.update_mode_ids.next_id()
    }

    pub fn next_call_specialization_id(&mut self) -> CallSpecId {
        let id = self.call_specialization_counter;
        let next = id.next();

        self.call_specialization_counter = next;

        id
    }

    fn push_literal(&mut self, literal: Literal) -> Index<Literal> {
        let index = Index::new(self.literals.len() as u32);
        self.literals.push(literal);
        index
    }

    fn push_expr(&mut self, expr: Expr) -> Index<Expr> {
        let index = Index::new(self.exprs.len() as u32);
        self.exprs.push(expr);
        self.expr_symbols.push(Symbol::MONO_TMP);
        index
    }

    fn push_stmt(&mut self, stmt: Stmt) -> Index<Stmt> {
        let index = Index::new(self.stmts.len() as u32);
        self.stmts.push(stmt);
        self.stmt_symbols.push(Symbol::MONO_TMP);
        index
    }

    fn push_str(&mut self, string: &str) -> Slice<str> {
        let start = self.strings.len() as u32;
        let length = string.len() as u16;
        self.strings.push_str(string);

        Slice::new(start, length)
    }

    fn push_branch_slice(
        &mut self,
        branches: &[(u64, BranchInfo, Index<Stmt>)],
    ) -> Slice<(u64, BranchInfo, Index<Stmt>)> {
        let start = self.branches.len() as u32;
        let length = branches.len() as u16;

        self.branches.extend(branches.iter().copied());

        Slice::new(start, length)
    }

    fn reserve_stmt(&mut self) -> Index<Stmt> {
        self.push_stmt(Stmt::Reserved)
    }

    fn reserve_expr(&mut self) -> Index<Expr> {
        self.push_expr(Expr::Reserved)
    }

    fn reserve_stmt_slice(&mut self, length: usize) -> Slice<Stmt> {
        let start = self.stmts.len() as u32;

        for _ in 0..length {
            self.reserve_stmt();
        }

        Slice::new(start, length as u16)
    }

    fn reserve_expr_slice(&mut self, length: usize) -> Slice<Expr> {
        let start = self.exprs.len() as u32;

        for _ in 0..length {
            self.reserve_expr();
        }

        Slice::new(start, length as u16)
    }

    fn reserve_stmt_index_slice(&mut self, length: usize) -> Slice<Index<Stmt>> {
        let start = self.stmt_slices.len() as u32;

        let it = std::iter::repeat(Index::new(0)).take(length);
        self.stmt_slices.extend(it);

        Slice::new(start, length as u16)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct UpdateModeIds {
    next: UpdateModeId,
}

impl UpdateModeIds {
    pub fn new() -> Self {
        Self {
            next: UpdateModeId::default(),
        }
    }

    pub fn next_id(&mut self) -> UpdateModeId {
        let id = self.next;
        let next = id.next();
        self.next = next;
        id
    }
}

impl Default for UpdateModeIds {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Param {
    pub symbol: Symbol,
    pub borrow: bool,
    pub layout: Index<Layout>,
}

/// in the block below, symbol `scrutinee` is assumed be be of shape `tag_id`
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BranchInfo {
    None,
    Constructor {
        scrutinee: Symbol,
        layout: Index<Layout>,
        tag_id: TagIdIntType,
    },
}

/// When positive, this is an index into the symbols array, when negative,
/// the absolute value is an index into the literals array
#[derive(Clone, Debug, PartialEq)]
pub struct SymbolOrLiteral(i32);

#[derive(Clone, Debug)]
pub enum Literal {
    Reserved,
    Int128(Index<u128>), // we could store the sign too here?
    Int(i64),
    Float(f64),
    Decimal(Index<RocDec>),
    Str(Slice<str>),
    Bool(bool),
    Byte(u8),
}

impl Literal {
    fn from_standard_dps(
        module: &mut Module<'_>,
        standard: &crate::ir::Literal<'_>,
        output: Index<Literal>,
    ) {
        let literal = Self::from_standard(module, standard);
        module.literals[output.index as usize] = literal;
    }

    fn from_standard(module: &mut Module<'_>, standard: &crate::ir::Literal<'_>) -> Literal {
        use crate::ir;

        match standard {
            ir::Literal::Int(value) => {
                debug_assert!(value >> 64 == 0);

                Literal::Int(*value as i64)
            }
            ir::Literal::U128(_) => todo!(),
            ir::Literal::Float(value) => Literal::Float(*value),
            ir::Literal::Decimal(_) => todo!(),
            ir::Literal::Str(string) => {
                let slice = module.push_str(string);
                Literal::Str(slice)
            }
            ir::Literal::Bool(value) => Literal::Bool(*value),
            ir::Literal::Byte(value) => Literal::Byte(*value),
        }
    }
}

/// A function passed to a higher-order function. e.g. `f` in `List.map f [ ... ]`
#[derive(Clone, Debug)]
pub struct PassedFunction {
    name: Symbol,
    layout: FunctionLayout,
    specialization_id: CallSpecId,

    /// some stuff related to closures; figure out if we really need this
    captured_environment: Symbol,
    owns_captured_environment: bool,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Reserved,

    /// An argument to the current function
    // argument name is implicit
    Argument {
        position: u32,
        next: Index<Stmt>,
    },

    Global {
        next: Index<Stmt>,
    },

    Let {
        // symbol: Symbol is implicit
        expr: Index<Expr>,
        layout: Index<Layout>,
        next: Index<Stmt>,
    },

    Switch {
        /// This *must* stand for an integer, because Switch potentially compiles to a jump table.
        // cond_symbol: Symbol is implicit
        cond_layout: Index<Layout>,
        /// The u64 in the tuple will be compared directly to the condition Expr.
        /// If they are equal, this branch will be taken.
        /// final branch in the slice is the default
        branches: Slice<(u64, BranchInfo, Index<Stmt>)>,
        /// Each branch must return a value of this type.
        ret_layout: Index<Layout>,
    },
    Ret(Index<Stmt>), // the return symbol is implicit

    /// Increment the refcount of `symbol` by `increment`
    RefcountInc {
        // symbol: Symbol, is implicit
        to_modify: Index<Stmt>,
        increment: u32,
        next: Index<Stmt>,
    },

    /// Decrement the refcount of `symbol` by 1
    RefcountDec {
        to_modify: Index<Stmt>,
        // symbol: Symbol, is implicit
        next: Index<Stmt>,
    },
    /// A DecRef is a non-recursive reference count decrement
    /// e.g. If we Dec a list of lists, then if the reference count of the outer list is one,
    /// a Dec will recursively decrement all elements, then free the memory of the outer list.
    /// A DecRef would just free the outer list.
    /// That is dangerous because you may not free the elements, but in our Zig builtins,
    /// sometimes we know we already dealt with the elements (e.g. by copying them all over
    /// to a new list) and so we can just do a DecRef, which is much cheaper in such a case.
    RefcountDecRef {
        to_modify: Index<Stmt>,
        // symbol: Symbol, is implicit
        next: Index<Stmt>,
    },
    /// a join point `join f <params> = <continuation> in remainder`
    Join {
        // id: JoinPointId, is implicit
        parameters: Slice<Param>,

        /// body: what happens after _jumping to_ the join point
        body: Index<Stmt>,
        // next: what happens after _defining_ the join point
        next: Index<Stmt>,
    },
    Jump(
        // JoinPointId, is implicit
        Slice<SymbolOrLiteral>,
    ),
    RuntimeError(Slice<str>),
}

#[derive(Debug, Clone, Copy)]
enum EndBlock {
    Return,
    Continue(Index<Stmt>),
    Jump(Index<Stmt>),
}

impl Stmt {
    fn end_block_with_expr(
        module: &mut Module<'_>,
        end_block: EndBlock,
        store_start_at: Index<Stmt>,
        expr: Index<Expr>,
        layout: Index<Layout>,
    ) {
        match end_block {
            EndBlock::Return => {
                // 0: Let expr layout <1>
                // 1: Ret <0>
                let next = module.reserve_stmt();
                let binding = Stmt::Let { expr, layout, next };

                module.stmts[store_start_at.index as usize] = binding;
                module.stmts[next.index as usize] = Stmt::Ret(store_start_at);
            }
            EndBlock::Jump(next) => {
                // 0: Let expr layout <1>
                // 1: Jump <next> [ <0> ]
                let next = module.reserve_stmt();
                let binding = Stmt::Let { expr, layout, next };

                // module.stmts[store_start_at.index as usize] = binding;
                // module.stmts[next.index as usize] = Stmt::Ret(store_start_at);
                todo!()
            }
            EndBlock::Continue(next) => {
                // 0: Let expr layout <next>
                let binding = Stmt::Let { expr, layout, next };

                module.stmts[store_start_at.index as usize] = binding;
            }
        }
    }

    fn from_can_expr_dps(
        module: &mut Module<'_>,
        subs: &Subs,
        standard: &roc_can::expr::Expr,
        end_block: EndBlock,
        store_start_at: Index<Stmt>,
    ) {
        use roc_can::expr;

        macro_rules! layout_from_var {
            ($var:expr) => {{
                match Layout::from_var(&mut module.layouts, subs, $var) {
                    Ok(layout) => layout,
                    Err(_) => {
                        //
                        let slice = module.push_str("invalid layout");
                        module.stmts[store_start_at.index as usize] = Stmt::RuntimeError(slice);
                        return;
                    }
                }
            }};
        }

        match standard {
            expr::Expr::Num(_, _, _, _) => todo!(),
            expr::Expr::Int(_, _, _, _, _) => todo!(),
            expr::Expr::Float(_, _, _, _, _) => todo!(),
            expr::Expr::Str(string) => {
                let slice = module.push_str(string.as_ref());
                let literal = Literal::Str(slice);
                let expr = Expr::Literal(literal);
                let expr_index = module.push_expr(expr);

                Self::end_block_with_expr(
                    module,
                    end_block,
                    store_start_at,
                    expr_index,
                    Layouts::STR_INDEX,
                );
            }
            expr::Expr::List {
                elem_var,
                loc_elems,
            } => todo!(),
            expr::Expr::Var(_) => todo!(),
            expr::Expr::When {
                cond_var,
                expr_var,
                region,
                loc_cond,
                branches,
            } => todo!(),
            expr::Expr::If {
                cond_var: _,
                branch_var,
                branches,
                final_else,
            } => {
                // control flow is different if this if is part of an assignment
                let branch_layout = layout_from_var!(*branch_var);
                let ret_layout = module.layouts.push_layout(branch_layout);

                debug_assert!(!branches.is_empty());

                match end_block {
                    EndBlock::Return => {
                        debug_assert_eq!(branches.len(), 1);
                        let (condition, then_block) = &branches[0];

                        let switch_index = module.reserve_stmt();

                        Self::from_can_expr_dps(
                            module,
                            subs,
                            &condition.value,
                            EndBlock::Continue(switch_index),
                            store_start_at,
                        );

                        let then_index = module.reserve_stmt();
                        let else_index = module.reserve_stmt();

                        Self::from_can_expr_dps(
                            module,
                            subs,
                            &then_block.value,
                            EndBlock::Return,
                            then_index,
                        );

                        Self::from_can_expr_dps(
                            module,
                            subs,
                            &final_else.value,
                            EndBlock::Return,
                            then_index,
                        );

                        // TODO re-use branch info
                        let branches = module.push_branch_slice(&[
                            (1, BranchInfo::None, then_index),
                            (0, BranchInfo::None, else_index),
                        ]);

                        let switch = Stmt::Switch {
                            cond_layout: Layouts::BOOL_INDEX,
                            branches,
                            ret_layout,
                        };

                        module.stmts[switch_index.index as usize] = switch;
                    }
                    EndBlock::Jump(_) => todo!(),
                    EndBlock::Continue(_) => todo!(),
                }
            }
            expr::Expr::LetRec(_, _, _) => todo!(),
            expr::Expr::LetNonRec(def, expr, _var) => {
                use roc_can::pattern::Pattern;
                match def.loc_pattern.value {
                    Pattern::Identifier(_symbol) => {
                        let next_index = module.reserve_stmt();

                        Self::from_can_expr_dps(
                            module,
                            subs,
                            &def.loc_expr.value,
                            EndBlock::Continue(next_index),
                            store_start_at,
                        );

                        Self::from_can_expr_dps(module, subs, &expr.value, end_block, next_index);
                    }

                    _ => todo!("convert into a When, then proceed as if it were a When"),
                }
            }
            expr::Expr::Call(function_info, arguments, _) => {
                let (function_var, function_expr, lambda_set_var, return_var) =
                    function_info.as_ref();

                // is the function available at the top-level of this module (defined, lifted or imported)
                // TODO make dynamic
                let is_toplevel = true;

                match &function_expr.value {
                    roc_can::expr::Expr::Var(symbol) if is_toplevel => {
                        // the simple case, we know exactly what function to call

                        let specialization_id = module.next_call_specialization_id();

                        let arguments_slice = module.reserve_stmt_slice(arguments.len());
                        let call_stmt_id = module.reserve_stmt();
                        let layouts_slice =
                            module.layouts.reserve_layout_slice(arguments.len() + 1);

                        let continue_with_it = arguments_slice
                            .indices()
                            .skip(1)
                            .chain(std::iter::once(call_stmt_id.index as usize));

                        let it = arguments_slice.indices().zip(continue_with_it);

                        for ((argument_index, continue_with), (_, argument)) in it.zip(arguments) {
                            Self::from_can_expr_dps(
                                module,
                                subs,
                                &argument.value,
                                EndBlock::Continue(Index::new(continue_with as u32)),
                                Index::new(argument_index as u32),
                            );
                        }

                        let it = layouts_slice.indices().zip(arguments);
                        for (layout_index, (variable, _)) in it {
                            let argument_layout = layout_from_var!(*variable);
                            module.layouts.layouts[layout_index] = argument_layout;
                        }

                        let return_layout = layout_from_var!(*return_var);
                        module.layouts.layouts[layouts_slice.indices().end] = return_layout;
                        let return_layout_index = Index::new(layouts_slice.indices().end as u32);

                        let expr = Expr::CallByName {
                            arguments: arguments_slice,
                            layouts: layouts_slice,
                            specialization_id,
                        };

                        let expr = module.push_expr(expr);

                        let index = module.reserve_stmt();
                        Self::end_block_with_expr(
                            module,
                            end_block,
                            index,
                            expr,
                            return_layout_index,
                        )
                    }
                    _ => {
                        // the more complicated case; we need to look at the lambda lambda_set_var

                        todo!()
                    }
                }
            }
            expr::Expr::RunLowLevel { op, args, ret_var } => todo!(),
            expr::Expr::ForeignCall {
                foreign_symbol,
                args,
                ret_var,
            } => todo!(),
            expr::Expr::Closure(_) => todo!(),
            expr::Expr::Record { record_var, fields } => todo!(),
            expr::Expr::EmptyRecord => todo!(),
            expr::Expr::Access {
                record_var,
                ext_var,
                field_var,
                loc_expr,
                field,
            } => todo!(),
            expr::Expr::Accessor {
                name,
                function_var,
                record_var,
                closure_ext_var,
                ext_var,
                field_var,
                field,
            } => todo!(),
            expr::Expr::Update {
                record_var,
                ext_var,
                symbol,
                updates,
            } => todo!(),
            expr::Expr::Tag {
                variant_var,
                ext_var,
                name,
                arguments,
            } => todo!(),
            expr::Expr::ZeroArgumentTag {
                closure_name,
                variant_var,
                ext_var,
                name,
                arguments,
            } => todo!(),
            expr::Expr::Expect(_, _) => todo!(),
            expr::Expr::RuntimeError(_) => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Reserved,

    Literal(Literal),

    // Functions
    CallByName {
        // name: Symbol, is implicit
        arguments: Slice<Stmt>,
        layouts: Slice<Layout>, // final element of the slice is the return type
        specialization_id: CallSpecId,
    },

    Foreign {
        foreign_symbol: ForeignSymbol,
        arguments: Slice<SymbolOrLiteral>,
        layouts: Slice<Layout>, // final element of the slice is the return type
    },

    LowLevel {
        op: LowLevel,
        arguments: Slice<SymbolOrLiteral>,
        update_mode: UpdateModeId,
    },

    HigherOrder {
        op: Index<crate::low_level::HigherOrder>,
        update_mode: UpdateModeId,
        passed_function: Index<PassedFunction>,
    },

    Struct(Slice<SymbolOrLiteral>),

    StructAtIndex {
        index: u32,
        field_layouts: Slice<Layout>,
        // structure: Symbol, is implicit
    },

    Tag {
        tag_layout: Slice<Slice<Layout>>,
        tag_name: Index<TagName>,
        tag_id: TagIdIntType, // currently u16
        arguments: Slice<SymbolOrLiteral>,
    },

    GetTagId {
        // structure: Symbol, is implicit
        union_layout: Slice<Slice<Layout>>,
    },

    UnionAtIndex {
        // structure: Symbol, is implicit
        tag_id: TagIdIntType,
        union_layout: Slice<Slice<Layout>>,
        index: u32,
    },

    Array {
        element_layout: Index<Layout>,
        elements: Slice<SymbolOrLiteral>,
    },

    Reuse {
        // symbol: Symbol, is implicit
        update_mode: UpdateModeId,
        update_tag_id: bool,
        // normal Tag fields
        tag_layout: Slice<Slice<Layout>>,
        tag_name: Index<TagName>,
        tag_id: TagIdIntType,
        arguments: Slice<SymbolOrLiteral>,
    },
    Reset {
        // symbol: Symbol, is implicit
        update_mode: UpdateModeId,
    },

    RuntimeErrorFunction(Index<str>),
}

impl Expr {
    fn from_standard_dps(
        module: &mut Module<'_>,
        symbol_to_stmt: &mut MutMap<Symbol, Index<Stmt>>,
        standard: &crate::ir::Expr<'_>,
        output: Index<Literal>,
    ) {
        let value = Self::from_standard(module, symbol_to_stmt, standard);
        module.exprs[output.index as usize] = value;
    }

    fn from_standard(
        module: &mut Module<'_>,
        symbol_to_stmt: &mut MutMap<Symbol, Index<Stmt>>,
        standard: &crate::ir::Expr<'_>,
    ) -> Expr {
        use crate::ir;

        match standard {
            ir::Expr::Literal(literal) => {
                let literal = Literal::from_standard(module, literal);
                Expr::Literal(literal)
            }
            ir::Expr::Call(_) => todo!(),
            ir::Expr::Tag {
                tag_layout,
                tag_name,
                tag_id,
                arguments,
            } => todo!(),
            ir::Expr::Struct(_) => todo!(),
            ir::Expr::StructAtIndex {
                index,
                field_layouts,
                structure,
            } => todo!(),
            ir::Expr::GetTagId {
                structure,
                union_layout,
            } => todo!(),
            ir::Expr::UnionAtIndex {
                structure,
                tag_id,
                union_layout,
                index,
            } => todo!(),
            ir::Expr::Array { elem_layout, elems } => todo!(),
            ir::Expr::EmptyArray => todo!(),
            ir::Expr::Reuse {
                symbol,
                update_tag_id,
                update_mode,
                tag_layout,
                tag_name,
                tag_id,
                arguments,
            } => todo!(),
            ir::Expr::Reset {
                symbol,
                update_mode,
            } => todo!(),
            ir::Expr::RuntimeErrorFunction(_) => todo!(),
        }
    }
}
