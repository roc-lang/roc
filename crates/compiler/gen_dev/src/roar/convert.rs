use super::ops::{to_stmt, Expr, OpCode, Sign, Stmt};
use super::proc::{Proc, Section};
use super::{ops, storage::*};
use super::{Error, Result};
use bumpalo;
use bumpalo::{
    collections,
    collections::{CollectIn, Vec},
    vec,
};
use roc_module::low_level::LowLevel;
use roc_module::{low_level, symbol};
use roc_mono::ir::Literal;
use crate::Env;
use roc_mono::{
    ir,
    layout::{InLayout, Layout, LayoutInterner, LayoutRepr, STLayoutInterner},
};
use std::borrow::Cow;
use std::cell::{Cell, RefCell};
use std::hash::Hash;
use std::collections::hash_map::Entry;
type SymbolMap<T> = Map<symbol::Symbol, T>;

///The type given as output by mono
type MonoInput<'a> = roc_collections::MutMap<(symbol::Symbol, ir::ProcLayout<'a>), ir::Proc<'a>>;

///The prefered type for maps, may change later
type Map<A: Hash, B> = std::collections::HashMap<A, B>;

///The type of information related to a symbol: what register holds it, how it is represented in mono, and whether it is stored by ref or value
type SymbolInfo<'a> = (Output, LayoutRepr<'a>, Form);

///Top level of ROAR, used to convert from Mono to ROAR
pub fn build_roar<'a,'b : 'a>(
    input: MonoInput<'a>,
    layout_interner: &'b mut STLayoutInterner<'a>,
    env: &'b Env<'a>,
    
) -> Result<Section<'b>> {
    let converter: &mut Converter<'_, '_> = env.arena.alloc(Converter::new(layout_interner,env));
    converter.build_section(&input)
}
///A expression that requires statements, ie splitting apart a variable
///Implemented as a simple way to model expressions in a way that does not require complex forms
#[derive(Clone, Debug, PartialEq)]
struct StmtExpr {
    stmts: Box<[Stmt]>,
    expr: Expr,
    form: Option<Form>,
}
impl StmtExpr {
    fn new(stmts: Box<[Stmt]>, expr: Expr) -> Self {
        Self {
            stmts: stmts,
            expr: expr,
            form: None,
        }
    }
    fn new_with_form(stmts: Box<[Stmt]>, expr: Expr, form: Form) -> Self {
        Self {
            stmts: stmts,
            expr: expr,
            form: Some(form),
        }
    }
    fn stmts(&self) -> &[Stmt] {
        &self.stmts[..]
    }
    fn expr(&self) -> &Expr {
        &self.expr
    }
    fn form(&self) -> &Option<Form> {
        &self.form
    }
    //TODO this should probably not be in place
    ///Make the expr into a statement with the given output, then add a new expression
    fn append_with(&self, with: Output, expr: Expr) -> Self {
        let new_stmt = to_stmt(&self.expr(), with);
        let mut stmts = self.stmts().to_vec().clone();
        stmts.push(new_stmt);
        Self {
            stmts: stmts.into(),
            expr: expr,
            form: None,
        }
    }
    fn append_with_form(&self, with: Output, expr: Expr, form: Form) -> Self {
        let new_stmt = to_stmt(&self.expr(), with);
        let mut stmts = self.stmts().to_vec().clone();
        stmts.push(new_stmt);
        Self {
            stmts: stmts.into(),
            expr: expr,
            form: Some(form),
        }
    }
    fn to_stmts(&self, with: Output) -> Box<[Stmt]> {
        [self.stmts(), &[to_stmt(self.expr(), with)]]
            .concat()
            .into()
    }
}
impl From<Expr> for StmtExpr {
    fn from(value: Expr) -> Self {
        Self::new(Box::new([]), value)
    }
}
impl From<(Expr, Form)> for StmtExpr {
    fn from(value: (Expr, Form)) -> Self {
        let (expr, form) = value else { todo!() };
        Self::new_with_form(Box::new([]), expr, form)
    }
}

///How a layout is passed to a procedure, and it's internals
#[derive(Clone, Copy, Debug, PartialEq)]
enum Form {
    ///It takes it as a struct register
    ByRef,
    ///It takes it as a value
    ByValue,
    ///It takes a value too large to be stored in one register
    Indirect,
    ///A function pointer
    Function,
    ///It is a float
    ByFloat,
}

//FIXME some of these lifetimes are proberaly unnenecassary
///Controlls the conversion from Mono to ROAR
pub(crate) struct Converter<'a, 'b> {
    ///The interner for Mono type layouts
    layout_interner: &'b mut STLayoutInterner<'a>,
    ///The arena, as this controls the Mono -> ROAR phase
    env: &'b Env<'a>,
    ///Register names that have not been yet allocated
    register_alloc: RegisterAllocater,
    ///Refrences from the symbol of a type to a given procedure
    //TODO get this working
    proc_map: Map<symbol::Symbol, ProcRef>,
    ///A list of proccedures that need to be implemented
    //TODO make this not a ref cell
    proc_queue: RefCell<Vec<'b, (symbol::Symbol, &'b ir::ProcLayout<'b>, &'b ir::Proc<'b>)>>,
}

pub(super) fn get_sym_reg<'f0>(
    sym: &symbol::Symbol,
    map: &'f0 SymbolMap<SymbolInfo>,
) -> Option<&'f0 Output> {
    map.get(sym).map(|(value, _, _)| value)
}

pub(super) fn arg_info<'a>(
    sym_map: &SymbolMap<SymbolInfo<'a>>,
    sym: &symbol::Symbol,
) -> Option<SymbolInfo<'a>> {
    sym_map.get(sym).cloned()
}

impl<'c, 'a: 'c, 'b: 'c> Converter<'a, 'b>
where 'b: 'a,
{
    pub fn new_register(&'c mut self) -> Register {
        self.register_alloc.new_register()
    }

    pub fn new_float_register(&'c mut self) -> FloatRegister {
        self.register_alloc.new_float_register()
    }
    ///Create a converter
    pub fn new(layout_interner: &'b mut STLayoutInterner<'a>, env: &'b Env<'a>) -> Self {
        Self {
            layout_interner: layout_interner,
            env : env,
            register_alloc: RegisterAllocater::new(),
            proc_map: Map::new(),
            proc_queue: RefCell::new(vec![in &env.arena]),
        }
    }
    ///Get the internal interner
    pub fn intern(&'c self) -> &'c STLayoutInterner<'a> {
        self.layout_interner
    }
    pub fn arena(&'c self) -> &'a bumpalo::Bump {
        self.env.arena
    }
    ///From an interned layout get a regular layout and if it's possible to store it in a register
    /// Note that this will remove indirection if it exists, ie (Literal,&Int) -> (Ref,Int)
    pub fn get_form(&self, layout: &InLayout<'b>) -> Result<(LayoutRepr<'b>, Form)> {
        use Form::*;
        let repr = self.intern().get_repr(*layout);
        println!("repr is {:?}", repr);
        Ok(match repr {
            LayoutRepr::Builtin(builtin) => {
                if self.intern().stack_size(*layout) <= 8 {
                    (repr, ByValue)
                } else {
                    (repr, Indirect)
                }
            }
            LayoutRepr::LambdaSet(lambda_set) => (repr, Function),
            LayoutRepr::FunctionPointer(function_pointer) => (repr, Function),
            LayoutRepr::Erased(erased) => (repr, Function),
            LayoutRepr::Ptr(ptr) => (self.intern().get_repr(ptr), ByRef),
            _ => (repr, ByRef),
        })
    }

    ///Convert the mono input into Roar
    pub fn build_section(self : &'c mut Self, input: &MonoInput<'a>) -> Result<Section<'c>> {
        let arena = self.arena();
        //TODO FIXME BUG REALLY THIS IS UNSAFE NEED TO FIX THIS, JUST A LITTLE BIT OF SCOTCH TAPE
        let other_self: *mut Converter<'a, 'b> = self;
        let procs = input.into_iter().map(|((symbol, layout), proc)| {
            unsafe { other_self.as_mut().expect("").build_proc(symbol, layout, proc).unwrap() }
        }).collect_in(arena);
        // let procs = input
        //     .into_iter()
        //     .map(|((symbol, layout), proc)| self.build_proc(symbol, layout, proc).unwrap()) //TODO
        //     .collect_in::<bumpalo::collections::Vec<Proc>>(&self.arena);
        //let mut roar = Section::new(&self.arena);
        let (roar, refs) = Section::new(arena).add_procs(procs,arena);
        Ok(roar)
    }
    ///Build a single Mono procedure
    pub fn build_proc<'f0>(
        &'c mut self,
        sym: &symbol::Symbol,
        layout: &ir::ProcLayout,
        proc: &ir::Proc<'a>,
    ) -> Result<Proc<'c>> {
        let mut sym_map: Map<symbol::Symbol, SymbolInfo<'_>> = Map::new();
        let args = proc
            .args
            .into_iter()
            .map(|(lay, sym): &(InLayout<'_>, symbol::Symbol)| -> Output {
                let (repr, form) = self.get_form(lay).unwrap(); //TODO
                let new_reg = self.register_alloc.new_register();
                &mut sym_map.insert(*sym, (Output::Register(new_reg.clone()), repr, form));
                Output::Register(new_reg)
            })
            .collect();
        let mut roar_proc = Proc::new(args, self.arena());
        Ok(roar_proc.add_ops(self.build_stmt(&proc.body, &mut sym_map)?))
    }
    ///Build a single Mono "statement". Because Mono uses a `let in` structure of binding while ROAR uses an imperiative style of simply setting values, a single Mono statement almost always corresponds to mutiple ROAR statements
    pub fn build_stmt(
        &'c mut self,
        stmt: &ir::Stmt<'a>,
        sym_map: &mut SymbolMap<SymbolInfo<'a>>,
    ) -> Result<collections::Vec<Stmt>> {
        println!("Trying to build stmt {:#?}", stmt);
        use super::ops::OpCode::*;
        use super::storage::Input::*;
        match stmt {
            //Build a let statement
            //Among the most important transformations
            ir::Stmt::Let(symbol, expr, in_layout, rest) => {
                let r_expr: StmtExpr = match expr {
                    ir::Expr::Literal(literal) => self.build_literal(literal)?.into(),
                    ir::Expr::Call(call) => self.build_call(call, sym_map)?,
                    ir::Expr::Tag {
                        tag_layout,
                        tag_id,
                        arguments,
                        reuse,
                    } => todo!(),
                    ir::Expr::Struct(_) => todo!(),
                    ir::Expr::NullPointer => todo!(),
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
                    ir::Expr::GetElementPointer {
                        structure,
                        union_layout,
                        indices,
                    } => todo!(),
                    ir::Expr::Array { elem_layout, elems } => todo!(),
                    ir::Expr::EmptyArray => todo!(),
                    ir::Expr::ErasedMake { value, callee } => todo!(),
                    ir::Expr::ErasedLoad { symbol, field } => todo!(),
                    ir::Expr::FunctionPointer { lambda_name } => todo!(),
                    ir::Expr::Alloca {
                        element_layout,
                        initializer,
                    } => todo!(),
                    ir::Expr::Reset {
                        symbol,
                        update_mode,
                    } => todo!(),
                    ir::Expr::ResetRef {
                        symbol,
                        update_mode,
                    } => todo!(),
                };
                let res = sym_map.entry(*symbol).or_insert_with(|| {
                    let new_reg = match r_expr.form() {
                        Some(Form::ByFloat) => Output::FloatRegister(self.new_float_register()),
                        Some(_) => Output::Register(self.new_register()),
                        None => todo!(),
                        //_ => self.register_alloc.new_register()
                    };
                    let new_form = match r_expr.form() {
                        Some(form) => form.clone(),
                        None => self.get_form(in_layout).unwrap().1, //TODO Fix unwrap
                    };
                    (new_reg.clone(), self.intern().get_repr(*in_layout), new_form)
                });
                if let Entry::Occupied(entry) = sym_map.entry(*symbol) {
                    let (reg, layout, ref form) = entry.get() else {
                        todo!()
                    };
                    //TODO Add consistency checking
                    //sym_map.insert(*symbol, (reg.clone(), self.intern().get_repr(*in_layout), *form));
                    r_expr.to_stmts(reg.clone())
                } else {
                    let new_reg = match r_expr.form() {
                        Some(Form::ByFloat) => Output::FloatRegister(self.new_float_register()),
                        Some(_) => Output::Register(self.new_register()),
                        None => todo!(),
                        //_ => self.register_alloc.new_register()
                    };
                    let new_form = match r_expr.form() {
                        Some(form) => form.clone(),
                        None => self.get_form(in_layout)?.1,
                    };
                    sym_map.insert(
                        *symbol,
                        (new_reg.clone(), self.intern().get_repr(*in_layout), new_form),
                    );
                    r_expr.to_stmts(new_reg)
                };
                todo!()
            }
            ir::Stmt::Switch {
                cond_symbol,
                cond_layout,
                branches,
                default_branch,
                ret_layout,
            } => todo!(),
            ir::Stmt::Ret(symbol) => todo!(),
            ir::Stmt::Refcounting(modify_rc, _) => todo!(),
            ir::Stmt::Expect {
                condition,
                region,
                lookups,
                variables,
                remainder,
            } => todo!(),
            ir::Stmt::Dbg {
                source_location,
                source,
                symbol,
                variable,
                remainder,
            } => todo!(),
            ir::Stmt::Join {
                id,
                parameters,
                body,
                remainder,
            } => todo!(),
            ir::Stmt::Jump(join_point_id, _) => todo!(),
            ir::Stmt::Crash(symbol, crash_tag) => todo!(),
        };
        todo!()
    }
    ///Get a literal representation
    fn build_literal(&'c mut self, literal: &Literal<'_>) -> Result<StmtExpr> {
        match literal {
            Literal::Int(int) => todo!(),
            Literal::U128(unsigned_int) => todo!(),
            Literal::Float(float) => todo!(),
            Literal::Decimal(decimal) => todo!(),
            Literal::Str(str) => Ok(StmtExpr::new_with_form(
                Box::new([]),
                (
                    OpCode::Create,
                    Input::Data(str.as_bytes().to_vec()),
                    Input::Null,
                ),
                Form::ByRef,
            )),
            Literal::Bool(bool) => Ok((
                (
                    OpCode::Move,
                    Input::Value(LiteralValue::Unsigned(*bool as u64)),
                    Input::Null,
                ),
                Form::ByValue,
            )
                .into()),
            Literal::Byte(byte) => Ok((
                (
                    OpCode::Move,
                    Input::Value(LiteralValue::Unsigned(*byte as u64)),
                    Input::Null,
                ),
                Form::ByValue,
            )
                .into()),
        }
    }
    ///Make a function call
    pub fn build_call(
        &'c mut self,
        call: &ir::Call<'_>,
        sym_map: &mut SymbolMap<SymbolInfo<'_>>,
    ) -> Result<StmtExpr> {
        Ok(match &call.call_type {
            ir::CallType::ByName {
                name,
                ret_layout,
                arg_layouts,
                specialization_id,
            } => todo!(),
            ir::CallType::ByPointer { .. } => todo!(), //? NOTE as far as I can tell, this is never used
            ir::CallType::Foreign { .. } => todo!(),   //? NOTE as far as I can tell, unused
            ir::CallType::LowLevel { op, update_mode } => {
                self.build_op(&op, &call.arguments, sym_map)?
            }
            ir::CallType::HigherOrder(higher_order_low_level) => todo!(),
        })
    }
    fn build_op(
        &'c mut self,
        op_code: &low_level::LowLevel,
        args: &[symbol::Symbol],
        sym_map: &mut SymbolMap<SymbolInfo<'_>>,
    ) -> Result<StmtExpr> {
        use low_level::LowLevel::*;
        Ok(match op_code {
            StrConcat => todo!(),
            StrJoinWith => todo!(),
            StrIsEmpty => todo!(),
            StrStartsWith => todo!(),
            StrEndsWith => todo!(),
            StrSplitOn => todo!(),
            StrCountUtf8Bytes => todo!(),
            StrFromInt => todo!(),
            StrFromUtf8 => todo!(),
            StrToUtf8 => todo!(),
            StrRepeat => todo!(),
            StrFromFloat => todo!(),
            StrTrim => todo!(),
            StrTrimStart => todo!(),
            StrTrimEnd => todo!(),
            StrToNum => todo!(),
            StrGetUnsafe => todo!(),
            StrSubstringUnsafe => todo!(),
            StrReserve => todo!(),
            StrWithCapacity => todo!(),
            StrReleaseExcessCapacity => todo!(),
            ListLenUsize => todo!(),
            ListLenU64 => todo!(),
            ListWithCapacity => todo!(),
            ListReserve => todo!(),
            ListReleaseExcessCapacity => todo!(),
            ListAppendUnsafe => todo!(),
            ListGetUnsafe => todo!(),
            ListReplaceUnsafe => todo!(),
            ListConcat => todo!(),
            ListPrepend => todo!(),
            ListSortWith => todo!(),
            ListSublist => todo!(),
            ListDropAt => todo!(),
            ListSwap => todo!(),
            ListGetCapacity => todo!(),
            ListIsUnique => todo!(),
            ListClone => todo!(),
            ListConcatUtf8 => todo!(),
            ListIncref => todo!(),
            ListDecref => todo!(),
            NumAdd => self.build_num_op(op_code, args, sym_map)?,
            NumAddWrap => self.build_num_op(op_code, args, sym_map)?,
            NumAddChecked => self.build_num_op(op_code, args, sym_map)?,
            NumAddSaturated => self.build_num_op(op_code, args, sym_map)?,
            NumSub => self.build_num_op(op_code, args, sym_map)?,
            NumSubWrap => self.build_num_op(op_code, args, sym_map)?,
            NumSubChecked => self.build_num_op(op_code, args, sym_map)?,
            NumSubSaturated => self.build_num_op(op_code, args, sym_map)?,
            NumMul => self.build_num_op(op_code, args, sym_map)?,
            NumMulWrap => self.build_num_op(op_code, args, sym_map)?,
            NumMulSaturated => self.build_num_op(op_code, args, sym_map)?,
            NumMulChecked => self.build_num_op(op_code, args, sym_map)?,
            NumGt => self.build_num_op(op_code, args, sym_map)?,
            NumGte => self.build_num_op(op_code, args, sym_map)?,
            NumLt => self.build_num_op(op_code, args, sym_map)?,
            NumLte => self.build_num_op(op_code, args, sym_map)?,
            NumCompare => self.build_num_op(op_code, args, sym_map)?,
            NumDivFrac => self.build_num_op(op_code, args, sym_map)?,
            NumDivTruncUnchecked => self.build_num_op(op_code, args, sym_map)?,
            NumDivCeilUnchecked => self.build_num_op(op_code, args, sym_map)?,
            NumRemUnchecked => self.build_num_op(op_code, args, sym_map)?,
            NumIsMultipleOf => self.build_num_op(op_code, args, sym_map)?,
            NumAbs => self.build_num_op(op_code, args, sym_map)?,
            NumNeg => self.build_num_op(op_code, args, sym_map)?,
            NumSin => self.build_num_op(op_code, args, sym_map)?,
            NumCos => self.build_num_op(op_code, args, sym_map)?,
            NumTan => self.build_num_op(op_code, args, sym_map)?,
            NumSqrtUnchecked => self.build_num_op(op_code, args, sym_map)?,
            NumLogUnchecked => self.build_num_op(op_code, args, sym_map)?,
            NumRound => self.build_num_op(op_code, args, sym_map)?,
            NumToFrac => self.build_num_op(op_code, args, sym_map)?,
            NumPow => self.build_num_op(op_code, args, sym_map)?,
            NumCeiling => self.build_num_op(op_code, args, sym_map)?,
            NumPowInt => self.build_num_op(op_code, args, sym_map)?,
            NumFloor => self.build_num_op(op_code, args, sym_map)?,
            NumIsNan => self.build_num_op(op_code, args, sym_map)?,
            NumIsInfinite => self.build_num_op(op_code, args, sym_map)?,
            NumIsFinite => self.build_num_op(op_code, args, sym_map)?,
            NumAtan => self.build_num_op(op_code, args, sym_map)?,
            NumAcos => self.build_num_op(op_code, args, sym_map)?,
            NumAsin => self.build_num_op(op_code, args, sym_map)?,
            NumBitwiseAnd => self.build_num_op(op_code, args, sym_map)?,
            NumBitwiseXor => self.build_num_op(op_code, args, sym_map)?,
            NumBitwiseOr => self.build_num_op(op_code, args, sym_map)?,
            NumShiftLeftBy => self.build_num_op(op_code, args, sym_map)?,
            NumShiftRightBy => self.build_num_op(op_code, args, sym_map)?,
            NumShiftRightZfBy => self.build_num_op(op_code, args, sym_map)?,
            NumIntCast => self.build_num_op(op_code, args, sym_map)?,
            NumToFloatCast => self.build_num_op(op_code, args, sym_map)?,
            NumToIntChecked => self.build_num_op(op_code, args, sym_map)?,
            NumToFloatChecked => self.build_num_op(op_code, args, sym_map)?,
            NumToStr => todo!(),
            NumCountLeadingZeroBits => todo!(),
            NumCountTrailingZeroBits => todo!(),
            NumCountOneBits => todo!(),
            NumWithoutDecimalPoint => todo!(),
            NumWithDecimalPoint => todo!(),
            NumF32ToParts => todo!(),
            NumF64ToParts => todo!(),
            NumF32FromParts => todo!(),
            NumF64FromParts => todo!(),
            Eq => todo!(),
            NotEq => todo!(),
            And => todo!(),
            Or => todo!(),
            Not => todo!(),
            Hash => todo!(),
            PtrCast => todo!(),
            PtrStore => todo!(),
            PtrLoad => todo!(),
            PtrClearTagId => todo!(),
            RefCountIncRcPtr => todo!(),
            RefCountDecRcPtr => todo!(),
            RefCountIncDataPtr => todo!(),
            RefCountDecDataPtr => todo!(),
            RefCountIsUnique => todo!(),
            BoxExpr => todo!(),
            UnboxExpr => todo!(),
            Unreachable => todo!(),
            DictPseudoSeed => todo!(),
            SetJmp => todo!(),
            LongJmp => todo!(),
            SetLongJmpBuffer => todo!(),
        })
    }
    ///Seperation of `build_op` for numerics
    fn build_num_op<'f0>(
        &'c mut self,
        op_code: &low_level::LowLevel,
        args: &[symbol::Symbol],
        sym_map: &mut SymbolMap<SymbolInfo<'f0>>,
    ) -> Result<StmtExpr> {
        use low_level::LowLevel::*;

        macro_rules! arg_info {
            ($arg:expr, $reg:ident, $layout:ident, $form:ident) => {
                let Some(($reg,$layout,$form)) = arg_info(sym_map,$arg) else {
                    todo!()
                };
            };
            ($($arg:expr, $reg:ident, $layout:ident, $form:ident);+) => {

                $(let Some(($reg,$layout,$form)) = arg_info(sym_map,$arg) else {
                    todo!()
                };)+
            };
        }
        macro_rules! bad_num {
            () => {
                unimplemented!("Should only run numeric ops with numeric operands")
            };
        }

        let [arg_a, arg_b] = args else { todo!() };
        arg_info!(arg_a, reg_a, repr_a, form_a; arg_b, reg_b, repr_b, form_b);

        match (form_a, form_b) {
            (Form::ByRef, Form::ByRef) => todo!(),
            (Form::ByRef, Form::ByValue) => todo!(),
            (Form::ByValue, Form::ByRef) => match op_code {
                NumAdd => match repr_b {
                    LayoutRepr::Builtin(builtin) => {
                        use roc_builtins::bitcode::IntWidth::*;
                        use roc_mono::layout::Builtin;
                        match builtin {
                            Builtin::Int(U8 | U16 | U32 | U64)
                            | Builtin::Int(I8 | I16 | I32 | I64) => {
                                let full_expr = self.build_access(
                                    &<Input as From<Output>>::from(reg_b),
                                    ByteSize(0),
                                    sym_map,
                                );
                                let new_reg = self.register_alloc.new_register();
                                let fin_expr = (
                                    OpCode::Add(Sign::Signed),
                                    <Output as Into<Input>>::into(reg_a.clone()),
                                    Input::Register(new_reg.clone()),
                                );
                                let fin_sexpr =
                                    full_expr?.append_with(Output::Register(new_reg), fin_expr);
                                fin_sexpr
                            }
                            Builtin::Float(float_width) => todo!(),
                            Builtin::Bool => todo!(),
                            Builtin::Decimal => todo!(),
                            _ => todo!(),
                        }
                    }
                    _ => bad_num!(),
                },
                StrConcat => todo!(),
                StrJoinWith => todo!(),
                StrIsEmpty => todo!(),
                StrStartsWith => todo!(),
                StrEndsWith => todo!(),
                StrSplitOn => todo!(),
                StrCountUtf8Bytes => todo!(),
                StrFromInt => todo!(),
                StrFromUtf8 => todo!(),
                StrToUtf8 => todo!(),
                StrRepeat => todo!(),
                StrFromFloat => todo!(),
                StrTrim => todo!(),
                StrTrimStart => todo!(),
                StrTrimEnd => todo!(),
                StrToNum => todo!(),
                StrGetUnsafe => todo!(),
                StrSubstringUnsafe => todo!(),
                StrReserve => todo!(),
                StrWithCapacity => todo!(),
                StrReleaseExcessCapacity => todo!(),
                ListLenUsize => todo!(),
                ListLenU64 => todo!(),
                ListWithCapacity => todo!(),
                ListReserve => todo!(),
                ListReleaseExcessCapacity => todo!(),
                ListAppendUnsafe => todo!(),
                ListGetUnsafe => todo!(),
                ListReplaceUnsafe => todo!(),
                ListConcat => todo!(),
                ListPrepend => todo!(),
                ListSortWith => todo!(),
                ListSublist => todo!(),
                ListDropAt => todo!(),
                ListSwap => todo!(),
                ListGetCapacity => todo!(),
                ListIsUnique => todo!(),
                ListClone => todo!(),
                ListConcatUtf8 => todo!(),
                ListIncref => todo!(),
                ListDecref => todo!(),
                NumAddWrap => todo!(),
                NumAddChecked => todo!(),
                NumAddSaturated => todo!(),
                NumSub => todo!(),
                NumSubWrap => todo!(),
                NumSubChecked => todo!(),
                NumSubSaturated => todo!(),
                NumMul => todo!(),
                NumMulWrap => todo!(),
                NumMulSaturated => todo!(),
                NumMulChecked => todo!(),
                NumGt => todo!(),
                NumGte => todo!(),
                NumLt => todo!(),
                NumLte => todo!(),
                NumCompare => todo!(),
                NumDivFrac => todo!(),
                NumDivTruncUnchecked => todo!(),
                NumDivCeilUnchecked => todo!(),
                NumRemUnchecked => todo!(),
                NumIsMultipleOf => todo!(),
                NumAbs => todo!(),
                NumNeg => todo!(),
                NumSin => todo!(),
                NumCos => todo!(),
                NumTan => todo!(),
                NumSqrtUnchecked => todo!(),
                NumLogUnchecked => todo!(),
                NumRound => todo!(),
                NumToFrac => todo!(),
                NumPow => todo!(),
                NumCeiling => todo!(),
                NumPowInt => todo!(),
                NumFloor => todo!(),
                NumIsNan => todo!(),
                NumIsInfinite => todo!(),
                NumIsFinite => todo!(),
                NumAtan => todo!(),
                NumAcos => todo!(),
                NumAsin => todo!(),
                NumBitwiseAnd => todo!(),
                NumBitwiseXor => todo!(),
                NumBitwiseOr => todo!(),
                NumShiftLeftBy => todo!(),
                NumShiftRightBy => todo!(),
                NumShiftRightZfBy => todo!(),
                NumIntCast => todo!(),
                NumToFloatCast => todo!(),
                NumToIntChecked => todo!(),
                NumToFloatChecked => todo!(),
                NumToStr => todo!(),
                NumCountLeadingZeroBits => todo!(),
                NumCountTrailingZeroBits => todo!(),
                NumCountOneBits => todo!(),
                NumWithoutDecimalPoint => todo!(),
                NumWithDecimalPoint => todo!(),
                NumF32ToParts => todo!(),
                NumF64ToParts => todo!(),
                NumF32FromParts => todo!(),
                NumF64FromParts => todo!(),
                Eq => todo!(),
                NotEq => todo!(),
                And => todo!(),
                Or => todo!(),
                Not => todo!(),
                Hash => todo!(),
                PtrCast => todo!(),
                PtrStore => todo!(),
                PtrLoad => todo!(),
                PtrClearTagId => todo!(),
                RefCountIncRcPtr => todo!(),
                RefCountDecRcPtr => todo!(),
                RefCountIncDataPtr => todo!(),
                RefCountDecDataPtr => todo!(),
                RefCountIsUnique => todo!(),
                BoxExpr => todo!(),
                UnboxExpr => todo!(),
                Unreachable => todo!(),
                DictPseudoSeed => todo!(),
                SetJmp => todo!(),
                LongJmp => todo!(),
                SetLongJmpBuffer => todo!(),
            },
            (Form::ByValue, Form::ByValue) => {
                //TODO add something with signing?
                //TODO add support for signing Add something for smaller bits
                //TODO add float support
                let r_op = match op_code {
                    NumAdd => OpCode::Add(Sign::Signed),
                    NumAddWrap => todo!(),
                    NumAddChecked => todo!(),
                    NumAddSaturated => todo!(),
                    NumSub => OpCode::Sub(Sign::Signed),
                    NumSubWrap => todo!(),
                    NumSubChecked => todo!(),
                    NumSubSaturated => todo!(),
                    NumMul => OpCode::Mul(Sign::Signed),
                    NumMulWrap => todo!(),
                    NumMulSaturated => todo!(),
                    NumMulChecked => todo!(),
                    NumGt => OpCode::Sub(Sign::Signed),
                    NumGte => OpCode::Sub(Sign::Signed),
                    NumLt => OpCode::Sub(Sign::Signed),
                    NumLte => OpCode::Sub(Sign::Signed),
                    NumCompare => OpCode::Sub(Sign::Signed),
                    NumDivFrac => todo!(),
                    NumDivTruncUnchecked => todo!(),
                    NumDivCeilUnchecked => todo!(),
                    NumRemUnchecked => todo!(),
                    NumIsMultipleOf => todo!(),
                    NumAbs => OpCode::Abs,
                    NumNeg => todo!(),
                    NumSin => todo!(),
                    NumCos => todo!(),
                    NumTan => todo!(),
                    NumSqrtUnchecked => todo!(),
                    NumLogUnchecked => todo!(),
                    NumRound => todo!(),
                    NumToFrac => todo!(),
                    NumPow => todo!(),
                    NumCeiling => todo!(),
                    NumPowInt => todo!(),
                    NumFloor => todo!(),
                    NumIsNan => todo!(),
                    NumIsInfinite => todo!(),
                    NumIsFinite => todo!(),
                    NumAtan => todo!(),
                    NumAcos => todo!(),
                    NumAsin => todo!(),
                    NumBitwiseAnd => todo!(),
                    NumBitwiseXor => todo!(),
                    NumBitwiseOr => todo!(),
                    NumShiftLeftBy => todo!(),
                    NumShiftRightBy => todo!(),
                    NumShiftRightZfBy => todo!(),
                    NumIntCast => todo!(),
                    NumToFloatCast => todo!(),
                    NumToIntChecked => todo!(),
                    NumToFloatChecked => todo!(),
                    NumToStr => todo!(),
                    NumCountLeadingZeroBits => todo!(),
                    NumCountTrailingZeroBits => todo!(),
                    NumCountOneBits => todo!(),
                    NumWithoutDecimalPoint => todo!(),
                    NumWithDecimalPoint => todo!(),
                    NumF32ToParts => todo!(),
                    NumF64ToParts => todo!(),
                    NumF32FromParts => todo!(),
                    NumF64FromParts => todo!(),
                    Eq => todo!(),
                    NotEq => todo!(),
                    And => todo!(),
                    Or => todo!(),
                    Not => todo!(),
                    _ => bad_num!(),
                };
                StmtExpr::from((
                    r_op,
                    <Output as Into<Input>>::into(reg_a.clone()),
                    <Output as Into<Input>>::into(reg_b.clone()),
                ))
            }
            (Form::ByRef, Form::Indirect) => todo!(),
            (Form::ByValue, Form::Indirect) => todo!(),
            (Form::Indirect, Form::ByRef) => todo!(),
            (Form::Indirect, Form::ByValue) => todo!(),
            (Form::Indirect, Form::Indirect) => todo!(),
            (_, _) => todo!(),
        };
        todo!()
    }

    ///Build an access into a specific register
    ///Just a utility
    ///Always by value
    fn build_access(
        &'c mut self,
        input: &Input,
        at_byte: Offset,
        sym_map: &mut SymbolMap<SymbolInfo<'_>>,
    ) -> Result<StmtExpr> {
        (OpCode::Load(at_byte), input, Input::Null);
        todo!()
    }
    ///Split an access into many registers
    ///Always by values
    fn build_split_access(
        &'c mut self,
        input: &Input,
        at_byte: Offset,
        number: u32,
        sym_map: &mut SymbolMap<SymbolInfo<'_>>,
    ) -> Result<collections::Vec<'b, StmtExpr>> {
        (0..number).into_iter().map(|x| {
            (
                OpCode::Load(add_offsets(at_byte, mul_offsets(WORD_SIZE, x))),
                input,
                Input::Null,
            )
        });
        todo!()
    }
    ///Build an access into a specific register
    ///Build an access into a structure
    ///Always by ref
    fn build_access_field(
        &'c mut self,
        input: &Input,
        at_byte: Offset,
        bytes: ByteSize,
        sym_map: &mut SymbolMap<SymbolInfo<'_>>,
    ) -> Result<StmtExpr> {
        todo!()
    }
    ///Build an access into a specific register
    ///Build an access into a structure
    ///Always by ref
    fn build_access_index(
        &'c mut self,
        input: &Input,
        at_byte: Offset,
        bytes: ByteSize,
        sym_map: &mut SymbolMap<SymbolInfo<'_>>,
    ) -> Result<StmtExpr> {
        todo!()
    }
    ///Build a given tag expression()
    fn build_tag(
        &'c mut self,
        tag: ir::Expr,
        sym_map: &mut SymbolMap<SymbolInfo<'_>>,
    ) -> Result<StmtExpr> {
        if let ir::Expr::Tag {
            tag_layout,
            tag_id,
            arguments,
            reuse,
        } = tag
        {
            todo!()
        } else {
            return Err(Error::Todo);
        }
    }
}

///A very rough opcode correspondence, only used as a utility
impl From<low_level::LowLevel> for OpCode {
    fn from(lowlevel: LowLevel) -> Self {
        use LowLevel::*;
        match lowlevel {
            StrConcat => todo!(),
            StrJoinWith => todo!(),
            StrIsEmpty => todo!(),
            StrStartsWith => todo!(),
            StrEndsWith => todo!(),
            StrSplitOn => todo!(),
            StrCountUtf8Bytes => todo!(),
            StrFromInt => todo!(),
            StrFromUtf8 => todo!(),
            StrToUtf8 => todo!(),
            StrRepeat => todo!(),
            StrFromFloat => todo!(),
            StrTrim => todo!(),
            StrTrimStart => todo!(),
            StrTrimEnd => todo!(),
            StrToNum => todo!(),
            StrGetUnsafe => todo!(),
            StrSubstringUnsafe => todo!(),
            StrReserve => todo!(),
            StrWithCapacity => todo!(),
            StrReleaseExcessCapacity => todo!(),
            ListLenUsize => todo!(),
            ListLenU64 => todo!(),
            ListWithCapacity => todo!(),
            ListReserve => todo!(),
            ListReleaseExcessCapacity => todo!(),
            ListAppendUnsafe => todo!(),
            ListGetUnsafe => todo!(),
            ListReplaceUnsafe => todo!(),
            ListConcat => todo!(),
            ListPrepend => todo!(),
            ListSortWith => todo!(),
            ListSublist => todo!(),
            ListDropAt => todo!(),
            ListSwap => todo!(),
            ListGetCapacity => todo!(),
            ListIsUnique => todo!(),
            ListClone => todo!(),
            ListConcatUtf8 => todo!(),
            ListIncref => todo!(),
            ListDecref => todo!(),
            NumAdd => todo!(),
            NumAddWrap => todo!(),
            NumAddChecked => todo!(),
            NumAddSaturated => todo!(),
            NumSub => todo!(),
            NumSubWrap => todo!(),
            NumSubChecked => todo!(),
            NumSubSaturated => todo!(),
            NumMul => todo!(),
            NumMulWrap => todo!(),
            NumMulSaturated => todo!(),
            NumMulChecked => todo!(),
            NumGt => todo!(),
            NumGte => todo!(),
            NumLt => todo!(),
            NumLte => todo!(),
            NumCompare => todo!(),
            NumDivFrac => todo!(),
            NumDivTruncUnchecked => todo!(),
            NumDivCeilUnchecked => todo!(),
            NumRemUnchecked => todo!(),
            NumIsMultipleOf => todo!(),
            NumAbs => todo!(),
            NumNeg => todo!(),
            NumSin => todo!(),
            NumCos => todo!(),
            NumTan => todo!(),
            NumSqrtUnchecked => todo!(),
            NumLogUnchecked => todo!(),
            NumRound => todo!(),
            NumToFrac => todo!(),
            NumPow => todo!(),
            NumCeiling => todo!(),
            NumPowInt => todo!(),
            NumFloor => todo!(),
            NumIsNan => todo!(),
            NumIsInfinite => todo!(),
            NumIsFinite => todo!(),
            NumAtan => todo!(),
            NumAcos => todo!(),
            NumAsin => todo!(),
            NumBitwiseAnd => todo!(),
            NumBitwiseXor => todo!(),
            NumBitwiseOr => todo!(),
            NumShiftLeftBy => todo!(),
            NumShiftRightBy => todo!(),
            NumShiftRightZfBy => todo!(),
            NumIntCast => todo!(),
            NumToFloatCast => todo!(),
            NumToIntChecked => todo!(),
            NumToFloatChecked => todo!(),
            NumToStr => todo!(),
            NumCountLeadingZeroBits => todo!(),
            NumCountTrailingZeroBits => todo!(),
            NumCountOneBits => todo!(),
            NumWithoutDecimalPoint => todo!(),
            NumWithDecimalPoint => todo!(),
            NumF32ToParts => todo!(),
            NumF64ToParts => todo!(),
            NumF32FromParts => todo!(),
            NumF64FromParts => todo!(),
            Eq => todo!(),
            NotEq => todo!(),
            And => todo!(),
            Or => todo!(),
            Not => todo!(),
            Hash => todo!(),
            PtrCast => todo!(),
            PtrStore => todo!(),
            PtrLoad => todo!(),
            PtrClearTagId => todo!(),
            RefCountIncRcPtr => todo!(),
            RefCountDecRcPtr => todo!(),
            RefCountIncDataPtr => todo!(),
            RefCountDecDataPtr => todo!(),
            RefCountIsUnique => todo!(),
            BoxExpr => todo!(),
            UnboxExpr => todo!(),
            Unreachable => todo!(),
            DictPseudoSeed => todo!(),
            SetJmp => todo!(),
            LongJmp => todo!(),
            SetLongJmpBuffer => todo!(),
        }
    }
}
