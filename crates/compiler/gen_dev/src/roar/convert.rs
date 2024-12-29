use super::ops::{to_stmt, Expr, OpCode, Operation, Sign};
use super::proc::{Proc, Section};
use super::{ops, storage::*};
use super::{Error, Result};
use bumpalo;
use bumpalo::{collections, collections::CollectIn};
use roc_module::low_level::LowLevel;
use roc_module::{low_level, symbol};
use roc_mono::ir::Literal;
use roc_mono::{
    ir,
    layout::{InLayout, Layout, LayoutInterner, LayoutRepr, STLayoutInterner},
};
use std::borrow::Cow;
use std::hash::Hash;
type SymbolMap<T> = Map<symbol::Symbol, T>;

///The type given as output by mono
type MonoInput<'a> = roc_collections::MutMap<(symbol::Symbol, ir::ProcLayout<'a>), ir::Proc<'a>>;

///The prefered type for maps, may change later
type Map<A: Hash, B> = std::collections::HashMap<A, B>;

///The type of information related to a symbol: what register holds it, how it is represented in mono, and whether it is stored by ref or value
type SymbolInfo<'a> = (Output, LayoutRepr<'a>, Form);

///A expression that requires statements, ie splitting apart a variable
///Implemented as a simple way to model expressions in a way that does not require complex forms
struct StmtExpr {
    stmts: Box<[Operation]>,
    expr: Expr,
}
impl<'a> StmtExpr
where
    Self: 'a,
{
    fn new(stmts: Box<[Operation]>, expr: Expr) -> Self {
        Self {
            stmts: stmts,
            expr: expr,
        }
    }
    fn stmts(self: &'a Self) -> &'a [Operation] {
        &self.stmts[..]
    }
    fn expr(self: &'a Self) -> &'a Expr {
        &self.expr
    }
    ///Make the expr into a statement with the given output, then add a new expression
    fn append_with(self, with: Output, expr: Expr) -> Self {
        let new_stmt = to_stmt(self.expr().clone(), with);
        Self {
            stmts: [self.stmts, Box::new([new_stmt])].concat().into(),
            expr: expr,
        }
    }
}
impl From<Expr> for StmtExpr {
    fn from(value: Expr) -> Self {
        Self::new(Box::new([]), value)
    }
}

///How a layout is passed to a procedure, and it's internals
#[derive(Clone, Debug)]
enum Form {
    ///It takes it as a struct register
    ByRef,
    ///It takes it as a value
    ByValue,
    ///It takes a value too large to be stored in one register
    Indirect,
}

///Controlls the conversion from Mono to ROAR
pub(crate) struct Converter<'a, 'b> {
    ///The interner for Mono type layouts
    layout_interner: &'b mut STLayoutInterner<'a>,
    ///The arena, as this controls the Mono -> ROAR phase
    arena: bumpalo::Bump,
    ///Register names that have not been yet allocated
    register_alloc: RegisterAllocater,
    ///Refrences from the symbol of a type to a given procedure
    //TODO get this working
    proc_map: Map<symbol::Symbol, ProcRef>,
}

pub(super) fn get_sym_reg<'a>(
    sym: &symbol::Symbol,
    map: &'a SymbolMap<SymbolInfo>,
) -> Option<&'a Output> {
    map.get(sym).map(|(value, _, _)| value)
}

pub(super) fn arg_info<'a>(
    sym_map: &SymbolMap<SymbolInfo<'a>>,
    sym: &symbol::Symbol,
) -> Option<SymbolInfo<'a>> {
    sym_map.get(sym).cloned()
}
impl<'a, 'b> Converter<'a, 'b> {
    ///Create a converter
    pub fn new(layout_interner: &'b mut STLayoutInterner<'a>, arena: bumpalo::Bump) -> Self {
        Self {
            layout_interner: layout_interner,
            arena: arena,
            register_alloc: RegisterAllocater::new(),
            proc_map: Map::new(),
        }
    }
    ///Get the internal interner
    pub fn intern(&'b self) -> &'b STLayoutInterner<'a> {
        self.layout_interner
    }
    ///From an interned layout get a regular layout and if it's possible to store it in a register
    pub fn get_form(&'b self, layout: &InLayout<'b>) -> Result<(LayoutRepr<'b>, Form)> {
        use Form::*;
        let repr = self.intern().get_repr(*layout);
        println!("repr is {:?}", repr);
        Ok(match repr {
            LayoutRepr::Builtin(builtin) if self.intern().stack_size(*layout) <= 8 => {
                (repr, ByValue)
            }
            LayoutRepr::LambdaSet(lambda_set) => todo!(),
            LayoutRepr::FunctionPointer(function_pointer) => todo!(),
            LayoutRepr::Erased(erased) => todo!(),
            LayoutRepr::Ptr(ptr) => (self.intern().get_repr(ptr), ByRef),
            _ => (repr, ByRef),
        })
    }
    ///Convert the mono input into Roar
    pub fn build_section(&'b self, input: &MonoInput<'a>) -> Result<Section<'b>> {
        let procs = input
            .into_iter()
            .map(|((symbol, layout), proc)| self.build_proc(symbol, layout, proc).unwrap()) //TODO
            .collect_in::<bumpalo::collections::Vec<Proc>>(&self.arena);
        //let mut roar = Section::new(&self.arena);
        let (roar, refs) = Section::new(&self.arena).add_procs(procs);
        Ok(roar)
    }
    ///Build a single Mono procedure
    pub fn build_proc(
        &'b self,
        sym: &symbol::Symbol,
        layout: &ir::ProcLayout,
        proc: &ir::Proc<'a>,
    ) -> Result<Proc<'b>> {
        let sym_map: &mut Map<symbol::Symbol, SymbolInfo<'_>> = &mut Map::new();
        let args = proc
            .args
            .into_iter()
            .map(|(lay, sym): &(InLayout<'_>, symbol::Symbol)| -> Output {
                let (repr, form) = self.get_form(lay).unwrap(); //TODO
                let new_reg = self.register_alloc.new_register();
                sym_map.insert(*sym, (Output::Register(new_reg.clone()), repr, form));
                Output::Register(new_reg)
            })
            .collect();
        let mut roar_proc = Proc::new(args, &self.arena);
        Ok(roar_proc.add_ops(self.build_stmt(&proc.body, sym_map)?))
    }
    ///Build a single Mono "statement". Because Mono uses a `let in` structure of binding while ROAR uses an imperiative style of simply setting values, a single Mono statement almost always corresponds to mutiple ROAR statements
    pub fn build_stmt(
        &'b self,
        stmt: &ir::Stmt<'_>,
        sym_map: &mut SymbolMap<SymbolInfo<'_>>,
    ) -> Result<collections::Vec<Operation>> {
        println!("Trying to build stmt {:?}", stmt);
        use super::ops::OpCode::*;
        use super::storage::Input::*;
        match stmt {
            ir::Stmt::Let(symbol, expr, in_layout, rest) => {
                let r_expr = match expr {
                    ir::Expr::Literal(literal) => (Move, self.build_literal(literal)?, Null).into(),
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
    fn build_literal(&'b self, literal: &Literal<'_>) -> Result<Input> {
        todo!()
    }
    ///Make a function call
    pub fn build_call(
        &'b self,
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
            ir::CallType::ByPointer {
                pointer,
                ret_layout,
                arg_layouts,
            } => todo!(),
            ir::CallType::Foreign {
                foreign_symbol,
                ret_layout,
            } => todo!(),
            ir::CallType::LowLevel { op, update_mode } => {
                self.build_op(&op, &call.arguments, sym_map)?
            }
            ir::CallType::HigherOrder(higher_order_low_level) => todo!(),
        })
    }
    fn build_op(
        &'b self,
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
    fn build_num_op<'c>(
        &'b self,
        op_code: &low_level::LowLevel,
        args: &[symbol::Symbol],
        sym_map: &mut SymbolMap<SymbolInfo<'c>>,
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
        };
        todo!()
    }

    ///Build an access into a specific register
    ///Just a utility
    ///Always by value
    fn build_access(
        &'b self,
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
        &'b self,
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
        &'b self,
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
        &'b self,
        input: &Input,
        at_byte: Offset,
        bytes: ByteSize,
        sym_map: &mut SymbolMap<SymbolInfo<'_>>,
    ) -> Result<Expr> {
        todo!()
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
