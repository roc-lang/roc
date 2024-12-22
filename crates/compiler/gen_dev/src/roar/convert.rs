use super::proc::{Proc, Section};
use super::storage::*;
use bumpalo;

use roc_module::symbol;
use roc_mono::ir::Literal;
use roc_mono::{
    ir,
    layout::{InLayout, Layout, LayoutInterner, LayoutRepr, STLayoutInterner},
};
type SymbolMap<T> = roc_collections::MutMap<symbol::Symbol, T>;
///The type given as output by mono
type MonoInput<'a> =
    roc_collections::MutMap<(symbol::Symbol, mono::ProcLayout<'a>), mono::Proc<'a>>;
///The prefered type for maps, may change later
type Map<A : Hash,B> = std::collections::HashMap<A,B>;
///The type of information related to a symbol: what register holds it, how it is represented in mono, and whether it is stored by ref or value
type SymbolInfo<'a> = (Output,LayoutRepr<'a>,Form);
///The type of expressions. Not really expressions, just the non-output portion of a ROAR op
type Expr = (OpCode,Input,Input);
///How a layout is passed to a procedure, and it's internals
#[derive(Clone, Debug)]
enum Form {
    ///It takes it as a struct register
    ByRef,
    ///It takes it as a value
    ByValue,
    
}

///Controlls the conversion from Mono to ROAR
pub(super) struct Converter<'a, 'b> {
    ///The interner for Mono type layouts
    layout_interner: &'b mut STLayoutInterner<'a>,
    ///The arena, as this controls the Mono -> ROAR phase
    arena: bumpalo::Bump,
    ///Register names that have not been yet allocated 
    register_alloc: RegisterAllocater
}

pub(super) fn get_sym_reg(sym : &symbol::Symbol, map : &SymbolMap<SymbolInfo>) -> Option<Output> {
    map.get(sym).map(|x| x.0)
}
impl<'a, 'b> Converter<'a, 'b> {
    ///Create a converter
    pub fn new(layout_interner: &'b mut STLayoutInterner, arena: bumpalo::Bump) -> Self {
        Self {
            layout_interner: layout_interner,
            arena: arena,
            register_alloc: RegisterAllocater::new()
        }
    }
    ///Get the internal interner 
    pub fn intern(&mut self) -> &'b mut STLayoutInterner<'a> {
        self.layout_interner
    }
    ///From an interned layout get a regular layout and if it's possible to store it in a register
    pub fn get_form(&mut self, layout: &InLayout<'a>) -> (LayoutRepr,Form) {
        use Form::*;
        let repr = self.intern().get_repr(layout);
        match repr {
            LayoutRepr::Builtin(builtin) if self.intern().stack_size(layout) <= 8 => (repr,ByValue),
            LayoutRepr::LambdaSet(lambda_set) => todo!(),
            LayoutRepr::FunctionPointer(function_pointer) => todo!(),
            LayoutRepr::Erased(erased) => todo!(),
            LayoutRepr::Ptr(ptr) => (self.intern().get_repr(ptr),ByRef),
            _ => (repr,ByRef)
        }
    }
    ///Convert the mono input into Roar
    pub fn build_section(&mut self, input: &MonoInput<'_>) -> Section {
        let mut roar = Proc::new(todo!());
        for ((symbol, layout), proc) in input {
            roar = self.build_proc(symbol, layout, proc)
        }
        todo!()
    }
    ///Build a single Mono procedure 
    pub fn build_proc(
        &mut self,
        sym: &symbol::Symbol,
        layout: &mono::ProcLayout,
        proc: &mono::Proc<'a>,
    ) -> Proc {
        let mut sym_map : SymbolMap<SymbolInfo<'_>> = Map::new();
        let args = proc.args.into_iter().map(|(lay,sym) : (&InLayout<'_>,symbol::Symbol)| -> Output {
            let (repr,form) = self.get_form(lay);
            let new_reg = self.register_alloc.new_register();
            sym_map.insert(sym,(new_reg,repr,form));
            Output::Register(new_reg)
        });
        let mut proc = Proc::new(args);
    }
    ///Build a single Mono "statement". Because Mono uses a `let in` structure of binding while ROAR uses an imperiative style of simply setting values, a single Mono statement almost always corresponds to mutiple ROAR statements 
    pub fn build_stmt(
        &mut self,
        stmt : ir::Stmt<'_>,
        sym_map : &mut SymbolMap<SymbolInfo<'_>> 
    ) -> collections::Vec<Operation> {
        use super::ops::OpCode::*;
        use super::storage::Input::*;
        match stmt {
            ir::Stmt::Let(symbol, expr, in_layout, rest) => {
                let (op,arg_a,arg_b) = match expr {
                    ir::Expr::Literal(literal) => (Move,self.build_literal(literal),Empty),
                    ir::Expr::Call(call) => self.build_call(call, sym_map),
                    ir::Expr::Tag { tag_layout, tag_id, arguments, reuse } => todo!(),
                    ir::Expr::Struct(_) => todo!(),
                    ir::Expr::NullPointer => todo!(),
                    ir::Expr::StructAtIndex { index, field_layouts, structure } => todo!(),
                    ir::Expr::GetTagId { structure, union_layout } => todo!(),
                    ir::Expr::UnionAtIndex { structure, tag_id, union_layout, index } => todo!(),
                    ir::Expr::GetElementPointer { structure, union_layout, indices } => todo!(),
                    ir::Expr::Array { elem_layout, elems } => todo!(),
                    ir::Expr::EmptyArray => todo!(),
                    ir::Expr::ErasedMake { value, callee } => todo!(),
                    ir::Expr::ErasedLoad { symbol, field } => todo!(),
                    ir::Expr::FunctionPointer { lambda_name } => todo!(),
                    ir::Expr::Alloca { element_layout, initializer } => todo!(),
                    ir::Expr::Reset { symbol, update_mode } => todo!(),
                    ir::Expr::ResetRef { symbol, update_mode } => todo!(),
                };
            },
            ir::Stmt::Switch { cond_symbol, cond_layout, branches, default_branch, ret_layout } => todo!(),
            ir::Stmt::Ret(symbol) => todo!(),
            ir::Stmt::Refcounting(modify_rc, _) => todo!(),
            ir::Stmt::Expect { condition, region, lookups, variables, remainder } => todo!(),
            ir::Stmt::Dbg { source_location, source, symbol, variable, remainder } => todo!(),
            ir::Stmt::Join { id, parameters, body, remainder } => todo!(),
            ir::Stmt::Jump(join_point_id, _) => todo!(),
            ir::Stmt::Crash(symbol, crash_tag) => todo!(),
        }
    }
    ///Get a literal representation
    fn build_literal(
        &mut self,
        literal : Literal<'_>
    ) -> Constant {
        todo!()
    }
    ///Make a function call
    pub fn build_call(
        &mut self,
        call : ir::Call<'_>,
        sym_map : &mut SymbolMap<SymbolInfo<'_>> 
    ) -> Expr {
    }

}
