use roc_mono::{ir as mono, layout::{InLayout, Layout, LayoutInterner, STLayoutInterner}};
use super::storage::*;
use super::proc::{Proc,Section};
use roc_module::symbol;
type SymbolMap<T> = roc_collections::MutMap<symbol::Symbol, T>;
type MonoInput<'a> = roc_collections::MutMap<(symbol::Symbol, mono::ProcLayout<'a>), mono::Proc<'a>>;

///Controlls the conversion from Mono to ROAR
pub(super) struct Converter<'a,'b> {
    layout_interner: &'b mut STLayoutInterner<'a>,
}
impl<'a,'b> Converter<'a,'b> {

    pub fn get_layout(&mut self,layout : &InLayout<'a>) -> Layout<'a> {
        self.layout_interner.get(*layout)
    }
    ///Convert the mono input into Roar
    pub fn build_section(&mut self,input: &MonoInput<'_>) -> Section {
        let mut roar = Proc::new(todo!());
        for ((symbol, layout), proc) in input {
            roar = self.build_proc(symbol, layout, proc)
        }
        todo!()
    }
    pub fn build_proc(&mut self,sym : &symbol::Symbol, layout: &mono::ProcLayout, proc: &mono::Proc<'a>) -> Proc {
        todo!()
    }
}


