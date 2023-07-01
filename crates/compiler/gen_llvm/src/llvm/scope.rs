use inkwell::{
    basic_block::BasicBlock,
    values::{BasicValueEnum, FunctionValue, PhiValue},
};
use roc_collections::ImMap;
use roc_module::symbol::{ModuleId, Symbol};
use roc_mono::{
    ir::{JoinPointId, Param, ProcLayout},
    layout::InLayout,
};

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub(crate) struct Scope<'a, 'ctx> {
    symbols: ImMap<Symbol, (InLayout<'a>, BasicValueEnum<'ctx>)>,
    top_level_thunks: ImMap<Symbol, (ProcLayout<'a>, FunctionValue<'ctx>)>,
    join_points: ImMap<JoinPointId, (BasicBlock<'ctx>, Vec<PhiValue<'ctx>>)>,
}

#[derive(Debug)]
pub(crate) struct JoinPointNotFound;

impl<'a, 'ctx> Scope<'a, 'ctx> {
    pub fn insert(&mut self, symbol: Symbol, layout: InLayout<'a>, value: BasicValueEnum<'ctx>) {
        self.symbols.insert(symbol, (layout, value));
    }

    pub fn load_symbol(&self, symbol: &Symbol) -> BasicValueEnum<'ctx> {
        match self.symbols.get(symbol) {
            Some((_, ptr)) => *ptr,

            None => panic!(
                "There was no entry for {:?} {} in scope {:?}",
                symbol, symbol, self
            ),
        }
    }

    pub fn load_symbol_and_layout(&self, symbol: &Symbol) -> (BasicValueEnum<'ctx>, InLayout<'a>) {
        match self.symbols.get(symbol) {
            Some((layout, ptr)) => (*ptr, *layout),
            None => panic!("There was no entry for {:?} in scope {:?}", symbol, self),
        }
    }

    pub fn insert_top_level_thunk(
        &mut self,
        symbol: Symbol,
        layout: ProcLayout<'a>,
        function_value: FunctionValue<'ctx>,
    ) {
        self.top_level_thunks
            .insert(symbol, (layout, function_value));
    }

    pub fn remove(&mut self, symbol: &Symbol) {
        self.symbols.remove(symbol);
    }

    pub fn retain_top_level_thunks_for_module(&mut self, module_id: ModuleId) {
        self.top_level_thunks
            .retain(|s, _| s.module_id() == module_id);
    }

    pub fn insert_join_point(
        &mut self,
        join_point_id: JoinPointId,
        bb: BasicBlock<'ctx>,
        phis: Vec<PhiValue<'ctx>>,
    ) {
        self.join_points.insert(join_point_id, (bb, phis));
    }

    pub fn remove_join_point(&mut self, join_point_id: JoinPointId) {
        self.join_points.remove(&join_point_id);
    }

    pub fn get_join_point(
        &self,
        join_point_id: JoinPointId,
    ) -> Option<&(BasicBlock<'ctx>, Vec<PhiValue<'ctx>>)> {
        self.join_points.get(&join_point_id)
    }

    pub fn bind_parameters_to_join_point(
        &mut self,
        join_point_id: JoinPointId,
        parameters: impl IntoIterator<Item = &'a Param<'a>>,
    ) -> Result<(), JoinPointNotFound> {
        let ref_join_points = &self
            .join_points
            .get(&join_point_id)
            .ok_or(JoinPointNotFound)?
            .1;

        for (phi_value, param) in ref_join_points.iter().zip(parameters.into_iter()) {
            let value = phi_value.as_basic_value();
            self.symbols.insert(param.symbol, (param.layout, value));
        }

        Ok(())
    }
}
