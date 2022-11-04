use morphic_lib::{
    BlockExpr, CalleeSpecVar, EntryPointName, Error, ExprContext, FuncDefBuilder, FuncName,
    ModDefBuilder, ModName, ProgramBuilder, TypeContext, UpdateModeVar,
};

#[test]
fn test_basic() {
    fn run() -> Result<(), Error> {
        let func1_def = {
            let mut f = FuncDefBuilder::new();
            let b = f.add_block();
            f.add_update(b, UpdateModeVar(b"var1"), f.get_argument())?;
            let ret = f.add_make_tuple(b, &[])?;
            let arg_type = f.add_heap_cell_type();
            let ret_type = f.add_tuple_type(&[])?;
            f.build(arg_type, ret_type, BlockExpr(b, ret))?
        };

        let func2_def = {
            let mut f = FuncDefBuilder::new();
            let b = f.add_block();
            let cell = f.add_new_heap_cell(b)?;
            let ret = f.add_call(
                b,
                CalleeSpecVar(b"var2"),
                ModName(b"main_mod"),
                FuncName(b"func1"),
                cell,
            )?;
            let unit_type = f.add_tuple_type(&[])?;
            f.build(unit_type, unit_type, BlockExpr(b, ret))?
        };

        let main_mod = {
            let mut m = ModDefBuilder::new();
            m.add_func(FuncName(b"func1"), func1_def)?;
            m.add_func(FuncName(b"func2"), func2_def)?;
            m.build()?
        };

        let program = {
            let mut p = ProgramBuilder::new();
            p.add_mod(ModName(b"main_mod"), main_mod)?;
            p.add_entry_point(
                EntryPointName(b"main"),
                ModName(b"main_mod"),
                FuncName(b"func2"),
            )?;
            p.build()?
        };

        let program_sol = morphic_lib::solve(program)?;

        let (_, _, func2_spec) = program_sol.entry_point_solution(EntryPointName(b"main"))?;

        let main_mod_sol = program_sol.mod_solutions(ModName(b"main_mod"))?;

        let func2_sol = main_mod_sol
            .func_solutions(FuncName(b"func2"))?
            .spec(&func2_spec)?;

        let func1_spec = func2_sol.callee_spec(CalleeSpecVar(b"var2"))?;

        let func1_sol = main_mod_sol
            .func_solutions(FuncName(b"func1"))?
            .spec(&func1_spec)?;

        let _update_mode = func1_sol.update_mode(UpdateModeVar(b"var1"))?;

        Ok(())
    }

    let result = run();
    if let Err(err) = result {
        panic!("error: {}", err);
    }
}
