use morphic_lib::{
    BlockExpr, CalleeSpecVar, EntryPointName, Error, ExprContext, FuncDefBuilder, FuncName,
    ModDefBuilder, ModName, ProgramBuilder, TypeContext, UpdateMode, UpdateModeVar,
};

#[test]
fn test_recursive() {
    fn run() -> Result<(), Error> {
        let rec_def = {
            let mut f = FuncDefBuilder::new();
            let b = f.add_block();
            let arg = f.get_argument();
            let case1 = {
                let b = f.add_block();
                BlockExpr(b, arg)
            };
            let case2 = {
                let b = f.add_block();
                f.add_update(b, UpdateModeVar(b"mode"), arg)?;
                let new = f.add_new_heap_cell(b)?;
                let result = f.add_call(
                    b,
                    CalleeSpecVar(b"call"),
                    ModName(b"main"),
                    FuncName(b"rec"),
                    new,
                )?;
                BlockExpr(b, result)
            };
            let result = f.add_choice(b, &[case1, case2])?;
            let heap_cell_type = f.add_heap_cell_type();
            f.build(heap_cell_type, heap_cell_type, BlockExpr(b, result))?
        };

        let main_def = {
            let mut f = FuncDefBuilder::new();
            let b = f.add_block();
            let init = f.add_new_heap_cell(b)?;
            let final_ = f.add_call(
                b,
                CalleeSpecVar(b"call"),
                ModName(b"main"),
                FuncName(b"rec"),
                init,
            )?;
            f.add_touch(b, final_)?;
            let result = f.add_make_tuple(b, &[])?;
            let unit_type = f.add_tuple_type(&[])?;
            f.build(unit_type, unit_type, BlockExpr(b, result))?
        };

        let mod_ = {
            let mut m = ModDefBuilder::new();
            m.add_func(FuncName(b"rec"), rec_def)?;
            m.add_func(FuncName(b"main"), main_def)?;
            m.build()?
        };

        let program = {
            let mut p = ProgramBuilder::new();
            p.add_mod(ModName(b"main"), mod_)?;
            p.add_entry_point(
                EntryPointName(b"entry"),
                ModName(b"main"),
                FuncName(b"main"),
            )?;
            p.build()?
        };

        let program_sol = morphic_lib::solve(program)?;

        let (_, _, main_spec) = program_sol.entry_point_solution(EntryPointName(b"entry"))?;

        let main_mod_sol = program_sol.mod_solutions(ModName(b"main"))?;

        let main_sol = main_mod_sol
            .func_solutions(FuncName(b"main"))?
            .spec(&main_spec)?;

        let rec_spec = main_sol.callee_spec(CalleeSpecVar(b"call"))?;

        let rec_sol = main_mod_sol
            .func_solutions(FuncName(b"rec"))?
            .spec(&rec_spec)?;

        let update_mode = rec_sol.update_mode(UpdateModeVar(b"mode"))?;
        assert_eq!(update_mode, UpdateMode::InPlace);

        Ok(())
    }

    let result = run();
    if let Err(err) = result {
        panic!("error: {}", err);
    }
}
