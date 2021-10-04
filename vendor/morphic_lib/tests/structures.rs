use morphic_lib::{
    BlockExpr, EntryPointName, Error, ExprContext, FuncDefBuilder, FuncName, ModDefBuilder,
    ModName, ProgramBuilder, TypeContext, UpdateMode, UpdateModeVar,
};

#[test]
fn test_structures() {
    fn run() -> Result<(), Error> {
        let main_def = {
            let mut f = FuncDefBuilder::new();
            let b = f.add_block();
            let h1 = f.add_new_heap_cell(b)?;
            let h2 = f.add_new_heap_cell(b)?;
            let t = f.add_make_tuple(b, &[h1, h2])?;
            let heap_cell_type = f.add_heap_cell_type();
            let u1 = f.add_make_union(b, &[heap_cell_type, heap_cell_type], 0, h1)?;
            let u2 = f.add_make_union(b, &[heap_cell_type, heap_cell_type], 1, h2)?;
            let h3 = f.add_get_tuple_field(b, t, 0)?;
            let h4 = f.add_get_tuple_field(b, t, 1)?;
            let h5 = f.add_unwrap_union(b, u1, 0)?;
            let h6 = f.add_unwrap_union(b, u2, 1)?;
            f.add_touch(b, h3)?;
            f.add_update_write_only(b, UpdateModeVar(b"mode1"), h1)?;
            f.add_update_write_only(b, UpdateModeVar(b"mode2"), h2)?;
            f.add_update_write_only(b, UpdateModeVar(b"mode3"), h5)?;
            f.add_update_write_only(b, UpdateModeVar(b"mode4"), h6)?;
            f.add_touch(b, h4)?;
            let unit = f.add_make_tuple(b, &[])?;
            let unit_type = f.add_tuple_type(&[])?;
            f.build(unit_type, unit_type, BlockExpr(b, unit))?
        };

        let main_mod = {
            let mut m = ModDefBuilder::new();
            m.add_func(FuncName(b"main"), main_def)?;
            m.build()?
        };

        let program = {
            let mut p = ProgramBuilder::new();
            p.add_mod(ModName(b"main"), main_mod)?;
            p.add_entry_point(EntryPointName(b"main"), ModName(b"main"), FuncName(b"main"))?;
            p.build()?
        };

        let program_sol = morphic_lib::solve(program)?;

        let (_, _, main_spec) = program_sol.entry_point_solution(EntryPointName(b"main"))?;

        let main_mod_sol = program_sol.mod_solutions(ModName(b"main"))?;

        let main_def_sol = main_mod_sol
            .func_solutions(FuncName(b"main"))?
            .spec(&main_spec)?;

        let mode1 = main_def_sol.update_mode(UpdateModeVar(b"mode1"))?;
        let mode2 = main_def_sol.update_mode(UpdateModeVar(b"mode2"))?;
        let mode3 = main_def_sol.update_mode(UpdateModeVar(b"mode3"))?;
        let mode4 = main_def_sol.update_mode(UpdateModeVar(b"mode4"))?;

        assert_eq!(mode1, UpdateMode::InPlace);
        assert_eq!(mode2, UpdateMode::Immutable);
        assert_eq!(mode3, UpdateMode::InPlace);
        assert_eq!(mode4, UpdateMode::Immutable);

        Ok(())
    }

    let result = run();
    if let Err(err) = result {
        panic!("error: {}", err);
    }
}
