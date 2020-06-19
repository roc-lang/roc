#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate libc;
extern crate roc_gen;

// #[macro_use]
mod helpers;

#[cfg(test)]
mod gen_module {
    use crate::helpers::{can_expr, infer_expr, uniq_expr, CanExprOut};
    use bumpalo::Bump;
    use inkwell::context::Context;
    use inkwell::passes::PassManager;
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc_builtins::std::Mode;
    use roc_can::{
        builtins::builtin_defs,
        def::Declaration,
        expected::Expected,
        expr::{canonicalize_expr, Env},
        module::{canonicalize_module_defs, Module},
        operator,
        scope::Scope,
    };
    use roc_collections::all::{ImMap, MutMap, MutSet};
    use roc_constrain::module::{
        constrain_imports, constrain_module, pre_constrain_imports, ConstrainableImports,
    };
    use roc_gen::llvm::build::{build_proc, build_proc_header};
    use roc_gen::llvm::convert::basic_type_from_layout;
    use roc_module::symbol::{IdentIds, Interns, ModuleId, ModuleIds};
    use roc_mono::expr::{Expr, Procs};
    use roc_mono::layout::Layout;
    use roc_parse::ast::{self, Attempting};
    use roc_parse::blankspace::space0_before;
    use roc_parse::parser::{self, loc, Parser};
    use roc_region::all::{Located, Region};
    use roc_solve::module::solve_module;
    use roc_types::subs::{Subs, VarStore};
    use roc_types::types::Type;

    static TEST_MODULE_NAME: &str = "Test";

    macro_rules! run_main {
        ($execution_engine:expr, $main_fn_name: expr, $expected:expr, $ty:ty, $transform:expr) => {
            unsafe {
                let main: inkwell::execution_engine::JitFunction<unsafe extern "C" fn() -> $ty> =
                    $execution_engine
                        .get_function($main_fn_name)
                        .ok()
                        .ok_or(format!("Unable to JIT compile `{}`", $main_fn_name))
                        .expect("errored");

                assert_eq!($transform(main.call()), $expected);
            }
        };
    }

    fn compile_main<'a>(
        arena: &'a Bump,
        src: &str,
        main_fn_name: &str,
        mode: Mode,
        ptr_bytes: u32,
    ) -> (ModuleId, Interns, Subs, Layout<'a>, roc_can::expr::Expr) {
        // Parse and desugar the src
        let loc_expr = space0_before(loc(roc_parse::expr::expr(0)), 0)
            .parse(&arena, parser::State::new(src, Attempting::Module))
            .map(|(loc_expr, _)| operator::desugar_expr(arena, &loc_expr))
            .unwrap_or_else(|(e, _)| {
                panic!(
                    "got a parse error when attempting to canonicalize:\n\n{:?} {:?}",
                    src, e
                )
            });

        // Make an already-parsed module with this expr exposed as main
        let main_pattern = ast::Pattern::Identifier(main_fn_name);
        let loc_main_pattern = Located {
            region: Region::zero(),
            value: main_pattern,
        };
        let loc_def = Located {
            region: Region::zero(),
            value: ast::Def::Body(&loc_main_pattern, &loc_expr),
        };
        let loc_defs = bumpalo::vec![in arena; loc_def];
        let module_ids = ModuleIds::default();
        let home: ModuleId = module_ids.get_or_insert(&TEST_MODULE_NAME.into());
        let dep_idents = MutMap::default();
        let exposed_imports = MutMap::default();
        let exposed_symbols = MutSet::default();
        let mut var_store = VarStore::default();
        let exposed_ident_ids: IdentIds = IdentIds::default();

        exposed_ident_ids.add(main_fn_name.into());

        // Canonicalize the module.
        let mut module_output = canonicalize_module_defs(
            arena,
            loc_defs,
            home,
            &module_ids,
            exposed_ident_ids,
            dep_idents,
            exposed_imports,
            exposed_symbols,
            &mut var_store,
        )
        .expect("Error canonicalizing test module");

        // Add the builtins as Declarations to the module.
        let module_output = {
            let builtins = builtin_defs(&mut var_store);
            let decls = &mut module_output.declarations;
            let references = module_output.references;

            decls.reserve(builtins.len());

            for (symbol, builtin_def) in builtins.into_iter() {
                // Only add decls for builtins that were actually referenced.
                if references.contains(&symbol) {
                    decls.push(Declaration::Builtin(builtin_def));
                }
            }

            // Release the borrows on declarations and references
            module_output
        };

        let module = Module {
            module_id: home,
            exposed_imports: module_output.exposed_imports,
            exposed_vars_by_symbol: module_output.exposed_vars_by_symbol,
            references: module_output.references,
            aliases: module_output.aliases,
            rigid_variables: module_output.rigid_variables,
        };

        let constraint = {
            let constraint = constrain_module(&module_output, home, mode, &mut var_store);

            let ConstrainableImports {
                imported_symbols,
                imported_aliases,
                unused_imports,
            } = pre_constrain_imports(
                home,
                &module.references,
                imported_modules,
                exposed_types,
                stdlib,
            );

            assert_eq!(unused_imports, MutSet::default());

            let constraint = constrain_imports(
                imported_symbols,
                imported_aliases,
                constraint,
                &mut var_store,
            );
            let mut constraint = load_builtin_aliases(stdlib.aliases, constraint, &mut var_store);

            // Turn Apply into Alias
            constraint.instantiate_aliases(&mut var_store);

            constraint
        };

        let (solved_subs, solved_module) = solve_module(module, constraint, var_store);

        // ModuleOutput
        // pub aliases: MutMap<Symbol, Alias>,
        // pub rigid_variables: MutMap<Variable, Lowercase>,
        // pub declarations: Vec<Declaration>,
        // pub exposed_imports: MutMap<Symbol, Variable>,
        // pub lookups: Vec<(Symbol, Variable, Region)>,
        // pub problems: Vec<Problem>,
        // pub ident_ids: IdentIds,
        // pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
        // pub references: MutSet<Symbol>,

        // Module
        // pub module_id: ModuleId,
        // pub exposed_imports: MutMap<Symbol, Variable>,
        // pub exposed_vars_by_symbol: Vec<(Symbol, Variable)>,
        // pub references: MutSet<Symbol>,
        // pub aliases: MutMap<Symbol, Alias>,
        // pub rigid_variables: MutMap<Variable, Lowercase>,
        // TODO type-check the module, making sure to use builtin types from std.rs/unique.rs
        // TODO monomorphize the module
        // TODO code gen the module

        let var = var_store.fresh();
        let expected = Expected::NoExpectation(Type::Variable(var));
        let module_ids = ModuleIds::default();

        let mut scope = Scope::new(home);
        let dep_idents = IdentIds::exposed_builtins(0);
        let mut env = Env::new(home, dep_idents, &module_ids, IdentIds::default());
        let (loc_expr, output) = canonicalize_expr(
            &mut env,
            &mut var_store,
            &mut scope,
            Region::zero(),
            &loc_expr.value,
        );

        let loc_expr = Located {
            region: loc_expr.region,
            value: with_builtins,
        };

        let constraint = constrain_expr(
            &roc_constrain::expr::Env {
                rigids: ImMap::default(),
                home,
            },
            loc_expr.region,
            &loc_expr.value,
            expected,
        );

        let types = roc_builtins::std::types();

        let imports: Vec<_> = types
            .iter()
            .map(|(symbol, (solved_type, region))| Import {
                loc_symbol: Located::at(*region, *symbol),
                solved_type: solved_type,
            })
            .collect();

        // load builtin values
        let (_introduced_rigids, constraint) =
            constrain_imported_values(imports, constraint, &var_store);

        // TODO determine what to do with those rigids
        //    for var in introduced_rigids {
        //        output.ftv.insert(var, format!("internal_{:?}", var).into());
        //    }

        //load builtin types
        let mut constraint =
            load_builtin_aliases(&roc_builtins::std::aliases(), constraint, &var_store);

        constraint.instantiate_aliases(&var_store);

        let mut all_ident_ids = MutMap::default();

        // When pretty printing types, we may need the exposed builtins,
        // so include them in the Interns we'll ultimately return.
        for (module_id, ident_ids) in IdentIds::exposed_builtins(0) {
            all_ident_ids.insert(module_id, ident_ids);
        }

        all_ident_ids.insert(home, env.ident_ids);

        let interns = Interns {
            module_ids: env.module_ids.clone(),
            all_ident_ids,
        };
        let errors = problems
            .into_iter()
            .filter(|problem| {
                use roc_problem::can::Problem::*;

                // Ignore "unused" problems
                match problem {
                    UnusedDef(_, _) | UnusedArgument(_, _, _) | UnusedImport(_, _) => false,
                    _ => true,
                }
            })
            .collect::<Vec<roc_problem::can::Problem>>();

        assert_eq!(errors, Vec::new(), "Encountered errors: {:?}", errors);

        let subs = Subs::new(var_store.into());
        let mut unify_problems = Vec::new();
        let (content, subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        assert_eq!(
            unify_problems,
            Vec::new(),
            "Encountered type mismatches: {:?}",
            unify_problems
        );

        let layout = Layout::new(&arena, content, &subs, ptr_bytes)
        .unwrap_or_else(|err| panic!("Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}", err, subs));

        (home, interns, subs, layout, loc_expr.value)
    }

    macro_rules! assert_llvm_evals_to {
        ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
            let arena = Bump::new();
            let target = target_lexicon::Triple::host();
            let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;
            let main_fn_name = "#main";
            let mode =
                if cfg!(debug_assertions) {
                    Mode::Standard
                } else { Mode::Uniqueness };
            let (home, interns, mut subs, main_layout, can_expr) = compile_main(&arena, $src, main_fn_name, mode, ptr_bytes);
            let context = Context::create();
            let module = roc_gen::llvm::build::module_from_builtins(&context, "app");
            let builder = context.create_builder();
            let opt_level = if cfg!(debug_assertions) {
                roc_gen::llvm::build::OptLevel::Normal
            } else {
                roc_gen::llvm::build::OptLevel::Optimize
            };
            let fpm = PassManager::create(&module);

            roc_gen::llvm::build::add_passes(&fpm, opt_level);

            fpm.initialize();

            // Compute main_fn_type before moving subs to Env
            let execution_engine =
                module
                .create_jit_execution_engine(OptimizationLevel::None)
                .expect("Error creating JIT execution engine for test");

            let main_fn_type = basic_type_from_layout(&arena, &context, &main_layout, ptr_bytes)
                .fn_type(&[], false);

            // Compile and add all the Procs before adding main
            let mut env = roc_gen::llvm::build::Env {
                arena: &arena,
                builder: &builder,
                context: &context,
                interns,
                module: arena.alloc(module),
                ptr_bytes
            };
            let mut procs = Procs::default();
            let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();
            let mut layout_ids = roc_gen::layout_id::LayoutIds::default();

            // Populate Procs and get the low-level Expr from the canonical Expr
            let mut mono_problems = Vec::new();
            let mut mono_env = roc_mono::expr::Env {
                arena: &arena,
                subs: &mut subs,
                problems: &mut mono_problems,
                home,
                ident_ids: &mut ident_ids,
                pointer_size: ptr_bytes,
                jump_counter: arena.alloc(0),
            };

            println!("==============================EXPR NEW");

            let main_body = Expr::new(&mut mono_env, can_expr, &mut procs);
            println!("==============================FIN EXPR NEW");

            let mut headers = Vec::with_capacity(procs.pending_specializations.len());
            let mut layout_cache = roc_mono::layout::LayoutCache::default();

            let (mut specializations, runtime_errors) =
                roc_mono::expr::specialize_all(&mut mono_env, procs, &mut layout_cache);

            assert_eq!(runtime_errors, roc_collections::all::MutSet::default());

            // Put this module's ident_ids back in the interns, so we can use them in env.
            // This must happen *after* building the headers, because otherwise there's
            // a conflicting mutable borrow on ident_ids.
            env.interns.all_ident_ids.insert(home, ident_ids);

            // Add all the Proc headers to the module.
            // We have to do this in a separate pass first,
            // because their bodies may reference each other.
            for (symbol, layout, proc) in specializations.drain(..) {
                let (fn_val, arg_basic_types) =
                    build_proc_header(&env, &mut layout_ids, symbol, &layout, &proc);

                headers.push((proc, fn_val, arg_basic_types));
            }

            // Build each proc using its header info.
            for (proc, fn_val, arg_basic_types) in headers {
                // NOTE: This is here to be uncommented in case verification fails.
                // (This approach means we don't have to defensively clone name here.)
                //
                // println!("\n\nBuilding and then verifying function {}\n\n", name);
                build_proc(&env, &mut layout_ids, proc, fn_val, arg_basic_types);

                if fn_val.verify(true) {
                    fpm.run_on(&fn_val);
                } else {
                    // NOTE: If this fails, uncomment the above println to debug.
                    panic!(
                        "Non-main function failed LLVM verification. Uncomment the above println to debug!"
                    );
                }
            }

            // Add main to the module.
            let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);
            let cc = roc_gen::llvm::build::get_call_conventions(target.default_calling_convention().unwrap());

            main_fn.set_call_conventions(cc);

            // Add main's body
            let basic_block = context.append_basic_block(main_fn, "entry");

            builder.position_at_end(basic_block);

            let ret = roc_gen::llvm::build::build_expr(
                &env,
                &mut layout_ids,
                &ImMap::default(),
                main_fn,
                &main_body,
            );

            builder.build_return(Some(&ret));

            // Uncomment this to see the module's un-optimized LLVM instruction output:
            // env.module.print_to_stderr();

            if main_fn.verify(true) {
                fpm.run_on(&main_fn);
            } else {
                panic!("Function {} failed LLVM verification.", main_fn_name);
            }

            // Verify the module
            if let Err(errors) = env.module.verify() {
                panic!("Errors defining module: {:?}", errors);
            }

            // Uncomment this to see the module's optimized LLVM instruction output:
            // env.module.print_to_stderr();

            run_main!(execution_engine, main_fn_name, $expected, $ty, $transform);
        };
    }

    // TODO this is almost all code duplication with assert_llvm_evals_to
    // the only difference is that this calls uniq_expr instead of can_expr.
    // Should extract the common logic into test helpers.
    // #[macro_export]
    macro_rules! assert_opt_evals_to {
        ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
            let arena = Bump::new();
            let target = target_lexicon::Triple::host();
            let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;
            let (loc_expr, _output, problems, subs, var, constraint, home, interns) = uniq_expr($src);
            let errors = problems.into_iter().filter(|problem| {
                use roc_problem::can::Problem::*;

                // Ignore "unused" problems
                match problem {
                    UnusedDef(_, _) | UnusedArgument(_, _, _) | UnusedImport(_, _) => false,
                    _ => true,
                }
            }).collect::<Vec<roc_problem::can::Problem>>();

            assert_eq!(errors, Vec::new(), "Encountered errors: {:?}", errors);

            let mut unify_problems = Vec::new();
            let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

            assert_eq!(unify_problems, Vec::new(), "Encountered one or more type mismatches: {:?}", unify_problems);

            let context = Context::create();
            let module = roc_gen::llvm::build::module_from_builtins(&context, "app");
            let builder = context.create_builder();
            let opt_level = if cfg!(debug_assertions) {
                roc_gen::llvm::build::OptLevel::Normal
            } else {
                roc_gen::llvm::build::OptLevel::Optimize
            };
            let fpm = PassManager::create(&module);

            roc_gen::llvm::build::add_passes(&fpm, opt_level);

            fpm.initialize();

            // Compute main_fn_type before moving subs to Env
            let layout = Layout::new(&arena, content, &subs, ptr_bytes)
        .unwrap_or_else(|err| panic!("Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}", err, subs));

            let execution_engine =
                module
                .create_jit_execution_engine(OptimizationLevel::None)
                .expect("Error creating JIT execution engine for test");

            let main_fn_type = basic_type_from_layout(&arena, &context, &layout, ptr_bytes)
                .fn_type(&[], false);
            let main_fn_name = "$Test.main";

            // Compile and add all the Procs before adding main
            let mut env = roc_gen::llvm::build::Env {
                arena: &arena,
                builder: &builder,
                context: &context,
                interns,
                module: arena.alloc(module),
                ptr_bytes
            };
            let mut procs = Procs::default();
            let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();
            let mut layout_ids = roc_gen::layout_id::LayoutIds::default();

            // Populate Procs and get the low-level Expr from the canonical Expr
            let mut mono_problems = Vec::new();
            let mut mono_env = roc_mono::expr::Env {
                arena: &arena,
                subs: &mut subs,
                problems: &mut mono_problems,
                home,
                ident_ids: &mut ident_ids,
                pointer_size: ptr_bytes,
                jump_counter: arena.alloc(0),
            };
            let main_body = Expr::new(&mut mono_env, loc_expr.value, &mut procs);

            let mut headers = Vec::with_capacity(procs.pending_specializations.len());
            let mut layout_cache = roc_mono::layout::LayoutCache::default();

            let (mut specializations, runtime_errors) =
                roc_mono::expr::specialize_all(&mut mono_env, procs, &mut layout_cache);

            assert_eq!(runtime_errors, roc_collections::all::MutSet::default());

            // Put this module's ident_ids back in the interns, so we can use them in env.
            // This must happen *after* building the headers, because otherwise there's
            // a conflicting mutable borrow on ident_ids.
            env.interns.all_ident_ids.insert(home, ident_ids);

            // Add all the Proc headers to the module.
            // We have to do this in a separate pass first,
            // because their bodies may reference each other.
            for (symbol, layout, proc) in specializations.drain(..) {
                let (fn_val, arg_basic_types) =
                    build_proc_header(&env, &mut layout_ids, symbol, &layout, &proc);

                headers.push((proc, fn_val, arg_basic_types));
            }

            // Build each proc using its header info.
            for (proc, fn_val, arg_basic_types) in headers {
                // NOTE: This is here to be uncommented in case verification fails.
                // (This approach means we don't have to defensively clone name here.)
                //
                // println!("\n\nBuilding and then verifying function {}\n\n", name);
                build_proc(&env, &mut layout_ids, proc, fn_val, arg_basic_types);

                if fn_val.verify(true) {
                    fpm.run_on(&fn_val);
                } else {
                    // NOTE: If this fails, uncomment the above println to debug.
                    panic!(
                        "Non-main function failed LLVM verification. Uncomment the above println to debug!"
                    );
                }
            }

            // Add main to the module.
            let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);
            let cc = roc_gen::llvm::build::get_call_conventions(target.default_calling_convention().unwrap());

            main_fn.set_call_conventions(cc);

            // Add main's body
            let basic_block = context.append_basic_block(main_fn, "entry");

            builder.position_at_end(basic_block);

            let ret = roc_gen::llvm::build::build_expr(
                &env,
                &mut layout_ids,
                &ImMap::default(),
                main_fn,
                &main_body,
            );

            builder.build_return(Some(&ret));

            // Uncomment this to see the module's un-optimized LLVM instruction output:
            // env.module.print_to_stderr();

            if main_fn.verify(true) {
                fpm.run_on(&main_fn);
            } else {
                panic!("Function {} failed LLVM verification.", main_fn_name);
            }

            // Verify the module
            if let Err(errors) = env.module.verify() {
                panic!("Errors defining module: {:?}", errors);
            }

            // Uncomment this to see the module's optimized LLVM instruction output:
            // env.module.print_to_stderr();

            run_main!(execution_engine, main_fn_name, $expected, $ty, $transform);
        };
    }

    // #[macro_export]
    macro_rules! assert_evals_to {
        ($src:expr, $expected:expr, $ty:ty) => {
            // Run un-optimized tests, and then optimized tests, in separate scopes.
            // These each rebuild everything from scratch, starting with
            // parsing the source, so that there's no chance their passing
            // or failing depends on leftover state from the previous one.
            {
                assert_llvm_evals_to!($src, $expected, $ty, (|val| val));
            }
            {
                assert_opt_evals_to!($src, $expected, $ty, (|val| val));
            }
        };
        ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
            // Same as above, except with an additional transformation argument.
            {
                assert_llvm_evals_to!($src, $expected, $ty, $transform);
            }
            {
                assert_opt_evals_to!($src, $expected, $ty, $transform);
            }
        };
    }

    #[test]
    fn f64_sqrt() {
        assert_evals_to!("False", false, bool);
    }
}
