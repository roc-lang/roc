use crate::repl::eval;
use bumpalo::Bump;
use inkwell::context::Context;
use roc_build::link::module_to_dylib;
use roc_collections::all::{MutMap, MutSet};
use roc_fmt::annotation::Formattable;
use roc_fmt::annotation::{Newlines, Parens};
use roc_gen::llvm::build::{build_proc, build_proc_header, OptLevel};
use roc_parse::parser::Fail;
use roc_types::pretty_print::{content_to_string, name_all_type_vars};
use std::path::{Path, PathBuf};
use std::str::from_utf8_unchecked;
use target_lexicon::Triple;

pub enum ReplOutput {
    Problems(Vec<String>),
    NoProblems { expr: String, expr_type: String },
}

pub fn gen_and_eval(src: &[u8], target: Triple, opt_level: OptLevel) -> Result<ReplOutput, Fail> {
    use roc_reporting::report::{
        can_problem, mono_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE,
    };

    let arena = Bump::new();

    // SAFETY: we've already verified that this is valid UTF-8 during parsing.
    let src_str: &str = unsafe { from_utf8_unchecked(src) };

    let stdlib = roc_builtins::std::standard_stdlib();
    let stdlib_mode = stdlib.mode;
    let filename = PathBuf::from("REPL.roc");
    let src_dir = Path::new("fake/test/path");

    let module_src = promote_expr_to_module(src_str);

    let exposed_types = MutMap::default();
    let loaded = roc_load::file::load_and_monomorphize_from_str(
        &arena,
        filename,
        &module_src,
        stdlib,
        src_dir,
        exposed_types,
    );

    let mut loaded = loaded.expect("failed to load module");

    use roc_load::file::MonomorphizedModule;
    let MonomorphizedModule {
        mut procedures,
        interns,
        exposed_to_host,
        mut subs,
        module_id: home,
        sources,
        ..
    } = loaded;

    let mut lines = Vec::new();

    for (home, (module_path, src)) in sources {
        let can_problems = loaded.can_problems.remove(&home).unwrap_or_default();
        let type_problems = loaded.type_problems.remove(&home).unwrap_or_default();
        let mono_problems = loaded.mono_problems.remove(&home).unwrap_or_default();

        let error_count = can_problems.len() + type_problems.len() + mono_problems.len();

        if error_count == 0 {
            continue;
        }

        let src_lines: Vec<&str> = src.split('\n').collect();
        let palette = DEFAULT_PALETTE;

        // Report parsing and canonicalization problems
        let alloc = RocDocAllocator::new(&src_lines, home, &interns);

        for problem in can_problems.into_iter() {
            let report = can_problem(&alloc, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }

        for problem in type_problems {
            let report = type_problem(&alloc, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }

        for problem in mono_problems {
            let report = mono_problem(&alloc, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }
    }

    if !lines.is_empty() {
        Ok(ReplOutput::Problems(lines))
    } else {
        let context = Context::create();
        let module = arena.alloc(roc_gen::llvm::build::module_from_builtins(&context, "app"));
        let builder = context.create_builder();

        debug_assert_eq!(exposed_to_host.len(), 1);
        let (main_fn_symbol, main_fn_var) = exposed_to_host.iter().next().unwrap();
        let main_fn_symbol = *main_fn_symbol;
        let main_fn_var = *main_fn_var;

        // pretty-print the expr type string for later.
        name_all_type_vars(main_fn_var, &mut subs);
        let content = subs.get(main_fn_var).content;
        let expr_type_str = content_to_string(content.clone(), &subs, home, &interns);

        let (_, main_fn_layout) = procedures
            .keys()
            .find(|(s, _)| *s == main_fn_symbol)
            .unwrap()
            .clone();

        let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;

        let module = arena.alloc(module);
        let (module_pass, function_pass) =
            roc_gen::llvm::build::construct_optimization_passes(module, opt_level);

        // Compile and add all the Procs before adding main
        let env = roc_gen::llvm::build::Env {
            arena: &arena,
            builder: &builder,
            context: &context,
            interns,
            module,
            ptr_bytes,
            leak: false,
            // important! we don't want any procedures to get the C calling convention
            exposed_to_host: MutSet::default(),
        };

        let mut layout_ids = roc_mono::layout::LayoutIds::default();
        let mut headers = Vec::with_capacity(procedures.len());

        // Add all the Proc headers to the module.
        // We have to do this in a separate pass first,
        // because their bodies may reference each other.
        let mut scope = roc_gen::llvm::build::Scope::default();
        for ((symbol, layout), proc) in procedures.drain() {
            let fn_val = build_proc_header(&env, &mut layout_ids, symbol, &layout, &proc);

            if proc.args.is_empty() {
                // this is a 0-argument thunk, i.e. a top-level constant definition
                // it must be in-scope everywhere in the module!
                scope.insert_top_level_thunk(symbol, layout, fn_val);
            }

            headers.push((proc, fn_val));
        }

        // Build each proc using its header info.
        for (proc, fn_val) in headers {
            let mut current_scope = scope.clone();

            // only have top-level thunks for this proc's module in scope
            // this retain is not needed for correctness, but will cause less confusion when debugging
            let home = proc.name.module_id();
            current_scope.retain_top_level_thunks_for_module(home);

            build_proc(&env, &mut layout_ids, scope.clone(), proc, fn_val);

            if fn_val.verify(true) {
                function_pass.run_on(&fn_val);
            } else {
                use roc_builtins::std::Mode;

                let mode = match stdlib_mode {
                    Mode::Uniqueness => "OPTIMIZED",
                    Mode::Standard => "NON-OPTIMIZED",
                };

                eprintln!(
                    "\n\nFunction {:?} failed LLVM verification in {} build. Its content was:\n",
                    fn_val.get_name().to_str().unwrap(),
                    mode,
                );

                fn_val.print_to_stderr();

                panic!(
                    "The preceding code was from {:?}, which failed LLVM verification in {} build.",
                    fn_val.get_name().to_str().unwrap(),
                    mode,
                );
            }
        }

        let (main_fn_name, main_fn) = roc_gen::llvm::build::promote_to_main_function(
            &env,
            &mut layout_ids,
            main_fn_symbol,
            &main_fn_layout,
        );

        // Uncomment this to see the module's un-optimized LLVM instruction output:
        // env.module.print_to_stderr();

        if main_fn.verify(true) {
            function_pass.run_on(&main_fn);
        } else {
            panic!("Main function {} failed LLVM verification in build. Uncomment things nearby to see more details.", main_fn_name);
        }

        module_pass.run_on(env.module);

        // Verify the module
        if let Err(errors) = env.module.verify() {
            panic!("Errors defining module: {:?}", errors);
        }

        // Uncomment this to see the module's optimized LLVM instruction output:
        // env.module.print_to_stderr();

        let lib = module_to_dylib(&env.module, &target, opt_level)
            .expect("Error loading compiled dylib for test");
        let answer = unsafe {
            eval::jit_to_ast(
                &arena,
                lib,
                main_fn_name,
                &main_fn_layout,
                &content,
                &env.interns,
                home,
                &subs,
                ptr_bytes,
            )
        };
        let mut expr = bumpalo::collections::String::new_in(&arena);

        answer.format_with_options(&mut expr, Parens::NotNeeded, Newlines::Yes, 0);

        Ok(ReplOutput::NoProblems {
            expr: expr.into_bump_str().to_string(),
            expr_type: expr_type_str,
        })
    }
}

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from("app Repl provides [ replOutput ] imports []\n\nreplOutput =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}
