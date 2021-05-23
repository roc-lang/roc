use crate::repl::eval;
use bumpalo::Bump;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::AddressSpace;
use roc_build::link::module_to_dylib;
use roc_build::program::FunctionIterator;
use roc_can::builtins::builtin_defs_map;
use roc_collections::all::{MutMap, MutSet};
use roc_fmt::annotation::Formattable;
use roc_fmt::annotation::{Newlines, Parens};
use roc_gen::llvm::build::{build_proc, build_proc_header, set_name, OptLevel, C_CALL_CONV};
use roc_gen::llvm::convert::ptr_int;
use roc_load::file::LoadingProblem;
use roc_parse::parser::SyntaxError;
use roc_types::pretty_print::{content_to_string, name_all_type_vars};
use std::path::{Path, PathBuf};
use std::str::from_utf8_unchecked;
use target_lexicon::Triple;

pub enum ReplOutput {
    Problems(Vec<String>),
    NoProblems { expr: String, expr_type: String },
}

pub fn gen_and_eval<'a>(
    src: &[u8],
    target: Triple,
    opt_level: OptLevel,
) -> Result<ReplOutput, SyntaxError<'a>> {
    use roc_reporting::report::{
        can_problem, mono_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE,
    };

    let arena = Bump::new();

    // SAFETY: we've already verified that this is valid UTF-8 during parsing.
    let src_str: &str = unsafe { from_utf8_unchecked(src) };

    let stdlib = roc_builtins::std::standard_stdlib();
    let filename = PathBuf::from("REPL.roc");
    let src_dir = Path::new("fake/test/path");

    let module_src = promote_expr_to_module(src_str);

    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;

    let exposed_types = MutMap::default();
    let loaded = roc_load::file::load_and_monomorphize_from_str(
        &arena,
        filename,
        &module_src,
        &stdlib,
        src_dir,
        exposed_types,
        ptr_bytes,
        builtin_defs_map,
    );

    let mut loaded = match loaded {
        Ok(v) => v,
        Err(LoadingProblem::FormattedReport(report)) => {
            return Ok(ReplOutput::Problems(vec![report]));
        }
        Err(e) => {
            panic!("error while loading module: {:?}", e)
        }
    };

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

        let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;

        let module = arena.alloc(roc_gen::llvm::build::module_from_builtins(&context, ""));

        // mark our zig-defined builtins as internal
        for function in FunctionIterator::from_module(module) {
            let name = function.get_name().to_str().unwrap();
            if name.starts_with("roc_builtins") {
                function.set_linkage(Linkage::Internal);
            }
        }

        // Add roc_alloc, roc_realloc, and roc_dealloc, since the repl has no
        // platform to provide them.
        let builder = context.create_builder();

        add_default_roc_externs(&context, module, &builder, ptr_bytes);

        debug_assert_eq!(exposed_to_host.len(), 1);
        let (main_fn_symbol, main_fn_var) = exposed_to_host.iter().next().unwrap();
        let main_fn_symbol = *main_fn_symbol;
        let main_fn_var = *main_fn_var;

        // pretty-print the expr type string for later.
        name_all_type_vars(main_fn_var, &mut subs);
        let content = subs.get(main_fn_var).content;
        let expr_type_str = content_to_string(content.clone(), &subs, home, &interns);

        let (_, main_fn_layout) = match procedures.keys().find(|(s, _)| *s == main_fn_symbol) {
            Some(layout) => *layout,
            None => {
                return Ok(ReplOutput::NoProblems {
                    expr: "<function>".to_string(),
                    expr_type: expr_type_str,
                });
            }
        };

        let module = arena.alloc(module);
        let (module_pass, function_pass) =
            roc_gen::llvm::build::construct_optimization_passes(module, opt_level);

        let (dibuilder, compile_unit) = roc_gen::llvm::build::Env::new_debug_info(module);

        // Compile and add all the Procs before adding main
        let env = roc_gen::llvm::build::Env {
            arena: &arena,
            builder: &builder,
            dibuilder: &dibuilder,
            compile_unit: &compile_unit,
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

            // call finalize() before any code generation/verification
            env.dibuilder.finalize();

            if fn_val.verify(true) {
                function_pass.run_on(&fn_val);
            } else {
                let mode = "NON-OPTIMIZED";

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

        env.dibuilder.finalize();

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
        let res_answer = unsafe {
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

        use eval::ToAstProblem::*;
        match res_answer {
            Ok(answer) => {
                answer.format_with_options(&mut expr, Parens::NotNeeded, Newlines::Yes, 0);
            }
            Err(FunctionLayout) => {
                expr.push_str("<function>");
            }
        }

        Ok(ReplOutput::NoProblems {
            expr: expr.into_bump_str().to_string(),
            expr_type: expr_type_str,
        })
    }
}

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer =
        String::from("app \"app\" provides [ replOutput ] to \"./platform\"\n\nreplOutput =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}

/// Define functions for roc_alloc, roc_realloc, and roc_dealloc
/// which use libc implementations (malloc, realloc, and free)
fn add_default_roc_externs<'ctx>(
    ctx: &'ctx Context,
    module: &Module<'ctx>,
    builder: &Builder<'ctx>,
    ptr_bytes: u32,
) {
    let usize_type = ptr_int(ctx, ptr_bytes);
    let i32_type = ctx.i32_type();
    let i8_ptr_type = ctx.i8_type().ptr_type(AddressSpace::Generic);

    // roc_alloc
    {
        let fn_val = module.add_function(
            "roc_alloc",
            i8_ptr_type.fn_type(
                &[
                    // alignment: u32
                    i32_type.into(),
                    // size: usize
                    usize_type.into(),
                ],
                false,
            ),
            Some(Linkage::External),
        );

        fn_val.set_call_conventions(C_CALL_CONV);

        let mut params = fn_val.get_param_iter();
        let alignment_arg = params.next().unwrap();
        let size_arg = params.next().unwrap();

        debug_assert!(params.next().is_none());

        set_name(alignment_arg, "alignment");
        set_name(size_arg, "size");

        // Add a basic block for the entry point
        let entry = ctx.append_basic_block(fn_val, "entry");

        builder.position_at_end(entry);

        // Call libc malloc()
        let retval = builder
            .build_array_malloc(ctx.i8_type(), size_arg.into_int_value(), "call_malloc")
            .unwrap();

        builder.build_return(Some(&retval));

        if cfg!(debug_assertions) {
            roc_gen::llvm::build::verify_fn(fn_val);
        }
    }

    // roc_realloc
    {
        let libc_realloc_val = {
            let fn_val = module.add_function(
                "realloc",
                i8_ptr_type.fn_type(
                    &[
                        // ptr: *void
                        i8_ptr_type.into(),
                        // size: usize
                        usize_type.into(),
                    ],
                    false,
                ),
                Some(Linkage::External),
            );

            fn_val.set_call_conventions(C_CALL_CONV);

            let mut params = fn_val.get_param_iter();
            let ptr_arg = params.next().unwrap();
            let size_arg = params.next().unwrap();

            debug_assert!(params.next().is_none());

            set_name(ptr_arg, "ptr");
            set_name(size_arg, "size");

            if cfg!(debug_assertions) {
                roc_gen::llvm::build::verify_fn(fn_val);
            }

            fn_val
        };

        let fn_val = module.add_function(
            "roc_realloc",
            i8_ptr_type.fn_type(
                &[
                    // alignment: u32
                    i32_type.into(),
                    // ptr: *void
                    i8_ptr_type.into(),
                    // old_size: usize
                    usize_type.into(),
                    // new_size: usize
                    usize_type.into(),
                ],
                false,
            ),
            Some(Linkage::External),
        );

        fn_val.set_call_conventions(C_CALL_CONV);

        let mut params = fn_val.get_param_iter();
        let alignment_arg = params.next().unwrap();
        let ptr_arg = params.next().unwrap();
        let old_size_arg = params.next().unwrap();
        let new_size_arg = params.next().unwrap();

        debug_assert!(params.next().is_none());

        set_name(alignment_arg, "alignment");
        set_name(ptr_arg, "ptr");
        set_name(old_size_arg, "old_size");
        set_name(new_size_arg, "new_size");

        // Add a basic block for the entry point
        let entry = ctx.append_basic_block(fn_val, "entry");

        builder.position_at_end(entry);

        // Call libc realloc()
        let call = builder.build_call(
            libc_realloc_val,
            &[ptr_arg, new_size_arg],
            "call_libc_realloc",
        );

        call.set_call_convention(C_CALL_CONV);

        let retval = call.try_as_basic_value().left().unwrap();

        builder.build_return(Some(&retval));

        if cfg!(debug_assertions) {
            roc_gen::llvm::build::verify_fn(fn_val);
        }
    }

    // roc_dealloc
    {
        let fn_val = module.add_function(
            "roc_dealloc",
            ctx.void_type().fn_type(
                &[
                    // alignment: u32
                    i32_type.into(),
                    // ptr: *void
                    i8_ptr_type.into(),
                ],
                false,
            ),
            Some(Linkage::External),
        );

        fn_val.set_call_conventions(C_CALL_CONV);

        let mut params = fn_val.get_param_iter();
        let alignment_arg = params.next().unwrap();
        let ptr_arg = params.next().unwrap();

        debug_assert!(params.next().is_none());

        set_name(alignment_arg, "alignment");
        set_name(ptr_arg, "ptr");

        // Call libc free()
        builder.build_free(ptr_arg.into_pointer_value());

        builder.build_return(None);

        if cfg!(debug_assertions) {
            roc_gen::llvm::build::verify_fn(fn_val);
        }
    }
}
