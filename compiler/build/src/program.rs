use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::passes::PassManager;
use inkwell::types::BasicType;
use inkwell::OptimizationLevel;
use roc_collections::all::ImMap;
use roc_gen::layout_id::LayoutIds;
use roc_gen::llvm::build::{
    build_proc, build_proc_header, get_call_conventions, module_from_builtins, OptLevel,
};
use roc_gen::llvm::convert::basic_type_from_layout;
use roc_load::file::LoadedModule;
use roc_module::symbol::Symbol;
use roc_mono::expr::{Env, Expr, PartialProc, Procs};
use roc_mono::layout::{Layout, LayoutCache};

use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use std::path::{Path, PathBuf};
use target_lexicon::{Architecture, OperatingSystem, Triple, Vendor};

// TODO how should imported modules factor into this? What if those use builtins too?
// TODO this should probably use more helper functions
// TODO make this polymorphic in the llvm functions so it can be reused for another backend.
#[allow(clippy::cognitive_complexity)]
pub fn build(
    arena: &Bump,
    loaded: LoadedModule,
    filename: PathBuf,
    target: Triple,
    dest_filename: &Path,
    opt_level: OptLevel,
) {
    use roc_reporting::report::{can_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE};

    let src = loaded.src;
    let home = loaded.module_id;
    let src_lines: Vec<&str> = src.split('\n').collect();
    let palette = DEFAULT_PALETTE;

    // Report parsing and canonicalization problems
    let alloc = RocDocAllocator::new(&src_lines, home, &loaded.interns);

    for problem in loaded.can_problems.into_iter() {
        let report = can_problem(&alloc, filename.clone(), problem);
        let mut buf = String::new();

        report.render_color_terminal(&mut buf, &alloc, &palette);

        println!("\n{}\n", buf);
    }

    for problem in loaded.type_problems.into_iter() {
        let report = type_problem(&alloc, filename.clone(), problem);
        let mut buf = String::new();

        report.render_color_terminal(&mut buf, &alloc, &palette);

        println!("\n{}\n", buf);
    }

    // Look up the types and expressions of the `provided` values

    // TODO instead of hardcoding this to `main`, use the `provided` list and gen all of them.
    let ident_ids = loaded.interns.all_ident_ids.get(&home).unwrap();
    let main_ident_id = *ident_ids.get_id(&"main".into()).unwrap_or_else(|| {
        todo!("TODO gracefully handle the case where `main` wasn't declared in the app")
    });
    let main_symbol = Symbol::new(home, main_ident_id);
    let mut main_var = None;
    let mut main_expr = None;

    for (symbol, var) in loaded.exposed_vars_by_symbol {
        if symbol == main_symbol {
            main_var = Some(var);

            break;
        }
    }

    let mut decls_by_id = loaded.declarations_by_id;
    let home_decls = decls_by_id
        .remove(&loaded.module_id)
        .expect("Root module ID not found in loaded declarations_by_id");

    // We use a loop label here so we can break all the way out of a nested
    // loop inside DeclareRec if we find the expr there.
    //
    // https://doc.rust-lang.org/1.30.0/book/first-edition/loops.html#loop-labels
    'find_expr: for decl in home_decls.iter() {
        use roc_can::def::Declaration::*;

        match decl {
            Declare(def) => {
                if def.pattern_vars.contains_key(&main_symbol) {
                    main_expr = Some(def.loc_expr.clone());

                    break 'find_expr;
                }
            }

            DeclareRec(defs) => {
                for def in defs {
                    if def.pattern_vars.contains_key(&main_symbol) {
                        main_expr = Some(def.loc_expr.clone());

                        break 'find_expr;
                    }
                }
            }
            InvalidCycle(_, _) | Builtin(_) => {
                // These can never contain main.
            }
        }
    }

    let loc_expr = main_expr.unwrap_or_else(|| {
        panic!("TODO gracefully handle the case where `main` was declared but not exposed")
    });
    let mut subs = loaded.solved.into_inner();
    let content = match main_var {
        Some(var) => subs.get_without_compacting(var).content,
        None => todo!("TODO gracefully handle the case where `main` was declared but not exposed"),
    };

    // Generate the binary

    let context = Context::create();
    let module = module_from_builtins(&context, "app");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);

    roc_gen::llvm::build::add_passes(&fpm, opt_level);

    fpm.initialize();

    // Compute main_fn_type before moving subs to Env
    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;
    let layout = Layout::new(&arena, content, &subs, ptr_bytes).unwrap_or_else(|err| {
        panic!(
            "Code gen error in Program: could not convert to layout. Err was {:?}",
            err
        )
    });

    let main_fn_type =
        basic_type_from_layout(&arena, &context, &layout, ptr_bytes).fn_type(&[], false);
    let main_fn_name = "$main";

    // Compile and add all the Procs before adding main
    let mut env = roc_gen::llvm::build::Env {
        arena: &arena,
        builder: &builder,
        context: &context,
        interns: loaded.interns,
        module: arena.alloc(module),
        ptr_bytes,
    };
    let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();
    let mut layout_ids = LayoutIds::default();
    let mut procs = Procs::default();
    let mut mono_problems = std::vec::Vec::new();
    let mut layout_cache = LayoutCache::default();
    let mut mono_env = Env {
        arena,
        subs: &mut subs,
        problems: &mut mono_problems,
        home,
        ident_ids: &mut ident_ids,
        pointer_size: ptr_bytes,
        jump_counter: arena.alloc(0),
    };

    // Add modules' decls to Procs
    for (_, mut decls) in decls_by_id
        .drain()
        .chain(std::iter::once((loaded.module_id, home_decls)))
    {
        for decl in decls.drain(..) {
            use roc_can::def::Declaration::*;
            use roc_can::expr::Expr::*;
            use roc_can::pattern::Pattern::*;

            match decl {
                Declare(def) | Builtin(def) => match def.loc_pattern.value {
                    Identifier(symbol) => {
                        match def.loc_expr.value {
                            Closure(annotation, _, _, loc_args, boxed_body) => {
                                let (loc_body, ret_var) = *boxed_body;

                                procs.insert_named(
                                    &mut mono_env,
                                    &mut layout_cache,
                                    symbol,
                                    annotation,
                                    loc_args,
                                    loc_body,
                                    ret_var,
                                );
                            }
                            body => {
                                let proc = PartialProc {
                                    annotation: def.expr_var,
                                    // This is a 0-arity thunk, so it has no arguments.
                                    pattern_symbols: bumpalo::collections::Vec::new_in(arena),
                                    body,
                                };

                                procs.partial_procs.insert(symbol, proc);
                                procs.module_thunks.insert(symbol);
                            }
                        };
                    }
                    other => {
                        todo!("TODO gracefully handle Declare({:?})", other);
                    }
                },
                DeclareRec(_defs) => {
                    todo!("TODO support DeclareRec");
                }
                InvalidCycle(_loc_idents, _regions) => {
                    todo!("TODO handle InvalidCycle");
                }
            }
        }
    }

    // Populate Procs further and get the low-level Expr from the canonical Expr
    let main_body = Expr::new(&mut mono_env, loc_expr.value, &mut procs);
    let mut headers = {
        let num_headers = match &procs.pending_specializations {
            Some(map) => map.len(),
            None => 0,
        };

        Vec::with_capacity(num_headers)
    };
    let mut procs = roc_mono::expr::specialize_all(&mut mono_env, procs, &mut layout_cache);

    assert_eq!(
        procs.runtime_errors,
        roc_collections::all::MutMap::default()
    );

    // Put this module's ident_ids back in the interns, so we can use them in env.
    // This must happen *after* building the headers, because otherwise there's
    // a conflicting mutable borrow on ident_ids.
    env.interns.all_ident_ids.insert(home, ident_ids);

    // Add all the Proc headers to the module.
    // We have to do this in a separate pass first,
    // because their bodies may reference each other.
    for ((symbol, layout), proc) in procs.specialized.drain() {
        use roc_mono::expr::InProgressProc::*;

        match proc {
            InProgress => {
                panic!("A specialization was still marked InProgress after monomorphization had completed: {:?} with layout {:?}", symbol, layout);
            }
            Done(proc) => {
                let (fn_val, arg_basic_types) =
                    build_proc_header(&env, &mut layout_ids, symbol, &layout, &proc);

                headers.push((proc, fn_val, arg_basic_types));
            }
        }
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
    let cc = get_call_conventions(target.default_calling_convention().unwrap());
    let main_fn = env.module.add_function(main_fn_name, main_fn_type, None);

    main_fn.set_call_conventions(cc);
    main_fn.set_linkage(Linkage::External);

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
        panic!("😱 LLVM errors when defining module: {:?}", errors);
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    // Emit the .o file

    // NOTE: arch_str is *not* the same as the beginning of the magic target triple
    // string! For example, if it's "x86-64" here, the magic target triple string
    // will begin with "x86_64" (with an underscore) instead.
    let arch_str = match target.architecture {
        Architecture::X86_64 => {
            Target::initialize_x86(&InitializationConfig::default());

            "x86-64"
        }
        Architecture::Arm(_) if cfg!(feature = "target-arm") => {
            // NOTE: why not enable arm and wasm by default?
            //
            // We had some trouble getting them to link properly. This may be resolved in the
            // future, or maybe it was just some weird configuration on one machine.
            Target::initialize_arm(&InitializationConfig::default());

            "arm"
        }
        Architecture::Wasm32 if cfg!(feature = "target-webassembly") => {
            Target::initialize_webassembly(&InitializationConfig::default());

            "wasm32"
        }
        _ => panic!(
            "TODO gracefully handle unsupported target architecture: {:?}",
            target.architecture
        ),
    };

    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;

    // Best guide I've found on how to determine these magic strings:
    //
    // https://stackoverflow.com/questions/15036909/clang-how-to-list-supported-target-architectures
    let target_triple_str = match target {
        Triple {
            architecture: Architecture::X86_64,
            vendor: Vendor::Unknown,
            operating_system: OperatingSystem::Linux,
            ..
        } => "x86_64-unknown-linux-gnu",
        Triple {
            architecture: Architecture::X86_64,
            vendor: Vendor::Pc,
            operating_system: OperatingSystem::Linux,
            ..
        } => "x86_64-pc-linux-gnu",
        Triple {
            architecture: Architecture::X86_64,
            vendor: Vendor::Unknown,
            operating_system: OperatingSystem::Darwin,
            ..
        } => "x86_64-unknown-darwin10",
        Triple {
            architecture: Architecture::X86_64,
            vendor: Vendor::Apple,
            operating_system: OperatingSystem::Darwin,
            ..
        } => "x86_64-apple-darwin10",
        _ => panic!("TODO gracefully handle unsupported target: {:?}", target),
    };
    let target_machine = Target::from_name(arch_str)
        .unwrap()
        .create_target_machine(
            &TargetTriple::create(target_triple_str),
            arch_str,
            "+avx2", // TODO this string was used uncritically from an example, and should be reexamined
            opt,
            reloc,
            model,
        )
        .unwrap();

    target_machine
        .write_to_file(&env.module, FileType::Object, &dest_filename)
        .expect("Writing .o file failed");

    println!("\nSuccess! 🎉\n\n\t➡ {}\n", dest_filename.display());
}
