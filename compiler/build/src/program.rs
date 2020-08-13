use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use inkwell::OptimizationLevel;
use roc_collections::all::default_hasher;
use roc_gen::layout_id::LayoutIds;
use roc_gen::llvm::build::{build_proc, build_proc_header, module_from_builtins, OptLevel};
use roc_load::file::LoadedModule;
use roc_mono::ir::{Env, PartialProc, Procs};
use roc_mono::layout::{Layout, LayoutCache};
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use target_lexicon::{Architecture, OperatingSystem, Triple, Vendor};

// TODO how should imported modules factor into this? What if those use builtins too?
// TODO this should probably use more helper functions
// TODO make this polymorphic in the llvm functions so it can be reused for another backend.
#[allow(clippy::cognitive_complexity)]
pub fn gen(
    arena: &Bump,
    mut loaded: LoadedModule,
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
    let mut decls_by_id = loaded.declarations_by_id;
    let home_decls = decls_by_id
        .remove(&loaded.module_id)
        .expect("Root module ID not found in loaded declarations_by_id");

    let mut subs = loaded.solved.into_inner();

    // Generate the binary

    let context = Context::create();
    let module = arena.alloc(module_from_builtins(&context, "app"));
    let builder = context.create_builder();
    let (mpm, fpm) = roc_gen::llvm::build::construct_optimization_passes(module, opt_level);

    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;

    let mut exposed_to_host =
        HashSet::with_capacity_and_hasher(loaded.exposed_vars_by_symbol.len(), default_hasher());

    for (symbol, _) in loaded.exposed_vars_by_symbol {
        exposed_to_host.insert(symbol);
    }

    let mut ident_ids = loaded.interns.all_ident_ids.remove(&home).unwrap();
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
                            Closure(annotation, _, recursivity, loc_args, boxed_body) => {
                                let is_tail_recursive =
                                    matches!(recursivity, roc_can::expr::Recursive::TailRecursive);

                                let (loc_body, ret_var) = *boxed_body;

                                // If this is an exposed symbol, we need to
                                // register it as such. Otherwise, since it
                                // never gets called by Roc code, it will never
                                // get specialized!
                                if exposed_to_host.contains(&symbol) {
                                    let mut pattern_vars =
                                        bumpalo::collections::Vec::with_capacity_in(
                                            loc_args.len(),
                                            arena,
                                        );

                                    for (var, _) in loc_args.iter() {
                                        pattern_vars.push(*var);
                                    }

                                    let layout = layout_cache.from_var(mono_env.arena, annotation, mono_env.subs).unwrap_or_else(|err|
                                        todo!("TODO gracefully handle the situation where we expose a function to the host which doesn't have a valid layout (e.g. maybe the function wasn't monomorphic): {:?}", err)
                                    );

                                    procs.insert_exposed(
                                        symbol,
                                        layout,
                                        pattern_vars.into_bump_slice(),
                                        annotation,
                                        ret_var,
                                    );
                                }

                                procs.insert_named(
                                    &mut mono_env,
                                    &mut layout_cache,
                                    symbol,
                                    annotation,
                                    loc_args,
                                    loc_body,
                                    is_tail_recursive,
                                    ret_var,
                                );
                            }
                            body => {
                                let annotation = def.expr_var;
                                let proc = PartialProc {
                                    annotation,
                                    // This is a 0-arity thunk, so it has no arguments.
                                    pattern_symbols: bumpalo::collections::Vec::new_in(
                                        mono_env.arena,
                                    ),
                                    is_tail_recursive: false,
                                    body,
                                };

                                // If this is an exposed symbol, we need to
                                // register it as such. Otherwise, since it
                                // never gets called by Roc code, it will never
                                // get specialized!
                                if exposed_to_host.contains(&symbol) {
                                    let pattern_vars = bumpalo::collections::Vec::new_in(arena);
                                    let ret_layout = layout_cache.from_var(mono_env.arena, annotation, mono_env.subs).unwrap_or_else(|err|
                                        todo!("TODO gracefully handle the situation where we expose a function to the host which doesn't have a valid layout (e.g. maybe the function wasn't monomorphic): {:?}", err)
                                    );
                                    let layout =
                                        Layout::FunctionPointer(&[], arena.alloc(ret_layout));

                                    procs.insert_exposed(
                                        symbol,
                                        layout,
                                        pattern_vars.into_bump_slice(),
                                        // It seems brittle that we're passing
                                        // annotation twice - especially since
                                        // in both cases we're giving the
                                        // annotation to the top-level value,
                                        // not the thunk function it will code
                                        // gen to. It seems to work, but that
                                        // may only be because at present we
                                        // only use the function annotation
                                        // variable during specialization, and
                                        // exposed values are never specialized
                                        // because they must be monomorphic.
                                        annotation,
                                        annotation,
                                    );
                                }

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

    // Compile and add all the Procs before adding main
    let mut env = roc_gen::llvm::build::Env {
        arena: &arena,
        builder: &builder,
        context: &context,
        interns: loaded.interns,
        module,
        ptr_bytes,
        leak: false,
        exposed_to_host,
    };

    // Populate Procs further and get the low-level Expr from the canonical Expr
    let mut headers = {
        let num_headers = match &procs.pending_specializations {
            Some(map) => map.len(),
            None => 0,
        };

        Vec::with_capacity(num_headers)
    };
    let procs = roc_mono::ir::specialize_all(&mut mono_env, procs, &mut layout_cache);

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
    for ((symbol, layout), proc) in procs.get_specialized_procs(arena) {
        let (fn_val, arg_basic_types) =
            build_proc_header(&env, &mut layout_ids, symbol, &layout, &proc);

        headers.push((proc, fn_val, arg_basic_types));
    }

    // Build each proc using its header info.
    for (proc, fn_val, arg_basic_types) in headers {
        // NOTE: This is here to be uncommented in case verification fails.
        // (This approach means we don't have to defensively clone name here.)
        //
        // println!("\n\nBuilding and then verifying function {:?}\n\n", proc);
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

    mpm.run_on(module);

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

    let opt = OptimizationLevel::Aggressive;
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
