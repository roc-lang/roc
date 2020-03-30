extern crate roc_gen;

use crate::helpers::{infer_expr, uniq_expr_with};
use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::passes::PassManager;
use inkwell::types::BasicType;
use inkwell::OptimizationLevel;
use roc_collections::all::ImMap;
use roc_gen::llvm::build::{build_proc, build_proc_header, get_call_conventions};
use roc_gen::llvm::convert::basic_type_from_layout;
use roc_mono::expr::{Expr, Procs};
use roc_mono::layout::Layout;
use std::time::SystemTime;

use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple,
};
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::Path;
use target_lexicon::Triple;

pub mod helpers;

fn main() -> io::Result<()> {
    let now = SystemTime::now();
    let argv = std::env::args().into_iter().collect::<Vec<String>>();

    match argv.get(1) {
        Some(filename) => {
            let mut file = File::open(filename)?;
            let mut contents = String::new();

            file.read_to_string(&mut contents)?;

            let dest_filename = Path::new(filename).with_extension("o");

            gen(contents.as_str(), Triple::host(), &dest_filename);

            let end_time = now.elapsed().unwrap();

            println!("Finished in {} ms\n", end_time.as_millis());

            Ok(())
        }
        None => {
            println!("Usage: roc FILENAME.roc");

            Ok(())
        }
    }
}

fn gen(src: &str, target: Triple, dest_filename: &Path) {
    // Build the expr
    let arena = Bump::new();

    let (loc_expr, _output, _problems, subs, var, constraint, home, interns) =
        uniq_expr_with(&arena, src, &ImMap::default());

    let mut unify_problems = Vec::new();
    let (content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

    let context = Context::create();
    let module = context.create_module("app");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);

    roc_gen::llvm::build::add_passes(&fpm);

    fpm.initialize();

    // Compute main_fn_type before moving subs to Env
    let pointer_bytes = target.pointer_width().unwrap().bytes() as u32;
    let layout = Layout::from_content(&arena, content, &subs, pointer_bytes)
    .unwrap_or_else(|err| panic!("Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}", err, subs));

    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .expect("Error creating JIT execution engine for test");

    let ptr_bytes = execution_engine
        .get_target_data()
        .get_pointer_byte_size(None);
    let main_fn_type =
        basic_type_from_layout(&arena, &context, &layout, ptr_bytes).fn_type(&[], false);
    let main_fn_name = "$Test.main";

    // Compile and add all the Procs before adding main
    let mut env = roc_gen::llvm::build::Env {
        arena: &arena,
        builder: &builder,
        context: &context,
        interns,
        module: arena.alloc(module),
        ptr_bytes,
    };
    let mut procs = Procs::default();
    let mut ident_ids = env.interns.all_ident_ids.remove(&home).unwrap();

    // Populate Procs and get the low-level Expr from the canonical Expr
    let main_body = Expr::new(
        &arena,
        &mut subs,
        loc_expr.value,
        &mut procs,
        home,
        &mut ident_ids,
        pointer_bytes,
    );

    // Put this module's ident_ids back in the interns, so we can use them in env.
    env.interns.all_ident_ids.insert(home, ident_ids);

    let mut headers = Vec::with_capacity(procs.len());

    // Add all the Proc headers to the module.
    // We have to do this in a separate pass first,
    // because their bodies may reference each other.
    for (symbol, opt_proc) in procs.as_map().into_iter() {
        if let Some(proc) = opt_proc {
            let (fn_val, arg_basic_types) = build_proc_header(&env, symbol, &proc);

            headers.push((proc, fn_val, arg_basic_types));
        }
    }

    // Build each proc using its header info.
    for (proc, fn_val, arg_basic_types) in headers {
        // NOTE: This is here to be uncommented in case verification fails.
        // (This approach means we don't have to defensively clone name here.)
        //
        // println!("\n\nBuilding and then verifying function {}\n\n", name);
        build_proc(&env, proc, &procs, fn_val, arg_basic_types);

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
        &ImMap::default(),
        main_fn,
        &main_body,
        &mut Procs::default(),
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

    // Emit
    Target::initialize_x86(&InitializationConfig::default());

    let opt = OptimizationLevel::Default;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target = Target::from_name("x86-64").unwrap();
    let target_machine = target
        .create_target_machine(
            &TargetTriple::create("x86_64-pc-linux-gnu"),
            "x86-64",
            "+avx2",
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
