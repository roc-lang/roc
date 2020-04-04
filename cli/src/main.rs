extern crate roc_gen;
extern crate roc_reporting;

use crate::helpers::{infer_expr, uniq_expr_with};
use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::passes::PassManager;
use inkwell::types::BasicType;
use roc_collections::all::ImMap;
use roc_gen::llvm::build::{
    build_proc, build_proc_header, get_call_conventions, module_from_builtins,
};
use roc_gen::llvm::convert::basic_type_from_layout;
use roc_gen::llvm::target::init_target_machine;
use roc_mono::expr::{Expr, Procs};
use roc_mono::layout::Layout;
use std::time::SystemTime;

use inkwell::targets::FileType;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use target_lexicon::Triple;

pub mod helpers;

fn main() -> io::Result<()> {
    let now = SystemTime::now();
    let argv = std::env::args().collect::<Vec<String>>();

    match argv.get(1) {
        Some(filename) => {
            let mut file = File::open(filename)?;
            let mut contents = String::new();

            file.read_to_string(&mut contents)?;

            let dest_filename = Path::new(filename).with_extension("o");

            gen(
                Path::new(filename).to_path_buf(),
                contents.as_str(),
                Triple::host(),
                &dest_filename,
            );

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

fn gen(filename: PathBuf, src: &str, target: Triple, dest_filename: &Path) {
    use roc_reporting::report::{can_problem, DEFAULT_PALETTE};
    use roc_reporting::type_error::type_problem;

    // Build the expr
    let arena = Bump::new();

    let (loc_expr, _output, can_problems, subs, var, constraint, home, interns) =
        uniq_expr_with(&arena, src, &ImMap::default());

    let mut type_problems = Vec::new();
    let (content, mut subs) = infer_expr(subs, &mut type_problems, &constraint, var);

    let src_lines: Vec<&str> = src.split('\n').collect();
    let palette = DEFAULT_PALETTE;

    // Report parsing and canonicalization problems
    for problem in can_problems.into_iter() {
        let report = can_problem(filename.clone(), problem);
        let mut buf = String::new();

        report
            .text
            .render_color_terminal(&mut buf, &mut subs, home, &src_lines, &interns, &palette);

        println!("\n{}\n", buf);
    }

    for problem in type_problems.into_iter() {
        let report = type_problem(filename.clone(), problem);
        let mut buf = String::new();

        report
            .text
            .render_color_terminal(&mut buf, &mut subs, home, &src_lines, &interns, &palette);

        println!("\n{}\n", buf);
    }

    // Generate the binary

    let context = Context::create();
    let module = module_from_builtins(&context, "app");
    let builder = context.create_builder();
    let fpm = PassManager::create(&module);

    roc_gen::llvm::build::add_passes(&fpm);

    fpm.initialize();

    // Compute main_fn_type before moving subs to Env
    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;
    let layout = Layout::from_content(&arena, content, &subs, ptr_bytes).unwrap_or_else(|err| {
        panic!(
            "Code gen error in test: could not convert to layout. Err was {:?} and Subs were {:?}",
            err, subs
        )
    });

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
        ptr_bytes,
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
        &Procs::default(),
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
    init_target_machine(target)
        .unwrap()
        .write_to_file(&env.module, FileType::Object, &dest_filename)
        .expect("Writing .o file failed");

    println!("\nSuccess! 🎉\n\n\t➡ {}\n", dest_filename.display());
}
