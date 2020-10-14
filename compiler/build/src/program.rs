use crate::target;
use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::targets::{CodeModel, FileType, RelocMode};
use inkwell::OptimizationLevel;
use roc_gen::layout_id::LayoutIds;
use roc_gen::llvm::build::{build_proc, build_proc_header, module_from_builtins, OptLevel, Scope};
use roc_load::file::MonomorphizedModule;
use std::path::{Path, PathBuf};
use target_lexicon::Triple;

// TODO how should imported modules factor into this? What if those use builtins too?
// TODO this should probably use more helper functions
// TODO make this polymorphic in the llvm functions so it can be reused for another backend.
#[allow(clippy::cognitive_complexity)]
pub fn gen_from_mono_module(
    arena: &Bump,
    loaded: MonomorphizedModule,
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

    // Generate the binary

    let context = Context::create();
    let module = arena.alloc(module_from_builtins(&context, "app"));
    let builder = context.create_builder();
    let (mpm, fpm) = roc_gen::llvm::build::construct_optimization_passes(module, opt_level);

    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;

    // Compile and add all the Procs before adding main
    let env = roc_gen::llvm::build::Env {
        arena: &arena,
        builder: &builder,
        context: &context,
        interns: loaded.interns,
        module,
        ptr_bytes,
        leak: false,
        exposed_to_host: loaded.exposed_to_host,
    };

    // Populate Procs further and get the low-level Expr from the canonical Expr
    let mut headers = Vec::with_capacity(loaded.procedures.len());

    // Add all the Proc headers to the module.
    // We have to do this in a separate pass first,
    // because their bodies may reference each other.
    let mut layout_ids = LayoutIds::default();

    let mut scope = Scope::default();
    for ((symbol, layout), proc) in loaded.procedures {
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
        // NOTE: This is here to be uncommented in case verification fails.
        // (This approach means we don't have to defensively clone name here.)
        //
        // println!("\n\nBuilding and then verifying function {:?}\n\n", proc);
        build_proc(&env, &mut layout_ids, scope.clone(), proc, fn_val);

        if fn_val.verify(true) {
            fpm.run_on(&fn_val);
        } else {
            // fn_val.print_to_stderr();
            // env.module.print_to_stderr();
            // NOTE: If this fails, uncomment the above println to debug.
            panic!(
                "Non-main function failed LLVM verification. Uncomment the above println to debug!"
            );
        }
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    mpm.run_on(module);

    // Verify the module
    if let Err(errors) = env.module.verify() {
        panic!("😱 LLVM errors when defining module: {:?}", errors);
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    // Emit the .o file

    let opt = OptimizationLevel::Aggressive;
    let reloc = RelocMode::Default;
    let model = CodeModel::Default;
    let target_machine = target::target_machine(&target, opt, reloc, model).unwrap();

    target_machine
        .write_to_file(&env.module, FileType::Object, &dest_filename)
        .expect("Writing .o file failed");

    println!("\nSuccess! 🎉\n\n\t➡ {}\n", dest_filename.display());
}
