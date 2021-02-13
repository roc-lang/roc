use crate::target;
use bumpalo::Bump;
use inkwell::context::Context;
use inkwell::targets::{CodeModel, FileType, RelocMode};
use inkwell::values::FunctionValue;
use roc_gen::llvm::build::{build_proc, build_proc_header, module_from_builtins, OptLevel, Scope};
use roc_load::file::MonomorphizedModule;
use roc_mono::layout::LayoutIds;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};
use target_lexicon::Triple;

#[derive(Debug, Clone, Copy, Default)]
pub struct CodeGenTiming {
    pub code_gen: Duration,
    pub emit_o_file: Duration,
}

// TODO how should imported modules factor into this? What if those use builtins too?
// TODO this should probably use more helper functions
// TODO make this polymorphic in the llvm functions so it can be reused for another backend.
#[allow(clippy::cognitive_complexity)]
pub fn gen_from_mono_module(
    arena: &Bump,
    mut loaded: MonomorphizedModule,
    _file_path: PathBuf,
    target: Triple,
    app_o_file: &Path,
    opt_level: OptLevel,
    emit_debug_info: bool,
) -> CodeGenTiming {
    use roc_reporting::report::{
        can_problem, mono_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE,
    };

    let code_gen_start = SystemTime::now();

    for (home, (module_path, src)) in loaded.sources {
        let src_lines: Vec<&str> = src.split('\n').collect();
        let palette = DEFAULT_PALETTE;

        // Report parsing and canonicalization problems
        let alloc = RocDocAllocator::new(&src_lines, home, &loaded.interns);

        let problems = loaded.can_problems.remove(&home).unwrap_or_default();
        for problem in problems.into_iter() {
            let report = can_problem(&alloc, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            println!("\n{}\n", buf);
        }

        let problems = loaded.type_problems.remove(&home).unwrap_or_default();
        for problem in problems {
            let report = type_problem(&alloc, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            println!("\n{}\n", buf);
        }

        let problems = loaded.mono_problems.remove(&home).unwrap_or_default();
        for problem in problems {
            let report = mono_problem(&alloc, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            println!("\n{}\n", buf);
        }
    }

    // Generate the binary
    let context = Context::create();
    let module = arena.alloc(module_from_builtins(&context, "app"));

    // strip Zig debug stuff
    // module.strip_debug_info();

    // mark our zig-defined builtins as internal
    use inkwell::attributes::{Attribute, AttributeLoc};
    use inkwell::module::Linkage;

    let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
    debug_assert!(kind_id > 0);
    let attr = context.create_enum_attribute(kind_id, 1);

    for function in FunctionIterator::from_module(module) {
        let name = function.get_name().to_str().unwrap();
        if name.starts_with("roc_builtins") {
            function.set_linkage(Linkage::Internal);
        }

        if name.starts_with("roc_builtins.dict") {
            function.add_attribute(AttributeLoc::Function, attr);
        }
    }

    let builder = context.create_builder();
    let (dibuilder, compile_unit) = roc_gen::llvm::build::Env::new_debug_info(module);
    let (mpm, fpm) = roc_gen::llvm::build::construct_optimization_passes(module, opt_level);

    // Compile and add all the Procs before adding main
    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;
    let env = roc_gen::llvm::build::Env {
        arena: &arena,
        builder: &builder,
        dibuilder: &dibuilder,
        compile_unit: &compile_unit,
        context: &context,
        interns: loaded.interns,
        module,
        ptr_bytes,
        leak: false,
        exposed_to_host: loaded.exposed_to_host.keys().copied().collect(),
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

        // call finalize() before any code generation/verification
        env.dibuilder.finalize();

        if fn_val.verify(true) {
            fpm.run_on(&fn_val);
        } else {
            fn_val.print_to_stderr();
            // env.module.print_to_stderr();
            // NOTE: If this fails, uncomment the above println to debug.
            panic!(
                r"Non-main function {:?} failed LLVM verification. Uncomment the above println to debug!",
                fn_val.get_name(),
            );
        }
    }

    env.dibuilder.finalize();

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    mpm.run_on(module);

    // Verify the module
    if let Err(errors) = env.module.verify() {
        panic!("😱 LLVM errors when defining module: {:?}", errors);
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    let code_gen = code_gen_start.elapsed().unwrap();
    let emit_o_file_start = SystemTime::now();

    // annotate the LLVM IR output with debug info
    // so errors are reported with the line number of the LLVM source
    if emit_debug_info {
        module.strip_debug_info();

        let mut app_ll_file = std::path::PathBuf::from(app_o_file);
        app_ll_file.set_extension("ll");

        let mut app_ll_dbg_file = std::path::PathBuf::from(app_o_file);
        app_ll_dbg_file.set_extension("dbg.ll");

        let mut app_bc_file = std::path::PathBuf::from(app_o_file);
        app_bc_file.set_extension("bc");

        use std::process::Command;

        // write the ll code to a file, so we can modify it
        module.print_to_file(&app_ll_file).unwrap();

        // run the debugir https://github.com/vaivaswatha/debugir tool
        match Command::new("debugir")
            .env_clear()
            .args(&[app_ll_file.to_str().unwrap()])
            .output()
        {
            Ok(_) => {}
            Err(error) => {
                use std::io::ErrorKind;
                match error.kind() {
                    ErrorKind::NotFound => panic!(
                        r"I could not find the `debugir` tool on the PATH, install it from https://github.com/vaivaswatha/debugir"
                    ),
                    _ => panic!("{:?}", error),
                }
            }
        }

        // assemble the .ll into a .bc
        let _ = Command::new("llvm-as-10")
            .env_clear()
            .args(&[
                app_ll_dbg_file.to_str().unwrap(),
                "-o",
                app_bc_file.to_str().unwrap(),
            ])
            .output()
            .unwrap();

        // write the .o file. Note that this builds the .o for the local machine,
        // and ignores the `target_machine` entirely.
        let _ = Command::new("llc-10")
            .env_clear()
            .args(&[
                "-filetype=obj",
                app_bc_file.to_str().unwrap(),
                "-o",
                app_o_file.to_str().unwrap(),
            ])
            .output()
            .unwrap();
    } else {
        // Emit the .o file

        let reloc = RelocMode::Default;
        let model = CodeModel::Default;
        let target_machine =
            target::target_machine(&target, opt_level.into(), reloc, model).unwrap();

        target_machine
            .write_to_file(&env.module, FileType::Object, &app_o_file)
            .expect("Writing .o file failed");
    }

    let emit_o_file = emit_o_file_start.elapsed().unwrap();

    CodeGenTiming {
        emit_o_file,
        code_gen,
    }
}

pub struct FunctionIterator<'ctx> {
    next: Option<FunctionValue<'ctx>>,
}

impl<'ctx> FunctionIterator<'ctx> {
    pub fn from_module(module: &inkwell::module::Module<'ctx>) -> Self {
        Self {
            next: module.get_first_function(),
        }
    }
}

impl<'ctx> Iterator for FunctionIterator<'ctx> {
    type Item = FunctionValue<'ctx>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next {
            Some(function) => {
                self.next = function.get_next_function();

                Some(function)
            }
            None => None,
        }
    }
}
