#[cfg(feature = "llvm")]
use roc_gen::llvm::build::module_from_builtins;
#[cfg(feature = "llvm")]
pub use roc_gen::llvm::build::FunctionIterator;
#[cfg(feature = "llvm")]
use roc_load::file::MonomorphizedModule;
#[cfg(feature = "llvm")]
use roc_mono::ir::OptLevel;
#[cfg(feature = "llvm")]
use std::path::{Path, PathBuf};
use std::time::Duration;

#[derive(Debug, Clone, Copy, Default)]
pub struct CodeGenTiming {
    pub code_gen: Duration,
    pub emit_o_file: Duration,
}

// TODO how should imported modules factor into this? What if those use builtins too?
// TODO this should probably use more helper functions
// TODO make this polymorphic in the llvm functions so it can be reused for another backend.
#[cfg(feature = "llvm")]
#[allow(clippy::cognitive_complexity)]
pub fn gen_from_mono_module(
    arena: &bumpalo::Bump,
    mut loaded: MonomorphizedModule,
    roc_file_path: &Path,
    target: target_lexicon::Triple,
    app_o_file: &Path,
    opt_level: OptLevel,
    emit_debug_info: bool,
) -> CodeGenTiming {
    use crate::target::{self, convert_opt_level};
    use inkwell::attributes::{Attribute, AttributeLoc};
    use inkwell::context::Context;
    use inkwell::module::Linkage;
    use inkwell::targets::{CodeModel, FileType, RelocMode};
    use std::time::SystemTime;

    use roc_reporting::report::{
        can_problem, mono_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE,
    };

    let code_gen_start = SystemTime::now();

    for (home, (module_path, src)) in loaded.sources {
        let mut src_lines: Vec<&str> = Vec::new();

        if let Some((_, header_src)) = loaded.header_sources.get(&home) {
            src_lines.extend(header_src.split('\n'));
            src_lines.extend(src.split('\n').skip(1));
        } else {
            src_lines.extend(src.split('\n'));
        }
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
    let app_ll_file = {
        let mut temp = PathBuf::from(roc_file_path);
        temp.set_extension("ll");

        temp
    };

    let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
    debug_assert!(kind_id > 0);
    let enum_attr = context.create_enum_attribute(kind_id, 1);

    for function in FunctionIterator::from_module(module) {
        let name = function.get_name().to_str().unwrap();

        // mark our zig-defined builtins as internal
        if name.starts_with("roc_builtins") {
            function.set_linkage(Linkage::Internal);
        }

        if name.starts_with("roc_builtins.dict")
            || name.starts_with("dict.RocDict")
            || name.starts_with("roc_builtins.list")
            || name.starts_with("list.RocList")
        {
            function.add_attribute(AttributeLoc::Function, enum_attr);
        }
    }

    let builder = context.create_builder();
    let (dibuilder, compile_unit) = roc_gen::llvm::build::Env::new_debug_info(module);
    let (mpm, _fpm) = roc_gen::llvm::build::construct_optimization_passes(module, opt_level);

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

    roc_gen::llvm::build::build_procedures(&env, opt_level, loaded.procedures);

    env.dibuilder.finalize();

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    mpm.run_on(module);

    // Verify the module
    if let Err(errors) = env.module.verify() {
        // write the ll code to a file, so we can modify it
        env.module.print_to_file(&app_ll_file).unwrap();

        panic!(
            "😱 LLVM errors when defining module; I wrote the full LLVM IR to {:?}\n\n {:?}",
            app_ll_file, errors,
        );
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    let code_gen = code_gen_start.elapsed().unwrap();
    let emit_o_file_start = SystemTime::now();

    // annotate the LLVM IR output with debug info
    // so errors are reported with the line number of the LLVM source
    if emit_debug_info {
        module.strip_debug_info();

        let mut app_ll_dbg_file = PathBuf::from(roc_file_path);
        app_ll_dbg_file.set_extension("dbg.ll");

        let mut app_bc_file = PathBuf::from(roc_file_path);
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
            target::target_machine(&target, convert_opt_level(opt_level), reloc, model).unwrap();

        target_machine
            .write_to_file(&env.module, FileType::Object, &app_o_file)
            .expect("Writing .o file failed");
    }

    let emit_o_file = emit_o_file_start.elapsed().unwrap();

    CodeGenTiming {
        code_gen,
        emit_o_file,
    }
}
