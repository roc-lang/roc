use inkwell::memory_buffer::MemoryBuffer;
use roc_error_macros::internal_error;
use roc_gen_llvm::llvm::build::{module_from_builtins, LlvmBackendMode};
use roc_gen_llvm::llvm::externs::add_default_roc_externs;
use roc_load::{EntryPoint, ExpectMetadata, LoadedModule, MonomorphizedModule};
use roc_mono::ir::OptLevel;
use roc_reporting::cli::{report_problems, Problems};
use std::ops::Deref;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

#[cfg(feature = "target-wasm32")]
use roc_collections::all::MutSet;

#[derive(Debug, Clone, Copy, Default)]
pub struct CodeGenTiming {
    pub code_gen: Duration,
}

pub fn report_problems_monomorphized(loaded: &mut MonomorphizedModule) -> Problems {
    report_problems(
        loaded.total_problems(),
        &loaded.sources,
        &loaded.interns,
        &mut loaded.can_problems,
        &mut loaded.type_problems,
    )
}

pub fn report_problems_typechecked(loaded: &mut LoadedModule) -> Problems {
    report_problems(
        loaded.total_problems(),
        &loaded.sources,
        &loaded.interns,
        &mut loaded.can_problems,
        &mut loaded.type_problems,
    )
}

pub enum CodeObject {
    MemoryBuffer(MemoryBuffer),
    Vector(Vec<u8>),
}

impl Deref for CodeObject {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            CodeObject::MemoryBuffer(memory_buffer) => memory_buffer.as_slice(),
            CodeObject::Vector(vector) => vector.as_slice(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CodeGenBackend {
    Assembly,
    Llvm,
    Wasm,
}

#[derive(Debug, Clone, Copy)]
pub struct CodeGenOptions {
    pub backend: CodeGenBackend,
    pub opt_level: OptLevel,
    pub emit_debug_info: bool,
}

type GenFromMono<'a> = (CodeObject, CodeGenTiming, ExpectMetadata<'a>);

#[allow(clippy::too_many_arguments)]
pub fn gen_from_mono_module<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    roc_file_path: &Path,
    target: &target_lexicon::Triple,
    code_gen_options: CodeGenOptions,
    preprocessed_host_path: &Path,
    wasm_dev_stack_bytes: Option<u32>,
) -> GenFromMono<'a> {
    match code_gen_options.backend {
        CodeGenBackend::Assembly => gen_from_mono_module_dev(
            arena,
            loaded,
            target,
            preprocessed_host_path,
            wasm_dev_stack_bytes,
        ),
        CodeGenBackend::Llvm => {
            gen_from_mono_module_llvm(arena, loaded, roc_file_path, target, code_gen_options)
        }
        CodeGenBackend::Wasm => {
            // emit wasm via the llvm backend
            gen_from_mono_module_llvm(arena, loaded, roc_file_path, target, code_gen_options)
        }
    }
}

// TODO how should imported modules factor into this? What if those use builtins too?
// TODO this should probably use more helper functions
// TODO make this polymorphic in the llvm functions so it can be reused for another backend.
fn gen_from_mono_module_llvm<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    roc_file_path: &Path,
    target: &target_lexicon::Triple,
    code_gen_options: CodeGenOptions,
) -> GenFromMono<'a> {
    use crate::target::{self, convert_opt_level};
    use inkwell::attributes::{Attribute, AttributeLoc};
    use inkwell::context::Context;
    use inkwell::module::Linkage;
    use inkwell::targets::{FileType, RelocMode};

    let code_gen_start = Instant::now();

    // Generate the binary
    let target_info = roc_target::TargetInfo::from(target);
    let context = Context::create();
    let module = arena.alloc(module_from_builtins(target, &context, "app"));

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

    for function in module.get_functions() {
        let name = function.get_name().to_str().unwrap();

        // mark our zig-defined builtins as internal
        if name.starts_with("roc_builtins") {
            function.set_linkage(Linkage::Internal);
        }

        if name.starts_with("roc_builtins.dict")
            || name.starts_with("roc_builtins.list")
            || name.starts_with("roc_builtins.dec")
            || name.starts_with("list.RocList")
            || name.starts_with("dict.RocDict")
            || name.contains("incref")
            || name.contains("decref")
        {
            function.add_attribute(AttributeLoc::Function, enum_attr);
        }
    }

    let CodeGenOptions {
        backend: _,
        opt_level,
        emit_debug_info,
    } = code_gen_options;

    let builder = context.create_builder();
    let (dibuilder, compile_unit) = roc_gen_llvm::llvm::build::Env::new_debug_info(module);
    let (mpm, _fpm) = roc_gen_llvm::llvm::build::construct_optimization_passes(module, opt_level);

    // Compile and add all the Procs before adding main
    let env = roc_gen_llvm::llvm::build::Env {
        arena,
        layout_interner: &loaded.layout_interner,
        builder: &builder,
        dibuilder: &dibuilder,
        compile_unit: &compile_unit,
        context: &context,
        interns: loaded.interns,
        module,
        target_info,
        mode: match opt_level {
            OptLevel::Development => LlvmBackendMode::BinaryDev,
            OptLevel::Normal | OptLevel::Size | OptLevel::Optimize => LlvmBackendMode::Binary,
        },

        exposed_to_host: loaded.exposed_to_host.values.keys().copied().collect(),
    };

    // does not add any externs for this mode (we have a host) but cleans up some functions around
    // expects that would confuse the surgical linker
    add_default_roc_externs(&env);

    let opt_entry_point = match loaded.entry_point {
        EntryPoint::Executable { symbol, layout, .. } => {
            Some(roc_mono::ir::EntryPoint { symbol, layout })
        }
        EntryPoint::Test => None,
    };

    roc_gen_llvm::llvm::build::build_procedures(
        &env,
        opt_level,
        loaded.procedures,
        opt_entry_point,
        Some(&app_ll_file),
    );

    env.dibuilder.finalize();

    // we don't use the debug info, and it causes weird errors.
    module.strip_debug_info();

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    mpm.run_on(module);

    // Verify the module
    if let Err(errors) = env.module.verify() {
        // write the ll code to a file, so we can modify it
        env.module.print_to_file(&app_ll_file).unwrap();

        internal_error!(
            "😱 LLVM errors when defining module; I wrote the full LLVM IR to {:?}\n\n {}",
            app_ll_file,
            errors.to_string(),
        );
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    // annotate the LLVM IR output with debug info
    // so errors are reported with the line number of the LLVM source
    let memory_buffer = if emit_debug_info {
        module.strip_debug_info();

        let mut app_ll_dbg_file = PathBuf::from(roc_file_path);
        app_ll_dbg_file.set_extension("dbg.ll");

        let mut app_bc_file = PathBuf::from(roc_file_path);
        app_bc_file.set_extension("bc");

        let mut app_o_file = PathBuf::from(roc_file_path);
        app_o_file.set_extension("o");

        use std::process::Command;

        // write the ll code to a file, so we can modify it
        module.print_to_file(&app_ll_file).unwrap();

        // run the debugir https://github.com/vaivaswatha/debugir tool
        match Command::new("debugir")
            .args(["-instnamer", app_ll_file.to_str().unwrap()])
            .output()
        {
            Ok(_) => {}
            Err(error) => {
                use std::io::ErrorKind;
                match error.kind() {
                    ErrorKind::NotFound => internal_error!(
                        r"I could not find the `debugir` tool on the PATH, install it from https://github.com/vaivaswatha/debugir"
                    ),
                    _ => internal_error!("{:?}", error),
                }
            }
        }

        use target_lexicon::Architecture;
        match target.architecture {
            Architecture::X86_64
            | Architecture::X86_32(_)
            | Architecture::Aarch64(_)
            | Architecture::Wasm32 => {
                let ll_to_bc = Command::new("llvm-as")
                    .args([
                        app_ll_dbg_file.to_str().unwrap(),
                        "-o",
                        app_bc_file.to_str().unwrap(),
                    ])
                    .output()
                    .unwrap();

                assert!(ll_to_bc.stderr.is_empty(), "{:#?}", ll_to_bc);

                let llc_args = &[
                    "-relocation-model=pic",
                    "-filetype=obj",
                    app_bc_file.to_str().unwrap(),
                    "-o",
                    app_o_file.to_str().unwrap(),
                ];

                // write the .o file. Note that this builds the .o for the local machine,
                // and ignores the `target_machine` entirely.
                //
                // different systems name this executable differently, so we shotgun for
                // the most common ones and then give up.
                let bc_to_object = Command::new("llc").args(llc_args).output().unwrap();

                assert!(bc_to_object.stderr.is_empty(), "{:#?}", bc_to_object);
            }
            _ => unreachable!(),
        }

        MemoryBuffer::create_from_file(&app_o_file).expect("memory buffer creation works")
    } else {
        // Emit the .o file
        use target_lexicon::Architecture;
        match target.architecture {
            Architecture::X86_64 | Architecture::X86_32(_) | Architecture::Aarch64(_) => {
                let reloc = RelocMode::PIC;
                let target_machine =
                    target::target_machine(target, convert_opt_level(opt_level), reloc).unwrap();

                target_machine
                    .write_to_memory_buffer(env.module, FileType::Object)
                    .expect("Writing .o file failed")
            }
            Architecture::Wasm32 => {
                // Useful for debugging
                // module.print_to_file(app_ll_file);
                module.write_bitcode_to_memory()
            }
            _ => internal_error!(
                "TODO gracefully handle unsupported architecture: {:?}",
                target.architecture
            ),
        }
    };

    let code_gen = code_gen_start.elapsed();

    (
        CodeObject::MemoryBuffer(memory_buffer),
        CodeGenTiming { code_gen },
        ExpectMetadata {
            interns: env.interns,
            layout_interner: loaded.layout_interner,
            expectations: loaded.expectations,
        },
    )
}

#[cfg(feature = "target-wasm32")]
fn gen_from_mono_module_dev<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    target: &target_lexicon::Triple,
    preprocessed_host_path: &Path,
    wasm_dev_stack_bytes: Option<u32>,
) -> GenFromMono<'a> {
    use target_lexicon::Architecture;

    match target.architecture {
        Architecture::Wasm32 => gen_from_mono_module_dev_wasm32(
            arena,
            loaded,
            preprocessed_host_path,
            wasm_dev_stack_bytes,
        ),
        Architecture::X86_64 | Architecture::Aarch64(_) => {
            gen_from_mono_module_dev_assembly(arena, loaded, target)
        }
        _ => todo!(),
    }
}

#[cfg(not(feature = "target-wasm32"))]
pub fn gen_from_mono_module_dev<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    target: &target_lexicon::Triple,
    _host_input_path: &Path,
    _wasm_dev_stack_bytes: Option<u32>,
) -> GenFromMono<'a> {
    use target_lexicon::Architecture;

    match target.architecture {
        Architecture::X86_64 | Architecture::Aarch64(_) => {
            gen_from_mono_module_dev_assembly(arena, loaded, target)
        }
        _ => todo!(),
    }
}

#[cfg(feature = "target-wasm32")]
fn gen_from_mono_module_dev_wasm32<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    preprocessed_host_path: &Path,
    wasm_dev_stack_bytes: Option<u32>,
) -> GenFromMono<'a> {
    let code_gen_start = Instant::now();
    let MonomorphizedModule {
        module_id,
        procedures,
        mut interns,
        layout_interner,
        ..
    } = loaded;

    let exposed_to_host = loaded
        .exposed_to_host
        .values
        .keys()
        .copied()
        .collect::<MutSet<_>>();

    let env = roc_gen_wasm::Env {
        arena,
        layout_interner: &layout_interner,
        module_id,
        exposed_to_host,
        stack_bytes: wasm_dev_stack_bytes.unwrap_or(roc_gen_wasm::Env::DEFAULT_STACK_BYTES),
    };

    let host_bytes = std::fs::read(preprocessed_host_path).unwrap_or_else(|_| {
        internal_error!(
            "Failed to read host object file {}! Try setting --prebuilt-platform=false",
            preprocessed_host_path.display()
        )
    });

    let host_module = roc_gen_wasm::parse_host(arena, &host_bytes).unwrap_or_else(|e| {
        internal_error!(
            "I ran into a problem with the host object file, {} at offset 0x{:x}:\n{}",
            preprocessed_host_path.display(),
            e.offset,
            e.message
        )
    });

    let final_binary_bytes =
        roc_gen_wasm::build_app_binary(&env, &mut interns, host_module, procedures);

    let code_gen = code_gen_start.elapsed();

    (
        CodeObject::Vector(final_binary_bytes),
        CodeGenTiming { code_gen },
        ExpectMetadata {
            interns,
            layout_interner,
            expectations: loaded.expectations,
        },
    )
}

fn gen_from_mono_module_dev_assembly<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    target: &target_lexicon::Triple,
) -> GenFromMono<'a> {
    let code_gen_start = Instant::now();

    let lazy_literals = true;
    let generate_allocators = false; // provided by the platform

    let MonomorphizedModule {
        module_id,
        procedures,
        mut interns,
        exposed_to_host,
        layout_interner,
        ..
    } = loaded;

    let env = roc_gen_dev::Env {
        arena,
        layout_interner: &layout_interner,
        module_id,
        exposed_to_host: exposed_to_host.values.keys().copied().collect(),
        lazy_literals,
        generate_allocators,
    };

    let module_object = roc_gen_dev::build_module(&env, &mut interns, target, procedures);

    let code_gen = code_gen_start.elapsed();

    let module_out = module_object
        .write()
        .expect("failed to build output object");

    (
        CodeObject::Vector(module_out),
        CodeGenTiming { code_gen },
        ExpectMetadata {
            interns,
            layout_interner,
            expectations: loaded.expectations,
        },
    )
}
