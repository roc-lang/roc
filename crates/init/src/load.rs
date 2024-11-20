use bumpalo::Bump;
use libloading::Library;
use roc_build::{
    link::{LinkType, LinkingStrategy},
    program::{
        build_file, standard_load_config, BuildOrdering, BuiltFile, CodeGenBackend, CodeGenOptions,
    },
};
use std::io;
use std::io::Write;

use roc_gen_llvm::run_roc::RocCallResult;
use roc_load::Threading;
use roc_mono::ir::CrashTag;
use roc_mono::ir::OptLevel;
use roc_packaging::cache::{self, RocCacheDir};
use roc_std::RocRefcounted;
use std::fs::File;
use std::mem::ManuallyDrop;
use std::path::{Component, Path, PathBuf};
use std::process;
use target_lexicon::Triple;

#[cfg(any(
    target_arch = "arm",
    target_arch = "aarch64",
    target_arch = "wasm32",
    target_arch = "x86",
    target_arch = "x86_64"
))]
#[derive(Clone, Debug, Default, Eq, Ord, Hash, PartialEq, PartialOrd)]
#[repr(C)]
pub struct InitFile {
    pub content: roc_std::RocStr,
    pub name: roc_std::RocStr,
}

impl RocRefcounted for InitFile {
    fn inc(&mut self) {
        self.content.inc();
        self.name.inc();
    }

    fn dec(&mut self) {
        self.content.dec();
        self.name.dec();
    }

    fn is_refcounted() -> bool {
        true
    }
}

pub fn generate(
    platform_path: &Path,
    output_path: &Path,
    backend: CodeGenBackend,
) -> io::Result<i32> {
    let target = Triple::host().into();
    let code_gen_options = CodeGenOptions {
        backend,
        opt_level: OptLevel::Development,
        emit_debug_info: false,
        emit_llvm_ir: false,
        fuzz: false,
    };

    let load_config = standard_load_config(
        target,
        BuildOrdering::BuildIfChecks,
        Threading::AllAvailable,
    );

    let arena = ManuallyDrop::new(Bump::new());
    let link_type = LinkType::Dylib;
    let linking_strategy = if roc_linker::supported(link_type, target) {
        LinkingStrategy::Surgical
    } else {
        LinkingStrategy::Legacy
    };

    let init_app = platform_path.join("init.roc");
    if !init_app.exists() {
        eprintln!("`roc init` was unable to generate the project because the target platform does not contain an `init.roc` file.");
        std::process::exit(1);
    }

    let tempdir_res = tempfile::tempdir();

    let res_binary_path = match &tempdir_res {
        Ok(dylib_dir) => build_file(
            &arena,
            target,
            init_app.to_path_buf(),
            code_gen_options,
            false,
            link_type,
            linking_strategy,
            true,
            None,
            RocCacheDir::Persistent(cache::roc_cache_packages_dir().as_path()),
            load_config,
            Some(dylib_dir.path()),
        ),
        Err(_) => {
            eprintln!("`roc init` was unable to create a tempdir.");
            std::process::exit(1);
        }
    };

    let answer = match res_binary_path {
        Ok(BuiltFile {
            binary_path,
            problems,
            total_time,
            expect_metadata: _,
        }) => {
            // TODO: Should binary_path be update to deal with extensions?
            use roc_target::OperatingSystem;
            let lib_path = match target.operating_system() {
                OperatingSystem::Windows => binary_path.with_extension("dll"),
                OperatingSystem::Mac => binary_path.with_extension("dylib"),

                _ => binary_path.with_extension("so"),
            };

            // TODO: Should glue try and run with errors, especially type errors.
            // My gut feeling is no or that we should add a flag for it.
            // Given glue will generally be run by random end users, I think it should default to full correctness.
            debug_assert_eq!(
                problems.errors, 0,
                "if there are errors, they should have been returned as an error variant"
            );
            if problems.warnings > 0 {
                problems.print_error_warning_count(total_time);
                println!(
                    ".\n\nRunning glue despite warnings…\n\n\x1B[36m{}\x1B[39m",
                    "─".repeat(80)
                );
            }

            let lib = unsafe { Library::new(lib_path) }.unwrap();
            let Some(platform_str) = platform_path.to_str() else {
                eprintln!("Could not transform platform path to str");
                std::process::exit(1);
            };
            let platform_roc_str =
                unsafe { roc_std::RocStr::from_slice_unchecked(platform_str.as_bytes()) };
            let files = call_make_init(&lib, backend, platform_roc_str);

            for InitFile { name, content } in &files {
                let valid_name = PathBuf::from(name.as_str())
                    .components()
                    .all(|comp| matches!(comp, Component::CurDir | Component::Normal(_)));
                if !valid_name || name.is_empty() {
                    eprintln!("File name was invalid: {:?}", &name);

                    process::exit(1);
                }
                let full_path = output_path.join(name.as_str());
                if let Some(dir_path) = full_path.parent() {
                    std::fs::create_dir_all(dir_path).unwrap_or_else(|err| {
                        eprintln!(
                            "Unable to create output directory {} - {:?}",
                            dir_path.display(),
                            err
                        );

                        process::exit(1);
                    });
                }
                let mut file = File::create(&full_path).unwrap_or_else(|err| {
                    eprintln!(
                        "Unable to create output file {} - {:?}",
                        full_path.display(),
                        err
                    );

                    process::exit(1);
                });

                file.write_all(content.as_bytes()).unwrap_or_else(|err| {
                    eprintln!(
                        "Unable to write bindings to output file {} - {:?}",
                        full_path.display(),
                        err
                    );

                    process::exit(1);
                });
            }

            println!("🎉 Initialized project in:\n\n\t{}", output_path.display());

            Ok(0)
        }
        Err(val) => {
            println!("{:?}", val);

            eprintln!("`roc glue` was unable to create a tempdir.");
            std::process::exit(1);
        }
    };

    // Extend the lifetime of the tempdir to after we're done with everything,
    // so it doesn't get dropped before we're done reading from it!
    let _ = tempdir_res;
    answer
}

fn call_make_init(
    lib: &Library,
    backend: CodeGenBackend,
    platform_roc_str: roc_std::RocStr,
) -> roc_std::RocList<InitFile> {
    let roc_call_result = match backend {
        CodeGenBackend::Assembly(_) => todo!(),
        CodeGenBackend::Llvm(_) => {
            type MakeInitReturnType =
                roc_std::RocResult<roc_std::RocList<InitFile>, roc_std::RocStr>;
            type MakeInit =
                unsafe extern "C" fn(*mut RocCallResult<MakeInitReturnType>, &roc_std::RocStr);

            let name_of_main = "main";

            let make_init: libloading::Symbol<MakeInit> = unsafe {
                lib.get(name_of_main.as_bytes())
                    .unwrap_or_else(|_| panic!("Unable to load init function"))
            };
            let mut files = RocCallResult::new(roc_std::RocResult::err(roc_std::RocStr::empty()));
            unsafe { make_init(&mut files, &platform_roc_str) };

            // Roc will free data passed into it. So forget that data.
            std::mem::forget(platform_roc_str);

            files
        }

        CodeGenBackend::Wasm => todo!(),
    };

    match Result::from(roc_call_result) {
        Err((msg, tag)) => match tag {
            CrashTag::Roc => panic!(r#"Roc failed with message: "{msg}""#),
            CrashTag::User => panic!(r#"User crash with message: "{msg}""#),
        },
        Ok(files_or_error) => match Result::from(files_or_error) {
            Err(err) => {
                eprintln!("Init generation failed: {err}");
                process::exit(1);
            }
            Ok(files) => files,
        },
    }
}

// TODO: implement some tests
#[test]
fn test_dummy() {
    assert_eq!(1, 2);
}
