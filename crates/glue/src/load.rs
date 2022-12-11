use crate::roc_type;
use crate::types::{Env, Types};
use bumpalo::Bump;
use libloading::Library;
use roc_load::{ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_mono::layout::GlobalLayoutInterner;
use roc_packaging::cache::{self, RocCacheDir};
use roc_reporting::report::{RenderTarget, DEFAULT_PALETTE};
use roc_target::{Architecture, OperatingSystem, TargetInfo};
use std::fs::File;
use std::io::{self, ErrorKind, Write};
use std::path::{Component, Path, PathBuf};
use std::process;
use strum::IntoEnumIterator;
use target_lexicon::Triple;

pub struct IgnoreErrors {
    pub can: bool,
}

impl IgnoreErrors {
    const NONE: Self = IgnoreErrors { can: false };
}

pub fn generate(input_path: &Path, output_path: &Path, spec_path: &Path) -> io::Result<i32> {
    match load_types(
        input_path.to_path_buf(),
        Threading::AllAvailable,
        IgnoreErrors::NONE,
    ) {
        Ok(types) => {
            // TODO: correctly setup building the glue spec. Then use the dylib it generates here.
            // For now, I am just passing in the dylib as the spec.
            // Also, it would be best if we directly call the internal build function from here instead of launch roc from the cli.
            // Lastly, we may need to modify the app file first before it can be loaded.
            // Somehow it has to point to the correct platform file which may not exist on the target machine.
            let lib = unsafe { Library::new(spec_path) }.unwrap();
            type MakeGlue = unsafe extern "C" fn(
                *mut roc_std::RocResult<roc_std::RocList<roc_type::File>, roc_std::RocStr>,
                &roc_std::RocList<roc_type::Types>,
            );

            let make_glue: libloading::Symbol<MakeGlue> = unsafe {
                lib.get("roc__makeGlueForHost_1_exposed_generic".as_bytes())
                    .ok()
                    .ok_or(format!("Unable to load glue function"))
                    .expect("errored")
            };
            let roc_types: roc_std::RocList<roc_type::Types> =
                types.iter().map(|x| x.into()).collect();
            let mut files = roc_std::RocResult::err(roc_std::RocStr::empty());
            unsafe { make_glue(&mut files, &roc_types) };
            let files: Result<roc_std::RocList<roc_type::File>, roc_std::RocStr> = files.into();
            let files = files.unwrap_or_else(|err| {
                eprintln!("Glue generation failed: {}", err);

                process::exit(1);
            });
            for roc_type::File { name, content } in &files {
                let valid_name = PathBuf::from(name.as_str())
                    .components()
                    .all(|comp| matches!(comp, Component::CurDir | Component::Normal(_)));
                if !valid_name {
                    eprintln!("File name was invalid: {}", &name);

                    process::exit(1);
                }
                let full_path = output_path.join(name.as_str());
                if let Some(dir_path) = full_path.parent() {
                    std::fs::create_dir_all(&dir_path).unwrap_or_else(|err| {
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

            println!(
                "🎉 Generated type declarations in:\n\n\t{}",
                output_path.display()
            );

            Ok(0)
        }
        Err(err) => match err.kind() {
            ErrorKind::NotFound => {
                eprintln!("Platform module file not found: {}", input_path.display());
                process::exit(1);
            }
            error => {
                eprintln!(
                    "Error loading platform module file {} - {:?}",
                    input_path.display(),
                    error
                );
                process::exit(1);
            }
        },
    }
}

pub fn load_types(
    full_file_path: PathBuf,
    threading: Threading,
    ignore_errors: IgnoreErrors,
) -> Result<Vec<Types>, io::Error> {
    let target_info = (&Triple::host()).into();
    let arena = &Bump::new();
    let LoadedModule {
        module_id: home,
        mut can_problems,
        mut type_problems,
        mut declarations_by_id,
        mut solved,
        interns,
        ..
    } = roc_load::load_and_typecheck(
        arena,
        full_file_path,
        RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
        LoadConfig {
            target_info,
            render: RenderTarget::Generic,
            palette: DEFAULT_PALETTE,
            threading,
            exec_mode: ExecutionMode::Check,
        },
    )
    .unwrap_or_else(|problem| match problem {
        LoadingProblem::FormattedReport(report) => {
            eprintln!("{}", report);

            process::exit(1);
        }
        problem => {
            todo!("{:?}", problem);
        }
    });

    let decls = declarations_by_id.remove(&home).unwrap();
    let subs = solved.inner_mut();

    let can_problems = can_problems.remove(&home).unwrap_or_default();
    let type_problems = type_problems.remove(&home).unwrap_or_default();

    if (!ignore_errors.can && !can_problems.is_empty()) || !type_problems.is_empty() {
        todo!(
            "Gracefully report compilation problems during glue generation: {:?}, {:?}",
            can_problems,
            type_problems
        );
    }

    let variables = (0..decls.len()).filter_map(|index| {
        use roc_can::expr::DeclarationTag::*;

        match decls.declarations[index] {
            Value | Function(_) | Recursive(_) | TailRecursive(_) => Some(decls.variables[index]),
            Destructure(_) => {
                // figure out if we need to export non-identifier defs - when would that
                // happen?
                None
            }
            MutualRecursion { .. } => {
                // handled by future iterations
                None
            }
            Expectation | ExpectationFx => {
                // not publicly visible
                None
            }
        }
    });

    let layout_interner = GlobalLayoutInterner::with_capacity(128, target_info);

    let architectures = Architecture::iter();
    let mut arch_types = Vec::with_capacity(architectures.len());
    for arch in architectures {
        let target_info = TargetInfo {
            architecture: arch,
            operating_system: OperatingSystem::Unix,
        };

        let types = {
            let mut env = Env::new(arena, subs, &interns, layout_interner.fork(), target_info);

            env.vars_to_types(variables.clone())
        };

        arch_types.push(types);
    }

    Ok(arch_types)
}
