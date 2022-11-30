use crate::rust_glue;
use crate::types::{Env, Types};
use bumpalo::Bump;
use roc_intern::GlobalInterner;
use roc_load::{ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_packaging::cache::{self, RocCacheDir};
use roc_reporting::report::{RenderTarget, DEFAULT_PALETTE};
use roc_target::{Architecture, OperatingSystem, TargetInfo};
use std::fs::File;
use std::io::{self, ErrorKind, Write};
use std::path::{Path, PathBuf};
use std::process;
use strum::IntoEnumIterator;
use target_lexicon::Triple;

pub struct IgnoreErrors {
    pub can: bool,
}

impl IgnoreErrors {
    const NONE: Self = IgnoreErrors { can: false };
}

pub fn generate(input_path: &Path, output_path: &Path) -> io::Result<i32> {
    match load_types(
        input_path.to_path_buf(),
        Threading::AllAvailable,
        IgnoreErrors::NONE,
    ) {
        Ok(types_and_targets) => {
            let mut file = File::create(output_path).unwrap_or_else(|err| {
                eprintln!(
                    "Unable to create output file {} - {:?}",
                    output_path.display(),
                    err
                );

                process::exit(1);
            });

            let mut buf = std::str::from_utf8(rust_glue::HEADER).unwrap().to_string();
            let body = rust_glue::emit(&types_and_targets);

            buf.push_str(&body);

            file.write_all(buf.as_bytes()).unwrap_or_else(|err| {
                eprintln!(
                    "Unable to write bindings to output file {} - {:?}",
                    output_path.display(),
                    err
                );

                process::exit(1);
            });

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
) -> Result<Vec<(Types, TargetInfo)>, io::Error> {
    let target_info = (&Triple::host()).into();
    let arena = &Bump::new();
    let subs_by_module = Default::default();
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
        subs_by_module,
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

    let layout_interner = GlobalInterner::with_capacity(128);

    let architectures = Architecture::iter();
    let mut types_and_targets = Vec::with_capacity(architectures.len());
    for arch in architectures {
        let target_info = TargetInfo {
            architecture: arch,
            operating_system: OperatingSystem::Unix,
        };

        let types = {
            let mut env = Env::new(arena, subs, &interns, layout_interner.fork(), target_info);

            env.vars_to_types(variables.clone())
        };

        types_and_targets.push((types, target_info));
    }

    Ok(types_and_targets)
}
