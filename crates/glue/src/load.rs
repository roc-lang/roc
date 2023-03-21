use crate::rust_glue;
use crate::types::Types;
use bumpalo::Bump;
use roc_collections::MutMap;
use roc_load::{ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_mono::ir::{generate_glue_procs, GlueProc};
use roc_mono::layout::{GlobalLayoutInterner, LayoutCache, LayoutInterner};
use roc_packaging::cache::{self, RocCacheDir};
use roc_reporting::report::{RenderTarget, DEFAULT_PALETTE};
use roc_target::{Architecture, TargetInfo};
use roc_types::subs::{Subs, Variable};
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

fn number_lambda_sets(subs: &Subs, initial: Variable) -> Vec<Variable> {
    let mut lambda_sets = vec![];
    let mut stack = vec![initial];

    macro_rules! var_slice {
        ($variable_subs_slice:expr) => {{
            let slice = $variable_subs_slice;
            subs.variables[slice.indices()].iter().rev()
        }};
    }

    while let Some(var) = stack.pop() {
        use roc_types::subs::Content::*;
        use roc_types::subs::FlatType::*;

        use roc_types::subs::GetSubsSlice;
        use roc_types::types::Uls;

        match subs.get_content_without_compacting(var) {
            RigidVar(_) | RigidAbleVar(_, _) | FlexVar(_) | FlexAbleVar(_, _) | Error => (),

            RecursionVar { .. } => {
                // we got here, so we've treated this type already
            }

            Structure(flat_type) => match flat_type {
                Apply(_, args) => {
                    stack.extend(var_slice!(*args));
                }

                Func(arg_vars, closure_var, ret_var) => {
                    lambda_sets.push(subs.get_root_key_without_compacting(*closure_var));

                    stack.push(*ret_var);
                    stack.push(*closure_var);
                    stack.extend(var_slice!(arg_vars));
                }

                EmptyRecord => (),
                EmptyTagUnion => (),
                EmptyTuple => (),

                Record(fields, ext) => {
                    let fields = *fields;
                    let ext = *ext;

                    stack.push(ext);
                    stack.extend(var_slice!(fields.variables()));
                }
                Tuple(_, _) => todo!(),
                TagUnion(tags, ext) => {
                    let tags = *tags;
                    let ext = *ext;

                    stack.push(ext.var());

                    for slice_index in tags.variables() {
                        let slice = subs.variable_slices[slice_index.index as usize];
                        stack.extend(var_slice!(slice));
                    }
                }
                FunctionOrTagUnion(_, _, ext) => {
                    stack.push(ext.var());
                }

                RecursiveTagUnion(rec_var, tags, ext) => {
                    let tags = *tags;
                    let ext = *ext;
                    let rec_var = *rec_var;

                    stack.push(ext.var());

                    for slice_index in tags.variables() {
                        let slice = subs.variable_slices[slice_index.index as usize];
                        stack.extend(var_slice!(slice));
                    }

                    stack.push(rec_var);
                }
            },
            Alias(_, args, var, _) => {
                let var = *var;
                let args = *args;

                stack.extend(var_slice!(args.all_variables()));

                stack.push(var);
            }
            LambdaSet(roc_types::subs::LambdaSet {
                solved,
                recursion_var,
                unspecialized,
                ambient_function: _,
            }) => {
                for slice_index in solved.variables() {
                    let slice = subs.variable_slices[slice_index.index as usize];
                    stack.extend(var_slice!(slice));
                }

                if let Some(rec_var) = recursion_var.into_variable() {
                    stack.push(rec_var);
                }

                for Uls(var, _, _) in subs.get_subs_slice(*unspecialized) {
                    stack.push(*var);
                }
            }
            &RangedNumber(_) => {}
        }
    }

    lambda_sets
}

pub fn load_types(
    full_file_path: PathBuf,
    threading: Threading,
    ignore_errors: IgnoreErrors,
) -> Result<Vec<(Types, TargetInfo)>, io::Error> {
    let target_info = (&Triple::host()).into();
    let arena = &Bump::new();
    let LoadedModule {
        module_id: home,
        mut can_problems,
        mut type_problems,
        mut declarations_by_id,
        mut solved,
        interns,
        exposed_to_host,
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

    // Get the variables for all the exposed_to_host symbols
    let variables = (0..decls.len()).filter_map(|index| {
        if exposed_to_host.contains_key(&decls.symbols[index].value) {
            Some(decls.variables[index])
        } else {
            None
        }
    });

    let operating_system = target_info.operating_system;
    let architectures = Architecture::iter();
    let mut types_and_targets = Vec::with_capacity(architectures.len());

    let layout_interner = GlobalLayoutInterner::with_capacity(128, target_info);

    for architecture in architectures {
        let mut interns = interns.clone(); // TODO there may be a way to avoid this.
        let target_info = TargetInfo {
            architecture,
            operating_system,
        };
        let mut layout_cache = LayoutCache::new(layout_interner.fork(), target_info);
        let mut glue_procs_by_layout = MutMap::default();

        let mut extern_names = MutMap::default();

        // Populate glue getters/setters for all relevant variables
        for var in variables.clone() {
            for (i, v) in number_lambda_sets(subs, var).iter().enumerate() {
                extern_names.insert(*v, i.to_string());
            }

            let in_layout = layout_cache
                .from_var(arena, var, subs)
                .expect("Something weird ended up in the content");

            let layout = layout_cache.interner.get(in_layout);

            // dbg!(layout);

            if layout.has_varying_stack_size(&layout_cache.interner, arena) {
                let ident_ids = interns.all_ident_ids.get_mut(&home).unwrap();
                let answer = generate_glue_procs(
                    home,
                    ident_ids,
                    arena,
                    &mut layout_interner.fork(),
                    arena.alloc(layout),
                );

                // Even though generate_glue_procs does more work than we need it to,
                // it's important that we use it in order to make sure we get exactly
                // the same names that mono::ir did for code gen!
                for (layout, glue_procs) in answer.getters {
                    let mut names =
                        bumpalo::collections::Vec::with_capacity_in(glue_procs.len(), arena);

                    // Record all the getter/setter names associated with this layout
                    for GlueProc { name, .. } in glue_procs {
                        // Given a struct layout (including lambda sets!) the offsets - and therefore
                        // getters/setters - are deterministic, so we can use layout as the hash key
                        // for these getters/setters. We also only need to store the name because
                        // since they are getters and setters, we can know their types (from a
                        // TypeId perspective) deterministically based on knowing the types of
                        // the structs and fields.
                        //
                        // Store them as strings, because symbols won't be useful to glue generators!
                        names.push(name.as_str(&interns).to_string());
                    }

                    glue_procs_by_layout.insert(layout, names.into_bump_slice());
                }
            }
        }

        let types = Types::new(
            arena,
            subs,
            variables.clone(),
            arena.alloc(interns),
            glue_procs_by_layout,
            layout_cache,
            target_info,
        );

        types_and_targets.push((types, target_info));
    }

    Ok(types_and_targets)
}
