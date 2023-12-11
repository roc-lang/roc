use crate::roc_type;
use crate::types::Types;
use bumpalo::Bump;
use libloading::Library;
use roc_build::{
    link::{LinkType, LinkingStrategy},
    program::{
        build_file, handle_error_module, handle_loading_problem, standard_load_config,
        BuildFileError, BuildOrdering, BuiltFile, CodeGenBackend, CodeGenOptions,
    },
};
use roc_collections::MutMap;
use roc_error_macros::todo_lambda_erasure;
use roc_gen_llvm::run_roc::RocCallResult;
use roc_load::{ExecutionMode, FunctionKind, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_mono::ir::{generate_glue_procs, CrashTag, GlueProc, OptLevel};
use roc_mono::layout::{GlobalLayoutInterner, LayoutCache, LayoutInterner};
use roc_packaging::cache::{self, RocCacheDir};
use roc_reporting::report::{RenderTarget, DEFAULT_PALETTE};
use roc_target::{Architecture, TargetInfo};
use roc_types::subs::{Subs, Variable};
use std::fs::File;
use std::io::{self, ErrorKind, Write};
use std::mem::ManuallyDrop;
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

pub fn generate(
    input_path: &Path,
    output_path: &Path,
    spec_path: &Path,
    backend: CodeGenBackend,
) -> io::Result<i32> {
    // TODO: Add verification around the paths. Make sure they heav the correct file extension and what not.
    match load_types(
        input_path.to_path_buf(),
        Threading::AllAvailable,
        IgnoreErrors::NONE,
    ) {
        Ok(types) => {
            // TODO: we should to modify the app file first before loading it.
            // Somehow it has to point to the correct platform file which may not exist on the target machine.
            let triple = Triple::host();

            let code_gen_options = CodeGenOptions {
                backend,
                opt_level: OptLevel::Development,
                emit_debug_info: false,
                emit_llvm_ir: false,
            };

            let load_config = standard_load_config(
                &triple,
                BuildOrdering::BuildIfChecks,
                Threading::AllAvailable,
            );

            let arena = ManuallyDrop::new(Bump::new());
            let link_type = LinkType::Dylib;
            let linking_strategy = if roc_linker::supported(link_type, &triple) {
                LinkingStrategy::Surgical
            } else {
                LinkingStrategy::Legacy
            };

            let tempdir_res = tempfile::tempdir();

            let res_binary_path = match tempdir_res {
                Ok(dylib_dir) => build_file(
                    &arena,
                    &triple,
                    spec_path.to_path_buf(),
                    code_gen_options,
                    false,
                    link_type,
                    linking_strategy,
                    true,
                    None,
                    RocCacheDir::Persistent(cache::roc_cache_dir().as_path()),
                    load_config,
                    Some(dylib_dir.path()),
                ),
                Err(_) => {
                    eprintln!("`roc glue` was unable to create a tempdir.");
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
                    use target_lexicon::OperatingSystem;
                    let lib_path = match triple.operating_system {
                        OperatingSystem::Windows => binary_path.with_extension("dll"),
                        OperatingSystem::Darwin | OperatingSystem::MacOSX { .. } => {
                            binary_path.with_extension("dylib")
                        }

                        _ => binary_path.with_extension("so.1.0"),
                    };

                    // TODO: Should glue try and run with errors, especially type errors.
                    // My gut feeling is no or that we should add a flag for it.
                    // Given glue will generally be run by random end users, I think it should default to full correctness.
                    debug_assert_eq!(
                        problems.errors, 0,
                        "if there are errors, they should have been returned as an error variant"
                    );
                    if problems.warnings > 0 {
                        problems.print_to_stdout(total_time);
                        println!(
                            ".\n\nRunning glue despite warnings…\n\n\x1B[36m{}\x1B[39m",
                            "─".repeat(80)
                        );
                    }

                    let lib = unsafe { Library::new(lib_path) }.unwrap();
                    type MakeGlueReturnType =
                        roc_std::RocResult<roc_std::RocList<roc_type::File>, roc_std::RocStr>;
                    type MakeGlue = unsafe extern "C" fn(
                        *mut RocCallResult<MakeGlueReturnType>,
                        &roc_std::RocList<roc_type::Types>,
                    );

                    let make_glue: libloading::Symbol<MakeGlue> = unsafe {
                        lib.get("roc__makeGlueForHost_1_exposed_generic".as_bytes())
                            .unwrap_or_else(|_| panic!("Unable to load glue function"))
                    };
                    let roc_types: roc_std::RocList<roc_type::Types> =
                        types.iter().map(|x| x.into()).collect();
                    let mut files =
                        RocCallResult::new(roc_std::RocResult::err(roc_std::RocStr::empty()));
                    unsafe { make_glue(&mut files, &roc_types) };

                    // Roc will free data passed into it. So forget that data.
                    std::mem::forget(roc_types);

                    let files: Result<roc_std::RocList<roc_type::File>, roc_std::RocStr> =
                        match Result::from(files) {
                            Err((msg, tag)) => match tag {
                                CrashTag::Roc => panic!(r#"Roc failed with message: "{msg}""#),
                                CrashTag::User => panic!(r#"User crash with message: "{msg}""#),
                            },
                            Ok(x) => x.into(),
                        };

                    let files = files.unwrap_or_else(|err| {
                        eprintln!("Glue generation failed: {err}");

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

                    println!(
                        "🎉 Generated type declarations in:\n\n\t{}",
                        output_path.display()
                    );

                    Ok(0)
                }
                Err(BuildFileError::ErrorModule { module, total_time }) => {
                    handle_error_module(module, total_time, spec_path.as_os_str(), true)
                }
                Err(BuildFileError::LoadingProblem(problem)) => handle_loading_problem(problem),
            };

            // Extend the lifetime of the tempdir to after we're done with everything,
            // so it doesn't get dropped before we're done reading from it!
            let _ = tempdir_res;

            answer
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
            ErasedLambda => todo_lambda_erasure!(),
            &RangedNumber(_) => {}
        }
    }

    lambda_sets
}

pub fn load_types(
    full_file_path: PathBuf,
    threading: Threading,
    ignore_errors: IgnoreErrors,
) -> Result<Vec<Types>, io::Error> {
    let target_info = (&Triple::host()).into();
    // TODO the function kind may need to be parameterizable.
    let function_kind = FunctionKind::LambdaSet;
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
            function_kind,
            render: RenderTarget::Generic,
            palette: DEFAULT_PALETTE,
            threading,
            exec_mode: ExecutionMode::Check,
        },
    )
    .unwrap_or_else(|problem| match problem {
        LoadingProblem::FormattedReport(report) => {
            eprintln!("{report}");

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
        let symbol = decls.symbols[index].value;
        exposed_to_host.get(&symbol).copied()
    });

    let operating_system = target_info.operating_system;
    let architectures = Architecture::iter();
    let mut arch_types = Vec::with_capacity(architectures.len());

    for architecture in architectures {
        let mut interns = interns.clone(); // TODO there may be a way to avoid this.
        let target_info = TargetInfo {
            architecture,
            operating_system,
        };
        let layout_interner = GlobalLayoutInterner::with_capacity(128, target_info);
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

            if layout_cache
                .interner
                .has_varying_stack_size(in_layout, arena)
            {
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

        let types = Types::new_with_entry_points(
            arena,
            subs,
            arena.alloc(interns),
            glue_procs_by_layout,
            layout_cache,
            target_info,
            exposed_to_host.clone(),
        );

        arch_types.push(types);
    }

    Ok(arch_types)
}
