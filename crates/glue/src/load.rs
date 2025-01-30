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
use roc_error_macros::{internal_error, todo_lambda_erasure};
use roc_gen_llvm::run_roc::RocCallResult;
use roc_load::{ExecutionMode, FunctionKind, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_mono::ir::{generate_glue_procs, CrashTag, GlueProc, OptLevel};
use roc_mono::layout::{GlobalLayoutInterner, LayoutCache, LayoutInterner};
use roc_packaging::cache::{self, RocCacheDir};
use roc_reporting::report::{RenderTarget, DEFAULT_PALETTE};
use roc_target::{Architecture, Target, TargetFromTripleError::TripleUnsupported};
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
    link_type: LinkType,
    linking_strategy: LinkingStrategy,
) -> io::Result<i32> {
    let target = Triple::host().into();
    // TODO: Add verification around the paths. Make sure they have the correct file extension and what not.
    match load_types(
        input_path.to_path_buf(),
        Threading::AllAvailable,
        IgnoreErrors::NONE,
        target,
    ) {
        Ok(types) => {
            // TODO: we should to modify the app file first before loading it.
            // Somehow it has to point to the correct platform file which may not exist on the target machine.

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
            let tempdir_res = tempfile::tempdir();

            // we don't need a host for glue, we will generate a dylib
            // that will be loaded by the roc compiler/cli
            let build_host = false;
            let suppress_build_host_warning = true;

            let res_binary_path = match tempdir_res {
                Ok(dylib_dir) => build_file(
                    &arena,
                    target,
                    spec_path.to_path_buf(),
                    code_gen_options,
                    false,
                    link_type,
                    linking_strategy,
                    build_host,
                    suppress_build_host_warning,
                    None,
                    RocCacheDir::Persistent(cache::roc_cache_packages_dir().as_path()),
                    load_config,
                    Some(dylib_dir.path()),
                    false,
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
                    let roc_types: roc_std::RocList<roc_type::Types> =
                        types.iter().map(|x| x.into()).collect();

                    // NOTE: DO NOT DROP LIB! the return value will include static roc strings that
                    // are only kept alive when the dynamic library is not unloaded!
                    let files = call_roc_make_glue(&lib, backend, roc_types);

                    for roc_type::File { name, content } in &files {
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

fn call_roc_make_glue(
    lib: &Library,
    backend: CodeGenBackend,
    roc_types: roc_std::RocList<roc_type::Types>,
) -> roc_std::RocList<roc_type::File> {
    let roc_call_result = match backend {
        CodeGenBackend::Assembly(_) => {
            type MakeGlueReturnType = RocCallResult<
                roc_std::RocResult<roc_std::RocList<roc_type::File>, roc_std::RocStr>,
            >;
            type MakeGlue =
                unsafe extern "C" fn(roc_std::RocList<roc_type::Types>) -> MakeGlueReturnType;

            let name_of_main = "test_main";

            let make_glue: libloading::Symbol<MakeGlue> = unsafe {
                lib.get(name_of_main.as_bytes())
                    .unwrap_or_else(|_| panic!("Unable to load glue function"))
            };

            unsafe { make_glue(roc_types) }
        }
        CodeGenBackend::Llvm(_) => {
            type MakeGlueReturnType =
                roc_std::RocResult<roc_std::RocList<roc_type::File>, roc_std::RocStr>;
            type MakeGlue = unsafe extern "C" fn(
                *mut RocCallResult<MakeGlueReturnType>,
                &roc_std::RocList<roc_type::Types>,
            );

            let name_of_main = "roc__make_glue_for_host_1_exposed_generic";

            let make_glue: libloading::Symbol<MakeGlue> = unsafe {
                lib.get(name_of_main.as_bytes())
                    .unwrap_or_else(|_| panic!("Unable to load glue function: {lib:?}"))
            };
            let mut files = RocCallResult::new(roc_std::RocResult::err(roc_std::RocStr::empty()));
            unsafe { make_glue(&mut files, &roc_types) };

            // Roc will free data passed into it. So forget that data.
            std::mem::forget(roc_types);

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
                eprintln!("Glue generation failed: {err}");
                process::exit(1);
            }
            Ok(files) => files,
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
            RigidVar(_)
            | RigidAbleVar(_, _)
            | FlexVar(_)
            | FlexAbleVar(_, _)
            | Pure
            | Effectful
            | Error => (),

            RecursionVar { .. } => {
                // we got here, so we've treated this type already
            }

            Structure(flat_type) => match flat_type {
                Apply(_, args) => {
                    stack.extend(var_slice!(*args));
                }

                Func(arg_vars, closure_var, ret_var, _fx_var) => {
                    lambda_sets.push(subs.get_root_key_without_compacting(*closure_var));

                    stack.push(*ret_var);
                    stack.push(*closure_var);
                    stack.extend(var_slice!(arg_vars));
                }

                EmptyRecord => (),
                EmptyTagUnion => (),
                EffectfulFunc => internal_error!(),

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
                        let slice = subs.variable_slices[slice_index.index()];
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
                        let slice = subs.variable_slices[slice_index.index()];
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
                    let slice = subs.variable_slices[slice_index.index()];
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
    target: Target,
) -> Result<Vec<Types>, io::Error> {
    let function_kind = FunctionKind::from_env();
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
        None,
        RocCacheDir::Persistent(cache::roc_cache_packages_dir().as_path()),
        LoadConfig {
            target,
            function_kind,
            render: RenderTarget::Generic,
            palette: DEFAULT_PALETTE,
            threading,
            exec_mode: ExecutionMode::Check,
        },
    )
    .unwrap_or_else(|problem| match problem {
        LoadingProblem::FormattedReport(report, _) => {
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

    let operating_system = target.operating_system();
    let architectures = Architecture::iter();
    let mut arch_types = Vec::with_capacity(architectures.len());

    for architecture in architectures {
        let mut interns = interns.clone(); // TODO there may be a way to avoid this.
        let target = match Target::try_from((architecture, operating_system)) {
            Ok(t) => t,
            Err(TripleUnsupported) => continue,
        };

        let layout_interner = GlobalLayoutInterner::with_capacity(128, target);
        let mut layout_cache = LayoutCache::new(layout_interner.fork(), target);
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
            target,
            exposed_to_host.clone(),
        );

        arch_types.push(types);
    }

    Ok(arch_types)
}
