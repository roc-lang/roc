use libloading::Library;
use roc_build::link::{link, LinkType};
use roc_collections::all::MutMap;
use tempfile::tempdir;

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from("app \"test\" provides [ main ] to \"./platform\"\n\nmain =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}

pub fn helper<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    stdlib: roc_builtins::std::StdLib,
    _leak: bool,
    lazy_literals: bool,
) -> (String, Vec<roc_problem::can::Problem>, Library) {
    use std::path::{Path, PathBuf};

    //let stdlib_mode = stdlib.mode;
    let dir = tempdir().unwrap();
    let filename = PathBuf::from("Test.roc");
    let src_dir = Path::new("fake/test/path");
    let app_o_file = dir.path().join("app.o");

    let module_src;
    let temp;
    if src.starts_with("app") {
        // this is already a module
        module_src = src;
    } else {
        // this is an expression, promote it to a module
        temp = promote_expr_to_module(src);
        module_src = &temp;
    }

    let exposed_types = MutMap::default();
    let loaded = roc_load::file::load_and_monomorphize_from_str(
        arena,
        filename,
        &module_src,
        stdlib,
        src_dir,
        exposed_types,
    );

    let mut loaded = loaded.expect("failed to load module");

    use roc_load::file::MonomorphizedModule;
    let MonomorphizedModule {
        procedures,
        interns,
        exposed_to_host,
        ..
    } = loaded;

    /*
    println!("=========== Procedures ==========");
    println!("{:?}", procedures);
    println!("=================================\n");

    println!("=========== Interns    ==========");
    println!("{:?}", interns);
    println!("=================================\n");

    println!("=========== Exposed    ==========");
    println!("{:?}", exposed_to_host);
    println!("=================================\n");
    */

    debug_assert_eq!(exposed_to_host.len(), 1);
    let main_fn_symbol = exposed_to_host.keys().copied().nth(0).unwrap();

    let (_, main_fn_layout) = procedures
        .keys()
        .find(|(s, _)| *s == main_fn_symbol)
        .unwrap()
        .clone();
    let mut layout_ids = roc_mono::layout::LayoutIds::default();
    let main_fn_name = layout_ids
        .get(main_fn_symbol, &main_fn_layout)
        .to_symbol_string(main_fn_symbol, &interns);

    let mut lines = Vec::new();
    // errors whose reporting we delay (so we can see that code gen generates runtime errors)
    let mut delayed_errors = Vec::new();

    for (home, (module_path, src)) in loaded.sources {
        use roc_reporting::report::{
            can_problem, mono_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE,
        };

        let can_problems = loaded.can_problems.remove(&home).unwrap_or_default();
        let type_problems = loaded.type_problems.remove(&home).unwrap_or_default();
        let mono_problems = loaded.mono_problems.remove(&home).unwrap_or_default();

        let error_count = can_problems.len() + type_problems.len() + mono_problems.len();

        if error_count == 0 {
            continue;
        }

        let src_lines: Vec<&str> = src.split('\n').collect();
        let palette = DEFAULT_PALETTE;

        // Report parsing and canonicalization problems
        let alloc = RocDocAllocator::new(&src_lines, home, &interns);

        use roc_problem::can::Problem::*;
        for problem in can_problems.into_iter() {
            // Ignore "unused" problems
            match problem {
                UnusedDef(_, _) | UnusedArgument(_, _, _) | UnusedImport(_, _) => {
                    delayed_errors.push(problem);
                    continue;
                }
                _ => {
                    let report = can_problem(&alloc, module_path.clone(), problem);
                    let mut buf = String::new();

                    report.render_color_terminal(&mut buf, &alloc, &palette);

                    lines.push(buf);
                }
            }
        }

        for problem in type_problems {
            let report = type_problem(&alloc, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }

        for problem in mono_problems {
            let report = mono_problem(&alloc, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }
    }

    if !lines.is_empty() {
        println!("{}", lines.join("\n"));
        assert_eq!(0, 1, "Mistakes were made");
    }

    let env = roc_gen_dev::Env {
        arena,
        interns,
        exposed_to_host: exposed_to_host.keys().copied().collect(),
        lazy_literals,
    };

    let target = target_lexicon::Triple::host();
    let module_object =
        roc_gen_dev::build_module(&env, &target, procedures).expect("failed to compile module");

    let module_out = module_object
        .write()
        .expect("failed to build output object");
    std::fs::write(&app_o_file, module_out).expect("failed to write object to file");

    let (mut child, dylib_path) = link(
        &target,
        app_o_file.clone(),
        &[app_o_file.to_str().unwrap()],
        LinkType::Dylib,
    )
    .expect("failed to link dynamic library");

    child.wait().unwrap();

    // Load the dylib
    let path = dylib_path.as_path().to_str().unwrap();

    // std::fs::copy(&app_o_file, "/tmp/app.o").unwrap();
    // std::fs::copy(&path, "/tmp/libapp.so").unwrap();

    let lib = Library::new(path).expect("failed to load shared library");

    (main_fn_name, delayed_errors, lib)
}

#[macro_export]
macro_rules! assert_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {{
        assert_evals_to!($src, $expected, $ty, (|val| val));
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        // Same as above, except with an additional transformation argument.
        {
            assert_evals_to!($src, $expected, $ty, $transform, true);
        }
    };
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr) => {
        // Run both with and without lazy literal optimization.
        {
            assert_evals_to!($src, $expected, $ty, $transform, $leak, false);
        }
        {
            assert_evals_to!($src, $expected, $ty, $transform, $leak, true);
        }
    };
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr, $lazy_literals:expr) => {
        use bumpalo::Bump;
        use roc_gen_dev::run_jit_function_raw;
        let stdlib = roc_builtins::std::standard_stdlib();

        let arena = Bump::new();
        let (main_fn_name, errors, lib) =
            $crate::helpers::eval::helper(&arena, $src, stdlib, $leak, $lazy_literals);

        let transform = |success| {
            let expected = $expected;
            let given = $transform(success);
            assert_eq!(&given, &expected);
        };
        run_jit_function_raw!(lib, main_fn_name, $ty, transform, errors)
    };
}
