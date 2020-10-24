use libloading::Library;
use roc_build::{
    link::{link, LinkType},
    program::gen_from_mono_module,
};
use roc_collections::all::{MutMap, MutSet};
use target_lexicon::Triple;
use tempfile::tempdir;

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from("app Test provides [ main ] imports []\n\nmain =\n");

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
    leak: bool,
    context: &'a inkwell::context::Context,
) -> (&'static str, Vec<roc_problem::can::Problem>, Library) {
    use roc_gen::llvm::build::{build_proc, build_proc_header, Scope};
    use std::path::{Path, PathBuf};

    let stdlib_mode = stdlib.mode;
    let filename = PathBuf::from("Test.roc");
    let src_dir = Path::new("fake/test/path");

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

    let loaded = loaded.expect("failed to load module");

    use roc_load::file::MonomorphizedModule;
    let MonomorphizedModule {
        module_id: home,
        can_problems,
        type_problems,
        mono_problems,
        mut procedures,
        interns,
        exposed_to_host,
        ..
    } = loaded;

    debug_assert_eq!(exposed_to_host.len(), 1);
    let main_fn_symbol = exposed_to_host.keys().copied().nth(0).unwrap();

    let (_, main_fn_layout) = procedures
        .keys()
        .find(|(s, _)| *s == main_fn_symbol)
        .unwrap()
        .clone();

    let target = target_lexicon::Triple::host();
    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;

    // don't panic based on the errors here, so we can test that RuntimeError generates the correct code
    let errors = can_problems
        .into_iter()
        .filter(|problem| {
            use roc_problem::can::Problem::*;

            // Ignore "unused" problems
            match problem {
                UnusedDef(_, _) | UnusedArgument(_, _, _) | UnusedImport(_, _) => false,
                _ => true,
            }
        })
        .collect::<Vec<roc_problem::can::Problem>>();

    use roc_reporting::report::{
        can_problem, mono_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE,
    };

    let error_count = errors.len() + type_problems.len() + mono_problems.len();
    let fatal_error_count = type_problems.len() + mono_problems.len();

    if error_count > 0 {
        // There were problems; report them and return.
        let src_lines: Vec<&str> = module_src.split('\n').collect();

        // Used for reporting where an error came from.
        //
        // TODO: maybe Reporting should have this be an Option?
        let path = PathBuf::new();

        // Report problems
        let palette = DEFAULT_PALETTE;

        // Report parsing and canonicalization problems
        let alloc = RocDocAllocator::new(&src_lines, home, &interns);

        let mut lines = Vec::with_capacity(error_count);

        let can_problems = errors.clone();
        for problem in can_problems.into_iter() {
            let report = can_problem(&alloc, path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }

        for problem in type_problems.into_iter() {
            let report = type_problem(&alloc, path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }

        for problem in mono_problems.into_iter() {
            let report = mono_problem(&alloc, path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }

        println!("{}", (&lines).join("\n"));

        // we want to continue onward only for canonical problems at the moment,
        // to check that they codegen into runtime exceptions
        if fatal_error_count > 0 {
            assert_eq!(0, 1, "problems occured");
        }
    }

    let opt_level = if cfg!(debug_assertions) {
        roc_gen::llvm::build::OptLevel::Normal
    } else {
        roc_gen::llvm::build::OptLevel::Optimize
    };

    let dir = tempdir().unwrap();
    let filename = PathBuf::from("Test.roc");
    let file_path = dir.path().join(filename.clone());
    let full_file_path = PathBuf::from(file_path.clone());
    let monomorphized_module = MonomorphizedModule {
        module_id: home,
        can_problems: Vec::new(),
        type_problems: Vec::new(),
        mono_problems: Vec::new(),
        procedures,
        interns,
        exposed_to_host,
        src: loaded.src,
        subs: loaded.subs,
        timings: loaded.timings,
    };

    // Generate app.o
    gen_from_mono_module(
        arena,
        monomorphized_module,
        filename,
        Triple::host(),
        &full_file_path,
        opt_level,
    );

    // Link app.o into a dylib - e.g. app.so or app.dylib
    let (mut child, dylib_path) = link(
        &Triple::host(),
        full_file_path.clone(),
        &[full_file_path.to_str().unwrap()],
        LinkType::Dylib,
    )
    .unwrap();

    child.wait().unwrap();

    // Load the dylib
    let path = dylib_path.as_path().to_str().unwrap();
    let lib = Library::new(path).expect("Error loading compiled dylib for test");

    todo!("TODO promote a main function");
    // (main_fn_name, errors, lib)
}

// TODO this is almost all code duplication with assert_llvm_evals_to
// the only difference is that this calls uniq_expr instead of can_expr.
// Should extract the common logic into test helpers.
#[macro_export]
macro_rules! assert_opt_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr) => {
        use bumpalo::Bump;
        use inkwell::context::Context;
        use roc_gen::run_jit_function;

        let arena = Bump::new();

        let context = Context::create();

        let stdlib = roc_builtins::unique::uniq_stdlib();

        let (main_fn_name, errors, lib) =
            $crate::helpers::eval::helper(&arena, $src, stdlib, $leak, &context);

        let transform = |success| {
            let expected = $expected;
            let given = $transform(success);
            assert_eq!(&given, &expected);
        };
        run_jit_function!(lib, main_fn_name, $ty, transform, errors)
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        assert_opt_evals_to!($src, $expected, $ty, $transform, true)
    };
}

#[macro_export]
macro_rules! assert_llvm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr) => {
        use bumpalo::Bump;
        use inkwell::context::Context;
        use roc_gen::run_jit_function;

        let arena = Bump::new();

        let context = Context::create();
        let stdlib = roc_builtins::std::standard_stdlib();

        let (main_fn_name, errors, lib) =
            $crate::helpers::eval::helper(&arena, $src, stdlib, $leak, &context);

        let transform = |success| {
            let expected = $expected;
            let given = $transform(success);
            assert_eq!(&given, &expected);
        };
        run_jit_function!(lib, main_fn_name, $ty, transform, errors)
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        assert_llvm_evals_to!($src, $expected, $ty, $transform, true);
    };
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
        // Run un-optimized tests, and then optimized tests, in separate scopes.
        // These each rebuild everything from scratch, starting with
        // parsing the source, so that there's no chance their passing
        // or failing depends on leftover state from the previous one.
        {
            assert_llvm_evals_to!($src, $expected, $ty, $transform, $leak);
        }
        {
            assert_opt_evals_to!($src, $expected, $ty, $transform, $leak);
        }
    };
}
