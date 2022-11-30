use std::path::{Path, PathBuf};

#[cfg(not(windows))]
use bumpalo::Bump;
use roc_module::symbol::ModuleId;
use roc_packaging::cache::RocCacheDir;

const SKIP_SUBS_CACHE: bool = {
    match option_env!("ROC_SKIP_SUBS_CACHE") {
        Some(s) => s.len() == 1 && s.as_bytes()[0] == b'1',
        None => false,
    }
};

// IFTTT: crates/compiler/load/src/lib.rs
const MODULES: &[(ModuleId, &str)] = &[
    (ModuleId::BOOL, "Bool.roc"),
    (ModuleId::DICT, "Dict.roc"),
    (ModuleId::SET, "Set.roc"),
    (ModuleId::RESULT, "Result.roc"),
    (ModuleId::NUM, "Num.roc"),
    (ModuleId::LIST, "List.roc"),
    (ModuleId::STR, "Str.roc"),
    (ModuleId::BOX, "Box.roc"),
    (ModuleId::ENCODE, "Encode.roc"),
    (ModuleId::DECODE, "Decode.roc"),
    (ModuleId::HASH, "Hash.roc"),
    (ModuleId::JSON, "Json.roc"),
];

fn main() {
    for (module_id, filename) in MODULES {
        write_subs_for_module(*module_id, filename);
    }
}

fn write_subs_for_module(module_id: ModuleId, filename: &str) {
    // Tell Cargo that if the given file changes, to rerun this build script.
    let filepath = PathBuf::from("..")
        .join("builtins")
        .join("roc")
        .join(filename);
    println!("cargo:rerun-if-changed={}", filepath.to_str().unwrap());

    let mut output_path = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    output_path.extend([filename]);
    output_path.set_extension("dat");

    #[cfg(not(windows))]
    if SKIP_SUBS_CACHE {
        write_types_for_module_dummy(&output_path)
    } else {
        write_types_for_module_real(module_id, filename, &output_path)
    }

    #[cfg(windows)]
    {
        let _ = SKIP_SUBS_CACHE;
        let _ = module_id;
        write_types_for_module_dummy(&output_path)
    }
}

fn write_types_for_module_dummy(output_path: &Path) {
    // write out a dummy file
    std::fs::write(output_path, []).unwrap();
}

#[cfg(not(windows))]
fn write_types_for_module_real(module_id: ModuleId, filename: &str, output_path: &Path) {
    use roc_can::module::TypeState;
    use roc_load_internal::file::{LoadingProblem, Threading};
    use roc_reporting::cli::report_problems;

    let arena = Bump::new();
    let cwd = std::env::current_dir().unwrap();
    let source = roc_builtins::roc::module_source(module_id);
    let target_info = roc_target::TargetInfo::default_x86_64();

    let res_module = roc_load_internal::file::load_and_typecheck_str(
        &arena,
        PathBuf::from(filename),
        source,
        cwd,
        Default::default(),
        target_info,
        roc_reporting::report::RenderTarget::ColorTerminal,
        roc_reporting::report::DEFAULT_PALETTE,
        RocCacheDir::Disallowed,
        Threading::AllAvailable,
    );

    let mut module = match res_module {
        Ok(v) => v,
        Err(LoadingProblem::FormattedReport(report)) => {
            panic!("{}", report);
        }
        Err(other) => {
            panic!("build_file failed with error:\n{:?}", other);
        }
    };

    let problems = report_problems(
        module.total_problems(),
        &module.sources,
        &module.interns,
        &mut module.can_problems,
        &mut module.type_problems,
    );

    if problems.errors + problems.warnings > 0 {
        panic!("Problems were found! Refusing to build cached subs.");
    }

    let subs = module.solved.into_inner();
    let exposed_vars_by_symbol: Vec<_> = module.exposed_to_host.into_iter().collect();
    let abilities = module.abilities_store;
    let solved_implementations = module.resolved_implementations;

    let mut file = std::fs::File::create(output_path).unwrap();

    let type_state = TypeState {
        subs,
        exposed_vars_by_symbol,
        abilities,
        solved_implementations,
    };

    type_state.serialize(&mut file).unwrap();
}
