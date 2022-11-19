use std::path::{Path, PathBuf};

#[cfg(not(windows))]
use bumpalo::Bump;
use roc_module::symbol::ModuleId;

#[cfg(not(windows))]
const ROC_SKIP_SUBS_CACHE: &str = "ROC_SKIP_SUBS_CACHE";

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

    let arena = Bump::new();
    let src_dir = PathBuf::from(".");
    let source = roc_builtins::roc::module_source(module_id);
    let target_info = roc_target::TargetInfo::default_x86_64();

    let res_module = roc_load_internal::file::load_and_typecheck_str(
        &arena,
        PathBuf::from(filename),
        source,
        src_dir,
        Default::default(),
        target_info,
        roc_reporting::report::RenderTarget::ColorTerminal,
        roc_reporting::report::DEFAULT_PALETTE,
        Threading::AllAvailable,
    );

    let module = match res_module {
        Ok(v) => v,
        Err(LoadingProblem::FormattedReport(report)) => {
            panic!("{}", report);
        }
        Err(other) => {
            panic!("build_file failed with error:\n{:?}", other);
        }
    };

    if module.total_problems() > 0 {
        panic!("Problems were found! Refusing to build cached subs.\nTry building with {}=1 to see them.", ROC_SKIP_SUBS_CACHE);
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
