use std::path::PathBuf;

use bumpalo::Bump;
use roc_load_internal::file::Threading;
use roc_module::symbol::ModuleId;

const MODULES: &[(ModuleId, &str)] = &[
    (ModuleId::BOOL, "Bool.roc"),
    (ModuleId::RESULT, "Result.roc"),
    (ModuleId::NUM, "Num.roc"),
    (ModuleId::LIST, "List.roc"),
    (ModuleId::STR, "Str.roc"),
    (ModuleId::DICT, "Dict.roc"),
    (ModuleId::SET, "Set.roc"),
    (ModuleId::BOX, "Box.roc"),
];

fn main() {
    for (module_id, filename) in MODULES {
        write_subs_for_module(*module_id, filename);
    }
}

fn write_subs_for_module(module_id: ModuleId, filename: &str) {
    // Tell Cargo that if the given file changes, to rerun this build script.
    println!("cargo:rerun-if-changed=../builtins/roc/{}", filename);

    let arena = Bump::new();
    let src_dir = PathBuf::from(".");
    let source = roc_builtins::roc::module_source(module_id);
    let target_info = roc_target::TargetInfo::default_x86_64();

    let res_module = roc_load_internal::file::load_and_typecheck_str(
        &arena,
        PathBuf::from(filename),
        source,
        &src_dir,
        Default::default(),
        target_info,
        roc_reporting::report::RenderTarget::ColorTerminal,
        Threading::AllAvailable,
    );

    let module = res_module.unwrap();
    let subs = module.solved.inner();
    let exposed_vars_by_symbol: Vec<_> = module.exposed_to_host.into_iter().collect();

    let mut output_path = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    output_path.extend(&[filename]);
    output_path.set_extension("dat");
    let mut file = std::fs::File::create(&output_path).unwrap();
    subs.serialize(&exposed_vars_by_symbol, &mut file).unwrap();
}
