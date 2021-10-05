use std::path::Path;

use bumpalo::Bump;
use roc_collections::all::MutMap;
use roc_load::file::LoadedModule;

pub fn load_module(src_file: &Path) -> LoadedModule {
    let subs_by_module = MutMap::default();

    let arena = Bump::new();
    let loaded = roc_load::file::load_and_typecheck(
        &arena,
        src_file.to_path_buf(),
        arena.alloc(roc_builtins::std::standard_stdlib()),
        src_file.parent().unwrap_or_else(|| {
            panic!(
                "src_file {:?} did not have a parent directory but I need to have one.",
                src_file
            )
        }),
        subs_by_module,
        8,
        roc_can::builtins::builtin_defs_map,
    );

    match loaded {
        Ok(x) => x,
        Err(roc_load::file::LoadingProblem::FormattedReport(report)) => {
            panic!(
                "Failed to load module from src_file {:?}. Report: {:?}",
                src_file, report
            );
        }
        Err(e) => panic!(
            "Failed to load module from src_file {:?}: {:?}",
            src_file, e
        ),
    }
}
