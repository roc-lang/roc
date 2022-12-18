use bumpalo::Bump;
use roc_load::{ExecutionMode, LoadConfig, LoadedModule, Threading};
use roc_packaging::cache::RocCacheDir;
use roc_reporting::report::DEFAULT_PALETTE;
use roc_target::TargetInfo;
use std::path::Path;

pub fn load_module(
    src_file: &Path,
    roc_cache_dir: RocCacheDir<'_>,
    threading: Threading,
) -> LoadedModule {
    let subs_by_module = Default::default();

    let load_config = LoadConfig {
        target_info: TargetInfo::default_x86_64(), // editor only needs type info, so this is unused
        render: roc_reporting::report::RenderTarget::ColorTerminal,
        palette: DEFAULT_PALETTE,
        threading,
        exec_mode: ExecutionMode::Check,
    };

    let arena = Bump::new();
    let loaded = roc_load::load_and_typecheck(
        &arena,
        src_file.to_path_buf(),
        subs_by_module,
        roc_cache_dir,
        load_config,
    );

    match loaded {
        Ok(x) => x,
        Err(roc_load::LoadingProblem::FormattedReport(report)) => {
            panic!(
                "Failed to load module from src_file: {:?}. Report: {}",
                src_file, report
            );
        }
        Err(e) => panic!(
            "Failed to load module from src_file {:?}: {:?}",
            src_file, e
        ),
    }
}
