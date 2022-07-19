use bumpalo::Bump;
use roc_load::{LoadedModule, Threading};
use roc_target::TargetInfo;
use std::path::Path;

pub fn load_module(src_file: &Path, threading: Threading) -> LoadedModule {
    let subs_by_module = Default::default();

    let arena = Bump::new();
    let loaded = roc_load::load_and_typecheck(
        &arena,
        src_file.to_path_buf(),
        subs_by_module,
        TargetInfo::default_x86_64(),
        roc_reporting::report::RenderTarget::ColorTerminal,
        threading,
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
