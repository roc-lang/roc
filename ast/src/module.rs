use std::path::Path;

use bumpalo::Bump;
use roc_load::LoadedModule;
use roc_target::TargetInfo;

pub fn load_module(src_file: &Path) -> LoadedModule {
    let subs_by_module = Default::default();

    let arena = Bump::new();
    let loaded = roc_load::load_and_typecheck(
        &arena,
        src_file.to_path_buf(),
        src_file.parent().unwrap_or_else(|| {
            panic!(
                "src_file {:?} did not have a parent directory but I need to have one.",
                src_file
            )
        }),
        subs_by_module,
        TargetInfo::default_x86_64(),
        roc_reporting::report::RenderTarget::ColorTerminal,
    );

    match loaded {
        Ok(x) => x,
        Err(roc_load::LoadingProblem::FormattedReport(report)) => {
            panic!(
                "Failed to load module from src_file {:?}. Report: {}",
                src_file, report
            );
        }
        Err(e) => panic!(
            "Failed to load module from src_file {:?}: {:?}",
            src_file, e
        ),
    }
}
