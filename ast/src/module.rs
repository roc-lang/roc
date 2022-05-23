use bumpalo::Bump;
use roc_load::{LoadedModule, Threading};
use roc_load_internal::file::ModuleHeader;
use roc_target::TargetInfo;
use std::path::Path;
pub fn load_module<'a>(arena: &'a Bump, src_file: &Path, threading: Threading) -> (LoadedModule, ModuleHeader<'a>) {

    let subs_by_module = Default::default();

    let loaded = roc_load::load_and_typecheck_editor(
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
        threading,
    );

    match loaded {
        Ok(mod_header_tup) => mod_header_tup,
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
