use bumpalo::Bump;
use roc_constrain::module::ExposedByModule;
use roc_target::TargetInfo;
use std::path::{Path, PathBuf};

pub use roc_load_internal::docs;
pub use roc_load_internal::file::{
    LoadResult, LoadStart, LoadedModule, LoadingProblem, MonomorphizedModule, Phase,
};

fn load<'a>(
    arena: &'a Bump,
    load_start: LoadStart<'a>,
    src_dir: &Path,
    exposed_types: ExposedByModule,
    goal_phase: Phase,
    target_info: TargetInfo,
) -> Result<LoadResult<'a>, LoadingProblem<'a>> {
    // dbg!(env!("OUT_DIR"));
    // TODO inject the stdlib subs
    roc_load_internal::file::load(
        arena,
        load_start,
        src_dir,
        exposed_types,
        goal_phase,
        target_info,
    )
}

pub fn load_and_monomorphize_from_str<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    src: &'a str,
    src_dir: &Path,
    exposed_types: ExposedByModule,
    target_info: TargetInfo,
) -> Result<MonomorphizedModule<'a>, LoadingProblem<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_str(arena, filename, src)?;

    match load(
        arena,
        load_start,
        src_dir,
        exposed_types,
        Phase::MakeSpecializations,
        target_info,
    )? {
        Monomorphized(module) => Ok(module),
        TypeChecked(_) => unreachable!(""),
    }
}

pub fn load_and_monomorphize<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    src_dir: &Path,
    exposed_types: ExposedByModule,
    target_info: TargetInfo,
) -> Result<MonomorphizedModule<'a>, LoadingProblem<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_path(arena, filename)?;

    match load(
        arena,
        load_start,
        src_dir,
        exposed_types,
        Phase::MakeSpecializations,
        target_info,
    )? {
        Monomorphized(module) => Ok(module),
        TypeChecked(_) => unreachable!(""),
    }
}

pub fn load_and_typecheck<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    src_dir: &Path,
    exposed_types: ExposedByModule,
    target_info: TargetInfo,
) -> Result<LoadedModule, LoadingProblem<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_path(arena, filename)?;

    match load(
        arena,
        load_start,
        src_dir,
        exposed_types,
        Phase::SolveTypes,
        target_info,
    )? {
        Monomorphized(_) => unreachable!(""),
        TypeChecked(module) => Ok(module),
    }
}
