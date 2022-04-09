use bumpalo::Bump;
use roc_collections::all::MutMap;
use roc_constrain::module::ExposedByModule;
use roc_module::symbol::{ModuleId, Symbol};
use roc_target::TargetInfo;
use roc_types::subs::{Subs, Variable};
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
    let cached_subs = read_cached_subs();

    roc_load_internal::file::load(
        arena,
        load_start,
        src_dir,
        exposed_types,
        goal_phase,
        target_info,
        cached_subs,
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

// const BOOL: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Bool.dat")) as &[_];
// const RESULT: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Result.dat")) as &[_];
// const LIST: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/List.dat")) as &[_];
// const STR: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Str.dat")) as &[_];
// const DICT: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Dict.dat")) as &[_];
// const SET: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Set.dat")) as &[_];
// const BOX: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Box.dat")) as &[_];
// const NUM: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Num.dat")) as &[_];

fn deserialize_help(bytes: &[u8]) -> (Subs, Vec<(Symbol, Variable)>) {
    let (subs, slice) = Subs::deserialize(bytes);

    (subs, slice.to_vec())
}

fn read_cached_subs() -> MutMap<ModuleId, (Subs, Vec<(Symbol, Variable)>)> {
    let mut output = MutMap::default();

    // output.insert(ModuleId::BOOL, deserialize_help(BOOL));
    // output.insert(ModuleId::RESULT, deserialize_help(RESULT));
    // output.insert(ModuleId::LIST, deserialize_help(LIST));
    // output.insert(ModuleId::STR, deserialize_help(STR));
    // output.insert(ModuleId::DICT, deserialize_help(DICT));
    // output.insert(ModuleId::SET, deserialize_help(SET));
    // output.insert(ModuleId::BOX, deserialize_help(BOX));
    // output.insert(ModuleId::NUM, deserialize_help(NUM));

    output
}
