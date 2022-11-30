//! Used to load a .roc file and coordinate the compiler pipeline, including
//! parsing, type checking, and [code generation](https://en.wikipedia.org/wiki/Code_generation_(compiler)).
use bumpalo::Bump;
use roc_can::module::{ExposedByModule, TypeState};
use roc_collections::all::MutMap;
use roc_module::symbol::ModuleId;
use roc_packaging::cache::RocCacheDir;
use roc_reporting::report::{Palette, RenderTarget};
use roc_target::TargetInfo;
use std::path::PathBuf;

const SKIP_SUBS_CACHE: bool = {
    match option_env!("ROC_SKIP_SUBS_CACHE") {
        Some(s) => s.len() == 1 && s.as_bytes()[0] == b'1',
        None => false,
    }
};

pub use roc_load_internal::docs;
pub use roc_load_internal::file::{
    EntryPoint, ExecutionMode, ExpectMetadata, Expectations, LoadConfig, LoadResult, LoadStart,
    LoadedModule, LoadingProblem, MonomorphizedModule, Phase, Threading,
};

#[allow(clippy::too_many_arguments)]
fn load<'a>(
    arena: &'a Bump,
    load_start: LoadStart<'a>,
    exposed_types: ExposedByModule,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
) -> Result<LoadResult<'a>, LoadingProblem<'a>> {
    let cached_types = read_cached_types();

    roc_load_internal::file::load(
        arena,
        load_start,
        exposed_types,
        cached_types,
        roc_cache_dir,
        load_config,
    )
}

/// Load using only a single thread; used when compiling to webassembly
#[allow(clippy::too_many_arguments)]
pub fn load_single_threaded<'a>(
    arena: &'a Bump,
    load_start: LoadStart<'a>,
    exposed_types: ExposedByModule,
    target_info: TargetInfo,
    render: RenderTarget,
    palette: Palette,
    roc_cache_dir: RocCacheDir<'_>,
    exec_mode: ExecutionMode,
) -> Result<LoadResult<'a>, LoadingProblem<'a>> {
    let cached_subs = read_cached_types();

    roc_load_internal::file::load_single_threaded(
        arena,
        load_start,
        exposed_types,
        target_info,
        cached_subs,
        render,
        palette,
        exec_mode,
        roc_cache_dir,
    )
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum LoadMonomorphizedError<'a> {
    LoadingProblem(LoadingProblem<'a>),
    /// Errors in the module that should be reported, without compiling the executable.
    /// Relevant in check-and-then-build mode.
    ErrorModule(LoadedModule),
}

impl<'a> From<LoadingProblem<'a>> for LoadMonomorphizedError<'a> {
    fn from(problem: LoadingProblem<'a>) -> Self {
        Self::LoadingProblem(problem)
    }
}

// HACK only relevant because of some uses of `map_err` that decay into this error, but call `todo` -
// rustc seems to be unhappy with that.
impl<'a> From<()> for LoadMonomorphizedError<'a> {
    fn from(_: ()) -> Self {
        todo!()
    }
}

#[allow(clippy::too_many_arguments)]
pub fn load_and_monomorphize_from_str<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    src: &'a str,
    src_dir: PathBuf,
    exposed_types: ExposedByModule,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
) -> Result<MonomorphizedModule<'a>, LoadingProblem<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_str(arena, filename, src, roc_cache_dir, src_dir)?;

    match load(arena, load_start, exposed_types, roc_cache_dir, load_config)? {
        Monomorphized(module) => Ok(module),
        TypeChecked(_) => unreachable!(""),
    }
}

pub fn load_and_monomorphize<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    exposed_types: ExposedByModule,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
) -> Result<MonomorphizedModule<'a>, LoadMonomorphizedError<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_path(
        arena,
        filename,
        load_config.render,
        roc_cache_dir,
        load_config.palette,
    )?;

    match load(arena, load_start, exposed_types, roc_cache_dir, load_config)? {
        Monomorphized(module) => Ok(module),
        TypeChecked(module) => Err(LoadMonomorphizedError::ErrorModule(module)),
    }
}

pub fn load_and_typecheck<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    exposed_types: ExposedByModule,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
) -> Result<LoadedModule, LoadingProblem<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_path(
        arena,
        filename,
        load_config.render,
        roc_cache_dir,
        load_config.palette,
    )?;

    match load(arena, load_start, exposed_types, roc_cache_dir, load_config)? {
        Monomorphized(_) => unreachable!(""),
        TypeChecked(module) => Ok(module),
    }
}

#[allow(clippy::too_many_arguments)]
pub fn load_and_typecheck_str<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    source: &'a str,
    src_dir: PathBuf,
    exposed_types: ExposedByModule,
    target_info: TargetInfo,
    render: RenderTarget,
    roc_cache_dir: RocCacheDir<'_>,
    palette: Palette,
) -> Result<LoadedModule, LoadingProblem<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_str(arena, filename, source, roc_cache_dir, src_dir)?;

    // NOTE: this function is meant for tests, and so we use single-threaded
    // solving so we don't use too many threads per-test. That gives higher
    // throughput for the test run overall
    match load_single_threaded(
        arena,
        load_start,
        exposed_types,
        target_info,
        render,
        palette,
        roc_cache_dir,
        ExecutionMode::Check,
    )? {
        Monomorphized(_) => unreachable!(""),
        TypeChecked(module) => Ok(module),
    }
}

// IFTTT: crates/compiler/load/build.rs
const BOOL: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Bool.dat")) as &[_];
const DICT: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Dict.dat")) as &[_];
const SET: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Set.dat")) as &[_];
const RESULT: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Result.dat")) as &[_];
const NUM: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Num.dat")) as &[_];
const LIST: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/List.dat")) as &[_];
const STR: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Str.dat")) as &[_];
const BOX: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Box.dat")) as &[_];
const ENCODE: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Encode.dat")) as &[_];
const DECODE: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Decode.dat")) as &[_];
const HASH: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/Hash.dat")) as &[_];

fn deserialize_help(bytes: &[u8]) -> TypeState {
    let (state, _offset) = TypeState::deserialize(bytes);
    debug_assert_eq!(bytes.len(), _offset);

    state
}

fn read_cached_types() -> MutMap<ModuleId, TypeState> {
    let mut output = MutMap::default();

    // Wasm seems to re-order definitions between build time and runtime, but only in release mode.
    // That is very strange, but we can solve it separately
    if !cfg!(target_family = "wasm") && !cfg!(windows) && !SKIP_SUBS_CACHE {
        output.insert(ModuleId::BOOL, deserialize_help(BOOL));

        output.insert(ModuleId::RESULT, deserialize_help(RESULT));
        output.insert(ModuleId::NUM, deserialize_help(NUM));

        output.insert(ModuleId::LIST, deserialize_help(LIST));
        output.insert(ModuleId::STR, deserialize_help(STR));
        output.insert(ModuleId::BOX, deserialize_help(BOX));

        output.insert(ModuleId::DICT, deserialize_help(DICT));
        output.insert(ModuleId::SET, deserialize_help(SET));

        output.insert(ModuleId::ENCODE, deserialize_help(ENCODE));
        output.insert(ModuleId::DECODE, deserialize_help(DECODE));

        output.insert(ModuleId::HASH, deserialize_help(HASH));
    }

    output
}
