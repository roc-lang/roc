//! Used to load a .roc file and coordinate the compiler pipeline, including
//! parsing, type checking, and [code generation](https://en.wikipedia.org/wiki/Code_generation_(compiler)).
use bumpalo::Bump;
use roc_can::module::{ExposedByModule, TypeState};
use roc_collections::all::MutMap;
use roc_module::symbol::ModuleId;
use roc_packaging::cache::RocCacheDir;
use roc_reporting::report::{Palette, RenderTarget};
use roc_target::Target;
use std::path::PathBuf;

const SKIP_SUBS_CACHE: bool = {
    match option_env!("ROC_SKIP_SUBS_CACHE") {
        Some(s) => s.len() == 1 && s.as_bytes()[0] == b'1',
        None => false,
    }
};

pub use roc_load_internal::docs;
pub use roc_load_internal::file::{
    ExecutionMode, ExpectMetadata, LoadConfig, LoadResult, LoadStart, LoadingProblem, Phase,
    Threading,
};
pub use roc_load_internal::module::{
    CheckedModule, EntryPoint, Expectations, ExposedToHost, LoadedModule, MonomorphizedModule,
};
pub use roc_solve::FunctionKind;

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
    target: Target,
    function_kind: FunctionKind,
    render: RenderTarget,
    palette: Palette,
    roc_cache_dir: RocCacheDir<'_>,
    exec_mode: ExecutionMode,
) -> Result<LoadResult<'a>, LoadingProblem<'a>> {
    let cached_subs = read_cached_types();
    let exposed_types = ExposedByModule::default();

    roc_load_internal::file::load_single_threaded(
        arena,
        load_start,
        exposed_types,
        target,
        function_kind,
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
    opt_main_path: Option<PathBuf>,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
) -> Result<MonomorphizedModule<'a>, LoadMonomorphizedError<'a>> {
    use LoadResult::*;

    let load_start =
        LoadStart::from_str(arena, filename, opt_main_path, src, roc_cache_dir, src_dir)?;
    let exposed_types = ExposedByModule::default();

    match load(arena, load_start, exposed_types, roc_cache_dir, load_config)? {
        Monomorphized(module) => Ok(module),
        TypeChecked(module) => Err(LoadMonomorphizedError::ErrorModule(module)),
    }
}

pub fn load_and_monomorphize<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    opt_main_path: Option<PathBuf>,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
) -> Result<MonomorphizedModule<'a>, LoadMonomorphizedError<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_path(
        arena,
        filename,
        opt_main_path,
        load_config.render,
        roc_cache_dir,
        load_config.palette,
    )?;

    let exposed_types = ExposedByModule::default();

    match load(arena, load_start, exposed_types, roc_cache_dir, load_config)? {
        Monomorphized(module) => Ok(module),
        TypeChecked(module) => Err(LoadMonomorphizedError::ErrorModule(module)),
    }
}

pub fn load_and_typecheck<'a>(
    arena: &'a Bump,
    filename: PathBuf,
    opt_main_path: Option<PathBuf>,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
) -> Result<LoadedModule, LoadingProblem<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_path(
        arena,
        filename,
        opt_main_path,
        load_config.render,
        roc_cache_dir,
        load_config.palette,
    )?;

    let exposed_types = ExposedByModule::default();

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
    opt_main_path: Option<PathBuf>,
    target: Target,
    function_kind: FunctionKind,
    render: RenderTarget,
    roc_cache_dir: RocCacheDir<'_>,
    palette: Palette,
) -> Result<LoadedModule, LoadingProblem<'a>> {
    use LoadResult::*;

    let load_start = LoadStart::from_str(
        arena,
        filename,
        opt_main_path,
        source,
        roc_cache_dir,
        src_dir,
    )?;

    // NOTE: this function is meant for tests, and so we use single-threaded
    // solving so we don't use too many threads per-test. That gives higher
    // throughput for the test run overall
    match load_single_threaded(
        arena,
        load_start,
        target,
        function_kind,
        render,
        palette,
        roc_cache_dir,
        ExecutionMode::Check,
    )? {
        Monomorphized(_) => unreachable!(""),
        TypeChecked(module) => Ok(module),
    }
}

macro_rules! include_bytes_align_as {
    ($align_ty:ty, $path:expr) => {{
        // const block expression to encapsulate the static

        #[repr(C)]
        pub struct AlignedAs<Align, Bytes: ?Sized> {
            pub _align: [Align; 0],
            pub bytes: Bytes,
        }

        // this assignment is made possible by CoerceUnsized
        static ALIGNED: &AlignedAs<$align_ty, [u8]> = &AlignedAs {
            _align: [],
            bytes: *include_bytes!($path),
        };

        &ALIGNED.bytes
    }};
}

// IFTTT: crates/compiler/load/build.rs

fn deserialize_help(bytes: &[u8]) -> TypeState {
    let (state, _offset) = TypeState::deserialize(bytes);
    debug_assert_eq!(bytes.len(), _offset);

    state
}

fn read_cached_types() -> MutMap<ModuleId, TypeState> {
    let mod_bool = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Bool.dat"));
    let mod_dict = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Dict.dat"));
    let mod_set = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Set.dat"));
    let mod_result = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Result.dat"));
    let mod_num = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Num.dat"));
    let mod_list = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/List.dat"));
    let mod_str = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Str.dat"));
    let mod_box = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Box.dat"));
    let mod_encode = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Encode.dat"));
    let mod_decode = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Decode.dat"));
    let mod_hash = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Hash.dat"));
    let mod_inspect = include_bytes_align_as!(u128, concat!(env!("OUT_DIR"), "/Inspect.dat"));

    let mut output = MutMap::default();

    // Wasm seems to re-order definitions between build time and runtime, but only in release mode.
    // That is very strange, but we can solve it separately
    if !cfg!(target_family = "wasm") && !SKIP_SUBS_CACHE {
        output.insert(ModuleId::BOOL, deserialize_help(mod_bool));

        output.insert(ModuleId::RESULT, deserialize_help(mod_result));
        output.insert(ModuleId::NUM, deserialize_help(mod_num));

        output.insert(ModuleId::LIST, deserialize_help(mod_list));
        output.insert(ModuleId::STR, deserialize_help(mod_str));
        output.insert(ModuleId::BOX, deserialize_help(mod_box));

        output.insert(ModuleId::DICT, deserialize_help(mod_dict));
        output.insert(ModuleId::SET, deserialize_help(mod_set));

        output.insert(ModuleId::ENCODE, deserialize_help(mod_encode));
        output.insert(ModuleId::DECODE, deserialize_help(mod_decode));

        output.insert(ModuleId::HASH, deserialize_help(mod_hash));
        output.insert(ModuleId::INSPECT, deserialize_help(mod_inspect));
    }

    output
}
