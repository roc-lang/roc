//! Surgical linker that links platforms to Roc applications. We created our own
//! linker for performance, since regular linkers add complexity that is not
//! needed for linking Roc apps. Because we want `roc` to manage the build
//! system and final linking of the executable, it is significantly less
//! practical to use a regular linker.
use memmap2::{Mmap, MmapMut};
use object::Object;
use roc_error_macros::internal_error;
use roc_load::ExposedToHost;
use roc_module::symbol::Interns;
use roc_target::{Architecture, OperatingSystem, Target};
use std::cmp::Ordering;
use std::mem;
use std::path::{Path, PathBuf};

mod elf;
mod macho;
mod pe;
mod util;

mod generate_dylib;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LinkType {
    // These numbers correspond to the --lib and --no-link flags
    Executable = 0,
    Dylib = 1,
    None = 2,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SupportLevel {
    Full,
    Wip,
    None,
}

pub fn support_level(link_type: LinkType, target: Target) -> SupportLevel {
    if let LinkType::Executable = link_type {
        match target {
            Target::LinuxX64 => SupportLevel::Full,
            Target::WinX64 => SupportLevel::Full,
            // macho support is incomplete
            Target::MacX64 => SupportLevel::None,
            Target::MacArm64 => SupportLevel::Wip,
            _ => SupportLevel::None,
        }
    } else {
        SupportLevel::None
    }
}

pub fn link_preprocessed_host(
    target: Target,
    roc_app_bytes: &[u8],
    binary_path: &Path,
    metadata: PathBuf,
    verbose: bool,
) {
    surgery(
        roc_app_bytes,
        &metadata,
        binary_path,
        verbose,
        false,
        target,
    )
}

pub fn generate_stub_lib_from_loaded(
    target: Target,
    platform_main_roc: &Path,
    stub_dll_symbols: &[String],
) -> PathBuf {
    let stub_lib = platform_main_roc.with_file_name(target.stub_app_lib_file_name());
    generate_dynamic_lib(target, stub_dll_symbols, stub_lib.as_path());

    stub_lib
}

pub struct ExposedSymbols {
    // usually just `mainForhost`
    pub top_level_values: Vec<String>,

    // old type exposing mechanism
    pub exported_closure_types: Vec<String>,
}

impl ExposedSymbols {
    pub fn from_exposed_to_host(interns: &Interns, exposed_to_host: &ExposedToHost) -> Vec<String> {
        let mut custom_names = Vec::new();

        for x in exposed_to_host.top_level_values.keys() {
            let sym = x.as_unsuffixed_str(interns);

            custom_names.extend([
                format!("roc__{sym}_1_exposed"),
                format!("roc__{sym}_1_exposed_generic"),
                format!("roc__{sym}_1_exposed_size"),
            ]);

            let exported_closure_types = exposed_to_host.closure_types.iter().map(|x| {
                format!(
                    "{}_{}",
                    x.module_string(interns),
                    x.as_unsuffixed_str(interns)
                )
            });

            for (i, _) in exported_closure_types.enumerate() {
                custom_names.extend([
                    format!("roc__{sym}_{i}_caller"),
                    format!("roc__{sym}_{i}_size"),
                    format!("roc__{sym}_{i}_result_size"),
                ]);
            }
        }

        for x in &exposed_to_host.getters {
            let sym = x.as_unsuffixed_str(interns);
            custom_names.extend([
                sym.to_string(),
                format!("{sym}_generic"),
                format!("{sym}_size"),
            ]);
        }

        for (top_level_value, lambda_set_id) in &exposed_to_host.lambda_sets {
            let sym = top_level_value.as_unsuffixed_str(interns);
            let id = lambda_set_id.0;
            custom_names.extend([format!("roc__{sym}_{id}_caller")]);
        }

        // on windows (PE) binary search is used on the symbols,
        // so they must be in alphabetical order
        custom_names.sort_unstable();

        custom_names
    }

    pub fn stub_dll_symbols(&self) -> Vec<String> {
        let mut custom_names = Vec::new();

        for sym in &self.top_level_values {
            custom_names.extend([
                format!("roc__{sym}_1_exposed"),
                format!("roc__{sym}_1_exposed_generic"),
                format!("roc__{sym}_size"),
            ]);

            for closure_type in &self.exported_closure_types {
                custom_names.extend([
                    format!("roc__{sym}_1_{closure_type}_caller"),
                    format!("roc__{sym}_1_{closure_type}_size"),
                    format!("roc__{sym}_1_{closure_type}_result_size"),
                ]);
            }
        }

        // on windows (PE) binary search is used on the symbols,
        // so they must be in alphabetical order
        custom_names.sort_unstable();

        custom_names
    }
}

fn generate_dynamic_lib(target: Target, stub_dll_symbols: &[String], stub_lib_path: &Path) {
    if !stub_lib_is_up_to_date(target, stub_lib_path, stub_dll_symbols) {
        let bytes = crate::generate_dylib::generate(target, stub_dll_symbols)
            .unwrap_or_else(|e| internal_error!("{e}"));

        if let Err(e) = std::fs::write(stub_lib_path, bytes) {
            internal_error!("failed to write stub lib to {:?}: {e}", stub_lib_path)
        }

        if target.operating_system() == OperatingSystem::Windows {
            generate_import_library(stub_lib_path, stub_dll_symbols);
        }
    }
}

fn generate_import_library(stub_lib_path: &Path, custom_names: &[String]) {
    let def_file_content = generate_def_file(custom_names).expect("write to string never fails");

    let mut def_path = stub_lib_path.to_owned();
    def_path.set_extension("def");

    if let Err(e) = std::fs::write(&def_path, def_file_content.as_bytes()) {
        internal_error!("failed to write import library to {:?}: {e}", def_path)
    }

    let mut def_filename = PathBuf::from(generate_dylib::APP_DLL);
    def_filename.set_extension("def");

    let mut lib_filename = PathBuf::from(generate_dylib::APP_DLL);
    lib_filename.set_extension("lib");

    let zig = std::env::var("ROC_ZIG").unwrap_or_else(|_| "zig".into());

    // use zig to generate the .lib file. Here is a good description of what is in an import library
    //
    // > https://www.codeproject.com/Articles/1253835/The-Structure-of-import-Library-File-lib
    //
    // For when we want to do this in-memory in the future. We can also consider using
    //
    // > https://github.com/messense/implib-rs
    let output = std::process::Command::new(zig)
        .current_dir(stub_lib_path.parent().unwrap())
        .args([
            "dlltool",
            "-d",
            def_filename.to_str().unwrap(),
            "-m",
            "i386:x86-64",
            "-D",
            generate_dylib::APP_DLL,
            "-l",
            lib_filename.to_str().unwrap(),
        ])
        .output()
        .unwrap();

    if !output.status.success() {
        use std::io::Write;

        std::io::stdout().write_all(&output.stdout).unwrap();
        std::io::stderr().write_all(&output.stderr).unwrap();

        panic!("zig dlltool failed");
    }
}

fn generate_def_file(custom_names: &[String]) -> Result<String, std::fmt::Error> {
    use std::fmt::Write;

    let mut def_file = String::new();

    writeln!(def_file, "LIBRARY libapp")?;
    writeln!(def_file, "EXPORTS")?;

    for (i, name) in custom_names.iter().enumerate() {
        // 1-indexed of course...
        let index = i + 1;

        writeln!(def_file, "    {name} @{index}")?;
    }

    Ok(def_file)
}

fn object_matches_target<'a>(target: Target, object: &object::File<'a, &'a [u8]>) -> bool {
    match target.architecture() {
        Architecture::X86_64 => {
            if object.architecture() != object::Architecture::X86_64 {
                return false;
            }

            let target_format = match target.operating_system() {
                OperatingSystem::Linux => object::BinaryFormat::Elf,
                OperatingSystem::Windows => object::BinaryFormat::Pe,
                _ => todo!("surgical linker does not support target {:?}", target),
            };

            object.format() == target_format
        }
        Architecture::Aarch64 => object.architecture() == object::Architecture::Aarch64,
        _ => todo!("surgical linker does not support target {:?}", target),
    }
}

/// Checks whether the stub `.dll/.so` is up to date, in other words that it exports exactly the
/// symbols that it is supposed to export, and is built for the right target. If this is the case,
/// we can skip rebuildingthe stub lib.
fn stub_lib_is_up_to_date(target: Target, stub_lib_path: &Path, custom_names: &[String]) -> bool {
    if !std::path::Path::exists(stub_lib_path) {
        return false;
    }

    let stub_lib = open_mmap(stub_lib_path);
    let object = object::File::parse(&*stub_lib).unwrap();

    // the user may have been cross-compiling.
    // The dynhost on disk must match our current target
    if !object_matches_target(target, &object) {
        return false;
    }

    // we made this dynhost file. For the file to be the same as what we'd generate,
    // we need all symbols to be there and in the correct order
    let dynamic_symbols: Vec<_> = object.exports().unwrap();

    let it1 = dynamic_symbols.iter().map(|e| e.name());
    let it2 = custom_names.iter().map(|s| s.as_bytes());

    it1.eq(it2)
}

/// Constructs a `Metadata` from a host executable binary, and writes it to disk
#[allow(clippy::too_many_arguments)]
pub fn preprocess_host(
    target: Target,
    host_exe_path: &Path,
    metadata_path: &Path,
    preprocessed_path: &Path,
    shared_lib: &Path,
    verbose: bool,
    time: bool,
) {
    if verbose {
        println!("Targeting: {target}");
    }

    match target.arch_os() {
        (_, OperatingSystem::Linux) => {
            crate::elf::preprocess_elf_le(
                host_exe_path,
                metadata_path,
                preprocessed_path,
                shared_lib,
                verbose,
                time,
            );
        }

        (_, OperatingSystem::Mac) => {
            crate::macho::preprocess_macho_le(
                target.architecture(),
                host_exe_path,
                metadata_path,
                preprocessed_path,
                shared_lib,
                verbose,
                time,
            );
        }

        (_, OperatingSystem::Windows) => {
            crate::pe::preprocess_windows(
                host_exe_path,
                metadata_path,
                preprocessed_path,
                shared_lib,
                verbose,
                time,
            )
            .unwrap_or_else(|e| internal_error!("{}", e));
        }

        (Architecture::Wasm32, _) => {
            todo!("Roc does not yet support web assembly hosts!");
        }
        other => {
            internal_error!(
                concat!(
                    r"Roc does not yet support the {:?} binary format. ",
                    r"Please file a bug report for this, describing what operating system you were targeting!"
                ),
                other,
            )
        }
    }
}

fn surgery(
    roc_app_bytes: &[u8],
    metadata_path: &Path,
    executable_path: &Path,
    verbose: bool,
    time: bool,
    target: Target,
) {
    match target.arch_os() {
        (_, OperatingSystem::Linux) => {
            crate::elf::surgery_elf(roc_app_bytes, metadata_path, executable_path, verbose, time);
        }

        (_, OperatingSystem::Mac) => {
            crate::macho::surgery_macho(
                roc_app_bytes,
                metadata_path,
                executable_path,
                verbose,
                time,
            );
        }

        (_, OperatingSystem::Windows) => {
            crate::pe::surgery_pe(executable_path, metadata_path, roc_app_bytes);
        }

        (Architecture::Wasm32, _) => {
            todo!("Roc does not yet support web assembly hosts!");
        }
        other => {
            internal_error!(
                concat!(
                    r"Roc does not yet support the {:?} binary format. ",
                    r"Please file a bug report for this, describing what operating system you were targeting!"
                ),
                other,
            )
        }
    }
}

pub(crate) fn align_by_constraint(offset: usize, constraint: usize) -> usize {
    if offset % constraint == 0 {
        offset
    } else {
        offset + constraint - (offset % constraint)
    }
}

pub(crate) fn align_to_offset_by_constraint(
    current_offset: usize,
    target_offset: usize,
    constraint: usize,
) -> usize {
    let target_remainder = target_offset % constraint;
    let current_remainder = current_offset % constraint;
    match target_remainder.cmp(&current_remainder) {
        Ordering::Greater => current_offset + (target_remainder - current_remainder),
        Ordering::Less => current_offset + ((target_remainder + constraint) - current_remainder),
        Ordering::Equal => current_offset,
    }
}

pub(crate) fn load_struct_inplace<T>(bytes: &[u8], offset: usize) -> &T {
    &load_structs_inplace(bytes, offset, 1)[0]
}

pub(crate) fn load_struct_inplace_mut<T>(bytes: &mut [u8], offset: usize) -> &mut T {
    &mut load_structs_inplace_mut(bytes, offset, 1)[0]
}

pub(crate) fn load_structs_inplace<T>(bytes: &[u8], offset: usize, count: usize) -> &[T] {
    let (head, body, tail) =
        unsafe { bytes[offset..offset + count * mem::size_of::<T>()].align_to::<T>() };
    assert!(head.is_empty(), "Data was not aligned");
    assert_eq!(count, body.len(), "Failed to load all structs");
    assert!(tail.is_empty(), "End of data was not aligned");
    body
}

pub(crate) fn load_structs_inplace_mut<T>(
    bytes: &mut [u8],
    offset: usize,
    count: usize,
) -> &mut [T] {
    let (head, body, tail) =
        unsafe { bytes[offset..offset + count * mem::size_of::<T>()].align_to_mut::<T>() };
    assert!(head.is_empty(), "Data was not aligned");
    assert_eq!(count, body.len(), "Failed to load all structs");
    assert!(tail.is_empty(), "End of data was not aligned");
    body
}

pub(crate) fn open_mmap(path: &Path) -> Mmap {
    let in_file = std::fs::OpenOptions::new()
        .read(true)
        .open(path)
        .unwrap_or_else(|e| internal_error!("failed to open file {path:?}: {e}"));

    unsafe { Mmap::map(&in_file).unwrap_or_else(|e| internal_error!("{e}")) }
}

pub(crate) fn open_mmap_mut(path: &Path, length: usize) -> MmapMut {
    let out_file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .truncate(false)
        .open(path)
        .unwrap_or_else(|e| internal_error!("failed to create or open file {path:?}: {e}"));
    out_file
        .set_len(length as u64)
        .unwrap_or_else(|e| internal_error!("{e}"));

    unsafe { MmapMut::map_mut(&out_file).unwrap_or_else(|e| internal_error!("{e}")) }
}

/// # dbg_hex
/// display dbg result in hexadecimal `{:#x?}` format.
#[macro_export]
macro_rules! dbg_hex {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
        eprintln!("[{}:{}]", file!(), line!());
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                eprintln!("[{}:{}] {} = {:#x?}",
                    file!(), line!(), stringify!($val), &tmp);
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($($crate::dbg_hex!($val)),+,)
    };
}

// These functions don't end up in the final Roc binary but Windows linker needs a definition inside the crate.
// On Windows, there seems to be less dead-code-elimination than on Linux or MacOS, or maybe it's done later.
#[cfg(test)]
#[cfg(windows)]
#[allow(unused_imports)]
use windows_roc_platform_functions::*;

#[cfg(test)]
#[cfg(windows)]
mod windows_roc_platform_functions {
    use core::ffi::c_void;

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
        libc::malloc(size)
    }

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_realloc(
        c_ptr: *mut c_void,
        new_size: usize,
        _old_size: usize,
        _alignment: u32,
    ) -> *mut c_void {
        libc::realloc(c_ptr, new_size)
    }

    /// # Safety
    /// The Roc application needs this.
    #[no_mangle]
    pub unsafe fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
        libc::free(c_ptr)
    }
}
