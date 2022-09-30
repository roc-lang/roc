use memmap2::{Mmap, MmapMut};
use object::Object;
use roc_build::link::{rebuild_host, LinkType};
use roc_error_macros::internal_error;
use roc_mono::ir::OptLevel;
use std::cmp::Ordering;
use std::mem;
use std::path::Path;
use target_lexicon::Triple;

mod elf;
mod macho;
mod pe;

mod generate_dylib;
mod metadata;

pub fn supported(link_type: LinkType, target: &Triple) -> bool {
    if let LinkType::Executable = link_type {
        match target {
            Triple {
                architecture: target_lexicon::Architecture::X86_64,
                operating_system: target_lexicon::OperatingSystem::Linux,
                binary_format: target_lexicon::BinaryFormat::Elf,
                ..
            } => true,

            // macho support is incomplete
            Triple {
                operating_system: target_lexicon::OperatingSystem::Darwin,
                binary_format: target_lexicon::BinaryFormat::Macho,
                ..
            } => false,

            Triple {
                architecture: target_lexicon::Architecture::X86_64,
                operating_system: target_lexicon::OperatingSystem::Windows,
                binary_format: target_lexicon::BinaryFormat::Coff,
                ..
            } => true,

            _ => false,
        }
    } else {
        false
    }
}

pub fn build_and_preprocess_host(
    opt_level: OptLevel,
    target: &Triple,
    host_input_path: &Path,
    preprocessed_host_path: &Path,
    exposed_to_host: Vec<String>,
    exported_closure_types: Vec<String>,
) {
    let dummy_lib = if let target_lexicon::OperatingSystem::Windows = target.operating_system {
        host_input_path.with_file_name("libapp.obj")
    } else {
        host_input_path.with_file_name("libapp.so")
    };

    let dynhost = if let target_lexicon::OperatingSystem::Windows = target.operating_system {
        host_input_path.with_file_name("dynhost.exe")
    } else {
        host_input_path.with_file_name("dynhost")
    };

    generate_dynamic_lib(target, exposed_to_host, exported_closure_types, &dummy_lib);
    rebuild_host(opt_level, target, host_input_path, Some(&dummy_lib));
    let metadata = host_input_path.with_file_name("metadata");
    // let prehost = host_input_path.with_file_name("preprocessedhost");

    preprocess(
        target,
        &dynhost,
        &metadata,
        preprocessed_host_path,
        &dummy_lib,
        false,
        false,
    )
}

pub fn link_preprocessed_host(
    target: &Triple,
    host_input_path: &Path,
    roc_app_bytes: &[u8],
    binary_path: &Path,
) {
    let metadata = host_input_path.with_file_name("metadata");
    surgery(roc_app_bytes, &metadata, binary_path, false, false, target)
}

fn generate_dynamic_lib(
    target: &Triple,
    exposed_to_host: Vec<String>,
    exported_closure_types: Vec<String>,
    dummy_lib_path: &Path,
) {
    let mut custom_names = Vec::new();

    for sym in exposed_to_host {
        custom_names.extend([
            format!("roc__{}_1_exposed", sym),
            format!("roc__{}_1_exposed_generic", sym),
            format!("roc__{}_size", sym),
        ]);

        for closure_type in &exported_closure_types {
            custom_names.extend([
                format!("roc__{}_1_{}_caller", sym, closure_type),
                format!("roc__{}_1_{}_size", sym, closure_type),
                format!("roc__{}_1_{}_result_size", sym, closure_type),
            ]);
        }
    }

    // on windows (PE) binary search is used on the symbols,
    // so they must be in alphabetical order
    custom_names.sort_unstable();

    if !dummy_lib_is_up_to_date(target, dummy_lib_path, &custom_names) {
        let bytes = crate::generate_dylib::generate(target, &custom_names)
            .unwrap_or_else(|e| internal_error!("{e}"));

        std::fs::write(dummy_lib_path, &bytes).unwrap_or_else(|e| internal_error!("{e}"))
    }
}

fn object_matches_target<'a>(target: &Triple, object: &object::File<'a, &'a [u8]>) -> bool {
    use target_lexicon::{Architecture as TLA, OperatingSystem as TLO};

    match target.architecture {
        TLA::X86_64 => {
            if object.architecture() != object::Architecture::X86_64 {
                return false;
            }

            let target_format = match target.operating_system {
                TLO::Linux => object::BinaryFormat::Elf,
                TLO::Windows => object::BinaryFormat::Pe,
                _ => todo!("surgical linker does not support target {:?}", target),
            };

            object.format() == target_format
        }
        TLA::Aarch64(_) => object.architecture() == object::Architecture::Aarch64,
        _ => todo!("surgical linker does not support target {:?}", target),
    }
}

/// Checks whether the dummy `.dll/.so` is up to date, in other words that it exports exactly the
/// symbols that it is supposed to export, and is built for the right target. If this is the case,
/// we can skip rebuildingthe dummy lib.
fn dummy_lib_is_up_to_date(
    target: &Triple,
    dummy_lib_path: &Path,
    custom_names: &[String],
) -> bool {
    if !std::path::Path::exists(dummy_lib_path) {
        return false;
    }

    let dummy_lib = open_mmap(dummy_lib_path);
    let object = object::File::parse(&*dummy_lib).unwrap();

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

/// Constructs a `metadata::Metadata` from a host executable binary, and writes it to disk
fn preprocess(
    target: &Triple,
    host_exe_path: &Path,
    metadata_path: &Path,
    preprocessed_path: &Path,
    shared_lib: &Path,
    verbose: bool,
    time: bool,
) {
    if verbose {
        println!("Targeting: {}", target);
    }

    let endianness = target
        .endianness()
        .unwrap_or(target_lexicon::Endianness::Little);

    match target.binary_format {
        target_lexicon::BinaryFormat::Elf => {
            crate::elf::preprocess_elf(
                endianness,
                host_exe_path,
                metadata_path,
                preprocessed_path,
                shared_lib,
                verbose,
                time,
            );
        }

        target_lexicon::BinaryFormat::Macho => {
            crate::macho::preprocess_macho(
                target,
                host_exe_path,
                metadata_path,
                preprocessed_path,
                shared_lib,
                verbose,
                time,
            );
        }

        target_lexicon::BinaryFormat::Coff => {
            crate::pe::preprocess_windows(
                host_exe_path,
                metadata_path,
                preprocessed_path,
                verbose,
                time,
            )
            .unwrap_or_else(|e| internal_error!("{}", e));
        }

        target_lexicon::BinaryFormat::Wasm => {
            todo!("Roc does not yet support web assembly hosts!");
        }
        target_lexicon::BinaryFormat::Unknown => {
            internal_error!("Roc does not support unknown host binary formats!");
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
    target: &Triple,
) {
    match target.binary_format {
        target_lexicon::BinaryFormat::Elf => {
            crate::elf::surgery_elf(roc_app_bytes, metadata_path, executable_path, verbose, time);
        }

        target_lexicon::BinaryFormat::Macho => {
            crate::macho::surgery_macho(
                roc_app_bytes,
                metadata_path,
                executable_path,
                verbose,
                time,
            );
        }

        target_lexicon::BinaryFormat::Coff => {
            crate::pe::surgery_pe(executable_path, metadata_path, roc_app_bytes);
        }

        target_lexicon::BinaryFormat::Wasm => {
            todo!("Roc does not yet support web assembly hosts!");
        }
        target_lexicon::BinaryFormat::Unknown => {
            internal_error!("Roc does not support unknown host binary formats!");
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
        .unwrap_or_else(|e| internal_error!("{e}"));

    unsafe { Mmap::map(&in_file).unwrap_or_else(|e| internal_error!("{e}")) }
}

pub(crate) fn open_mmap_mut(path: &Path, length: usize) -> MmapMut {
    let out_file = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(path)
        .unwrap_or_else(|e| internal_error!("{e}"));
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
