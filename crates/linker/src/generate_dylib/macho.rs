use object::write;
use object::{Architecture, BinaryFormat, Endianness, SymbolFlags, SymbolKind, SymbolScope};
use roc_error_macros::internal_error;
use std::path::Path;
use std::process::Command;
use target_lexicon::Triple;

use crate::pe::next_multiple_of;

// TODO: Eventually do this from scratch and in memory instead of with ld.
#[allow(dead_code)]
pub fn create_dylib_macho_old(
    custom_names: &[String],
    triple: &Triple,
) -> object::read::Result<Vec<u8>> {
    let dummy_obj_file = tempfile::Builder::new()
        .prefix("roc_lib")
        .suffix(".o")
        .tempfile()
        .unwrap_or_else(|e| internal_error!("{}", e));
    let tmp = tempfile::tempdir().unwrap_or_else(|e| internal_error!("{}", e));
    let dummy_lib_file = tmp.path().to_path_buf().with_file_name("libapp.so");

    let obj_target = BinaryFormat::MachO;
    let obj_arch = match triple.architecture {
        target_lexicon::Architecture::X86_64 => Architecture::X86_64,
        target_lexicon::Architecture::Aarch64(_) => Architecture::Aarch64,
        _ => {
            // We should have verified this via supported() before calling this function
            unreachable!()
        }
    };
    let mut out_object = write::Object::new(obj_target, obj_arch, Endianness::Little);

    let text_section = out_object.section_id(write::StandardSection::Text);

    for name in custom_names {
        out_object.add_symbol(write::Symbol {
            name: name.as_bytes().to_vec(),
            value: 0,
            size: 0,
            kind: SymbolKind::Text,
            scope: SymbolScope::Dynamic,
            weak: false,
            section: write::SymbolSection::Section(text_section),
            flags: SymbolFlags::None,
        });
    }

    std::fs::write(
        dummy_obj_file.path(),
        out_object.write().expect("failed to build output object"),
    )
    .expect("failed to write object to file");

    // This path only exists on macOS Big Sur, and it causes ld errors
    // on Catalina if it's specified with -L, so we replace it with a
    // redundant -lSystem if the directory isn't there.
    let big_sur_path = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib";
    let big_sur_fix = if Path::new(big_sur_path).exists() {
        "-L/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/lib"
    } else {
        "-lSystem" // We say -lSystem twice in the case of non-Big-Sur OSes, but it's fine.
    };

    let ld_flag_soname = "-install_name";
    let ld_prefix_args = [big_sur_fix, "-lSystem", "-dylib"];

    let output = Command::new("ld")
        .args(ld_prefix_args)
        .args([
            ld_flag_soname,
            dummy_lib_file.file_name().unwrap().to_str().unwrap(),
            dummy_obj_file.path().to_str().unwrap(),
            "-o",
            dummy_lib_file.to_str().unwrap(),
        ])
        .output()
        .unwrap();

    // Extend the lifetime of the tempfile so it doesn't get dropped
    // (and thus deleted) before the linker process is done using it!
    let _ = dummy_obj_file;

    if !output.status.success() {
        match std::str::from_utf8(&output.stderr) {
            Ok(stderr) => panic!(
                "Failed to link dummy shared library - stderr of the `ld` command was:\n{}",
                stderr
            ),
            Err(utf8_err) => panic!(
                "Failed to link dummy shared library  - stderr of the `ld` command was invalid utf8 ({:?})",
                utf8_err
            ),
        }
    }

    Ok(std::fs::read(dummy_lib_file).expect("Failed to load dummy library"))
}

pub fn create_dylib_macho(
    custom_names: &[String],
    triple: &Triple,
) -> object::read::Result<Vec<u8>> {
    // exported symbols always start with a `_` in Mach-O
    let prefixed_custom_names: Vec<_> = custom_names.iter().map(|s| format!("_{s}")).collect();
    let symbols = prefixed_custom_names.as_slice();

    let mut bytes = Vec::new();

    // keep updated by hand. We currently have 5  segment commands, and 6 others
    const COMMAND_COUNT: u32 = 5 + 6;

    // we don't know the value of the cmdsize up-front. It is the total number of bytes occupied
    // by load commands. So we put in a placeholder for now, and patch it up later.
    let commands = Commands {
        count: COMMAND_COUNT,
        size: 0x00,
    };

    let placeholder_header = macho_dylib_header(triple, commands);
    bytes.extend_from_slice(placeholder_header.as_slice());

    let start_of_cmds = bytes.len();

    //

    let command = SegmentCommand64 {
        cmd: 0x19,
        cmdsize: 0x48,
        segname: bstring16("__PAGEZERO"),
        vmaddr: 0,
        vmsize: 0x0000000100000000,
        fileoff: 0,
        filesize: 0,
        maxprot: 0,
        initprot: 0,
        nsects: 0,
        flags: 0,
    };

    bytes.extend(command.with_segments(&[]));

    //

    let command = SegmentCommand64 {
        cmd: 0x19,
        cmdsize: 0x48,
        segname: bstring16("__TEXT"),
        vmaddr: 0x0000000100000000,
        vmsize: 0x1000,
        fileoff: 0,
        filesize: 0x1000,
        maxprot: 5,  // TODO why?
        initprot: 5, // TODO why?
        nsects: 0,
        flags: 0,
    };

    bytes.extend(command.with_segments(&[
        Section64 {
            sectname: bstring16("__text"),
            segname: bstring16("__TEXT"),
            addr: 0x0000000100000ff1,
            size: 0,
            offset: 0xff1,
            align: 0,
            reloff: 0,
            nreloc: 0,
            flags: 0x80000400,
            reserved1: 0,
            reserved2: 0,
            reserved3: 0,
        },
        Section64 {
            sectname: bstring16("__stubs"),
            segname: bstring16("__TEXT"),
            addr: 0x0000000100000ff1,
            size: 0,
            offset: 0xff1,
            align: 0,
            reloff: 0,
            nreloc: 0,
            flags: 0x80000408,
            reserved1: 0,
            reserved2: 6, // random, but makes the diff equal
            reserved3: 0,
        },
        Section64 {
            sectname: bstring16("__stub_helper"),
            segname: bstring16("__TEXT"),
            addr: 0x0000000100000ff1,
            size: 0xf,
            offset: 0xff1,
            align: 0,
            reloff: 0,
            nreloc: 0,
            flags: 0x80000400,
            reserved1: 0,
            reserved2: 0,
            reserved3: 0,
        },
    ]));

    //

    let command = SegmentCommand64 {
        cmd: 0x19,
        cmdsize: 0x48,
        segname: bstring16("__DATA_CONST"),
        vmaddr: 0x0000000100001000,
        vmsize: 0x1000,
        fileoff: 0x1000,
        filesize: 0x1000,
        maxprot: 3,
        initprot: 3,
        nsects: 0,
        flags: 0,
    };

    bytes.extend(command.with_segments(&[Section64 {
        sectname: bstring16("__got"),
        segname: bstring16("__DATA_CONST"),
        addr: 0x0000000100001000,
        size: 0x8,
        offset: 0x1000,
        align: 3,
        reloff: 0,
        nreloc: 0,
        flags: 0x6,
        reserved1: 0,
        reserved2: 0,
        reserved3: 0,
    }]));

    //

    let command = SegmentCommand64 {
        cmd: 0x19,
        cmdsize: 0x48,
        segname: bstring16("__DATA"),
        vmaddr: 0x0000000100002000,
        vmsize: 0x1000,
        fileoff: 0x2000,
        filesize: 0x1000,
        maxprot: 3,
        initprot: 3,
        nsects: 0,
        flags: 0,
    };

    bytes.extend(command.with_segments(&[
        Section64 {
            sectname: bstring16("__la_symbol_ptr"),
            segname: bstring16("__DATA"),
            addr: 0x0000000100002000,
            size: 0x00,
            offset: 0x2000,
            align: 3,
            reloff: 0,
            nreloc: 0,
            flags: 0x07,
            reserved1: 0x1,
            reserved2: 0,
            reserved3: 0,
        },
        Section64 {
            sectname: bstring16("__data"),
            segname: bstring16("__DATA"),
            addr: 0x0000000100002000,
            size: 0x8,
            offset: 0x2000,
            align: 3,
            reloff: 0,
            nreloc: 0,
            flags: 0,
            reserved1: 0,
            reserved2: 0,
            reserved3: 0,
        },
        Section64 {
            sectname: bstring16("__thread_vars"),
            segname: bstring16("__DATA"),
            addr: 0x0000000100002008,
            size: 0,
            offset: 0x2008,
            align: 3,
            reloff: 0,
            nreloc: 0,
            flags: 0x13,
            reserved1: 0,
            reserved2: 0,
            reserved3: 0,
        },
        Section64 {
            sectname: bstring16("__thread_data"),
            segname: bstring16("__DATA"),
            addr: 0x0000000100002008,
            size: 0,
            offset: 0x2008,
            align: 3,
            reloff: 0,
            nreloc: 0,
            flags: 0x11,
            reserved1: 0,
            reserved2: 0,
            reserved3: 0,
        },
        Section64 {
            sectname: bstring16("__thread_bss"),
            segname: bstring16("__DATA"),
            addr: 0x0000000100002008,
            size: 0x0,
            offset: 0,
            align: 3,
            reloff: 0,
            nreloc: 0,
            flags: 0x12,
            reserved1: 0,
            reserved2: 0,
            reserved3: 0,
        },
        Section64 {
            sectname: bstring16("__bss"),
            segname: bstring16("__DATA"),
            addr: 0x0000000100002008,
            size: 0x0,
            offset: 0,
            align: 3,
            reloff: 0,
            nreloc: 0,
            flags: 0x01,
            reserved1: 0,
            reserved2: 0,
            reserved3: 0,
        },
    ]));

    //

    let command = SegmentCommand64 {
        cmd: 0x19,
        cmdsize: 0x48,
        segname: bstring16("__LINKEDIT"),
        vmaddr: 0x0000000100003000,
        vmsize: 0x1000,
        fileoff: 0x3000,
        filesize: 0x08bc,
        maxprot: 1,
        initprot: 1,
        nsects: 0,
        flags: 0,
    };

    bytes.extend(command.with_segments(&[]));

    let mut trie_symbols: Vec<&str> = prefixed_custom_names.iter().map(|s| s.as_str()).collect();
    let trie = crate::generate_dylib::export_trie::build(&mut trie_symbols);

    let export_size = trie.len() as u32;

    let dyld_info_only = DyldCommand {
        rebase_off: 0x3000,
        rebase_size: 0x8,
        bind_off: 0x3008,
        bind_size: 0x18,
        weak_bind_off: 0x00,
        weak_bind_size: 0x00,
        lazy_bind_off: 0x3020,
        lazy_bind_size: 0x00,
        export_off: 0x3020,
        export_size,
    };

    bytes.extend(dyld_info_only.to_bytes());

    let symbol_table = trivial_symbol_table(symbols.iter().map(|s| s.as_str()));
    let string_table = trivial_string_table(symbols.iter().map(|s| s.as_str()));

    let string_table_len = string_table.len();

    let (symbol_table_start, string_table_start, linkedit_section) = {
        let mut bytes = Vec::new();

        // rebase
        bytes.extend(std::iter::repeat(0).take(0x8));

        // binding

        let a = bytes.len();
        bytes.extend([0x11, 0x51, 0x40]);
        bytes.extend(b"dyld_stub_binder\0");
        bytes.extend([0x72, 0x00, 0x90, 0x00]);
        assert_eq!(bytes.len() - a, 0x18);

        // export trie
        bytes.extend(trie);

        bytes.extend(0x1ff1u64.to_le_bytes()); // TODO what is this?
        let symbol_table_start = 0x3000 + bytes.len();
        bytes.extend(symbol_table);

        // the number of exported symbols
        // TODO make dynamic
        bytes.extend(0x19u32.to_le_bytes());

        let string_table_start = 0x3000 + bytes.len();
        bytes.extend(string_table);

        (symbol_table_start, string_table_start, bytes)
    };

    let symtab = SymtabCommand {
        symoff: symbol_table_start as u32,
        nsyms: symbols.len() as u32,
        stroff: string_table_start as u32,
        strsize: string_table_len as u32,
    };

    bytes.extend(symtab.to_bytes());

    let dysym = DySymTabCommand {
        ilocalsym: 0,
        nlocalsym: 0,
        // the first N symbols are externally defined
        iextdefsym: 0,
        nextdefsym: symbols.len() as u32, // was 0x19
        // we have no undefined symbols
        iundefsym: 0,
        nundefsym: 0,
        tocoff: 0,
        ntoc: 0,
        modtaboff: 0,
        nmodtab: 0,
        extrefsymoff: 0,
        nextrefsyms: 0,
        indirectsymoff: 0,
        nindirectsyms: 0,
        extreloff: 0,
        nextrel: 0,
        locreloff: 0,
        nlocrel: 0,
    };

    bytes.extend(dysym.to_bytes());

    bytes.extend(load_dylinker_command("/usr/lib/dyld"));

    bytes.extend(dylib_id_command("librocthing.dylib", 0x2, 0x10000, 0x10000));

    bytes.extend(dylib_load_command(
        "/usr/lib/libSystem.B.dylib",
        0x2,
        0x05016401,
        0x10000,
    ));

    // we've now written all commands, and can patch the size of the commands
    let end_of_cmds = bytes.len();

    let commands = Commands {
        count: COMMAND_COUNT,
        size: (end_of_cmds - start_of_cmds) as u32,
    };

    let complete_header = macho_dylib_header(triple, commands);
    bytes[..complete_header.len()].copy_from_slice(complete_header.as_slice());

    let delta = 0x3000 - bytes.len();
    bytes.extend(std::iter::repeat(0).take(delta));

    bytes.extend(linkedit_section);

    Ok(bytes)
}

struct Commands {
    count: u32,
    size: u32,
}

fn macho_dylib_header(triple: &Triple, commands: Commands) -> [u8; 8 * 4] {
    let mut buffer = [0u8; 8 * 4];

    let (cpu_type, cpu_subtype) = match triple.architecture {
        target_lexicon::Architecture::X86_64 => (
            mach_object::CPU_TYPE_X86_64,
            mach_object::CPU_SUBTYPE_I386_ALL,
        ),
        target_lexicon::Architecture::Aarch64(_) => (
            mach_object::CPU_TYPE_ARM64,
            mach_object::CPU_SUBTYPE_ARM_ALL,
        ),
        _ => unreachable!(),
    };

    let flags: u32 = mach_object::MH_NOUNDEFS
        | mach_object::MH_DYLDLINK
        | mach_object::MH_TWOLEVEL
        | mach_object::MH_NO_REEXPORTED_DYLIBS
        | mach_object::MH_PIE
        | mach_object::MH_HAS_TLV_DESCRIPTORS;

    buffer[0..][..4].copy_from_slice(&mach_object::MH_MAGIC_64.to_le_bytes());
    buffer[4..][..4].copy_from_slice(&cpu_type.to_le_bytes());
    buffer[8..][..4].copy_from_slice(&cpu_subtype.to_le_bytes());
    buffer[12..][..4].copy_from_slice(&mach_object::MH_DYLIB.to_le_bytes());
    buffer[16..][..4].copy_from_slice(&commands.count.to_le_bytes());
    buffer[20..][..4].copy_from_slice(&commands.size.to_le_bytes());
    buffer[24..][..4].copy_from_slice(&flags.to_le_bytes());
    buffer[28..][..4].copy_from_slice(&0u32.to_le_bytes());

    buffer
}

fn bstring16(string: &str) -> [u8; 16] {
    let mut result = [0; 16];

    result[..string.len()].copy_from_slice(string.as_bytes());

    result
}

#[repr(C)]
#[derive(Default)]
struct Section64 {
    sectname: [u8; 16],
    segname: [u8; 16],
    addr: u64,
    size: u64,
    offset: u32,
    align: u32,
    reloff: u32,
    nreloc: u32,
    flags: u32,
    reserved1: u32,
    reserved2: u32,
    reserved3: u32,
}

impl Section64 {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buffer = Vec::new();

        buffer.extend(self.sectname);
        buffer.extend(self.segname);

        buffer.extend(self.addr.to_le_bytes());
        buffer.extend(self.size.to_le_bytes());
        buffer.extend(self.offset.to_le_bytes());
        buffer.extend(self.align.to_le_bytes());
        buffer.extend(self.reloff.to_le_bytes());
        buffer.extend(self.nreloc.to_le_bytes());
        buffer.extend(self.flags.to_le_bytes());
        buffer.extend(self.reserved1.to_le_bytes());
        buffer.extend(self.reserved2.to_le_bytes());
        buffer.extend(self.reserved3.to_le_bytes());

        buffer
    }
}

#[repr(C)]
#[derive(Default, Clone, Copy)]
struct DyldCommand {
    rebase_off: u32,
    rebase_size: u32,
    bind_off: u32,
    bind_size: u32,
    weak_bind_off: u32,
    weak_bind_size: u32,
    lazy_bind_off: u32,
    lazy_bind_size: u32,
    export_off: u32,
    export_size: u32,
}

impl DyldCommand {
    fn to_bytes(self) -> Vec<u8> {
        let mut buffer = Vec::new();

        buffer.extend(0x80000022u32.to_le_bytes());
        buffer.extend(0x30u32.to_le_bytes());

        buffer.extend(self.rebase_off.to_le_bytes());
        buffer.extend(self.rebase_size.to_le_bytes());
        buffer.extend(self.bind_off.to_le_bytes());
        buffer.extend(self.bind_size.to_le_bytes());
        buffer.extend(self.weak_bind_off.to_le_bytes());
        buffer.extend(self.weak_bind_size.to_le_bytes());
        buffer.extend(self.lazy_bind_off.to_le_bytes());
        buffer.extend(self.lazy_bind_size.to_le_bytes());
        buffer.extend(self.export_off.to_le_bytes());
        buffer.extend(self.export_size.to_le_bytes());

        buffer
    }
}

#[repr(C)]
#[derive(Default)]
struct SegmentCommand64 {
    cmd: u32,
    cmdsize: u32,
    segname: [u8; 16],
    vmaddr: u64,
    vmsize: u64,
    fileoff: u64,
    filesize: u64,
    maxprot: u32,
    initprot: u32,
    nsects: u32,
    flags: u32,
}

const _X: () = assert!(std::mem::size_of::<SegmentCommand64>() == 0x48);

impl SegmentCommand64 {
    fn with_segments(&self, sections: &[Section64]) -> Vec<u8> {
        let mut buffer = Vec::new();

        let cmdsize = std::mem::size_of::<SegmentCommand64>()
            + std::mem::size_of::<Section64>() * sections.len();

        // Write the header
        buffer.extend(self.cmd.to_le_bytes());
        buffer.extend((cmdsize as u32).to_le_bytes());
        buffer.extend(self.segname);

        buffer.extend(self.vmaddr.to_le_bytes());
        buffer.extend(self.vmsize.to_le_bytes());
        buffer.extend(self.fileoff.to_le_bytes());
        buffer.extend(self.filesize.to_le_bytes());

        buffer.extend(self.maxprot.to_le_bytes());
        buffer.extend(self.initprot.to_le_bytes());

        buffer.extend((sections.len() as u32).to_le_bytes());
        buffer.extend(self.flags.to_le_bytes());

        for section in sections {
            buffer.extend(section.to_bytes());
        }

        buffer
    }
}

struct DySymTabCommand {
    ilocalsym: u32,
    nlocalsym: u32,
    iextdefsym: u32,
    nextdefsym: u32,
    iundefsym: u32,
    nundefsym: u32,
    tocoff: u32,
    ntoc: u32,
    modtaboff: u32,
    nmodtab: u32,
    extrefsymoff: u32,
    nextrefsyms: u32,
    indirectsymoff: u32,
    nindirectsyms: u32,
    extreloff: u32,
    nextrel: u32,
    locreloff: u32,
    nlocrel: u32,
}

impl DySymTabCommand {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buffer = Vec::new();
        let size = 4 + 4 + std::mem::size_of::<Self>();

        buffer.extend(mach_object::LC_DYSYMTAB.to_le_bytes());
        buffer.extend((size as u32).to_le_bytes());

        buffer.extend(self.ilocalsym.to_le_bytes());
        buffer.extend(self.nlocalsym.to_le_bytes());
        buffer.extend(self.iextdefsym.to_le_bytes());
        buffer.extend(self.nextdefsym.to_le_bytes());
        buffer.extend(self.iundefsym.to_le_bytes());
        buffer.extend(self.nundefsym.to_le_bytes());
        buffer.extend(self.tocoff.to_le_bytes());
        buffer.extend(self.ntoc.to_le_bytes());
        buffer.extend(self.modtaboff.to_le_bytes());
        buffer.extend(self.nmodtab.to_le_bytes());
        buffer.extend(self.extrefsymoff.to_le_bytes());
        buffer.extend(self.nextrefsyms.to_le_bytes());
        buffer.extend(self.indirectsymoff.to_le_bytes());
        buffer.extend(self.nindirectsyms.to_le_bytes());
        buffer.extend(self.extreloff.to_le_bytes());
        buffer.extend(self.nextrel.to_le_bytes());
        buffer.extend(self.locreloff.to_le_bytes());
        buffer.extend(self.nlocrel.to_le_bytes());

        buffer
    }
}

fn dylib_id_command(
    name: &str,
    timestamp: u32,
    current_version: u32,
    compatibility_version: u32,
) -> Vec<u8> {
    dylib_command(
        mach_object::LC_ID_DYLIB,
        name,
        timestamp,
        current_version,
        compatibility_version,
    )
}

fn dylib_load_command(
    name: &str,
    timestamp: u32,
    current_version: u32,
    compatibility_version: u32,
) -> Vec<u8> {
    dylib_command(
        mach_object::LC_LOAD_DYLIB,
        name,
        timestamp,
        current_version,
        compatibility_version,
    )
}

fn dylib_command(
    command: u32,
    name: &str,
    timestamp: u32,
    current_version: u32,
    compatibility_version: u32,
) -> Vec<u8> {
    let mut buffer = Vec::new();

    // string data starts after the 6 u32 fields
    let string_offset = 6 * 4u32;

    // commands round up to a multiple of 8 (when targetting 64-bit)
    // string must be null-terminated
    let size = next_multiple_of(string_offset as usize + name.len() + 1, 8);

    buffer.extend(command.to_le_bytes());
    buffer.extend((size as u32).to_le_bytes());

    buffer.extend((string_offset as u32).to_le_bytes());

    buffer.extend(timestamp.to_le_bytes());
    buffer.extend(current_version.to_le_bytes());
    buffer.extend(compatibility_version.to_le_bytes());

    buffer.extend(name.as_bytes());
    buffer.push(0);

    let padding = size - buffer.len();
    buffer.extend(std::iter::repeat(0).take(padding));

    buffer
}

struct SymtabCommand {
    symoff: u32,
    nsyms: u32,
    stroff: u32,
    strsize: u32,
}

impl SymtabCommand {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buffer = Vec::new();

        buffer.extend(mach_object::LC_SYMTAB.to_le_bytes());
        buffer.extend((4 + 4 + std::mem::size_of::<Self>() as u32).to_le_bytes());

        buffer.extend(self.symoff.to_le_bytes());
        buffer.extend(self.nsyms.to_le_bytes());
        buffer.extend(self.stroff.to_le_bytes());
        buffer.extend(self.strsize.to_le_bytes());

        buffer
    }
}

struct Nlist64 {
    string_table_offset: u32,
    n_type: u8,
    n_sect: u8,
    n_desc: u16,
    n_value: u64,
}

impl Nlist64 {
    fn to_bytes(&self) -> Vec<u8> {
        let mut buffer = Vec::new();

        buffer.extend(self.string_table_offset.to_le_bytes());
        buffer.extend(self.n_type.to_le_bytes());
        buffer.extend(self.n_sect.to_le_bytes());
        buffer.extend(self.n_desc.to_le_bytes());
        buffer.extend(self.n_value.to_le_bytes());

        buffer
    }
}

fn trivial_symbol_table<'a>(input: impl Iterator<Item = &'a str>) -> Vec<u8> {
    let mut buffer = Vec::new();

    let mut string_table_offset = 1u32;

    for string in input {
        let list = Nlist64 {
            string_table_offset,
            n_type: 0x0f,
            n_sect: 0x00,
            n_desc: 0x00,
            n_value: 0x00,
        };

        buffer.extend(list.to_bytes());

        string_table_offset += string.len() as u32 + 1;
    }

    buffer
}

fn trivial_string_table<'a>(input: impl Iterator<Item = &'a str>) -> Vec<u8> {
    let mut buffer = Vec::new();

    // always start with a NULL byte, so index 0 into the string table is the empty string
    buffer.push(0);

    for string in input {
        buffer.extend(string.as_bytes());
        buffer.push(0);
    }

    let padding = next_multiple_of(buffer.len(), 8) - buffer.len();
    buffer.extend(std::iter::repeat(0).take(padding));

    buffer
}

fn load_dylinker_command(linker: &str) -> Vec<u8> {
    let mut buffer = Vec::new();
    let size = next_multiple_of(4 + 4 + 4 + linker.len() + 1, 8);

    buffer.extend(mach_object::LC_LOAD_DYLINKER.to_le_bytes());
    buffer.extend((size as u32).to_le_bytes());
    buffer.extend(12u32.to_le_bytes());

    buffer.extend(linker.as_bytes());
    buffer.push(0);

    let padding = next_multiple_of(buffer.len(), 8) - buffer.len();
    buffer.extend(std::iter::repeat(0).take(padding));

    buffer
}

#[cfg(test)]
mod test {
    use super::*;

    use object::Object;

    // turn a string of hex symbols into a sequence of bytes
    #[allow(unused)]
    fn hexstring(input: &str) -> Vec<u8> {
        assert!(input.len() % 2 == 0);

        input
            .as_bytes()
            .chunks_exact(2)
            .map(|slice| std::str::from_utf8(slice).unwrap())
            .map(|c| u8::from_str_radix(c, 16).unwrap())
            .collect()
    }

    #[test]
    fn header_x86_64() {
        let triple = Triple {
            architecture: target_lexicon::Architecture::X86_64,
            operating_system: target_lexicon::OperatingSystem::Darwin,
            binary_format: target_lexicon::BinaryFormat::Macho,
            vendor: target_lexicon::Vendor::Apple,
            environment: target_lexicon::Environment::Gnu,
        };

        let commands = Commands {
            count: 0x10,
            size: 0x610,
        };

        // cffaedfe 07000001 03000000 06000000 10000000 10060000 8500b00 0000000
        let expected = "cffaedfe07000001030000000600000010000000100600008500b00000000000";

        let actual = macho_dylib_header(&triple, commands);
        let mut actual_str = String::new();

        for b in actual {
            use std::fmt::Write;
            write!(&mut actual_str, "{:02x}", b).unwrap();
        }

        assert_eq!(actual_str, expected);
    }

    #[test]
    fn segment_command_size_0() {
        let command = SegmentCommand64::default();

        assert_eq!(
            command.with_segments(&[]).len(),
            std::mem::size_of::<SegmentCommand64>()
        );
    }

    #[test]
    fn segment_command_size_3() {
        let command = SegmentCommand64::default();

        assert_eq!(
            command
                .with_segments(&[
                    Section64::default(),
                    Section64::default(),
                    Section64::default(),
                ])
                .len(),
            0x138,
        );
    }

    #[test]
    fn test_dylib_id_command() {
        let command = dylib_id_command("librocthing.dylib", 0x2, 0x10000, 0x10000);

        let mut actual_str = String::new();
        for b in command {
            use std::fmt::Write;
            write!(&mut actual_str, "{:02x}", b).unwrap();
        }

        let expected = r"0d00000030000000180000000200000000000100000001006c6962726f637468696e672e64796c696200000000000000";

        assert_eq!(actual_str, expected);
    }

    #[test]
    fn test_dylib_load_command() {
        let command = dylib_load_command("/usr/lib/libSystem.B.dylib", 0x2, 0x05016401, 0x10000);

        let mut actual_str = String::new();
        for b in command {
            use std::fmt::Write;
            write!(&mut actual_str, "{:02x}", b).unwrap();
        }

        let expected = r"0c00000038000000180000000200000001640105000001002f7573722f6c69622f6c696253797374656d2e422e64796c6962000000000000";

        assert_eq!(actual_str, expected);
    }

    #[test]
    fn check_exported_symbols() {
        let custom_names = &[
            "roc__mainForHost_1_Decode_DecodeError_caller".to_string(),
            "roc__mainForHost_1_Decode_DecodeError_result_size".to_string(),
            "roc__mainForHost_1_Decode_DecodeError_size".to_string(),
            "roc__mainForHost_1_Decode_DecodeResult_caller".to_string(),
            "roc__mainForHost_1_Decode_DecodeResult_result_size".to_string(),
            "roc__mainForHost_1_Decode_DecodeResult_size".to_string(),
            "roc__mainForHost_1_Decode_Decoder_caller".to_string(),
            "roc__mainForHost_1_Decode_Decoder_result_size".to_string(),
            "roc__mainForHost_1_Decode_Decoder_size".to_string(),
            "roc__mainForHost_1_Dict_Dict_caller".to_string(),
            "roc__mainForHost_1_Dict_Dict_result_size".to_string(),
            "roc__mainForHost_1_Dict_Dict_size".to_string(),
            "roc__mainForHost_1_Dict_LowLevelHasher_caller".to_string(),
            "roc__mainForHost_1_Dict_LowLevelHasher_result_size".to_string(),
            "roc__mainForHost_1_Dict_LowLevelHasher_size".to_string(),
            "roc__mainForHost_1_Encode_Encoder_caller".to_string(),
            "roc__mainForHost_1_Encode_Encoder_result_size".to_string(),
            "roc__mainForHost_1_Encode_Encoder_size".to_string(),
            "roc__mainForHost_1_Set_Set_caller".to_string(),
            "roc__mainForHost_1_Set_Set_result_size".to_string(),
            "roc__mainForHost_1_Set_Set_size".to_string(),
            "roc__mainForHost_1_exposed".to_string(),
            "roc__mainForHost_1_exposed_generic".to_string(),
            "roc__mainForHost_size".to_string(),
        ];

        let expected: Vec<_> = custom_names.iter().map(|s| format!("_{s}")).collect();

        let triple = Triple {
            architecture: target_lexicon::Architecture::X86_64,
            operating_system: target_lexicon::OperatingSystem::Darwin,
            binary_format: target_lexicon::BinaryFormat::Macho,
            vendor: target_lexicon::Vendor::Apple,
            environment: target_lexicon::Environment::Gnu,
        };

        let bytes = create_dylib_macho(custom_names, &triple).unwrap();

        let object = object::File::parse(bytes.as_slice()).unwrap();

        let mut triple = Triple::host();
        triple.binary_format = target_lexicon::BinaryFormat::Elf;

        let mut keys: Vec<_> = object
            .exports()
            .unwrap()
            .iter()
            .filter_map(|s| std::str::from_utf8(s.name()).ok())
            .collect();
        keys.sort_unstable();

        assert_eq!(keys.as_slice(), expected.as_slice())
    }
}
