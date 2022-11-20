use object::write;
use object::{Architecture, BinaryFormat, Endianness, SymbolFlags, SymbolKind, SymbolScope};
use roc_error_macros::{assert_sizeof_non_wasm, internal_error};
use std::path::Path;
use std::process::Command;
use target_lexicon::Triple;

use crate::pe::next_multiple_of;

// TODO: Eventually do this from scratch and in memory instead of with ld.
pub fn create_dylib_macho(
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
        target_lexicon::Architecture::Aarch64(_) => todo!(),
        _ => unreachable!(),
    };

    let flags: u32 = mach_object::MH_NOUNDEFS
        | mach_object::MH_DYLDLINK
        | mach_object::MH_TWOLEVEL
        | mach_object::MH_NO_REEXPORTED_DYLIBS
        | mach_object::MH_HAS_TLV_DESCRIPTORS
        | mach_object::MH_PIE;

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

type vm_prot_t = u32;

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
#[derive(Default)]
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
    fn to_bytes(&self) -> Vec<u8> {
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
    maxprot: vm_prot_t,
    initprot: vm_prot_t,
    nsects: u32,
    flags: u32,
}

const _X: () = assert!(std::mem::size_of::<SegmentCommand64>() == 0x48);

impl SegmentCommand64 {
    fn with_segments(&self, sections: &[Section64]) -> Vec<u8> {
        let mut buffer = Vec::new();

        let cmdsize = std::mem::size_of::<SegmentCommand64>()
            + std::mem::size_of::<SegmentCommand64>() * sections.len();

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

        buffer.extend(self.nsects.to_le_bytes());
        buffer.extend(self.flags.to_le_bytes());

        for section in sections {
            buffer.extend(section.to_bytes());
        }

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

fn export_trie(input: &[(u32, &str)]) -> Vec<u8> {
    let mut buffer = Vec::new();

    // Terminal size(0)
    buffer.push(0);

    // Child count(1)
    buffer.push(1);

    // Node label("_")
    buffer.extend(b"_\0");

    // Node offset(5)
    buffer.push(5);
    assert_eq!(buffer.len(), 5);

    // Terminal size(0)
    buffer.push(0);

    //

    // Child count(n)
    debug_assert!(input.len() < 128);
    buffer.push(input.len() as u8);

    let mut offset_base = buffer.len();
    for (_, name) in input {
        offset_base += name.len() + 1; // null-terminated

        offset_base += 1; // will become variable with LEB128
    }

    for (i, (_address, name)) in input.iter().enumerate() {
        // Node label("name")
        buffer.extend(name.as_bytes());
        buffer.push(0); // null-terminated

        // Node offset
        buffer.push((offset_base + 5 * i) as u8);
    }

    for _ in input {
        // Terminal size (number of words)
        buffer.push(11);

        // Flags
        buffer.push(0);

        // Symbol offset(-8) I think this means an export without a definition
        buffer.extend([0x80, 0x80, 0x80, 0x80, 0xf0, 0xff, 0xff, 0xff, 0xff, 0x01]);

        // Child count(0)
        buffer.push(0);
    }

    // commands round up to a multiple of 8 (when targetting 64-bit)
    // string must be null-terminated
    let padding = next_multiple_of(buffer.len(), 8) - buffer.len();
    buffer.extend(std::iter::repeat(0).take(padding));

    buffer
}

#[cfg(test)]
mod test {
    use super::*;

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

    const STRINGS: &[&str] = &[
        "__mh_execute_header",
        "_roc__mainForHost_1_Decode_DecodeError_caller",
        "_roc__mainForHost_1_Decode_DecodeError_result_size",
        //        "_roc__mainForHost_1_Decode_DecodeError_size",
        //        "_roc__mainForHost_1_Decode_DecodeResult_caller",
        //        "_roc__mainForHost_1_Decode_DecodeResult_result_size",
        //        "_roc__mainForHost_1_Decode_DecodeResult_size",
        //        "_roc__mainForHost_1_Decode_Decoder_caller",
        //        "_roc__mainForHost_1_Decode_Decoder_result_size",
        //        "_roc__mainForHost_1_Decode_Decoder_size",
        //        "_roc__mainForHost_1_Dict_Dict_caller",
        //        "_roc__mainForHost_1_Dict_Dict_result_size",
        //        "_roc__mainForHost_1_Dict_Dict_size",
        //        "_roc__mainForHost_1_Dict_LowLevelHasher_caller",
        //        "_roc__mainForHost_1_Dict_LowLevelHasher_result_size",
        //        "_roc__mainForHost_1_Dict_LowLevelHasher_size",
        //        "_roc__mainForHost_1_Encode_Encoder_caller",
        //        "_roc__mainForHost_1_Encode_Encoder_result_size",
        //        "_roc__mainForHost_1_Encode_Encoder_size",
        //        "_roc__mainForHost_1_Set_Set_caller",
        //        "_roc__mainForHost_1_Set_Set_result_size",
        //        "_roc__mainForHost_1_Set_Set_size",
        //        "_roc__mainForHost_1_exposed",
        //        "_roc__mainForHost_1_exposed_generic",
        //        "_roc__mainForHost_size",
    ];

    #[test]
    fn running_example() {
        let mut bytes = Vec::new();

        let triple = Triple {
            architecture: target_lexicon::Architecture::X86_64,
            operating_system: target_lexicon::OperatingSystem::Darwin,
            binary_format: target_lexicon::BinaryFormat::Macho,
            vendor: target_lexicon::Vendor::Apple,
            environment: target_lexicon::Environment::Gnu,
        };

        let commands = Commands {
            count: 5 + 2 + 1,
            size: 5 * 0x48 + 0x30 + 0x38 + 0x30,
        };

        bytes.extend_from_slice(macho_dylib_header(&triple, commands).as_slice());

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
            vmaddr: 0,
            vmsize: 0x1000,
            fileoff: 0,
            filesize: 0x1000,
            maxprot: 5,  // TODO why?
            initprot: 5, // TODO why?
            nsects: 0,
            flags: 0,
        };

        bytes.extend(command.with_segments(&[]));

        //

        let command = SegmentCommand64 {
            cmd: 0x19,
            cmdsize: 0x48,
            segname: bstring16("__DATA_CONST"),
            vmaddr: 0x0000000100001000,
            vmsize: 0x1000,
            fileoff: 0x1000,
            filesize: 0x1000,
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
            segname: bstring16("__DATA"),
            vmaddr: 0x0000000100002000,
            vmsize: 0x1000,
            fileoff: 0x2000,
            filesize: 0x1000,
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
            segname: bstring16("__LINKEDIT"),
            vmaddr: 0x0000000100003000,
            vmsize: 0x1000,
            fileoff: 0x3000,
            filesize: 0x1000,
            maxprot: 0,
            initprot: 0,
            nsects: 0,
            flags: 0,
        };

        bytes.extend(command.with_segments(&[]));

        bytes.extend(dylib_id_command("librocthing.dylib", 0x2, 0x10000, 0x10000));

        bytes.extend(dylib_load_command(
            "/usr/lib/libSystem.B.dylib",
            0x2,
            0x05016401,
            0x10000,
        ));

        let trie = export_trie(&[(1234, "_mh_execute_header"), (5678, "xxx"), (9090, "yyy")]);
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

        let delta = 0x3000 - bytes.len();
        bytes.extend(std::iter::repeat(0).take(delta));

        // rebase
        bytes.extend(std::iter::repeat(0).take(0x8));

        // binding

        let a = bytes.len();
        bytes.extend([0x11, 0x51, 0x40]);
        bytes.extend(b"dyld_stub_binder\0");
        bytes.extend([0x72, 0x00, 0x90, 0x00]);
        assert_eq!(bytes.len() - a, 0x18);

        // export
        bytes.extend(trie);

        std::fs::write("/tmp/test.dylib", &bytes);
    }
}
