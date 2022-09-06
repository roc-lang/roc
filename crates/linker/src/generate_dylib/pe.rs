use object::pe;
use object::LittleEndian as LE;

fn synthetic_image_export_directory(
    name: &str,
    virtual_address: u32,
    custom_names: &[String],
) -> pe::ImageExportDirectory {
    use object::{U16, U32};

    let directory_size = std::mem::size_of::<pe::ImageExportDirectory>() as u32;

    // actual data is after the directory and the name of the dll file (null-terminated)
    let start = virtual_address + directory_size + name.len() as u32 + 1;

    let address_of_functions = start;
    let address_of_names = address_of_functions + 4 * custom_names.len() as u32;
    let address_of_name_ordinals = address_of_names + 4 * custom_names.len() as u32;

    pe::ImageExportDirectory {
        characteristics: U32::new(LE, 0),
        time_date_stamp: U32::new(LE, 0),
        major_version: U16::new(LE, 0),
        minor_version: U16::new(LE, 0),
        name: U32::new(LE, virtual_address + directory_size),
        base: U32::new(LE, 0),
        number_of_functions: U32::new(LE, custom_names.len() as u32),
        number_of_names: U32::new(LE, custom_names.len() as u32),
        address_of_functions: U32::new(LE, address_of_functions),
        address_of_names: U32::new(LE, address_of_names),
        address_of_name_ordinals: U32::new(LE, address_of_name_ordinals),
    }
}

fn synthetic_export_dir(virtual_address: u32, custom_names: &[String]) -> Vec<u8> {
    let mut vec = vec![0; std::mem::size_of::<pe::ImageExportDirectory>()];

    let ptr = vec.as_mut_ptr();

    let name = "roc-cheaty-lib.dll";
    let directory = synthetic_image_export_directory(name, virtual_address, custom_names);

    unsafe {
        std::ptr::write_unaligned(ptr as *mut pe::ImageExportDirectory, directory);
    }

    // write the .dll name, null-terminated
    vec.extend(name.as_bytes());
    vec.push(0);

    let n = custom_names.len();

    // Unsure what this one does; it does not seem important for our purposes
    //
    // Export Address Table -- Ordinal Base 0
    //  [   1] +base[   1] 1020 Export RVA
    //  [   2] +base[   2] d030 Export RVA
    for _ in custom_names {
        vec.extend(42u32.to_le_bytes());
    }

    // Maps the index to a name
    //
    // [Ordinal/Name Pointer] Table
    //  [   1] _CRT_INIT
    //  [   2] _CRT_MT
    let mut acc = directory.address_of_name_ordinals.get(LE) + n as u32 * 2;
    for name in custom_names {
        vec.extend(acc.to_le_bytes());
        acc += name.len() as u32 + 1;
    }

    // the ordinals, which just map to the index in our case
    for (i, _) in custom_names.iter().enumerate() {
        vec.extend((i as u16).to_le_bytes());
    }

    // write out the names of the symbols, as null-terminated strings
    for name in custom_names {
        vec.extend(name.as_bytes());
        vec.push(0);
    }

    vec
}

pub fn synthetic_dll(custom_names: &[String]) -> Vec<u8> {
    let mut out_data = Vec::new();
    let mut writer = object::write::pe::Writer::new(true, 8, 8, &mut out_data);

    // fairly randomly chosen. Not sure if this is relevant
    let virtual_address = 0x138;

    let exports = synthetic_export_dir(virtual_address, custom_names);

    // Reserve file ranges and virtual addresses.
    writer.reserve_dos_header_and_stub();

    // we will have one header: the export directory
    writer.reserve_nt_headers(1);

    writer.set_data_directory(
        pe::IMAGE_DIRECTORY_ENTRY_EXPORT,
        virtual_address,
        exports.len() as _,
    );

    writer.reserve_section_headers(1);

    // we store the export directory in a .rdata section
    let rdata_section: (_, Vec<u8>) = {
        let range = writer.reserve_section(
            *b".rdata\0\0",
            1073741888,
            // virtual size
            exports.len() as u32,
            // size_of_raw_data
            exports.len() as u32,
        );

        (range.file_offset, exports)
    };

    // Start writing.
    writer.write_dos_header_and_stub().unwrap();

    // the header on my machine
    let headers = object::write::pe::NtHeaders {
        machine: 34404,
        time_date_stamp: 1661696130,
        characteristics: 8226,
        major_linker_version: 14,
        minor_linker_version: 0,
        address_of_entry_point: 4560,
        image_base: 6442450944,
        major_operating_system_version: 6,
        minor_operating_system_version: 0,
        major_image_version: 0,
        minor_image_version: 0,
        major_subsystem_version: 6,
        minor_subsystem_version: 0,
        subsystem: 2,
        dll_characteristics: 352,
        size_of_stack_reserve: 1048576,
        size_of_stack_commit: 4096,
        size_of_heap_reserve: 1048576,
        size_of_heap_commit: 4096,
    };

    writer.write_nt_headers(headers);

    writer.write_section_headers();

    let (offset, data) = rdata_section;
    writer.write_section(offset, &data);

    debug_assert_eq!(writer.reserved_len() as usize, writer.len());

    out_data
}
