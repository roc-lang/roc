//! The PE thread-local storage directory for an MSVC-ABI image.
//!
//! Code that uses a `threadlocal` references `_tls_index`; resolving it from
//! this archive member also defines `_tls_used`, which `lld-link` records as
//! the image's TLS directory. Mirrors Zig's `std/os/windows/tls.zig`, which
//! `std.start` only provides to executables Zig links itself.

const std = @import("std");
const windows = std.os.windows;

export var _tls_index: u32 = windows.TLS_OUT_OF_INDEXES;
export var _tls_start: ?*anyopaque linksection(".tls") = null;
export var _tls_end: ?*anyopaque linksection(".tls$ZZZ") = null;
export var __xl_a: windows.PIMAGE_TLS_CALLBACK linksection(".CRT$XLA") = null;
export var __xl_z: windows.PIMAGE_TLS_CALLBACK linksection(".CRT$XLZ") = null;

const ImageTlsDirectory = extern struct {
    start_address_of_raw_data: *?*anyopaque,
    end_address_of_raw_data: *?*anyopaque,
    address_of_index: *u32,
    address_of_callbacks: [*:null]windows.PIMAGE_TLS_CALLBACK,
    size_of_zero_fill: u32,
    characteristics: u32,
};

export const _tls_used linksection(".rdata$T") = ImageTlsDirectory{
    .start_address_of_raw_data = &_tls_start,
    .end_address_of_raw_data = &_tls_end,
    .address_of_index = &_tls_index,
    // `__xl_a` is a null sentinel; the callbacks sit between it and `__xl_z`.
    .address_of_callbacks = @as([*:null]windows.PIMAGE_TLS_CALLBACK, @ptrCast(&__xl_a)) + 1,
    .size_of_zero_fill = 0,
    .characteristics = 0,
};
