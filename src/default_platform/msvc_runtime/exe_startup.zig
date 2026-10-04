//! Process entry point for an MSVC-ABI Windows executable.
//!
//! An MSVC link takes nothing from the machine it runs on, so the platform
//! supplies the symbol `lld-link` infers as a console program's entry. This
//! one gets `argc`/`argv` from the Universal CRT, runs the image's
//! initializers, and calls the host's C `main`.

const initializers = @import("initializers.zig");

extern fn main(argc: c_int, argv: [*][*:0]u8) callconv(.c) c_int;

extern fn _configure_narrow_argv(mode: c_int) callconv(.c) c_int;
extern fn __p___argc() callconv(.c) *c_int;
extern fn __p___argv() callconv(.c) *[*][*:0]u8;
extern fn exit(status: c_int) callconv(.c) noreturn;

/// `_crt_argv_unexpanded_arguments`: split the command line without
/// expanding wildcards.
const argv_unexpanded = 1;

export fn mainCRTStartup() callconv(.winapi) noreturn {
    if (_configure_narrow_argv(argv_unexpanded) != 0) exit(255);
    initializers.run();
    exit(main(__p___argc().*, __p___argv().*));
}
