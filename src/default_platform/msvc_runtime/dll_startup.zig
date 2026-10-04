//! DLL entry point for an MSVC-ABI Windows shared library.
//!
//! The counterpart of `exe_startup.zig` for a `Shared` output. Roc's own
//! hosts define no `DllMain`, so attaching only runs the image's initializers.

const initializers = @import("initializers.zig");

const dll_process_attach = 1;

export fn _DllMainCRTStartup(instance: ?*anyopaque, reason: u32, reserved: ?*anyopaque) callconv(.winapi) c_int {
    _ = instance;
    _ = reserved;
    if (reason == dll_process_attach) initializers.run();
    return 1;
}
