//! DLL entry point for an MSVC-ABI Windows shared library.
//!
//! The counterpart of `exe_startup.zig` for a `Shared` output. It serves
//! Roc's own test and default platforms, whose hosts define no `DllMain` and
//! register no static destructors or exit handlers, so attaching runs the
//! image's initializers and detaching has nothing to undo. It is not a
//! general C runtime: a platform whose host needs per-module teardown lists
//! its own startup input instead.

const initializers = @import("initializers.zig");

const dll_process_attach = 1;

export fn _DllMainCRTStartup(_: ?*anyopaque, reason: u32, _: ?*anyopaque) callconv(.winapi) c_int {
    if (reason == dll_process_attach and initializers.run() != 0) return 0;
    return 1;
}
