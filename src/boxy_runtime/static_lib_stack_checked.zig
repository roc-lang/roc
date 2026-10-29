//! Boxy runtime object for wasm platforms that reserve low memory.
//!
//! Identical to `static_lib.zig` except that every runtime function that can
//! recurse checks the stack bounds first (`builtins.stack_guard`), since such
//! a platform's stack sits above static data, where nothing traps an overflow.

const builtins = @import("builtins");
const runtime = @import("static_lib.zig");

/// This object is linked into a platform's program, like `static_lib.zig`.
pub const roc_host_role: builtins.host_abi.HostRole = runtime.roc_host_role;
/// Turns on `builtins.stack_guard` throughout this object.
pub const roc_wasm_stack_checks = true;
pub const panic = runtime.panic;
pub const std_options_elf_debug_info_search_paths = runtime.std_options_elf_debug_info_search_paths;
pub const std_options_debug_io = runtime.std_options_debug_io;
pub const std_options_debug_threaded_io = runtime.std_options_debug_threaded_io;
pub const std_options = runtime.std_options;

comptime {
    // Force semantic analysis of the wrapped root so its comptime export
    // block runs; merely aliasing its decls above is lazy and would leave
    // the object empty.
    if (runtime != runtime) unreachable; // a type always equals itself; the comparison only forces analysis
}
