//! Root for the extern-symbol-mode core builtin bitcode. Identical to
//! static_lib_core.zig except that builtins reach the host through
//! linker-resolved symbols (roc_alloc and friends) instead of the RocOps
//! vtable, which is what compiled output links against under the symbol ABI.

const static_lib_core = @import("static_lib_core.zig");

/// Switches host_abi's helper methods to call the extern runtime symbols.
pub const roc_host_call_mode = .extern_symbols;

pub const panic = static_lib_core.panic;
pub const std_options_elf_debug_info_search_paths = static_lib_core.std_options_elf_debug_info_search_paths;
pub const std_options_debug_io = static_lib_core.std_options_debug_io;
pub const std_options_debug_threaded_io = static_lib_core.std_options_debug_threaded_io;
pub const std_options = static_lib_core.std_options;

comptime {
    // Force semantic analysis of the wrapped root so its comptime export
    // blocks run; merely aliasing its decls above is lazy and would leave
    // the prebuilt object empty.
    if (static_lib_core != static_lib_core) unreachable;
}
