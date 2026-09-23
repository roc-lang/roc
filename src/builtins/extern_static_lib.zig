//! Root for the extern-symbol-mode prebuilt builtins objects. Identical to
//! static_lib.zig except that builtins reach the host through linker-resolved
//! symbols (roc_alloc and friends) instead of the RocOps vtable, which is what
//! compiled output links against under the symbol ABI.

const static_lib = @import("static_lib.zig");

/// The host that links this archive defines the runtime symbols.
pub const roc_host_role: @import("host_abi.zig").HostRole = .platform;

pub const panic = static_lib.panic;
pub const std_options_elf_debug_info_search_paths = static_lib.std_options_elf_debug_info_search_paths;
pub const std_options_debug_io = static_lib.std_options_debug_io;
pub const std_options_debug_threaded_io = static_lib.std_options_debug_threaded_io;
pub const std_options = static_lib.std_options;

comptime {
    // Force semantic analysis of the wrapped root so its comptime export
    // blocks run; merely aliasing its decls above is lazy and would leave
    // the prebuilt object empty.
    if (static_lib != static_lib) unreachable;
}
