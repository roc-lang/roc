//! Root for the stack-checked extern-symbol-mode builtins for wasm
//! platforms that reserve low memory. Identical to `extern_static_lib.zig`
//! except that it turns on `stack_guard`, which keeps every builtin free of
//! recursion, since such a platform's stack sits above static data, where
//! nothing traps an overflow.

const static_lib = @import("static_lib.zig");

/// The host that links this archive defines the runtime symbols.
pub const roc_host_role: @import("host_abi.zig").HostRole = .platform;
/// Turns on `stack_guard` throughout this object.
pub const roc_wasm_stack_checks = true;

pub const panic = static_lib.panic;
pub const std_options_elf_debug_info_search_paths = static_lib.std_options_elf_debug_info_search_paths;
pub const std_options_debug_io = static_lib.std_options_debug_io;
pub const std_options_debug_threaded_io = static_lib.std_options_debug_threaded_io;
pub const std_options = static_lib.std_options;

comptime {
    // Force semantic analysis of the wrapped root so its comptime export
    // blocks run; merely aliasing its decls above is lazy and would leave
    // the prebuilt object empty.
    if (static_lib != static_lib) unreachable; // a type always equals itself; the comparison only forces analysis
}
