//! Boxy runtime artifact for in-process Wasm evaluation.
//!
//! Unlike standalone output, evaluator Wasm reaches host operations through
//! the RocOps table installed by its generated entrypoint.

const builtins = @import("builtins");
const runtime = @import("main.zig");

/// Configure the shared runtime root for evaluator-vtable host calls.
pub const roc_host_call_mode: builtins.host_abi.HostCallMode = .vtable;
pub const roc_disable_tracy = runtime.roc_disable_tracy;
pub const panic = runtime.panic;
pub const std_options_elf_debug_info_search_paths = runtime.std_options_elf_debug_info_search_paths;
pub const std_options_debug_io = runtime.std_options_debug_io;
pub const std_options_debug_threaded_io = runtime.std_options_debug_threaded_io;
pub const std_options = runtime.std_options;

comptime {
    _ = &runtime.roc_boxy_init_embedded;
}
