//! Core LLVM builtin bitcode for common string/list/refcount/debug operations.
//!
//! The full `static_lib.zig` payload includes decimal, float parsing/formatting,
//! and wide-integer helpers. LLVM builds link this smaller payload when the app
//! only declares roots classified `.core` in the builtin registry.

const std = @import("std");
const shim_io = @import("shim_io");

/// Builtin payloads must not pull in Zig's panic formatting machinery.
pub const panic = std.debug.no_panic;

/// Uses the same freestanding debug-info search path hook as the full builtins payload.
pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
/// Minimal debug output override; avoids pulling in the full threaded IO vtable.
pub const std_options_debug_io = shim_io.io();
/// Disables threaded debug IO to prevent the threaded vtable from being linked into user programs.
pub const std_options_debug_threaded_io = null;

/// Disables stack-trace capture; see `shim_io.std_options_no_stack_tracing`.
pub const std_options = shim_io.std_options_no_stack_tracing;

const builtin_registry = @import("builtin_registry.zig");

comptime {
    builtin_registry.exportOverflowWrappers();
}

/// Wrapper namespace for consumers that call builtins via direct function
/// pointers rather than the exported symbols.
pub const dev_wrappers = @import("dev_wrappers.zig");

// Export only the registry wrappers classified `.core`.
comptime {
    builtin_registry.exportWrappers(.core);
}
