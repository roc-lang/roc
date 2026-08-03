//! Static library version of builtins that provides minimal exports
//! This is a separate entry point to avoid circular imports with builtins module
//!
//! This library provides:
//! - Numeric overflow functions (for compiler-rt)
//! - Dev backend wrapper functions (for roc build --opt=dev)

const std = @import("std");
const shim_io = @import("shim_io");

pub const panic = std.debug.no_panic;

pub const std_options_elf_debug_info_search_paths = shim_io.elfDebugInfoSearchPaths;
/// Minimal debug output override; avoids pulling in the full threaded IO vtable.
pub const std_options_debug_io = shim_io.io();
/// Disables threaded debug IO to prevent the threaded vtable from being linked into user programs.
pub const std_options_debug_threaded_io = null;

/// Disables stack-trace capture; see `shim_io.std_options_no_stack_tracing`.
pub const std_options = shim_io.std_options_no_stack_tracing;

const builtin_registry = @import("builtin_registry.zig");

// Export overflow functions that might need compiler-rt symbols.
comptime {
    builtin_registry.exportOverflowWrappers();
}

// Wrapper namespace for consumers that call builtins via direct function
// pointers rather than the exported symbols.
pub const dev_wrappers = @import("dev_wrappers.zig");

// Export every linkable registry wrapper - these are used by
// `roc build --opt=dev` to call builtin functions via symbol references.
comptime {
    builtin_registry.exportWrappers(.all_linked);
}
