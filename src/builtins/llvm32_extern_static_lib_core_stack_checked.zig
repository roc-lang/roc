//! Root for the 32-bit LLVM builtins bitcode built from
//! `extern_static_lib_core_stack_checked.zig`. The payload is compiled for
//! wasm32 and retargeted, so the builtins the LLVM backend links per target,
//! like UTF-8 validation (see `validate` in utf8.zig), reference their linked
//! definitions here; `roc_llvm_payload` marks this object as that payload.

const inner = @import("extern_static_lib_core_stack_checked.zig");

/// Marks this object as an LLVM builtins payload.
pub const roc_llvm_payload = true;
/// The host role of the wrapped root.
pub const roc_host_role = inner.roc_host_role;
/// Turns on `stack_guard`, as the wrapped root does.
pub const roc_wasm_stack_checks = inner.roc_wasm_stack_checks;

pub const panic = inner.panic;
pub const std_options_elf_debug_info_search_paths = inner.std_options_elf_debug_info_search_paths;
pub const std_options_debug_io = inner.std_options_debug_io;
pub const std_options_debug_threaded_io = inner.std_options_debug_threaded_io;
pub const std_options = inner.std_options;

comptime {
    // Force semantic analysis of the wrapped root so its comptime export
    // blocks run; merely aliasing its decls above is lazy and would leave
    // the payload empty.
    if (inner != inner) unreachable; // a type always equals itself; the comparison only forces analysis
}
