//! Explicit MSVC-ABI startup and library inputs for default program builds.

const mingw_runtime = @import("mingw_runtime.zig");

/// Startup, TLS directory, and compiler-rt, built from
/// `src/default_platform/msvc_runtime/`.
pub const runtime_archive = "msvc_runtime.lib";

/// UCRT and Win32 import libraries, shared with the matching MinGW target.
pub const import_libraries = mingw_runtime.import_libraries;

/// Runtime libraries in link order after the app object.
pub const libraries = [_][]const u8{runtime_archive} ++ import_libraries;

/// All files copied into platforms and embedded in the compiler.
pub const files = libraries;

/// The checked-in MinGW target directory holding `target_name`'s import
/// libraries.
pub fn importLibrarySourceTarget(target_name: []const u8) []const u8 {
    return if (target_name.len >= 5 and target_name[0] == 'a') "arm64mingw" else "x64mingw";
}

/// Platform link inputs that follow `app` in every MSVC target.
pub const inputs_after_app = blk: {
    var inputs: []const u8 = "";
    for (libraries) |name| inputs = inputs ++ ", \"" ++ name ++ "\"";
    break :blk inputs;
};

/// Platform link inputs for a console program.
pub const executable_inputs = "[app" ++ inputs_after_app ++ "]";
