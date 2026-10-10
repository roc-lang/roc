//! C runtime initializer tables for an MSVC-ABI image.
//!
//! Compilers place pointers to startup functions in `.CRT$XI*` (C) and
//! `.CRT$XC*` (C++ and Rust) sections. The linker sorts those sections by
//! name, so the sentinels declared here bracket every pointer an input
//! contributed.

/// A C initializer reports failure with a nonzero result.
const CInitializer = ?*const fn () callconv(.c) c_int;
const CppInitializer = ?*const fn () callconv(.c) void;

var xi_a: CInitializer linksection(".CRT$XIA") = null;
var xi_z: CInitializer linksection(".CRT$XIZ") = null;
var xc_a: CppInitializer linksection(".CRT$XCA") = null;
var xc_z: CppInitializer linksection(".CRT$XCZ") = null;

/// Runs every initializer the image's inputs registered, C before C++.
/// Stops at the first C initializer that fails and returns its result;
/// returns zero when all of them succeeded.
pub fn run() c_int {
    var c_entry: [*]CInitializer = @ptrCast(&xi_a);
    const c_end: [*]CInitializer = @ptrCast(&xi_z);
    while (@intFromPtr(c_entry) < @intFromPtr(c_end)) : (c_entry += 1) {
        if (c_entry[0]) |initializer| {
            const result = initializer();
            if (result != 0) return result;
        }
    }

    var cpp_entry: [*]CppInitializer = @ptrCast(&xc_a);
    const cpp_end: [*]CppInitializer = @ptrCast(&xc_z);
    while (@intFromPtr(cpp_entry) < @intFromPtr(cpp_end)) : (cpp_entry += 1) {
        if (cpp_entry[0]) |initializer| initializer();
    }
    return 0;
}
