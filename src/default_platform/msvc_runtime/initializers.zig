//! C runtime initializer tables for an MSVC-ABI image.
//!
//! Compilers place pointers to startup functions in `.CRT$XI*` (C) and
//! `.CRT$XC*` (C++ and Rust) sections. The linker sorts those sections by
//! name, so the sentinels declared here bracket every pointer an input
//! contributed.

const Initializer = ?*const fn () callconv(.c) void;

var xi_a: Initializer linksection(".CRT$XIA") = null;
var xi_z: Initializer linksection(".CRT$XIZ") = null;
var xc_a: Initializer linksection(".CRT$XCA") = null;
var xc_z: Initializer linksection(".CRT$XCZ") = null;

fn runTable(first: *Initializer, last: *Initializer) void {
    var entry: [*]Initializer = @ptrCast(first);
    const end: [*]Initializer = @ptrCast(last);
    while (@intFromPtr(entry) < @intFromPtr(end)) : (entry += 1) {
        if (entry[0]) |initializer| initializer();
    }
}

/// Runs every initializer the image's inputs registered, C before C++.
pub fn run() void {
    runTable(&xi_a, &xi_z);
    runTable(&xc_a, &xc_z);
}
