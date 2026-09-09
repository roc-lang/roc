//! Explicit vendored MinGW startup and library inputs for default program builds.

/// Runtime libraries in link order after the app object.
pub const libraries = [_][]const u8{
    "libmingw32.lib",
    "zigc.lib",
    "compiler_rt.lib",
    "api-ms-win-crt-conio-l1-1-0.lib",
    "api-ms-win-crt-convert-l1-1-0.lib",
    "api-ms-win-crt-environment-l1-1-0.lib",
    "api-ms-win-crt-filesystem-l1-1-0.lib",
    "api-ms-win-crt-heap-l1-1-0.lib",
    "api-ms-win-crt-locale-l1-1-0.lib",
    "api-ms-win-crt-math-l1-1-0.lib",
    "api-ms-win-crt-multibyte-l1-1-0.lib",
    "api-ms-win-crt-private-l1-1-0.lib",
    "api-ms-win-crt-process-l1-1-0.lib",
    "api-ms-win-crt-runtime-l1-1-0.lib",
    "api-ms-win-crt-stdio-l1-1-0.lib",
    "api-ms-win-crt-string-l1-1-0.lib",
    "api-ms-win-crt-time-l1-1-0.lib",
    "api-ms-win-crt-utility-l1-1-0.lib",
    "advapi32.lib",
    "kernel32.lib",
    "ntdll.lib",
    "shell32.lib",
    "user32.lib",
    // The http-headers host calls into Winsock; `/nodefaultlib` drops the
    // `.drectve /defaultlib:ws2_32` its object carries, so platforms that need
    // sockets list this explicitly.
    "ws2_32.lib",
};

/// All vendored files copied into platforms and embedded in the compiler.
pub const files = [_][]const u8{ "crt2.obj", "dllcrt2.obj" } ++ libraries;

/// Platform link inputs for a console program.
pub const executable_inputs = blk: {
    var inputs: []const u8 = "[\"crt2.obj\", app";
    for (libraries) |name| inputs = inputs ++ ", \"" ++ name ++ "\"";
    break :blk inputs ++ "]";
};
