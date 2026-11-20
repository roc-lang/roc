//! AFL++ entry points for the fuzz target that the build names in
//! `fuzz_options.target`.

const fuzz_target = @field(@import("targets.zig"), @import("fuzz_options").target);

/// Hook for AFL++ to initialize the fuzz test environment.
export fn zig_fuzz_init() void {
    fuzz_target.zig_fuzz_init();
}

/// Hook for AFL++ to run the fuzz test.
export fn zig_fuzz_test(buf: [*]u8, len: isize) void {
    fuzz_target.zig_fuzz_test_inner(buf, len, false);
}
