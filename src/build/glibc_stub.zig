//! GNU libc stub generation for test platforms

const std = @import("std");

/// Generate assembly stub with essential libc symbols
pub fn generateComprehensiveStub(
    writer: anytype,
    target_arch: std.Target.Cpu.Arch,
) !void {
    const ptr_width: u32 = switch (target_arch) {
        .x86_64, .aarch64 => 8,
        else => 4,
    };

    try writer.writeAll(".text\n");

    // Generate __sysctl symbol
    try writer.print(".balign 8\n.globl __sysctl\n.type __sysctl, %function\n__sysctl:", .{});
    switch (target_arch) {
        .x86_64 => try writer.writeAll("    xor %rax, %rax\n    ret\n\n"),
        .aarch64 => try writer.writeAll("    mov x0, #0\n    ret\n\n"),
        else => try writer.writeAll("    ret\n\n"),
    }

    // Essential libc symbols that must be present for linking
    // These are resolved at runtime from real glibc
    const essential_symbols = [_][]const u8{
        // Core libc
        "__libc_start_main",
        "abort",
        "getauxval",
        "__tls_get_addr", // Thread-local storage
        "__errno_location", // Thread-safe errno access
        // Memory operations
        "memcpy",
        "memmove",
        "mmap",
        "mmap64",
        "munmap",
        "mremap",
        "msync",
        // File I/O
        "close",
        "read",
        "write",
        "readv",
        "writev",
        "openat64",
        "lseek64",
        "pread64",
        "pwritev64",
        "flock",
        "copy_file_range",
        "sendfile64",
        // Path operations
        "realpath",
        "readlink",
        // Environment
        "getenv",
        "isatty",
        // Signal handling
        "sigaction",
        "sigemptyset",
        // Dynamic linker
        "dl_iterate_phdr",
        "getcontext",
        // Math functions
        "fmod",
        "fmodf",
        "trunc",
        "truncf",
    };

    for (essential_symbols) |symbol| {
        try writer.print(".balign 8\n.globl {s}\n.type {s}, %function\n{s}:\n", .{ symbol, symbol, symbol });

        if (std.mem.eql(u8, symbol, "abort")) {
            // abort should exit with code 1
            switch (target_arch) {
                .x86_64 => try writer.writeAll("    mov $1, %rdi\n    mov $60, %rax\n    syscall\n\n"),
                .aarch64 => try writer.writeAll("    mov x0, #1\n    mov x8, #93\n    svc #0\n\n"),
                else => try writer.writeAll("    ret\n\n"),
            }
        } else {
            // Other symbols return 0 or are no-ops (resolved at runtime)
            switch (target_arch) {
                .x86_64 => try writer.writeAll("    xor %rax, %rax\n    ret\n\n"),
                .aarch64 => try writer.writeAll("    mov x0, #0\n    ret\n\n"),
                else => try writer.writeAll("    ret\n\n"),
            }
        }
    }

    // Add data section
    try writer.writeAll(".data\n");
    try writer.print("_IO_stdin_used: ", .{});
    if (ptr_width == 8) {
        try writer.writeAll(".quad 1\n");
    } else {
        try writer.writeAll(".long 1\n");
    }

    // environ is a global variable (char **environ)
    try writer.writeAll(".globl environ\n.type environ, %object\nenviron: ");
    if (ptr_width == 8) {
        try writer.writeAll(".quad 0\n");
    } else {
        try writer.writeAll(".long 0\n");
    }
}

/// Compile assembly stub to shared library using Zig's build system
pub fn compileAssemblyStub(
    b: *std.Build,
    asm_path: std.Build.LazyPath,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) *std.Build.Step.Compile {
    // Create a dynamic (shared) library
    const lib = b.addLibrary(.{
        .name = "c",
        .linkage = .dynamic, // replaces addSharedLibrary(...)
        .version = .{ .major = 6, .minor = 0, .patch = 0 },
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
        }),
    });
    // Add the assembly file as a source
    lib.addAssemblyFile(asm_path);

    // Allow unresolved symbols at link time
    lib.linker_allow_shlib_undefined = true;

    // Shared libraries should not be PIE
    lib.pie = false;
    return lib;
}
