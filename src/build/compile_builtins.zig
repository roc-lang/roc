//! Builtin .roc file compilation at build time
//!
//! Uses the bootstrap compiler to compile builtin .roc files and capture their
//! serialized ModuleEnvs.

const std = @import("std");
const Build = std.Build;
const Step = Build.Step;

/// Validate that the builtin list matches the .roc files on disk
pub fn validateBuiltinList(
    b: *Build,
    builtin_roc_files: []const []const u8,
) !void {
    const builtins_dir = "src/builtins/roc";

    // Get actual .roc files on disk
    var dir = try std.fs.cwd().openDir(builtins_dir, .{ .iterate = true });
    defer dir.close();

    var actual_files = std.ArrayList([]const u8).init(b.allocator);
    defer {
        for (actual_files.items) |file| b.allocator.free(file);
        actual_files.deinit();
    }

    var it = dir.iterate();
    while (try it.next()) |entry| {
        if (entry.kind == .file and std.mem.endsWith(u8, entry.name, ".roc")) {
            // Skip main.roc (not a builtin module)
            if (std.mem.eql(u8, entry.name, "main.roc")) continue;
            try actual_files.append(try b.allocator.dupe(u8, entry.name));
        }
    }

    // Sort for comparison
    std.mem.sort([]const u8, actual_files.items, {}, struct {
        fn lessThan(_: void, a: []const u8, str_b: []const u8) bool {
            return std.mem.lessThan(u8, a, str_b);
        }
    }.lessThan);

    // Check if lists match
    if (actual_files.items.len != builtin_roc_files.len) {
        std.debug.print("\n❌ ERROR: Builtin list mismatch!\n", .{});
        std.debug.print("Expected {d} files in load_builtins.zig, found {d} .roc files on disk\n\n", .{
            builtin_roc_files.len,
            actual_files.items.len,
        });
        std.debug.print("Files on disk in {s}:\n", .{builtins_dir});
        for (actual_files.items) |file| {
            std.debug.print("  - {s}\n", .{file});
        }
        std.debug.print("\nFiles in load_builtins.zig:\n", .{});
        for (builtin_roc_files) |file| {
            std.debug.print("  - {s}\n", .{file});
        }
        std.debug.print("\nPlease update src/builtins/load_builtins.zig to match the files on disk.\n", .{});
        return error.BuiltinListMismatch;
    }

    // Check each file exists
    for (builtin_roc_files) |file| {
        var found = false;
        for (actual_files.items) |actual| {
            if (std.mem.eql(u8, file, actual)) {
                found = true;
                break;
            }
        }
        if (!found) {
            std.debug.print("\n❌ ERROR: File '{s}' listed in load_builtins.zig but not found in {s}/\n", .{ file, builtins_dir });
            std.debug.print("Please update src/builtins/load_builtins.zig to remove it.\n", .{});
            return error.BuiltinListMismatch;
        }
    }
}

/// Compile builtin .roc files and capture their serialized ModuleEnvs
pub fn compileBuiltins(
    b: *Build,
    bootstrap_compiler: *Step.Compile,
    builtin_roc_files: []const []const u8,
) *Step.WriteFile {
    const write_step = b.addWriteFiles();

    for (builtin_roc_files) |roc_file| {
        // Construct path: src/builtins/roc/<file>
        const full_path = b.fmt("src/builtins/roc/{s}", .{roc_file});

        // Run: roc_bootstrap check --output-module-env <file>
        const run_compile = b.addRunArtifact(bootstrap_compiler);
        run_compile.addArg("check");
        run_compile.addArg("--output-module-env");
        run_compile.addFileArg(b.path(full_path));

        // Capture stdout (serialized ModuleEnv bytes)
        const serialized = run_compile.captureStdOut();

        // Generate output filename: Bool.roc -> Bool.roc.env
        const env_filename = b.fmt("{s}.env", .{roc_file});

        // Add to write step
        _ = write_step.addCopyFile(serialized, env_filename);
    }

    return write_step;
}

/// Generate the embedded_envs.zig file that @embedFile's all the .env files
/// This file is generated in the same WriteFile step output as the .env files,
/// so @embedFile paths work correctly.
pub fn generateEmbeddedEnvs(
    b: *Build,
    compiled_builtins: *Step.WriteFile,
    builtin_roc_files: []const []const u8,
) !Build.LazyPath {
    var code = std.ArrayList(u8).init(b.allocator);
    const writer = code.writer();

    try writer.writeAll(
        \\//! Generated file - embeds compiled builtin ModuleEnvs
        \\//! This file is auto-generated at build time by compile_builtins.zig
        \\//! It lives in the build cache alongside the .env files it references
        \\
        \\const std = @import("std");
        \\const can = @import("can");
        \\const ModuleEnv = can.ModuleEnv;
        \\
        \\
    );

    // Generate const for each .env file
    for (builtin_roc_files) |roc_file| {
        const stem = std.fs.path.stem(roc_file);
        const env_filename = b.fmt("{s}.env", .{roc_file});

        try writer.print(
            \\/// Serialized ModuleEnv bytes for {s}
            \\pub const {s}_env_bytes = @embedFile("{s}");
            \\
        , .{ stem, stem, env_filename });
    }

    try writer.writeAll(
        \\
        \\/// Load all embedded builtin ModuleEnvs by deserializing them
        \\pub fn loadAll(gpa: std.mem.Allocator) ![]const *const ModuleEnv {
        \\    const compile = @import("compile");
        \\    const CacheModule = compile.CacheModule;
        \\
        \\    var envs = std.ArrayList(*const ModuleEnv).init(gpa);
        \\    errdefer {
        \\        for (envs.items) |env| {
        \\            @constCast(env).deinit();
        \\            gpa.destroy(@constCast(env));
        \\        }
        \\        envs.deinit();
        \\    }
        \\
        \\
    );

    // Generate deserialization code for each builtin
    for (builtin_roc_files) |roc_file| {
        const stem = std.fs.path.stem(roc_file);
        try writer.print(
            \\    // Deserialize {s}
            \\    {{
            \\        const env_ptr = try gpa.create(ModuleEnv);
            \\        errdefer gpa.destroy(env_ptr);
            \\        env_ptr.* = try CacheModule.load(gpa, {s}_env_bytes);
            \\        try envs.append(env_ptr);
            \\    }}
            \\
        , .{ stem, stem });
    }

    try writer.writeAll(
        \\
        \\    return envs.toOwnedSlice();
        \\}
        \\
    );

    const generated_file = compiled_builtins.add("embedded_envs.zig", code.items);
    return generated_file;
}
