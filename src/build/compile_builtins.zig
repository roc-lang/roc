//! Builtin .roc file compilation at build time
//!
//! Uses the bootstrap compiler to compile builtin .roc files and capture their
//! serialized ModuleEnvs.

const std = @import("std");
const Build = std.Build;
const Step = Build.Step;

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
pub fn generateEmbeddedEnvs(
    b: *Build,
    compiled_builtins: *Step.WriteFile,
    builtin_roc_files: []const []const u8,
) Build.LazyPath {
    var code = std.ArrayList(u8).init(b.allocator);
    const writer = code.writer();

    writer.writeAll(
        \\//! Generated file - embeds compiled builtin ModuleEnvs
        \\//! DO NOT EDIT - This file is auto-generated at build time
        \\
        \\const std = @import("std");
        \\const can = @import("can");
        \\const ModuleEnv = can.ModuleEnv;
        \\
        \\
    ) catch @panic("OOM");

    // Generate const for each .env file
    for (builtin_roc_files) |roc_file| {
        const stem = std.fs.path.stem(roc_file);
        const env_filename = b.fmt("{s}.env", .{roc_file});

        writer.print(
            \\/// Serialized ModuleEnv bytes for {s}
            \\pub const {s}_env_bytes = @embedFile("{s}");
            \\
        , .{ stem, stem, env_filename }) catch @panic("OOM");
    }

    writer.writeAll(
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
    ) catch @panic("OOM");

    // Generate deserialization code for each builtin
    for (builtin_roc_files) |roc_file| {
        const stem = std.fs.path.stem(roc_file);
        writer.print(
            \\    // Deserialize {s}
            \\    {{
            \\        const env_ptr = try gpa.create(ModuleEnv);
            \\        errdefer gpa.destroy(env_ptr);
            \\        env_ptr.* = try CacheModule.load(gpa, {s}_env_bytes);
            \\        try envs.append(env_ptr);
            \\    }}
            \\
        , .{ stem, stem }) catch @panic("OOM");
    }

    writer.writeAll(
        \\
        \\    return envs.toOwnedSlice();
        \\}
        \\
    ) catch @panic("OOM");

    const generated_file = compiled_builtins.add("embedded_envs.zig", code.items);
    return generated_file;
}
