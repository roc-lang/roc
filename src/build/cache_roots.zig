//! Where `build-ci` leaves the cache entries each top-level step reads.
//!
//! A MiniCI shard restores only the part of `build-ci`'s cache that the steps
//! it runs read (see src/build/minici_pack.zig). `addOptions` walks the step
//! graph, finds every step under `build-ci` whose result lives in a cache
//! entry, and makes an Options module that names each such entry's path,
//! as the build runner resolves it, and for every top-level step which of
//! those entries its graph contains. Building the module after `build-ci`
//! is a cache hit for every step it names, so it only reads where they are.

const std = @import("std");
const Step = std.Build.Step;
const LazyPath = std.Build.LazyPath;

/// What a step's cache lookup lands on.
const CacheResult = union(enum) {
    /// The step runs every time it is in the graph, or caches nothing.
    none,
    /// The step is cached, and this path lies in its cache entry.
    entry: LazyPath,
    /// The step is cached, but it declares no output whose path names its
    /// entry, so nothing can say where that entry is.
    unaddressable,
};

fn generated(index: std.Build.Configuration.GeneratedFileIndex) LazyPath {
    return .{ .generated = .{ .index = index } };
}

/// Mirrors how the build runner decides whether a Run step has side effects
/// (`lib/compiler/Maker/Step/Run.zig` in Zig 0.17): only a Run step without
/// them is cached.
fn runCacheResult(run: *Step.Run) CacheResult {
    var output: ?std.Build.Configuration.GeneratedFileIndex = null;
    var passthru = false;
    for (run.argv.items) |arg| switch (arg) {
        .output_file, .output_file_dep, .output_directory => |out| {
            if (output == null) output = out.generated_file;
        },
        .passthru => passthru = true,
        .artifact,
        .lazy_path,
        .decorated_directory,
        .file_content,
        .bytes,
        .enable_darling,
        .enable_qemu,
        .enable_rosetta,
        .enable_wasmtime,
        .enable_wine,
        => {},
    };
    const captured = run.captured_stdout orelse run.captured_stderr;
    const has_side_effects = run.has_side_effects or passthru or switch (run.stdio) {
        .infer_from_args => output == null and captured == null,
        .inherit => true,
        .check, .zig_test, .protocol => false,
    };
    if (has_side_effects) return .none;
    if (output) |index| return .{ .entry = generated(index) };
    if (captured) |c| return .{ .entry = generated(c.generated_file) };
    return .unaddressable;
}

fn cacheResult(b: *std.Build, step: *Step) CacheResult {
    switch (step.tag) {
        .compile => {
            // Any output the compilation already emits names its entry.
            // Requesting one it does not emit would change what it builds.
            const compile = step.cast(Step.Compile).?;
            for ([_]std.Build.Configuration.OptionalGeneratedFileIndex{
                compile.generated_bin,
                compile.generated_h,
                compile.generated_docs,
                compile.generated_asm,
                compile.generated_pdb,
                compile.generated_implib,
                compile.generated_llvm_bc,
                compile.generated_llvm_ir,
                compile.emit_directory,
            }) |optional| {
                if (optional.unwrap()) |index| return .{ .entry = generated(index) };
            }
            // A compilation that emits nothing (a compile-time check) still
            // has an entry. Its emit directory names it without changing what
            // it builds: the build runner resolves that path to the entry and
            // passes the compiler nothing for it.
            const index = b.graph.addGeneratedFile(&compile.step);
            compile.emit_directory = .init(index);
            return .{ .entry = generated(index) };
        },
        .translate_c => return .{ .entry = step.cast(Step.TranslateC).?.getOutput() },
        .options => return .{ .entry = step.cast(Step.Options).?.getOutput() },
        .config_header => return .{ .entry = step.cast(Step.ConfigHeader).?.getOutputDir() },
        .obj_copy => return .{ .entry = step.cast(Step.ObjCopy).?.getOutput() },
        .write_file => {
            const write_file = step.cast(Step.WriteFile).?;
            return switch (write_file.mode) {
                .whole_cached => .{ .entry = write_file.getDirectory() },
                .tmp, .mutate => .none,
            };
        },
        .run => return runCacheResult(step.cast(Step.Run).?),
        .check_file,
        .fail,
        .find_program,
        .fmt,
        .install_artifact,
        .install_dir,
        .install_file,
        .top_level,
        .update_source_files,
        => return .none,
    }
}

/// The steps `step` depends on: its declared dependencies, and for a Compile
/// step those its module graph implies. The build runner adds the latter only
/// when it serializes the graph, so this mirrors that
/// (`createModuleDependenciesForStep` in `lib/std/Build/Serialize.zig` in Zig
/// 0.17).
fn appendDependencies(b: *std.Build, step: *Step, out: *std.ArrayList(*Step)) void {
    const gpa = b.allocator;
    out.appendSlice(gpa, step.dependencies.items) catch @panic("OOM");
    const compile = step.cast(Step.Compile) orelse return;

    // `getEmittedIncludeTree` can add a generated file, so the list is read
    // at each use.
    const build_graph = b.graph;
    const Lazy = struct {
        fn add(graph: *std.Build.Graph, list: *std.ArrayList(*Step), allocator: std.mem.Allocator, lazy_path: LazyPath) void {
            switch (lazy_path) {
                .generated => |gen| list.append(allocator, graph.generated_files.items[@backingInt(gen.index)]) catch @panic("OOM"),
                .src_path, .cwd_relative, .relative, .dependency => {},
            }
        }
    };
    for (compile.root_module.getGraph().modules) |module| {
        if (module.root_source_file) |lp| Lazy.add(build_graph, out, gpa, lp);
        for (module.include_dirs.items) |include_dir| switch (include_dir) {
            .path,
            .path_system,
            .path_after,
            .framework_path,
            .framework_path_system,
            .embed_path,
            => |lp| Lazy.add(build_graph, out, gpa, lp),
            .other_step => |other| {
                Lazy.add(build_graph, out, gpa, other.getEmittedIncludeTree());
                out.append(gpa, &other.step) catch @panic("OOM");
            },
            .config_header_step => |other| out.append(gpa, &other.step) catch @panic("OOM"),
        };
        for (module.lib_paths.items) |lp| Lazy.add(build_graph, out, gpa, lp);
        for (module.rpaths.items) |rpath| switch (rpath) {
            .lazy_path => |lp| Lazy.add(build_graph, out, gpa, lp),
            .special => {},
        };
        for (module.link_objects.items) |link_object| switch (link_object) {
            .static_path, .assembly_file => |lp| Lazy.add(build_graph, out, gpa, lp),
            .other_step => |other| out.append(gpa, &other.step) catch @panic("OOM"),
            .system_lib => {},
            .c_source_file => |source| Lazy.add(build_graph, out, gpa, source.file),
            .c_source_files => |source_files| Lazy.add(build_graph, out, gpa, source_files.root),
            .win32_resource_file => |rc_source| {
                Lazy.add(build_graph, out, gpa, rc_source.file);
                for (rc_source.include_paths) |lp| Lazy.add(build_graph, out, gpa, lp);
            },
        };
    }
}

/// Every step's dependencies, computed once.
const Graph = struct {
    dependencies: std.AutoHashMapUnmanaged(*Step, []const *Step) = .empty,

    fn of(self: *Graph, b: *std.Build, step: *Step) []const *Step {
        const entry = self.dependencies.getOrPut(b.allocator, step) catch @panic("OOM");
        if (!entry.found_existing) {
            var list: std.ArrayList(*Step) = .empty;
            appendDependencies(b, step, &list);
            entry.value_ptr.* = list.items;
        }
        return entry.value_ptr.*;
    }
};

/// Every step reachable from `root`, `root` included.
fn closure(b: *std.Build, graph: *Graph, root: *Step) std.AutoArrayHashMapUnmanaged(*Step, void) {
    var reached: std.AutoArrayHashMapUnmanaged(*Step, void) = .empty;
    var pending: std.ArrayList(*Step) = .empty;
    pending.append(b.allocator, root) catch @panic("OOM");
    while (pending.pop()) |step| {
        if ((reached.getOrPut(b.allocator, step) catch @panic("OOM")).found_existing) continue;
        pending.appendSlice(b.allocator, graph.of(b, step)) catch @panic("OOM");
    }
    return reached;
}

/// The numbers (in `cached`) of the cached steps each step's graph contains,
/// computed once per step.
const Reach = struct {
    cached: *const std.AutoHashMapUnmanaged(*Step, usize),
    sets: std.AutoHashMapUnmanaged(*Step, std.DynamicBitSetUnmanaged) = .empty,

    fn of(self: *Reach, b: *std.Build, graph: *Graph, root: *Step) std.DynamicBitSetUnmanaged {
        const gpa = b.allocator;
        const Pending = struct { step: *Step, dependencies_done: bool };
        var pending: std.ArrayList(Pending) = .empty;
        defer pending.deinit(gpa);
        pending.append(gpa, .{ .step = root, .dependencies_done = false }) catch @panic("OOM");
        while (pending.pop()) |item| {
            if (self.sets.contains(item.step)) continue;
            const dependencies = graph.of(b, item.step);
            if (!item.dependencies_done) {
                pending.append(gpa, .{ .step = item.step, .dependencies_done = true }) catch @panic("OOM");
                for (dependencies) |dependency| {
                    if (!self.sets.contains(dependency)) pending.append(gpa, .{ .step = dependency, .dependencies_done = false }) catch @panic("OOM");
                }
                continue;
            }
            var set = std.DynamicBitSetUnmanaged.initEmpty(gpa, self.cached.count()) catch @panic("OOM");
            if (self.cached.get(item.step)) |number| set.set(number);
            for (dependencies) |dependency| set.setUnion(self.sets.get(dependency).?);
            self.sets.put(gpa, item.step, set) catch @panic("OOM");
        }
        return self.sets.get(root).?;
    }
};

/// The top-level step whose cache entries shard artifacts carry.
const build_ci_step_name = "build-ci";

fn joinNames(b: *std.Build, names: []const []const u8) []const u8 {
    return std.mem.join(b.allocator, "\n", names) catch @panic("OOM");
}

/// Returns the Options step described in the file comment. Its module has:
///
/// - `local_cache`, `global_cache`, `zig_lib`, `cwd`, `build_root` and
///   `install_prefix`: those directories as the build runner resolves them;
/// - `output_0` through `output_<output_count - 1>`: a path in each cache
///   entry `build-ci` leaves;
/// - `top_level_steps`, and per top-level step, in the same order,
///   `top_level_outputs` (the space-separated indices of the outputs its
///   graph contains) and `top_level_unaddressable` (the newline-separated
///   names of cached steps in both its graph and `build-ci`'s whose entry
///   no output names).
///
/// The entries are those of `build-ci`, the step CI builds MiniCI with, which
/// is looked up by name so that no other step's graph can stand in for it.
/// Call it after every step is defined.
pub fn addOptions(b: *std.Build) *Step.Options {
    const gpa = b.allocator;
    const build_ci = &(b.top_level_steps.get(build_ci_step_name) orelse
        std.debug.panic("cache_roots.addOptions needs the `{s}` step", .{build_ci_step_name})).step;
    const options = b.addOptions();
    inline for (.{ "local_cache", "global_cache", "zig_lib", "cwd", "install_prefix" }) |base| {
        options.addOptionPathUntracked(base, .{ .relative = .{ .base = @field(std.Build.Configuration.LazyPath.Relative.Base, base) } });
    }
    options.addOptionPathUntracked("build_root", b.path("."));

    var graph: Graph = .{};
    const built = closure(b, &graph, build_ci);

    // A number for each cached step `build-ci` runs, and an output index for
    // each one whose entry an output names.
    var cached: std.AutoHashMapUnmanaged(*Step, usize) = .empty;
    var cached_steps: std.ArrayList(*Step) = .empty;
    var output_of_cached: std.ArrayList(?usize) = .empty;
    var output_count: usize = 0;
    for (built.keys()) |step| {
        const output: ?usize = switch (cacheResult(b, step)) {
            .none => continue,
            .entry => |lp| output: {
                options.addOptionPathUntracked(b.fmt("output_{d}", .{output_count}), lp);
                output_count += 1;
                break :output output_count - 1;
            },
            .unaddressable => null,
        };
        cached.put(gpa, step, cached_steps.items.len) catch @panic("OOM");
        cached_steps.append(gpa, step) catch @panic("OOM");
        output_of_cached.append(gpa, output) catch @panic("OOM");
    }
    options.addOption(usize, "output_count", output_count);

    var reach: Reach = .{ .cached = &cached };
    var names: std.ArrayList([]const u8) = .empty;
    var outputs: std.ArrayList([]const u8) = .empty;
    var unaddressable_names: std.ArrayList([]const u8) = .empty;
    for (b.top_level_steps.keys(), b.top_level_steps.values()) |name, top_level| {
        const reached = reach.of(b, &graph, &top_level.step);
        var indices: std.ArrayList(u8) = .empty;
        var missing: std.ArrayList([]const u8) = .empty;
        var it = reached.iterator(.{});
        while (it.next()) |number| {
            if (output_of_cached.items[number]) |index| {
                if (indices.items.len != 0) indices.append(gpa, ' ') catch @panic("OOM");
                indices.print(gpa, "{d}", .{index}) catch @panic("OOM");
            } else {
                missing.append(gpa, cached_steps.items[number].name) catch @panic("OOM");
            }
        }
        names.append(gpa, name) catch @panic("OOM");
        outputs.append(gpa, indices.items) catch @panic("OOM");
        unaddressable_names.append(gpa, joinNames(b, missing.items)) catch @panic("OOM");
    }
    options.addOption([]const []const u8, "top_level_steps", names.items);
    options.addOption([]const []const u8, "top_level_outputs", outputs.items);
    options.addOption([]const []const u8, "top_level_unaddressable", unaddressable_names.items);
    return options;
}
