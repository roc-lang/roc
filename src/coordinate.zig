const std = @import("std");
const base = @import("base.zig");
const collections = @import("collections.zig");
const tokenize = @import("check/parse/tokenize.zig");
const parse = @import("check/parse.zig");
const can = @import("check/canonicalize.zig");
const resolve = @import("check/resolve_imports.zig");
const typecheck = @import("check/typecheck.zig");
const type_spec = @import("build/specialize_types.zig");
const func_lift = @import("build/lift_functions.zig");
const func_spec = @import("build/specialize_types.zig");
const func_solve = @import("build/solve_functions.zig");
const lower = @import("build/lower_statements.zig");
const refcount = @import("build/reference_count.zig");
const cache = @import("cache.zig");
const utils = @import("coordinate/utils.zig");
const ModuleGraph = @import("coordinate/ModuleGraph.zig");

const Package = base.Package;
const ModuleWork = base.ModuleWork;
const ModuleWorkIdx = base.ModuleWorkIdx;
const exitOnOom = collections.utils.exitOnOom;

const ResolveIR = resolve.IR;
const TypeSpecIR = type_spec.IR;
const RefCountIR = refcount.IR;

// to compile:
//
// load file, returning early on failure to load (e.g. missing file)
// parse just header, taking out:
// - exported idents
// - packages as pairs of shorthands and URLs
// - position to finish parsing rest of file from
// - if we are trying to build on top of typechecking, fail if module is not an app/platform/package
// for package in discovered packages, do the same as above recursively
// assemble a directed graph of packages and modules with the loaded module as the root

// we have a map where the key is the package "key" (pending what key we use)
// a package has these attributes
// - download URL
// - content hash
// - version string
// - a root file path
// - a collection of files with paths relative to the root file of the package (AKA `main.roc`)
//
// we have many external packages with the above qualities, and we also have a single "current" package
// it only has these attributes
// - a root file path
// - a collection of files with paths relative to the root file of the package (AKA `main.roc`)
//
// traverse graph of dependencies looking for a DAG relationship between the packages
// - If this isn't the case, show an error with the cyclic dependency
// reverse the dependency graph and traverse it for compilation order
//
// In full parallel (no inter-module dependencies considered):
// - get the base64-encoded BLAKE3 hash of each file
// - if we have a cached artifact of that module's hash for that Roc version:
//   - load it into an arena that contains the CanIR for that module with no more or less data in that arena
// - otherwise, parse and then (solo) canonicalize that module, and cache when finished
//
// For future compilation of stages, compile the modules in order of dependencies first, appending freshly-compiled modules to a full list of modules such that they are consistently ordered between stages and densely stored
//
// Then, in reverse order of the dependencies of each module (AKA dependencies first):
// - resolve imports
// - typecheck
//
// - if just typechecking, return here
// - if using the interpreter as a backend, use the typechecked ResolveIR to run the code as-is
// - if using LLVM as a backend, use the typechecked ResolveIR to compile constants to their values or to errors, keeping a mapping of generic constants to their possible types
//
// Afterwards, in reverse order of the dependencies of each module (AKA dependencies first):
// - specialize types
// - lift functions
// - solve functions
// - specialize functions
// - lower statements
// - reference count
//
// Lastly, pass all modules together to LLVM codegen

pub const TypeCheckResult = union(enum) {
    Success: Success,
    FailedToReadFile: struct { filepath: []u8 },
    FailedToLoadPackage: anyerror,
    FoundCycles: std.ArrayList(std.ArrayList(ModuleWork(can.IR))),
    // other errors...

    pub const Success = struct {
        packages: Package.Store,
        main_module_idx: ModuleWorkIdx,
        resolved: ModuleWork(ResolveIR).Store,
        typechecked: ModuleWork(ResolveIR).Store,
    };
};

pub fn typecheckModule(
    filepath: []u8,
    allocator: std.mem.Allocator,
) TypeCheckResult {
    const packages = switch (discoverModulesStartingFromRoot(filepath, allocator)) {
        .Success => |store| store,
    };

    const module_graph = ModuleGraph.fromPackages(packages, allocator);
    const sccs = module_graph.findStronglyConnectedComponents();

    const modules = switch (module_graph.putModulesInCompilationOrder(sccs, allocator)) {
        .Ordered => |modules| modules,
        .FoundCycles => |cycles| {
            return .{ .FoundCycles = cycles };
        },
    };

    sccs.groups.deinit();

    const resolve_irs = ModuleWork(ResolveIR).Store.init(allocator);
    for (modules.items) |module| {
        const resolved = resolve.resolveImports(module.work, &resolve_irs);
        resolve_irs.append(resolved) catch exitOnOom();
    }

    const typecheck_irs = ModuleWork(TypeCheckResult).Store.init(allocator);
    for (modules.items) |module| {
        const resolved = typecheck.checkTypes(module.work, &resolve_irs);
        typecheck_irs.insert(resolved) catch exitOnOom();
    }

    return TypeCheckResult{
        .Success = .{
            .packages = packages,
            // .main_module =
            .resolved = resolve_irs,
            .typechecked = typecheck_irs,
        },
    };
}

// pub const BuildResult = union(enum) {};

// pub fn prepareModuleForCodegen(
//     filepath: []u8,
//     allocator: std.mem.Allocator,
// ) BuildResult {
//     const typecheck_result = switch (typecheckModule(filepath, allocator)) {
//         .Success => |data| data,
//         else => {},
//     };
// }

pub const ModuleDiscoveryResult = union(enum) {
    Success: Package.Store,
    FailedToReadRootFile: utils.FileReadError,
    InvalidAbsPathForPackageRoot: std.fs.Dir.RealPathError,
    PackageContainedNonFile: std.fs.Dir.Entry,
};

fn discoverModulesStartingFromRoot(
    root_relative_path: []u8,
    allocator: std.mem.Allocator,
) ModuleDiscoveryResult {
    const contents = utils.readFile(root_relative_path, allocator) catch |err| {
        return .{ .FailedToReadRootFile = err };
    };
    const parsed_ir = parse.parse(allocator, contents);
    _ = parsed_ir;

    const root_abs_path = std.fs.realpathAlloc(allocator, root_relative_path) catch |err| {
        return ModuleDiscoveryResult{ .InvalidAbsPathForPackageRoot = err };
    };

    // TODO: check if this is the root of a package/app/platform or not,
    // AKA whether this file has other files and packages as dependencies
    //
    // If no (e.g. this is a one-off module), return success early with no deps
    const main_filepath = findMainRocRecursively(root_abs_path, allocator) orelse
        return ModuleDiscoveryResult{ .Success = Package.Store.init(root_abs_path, &.{}, allocator) };

    const relative_paths = switch (findAllFilesWithinPackageDir(main_filepath, allocator)) {
        .RelativeFiles => |paths| paths,
        .NonFileFound => |entry| return .{ .PackageContainedNonFile = entry },
    };

    const package_store = Package.Store.init(root_abs_path, relative_paths, allocator);
    // const imported_packages: [0]ImportedPackage = undefined;

    for (imported_packages) |imported_package| {
        const package = switch (Package.Store.parseFromUrl(imported_package.url)) {
            .Success => |pkg| pkg,
        };
        const idx = package_store.insert(package);
    }

    return ModuleDiscoveryResult{ .Success = package_store };
}

const DEFAULT_MAIN_FILENAME = "main.roc";

// TODO: implement
fn findMainRocRecursively(target_path: []u8, allocator: std.mem.Allocator) ?[]u8 {
    _ = target_path;
    _ = allocator;

    // while (true) {
    //     // const joined = std.fs.path.join(allocator: Allocator, paths: []const []const u8)
    //     const canonical = std.fs.cwd().realpathAlloc(allocator, DEFAULT_MAIN_FILENAME);
    //     // src_dir.join(DEFAULT_MAIN_NAME).canonicalize() {
    //     switch (canonical) {

    //         // Ok(main_roc) => break Some(main_roc),
    //         // Err(_) => {
    //         //     if !src_dir.pop() {
    //         //         // reached the root, no main.roc found
    //         //         *src_dir = original_src_dir;
    //         //         break None;
    //         //     }
    //         // }
    //     }
    // }

    return null;
}

pub const WalkPackageFilesResult = union(enum) {
    RelativeFiles: std.ArrayList([]u8),
    NonFileFound: std.fs.Dir.Entry,
};

fn findAllFilesWithinPackageDir(
    package_root_abspath: []u8,
    allocator: std.mem.Allocator,
) WalkPackageFilesResult {
    const dir = try std.fs.openDirAbsolute(package_root_abspath, .{
        .access_sub_paths = true,
        .iterate = true,
        // packages should have no symlinks, so don't follow them for better security!
        // (prevents reading files outside the package's tree)
        .no_follow = true,
    });

    var files = std.ArrayList([]u8).init(allocator);

    var dir_iter = dir.iterate();
    while (try dir_iter.next()) |entry| {
        switch (entry.kind) {
            .file => {
                // TODO: make sure name is relative to package root
                const name = allocator.dupe(u8, entry.name) catch exitOnOom();
                files.append(name) catch exitOnOom();
            },
            .directory => {
                // ignore
            },
            else => {
                const name = allocator.dupe(u8, entry.name) catch exitOnOom();
                return .{ .NonFileFound = .{ .name = name, .kind = entry.kind } };
            },
        }
    }

    return .{ .RelativeFiles = files };
}
