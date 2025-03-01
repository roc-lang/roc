const std = @import("std");
const base = @import("base.zig");
const cache = @import("cache.zig");
const types = @import("types.zig");
const collections = @import("collections.zig");
const tokenize = @import("check/parse/tokenize.zig");
const parse = @import("check/parse.zig");
const can = @import("check/canonicalize.zig");
const resolve = @import("check/resolve_imports.zig");
const check_types = @import("check/check_types.zig");
const type_spec = @import("build/specialize_types.zig");
const func_lift = @import("build/lift_functions.zig");
const func_spec = @import("build/specialize_types.zig");
const func_solve = @import("build/solve_functions.zig");
const lower = @import("build/lower_statements.zig");
const refcount = @import("build/reference_count.zig");
const utils = @import("coordinate/utils.zig");
const Filesystem = @import("coordinate/Filesystem.zig");
const ModuleGraph = @import("coordinate/ModuleGraph.zig");

const Region = base.Region;
const Package = base.Package;
const PackageUrl = base.PackageUrl;
const ModuleWork = base.ModuleWork;
const ModuleWorkIdx = base.ModuleWorkIdx;
const Type = types.Type;
const Allocator = std.mem.Allocator;
const exitOnOom = collections.utils.exitOnOom;

const DEFAULT_MAIN_FILENAME: []const u8 = "main.roc";
const BUILTIN_FILENAMES: []const []const u8 = &.{};

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

// pub const CoordinateProblem = union(enum) {
//     empty_shorthand: struct {
//         in_package: Package.Idx,
//         module: Package.Module.Idx,
//         shorthand_region: Region,
//     },
//     duplicate_shorthand: struct {
//         in_package: Package.Idx,
//         module: Package.Module.Idx,
//         first_shorthand_region: Region,
//         shadowing_shorthand_region: Region,
//     },
// };

pub const TypecheckResult = union(enum) {
    success: Success,
    err: Err,

    pub const Success = struct {
        packages: Package.Store,
        main_module_idx: ModuleWorkIdx,
        resolved: ModuleWork(resolve.IR).Store,
        typechecked: ModuleWork(Type.Store).Store,
    };

    pub const Err = union(enum) {
        package_root_search_err: PackageRootSearchErr,
        discovery_err: ModuleDiscoveryResult.Err,
        failed_to_build_module_graph: Filesystem.OpenError,
        found_cycle: struct {
            packages: Package.Store,
            cycle: std.ArrayList(ModuleWork(can.IR)),
        },
        // other errors...
    };
};

pub fn typecheckModule(
    entry_relative_path: []const u8,
    fs: Filesystem,
    gpa: std.mem.Allocator,
) TypecheckResult {
    var packages = switch (discoverModulesStartingFromEntry(entry_relative_path, fs, gpa)) {
        .success => |store| store,
        .err => |err| return .{ .err = .{ .discovery_err = err } },
    };

    var module_graph = switch (ModuleGraph.fromPackages(&packages, fs, gpa)) {
        .success => |graph| graph,
        .failed_to_open_module => |err| return .{ .err = .{ .failed_to_build_module_graph = err } },
    };
    defer module_graph.deinit();

    const sccs = module_graph.findStronglyConnectedComponents(gpa);
    defer sccs.groups.deinit();

    const modules = switch (module_graph.putModulesInCompilationOrder(&sccs, gpa)) {
        .ordered => |modules| modules,
        .found_cycle => |cycle| {
            return .{ .err = .{ .found_cycle = .{
                .packages = packages,
                .cycle = cycle,
            } } };
        },
    };

    var resolve_irs = ModuleWork(resolve.IR).Store.init(gpa);
    for (modules.items) |module| {
        const resolved = resolve.resolveImports(&module.work, &resolve_irs);
        _ = resolve_irs.insert(can.IR, &module, resolved);
    }

    var typecheck_irs = ModuleWork(Type.Store).Store.init(gpa);
    for (resolve_irs.items.items) |module| {
        const type_store = check_types.checkTypes(&module.work, &resolve_irs, &typecheck_irs);
        _ = typecheck_irs.insert(resolve.IR, &module, type_store);
    }

    return .{
        .success = .{
            .packages = packages,
            // TODO: set this based on the actual main module idx
            .main_module_idx = @enumFromInt(0),
            .resolved = resolve_irs,
            .typechecked = typecheck_irs,
        },
    };
}

// pub const BuildResult = union(enum) {};

// pub fn prepareModuleForCodegen(
//     filepath: []u8,
//     gpa: std.mem.Allocator,
// ) BuildResult {
//     const typecheck_result = switch (typecheckModule(filepath, gpa)) {
//         .Success => |data| data,
//         else => {},
//     };
// }

pub const ModuleDiscoveryResult = union(enum) {
    success: Package.Store,
    err: Err,

    pub const Err = union(enum) {
        package_root_search_err: PackageRootSearchErr,
        failed_to_open_root_dir: Filesystem.OpenError,
        init_packages: Package.Store.InitResult.Err,
        parse_deps: ParsePackageDepsErr,
        // TODO: model this error union
        failed_to_walk_files: anyerror,
        failed_to_canonicalize_root_file: Filesystem.CanonicalizeError,
        failed_to_read_root_file: Filesystem.OpenError,
    };
};

const DesiredPackageDep = struct {
    parent_package: Package.Idx,
    shorthand: []const u8,
    shorthand_region: Region,
    url: []const u8,
    url_region: Region,
};

fn discoverModulesStartingFromEntry(
    entry_relative_path: []const u8,
    fs: Filesystem,
    gpa: std.mem.Allocator,
) ModuleDiscoveryResult {
    var root = findRootOfPackage(entry_relative_path, fs, gpa) catch |err| {
        return .{ .err = .{ .package_root_search_err = err } };
    };
    defer root.deinit();

    var root_dir = fs.openDir(root.absolute_dir) catch |err| {
        return .{ .err = .{ .failed_to_open_root_dir = err } };
    };
    defer root_dir.close();

    var string_arena = std.heap.ArenaAllocator.init(gpa);
    const relative_paths = root_dir.findAllFilesRecursively(gpa, &string_arena) catch |err|
        return .{ .err = .{ .failed_to_walk_files = err } };

    var builtins = std.ArrayList([]const u8).initCapacity(gpa, BUILTIN_FILENAMES.len) catch exitOnOom();
    builtins.appendSlice(BUILTIN_FILENAMES) catch exitOnOom();

    var package_store = switch (Package.Store.init(
        root.absolute_dir,
        root.root_filepath(),
        relative_paths,
        builtins,
        gpa,
    )) {
        .success => |store| store,
        .err => |err| return .{ .err = .{ .init_packages = err } },
    };

    var desired_dep_queue = std.ArrayList(DesiredPackageDep).init(gpa);

    if (parseDependenciesFromPackageRoot(
        &root_dir,
        root,
        Package.Store.primary_idx,
        &desired_dep_queue,
        fs,
        gpa,
    )) |err| return .{ .err = .{ .parse_deps = err } };

    while (desired_dep_queue.popOrNull()) |next_dep| {
        const dep_package_idx = if (package_store.findWithUrl(next_dep.url)) |dep_idx|
            dep_idx
        else blk: {
            const url_data = Package.Url.parse(next_dep.url) catch |err| {
                // TODO: save a CoordinationError?
                _ = err catch {};

                continue;
            };
            const root_absdir = cache.getPackageRootAbsDir(url_data, gpa);
            defer gpa.free(root_absdir);

            var dir = fs.openDir(root_absdir) catch |err| {
                // TODO: save a CoordinationError?
                _ = err catch {};

                continue;
            };
            defer dir.close();

            const rel_paths = dir.findAllFilesRecursively(gpa, &string_arena) catch |err| {
                // TODO: save a CoordinationError?
                _ = err catch {};

                continue;
            };

            break :blk package_store.add(
                url_data,
                root_absdir,
                DEFAULT_MAIN_FILENAME,
                rel_paths,
                gpa,
            ) catch |err| {
                // TODO: save a CoordinationError?
                _ = err catch {};

                continue;
            };
        };

        package_store.addDependencyToPackage(
            next_dep.parent_package,
            dep_package_idx,
            next_dep.shorthand,
            next_dep.shorthand_region,
        );
    }

    return .{ .success = package_store };
}

const PackageRoot = struct {
    absolute_dir: []const u8,
    entry_relative_path: []const u8,
    has_root_main_file: bool,
    gpa: std.mem.Allocator,

    pub fn root_filepath(self: *const PackageRoot) []const u8 {
        return if (self.has_root_main_file) DEFAULT_MAIN_FILENAME else self.entry_relative_path;
    }

    pub fn deinit(self: *PackageRoot) void {
        self.gpa.free(self.absolute_dir);
        self.gpa.free(self.entry_relative_path);
    }
};

pub const PackageRootSearchErr = error{
    invalid_abs_path_for_entry,
    entry_not_in_a_directory,
    could_not_find_entry_in_package,
};

fn findRootOfPackage(
    entry_relative_path: []const u8,
    fs: Filesystem,
    gpa: std.mem.Allocator,
) PackageRootSearchErr!PackageRoot {
    const entry_abs_path = fs.canonicalize(entry_relative_path, gpa) catch
        return error.invalid_abs_path_for_entry;
    defer gpa.free(entry_abs_path);

    const entry_dirpath = fs.dirName(entry_abs_path) orelse
        return error.entry_not_in_a_directory;

    var current_dirpath = gpa.dupe(u8, entry_dirpath) catch exitOnOom();
    while (true) {
        var dir = fs.openDir(current_dirpath) catch break;
        const has_main = dir.hasFile(DEFAULT_MAIN_FILENAME) catch break;
        dir.close();

        if (has_main) {
            const entry_relative_to_dir = gpa.alloc(u8, entry_abs_path.len - current_dirpath.len) catch exitOnOom();
            std.mem.copyForwards(u8, entry_relative_to_dir, entry_abs_path[current_dirpath.len..]);

            return PackageRoot{
                .absolute_dir = current_dirpath,
                .entry_relative_path = entry_relative_to_dir,
                .has_root_main_file = true,
                .gpa = gpa,
            };
        } else {
            const parent_dirpath = std.fs.path.resolve(gpa, &.{ current_dirpath, ".." }) catch break;
            gpa.free(current_dirpath);
            current_dirpath = parent_dirpath;
        }
    }

    // cannot naively reuse this since it may have been set to a parent dir
    gpa.free(current_dirpath);

    const alloced_entry_dirpath = gpa.dupe(u8, entry_dirpath) catch exitOnOom();
    const entry_filename = gpa.dupe(u8, entry_abs_path[entry_dirpath.len..]) catch exitOnOom();

    return PackageRoot{
        .absolute_dir = alloced_entry_dirpath,
        .entry_relative_path = entry_filename,
        .has_root_main_file = false,
        .gpa = gpa,
    };
}

const ParsePackageDepsErr = union(enum) {
    failed_to_canonicalize_root_file: Filesystem.CanonicalizeError,
    failed_to_read_root_file: Filesystem.OpenError,
};

fn parseDependenciesFromPackageRoot(
    package_dir: *Filesystem.Dir,
    root: PackageRoot,
    package_idx: Package.Idx,
    desired_dep_queue: *std.ArrayList(DesiredPackageDep),
    fs: Filesystem,
    gpa: Allocator,
) ?ParsePackageDepsErr {
    const abspath = package_dir.canonicalize(root.root_filepath(), gpa) catch |err|
        return .{ .failed_to_canonicalize_root_file = err };
    defer gpa.free(abspath);

    const contents = fs.readFile(abspath, gpa) catch |err|
        return .{ .failed_to_read_root_file = err };
    defer gpa.free(contents);

    var env = base.ModuleEnv.init(gpa);
    defer env.deinit();

    var parse_ast = parse.parse(&env, gpa, contents);
    defer parse_ast.deinit();

    const file = parse_ast.store.getFile();
    const header = parse_ast.store.getHeader(file.header);

    const package_list = switch (header) {
        .app => |app| app.packages,
        .module => &.{},
        .package => |pkg| pkg.packages,
        // TODO: get packages for hosted/platform modules once their headers are being parsed.
        .platform => |_| &.{},
        .hosted => |_| &.{},
    };

    for (package_list) |package_import| {
        const import = parse_ast.store.getRecordField(package_import);

        // TODO: get URL when it is stored in `StringLiteral.Store`
        const url = gpa.dupe(u8, "") catch exitOnOom();
        const url_region = Region.zero();

        desired_dep_queue.append(DesiredPackageDep{
            .parent_package = package_idx,
            .shorthand = gpa.dupe(u8, parse_ast.resolve(import.name)) catch exitOnOom(),
            .shorthand_region = parse_ast.tokens.resolve(import.name),
            .url = url,
            .url_region = url_region,
        }) catch exitOnOom();
    }

    return null;
}
