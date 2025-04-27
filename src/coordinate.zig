//! Coordination of all compilation stages in our compiler: the brains of the operation.

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
const func_solve = @import("build/solve_functions.zig");
const func_spec = @import("build/specialize_functions.zig");
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

/// The result of attempting to typecheck a module and all its dependencies.
pub const TypecheckResult = union(enum) {
    success: Success,
    err: Err,

    /// Data returned on the successful typechecking of a module.
    pub const Success = struct {
        packages: Package.Store,
        main_module_idx: ModuleWorkIdx,
        can_irs: ModuleWork(can.IR).Store,
        resolve_irs: ModuleWork(resolve.IR).Store,
        type_stores: ModuleWork(Type.Store).Store,
    };

    /// Failure to typecheck a module.
    pub const Err = union(enum) {
        package_root_search_err: PackageRootSearchErr,
        discovery_err: ModuleDiscoveryResult.Err,
        failed_to_build_module_graph: struct {
            err: Filesystem.ReadError,
            filename: []const u8,
        },
        could_not_find_root_module,
        found_cycle: struct {
            packages: Package.Store,
            cycle: std.ArrayList(ModuleWork(void)),
        },

        /// Deinitialize the memory for a `TypecheckResult.Err`.
        pub fn deinit(err: *Err, gpa: std.mem.Allocator) void {
            switch (err.*) {
                .package_root_search_err => {},
                .discovery_err => |*data| data.deinit(gpa),
                .failed_to_build_module_graph => |data| gpa.free(data.filename),
                .could_not_find_root_module => {},
                .found_cycle => |*data| {
                    data.packages.deinit();
                    data.cycle.deinit();
                },
            }
        }
    };
};

/// Check the types of a module and its dependencies.
pub fn typecheckModule(
    gpa: std.mem.Allocator,
    fs: Filesystem,
    entry_relative_path: []const u8,
) TypecheckResult {
    const discover_data = switch (discoverModulesStartingFromEntry(gpa, fs, entry_relative_path)) {
        .success => |data| data,
        .err => |err| return .{ .err = .{ .discovery_err = err } },
    };
    const packages = discover_data.packages;
    var root = discover_data.root;
    defer root.deinit();

    var module_graph = switch (ModuleGraph.fromPackages(gpa, fs, &packages)) {
        .success => |graph| graph,
        .failed_to_open_module => |data| return .{
            .err = .{
                .failed_to_build_module_graph = .{
                    .err = data.err,
                    .filename = data.filename,
                },
            },
        },
    };
    defer module_graph.deinit();

    const sccs = module_graph.findStronglyConnectedComponents(gpa);
    defer sccs.groups.deinit();

    const can_irs = switch (module_graph.putModulesInCompilationOrder(&sccs, gpa)) {
        .ordered => |modules| modules,
        .found_cycle => |cycle| {
            return .{ .err = .{ .found_cycle = .{
                .packages = packages,
                .cycle = cycle,
            } } };
        },
    };

    var main_module_idx: ?ModuleWorkIdx = null;
    var index_iter = can_irs.iterIndices();
    while (index_iter.next()) |idx| {
        const module = can_irs.getModule(idx, &packages);

        if (std.mem.eql(u8, module.filepath_relative_to_package_root, root.entry_relative_path)) {
            main_module_idx = idx;
        }
    }

    if (main_module_idx == null) {
        return .{ .err = .could_not_find_root_module };
    }

    const resolve_irs = ModuleWork(resolve.IR).Store.initFromCanIrs(gpa, &can_irs);
    index_iter = resolve_irs.iterIndices();
    while (index_iter.next()) |idx| {
        resolve.resolveImports(resolve_irs.getWork(idx), can_irs.getWork(idx), &resolve_irs);
    }

    const type_stores = ModuleWork(Type.Store).Store.initFromCanIrs(gpa, &can_irs);
    index_iter = type_stores.iterIndices();
    while (index_iter.next()) |idx| {
        check_types.checkTypes(type_stores.getWork(idx), resolve_irs.getWork(idx), &resolve_irs, &type_stores);
    }

    return .{
        .success = .{
            .packages = packages,
            .main_module_idx = main_module_idx.?,
            .can_irs = can_irs,
            .resolve_irs = resolve_irs,
            .type_stores = type_stores,
        },
    };
}

/// The result of attempting to prepare a module and its dependencies for codegen.
pub const BuildResult = union(enum) {
    success: Success,
    typecheck_err: TypecheckResult.Err,

    /// The data returned on a successful attempt to prepare a module for codegen.
    pub const Success = struct {
        packages: Package.Store,
        main_module_idx: ModuleWorkIdx,
        can_irs: ModuleWork(can.IR).Store,
        refcount_irs: ModuleWork(refcount.IR).Store,
    };
};

/// Check the types of a module and its dependencies and then prepare the typechecked modules for compilation
/// into a binary built by LLVM.
pub fn prepareModuleForCodegen(
    entry_filepath: []u8,
    fs: Filesystem,
    gpa: std.mem.Allocator,
) BuildResult {
    const typecheck_result = switch (typecheckModule(entry_filepath, fs, gpa)) {
        .success => |data| data,
        .err => |err| return .{ .typecheck_err = err },
    };

    const packages = typecheck_result.packages;
    const main_module_idx = typecheck_result.main_module_idx;
    const can_irs = typecheck_result.can_irs;
    const resolve_irs = typecheck_result.resolve_irs;
    const type_stores = typecheck_result.type_stores;

    const all_type_speced = ModuleWork(type_spec.IR).Store.initFromCanIrs(gpa, &can_irs);
    var index_iter = all_type_speced.iterIndices();
    while (index_iter.next()) |idx| {
        type_spec.specializeTypes(
            all_type_speced.getWork(idx),
            resolve_irs.getWork(idx),
            type_stores.getWork(idx),
            &all_type_speced,
        );
    }

    const all_func_lifted = ModuleWork(func_lift.IR).Store.initFromCanIrs(gpa, &can_irs);
    index_iter = all_func_lifted.iterIndices();
    while (index_iter.next()) |idx| {
        func_lift.liftFunctions(all_func_lifted.getWork(idx), all_type_speced.getWork(idx), &all_func_lifted);
    }

    const all_func_solved = ModuleWork(func_solve.IR).Store.initFromCanIrs(gpa, &can_irs);
    index_iter = all_func_solved.iterIndices();
    while (index_iter.next()) |idx| {
        func_solve.solveFunctions(all_func_solved.getWork(idx), all_func_lifted.getWork(idx), &all_func_solved);
    }

    const all_func_speced = ModuleWork(func_spec.IR).Store.initFromCanIrs(gpa, &can_irs);
    index_iter = all_func_speced.iterIndices();
    while (index_iter.next()) |idx| {
        func_spec.specializeFunctions(
            all_func_speced.getWork(idx),
            all_func_lifted.getWork(idx),
            all_func_solved.getWork(idx),
            &all_func_speced,
        );
    }

    const all_lowered = ModuleWork(lower.IR).Store.initFromCanIrs(gpa, &can_irs);
    index_iter = all_lowered.iterIndices();
    while (index_iter.next()) |idx| {
        lower.lowerStatements(all_lowered.getWork(idx), all_func_speced.getWork(idx), &all_lowered);
    }

    const all_refcounted = ModuleWork(refcount.IR).Store.initFromCanIrs(gpa, &can_irs);
    index_iter = all_refcounted.iterIndices();
    while (index_iter.next()) |idx| {
        refcount.referenceCount(all_refcounted.getWork(idx), all_lowered.getWork(idx), &all_refcounted);
    }

    return .{ .success = .{
        .packages = packages,
        .main_module_idx = main_module_idx,
        .can_irs = can_irs,
        .refcount_irs = all_refcounted,
    } };
}

/// The result of an attempt to discover all modules in all packages for compilation.
pub const ModuleDiscoveryResult = union(enum) {
    success: Success,
    err: Err,

    /// The data returned when all modules in all packages are successfully discovered.
    pub const Success = struct {
        packages: Package.Store,
        root: PackageRoot,
    };

    /// Errors that can occur when attempting to discover all modules in all used packages.
    pub const Err = union(enum) {
        package_root_search_err: PackageRootSearchErr,
        failed_to_open_root_dir: Filesystem.OpenError,
        init_packages: Package.Store.InitResult.Err,
        parse_deps: ParsePackageDepsErr,
        // TODO: model this error union
        failed_to_walk_files: anyerror,
        failed_to_canonicalize_root_file: Filesystem.CanonicalizeError,
        failed_to_read_root_file: Filesystem.OpenError,

        /// Deinitialize the memory for this `Err`.
        pub fn deinit(err: *Err, gpa: std.mem.Allocator) void {
            switch (err.*) {
                .package_root_search_err => {},
                .failed_to_open_root_dir => {},
                .init_packages => |*data| data.deinit(gpa),
                .parse_deps => {},
                .failed_to_walk_files => {},
                .failed_to_canonicalize_root_file => {},
                .failed_to_read_root_file => {},
            }
        }
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
    gpa: std.mem.Allocator,
    fs: Filesystem,
    entry_relative_path: []const u8,
) ModuleDiscoveryResult {
    var root = findRootOfPackage(entry_relative_path, fs, gpa) catch |err| {
        return .{ .err = .{ .package_root_search_err = err } };
    };

    var root_dir = fs.openDir(root.absolute_dir) catch |err| {
        return .{ .err = .{ .failed_to_open_root_dir = err } };
    };
    defer root_dir.close();

    var string_arena = std.heap.ArenaAllocator.init(gpa);
    const relative_paths = root_dir.findAllFilesRecursively(gpa, &string_arena) catch |err|
        return .{ .err = .{ .failed_to_walk_files = err } };

    var builtins = std.ArrayListUnmanaged([]const u8).initCapacity(gpa, BUILTIN_FILENAMES.len) catch |err| exitOnOom(err);
    builtins.appendSlice(gpa, BUILTIN_FILENAMES) catch |err| exitOnOom(err);

    var package_store = switch (Package.Store.init(
        gpa,
        root.absolute_dir,
        root.root_filepath(),
        relative_paths,
        builtins,
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

    while (desired_dep_queue.pop()) |next_dep| {
        const dep_package_idx = if (package_store.findWithUrl(next_dep.url)) |dep_idx|
            dep_idx
        else blk: {
            const url_data = Package.Url.parse(next_dep.url) catch |err| {
                // TODO: save a CoordinationError?
                _ = err catch {};

                continue;
            };
            const root_absdir = cache.getPackageRootAbsDir(url_data, gpa, fs);
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
            gpa,
            next_dep.parent_package,
            dep_package_idx,
            next_dep.shorthand,
            next_dep.shorthand_region,
        );
    }

    return .{ .success = .{
        .packages = package_store,
        .root = root,
    } };
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

/// Errors that can occur when trying to find the root of a package.
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

    var current_dirpath = gpa.dupe(u8, entry_dirpath) catch |err| exitOnOom(err);
    while (true) {
        var dir = fs.openDir(current_dirpath) catch break;
        const has_main = dir.hasFile(DEFAULT_MAIN_FILENAME) catch break;
        dir.close();

        if (has_main) {
            const entry_relative_to_dir = gpa.alloc(u8, entry_abs_path.len - current_dirpath.len - 1) catch |err| exitOnOom(err);
            std.mem.copyForwards(u8, entry_relative_to_dir, entry_abs_path[(current_dirpath.len + 1)..]);

            return PackageRoot{
                .absolute_dir = current_dirpath,
                .entry_relative_path = entry_relative_to_dir,
                .has_root_main_file = true,
                .gpa = gpa,
            };
        } else {
            const parent_dirpath = std.fs.path.resolve(gpa, &.{ current_dirpath, ".." }) catch break;
            // If these are the same, we have reached the root of the filesystem
            if (std.mem.eql(u8, current_dirpath, parent_dirpath)) break;

            gpa.free(current_dirpath);
            current_dirpath = parent_dirpath;
        }
    }

    // cannot naively reuse this since it may have been set to a parent dir
    gpa.free(current_dirpath);

    const alloced_entry_dirpath = gpa.dupe(u8, entry_dirpath) catch |err| exitOnOom(err);
    const entry_filename = gpa.dupe(u8, entry_abs_path[(entry_dirpath.len + 1)..]) catch |err| exitOnOom(err);

    return PackageRoot{
        .absolute_dir = alloced_entry_dirpath,
        .entry_relative_path = entry_filename,
        .has_root_main_file = false,
        .gpa = gpa,
    };
}

const ParsePackageDepsErr = union(enum) {
    failed_to_canonicalize_root_file: Filesystem.CanonicalizeError,
    failed_to_read_root_file: Filesystem.ReadError,
    malformed_header,
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

    var parse_ast = parse.parse(&env, contents);
    defer parse_ast.deinit();

    parse_ast.store.emptyScratch();
    const file = parse_ast.store.getFile();
    const header = parse_ast.store.getHeader(file.header);

    const package_list = switch (header) {
        .app => |app| parse.IR.NodeStore.RecordFieldSpan{ .span = parse_ast.store.getCollection(app.packages).span },
        .module => parse.IR.NodeStore.RecordFieldSpan{ .span = .{
            .start = 0,
            .len = 0,
        } },
        .package => |pkg| parse.IR.NodeStore.RecordFieldSpan{ .span = parse_ast.store.getCollection(pkg.packages).span },
        // TODO: get packages for hosted/platform modules once their headers are being parsed.
        .platform => |_| parse.IR.NodeStore.RecordFieldSpan{ .span = .{
            .start = 0,
            .len = 0,
        } },
        .hosted => |_| parse.IR.NodeStore.RecordFieldSpan{ .span = .{
            .start = 0,
            .len = 0,
        } },
        .malformed => {
            return ParsePackageDepsErr.malformed_header;
        },
    };

    for (parse_ast.store.recordFieldSlice(package_list)) |package_import| {
        const import = parse_ast.store.getRecordField(package_import);

        // TODO: get URL when it is stored in `StringLiteral.Store`
        const url = gpa.dupe(u8, "") catch |err| exitOnOom(err);
        const url_region = Region.zero();

        desired_dep_queue.append(DesiredPackageDep{
            .parent_package = package_idx,
            .shorthand = gpa.dupe(u8, parse_ast.resolve(import.name)) catch |err| exitOnOom(err),
            .shorthand_region = parse_ast.tokens.resolve(import.name),
            .url = url,
            .url_region = url_region,
        }) catch |err| exitOnOom(err);
    }

    return null;
}
