//! A package representing multiple
const std = @import("std");
const base = @import("../base.zig");
const collections = @import("../collections.zig");
const utils = @import("../collections/utils.zig");
const path = std.fs.path;

const ParseRegion = base.ParseRegion;
const exitOnOom = collections.utils.exitOnOom;

const Self = @This();

const ROC_EXTENSION = ".roc";
const DEFAULT_MAIN_FILENAME = "main.roc";

/// The full download URL for the package, including the name, content hash, and version.
download_url: []u8,
/// The BLAKE3 hash of the tarball's contents.
content_hash: []u8,
/// The (usually semantic) version of this package, AKA 1.2.3.
version_string: []u8,
/// The absolute path to the root folder of this package.
absolute_dirpath: []u8,
/// All Roc modules in this package, irrespective if they are referenced at all from the root.
modules: Module.List,
/// Usually `main.roc` if it exists, or sometimes the application, e.g. `my-script.roc`.
root_module_idx: ?Module.Idx,
/// All non-Roc files in this package, relative to the package's root.
non_roc_relative_filepaths: std.ArrayList([]u8),
/// All packages depended on by this package.
dependencies: Dependency.List,

const List = collections.SafeList(@This());
pub const Idx = List.Idx;

pub const Module = struct {
    /// The full name of a module, e.g. `Foo.Bar`.
    name: []const u8,
    /// The absolute path to this module minus the folder path
    /// for the package's source code root.
    ///
    /// Though we could calculate this from the name whenever we need it,
    /// that would take an allocation that we'd like to avoid.
    filepath_relative_to_package_root: []const u8,
    /// Whether the module is a builtin module.
    is_builtin: bool,

    pub const List = collections.SafeList(@This());
    pub const Idx = Module.List.Idx;

    pub const NameError = error{
        NonAsciiPath,
        EmptyName,
        EmptyNameSegment,
        DotInPath,
    };

    pub fn from_relative_path(
        relative_path: []u8,
        is_builtin: bool,
        allocator: std.mem.Allocator,
    ) NameError!Module {
        var component_iter = try std.fs.path.componentIterator(relative_path);
        if (component_iter.peekNext() == null) {
            return error.EmptyName;
        }

        // The filepath should always be `Path/To/Module.roc`, meaning we can just
        // replace the separators (the / in this example) with dots and remove
        // the last four characters for the extension.
        std.debug.assert(relative_path.len > ROC_EXTENSION.len);
        var name = allocator.alloc(u8, relative_path.len - ROC_EXTENSION.len) catch exitOnOom();

        while (component_iter.next()) |component| {
            const not_last = component_iter.peekNext() != null;

            const segment = if (not_last) component.name else blk: {
                const extension = path.extension(component.name);
                break :blk if (std.mem.eql(u8, extension, ROC_EXTENSION))
                    path.stem(component.name)
                else
                    component.name;
            };

            for (segment) |char| {
                if (char == '.') {
                    return error.DotInPath;
                } else if (!std.ascii.isASCII(char)) {
                    // TODO: what is a legal module name?
                    return error.NonAsciiPath;
                }
            }

            std.mem.copyForwards(u8, name[name.len..], segment);
            if (not_last) {
                std.mem.copyForwards(u8, name[name.len], ".");
            }
        }

        return Self{
            .name = name,
            .filepath_relative_to_package_root = relative_path,
            .is_builtin = is_builtin,
        };
    }
};

pub const Dependency = struct {
    shorthand: []u8,
    package: Idx,

    pub const AddResult = union(enum) {
        Success,
        EmptyShorthand,
        DuplicateShorthand: Dependency,
        DuplicateUrl: Dependency,
    };

    pub const List = collections.SafeList(@This());
};

pub const Store = struct {
    packages: List,
    builtins_idx: Idx,
    primary_idx: Idx,
    arena: std.heap.ArenaAllocator,

    pub const InitResult = union(enum) {
        Success: Store,
        InvalidModuleName: struct {
            err: Module.NameError,
            filename: []u8,
        },
    };

    pub fn init(
        primary_root_module_absdir: []u8,
        primary_root_module_path: ?[]u8,
        primary_relative_filepaths: [][]u8,
        builtin_filenames: [][]u8,
        allocator: std.mem.Allocator,
    ) InitResult {
        const arena = std.heap.ArenaAllocator.init(allocator);

        var packages = List.init(arena);

        var builtins_package = Self{
            .download_url = &.{},
            .content_hash = &.{},
            .version_string = &.{},
            // TODO: set this to the cache path for builtins once they are saved there.
            .absolute_dirpath = &.{},
            .modules = Module.List.init(arena),
            .root_module_idx = null,
            .non_roc_relative_filepaths = std.ArrayList([]u8).init(arena),
            .dependencies = Dependency.List.init(arena),
        };
        const builtin_idx = packages.append(builtins_package);

        for (builtin_filenames) |builtin_filename| {
            const module = Module.from_relative_path(builtin_filename, true, arena) catch |err| {
                return .{ .InvalidModuleName = .{
                    .err = err,
                    .filename = builtin_filename,
                } };
            };
            const module_idx = builtins_package.modules.append(module);

            if (std.mem.eql(u8, builtin_filename, DEFAULT_MAIN_FILENAME)) {
                builtins_package.root_module_idx = module_idx;
            }
        }

        const primary_package = Self{
            .download_url = &.{},
            .content_hash = &.{},
            .version_string = &.{},
            .absolute_dirpath = primary_root_module_absdir,
            .modules = Module.List.init(arena),
            .root_module_idx = null,
            .non_roc_relative_filepaths = &.{},
            .dependencies = Dependency.List.init(arena),
        };
        const primary_idx = packages.append(primary_package);

        for (primary_relative_filepaths) |relative_path| {
            const module = Module.from_relative_path(relative_path, false, arena) catch |err| {
                return .{ .InvalidModuleName = .{
                    .err = err,
                    .filename = relative_path,
                } };
            };
            const module_idx = builtins_package.modules.append(module);

            if (std.mem.eql(u8, relative_path, primary_root_module_path)) {
                builtins_package.root_module_idx = module_idx;
            }
        }

        return Store{
            .packages = packages,
            .builtin_idx = builtin_idx,
            .primary_idx = primary_idx,
            .arena = arena,
        };
    }

    pub fn deinit(self: *Store) void {
        self.arena.deinit();
    }

    pub const ParseResult = struct {
        Success: Self,
    };

    pub fn parseFromUrl(url: []u8) ParseResult {
        _ = url;

        @panic("not implemented");
    }

    pub fn insert(self: *Store, package: Self) Idx {
        return self.packages.append(package);
    }

    pub fn addDependencyToPackage(
        self: *Store,
        package_idx: Idx,
        dependency: Dependency,
        shorthand: []u8,
    ) Dependency.AddResult {
        if (shorthand.len == 0) {
            return Dependency.AddResult{.EmptyShorthand};
        }

        const idx = @intFromEnum(package_idx);
        const pkg = self.packages.items.get(idx);
        var dep_iter = pkg.dependencies.iterator();
        while (dep_iter.next()) |entry| {
            if (std.meta.eql(entry.value_ptr.shorthand_region, dependency.shorthand_region)) {
                return Dependency.AddResult{
                    .DuplicateShorthand = entry.value_ptr.*,
                };
            }

            const dep_idx = @intFromEnum(entry.value_ptr.package);
            const dep_download_url = self.packages.items.items(.download_url)[dep_idx];
            if (std.mem.eql(u8, pkg.download_url, dep_download_url)) {
                return Dependency.AddResult{
                    .DuplicateUrl = entry.value_ptr.*,
                };
            }
        }

        // TODO: Insert the new dependency into the hashmap
        // Not sure what the dependency idx should be here
        // pkg.dependencies.put(dep_idx, dependency);

        return Dependency.AddResult.Success;
    }
};
