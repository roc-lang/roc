const std = @import("std");
const path = std.fs.path;
const collections = @import("../collections.zig");

const exitOnOom = collections.utils.exitOnOom;

const Self = @This();

/// The full name of a module, e.g. `Foo.Bar`.
name: []const u8,
/// The absolute path to this module minus the folder path
/// for the package's source code root.
///
/// Though we could calculate this from the name, that would
/// take an allocation that we'd like to avoid.
filepath_relative_to_package_root: []const u8,
/// Whether the module is a builtin module.
is_builtin: bool,

pub const ModuleNameError = error{
    NonAsciiPath,
    EmptyName,
    EmptyNameSegment,
    DotInPath,
};

const ROC_EXTENSION = ".roc";

pub fn from_relative_path(
    relative_path: []u8,
    is_builtin: bool,
    allocator: std.mem.Allocator,
) ModuleNameError!Self {
    var component_iter = try std.fs.path.componentIterator(relative_path);
    if (component_iter.peekNext() == null) {
        return error.EmptyName;
    }

    // The filepath should always be `Path/To/Module.roc`, meaning we can just
    // replace the separators (the / in this example) with dots and remove
    // the last four characters for the extension.
    std.debug.assert(relative_path.len >= ROC_EXTENSION.len);
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
