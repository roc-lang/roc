//! A potentially-imported module from the perspective of some siloed module.
//!
//! During early compiler stages, we only know about the contents of
//! a single module at a time, and this type represents a module import
//! that hasn't been resolved to a separate file yet.
const std = @import("std");
const problem = @import("../problem.zig");
const collections = @import("../collections.zig");

const Ident = @import("Ident.zig");
const Region = @import("Region.zig");
const Problem = problem.Problem;

const Module = @This();

/// The full name of a module, e.g. `Foo.Bar`.
name: []u8,
/// The shorthand for the package this module is imported from
/// if it is not from the current package, e.g. `json` in `json.Json`.
package_shorthand: ?[]u8,
/// Whether the module is a builtin module.
is_builtin: bool,
/// The list of all idents exposed by this module.
exposed_idents: collections.SafeList(Ident.Idx),

pub const List = collections.SafeMultiList(@This());
pub const Idx = List.Idx;

/// A store of all modules visible to a siloed module, including the
/// module itself and builtin modules.
pub const Store = struct {
    modules: List,
    ident_store: Ident.Store,
    allocator: std.mem.Allocator,

    pub const LookupResult = struct {
        module_idx: Idx,
        was_present: bool,
    };

    pub fn init(allocator: std.mem.Allocator) Store {
        var modules = collections.SafeMultiList(Module).init(allocator);
        _ = modules.append(Module{
            .name = &.{},
            .package_shorthand = null,
            .is_builtin = false,
            .exposed_idents = collections.SafeList(Ident.Idx).init(allocator),
        });

        // TODO: insert builtins automatically?

        return Store{
            .modules = modules,
            .ident_store = Ident.Store.init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Store) void {
        const modules = self.modules.items;
        for (0..self.modules.len()) |index| {
            var module = modules.get(index);
            module.exposed_idents.deinit();
        }
        self.modules.deinit();
        self.ident_store.deinit();
    }

    /// Search for a module that's visible to the main module.
    ///
    /// NOTE: This only works for modules in this package, so callers must
    /// first ensure that they are looking within the right package.
    pub fn lookup(
        self: *Store,
        name: []const u8,
        package_shorthand: ?[]u8,
    ) ?Idx {
        const items = self.modules.items;
        for (0..self.modules.len()) |index| {
            const item = items.get(index);
            if (std.mem.eql(u8, name, item.name)) {
                if (package_shorthand == null and item.package_shorthand == null) {
                    return @enumFromInt(@as(u32, @intCast(index)));
                } else if (package_shorthand != null and item.package_shorthand != null and std.mem.eql(u8, package_shorthand.?, item.package_shorthand.?)) {
                    return @enumFromInt(@as(u32, @intCast(index)));
                }
            }
        }

        return null;
    }

    /// Look up a module by name and package shorthand and return an [Idx],
    /// reusing an existing [Idx] if the module was already imported.
    pub fn getOrInsert(
        self: *Store,
        name: []u8,
        package_shorthand: ?[]u8,
    ) LookupResult {
        if (self.lookup(name, package_shorthand)) |idx| {
            return LookupResult{ .module_idx = idx, .was_present = true };
        } else {
            const idx = self.modules.append(Module{
                .name = name,
                .package_shorthand = package_shorthand,
                .is_builtin = false,
                .exposed_idents = collections.SafeList(Ident.Idx).init(self.allocator),
            });

            return LookupResult{ .module_idx = idx, .was_present = false };
        }
    }

    pub fn getName(self: *Store, idx: Idx) []u8 {
        return self.modules.items.items(.name)[@as(usize, @intFromEnum(idx))];
    }

    pub fn getPackageShorthand(self: *Store, idx: Idx) ?[]u8 {
        return self.modules.items.items(.package_shorthand)[@as(usize, @intFromEnum(idx))];
    }

    /// Add an ident to this modules list of exposed idents, reporting a problem
    /// if a duplicate is found.
    ///
    /// NOTE: This should not be called directly, but rather the [ModuleEnv.addExposedIdentForModule]
    /// method that will also set the ident's exposing module.
    pub fn addExposedIdent(
        self: *Store,
        module_idx: Module.Idx,
        ident: Ident.Idx,
        problems: *collections.SafeList(problem.Problem),
    ) void {
        const module_index = @intFromEnum(module_idx);
        var module = self.modules.items.get(module_index);

        for (module.exposed_idents.items.items) |exposed_ident| {
            if (std.meta.eql(exposed_ident, ident)) {
                _ = problems.append(Problem.Canonicalize.make(Problem.Canonicalize{ .DuplicateExposes = .{
                    .first_exposes = exposed_ident,
                    .duplicate_exposes = ident,
                } }));
                return;
            }
        }

        _ = module.exposed_idents.append(ident);
    }
};
