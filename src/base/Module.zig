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
    allocator: std.mem.Allocator,

    pub const LookupResult = struct {
        module_idx: Idx,
        was_present: bool,
    };

    pub fn init(allocator: std.mem.Allocator) Store {
        const modules = collections.SafeMultiList(Module).init(allocator);
        modules.append(Module{
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
        for (self.modules.items.items(.ident_store)) |store| {
            store.deinit();
        }

        self.modules.deinit();
    }

    /// Search for a module that's visible to the main module.
    ///
    /// NOTE: This only works for modules in this package, so callers must
    /// first ensure that they are looking within the right package.
    pub fn lookup(
        self: *Store,
        name: []const u8,
        package_shorthand: ?[]const u8,
    ) ?Idx {
        const items = self.modules.items;

        for (0..self.modules.len()) |index| {
            const other_name = items.items(.name_segments)[index];
            if (name == other_name) {
                const other_package_shorthand = items.items(.package_shorthand)[index];
                if (other_package_shorthand == package_shorthand) {
                    return @enumFromInt(@as(u32, index));
                }
            }
        }

        return null;
    }

    /// Look up a module by name and package shorthand and return an [Idx],
    /// reusing an existing [Idx] if the module was already imported.
    pub fn getOrInsert(
        self: *Store,
        name: []const u8,
        package_shorthand: ?[]const u8,
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
        module: Module.Idx,
        ident: Ident.Idx,
        problems: *collections.SafeList(problem.Problem),
    ) void {
        const module_index = @intFromEnum(module);
        const module_exposed_idents = self.modules.items.items(.exposed_idents)[module_index];
        for (module_exposed_idents) |exposed_ident| {
            if (exposed_ident == ident) {
                problems.append(Problem.Canonicalize.make(.DuplicateExposes{
                    .first_exposes = exposed_ident,
                    .duplicate_exposes = ident,
                }));
                return;
            }
        }

        module_exposed_idents.append(ident);
    }
};
