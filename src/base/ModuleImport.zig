//! A potentially-imported module from the perspective of some siloed module.
//!
//! During early compiler stages, we only know about the contents of
//! a single module at a time, and this type represents a module import
//! that hasn't been resolved to a separate source file yet.
const std = @import("std");
const base = @import("../base.zig");
const problem = @import("../problem.zig");
const collections = @import("../collections.zig");

const Ident = base.Ident;
const Package = base.Package;
const Problem = problem.Problem;
const exitOnOom = collections.utils.exitOnOom;

const Self = @This();

/// The full name of a module, e.g. `Foo.Bar`.
name: []const u8,
/// The shorthand for the package this module is imported from
/// if it is not from the current package, e.g. `json` in `json.Json`.
package_shorthand: ?[]const u8,
/// The list of all idents exposed via this module import.
exposed_idents: collections.SafeList(Ident.Idx),
/// The real package and module that this import resolves to if found.
///
/// This should be populated during global import resolution.
resolved: ?Resolved,

pub const List = collections.SafeList(@This());
pub const Idx = List.Idx;

pub const Resolved = struct {
    package_idx: Package.Idx,
    module_idx: Package.Module.Idx,
};

/// A store of all modules visible to a siloed module, including the
/// module itself and builtin modules.
pub const Store = struct {
    imports: List,
    ident_store: *Ident.Store,
    arena: *std.heap.ArenaAllocator,

    pub const primary_idx: Idx = @enumFromInt(0);

    pub const LookupResult = struct {
        import_idx: Idx,
        was_present: bool,
    };

    pub fn init(
        builtin_names: []const []const u8,
        ident_store: *Ident.Store,
        arena: *std.heap.ArenaAllocator,
    ) Store {
        var modules = List.init(arena);
        _ = modules.append(Self{
            .name = &.{},
            .package_shorthand = null,
            .is_builtin = false,
            .exposed_idents = collections.SafeList(Ident.Idx).init(arena),
        });

        for (builtin_names) |builtin| {
            _ = modules.append(Self{
                .name = builtin,
                .package_shorthand = null,
                .is_builtin = true,
                .exposed_idents = collections.SafeList(Ident.Idx).init(arena),
            });
        }

        return Store{
            .imports = modules,
            .ident_store = ident_store,
            .arena = arena,
        };
    }

    // TODO: Remove this if we don't need it, it seems unnecessary
    //
    /// Search for a module that's visible to the main module.
    ///
    /// NOTE: This only works for modules in this package, so callers must
    /// first ensure that they are looking within the right package.
    pub fn lookup(
        self: *Store,
        name: []const u8,
        package_shorthand: ?[]const u8,
    ) ?Idx {
        const items = self.imports.items;
        for (0..self.imports.len()) |index| {
            const item = items.get(index);
            if (std.mem.eql(u8, name, item.name)) {
                const neither_has_shorthand = package_shorthand == null and item.package_shorthand == null;
                const both_have_shorthand = package_shorthand != null and item.package_shorthand != null;

                if (neither_has_shorthand) {
                    return @enumFromInt(@as(u32, @intCast(index)));
                } else if (both_have_shorthand) {
                    if (std.mem.eql(u8, package_shorthand.?, item.package_shorthand.?)) {
                        return @enumFromInt(@as(u32, @intCast(index)));
                    }
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
            return LookupResult{ .import_idx = idx, .was_present = true };
        } else {
            const idx = self.imports.append(Self{
                .name = name,
                .package_shorthand = package_shorthand,
                .is_builtin = false,
                .exposed_idents = collections.SafeList(Ident.Idx).init(self.arena.allocator()),
            });

            return LookupResult{ .import_idx = idx, .was_present = false };
        }
    }

    /// Add an ident to this modules list of exposed idents, reporting a problem
    /// if a duplicate is found.
    ///
    /// NOTE: This should not be called directly, but rather the [ModuleEnv.addExposedIdentForModule]
    /// method that will also set the ident's exposing module.
    pub fn addExposedIdent(
        self: *Store,
        module_idx: Idx,
        ident_idx: Ident.Idx,
        problems: *std.ArrayList(problem.Problem),
    ) void {
        const module_index = @intFromEnum(module_idx);
        var module = self.imports.items.get(module_index);

        for (module.exposed_idents.items.items) |exposed_ident| {
            if (std.meta.eql(exposed_ident, ident_idx)) {
                problems.append(Problem.Canonicalize.make(.{ .DuplicateExposes = .{
                    .first_exposes = exposed_ident,
                    .duplicate_exposes = ident_idx,
                } })) catch exitOnOom();
                return;
            }
        }

        _ = module.exposed_idents.append(ident_idx);
    }
};
