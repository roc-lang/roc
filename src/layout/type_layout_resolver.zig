//! Resolves type-checker vars into canonical ordinary-data layouts through the shared layout store.

const std = @import("std");
const types = @import("types");
const mir = @import("mir");

const layout = @import("layout.zig");
const Store = @import("store.zig").Store;
const MirMonotypeLayoutResolver = @import("mir_monotype_resolver.zig").Resolver;

/// Temporary type-driven resolver while ordinary-data layout moves fully out of Store.
pub const Resolver = struct {
    store: *Store,

    /// Create a resolver backed by the shared layout store.
    pub fn init(store: *Store) Resolver {
        return .{ .store = store };
    }

    /// Resolve a type var to a canonical layout id.
    pub fn resolve(
        self: *Resolver,
        module_idx: u32,
        type_var: types.Var,
        _: *const types.TypeScope,
        _: ?u32,
    ) std.mem.Allocator.Error!layout.Idx {
        const module_env = self.store.moduleEnvs()[module_idx];
        const types_store = if (self.store.override_types_store) |override|
            override
        else
            &module_env.types;

        var mono_store = try mir.Monotype.Store.init(self.store.allocator);
        defer mono_store.deinit(self.store.allocator);

        var scratches = try mir.Monotype.Store.Scratches.init(self.store.allocator);
        defer scratches.deinit();

        var specializations = std.AutoHashMap(types.Var, mir.Monotype.Idx).init(self.store.allocator);
        defer specializations.deinit();
        var nominal_cycle_breakers = std.AutoHashMap(types.Var, mir.Monotype.Idx).init(self.store.allocator);
        defer nominal_cycle_breakers.deinit();

        const mono_idx = try mono_store.fromTypeVar(
            self.store.allocator,
            types_store,
            type_var,
            module_env.idents,
            &specializations,
            &nominal_cycle_breakers,
            &scratches,
        );

        var mir_resolver = MirMonotypeLayoutResolver.init(self.store.allocator, &mono_store, self.store);
        defer mir_resolver.deinit();
        return mir_resolver.resolve(mono_idx, null);
    }
};
