//! Checked static-dispatch target registry and normalized dispatch-site records.
//!
//! The registry is built at the checked-artifact boundary. Later MIR stages use
//! it as a target table only; the dispatch-site record chooses the dispatcher
//! type variable explicitly.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const symbol = @import("symbol");
const TypedCIR = @import("typed_cir.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;

pub const MethodOwner = union(enum) {
    nominal: struct {
        module_idx: u32,
        type_ident: Ident.Idx,
        qualified_module_ident: Ident.Idx,
    },
    builtin: BuiltinOwner,
};

pub const BuiltinOwner = enum {
    list,
    box,
    bool,
    str,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
};

pub const MethodKey = struct {
    owner: MethodOwner,
    method_ident: Ident.Idx,
};

pub const MethodTarget = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    qualified_ident: Ident.Idx,
    callable_var: Var,
    proc_symbol: symbol.Symbol = .none,
};

pub const MethodRegistryEntry = struct {
    key: MethodKey,
    target: MethodTarget,
};

pub const MethodRegistry = struct {
    entries: []MethodRegistryEntry = &.{},

    pub fn deinit(self: *MethodRegistry, allocator: Allocator) void {
        allocator.free(self.entries);
        self.* = .{};
    }

    pub fn fromTypedModules(allocator: Allocator, modules: *const TypedCIR.Modules) Allocator.Error!MethodRegistry {
        var entries = std.ArrayList(MethodRegistryEntry).empty;
        errdefer entries.deinit(allocator);

        var module_idx: u32 = 0;
        while (module_idx < modules.moduleCount()) : (module_idx += 1) {
            const module = modules.module(module_idx);
            const module_env = module.moduleEnvConst();

            for (module.methodIdentEntries()) |entry| {
                const def_node_idx = module_env.getExposedNodeIndexById(entry.value) orelse {
                    std.debug.panic(
                        "checked static dispatch registry invariant violated: method ident {d} has no exposed definition",
                        .{@as(u32, @bitCast(entry.value))},
                    );
                };
                const def_idx: CIR.Def.Idx = @enumFromInt(@as(u32, @intCast(def_node_idx)));

                try entries.append(allocator, .{
                    .key = .{
                        .owner = .{ .nominal = .{
                            .module_idx = module_idx,
                            .type_ident = entry.key.type_ident,
                            .qualified_module_ident = module.qualifiedModuleIdent(),
                        } },
                        .method_ident = entry.key.method_ident,
                    },
                    .target = .{
                        .module_idx = module_idx,
                        .def_idx = def_idx,
                        .qualified_ident = entry.value,
                        .callable_var = ModuleEnv.varFrom(def_idx),
                    },
                });
            }
        }

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }
};

pub const StaticDispatchResultMode = union(enum) {
    ordinary,
    equality: struct {
        structural_allowed: bool,
        negated: bool,
    },
};

pub const StaticDispatchCallPlan = struct {
    expr: CIR.Expr.Idx,
    method_ident: Ident.Idx,
    dispatcher_var: Var,
    callable_var: Var,
    args: []const CIR.Expr.Idx,
    result_mode: StaticDispatchResultMode,
};

pub const StaticDispatchPlanTable = struct {
    plans: []StaticDispatchCallPlan = &.{},

    pub fn deinit(self: *StaticDispatchPlanTable, allocator: Allocator) void {
        for (self.plans) |plan| allocator.free(plan.args);
        allocator.free(self.plans);
        self.* = .{};
    }
};

test "method registry can be empty" {
    var registry: MethodRegistry = .{};
    registry.deinit(std.testing.allocator);
}
