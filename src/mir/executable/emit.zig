//! Source-blind executable emission from a completed representation plan.

const std = @import("std");
const base = @import("base");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("symbol");
const layouts_mod = @import("layouts.zig");
const plan_mod = @import("plan.zig");

/// Final executable lambdamono program emitted from a completed plan.
pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    layouts: layouts_mod.Layouts,
    strings: base.StringLiteral.Store,
    entrypoint_wrappers: []symbol_mod.Symbol,

    /// Release all memory owned by the emitted executable program.
    pub fn deinit(self: *Result) void {
        self.store.deinit();
        self.root_defs.deinit(self.store.allocator);
        self.symbols.deinit();
        self.layouts.deinit(self.store.allocator);
        self.types.deinit();
        self.strings.deinit(self.store.allocator);
        if (self.entrypoint_wrappers.len > 0) {
            self.store.allocator.free(self.entrypoint_wrappers);
        }
    }

    /// Move this result out and leave an empty owned result behind.
    pub fn take(self: *Result, allocator: std.mem.Allocator) std.mem.Allocator.Error!Result {
        const result = self.*;
        var empty_store = ast.Store.init(allocator);
        errdefer empty_store.deinit();
        const empty_layouts = try layouts_mod.Layouts.initEmpty(allocator, &empty_store);
        self.* = .{
            .store = empty_store,
            .root_defs = .empty,
            .symbols = symbol_mod.Store.init(allocator),
            .types = type_mod.Store.init(allocator),
            .layouts = empty_layouts,
            .strings = .{},
            .entrypoint_wrappers = &.{},
        };
        return result;
    }
};

/// Emit the final executable program from a completed representation plan.
pub fn run(allocator: std.mem.Allocator, executable_plan: *plan_mod.ExecPlan) std.mem.Allocator.Error!Result {
    var empty_store = plan_mod.Store.init(allocator);
    errdefer empty_store.deinit();
    const empty_layouts = try layouts_mod.Layouts.initEmpty(allocator, &empty_store);

    const result = Result{
        .store = takeStore(&executable_plan.store),
        .root_defs = executable_plan.root_defs,
        .symbols = executable_plan.symbols,
        .types = executable_plan.types,
        .layouts = executable_plan.layouts,
        .strings = executable_plan.strings,
        .entrypoint_wrappers = executable_plan.entrypoint_wrappers,
    };

    executable_plan.store = empty_store;
    executable_plan.root_defs = .empty;
    executable_plan.symbols = symbol_mod.Store.init(allocator);
    executable_plan.types = type_mod.Store.init(allocator);
    executable_plan.layouts = empty_layouts;
    executable_plan.strings = .{};
    executable_plan.entrypoint_wrappers = &.{};

    return result;
}

fn takeStore(store: *plan_mod.Store) ast.Store {
    return ast.Store{
        .allocator = store.allocator,
        .exprs = store.exprs,
        .pats = store.pats,
        .branches = store.branches,
        .stmts = store.stmts,
        .defs = store.defs,
        .expr_ids = store.expr_ids,
        .pat_ids = store.pat_ids,
        .stmt_ids = store.stmt_ids,
        .branch_ids = store.branch_ids,
        .field_exprs = store.field_exprs,
        .typed_symbols = store.typed_symbols,
    };
}
