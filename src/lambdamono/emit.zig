//! Source-blind executable emission from a completed representation plan.

const std = @import("std");
const base = @import("base");
const ast = @import("ast.zig");
const type_mod = @import("type.zig");
const symbol_mod = @import("symbol");
const layouts_mod = @import("layouts.zig");
const plan_mod = @import("plan.zig");

pub const Result = struct {
    store: ast.Store,
    root_defs: std.ArrayList(ast.DefId),
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    layouts: layouts_mod.Layouts,
    strings: base.StringLiteral.Store,
    entrypoint_wrappers: []symbol_mod.Symbol,

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
};

/// Emit the final executable program from a completed representation plan.
pub fn run(allocator: std.mem.Allocator, exec_plan: *plan_mod.ExecPlan) std.mem.Allocator.Error!Result {
    var empty_store = plan_mod.Store.init(allocator);
    errdefer empty_store.deinit();
    const empty_layouts = try layouts_mod.Layouts.initEmpty(allocator, &empty_store);

    const result = Result{
        .store = takeStore(&exec_plan.store),
        .root_defs = exec_plan.root_defs,
        .symbols = exec_plan.symbols,
        .types = exec_plan.types,
        .layouts = exec_plan.layouts,
        .strings = exec_plan.strings,
        .entrypoint_wrappers = exec_plan.entrypoint_wrappers,
    };

    exec_plan.store = empty_store;
    exec_plan.root_defs = .empty;
    exec_plan.symbols = symbol_mod.Store.init(allocator);
    exec_plan.types = type_mod.Store.init(allocator);
    exec_plan.layouts = empty_layouts;
    exec_plan.strings = .{};
    exec_plan.entrypoint_wrappers = &.{};

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
