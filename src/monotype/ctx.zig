//! Shared lowering context for cor-style monotype construction.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const symbol_mod = @import("symbol");
const type_mod = @import("type.zig");

pub const Ctx = struct {
    allocator: std.mem.Allocator,
    symbols: symbol_mod.Store,
    types: type_mod.Store,
    all_module_envs: []const *const can.ModuleEnv,
    builtin_module_idx: u32,
    top_level_symbols: std.AutoHashMap(TopLevelKey, symbol_mod.Symbol),
    pattern_symbols: std.AutoHashMap(PatternKey, symbol_mod.Symbol),

    const TopLevelKey = struct {
        module_idx: u32,
        def_idx: u32,
    };

    const PatternKey = struct {
        module_idx: u32,
        pattern_idx: u32,
    };

    pub fn init(
        allocator: std.mem.Allocator,
        all_module_envs: []const *const can.ModuleEnv,
        builtin_module_idx: u32,
    ) Ctx {
        return .{
            .allocator = allocator,
            .symbols = symbol_mod.Store.init(allocator),
            .types = type_mod.Store.init(allocator),
            .all_module_envs = all_module_envs,
            .builtin_module_idx = builtin_module_idx,
            .top_level_symbols = std.AutoHashMap(TopLevelKey, symbol_mod.Symbol).init(allocator),
            .pattern_symbols = std.AutoHashMap(PatternKey, symbol_mod.Symbol).init(allocator),
        };
    }

    pub fn deinit(self: *Ctx) void {
        self.pattern_symbols.deinit();
        self.top_level_symbols.deinit();
        self.symbols.deinit();
        self.types.deinit();
    }

    pub fn env(self: *const Ctx, module_idx: u32) *const can.ModuleEnv {
        return self.all_module_envs[module_idx];
    }

    pub fn getOrCreateTopLevelSymbol(
        self: *Ctx,
        module_idx: u32,
        def_idx: can.CIR.Def.Idx,
        name: base.Ident.Idx,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const key: TopLevelKey = .{
            .module_idx = module_idx,
            .def_idx = @intFromEnum(def_idx),
        };
        if (self.top_level_symbols.get(key)) |symbol| return symbol;

        const symbol = try self.symbols.add(name, .{
            .top_level_def = .{
                .module_idx = module_idx,
                .def_idx = @intFromEnum(def_idx),
            },
        });
        try self.top_level_symbols.put(key, symbol);
        return symbol;
    }

    pub fn getOrCreatePatternSymbol(
        self: *Ctx,
        module_idx: u32,
        pattern_idx: can.CIR.Pattern.Idx,
        name: base.Ident.Idx,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const key: PatternKey = .{
            .module_idx = module_idx,
            .pattern_idx = @intFromEnum(pattern_idx),
        };
        if (self.pattern_symbols.get(key)) |symbol| return symbol;

        const symbol = try self.symbols.add(name, .{
            .local_pattern = .{
                .module_idx = module_idx,
                .pattern_idx = @intFromEnum(pattern_idx),
            },
        });
        try self.pattern_symbols.put(key, symbol);
        return symbol;
    }

    pub fn addSpecializedTopLevelSymbol(
        self: *Ctx,
        source_symbol: symbol_mod.Symbol,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        const source_entry = self.symbols.get(source_symbol);
        return self.symbols.add(source_entry.name, .{
            .specialized_top_level_def = .{
                .source_symbol = @intFromEnum(source_symbol),
            },
        });
    }

    pub fn addSyntheticSymbol(self: *Ctx, name: base.Ident.Idx) std.mem.Allocator.Error!symbol_mod.Symbol {
        return self.symbols.add(name, .synthetic);
    }
};

test "monotype ctx tests" {
    std.testing.refAllDecls(@This());
}
