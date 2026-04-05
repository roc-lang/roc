//! Cor-style top-level function specialization queue for the monotype pass.

const std = @import("std");
const can = @import("can");
const symbol_mod = @import("../symbol/mod.zig");
const type_mod = @import("type.zig");
const ctx_mod = @import("ctx.zig");

pub const SourceFn = struct {
    module_idx: u32,
    def_idx: can.CIR.Def.Idx,
};

pub const Pending = struct {
    source_symbol: symbol_mod.Symbol,
    source: SourceFn,
    ty: type_mod.TypeId,
    specialized_symbol: symbol_mod.Symbol,
    emitted: bool = false,
};

pub const Queue = struct {
    allocator: std.mem.Allocator,
    pending: std.ArrayList(Pending),

    pub fn init(allocator: std.mem.Allocator) Queue {
        return .{
            .allocator = allocator,
            .pending = .empty,
        };
    }

    pub fn deinit(self: *Queue) void {
        self.pending.deinit(self.allocator);
    }

    pub fn specializeFn(
        self: *Queue,
        ctx: *ctx_mod.Ctx,
        source_symbol: symbol_mod.Symbol,
        source: SourceFn,
        ty: type_mod.TypeId,
    ) std.mem.Allocator.Error!symbol_mod.Symbol {
        for (self.pending.items) |item| {
            if (item.source_symbol == source_symbol and ctx.types.equalIds(item.ty, ty)) {
                return item.specialized_symbol;
            }
        }

        const specialized_symbol = try ctx.addSpecializedTopLevelSymbol(source_symbol);
        try self.pending.append(self.allocator, .{
            .source_symbol = source_symbol,
            .source = source,
            .ty = ty,
            .specialized_symbol = specialized_symbol,
        });
        return specialized_symbol;
    }

    pub fn nextNeededSpecialization(self: *Queue) ?usize {
        for (self.pending.items, 0..) |item, idx| {
            if (!item.emitted) return idx;
        }
        return null;
    }

    pub fn get(self: *Queue, idx: usize) *Pending {
        return &self.pending.items[idx];
    }
};

test "monotype specializations tests" {
    std.testing.refAllDecls(@This());
}
