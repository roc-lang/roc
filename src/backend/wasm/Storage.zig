//! Tracks where MonoSymbols live in wasm (local variables vs linear memory).
//!
//! Maps MonoSymbol → wasm local index so that `block` (let bindings) and
//! `lookup` can store / retrieve values.

const std = @import("std");
const Allocator = std.mem.Allocator;
const mono = @import("mono");
const MonoSymbol = mono.MonoIR.MonoSymbol;
const WasmModule = @import("WasmModule.zig");
const ValType = WasmModule.ValType;

const Self = @This();

pub const LocalInfo = struct {
    idx: u32,
    val_type: ValType,
};

/// Symbol → wasm local mapping. Key is the u48 bitcast of MonoSymbol.
locals: std.AutoHashMap(u48, LocalInfo),
/// Next local index to allocate.
next_local_idx: u32,
/// Ordered list of local types for the function's locals declaration.
local_types: std.ArrayList(ValType),
allocator: Allocator,

pub fn init(allocator: Allocator) Self {
    return .{
        .locals = std.AutoHashMap(u48, LocalInfo).init(allocator),
        .next_local_idx = 0,
        .local_types = .empty,
        .allocator = allocator,
    };
}

pub fn deinit(self: *Self) void {
    self.locals.deinit();
    self.local_types.deinit(self.allocator);
}

/// Allocate a new wasm local for the given symbol.
pub fn allocLocal(self: *Self, symbol: MonoSymbol, val_type: ValType) !u32 {
    const idx = self.next_local_idx;
    self.next_local_idx += 1;
    try self.local_types.append(self.allocator, val_type);
    const key: u48 = @bitCast(symbol);
    try self.locals.put(key, .{ .idx = idx, .val_type = val_type });
    return idx;
}

/// Allocate an anonymous local (not bound to any symbol).
pub fn allocAnonymousLocal(self: *Self, val_type: ValType) !u32 {
    const idx = self.next_local_idx;
    self.next_local_idx += 1;
    try self.local_types.append(self.allocator, val_type);
    return idx;
}

/// Look up the local index for a previously-allocated symbol.
pub fn getLocal(self: *const Self, symbol: MonoSymbol) ?u32 {
    const key: u48 = @bitCast(symbol);
    if (self.locals.get(key)) |info| {
        return info.idx;
    }
    return null;
}

/// Number of locals allocated.
pub fn localCount(self: *const Self) u32 {
    return self.next_local_idx;
}

/// Reset for a new function scope (keeps allocated memory).
pub fn reset(self: *Self) void {
    self.locals.clearRetainingCapacity();
    self.next_local_idx = 0;
    self.local_types.clearRetainingCapacity();
}
