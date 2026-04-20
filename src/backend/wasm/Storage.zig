//! Tracks where LIR locals live in wasm locals.
//!
//! The active statement-only LIR path uses compact `LocalId`s everywhere.
//! Wasm codegen therefore binds executable values by local id, not by symbol.

const std = @import("std");
const Allocator = std.mem.Allocator;
const lir = @import("lir");
const LocalId = lir.LIR.LocalId;
const WasmModule = @import("WasmModule.zig");
const ValType = WasmModule.ValType;

const Self = @This();

/// Info about a wasm local variable: its index and value type.
pub const LocalInfo = struct {
    idx: u32,
    val_type: ValType,
};

/// LIR local → wasm local mapping. Key is the u32 enum payload of `LocalId`.
locals: std.AutoHashMap(u64, LocalInfo),
/// Next local index to allocate.
next_local_idx: u32,
/// Ordered list of local types for the function's locals declaration.
local_types: std.ArrayList(ValType),
allocator: Allocator,

pub fn init(allocator: Allocator) Self {
    return .{
        .locals = std.AutoHashMap(u64, LocalInfo).init(allocator),
        .next_local_idx = 0,
        .local_types = .empty,
        .allocator = allocator,
    };
}

pub fn deinit(self: *Self) void {
    self.locals.deinit();
    self.local_types.deinit(self.allocator);
}

/// Allocate a new wasm local for the given LIR local id.
pub fn allocLocal(self: *Self, local_id: LocalId, val_type: ValType) !u32 {
    const idx = self.next_local_idx;
    self.next_local_idx += 1;
    try self.local_types.append(self.allocator, val_type);
    const key = localKey(local_id);
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

/// Look up the wasm local index for a previously-allocated LIR local.
pub fn getLocal(self: *const Self, local_id: LocalId) ?u32 {
    const key = localKey(local_id);
    if (self.locals.get(key)) |info| {
        return info.idx;
    }
    return null;
}

/// Look up the full wasm-local info for a previously-allocated LIR local.
pub fn getLocalInfo(self: *const Self, local_id: LocalId) ?LocalInfo {
    return self.locals.get(localKey(local_id));
}

/// Reset for a new function scope (keeps allocated memory).
pub fn reset(self: *Self) void {
    self.locals.clearRetainingCapacity();
    self.next_local_idx = 0;
    self.local_types.clearRetainingCapacity();
}

fn localKey(local_id: LocalId) u64 {
    return @as(u64, @intFromEnum(local_id));
}
