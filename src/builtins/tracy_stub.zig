//! No-op tracy stub for standalone static library builds where the real
//! tracy module is not available. All functions are inlined no-ops.

/// Whether tracy allocation tracking is enabled (always false for stub).
pub const enable_allocation = false;

/// Begin a tracy trace zone (no-op).
pub inline fn trace(_: anytype) Ctx {
    return .{};
}

/// Record an allocation event (no-op).
pub inline fn alloc(_: [*]u8, _: usize) void {}

/// Record a free event (no-op).
pub inline fn free(_: [*]u8) void {}

/// No-op trace context returned by `trace`.
pub const Ctx = struct {
    /// End the trace zone (no-op).
    pub inline fn end(_: @This()) void {}
};
