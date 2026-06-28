const abi = @import("roc_platform_abi.zig");
const retained_values = @import("retained_values.zig");
const scope_tree = @import("scope_tree.zig");

pub const HostValueCell = retained_values.HostValueCell;

/// Per-row payload carried in an `Ui.each` scope: the row's key and item cells,
/// keyed by the construction-site ordinal.
pub const EachRowScopeStep = struct {
    site_ordinal: u64,
    key_hash: u64,
    key: HostValueCell,
    item: HostValueCell,
};

pub const ScopeStep = scope_tree.Step(EachRowScopeStep);
pub const Scope = scope_tree.Scope(EachRowScopeStep);

pub const EachSite = struct {
    parent_scope_id: u64,
    site_ordinal: u64,
};

/// Drop the retained cells owned by an each-row scope step (no-op for the
/// structural scope kinds, which carry no Roc values).
pub fn deinitScopeStep(step: *ScopeStep, ctx: anytype, roc_host: *abi.RocHost, metrics: anytype) void {
    switch (step.*) {
        .each_row => |*row| {
            row.key.deinit(ctx, roc_host, metrics);
            row.item.deinit(ctx, roc_host, metrics);
        },
        .root, .component, .when_branch => {},
    }
}
