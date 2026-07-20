//! Shared, host-agnostic Signals runtime module.
//!
//! Native and wasm hosts import this module by name as `signals`. Files outside
//! this directory own host-only concerns such as simulated DOM execution,
//! allocation diagnostics, specs, and benchmarks.

const std = @import("std");

pub const abi = @import("roc_platform_abi.zig");
pub const active_signal_graph = @import("active_signal_graph.zig");
pub const descriptor_stream = @import("descriptor_stream.zig");
pub const each_runtime = @import("each_runtime.zig");
pub const effects_runtime = @import("effects_runtime.zig");
pub const engine = @import("engine.zig");
pub const engine_contract = @import("engine_contract.zig");
pub const engine_metrics = @import("engine_metrics.zig");
pub const engine_scratch = @import("engine_scratch.zig");
pub const erased_calls = @import("erased_calls.zig");
pub const host_value_registry = @import("host_value_registry.zig");
pub const host_values = @import("host_values.zig");
pub const identity_table = @import("identity_table.zig");
pub const render = @import("render_commands.zig");
pub const render_cache = @import("render_cache.zig");
pub const render_sink = @import("render_sink.zig");
pub const retained_values = @import("retained_values.zig");
pub const scope_runtime = @import("scope_runtime.zig");
pub const scope_tree = @import("scope_tree.zig");
pub const signal_graph = @import("signal_graph.zig");
pub const signal_records = @import("signal_records.zig");
pub const structural_splice = @import("structural_splice.zig");

test {
    std.testing.refAllDecls(active_signal_graph);
    std.testing.refAllDecls(descriptor_stream);
    std.testing.refAllDecls(each_runtime);
    std.testing.refAllDecls(effects_runtime);
    std.testing.refAllDecls(engine);
    std.testing.refAllDecls(engine_contract);
    std.testing.refAllDecls(engine_metrics);
    std.testing.refAllDecls(engine_scratch);
    std.testing.refAllDecls(host_value_registry);
    std.testing.refAllDecls(identity_table);
    std.testing.refAllDecls(render);
    std.testing.refAllDecls(render_cache);
    std.testing.refAllDecls(render_sink);
    std.testing.refAllDecls(scope_runtime);
    std.testing.refAllDecls(scope_tree);
    std.testing.refAllDecls(signal_graph);
    std.testing.refAllDecls(signal_records);
    std.testing.refAllDecls(structural_splice);
}
