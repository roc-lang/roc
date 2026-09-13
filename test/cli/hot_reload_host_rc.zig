//! Reference-counting helpers linked into the hot-reload test platform host.
//! The host owns its retained callable references; the run shim does not export
//! compiler-private builtin symbols for platform code to link against.
const callable = @import("erased_callable");

/// This object belongs to the test platform, outside compiler profiling.
pub const roc_disable_tracy = true;

export fn hot_reload_host_incref(data: ?[*]u8, amount: isize, ops: *callable.RocOps) void {
    callable.incref(data, amount, ops);
}

export fn hot_reload_host_decref(data: ?[*]u8, ops: *callable.RocOps) void {
    callable.decref(data, ops);
}
