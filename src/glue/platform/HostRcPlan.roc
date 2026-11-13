## Host-visible refcount summary for a glue type.
##
## Authoritative compiler sources:
## - src/layout/store.zig `layoutContainsRefcounted` records whether a committed
##   layout contains Roc-owned refcounted values.
## - src/layout/rc_helper.zig is the detailed helper-plan source for future
##   data-driven helper emission.
##
## `RcNoop`: the value holds no Roc reference.
##
## `RcRefcounted`: the value holds Roc references, and a host that owns the
## value can release all of them.
##
## `RcRocReleased`: the value holds a reference to an opaque heap target (a
## `Box` payload or `List` item that is `RocUnknown`), directly or through
## anything it owns. Releasing that reference means tearing down the target
## and freeing an allocation whose header depends on the target's layout, and
## only compiled Roc code knows either. A host may move such a value, retain
## it, and pass it back to Roc. It releases it by passing it to a Roc function
## that takes ownership. Generators must not emit a release for it, nor an
## allocation, and must not claim a list's allocation header from its items.
##
## Detailed decref/incref helper plans should be added here from
## `rc_helper.Plan`, not reconstructed in glue.
HostRcPlan := [
	RcNoop,
	RcRefcounted,
	RcRocReleased,
]
