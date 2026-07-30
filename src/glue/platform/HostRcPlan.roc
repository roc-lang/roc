## Host-visible refcount summary for a glue type.
##
## Authoritative compiler sources:
## - src/layout/store.zig `layoutContainsRefcounted` records whether a committed
##   layout contains Roc-owned refcounted values.
## - src/layout/rc_helper.zig is the detailed helper-plan source for future
##   data-driven helper emission.
##
## This summary is intentionally small today: generators use it to choose public
## API shapes such as list element header handling. Detailed decref/incref helper
## plans should be added here from `rc_helper.Plan`, not reconstructed in glue.
HostRcPlan := [
	RcNoop,
	RcRefcounted,
]
