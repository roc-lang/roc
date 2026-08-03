# Regression fixture for https://github.com/roc-lang/roc/issues/9934 — the
# first expect's compile-time root (module-local id 0) requests Clock.create's
# template, and Clock's hoisted `minutes_per_day` constant also has root id 0
# in its own module. The traveling entry-root fact must stay module-qualified,
# or the collision makes the lowerer treat Clock's hoisted const as the root
# currently being lowered and look up a binder that was never bound.
import Clock

expect {
	_clock = Clock.create({ hour: 8, minute: 30 })
	Bool.True
}

main! = |_args| Ok({})
