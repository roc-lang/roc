# CTFE static-root cache reuse

These modules exercise cached code that reads a checked constant and a guarded
hoist whose evaluation failed. They require no platform host build.

`Probe.roc` seeds successful calls and validates their values through an ordinary
compile-time constant, not only test-only expects. A consumer must still evaluate
an edited probe correctly with the unchanged `Static.roc`, rather than merely
restore the probe's cached result. `Overflow.roc` fails inside the helper;
`FailedGuard.roc` demands the guarded division-by-zero result. Their diagnostics
must agree with uncached execution after the helper has been cached.

Run the focused check workflow directly with the compiler under test and an
isolated `ROC_CACHE_DIR`. Cache-hit instrumentation must establish actual helper
reuse; successful checks or cache-file creation alone are not that evidence.
