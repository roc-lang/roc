# Signals Test Platform

This directory contains a small Roc platform for researching a signal-based UI
framework. The host is a simulated UI and spec runner: it does not draw pixels,
but it exercises the same event, render, and command-application path a real
renderer would need.

For an introduction written for web developers, see
[`GUIDE.md`](GUIDE.md).

The current host boundary keeps the app description pure and puts runtime
mechanics in the host:

- apps expose pure `main : {} -> Elem`,
- the platform provides `roc_ui_init`, which returns a retained descriptor tree,
- the host owns state values, event routing, scope/key lifecycle, signal
  evaluation, and simulated DOM patch application.

The maintained coverage is the representative app suite in
[`apps/`](apps/README.md):

- ops dashboard,
- checkout wizard,
- kanban board,
- identity stress.

## Running

From the repository root:

```sh
zig build run-test-signals
```

That step checks, builds, and runs all four app specs.

The host can also be rebuilt directly:

```sh
zig build build-test-hosts -Dplatform=signals
```

Run the three-app representative benchmark suite:

```sh
zig build run-signals-bench
```

The benchmark replays each app's semantic spec as a user-event trace and prints
CSV rows with Roc dispatch time, host command-apply time, allocation counts,
emitted command counts, and Roc runtime counters.

Current note: the optimized benchmark build is blocked by roc-lang/roc#9717 in
the LLVM `--opt=speed` ops-dashboard build. Keep that failure visible; do not
skip the case or silently build it in dev mode.

Regenerate glue after changing exposed platform types or provided entrypoints:

```sh
./zig-out/bin/roc glue src/glue/src/ZigGlue.roc test/signals/src test/signals/platform/main.roc
```

## Spec Language

Specs use semantic locators rather than positional DOM indices:

```txt
expect_visible role:heading name:"Checkout wizard"
fill label:"Email" "team@example.com"
expect_value label:"Email" "team@example.com"
check label:"Accept terms"
expect_checked label:"Accept terms" true
click role:button name:"Place order"
```

Supported locators:

- `role:<role> name:"<accessible name>"`
- `label:"<label>"`
- `text:"<exact text>"`
- `test_id:"<id>"`

Supported commands:

- `click <locator>`
- `fill <locator> "<text>"`
- `check <locator>` and `uncheck <locator>`
- `expect_visible <locator>`
- `expect_absent <locator>`
- `expect_text <locator> "<text>"`
- `expect_value <locator> "<text>"`
- `expect_checked <locator> true|false`
- `expect_disabled <locator> true|false`
- `expect_updates <locator> <count>`

## Roc API Shape

Apps import:

```roc
import pf.Elem exposing [Elem]
import pf.Html
import pf.Signal
import pf.Ui
```

`Signal`, `Html`, and `Ui` build pure descriptor trees:

- `Signal.Signal(a)` is an opaque typed descriptor.
- `Ui.state` introduces local state through a closure binder.
- `Ui.when` and `Ui.each` introduce explicit dynamic scopes.
- `Html` creates static markup, signal-backed text/attrs, and event bindings.

Apps no longer define `NodeValue` encode/decode boilerplate for row fixtures.
`NodeValue` is still an internal platform representation until confined erasure
removes it from the platform API.

## Host Boundary

The host calls `roc_ui_init` once, stores the returned boxed `Elem`, walks the
descriptor tree, evaluates signal expressions against host-owned state, applies
patches to the simulated DOM, and dispatches events through retained Roc
reducers. Branch and keyed-row scopes are disposed by the host when they leave
the active tree. Non-structural state changes patch only the dirty signal-backed
leaf sinks recorded in the retained descriptor stream; structural `when`/`each`
changes still rebuild the active stream while subtree-level DOM patching is
finished.

## Benchmark Mode

Each built app binary accepts a benchmark mode:

```sh
./zig-out/bin/signals-ops-dashboard --bench-app --bench-name signals-ops-dashboard --bench-iterations 100 --bench-samples 3 test/signals/apps/ops_dashboard.txt
```

The host initializes a fresh app per iteration, applies the initial command
batch, then replays only action commands (`click`, `fill`, `check`, `uncheck`).
Expectation commands remain the semantic correctness suite used by
`zig build run-test-signals`.
