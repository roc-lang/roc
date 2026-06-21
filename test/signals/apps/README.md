# Signals Representative Apps

These apps are the maintained signals research fixtures. They are standalone Roc
applications that use `../platform/main.roc` and are run by
`zig build run-test-signals`.

- `ops_dashboard.roc` stresses scalar signal chains, fanout, dynamic incident
  branches, keyed alert rows, form input state, and per-row local state.
- `checkout_wizard.roc` stresses keyed dynamic wizard steps, form input state,
  checkbox state, disabled actions, cart list replacement, and keyed line-item
  local state.
- `kanban_board.roc` stresses keyed reorder/archive/reset flows, local card
  state preservation, list filtering through generic `Signal.map2`, and form
  input state.
- `identity_stress.roc` is a Phase 2 G2 regression fixture. It is test-only, not
  part of `run-signals-bench`, and stresses row-local state through
  `when -> each -> when` plus reorder, mid-list insert, filtering/removal, and
  enclosing branch disposal.
- `component_composition.roc` is a Phase 2 G2 regression fixture. It is
  test-only, not part of `run-signals-bench`, and proves reusable stateful
  `Ui.component` scopes keep local state across keyed row movement while row
  disposal still drops the removed component instance.

Useful checks from the repository root:

```sh
./zig-out/bin/roc check test/signals/apps/ops_dashboard.roc
./zig-out/bin/roc check test/signals/apps/checkout_wizard.roc
./zig-out/bin/roc check test/signals/apps/kanban_board.roc
./zig-out/bin/roc check test/signals/apps/identity_stress.roc
./zig-out/bin/roc check test/signals/apps/component_composition.roc

./zig-out/bin/roc build --opt=speed --debug --no-cache --output=zig-out/bin/signals-ops-dashboard test/signals/apps/ops_dashboard.roc
./zig-out/bin/signals-ops-dashboard test/signals/apps/ops_dashboard.txt

./zig-out/bin/roc build --opt=speed --debug --no-cache --output=zig-out/bin/signals-checkout-wizard test/signals/apps/checkout_wizard.roc
./zig-out/bin/signals-checkout-wizard test/signals/apps/checkout_wizard.txt

./zig-out/bin/roc build --opt=speed --debug --no-cache --output=zig-out/bin/signals-kanban-board test/signals/apps/kanban_board.roc
./zig-out/bin/signals-kanban-board test/signals/apps/kanban_board.txt

./zig-out/bin/roc build --opt=speed --debug --no-cache --output=zig-out/bin/signals-identity-stress test/signals/apps/identity_stress.roc
./zig-out/bin/signals-identity-stress test/signals/apps/identity_stress.txt

./zig-out/bin/roc build --opt=speed --debug --no-cache --output=zig-out/bin/signals-component-composition test/signals/apps/component_composition.roc
./zig-out/bin/signals-component-composition test/signals/apps/component_composition.txt
```

The same binaries can replay their spec actions as benchmark traces:

```sh
zig build run-signals-bench

./zig-out/bin/signals-ops-dashboard --bench-app --bench-name signals-ops-dashboard --bench-iterations 100 --bench-samples 3 test/signals/apps/ops_dashboard.txt
```

The optimized benchmark build currently hits roc-lang/roc#9717 for
`signals-ops-dashboard`. Keep the failure explicit until the compiler bug is
fixed; the correctness gate remains `zig build run-test-signals`.

Specs use semantic locators:

```txt
expect_visible role:heading name:"Checkout wizard"
fill label:"Email" "team@example.com"
check label:"Accept terms"
click role:button name:"Place order"
```

The apps expose pure `main : {} -> Elem.Elem`. Signal state is retained by
host-owned construction-site ids inside explicit scopes; row fixtures use string
labels as typed keys, and the host owns event routing, scope/key lifecycle, and
render application.
