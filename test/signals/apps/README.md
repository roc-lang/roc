# Signals Representative Apps

These apps are research fixtures for shaping a more realistic Signals platform.
They are standalone Roc applications that use `../platform/main.roc` and do not
replace the primary `test/signals/app.roc` acceptance demo.

- `ops_dashboard.roc` stresses scalar signal chains, fanout, incident toggles,
  dynamic branches, keyed alert rows, and lifecycle counters.
- `checkout_wizard.roc` stresses dynamic wizard steps, cart list replacement,
  keyed line-item local state, scalar progress, and form-like toggles.
- `kanban_board.roc` stresses keyed reorder/archive/reset flows, local card
  state preservation, list filtering through generic `Signal.map2`, and mount
  churn counters.

Useful checks from the repository root:

```sh
./zig-out/bin/roc check test/signals/apps/ops_dashboard.roc
./zig-out/bin/roc check test/signals/apps/checkout_wizard.roc
./zig-out/bin/roc check test/signals/apps/kanban_board.roc
./zig-out/bin/roc build --opt=speed --debug --no-cache --output=zig-out/bin/signals-ops-dashboard test/signals/apps/ops_dashboard.roc
./zig-out/bin/signals-ops-dashboard test/signals/apps/ops_dashboard.txt
./zig-out/bin/roc build --opt=speed --debug --no-cache --output=zig-out/bin/signals-checkout-wizard test/signals/apps/checkout_wizard.roc
./zig-out/bin/signals-checkout-wizard test/signals/apps/checkout_wizard.txt
./zig-out/bin/roc build --opt=speed --debug --no-cache --output=zig-out/bin/signals-kanban-board test/signals/apps/kanban_board.roc
./zig-out/bin/signals-kanban-board test/signals/apps/kanban_board.txt
```

Current research note: avoid walking long-lived fold signal descriptions for the
first time inside a dynamic branch after events have already occurred. That
constructs a fresh host node from the fold's initial value. The checkout fixture
keeps the dynamic wizard branch and the keyed cart list as sibling regions so
both behaviors stay observable without relying on late graph construction.
