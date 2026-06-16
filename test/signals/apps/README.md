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

Specs use semantic locators:

```txt
expect_visible role:heading name:"Checkout wizard"
fill label:"Email" "team@example.com"
check label:"Accept terms"
click role:button name:"Place order"
```

The apps use eager stateful signal constructors (`hold!`, `fold!`,
`fold_i64!`, `fold_bool_toggle!`, and `zip_with!`) so host node identity is
created when the app builds the graph, not during a later dynamic render.
