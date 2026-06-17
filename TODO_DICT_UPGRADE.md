# TODO: Dict Upgrade

Blocked on: https://github.com/roc-lang/roc/issues/9682

Once compiled `Dict` operations work on the `fx` platform and the issue repro passes in both dev and speed builds, revisit the Signals runtime retained table shape.

The current runtime uses explicit retained lists plus indexed scans because compiled `Dict.single`/`Dict.get` currently crashes with `dict_pseudo_seed`. Algorithmically, the right shape is keyed tables for string-keyed runtime metadata:

- `event_ids`: `Dict(Str, U64)`
- `signal_cache`: `Dict(Str, SignalCacheEntry)`
- `event_state_deps`: `Dict(Str, List(Str))`
- state storage: either `Dict(Str, StateSlot)` or `Dict(Str, U64)` plus dense state slots

Keep the dense reverse event lookup (`event_keys_by_id : List(Str)`) unless a better explicit id-indexed structure exists; dispatch needs event id -> event key without a string-key scan.

Before switching:

1. Verify the issue #9682 repro with `roc check`, interpreter, dev build, and speed build.
2. Replace only the lookup/update tables where `Dict` removes real scans or whole-list rewrites.
3. Regenerate platform glue if ABI types change.
4. Run the three Signals app checks, `zig build build-test-hosts -Dplatform=signals`, `zig build run-test-signals`, and benchmark before/after.
