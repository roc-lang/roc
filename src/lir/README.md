# Low-level intermediate representation

LIR is the common, layout-complete boundary between checked-program lowering
and execution. Ownership-neutral transformations preserve explicit value,
control-flow, and frame inventories; ARC then makes every ownership operation
explicit so backends and the interpreter need no reference-counting policy.

Parallel procedure rewrites and ARC emission share frozen inputs and publish
private body output in deterministic order. Global analyses and identity
reservation remain coordinator-owned. `LirImage` preserves the resulting
ARC-complete program without compiler scratch or worker state.

`use_order.zig` owns the neutral control-flow queries shared by loop promotion
and ARC. Procedure rewrites use compact statement/local domains; ARC retains
its store-indexed topology and reuses scratch across procedure queries, resetting
only touched entries. Neither client reconstructs ownership decisions from the
other.

See `design.md` for the transformation, ownership, and serialization contracts.
