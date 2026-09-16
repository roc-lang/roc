# Low-level intermediate representation

LIR is the common, layout-complete boundary between checked-program lowering
and execution. Ownership-neutral transformations preserve explicit value,
control-flow, and frame inventories; ARC then makes every ownership operation
explicit so backends and the interpreter need no reference-counting policy.

Parallel procedure rewrites and ARC emission share frozen inputs and publish
private body output in deterministic order. Global analyses and identity
reservation remain coordinator-owned. `LirImage` preserves the resulting
ARC-complete program without compiler scratch or worker state.

See `design.md` for the transformation, ownership, and serialization contracts.
