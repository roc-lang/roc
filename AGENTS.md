# AGENTS

- Read `design.md` before making code changes. It is the forward-looking design reference for checked artifacts, MIR, IR, LIR, ARC, backends, runtime images, and compiler invariants.
- Workarounds are categorically forbidden in this code base.
- Fallbacks are categorically forbidden in every stage of compilation other than specifically parsing and error reporting.
- Heuristics are categorically forbidden in every stage of compilation other than specifically parsing and error reporting.
- Every compiler stage other than specifically parsing and error reporting must consume explicit data produced by earlier stages rather than trying to recover, guess, reconstruct, approximate, or "best effort" its way to missing information.
- Backends are categorically forbidden from thinking about reference counting in any way other than specifically dumbly following the explicit LIR `incref` and `decref` statements emitted by earlier compilation steps.
