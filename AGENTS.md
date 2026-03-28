# AGENTS

- Workarounds are categorically forbidden in this code base.
- Backends are categorically forbidden from thinking about reference counting in any way other than specifically dumbly following the explicit LIR `incref` and `decref` statements emitted by earlier compilation steps.
