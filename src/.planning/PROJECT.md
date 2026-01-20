# Roc Compiler

## What This Is

A compiler and tooling for the Roc programming language, being rewritten in Zig. The rewrite is driven by performance goals, architectural simplification, the move to Zig, and significant language changes including static dispatch. The compiler is largely functional—current focus is bug fixes, tooling improvements, and feature completion.

## Core Value

A fast, correct compiler with excellent developer experience—specifically LSP completion that responds in under 100ms using cached analysis.

## Requirements

### Validated

- ✓ Lexing and parsing — existing
- ✓ Canonicalization (AST → CIR) — existing
- ✓ Hindley-Milner type inference with constraint solving — existing
- ✓ Native code generation via LLVM backend — existing
- ✓ Interpreter for REPL and snapshot testing — existing
- ✓ CLI commands (run, build, check, repl) — existing
- ✓ LSP diagnostics — existing
- ✓ LSP type hover — existing
- ✓ LSP go-to-definition — existing
- ✓ Basic LSP completion — existing

### Active

- [ ] Local variable completions
- [ ] Imported symbol completions
- [ ] Module and module export completions
- [ ] Static dispatch completions (method calls)
- [ ] Record field completions
- [ ] Type and module import completions (lower priority)

### Out of Scope

- Package manager/registry — other team members handling
- New backends — not current focus
- Editor integrations beyond LSP — not current focus
- Debugger integration — not current focus
- Documentation generator — not current focus

## Context

**Rewrite Context:**
- Migrating from Rust to Zig for performance and simplicity
- Static dispatch is a significant language change affecting many internals
- Technical debt from original implementation being addressed
- Team is distributed across different focus areas

**Current State:**
- Compiler pipeline working end-to-end (parse → canonicalize → check → codegen)
- LSP has foundation: diagnostics, hover, go-to-def, basic completion
- Completion is the active development area
- Other team members working on packages, stability, type system bugs

**Codebase:**
- LSP implementation in `src/lsp/`
- Type system in `src/types/` and `src/check/`
- Canonicalization (scope resolution) in `src/canonicalize/`
- See `.planning/codebase/` for full architecture documentation

## Constraints

- **Performance**: LSP completion must respond in ≤100ms using cached analysis
- **Code Style**: Must follow existing Zig 0.15 patterns (see AGENTS.md)
- **Architecture**: Must integrate with existing staged pipeline and ModuleEnv structure

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Zig rewrite over Rust maintenance | Performance, simplicity, language changes | — Pending |
| Cached analysis for LSP performance | 100ms completion requirement | — Pending |
| Static dispatch in language | Significant language improvement | — Pending |

---
*Last updated: 2026-01-20 after initialization*
