# Consolidated nominal-coercion chain composition — design

**Status:** design, follow-up to PR #9792 (explicit nominal construction, issue #9770). Not yet scheduled.

**Predecessor spec:** `docs/superpowers/specs/2026-06-23-explicit-nominal-construction-design.md` — that spec called for "full consolidation" of lifting, but the landed implementation kept the pre-existing per-shape `unify` arms for low risk and added the literal backing-chain walk alongside them. This spec finishes that consolidation.

**Related issues:** #9793 (cross-module opaque construction no diagnostic), #9794 (cross-module chain non-traversal).

---

## Problem

Coercing a value into a nominal type is implemented as several **ad-hoc, per-shape** mechanisms that behave **inconsistently across transparent-newtype chains**. They share one root cause: each does its own one-off lifting instead of recursing through the backing chain. Three concrete gaps:

1. **Literal path over-walks (shadows mid-chain custom conversions).**
   `literalConstraintSatisfiedByNominalBacking` (`src/check/Check.zig`) descends a nominal's backing chain looking for a builtin `Num`/`Str`, validating the literal against the *terminal* builtin. PR #9792 gated it so a *root* custom `from_numeral` wins, but a **mid-chain** custom conversion is still skipped.
   - Repro (currently `INVALID NUMBER`, should be `Outer`):
     ```roc
     Inner := U8.{
         from_numeral : Numeral -> Try(Inner, [InvalidNumeral(Str)])
         from_numeral = |_| Ok(Inner.(0))
     }
     Outer := Inner
     x : Outer
     x = 300
     ```
     The walk descends `Outer → Inner → U8` and validates `300` against `U8` (out of range), shadowing `Inner`'s own `from_numeral`, which would accept it.

2. **Structural path under-walks (chains don't compose).**
   `unifyRecordWithNominal` / `unifyTagUnionWithNominal` (`src/check/unify.zig`) require the nominal's **immediate** backing to be the matching shape and return `TypeMismatch` otherwise — no recursion into an inner nominal.
   - Repro (currently `TYPE MISMATCH`, should be `Outer`):
     ```roc
     Inner := { x : U64 }
     Outer := Inner
     p : Outer
     p = { x: 1 }
     ```
   - The single-level form (`Point := { x: U64 }; p : Point = { x: 1 }`) works; the chained form does not. Same for tag unions.

3. **Cross-module chains don't traverse (#9794).**
   The chain walk resolves backing vars only in the current module's type store, so it cannot descend through an *imported* newtype's backing — transparent or opaque.

The net inconsistency: a numeric literal lifts through `Outer := Inner := U64`, but a record literal does **not** lift through `Outer := Inner := { … }`, and a mid-chain custom conversion is silently ignored. One conceptual operation — "lift a value through transparent newtypes into the outer nominal" — with three different, divergent implementations.

## Semantics decision to ratify first

**Adopt: "first conversion/shape in the backing chain wins."** For `Outer := Inner`, constructing/coercing into `Outer` uses the first provider found while descending `Outer`'s backing chain:
- a nominal that defines its own `from_numeral`/`from_quote` (custom method), or
- a builtin `Num`/`Str` (builtin conversion), or
- a matching structural shape (record/tag/tuple) to unify against.

So `Outer := Inner` inherits `Inner`'s custom `from_numeral`; `Outer := Inner := { … }` lifts a record literal through to `Inner`'s record backing.

**Alternative (rejected):** "only the terminal builtin's conversion is inherited; mid-chain custom methods and intermediate structural shapes are ignored." This is closer to today's literal behavior but is the source of the inconsistency and silently discards a user's explicit mid-chain conversion.

This decision must be made before implementation; the design below assumes first-in-chain-wins.

## Proposed design

Replace the three mechanisms with **one backing-chain resolution rule**, expressible two equivalent ways:

- **Operationally:** unify the candidate value against a *fresh copy* of the nominal's backing **var**, and let unification recurse. When the backing is itself a nominal, the same arm re-enters and descends one level; when it is a structural shape, fields/tags unify; when it is a builtin and the value is a literal, the literal constraint discharges against it. The result type is always the outermost nominal.
- **Declaratively:** the first nominal in the backing chain that *provides* the needed conversion or shape wins; dispatch to it (custom method, builtin conversion, or structural unification) and re-wrap the result up the chain.

This composes through chains for free (recursion is what unification already does), handles root + mid-chain + builtin uniformly, and lets us **delete**:
- the special-case `literalConstraintSatisfiedByNominalBacking` walk,
- the root-precedence gate PR #9792 added in the deferred-constraint loop,
- and the per-shape `unify` arms' requirement that the *immediate* backing match (they become "unify against the backing var" + recursion).

How each gap closes:
- **Gap 1 (mid-chain literal):** descending stops at the first nominal that provides `from_numeral` — `Inner`'s custom method — instead of walking past it to the builtin.
- **Gap 2 (structural chain):** unifying the record/tag value against the backing *var* re-enters the nominal arm for an inner nominal backing, so chains compose.
- **Gap 3 (cross-module):** the recursion must pull an imported nominal's backing into the current type store (cf. `copyVar` / `resolveVarFromExternal`) so the descent can cross the module boundary; this is the prerequisite work for #9794.

## Invariants to preserve

- **Hard rule:** two nominals of different identity never unify; a value already typed as some nominal/primitive never *implicitly* lifts into a different nominal — only literals and constructor expressions coerce. Committed values use explicit `Nominal.(…)`. (`|n| n : U64 -> Distance` stays a `TYPE MISMATCH`.)
- **Read-only backing during unification:** unify against a *fresh copy* of the backing var, never the stored backing var, preserving the `structurallyIncompatiblePair` soundness fence (backings are only read during a probe).
- **Per-level opacity:** re-check `canLiftInner` at every descended level so a literal cannot be coerced through a foreign opaque boundary even when an outer wrapper is transparent.

## Scope / non-goals

- This is a **refactor/consolidation of the type-checker's coercion logic**, not new surface syntax. The construction syntax (`Nominal.(…)`, `.{…}`, `.Tag(…)`) and the literal-vs-committed boundary are unchanged.
- New capability required: **inherited literal-conversion dispatch through the backing chain** (run an inner nominal's `from_numeral`/`from_quote` for an outer-typed literal and re-wrap). This is the main implementation effort.
- Cross-module traversal (#9794) is in scope as the gap-3 prerequisite but may be staged separately.

## Test plan

A target test set lands alongside this spec in `src/check/test/type_checking_integration.zig` under the "CONSOLIDATED NOMINAL-COERCION CHAIN COMPOSITION" header. Today:
- **Green (controls):** single-level record lift; single-level tag lift; (existing) single-level numeric/string literal lift; root custom `from_numeral`; explicit construction; committed-value-does-not-lift.
- **Red (targets, this refactor makes them green):** record literal through a newtype chain; tag through a newtype chain; mid-chain custom `from_numeral`.

The refactor is complete when all target tests are green with **no regression** to the existing PR #9792 tests, the full Zig suite, and multi-backend eval — and the deleted mechanisms (special walk + root gate + per-shape immediate-backing checks) are gone.
