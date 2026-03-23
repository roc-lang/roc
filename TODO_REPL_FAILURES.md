# Remaining Snapshot Failures (lir-interpreter branch)

## 1. Cross-def closure evaluation regression

**Files:**
- `test/snapshots/mono_nested_closures.md`
- `test/snapshots/mono_static_dispatch_closure.md`

These no longer panic (fixed by `ensureDefiningContextParamsBound` in Lower.zig)
but produce evaluation errors instead of folded constants. The MONO section shows
`result = add_five(3)` with `COMPTIME EVAL ERROR` instead of `result = 18`.

### The problem

The comptime evaluator (`comptime_evaluator.zig:evalAll`) evaluates each top-level
def in isolation — each def gets its own `lowerExpr` call (fresh Monomorphize +
MIR Store + MIR Lower + MirToLir + RC insert + LirInterpreter). When a def returns
a closure value (e.g. `add_five = make_adder(5)`), `tryFoldExprFromValue` can't
represent closures in CIR (closures are `.unsupported` in `fold_type.zig:225`), so
the CIR expression stays as `make_adder(5)`. When the next def
(`result = add_five(3)`) is lowered in a SEPARATE pass, the fresh lowering must
re-derive the entire call chain from stale CIR — and fails.

**This is unique to the comptime evaluator.** Every other evaluation path avoids it:
- The **dev backend** (`dev_evaluator.zig:generateCode`) receives a single expression
  and calls `lowerExpr` once. All closures are discovered by one Monomorphize pass
  and stay live in one LIR store.
- The **closure_test.zig** tests use block expressions like
  `{ make_adder = |n| |x| x+n; add5 = make_adder(5); add5(10) }` — parsed as a
  single `e_block`, lowered in one pass.
- The **snapshot tool** calls `comptime_evaluator.evalAll()` which does per-def
  evaluation — this is the only path that breaks.

### What we traced (investigation details)

**Per-def #2 (`add_five = make_adder(5)`) succeeds.** The LIR interpreter correctly
calls `make_adder`, creates the closure struct, and returns it. But `tryFoldExprFromValue`
can't fold it (closures → `.unsupported` in fold_type → `replaceExpr` returns false in
value_to_cir). The CIR expression stays unchanged.

**Per-def #3 (`result = add_five(3)`) fails.** In the fresh lowering pass:
- The CIR expression is `e_call(e_lookup_local(add_five), 3)`
- The Lower resolves `add_five` → its CIR def expr `e_call(make_adder, 5)`
- Effectively lowers `(make_adder(5))(3)` — a nested call where the callee is itself a call
- **For `mono_nested_closures`**: Lowering succeeds (LIR is a `block`), but the LIR
  interpreter hits a symbol lookup failure at `call_depth=0`. The failing symbol
  (4294967280 = 0xFFFFFFF0) is a synthetic ident created during closure call
  dispatch. It has no MIR value def, no source expr, and no lambda set. Available
  defs in the LIR store are different symbols (4294967264, 5720).
- **For `mono_static_dispatch_closure`**: The `lowerExpr` itself produces a
  `runtime_error` LIR expression — the lowering can't even produce valid LIR.

### What we tried

#### 1. Value injection into interpreter bindings
Cache the closure value from def #2 (copy bytes to arena), inject into the next
interpreter's `bindings` map before `eval()`. **Failed because**: the injected
symbol is the CIR-level symbol (`packLocalSymbolId(0, add_five_ident) = 5856`),
but the LIR interpreter looks up a SYNTHETIC symbol (`4294967280`) created during
MirToLir closure call dispatch. The symbols don't match.

#### 2. Synthetic CIR block wrapping all defs
Build a synthetic `e_block` with all defs as `s_decl` statements, lower once.
**Partially worked** — when it compiled, both tests produced correct results
(`result = 18`, `result = 15`). But caused two classes of failures:

- **MIR duplicate value definition**: Block `s_decl` calls `registerBoundSymbolDefIfNeeded`
  which registers the value def. Then `e_lookup_local` resolution (line 2904-2931 in
  Lower.zig) finds the same pattern in `module_env.all_defs` and calls
  `lowerExternalDefWithType` which registers AGAIN via line 7398. **Fix found**: adding
  `lowered_symbols.put(symbol, expr)` after `registerBoundSymbolDefIfNeeded` in `lowerBlock`
  prevents the re-registration (lowerExternalDefWithType checks `lowered_symbols` cache first).

- **Monomorphization/lowering panics on unrelated modules**: CIR expressions are
  type-checked in module-level context. Placing them inside a synthetic block changes
  the monomorphization context, causing `lowerDotAccess: field access receiver is not
  a record monotype` and `Monomorphize: conflicting monotype binding` panics on
  other modules (LSP tests, etc.). **This is fundamental** — the block approach is
  brittle because it changes the context in which expressions are lowered.

### Key code locations

| Location | What |
|----------|------|
| `comptime_evaluator.zig:evalAll` (~line 1447) | Per-def evaluation loop |
| `comptime_evaluator.zig:evalDecl` (~line 546) | Single def: lowerExpr → interpret → tryFold |
| `comptime_evaluator.zig:tryFoldExprFromValue` (~line 674) | Attempts to fold Value → CIR constant |
| `fold_type.zig:225` | Closures explicitly return `.unsupported` |
| `value_to_cir.zig:128,268,385` | Closures rejected in replaceExpr/createExpr |
| `cir_to_lir.zig:lowerExprInner` (~line 382) | Creates fresh MIR + Mono + Lower per call |
| `dev_evaluator.zig:generateCode` (~line 646) | Dev backend: single lowerExpr call (works) |
| `Lower.zig:lowerBlock` (~line 6088) | Block lowering — processes s_decl statements |
| `Lower.zig:e_lookup_local` (~line 2863) | Lookup resolution — calls lowerExternalDefWithType |
| `Lower.zig:lowerExternalDefWithType` (~line 7214) | External def lowering — registers value def (line 7398) |
| `Lower.zig:registerBoundSymbolDefIfNeeded` (~line 1254) | Registers bound symbol def |
| `MirToLir.zig:lowerCall` (~line 3716) | Call lowering — closure dispatch path |
| `MirToLir.zig:lowerClosureCall` (~line 4256) | Closure call dispatch — creates synthetic symbols |
| `interpreter.zig:evalLookup` (~line 1005) | Symbol lookup — where RuntimeError originates |

### How other compilers solve this

The problem is the **evaluate → serialize-to-IR → re-evaluate round-trip**. Closures
can't survive the serialize step. Three standard approaches:

1. **Persistent interpreter state** (Zig comptime, Rust/Miri, C++ constexpr): Keep a
   single interpreter alive across all definitions. Values stay as interpreter values —
   never need to serialize closures back to IR. The evaluator is stateful.

2. **IR-level inlining / beta-reduction** (GHC simplifier, LLVM): Don't evaluate at
   all — transform the IR. Beta-reduce `(\x -> \y -> x + y) 5` to `\y -> 5 + y` at
   the MIR level. The subsequent call `add_five(3)` then sees a concrete lambda.

3. **Rich constant representation** (JVM, .NET): Extend the IR to represent closures
   as constants. The fold-back-to-IR step always succeeds.

### Recommended next steps

The synthetic block approach proved that the LIR interpreter CAN evaluate these
closures correctly when all defs share a single lowering pass. The challenge is
doing this without changing the lowering context.

**Most promising direction: shared LIR store + persistent interpreter across defs.**

We proved this works: the synthetic block approach produced correct results
(`result = 18`, `result = 15`) when it compiled. The block failed because it changed
the lowering context (module-level defs became block-local bindings). But the
underlying principle is sound — all defs sharing ONE LIR store and ONE interpreter
is the right architecture.

The cleanest way to achieve this: add a `lowerModuleDefs(defs: []CIR.Def.Idx)`
function to `cir_to_lir.zig` that creates ONE MIR store, runs Monomorphize on all
def expressions together, and lowers them all with a single MIR Lower — but as
**top-level defs** (not block-local bindings), preserving the module-level context.
Then evaluate with a single interpreter that accumulates bindings across defs.

This matches how the dev backend works conceptually: `generateCode` in
`dev_evaluator.zig` receives a single expression, does one Monomorphize pass, and
compiles everything together. The new API would do the same but for module-level defs.

Key implementation notes:
- `Monomorphize.runExpr` currently takes a single `CIR.Expr.Idx`. Would need a
  variant that seeds from multiple root expressions (or run it on a synthetic
  wrapper that references all defs).
- `mir.Lower.lowerExpr` processes one expression. Would need to loop over defs,
  lowering each as a top-level def into the shared MIR store.
- After lowering, the single LIR store has symbol_defs for ALL defs.
- The interpreter evaluates defs in topological order, accumulating bindings.
  Closure values stay live because they're in the same interpreter.
- After evaluation, iterate defs and fold values back to CIR using
  `tryFoldExprFromValue` (closures stay unfoldable, scalars get folded).
- If evaluation crashes/errors, fall back to per-def for error isolation.

**Secondary direction: fix nested-call lowering.**

The per-def lowering of `result = add_five(3)` effectively tries to lower
`(make_adder(5))(3)` — a call where the callee is itself a call returning a closure.
The closure_test proves this pattern works inside blocks. The question is: why does
the Monomorphize/Lower/MirToLir pipeline fail to handle this pattern when started
from a top-level def context?

Specific things to investigate:
- Does `Monomorphize.runExpr` correctly trace through `e_call(make_adder, 5)` to
  discover the inner lambda's proc template and lambda set?
- In MirToLir, when `lowerCall` processes the outer call, does `lambdaSetForExpr`
  find the lambda set for the callee (which is a call result, not a direct lookup)?
- The synthetic symbols created by MirToLir's closure dispatch (4294967280 etc.) —
  are they correctly registered in the LIR store's symbol_defs?

### Long-term ideal architecture

The root of this bug — and a whole class of future bugs — is that the comptime
evaluator treats CIR as the "lingua franca" between def evaluations. It evaluates
a def's LIR, then tries to fold the result BACK to CIR so the next def can see it.
This evaluate→serialize→re-evaluate round-trip is lossy: any value that CIR can't
represent (closures today, opaque types or complex data structures tomorrow) breaks
the chain.

The ideal architecture eliminates the round-trip entirely, following how Zig's
comptime and Rust's const-eval (Miri) work:

**Principle: CIR folding is a presentation concern, not an evaluation concern.**

The evaluator should never need to serialize values back to CIR to make progress.
It accumulates values in its own memory and only folds to CIR at the end for
display (REPL output, MONO section, error messages).

```
Current (per-def, lossy round-trip):

  For each def:
    CIR → [Mono + Lower + MirToLir + RC] → LIR → [Interpret] → Value → [Fold to CIR]
    ↑ fresh stores each time                                      ↑ lossy! closures lost
    next def starts from (possibly stale) CIR ─────────────────────┘

Ideal (single pass, persistent state):

  All defs:
    CIR → [Mono + Lower + MirToLir + RC] → LIR → [Interpret all defs in order] → live Values
    ↑ one shared set of stores                ↑ one persistent interpreter
                                                bindings accumulate across defs
                                                closures stay live as interpreter values

  Then, as a separate presentation step:
    For each def: look up binding → [Fold to CIR if representable]
    (unfoldable values just keep their source CIR expression — fine for display)
```

**What this gives us:**
- Closures, partial applications, opaque values, etc. all "just work" because
  they're never serialized — they stay as live interpreter values.
- One lowering pass instead of N (performance win — no repeated Monomorphize +
  MIR Lower + MirToLir + RC insert).
- Matches how the dev backend already works (single `lowerExpr` call).
- Error isolation via interpreter checkpointing: save bindings before each def,
  roll back on crash, continue with next independent def.

**What it requires:**
- A `lowerModuleDefs` API in `cir_to_lir.zig` that lowers all defs into shared
  MIR/LIR stores while preserving module-level context (NOT as block-local bindings).
  The key difference from the synthetic block approach: defs are lowered as top-level
  defs, so monomorphization and type resolution work identically to today.
- `Monomorphize.runExpr` needs a variant that seeds from multiple root expressions
  (or iteratively adds roots to the same result).
- `mir.Lower` needs to loop over defs, lowering each into the shared MIR store.
- The interpreter evaluates each def's LIR expression in dependency order,
  accumulating bindings. After all defs, iterate bindings and fold what we can.
- The comptime evaluator's `evalAll` becomes: lower all → interpret all → fold all.

This is a bigger refactor than the quick fixes we tried, but it eliminates the
entire class of "value can't survive the CIR round-trip" problems permanently.
