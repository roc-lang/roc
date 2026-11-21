# Range prover rules

One judgment per rewrite, **R-Valid(P, s, claim)**: in procedure P as the proof round found it, no execution that reaches
statement s violates the rewrite's claim. `rpv.py` decides it with Z3 bit-vectors at the real width, by symbolic execution
of P over all paths, unrolling each loop up to k times (default 3).

| Rule | Event | Claim checked at s (LIR before the round) | design.md section | Compiler site (`src/lir/range_prove.zig`) |
|---|---|---|---|---|
| R-Cmp | `cmp s t` | the comparison at s always evaluates to t | none (pass-internal) | `modelCompare` (rewrite to `assign_tag`) |
| R-Ovf | `ovf s t` | the overflow predicate at s always evaluates to t | "The predicate is a separate operation, not a paired result" | `modelFamilyArith`, `.overflows` mode |
| R-Proven | `proven s` | the family op at s never overflows (signedness from the operand layout) | "Only the range prover mints the proven form" | `modelFamilyArith`, then `provenForm` |
| R-Div | `div s` | the divisor at s is never 0 | "Division and shifts" | `dischargeDivisor` |
| R-Switch | `switch s v` | the switch condition at s always equals v | none (pass-internal) | `foldSwitch` |
| R-Crash | `crash s` | the op at s always overflows | "Plain arithmetic always crashes on overflow" | `rewriteAsArithmeticCrash` |
| R-Chain | `chain s i l` | `x op c1 op c2` crashes exactly when `x op (c1+c2)` overflows; checked at the inner op i and the outer op s | "Plain arithmetic always crashes on overflow" | `foldSameSignConstantChain` |
| R-Simd | `simd s n` | the count operand at s is always n | none (pass-internal) | `simd_concat_shift_bytes` annotation in the walk |

## Semantics the validator assumes

These are written from the language and the builtins, not from the pass.

- Overflow is exactly Z3's `BV*NoOverflow` / `BV*NoUnderflow` at the operand width and signedness. `*_crash_on_overflow`
  and `*_checked` ops continue only where the check passed; the crashing path ends.
- Shift counts are taken modulo the width (the `shl_wrap` / `shr_wrap` docs in `Builtin.roc`).
- A list value carries its length, below 2^63, and `list_len` of the same value agrees. `append`, `prepend`, `concat`,
  `drop_at`, `sublist`, `clear`, `with_capacity`, `reserve*`, `set*` and `map_*` have their length effects. Other list ops
  and every call return unknown values of their layout. Integer elements read by `list_get_unsafe` come from one array per
  list value.
- Bool is one bit and tag discriminants are 32 bits. Unknown aggregates are expanded lazily and memoized, so two reads of
  the same field of the same unknown value agree.

## Statuses

| Status | Meaning |
|---|---|
| `ok` | no path within the bound reaches s with the claim false. This includes statements that only infeasible paths reach, where the claim holds vacuously. |
| `cex` | a countermodel: input values and a path on which the claim is false |
| `unreached` | no path within the bound reaches s: dead code, a crash before s, or the bound |
| `skip` | the statement at s has a shape the claim check does not handle (not counted as checked) |
| `budget` | a round ran out of time or paths (counted per round and pass, not per rewrite) |

`driver.py` and `./run.sh prove` run a second pass over the rewrites the bounded pass left `unreached`. At the unrolling
bound it havocs every local assigned on the loop's cycle (the statements that reach the loop's own back edge) and keeps
going, which covers every later iteration. That pass forgets loop invariants such as `i <= len`, so on targets the bounded
pass did reach it would only add spurious countermodels, and it is not run on them.

## Mistakes in the model that were fixed

None of these were compiler bugs; they are listed because each would come back as a false countermodel.

1. The exporter first printed `bytes_literal` as `x`, but the pass reads its length. Dict internals compare a static list's
   length with 0, so `test/fx/issue_10038_top_level_dict.roc` gave countermodels. The exporter now prints `len=N`.
2. Overflow was first encoded with a (2w+2)-bit product, which made 64-bit multiplications slow enough to exhaust budgets.
   Z3's no-overflow predicates replaced it, checked equal to the wide encoding at 8 bits for all six cases.
3. Unrolling counted visits per path, so inner loops got no iterations after the first outer one. The count now restarts
   when a join is entered again through its declaration.
4. Havoc mode first havocked every local in a loop body's lexical region. In LIR that region includes the code after the
   loop's exit, and so the enclosing loop's counters, which gave spurious countermodels on
   `test/fx/le_bytes_append_loop.roc`. Now only locals assigned on statements that reach the loop's own back edge are
   havocked.
5. Havoc mode was consulted on every target. It is now only consulted on targets the bounded pass left unreached.
