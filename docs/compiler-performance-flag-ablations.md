# Final compiler performance flag ablations

The final suite has three strong marginal wins: nominal views, constructor
projection, and early CTFE cache reuse. Inhabitedness memoization has a modest
cold benefit; tag projection and settled scratch are effectively neutral in
this workload; constant completion's individual elapsed-time benefit remains
unproven. There is **no demonstrated meaningful net-negative flag**, but that is
not evidence that every flag improves speed. Keep the current suite under the
requested “retain unless net-negative” rule; do not remove a component based on
the small differences below. Neutral components need a separate engineering
justification, not a claimed benchmark win.

User decision: retain tag projection for its measured artifact saving, rather
than a speed claim. Removing it added 891,264 aggregate cache bytes in both the
inline and shared workloads (about 0.85 MiB; 166 files per cache root).

This closes the missing *final-suite marginal comparison*, not the full
interaction problem. Every ablation is the same final all-on compiler with
exactly one feature removed. Benefits are conditional on the other six
features being enabled; neither standalone feature tests nor the previous
all-off comparison establish these marginal contributions. Existing rollout
and measurement reports are unchanged.

## Identity and execution

All eight binaries were freshly built with Zig 0.16.0, ReleaseFast,
`zig build roc -j2 -Doptimize=ReleaseFast -Dperf-all`, adding exactly
`-Dperf-<removed-feature>=false` for each ablation. Generated build options were
inspected and retained with their complete contents and hashes. The all-on mask
is 127; removed-bit masks and binary SHA-256 values are:

| Removed feature | Mask | Binary SHA-256 |
| --- | ---: | --- |
| none | 127 | `14b30846d2543ffc647b96a2f5bc5aae1e532ba6a8495c80447d86856e5a878d` |
| inhabitedness-memo | 126 | `6c87f6000534f05b62d043e782933b28db17e686e84772b74b1fba0ca417bece` |
| nominal-views | 125 | `4301dad17d73a7c211f6313b1f56534cfaa23b2fcb219cec375ff426d88e89a2` |
| constructor-projection | 123 | `03208ac2159b22976e17e82a1a2b54e5a68b2a154c81fcd674056aed57cc3841` |
| tag-projection | 119 | `907ebce8c20be2f3f10fa2452dad85acca8e8b6da1e6a333685eba471dabceee` |
| settled-scratch | 111 | `bf0a33123b92752e764721d9d841ad76e7f967f2b5056ff00b14173332fd0446` |
| early-ctfe-cache | 95 | `eadb7a97942b1bbcc1f277d5beaf6d06808493f334f7547c3b5034cf3a22898c` |
| const-completion | 63 | `48ccff86db5e951d84b9a7fcfd85d58745f15573e3c7171fc46296df833da8cc` |

The complete retained final source manifest matched before/after each build
and paired matrix. In particular, `src/check/exhaustive.zig` SHA-256 remained
`ab1aea43ada9dbb86e37f768d944a97e4819d65f750d949d2f9919ea20ce0efb`.
No old prototype binary was reused. No compiler source edits, Git diagnostics,
publishing, or historical-input edits were performed.

Read-only inputs were the retained baseline and null-study directories:

```text
original: /Users/rtfeldman/code/roc-pg/.delta/worktrees/9g3vmbn2qc0m/roc-pg/local/baseline
inline:   /Users/rtfeldman/code/roc-pg/.delta/worktrees/vme4vvh6eg3a/roc-pg/local/null-study/inline
shared:   /Users/rtfeldman/code/roc-pg/.delta/worktrees/vme4vvh6eg3a/roc-pg/local/null-study/shared
```

`Actions.roc` / `Node.roc` hashes were reverified for every measurement:

| Source | Actions SHA-256 | Node SHA-256 |
| --- | --- | --- |
| original | `cdd2d59be288ecce0cbadebd0d703f575aae9bdc359a6ceb4ad697ef89b79120` | `581da93bb16cc146b5f86d5402fdd753abbfa56250c8d0cfce0a307b7b2b874c` |
| inline | `d04e9915ab6971f52932a088202cf5500781855ed4c689f88ee22740ea2536c8` | `f1bf21d8ec43fe8d5d4a0a049d77caad34f6579c0eac53fc48e9a734369f0603` |
| shared | `2575bfbbb0387545c0977e44a97de29aa50b054dc5b561980f95797a313181b3` | `f1bf21d8ec43fe8d5d4a0a049d77caad34f6579c0eac53fc48e9a734369f0603` |

All three `examples/small/main.roc` inputs matched
`3c938c3e3e572ddb22b44c91a28d9edc4af9313153ceacc8d7ec07750ac70685`.
App-edit fixtures were copied only into this worktree. Each measured edit
added a distinct comment to the app; all other `.roc` files were hash-checked
unchanged.

## Method and decisions

The **full common matrix** ran for all seven removals, including shared-null:
three cold `check package/main.roc --no-cache --jobs=2 --timings` repetitions,
and three warm `build examples/small/main.roc --opt=dev --jobs=2 --timings
--verbose` repetitions after two successful warmups. Each ablation had fresh
per-source/per-pair caches and its own all-on anchors, interleaved with reversed
ordering. App-edit pairs likewise had independent caches and two warmups.
Warm measurements verified 61/61 module hits; all 42 measured app-edit runs
verified 60/61 hits with precisely one app module rebuilt.

Primary evidence contains 406 runs (294 measured plus 112 warmups); 24 further
paired cold confirmations are kept separately, not silently pooled into the
primary medians. `/usr/bin/time -l` wall time, user/system time and RSS are
retained, alongside complete compiler phase/counter output. Phase timings are
nested and must not be added together.

The percentages below are `(removed / paired all-on - 1) * 100`:
**positive means the feature helps**. O/I/S mean original/inline/shared.

| Feature | Cold O / I / S | Warm O / I / S | Inline app edit | Recommendation |
| --- | --- | --- | --- | --- |
| inhabitedness memo | +2.7% / +0.6% / +1.9% | -0.7% / +1.4% / +0.7% | 0.0% | **Keep, modest benefit.** Original confirmation shrank to +0.8%; no meaningful warm effect. Primary cold RSS was slightly higher with memo; this memory difference did not consistently reproduce. |
| nominal views | +17.6% / +37.6% / +33.4% | +17.5% / +7.1% / +7.9% | +2.0% | **Strong keep.** Large cold gains, lower warm memory and artifact bytes; final-source marginal evidence, not the earlier pre-pruning no-views binary. |
| constructor projection | +27.8% / +15.7% / +16.2% | +14.0% / +4.9% / +5.0% | +1.2% | **Strong keep.** Cold and warm improvements, substantial memory/artifact savings. |
| tag projection | +0.1% / +0.3% / -0.2% | 0.0% / +1.4% / -0.7% | 0.0% | **Neutral.** Small artifact saving; speed differences are noise-sized and mixed. Keep under the stated rule, not as a demonstrated speed win. |
| settled scratch | +0.4% / -0.1% / -3.2% | 0.0% / 0.0% / 0.0% | 0.0% | **Neutral / possible tiny cold overhead.** Noisy shared result shrank to -0.7% on confirmation. No demonstrated meaningful net negative. Separate scratch ownership is an engineering rationale, not a measured speed benefit. |
| early CTFE cache | approximately 0% / -0.2% / -1.1% | +1901.4% / +1097.9% / +4395.0% | +404.7% | **Strong keep, explicit tradeoff.** Massive cached-build/body-skip benefit; small no-cache overhead/noise (shared confirmation -0.3%). There are no early object hits in no-cache runs. |
| constant completion | +2.5% / +0.7% / +0.9% | -0.7% / -0.7% / +0.7% | -0.2% | **Unproven / approximately neutral.** Original confirmation reversed to -0.3%, with overlapping ranges. No defensible independent large benefit, especially on warm shared-null. Keep under the stated rule without claiming positive marginal speed. |

Neither constant completion nor settled scratch should be removed on these
small observations. If the acceptance rule instead becomes “every feature
must prove a material speedup,” tags, scratch and completion do not pass it;
memo's benefit is modest rather than decisive. No arbitrary combined
cold/warm score is used. Any future production change belongs to the parent,
not this measurement work.

## Complete primary elapsed-time table

Seconds are **median [minimum–maximum]**, three repetitions per entry.
Each row uses its own nearby all-on anchors; using a single historical
baseline would exaggerate the small effects.

| Removed feature | Workflow/source | Paired all-on | One removed |
| --- | --- | --- | --- |
| memo | cold O | 31.29 [30.84–31.37] | 32.15 [32.00–32.98] |
| memo | cold I | 19.09 [18.90–19.47] | 19.21 [18.97–19.61] |
| memo | cold S | 18.86 [18.83–18.91] | 19.21 [18.95–19.28] |
| memo | warm O | 1.43 [1.43–1.43] | 1.42 [1.42–1.42] |
| memo | warm I | 1.40 [1.40–1.42] | 1.42 [1.41–1.42] |
| memo | warm S | 1.41 [1.41–1.42] | 1.42 [1.40–1.42] |
| memo | edit I | 4.02 [3.97–4.02] | 4.02 [4.02–4.04] |
| nominal views | cold O | 30.58 [30.54–30.74] | 35.96 [35.82–36.03] |
| nominal views | cold I | 18.80 [18.78–19.72] | 25.86 [25.29–26.19] |
| nominal views | cold S | 18.73 [18.69–18.92] | 24.98 [24.95–25.11] |
| nominal views | warm O | 1.43 [1.42–1.43] | 1.68 [1.68–1.69] |
| nominal views | warm I | 1.41 [1.41–1.41] | 1.51 [1.51–1.52] |
| nominal views | warm S | 1.39 [1.39–1.40] | 1.50 [1.49–1.50] |
| nominal views | edit I | 4.03 [4.01–4.03] | 4.11 [4.11–4.12] |
| constructor projection | cold O | 30.59 [30.53–31.82] | 39.08 [39.03–39.24] |
| constructor projection | cold I | 18.77 [18.74–18.98] | 21.72 [21.57–21.72] |
| constructor projection | cold S | 19.17 [18.80–19.68] | 22.28 [21.72–22.68] |
| constructor projection | warm O | 1.43 [1.42–1.43] | 1.63 [1.63–1.64] |
| constructor projection | warm I | 1.42 [1.41–1.42] | 1.49 [1.48–1.49] |
| constructor projection | warm S | 1.41 [1.40–1.42] | 1.48 [1.47–1.48] |
| constructor projection | edit I | 4.03 [4.02–4.04] | 4.08 [4.07–4.09] |
| tag projection | cold O | 30.68 [30.54–31.28] | 30.71 [30.54–30.91] |
| tag projection | cold I | 18.81 [18.76–18.81] | 18.87 [18.80–18.96] |
| tag projection | cold S | 18.76 [18.67–18.81] | 18.73 [18.64–18.84] |
| tag projection | warm O | 1.43 [1.43–1.43] | 1.43 [1.43–1.45] |
| tag projection | warm I | 1.41 [1.41–1.42] | 1.43 [1.42–1.43] |
| tag projection | warm S | 1.41 [1.40–1.41] | 1.40 [1.39–1.41] |
| tag projection | edit I | 4.02 [4.01–4.03] | 4.02 [4.01–4.03] |
| settled scratch | cold O | 30.53 [30.46–30.73] | 30.65 [30.48–30.83] |
| settled scratch | cold I | 18.98 [18.84–18.98] | 18.97 [18.87–19.10] |
| settled scratch | cold S | 19.69 [18.72–19.74] | 19.06 [18.88–19.64] |
| settled scratch | warm O | 1.43 [1.43–1.44] | 1.43 [1.42–1.44] |
| settled scratch | warm I | 1.41 [1.41–1.41] | 1.41 [1.40–1.42] |
| settled scratch | warm S | 1.41 [1.40–1.42] | 1.41 [1.41–1.41] |
| settled scratch | edit I | 4.03 [4.01–4.03] | 4.03 [4.02–4.03] |
| early CTFE cache | cold O | 30.52 [30.51–30.76] | 30.51 [30.42–30.57] |
| early CTFE cache | cold I | 18.84 [18.82–19.02] | 18.80 [18.70–18.81] |
| early CTFE cache | cold S | 18.84 [18.68–18.86] | 18.63 [18.61–18.71] |
| early CTFE cache | warm O | 1.42 [1.42–1.42] | 28.42 [28.37–28.43] |
| early CTFE cache | warm I | 1.42 [1.42–1.42] | 17.01 [16.98–17.02] |
| early CTFE cache | warm S | 1.40 [1.40–1.40] | 62.93 [62.90–63.60] |
| early CTFE cache | edit I | 4.03 [4.02–4.03] | 20.34 [20.34–20.37] |
| constant completion | cold O | 30.61 [30.49–31.34] | 31.37 [30.77–31.78] |
| constant completion | cold I | 18.77 [18.73–19.01] | 18.90 [18.81–18.95] |
| constant completion | cold S | 18.67 [18.65–18.68] | 18.83 [18.71–18.87] |
| constant completion | warm O | 1.44 [1.44–1.45] | 1.43 [1.42–1.43] |
| constant completion | warm I | 1.43 [1.41–1.52] | 1.42 [1.41–1.43] |
| constant completion | warm S | 1.44 [1.44–1.45] | 1.45 [1.44–1.45] |
| constant completion | edit I | 4.02 [4.02–4.03] | 4.01 [4.01–4.03] |

### Independent cold confirmations

These reverse initial pair order and use the same frozen binaries, inputs and
no-cache commands. They prevent treating one noisy triplet as decisive.

| Removed feature/source | All-on seconds | Removed seconds | Interpretation |
| --- | --- | --- | --- |
| memo / original | 30.61 [30.56–30.75] | 30.85 [30.79–30.94] | +0.8% removal cost; modest, reproducible direction, smaller than initial +2.7%. |
| completion / original | 30.67 [30.64–31.82] | 30.58 [30.50–31.91] | -0.3%; initial +2.5% benefit did not reproduce. |
| scratch / shared | 18.72 [18.66–18.73] | 18.59 [18.58–18.64] | -0.7%; possible small overhead, not the initial -3.2% magnitude. |
| early cache / shared | 18.70 [18.67–18.71] | 18.64 [18.57–18.65] | -0.3%; no-cache overhead/noise, not reuse. |

## Memory, artifacts and causal counters

Median maximum RSS in GiB, all-on → removed:

| Feature removed | Cold O / I / S | Warm O / I / S | Inline edit |
| --- | --- | --- | --- |
| memo | 6.06→5.93 / 4.14→4.08 / 4.11→4.08 | 1.07→1.08 / 1.02→1.01 / 0.99→0.98 | 2.03→2.03 |
| nominal views | 5.97→6.81 / 4.08→4.44 / 4.06→4.38 | 1.07→2.79 / 1.02→1.63 / 0.98→1.60 | 2.03→2.62 |
| constructor projection | 5.98→6.74 / 4.07→4.38 / 4.03→4.40 | 1.08→2.39 / 1.01→1.44 / 0.99→1.41 | 2.02→2.45 |
| tag projection | 6.01→6.05 / 4.06→4.08 / 4.03→4.08 | 1.07→1.10 / 1.01→1.02 / 1.00→0.99 | 2.00→2.02 |
| settled scratch | 5.92→5.98 / 4.05→4.09 / 4.00→4.00 | 1.07→1.09 / 1.02→1.03 / 1.00→0.99 | 2.02→2.01 |
| early cache | 5.95→5.94 / 4.09→4.04 / 4.06→3.98 | 1.07→6.11 / 1.01→4.40 / 1.00→4.10 | 2.02→4.87 |
| completion | 6.10→5.97 / 4.08→4.06 / 4.05→4.04 | 1.07→1.08 / 1.02→1.01 / 0.97→0.98 | 2.03→2.01 |

After identical two-warmup/three-measurement build workflows, aggregate cache
file bytes (166 files each) are:

| Configuration | Inline bytes | Shared bytes |
| --- | ---: | ---: |
| all-on | 294,704,941 | 274,900,089 |
| no nominal views | 623,725,021 | 603,920,169 |
| no constructor projection | 524,221,725 | 504,416,873 |
| no tag projection | 295,596,205 | 275,791,353 |
| other four removals | 294,704,941 | 274,900,089 |

These are aggregate artifact bytes, not solely serialized types or objects.
The large warm RSS improvements from views/projection coexist with nearly
unchanged shared-CTFE phase time: smaller cached checker state has a separate
cost outside that phase. For example, nominal views removed changes warm
shared total from 1.39 to 1.50 s and RSS from 0.98 to 1.60 GiB, while shared
lowering/CTFE is 1224 → 1223 ms. Cold constructor removal changes original
total 30.59 → 39.08 s while that phase stays 24139 → 24052 ms. Whole-compiler
measurements therefore matter more than timing only the lowering phase.

Early-cache causal evidence is especially strong:

| Workflow | Shared CTFE ms, all → removed | Early object hits, all → removed | Shared body contexts, all → removed |
| --- | --- | --- | --- |
| warm original | 1236→28160 | 339→0 | 42120→284058 |
| warm inline | 1236→16782 | 339→0 | 42108→210761 |
| warm shared | 1228→62698 | 339→0 | 42105→210872 |
| app-edited inline | 3176→19449 | 503→0 | 68147→242275 |

These are medians from the early-cache pair, not counts inferred from RSS.
Shared-null still becomes expensive without early reuse despite constant
completion remaining on: completion alone does not solve that late replay
cost. Conversely, completion's no-op warm ablation while early reuse remains
on does not prove completion has no value when early reuse is unavailable.
That is precisely the limit of marginal, rather than factorial, comparisons.

## Correctness, controls and retained evidence

All eight configurations passed all 383 inline parser expectations in
separate correctness caches. Each also passed
`BoxyProjectedNominalConstruction`, `JsonNestedNominalContract`, and
`ProjectedNominalLoopCondition` tests and three no-cache checks each.
Small-check timings stayed at 0.01 s, 0.05–0.06 s and 0.02 s respectively;
the timer's 0.01 s granularity makes relative percentages misleading here.
No obvious global overhead was observed, but these controls cannot establish
zero overhead for every small program.

Every built app was executed; exit status, stderr and stdout were checked
against the retained correct runtime output, including all warmups and edits.
All cold checks exited successfully. There is no failed-correctness
configuration being presented as a valid performance comparison.

Owned artifacts live under `.tmp/flag-ablations/`:

- `run.py`: frozen-source verification, fresh one-bit builds, option-mask
  proof, correctness controls, serialized paired matrix and app-only edits.
  It reads the earlier retained `measure.py` helper without modifying that
  checkout; the helper's complete source/hash is additionally retained in
  `retained-helper.json`.
- `confirm.py`: the four independent small paired confirmation sets.
- `summarize.py`, `measurements.csv`, `summary.json`, `summary.stdout`:
  all 430 raw main/warmup/confirmation measurements, plus 98 primary
  three-replicate summaries with elapsed time, RSS, phases and counters.
- `validate.py`, `validation.json`: independent audit of all eight binary
  hashes, frozen source/input identities, 430 successful runs, 280 correct
  runtime outputs, 104 correctness/control runs and all 49 primary report
  rows against the extracted measurements.
- `builds/`, `binaries/`, `runs/`, `controls/`, `artifacts/`, `fixtures/`:
  commands, source/binary hashes, generated option contents, full
  stdout/stderr/time/runtime logs, cache byte counts and owned edit sources.
- `source-manifest.json`, `suite.stdout`, `suite.stderr`,
  `confirm.stdout`, `confirm.stderr`: source freeze and execution evidence.
- `retain.py`, `complete-text-evidence.jsonl`: complete text artifacts with
  individual hashes, preserving logs even where generated directories are
  machine-local (3,142 text artifacts, approximately 47 MB).

Heavy work was serialized. Initial disk availability was 50 GiB; the retained
matrix used approximately 25 GiB, with about 30 GiB available at completion.
Custom `.tmp/flag-ablations/generated-build-cache` and `generated-install`
avoided periodic `.zig-cache`/`zig-out` deletion. Only this runner's completed
build cache and install prefix were reclaimed after preserving each binary
and its options proof; benchmark caches and logs were retained. No ENOSPC,
broad cache deletion, full minici, or parallel heavy worker was used.

The final source manifest and input identities were reverified after the
confirmations. The heavy window is released; no build/benchmark process
remains active for this work.
