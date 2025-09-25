    +# Interpreter2 Progress, Design, and Roadmap

    This document summarizes the new Interpreter2 work: what’s been built, how it works, and what remains. It’s meant as a
        single place to get oriented and plan next steps.

    ## High‑Level Goals

    - Adopt a type‑carrying interpreter architecture compatible with Roc’s polymorphism and unboxed values.
    - Keep runtime performance characteristics attractive: O(1) Var→Layout lookup, avoid repeated freshening/unification
        at hot call sites.
    - Preserve a clean migration path by building the new interpreter alongside the current one, with its own tests.
    - Add a minimal evaluator so we can run end‑to‑end Roc‑syntax tests that begin and end with “what the user sees” at
        the REPL.

    ## What’s Implemented

    ### 1) Interpreter2 scaffolding
    - New module: `src/eval/interpreter2.zig` exported as `Interpreter2` from `src/eval/mod.zig`.
    - Interpreter2 owns its own runtime type store and layout store, separate from compile‑time stores.
    - Interpreter2 provides:
      - A translation cache from compile‑time `types.Var` to runtime `types.Var` (memoized deep copy).
      - A per‑var slot array for O(1) Var→Layout lookup (zero = unset, otherwise `layout_idx + 1`).
      - A polymorphic instantiation cache (poly cache) keyed by function id + root arg types.
      - Integration with the runtime unifier to constrain return types at call preparation time.
      - A small stack allocator (`stack.Stack`) for minimal evaluation.

    ### 2) Type‑Carrying Architecture
    - Keep types as runtime type variables (one unified runtime `types.Store`) instead of prematurely converting to
        layouts.
    - Convert var→layout lazily and cache by slot to avoid hash map lookups in hot paths.
    - Translate compile‑time vars to runtime vars:
      - Implemented support: str, numeric compacts (int/frac), int/frac precision forms, tuple, record, alias, nominal,
        flex, rigid, function types (pure/effectful/unbound).
      - Record variants (`record`, `record_unbound`, `record_poly`) translate to concrete runtime records.

    ### 3) O(1) Var→Layout slot array
    - `var_to_layout_slot: []u32` indexed by the root runtime var id (resolved var) and stores `layout_idx+1`.
    - Eliminates hash‑based lookups for layout, which happen heavily during eval.
    - Extended to ensure capacity as the runtime type store grows.

    ### 4) Polymorphic instantiation cache (poly cache)
    - `PolyKey = { func_id, arity, args[] }` where args are runtime root vars; `PolyEntry = { return_var,
        return_layout_slot }`.
    - `prepareCall(func_id, args, hint?)`: on hit returns entry; on miss with a hint inserts entry for fast paths.
    - `prepareCallWithFuncVar(func_id, func_type_var, args)`: integrates runtime unification to constrain the return type,
        then caches result.

    ### 5) Minimal evaluation path
    The goal is not full semantics yet, but enough to run end‑to‑end Roc tests and exercise the type‑carrying pieces.

    - Implemented expressions:
      - Strings: `e_str`, `e_str_segment` (create `RocStr` with `RocOps`), REPL‑style string rendering.
      - Integers: `e_int`, and `e_binop` with `+` (reads/writes using runtime‑chosen layout), REPL‑style integer
        rendering.
      - Tuples: `e_tuple` (allocate and fill via layout store accessors), REPL‑style rendering `(a, b, ...)`.
      - Records: `e_record` (allocate and fill by field name via accessor), REPL‑style rendering `{ x: 1, y: 2 }`.
      - Lambdas: `e_lambda` as minimal placeholder; `e_call` supports a one‑arg lambda by binding the parameter to the
        evaluated argument and evaluating the body; `e_lookup_local` finds values in a simple binding stack.
      - Nominal wrappers: `e_nominal` / `e_nominal_external` delegate evaluation to the backing expression.

    - Not implemented in minimal path (intentionally deferred): booleans, `if`, `match` tag destructuring, guards, tag
        unions, lists, boxes, effects, and most operators. (See Roadmap.)

    ### 6) REPL‑style rendering
    - `renderValueRoc(value: StackValue) -> []u8` for a subset of shapes:
      - Strings render as quoted Roc strings with `"` and `\` escaping.
      - Integers render as decimal.
      - Tuples render `(elem1, elem2, ...)`.
      - Records render `{ x: 1, y: 2 }`.
      - Unknown types render `<unsupported>` as a placeholder during bring‑up.

    ### 7) Roc‑syntax tests (begin/end with Roc code)
    - New file: `src/eval/test/interpreter2_style_test.zig`.
    - Tests parse and canonicalize with early failure (using helpers) to surface syntax or type issues immediately.
    - Tests then exercise Interpreter2 minimal eval and assert on REPL‑style rendered results for readability:
      - `(|x| x)("Hello")` → `"Hello"`
      - `(|n| n + 1)(41)` → `42`
      - `(1, 2)` → `(1, 2)`
      - `{ x: 1, y: 2 }` → `{ x: 1, y: 2 }`

    ## Design Summary

    - Single runtime `types.Store` dedicated to the interpreter, decoupled from compile‑time stores.
    - Translation layer from compile‑time var → runtime var (memoized) so cross‑module types unify correctly at runtime.
    - O(1) slot array for Var→Layout to avoid hash maps in hot paths.
    - Poly cache to avoid re‑instantiating and unifying the same polymorphic call on hot loops.
    - Minimal evaluator runs directly on the canonical IR and uses the runtime layout store for sizes/offsets.
    - REPL‑style rendering helps make tests readable and validate end‑user visible output quickly.

    ## What Remains (Roadmap)

    The list below describes the large remaining areas, with suggested order of implementation and test approach.

    1) Closures and captures
    - Construct concrete closures (header + captures record) instead of placeholders for `e_lambda`.
    - Support `e_closure` creation and pass captures correctly into calls.
    - Test: lambda that uses a captured variable (Roc code), evaluate and check rendered output.

    2) Multi‑argument lambdas and nested calls
    - Add binding and calling convention for `|a, b| ...` and support nested lambda calls.
    - Ensure arg order and binding semantics match canonical IR expectations.
    - Test: `(|a, b| a + b)(1, 2)` → `3`; nested lambdas applying each other.

    3) Booleans
    - Implement booleans via type‑driven logic (no string name hacks):
      - Render as `True`/`False` when the runtime layout/type indicates boolean.
      - `!`, `and`, `or` implementations.
    - Test: `!Bool.True` → `False`, `Bool.True and Bool.False` → `False`.

    4) If‑expressions
    - Evaluate `e_if` using runtime conditions (boolean), not string comparisons.
    - Test: `if Bool.True "yes" else "no"` → `"yes"`.

    5) Match (wider coverage)
    - Current minimal support: assign, underscore, int & string literals, nominal passthrough, OR patterns.
    - Progress:
      - Tuple destructuring patterns now match in Interpreter2 (see `match (1, 2)` test).
    - TODO:
      - Record destructuring patterns (bind sub-components recursively).
      - Tag union patterns (tag name and payloads) once tag unions are represented for runtime.
      - Guards support.
    - Add tests incrementally (starting from simple patterns), always with Roc syntax and early compilation failure
        checks.

    6) Tag unions
    - Represent zero‑arg and payload tags with concrete layouts.
    - Add evaluation and rendering for tags.
    - Add pattern matching on tags.
    - Test: simple ADTs with zero‑arg tags and with payloads.

    7) Lists and boxes
    - Add runtime list/box layout and evaluation for basic operations.
    - Rendering for `[1, 2, 3]` and `Box(42)` (or the Roc rendering thereof).

    8) Full function call path
    - Move beyond `prepareCallWithFuncVar` into full minimal call execution with type‑carrying propagation.
    - Unify parameter types with argument runtime vars consistently and constrain return type at runtime.
    - Tests for polymorphic functions (e.g., identity) across module boundaries.

    9) Error handling & diagnostics integration
    - Expose fast‑fail diagnostic printing in Interpreter2 tests similar to helpers.
    - Good error messages when evaluation fails due to NotImplemented vs TypeMismatch.

    10) Performance passes (once feature‑complete subset exists)
    - Profile slot array usage, translation cache, and poly cache hit rates.
    - Confirm we avoid needless unification and allocations in hot loops.

    ## Key Files and Entry Points

    - `src/eval/interpreter2.zig` — Interpreter2 implementation and minimal evaluator.
    - `src/eval/mod.zig` — exports Interpreter2 and wires tests.
    - `src/eval/test/interpreter2_style_test.zig` — Roc‑syntax end‑to‑end tests for Interpreter2.
    - `src/layout/store.zig` — layout store used by Interpreter2 at runtime.
    - `src/types/*` — stores, descriptors, and unifier used both compile‑time and runtime.
    - `src/eval/StackValue.zig` — helpers for reading/writing typed values while respecting layouts.

    ## How to Run

    - Run all tests (including Interpreter2):
      - `zig build test`
    - Tests will fail fast on parsing/canonicalization problems with proper diagnostics, and then run minimal evaluation
        on Interpreter2 for covered shapes.

    ## Notes and Rationale (Highlights)

    - Slot array for Var→Layout is critical: it removes hash‑based overhead from the hottest path.
    - Runtime translation unifies types across modules in a single store, so polymorphic functions behave correctly at
        runtime.
    - Poly cache prevents repeated freshening + unification on identical call shapes (tight loops) and bounds memory
        growth.
    - Building a minimal evaluator first let us add Roc‑syntax tests and verify architecture decisions without committing
        to full semantics immediately.
    - We intentionally removed any ad‑hoc runtime name checks (e.g., booleans by tag name) and will implement those
        features with type‑driven logic.

    ## Short‑Term Next Steps (Suggested)

    1. Closures and captures (construct closures; test a captured variable case).
    2. Multi‑arg lambdas; nested calls.
    3. Type‑driven booleans + simple if; Roc‑syntax tests.
    4. Tuple/record destructuring patterns in match; tests.
    5. Tag unions representation + evaluation + rendering; tests.
    6. Full call execution with runtime unification across call boundaries (beyond prepareCall).

    This roadmap should get Interpreter2 to a robust, demonstrable subset that exercises the type‑carrying architecture in
        realistic end‑to‑end scenarios, with readable, REPL‑style tests.
