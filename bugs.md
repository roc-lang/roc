# Roc New Compiler Bug Hunt

This file tracks compiler-bug repros found with `./zig-out/bin/roc`.
Obvious alternate repros for the same suspected root cause do not increase the count.

Current verified compiler-bug repro count: 200.

IDs are stable. Gaps are stale items that were rechecked with current syntax and
removed because they no longer reproduce.

Exact Roc sources live in the opt-in test files:

- Eval: `src/eval/test/eval_bughunt_repros.zig`
- CLI: `src/cli/test/bughunt_cli_repros.zig`

Run all eval repros:

```sh
zig build test-eval -- known-bugs --filter bughunt --threads 1 --timeout 5000
```

Run all CLI repros:

```sh
zig build test-bughunt-cli -- --filter bughunt --threads 1 --timeout 5000
```

Last verification results:

- `zig build test-eval -- known-bugs --filter bughunt --threads 1 --timeout 5000`
  reproduced 53 known bugs: 32 failed, 21 crashed, 0 passed.
- `zig build test-bughunt-cli -- --filter bughunt --threads 1 --timeout 5000`
  reproduced 147 known bugs: 65 failed, 79 crashed, 3 hung, 0 passed.

Removed after recheck: B003, B016, B020, B025, B027, B048, B057, B058, B064,
B067, and B069.

## Verified Bugs

| ID | Layer | Repro command | Summary |
| --- | --- | --- | --- |
| B001 | CLI | `zig build test-bughunt-cli -- --filter B001 --threads 1 --timeout 5000` | Missing import reaches a coordinator missing-env invariant instead of a user diagnostic. |
| B002 | CLI | `zig build test-bughunt-cli -- --filter B002 --threads 1 --timeout 5000` | Explicit builtin import reaches a coordinator missing-env invariant. |
| B004 | Eval | `zig build test-eval -- known-bugs --filter B004 --threads 1 --timeout 5000` | Function value stored inside a tuple constant has no sealed concrete instance. |
| B005 | Eval | `zig build test-eval -- known-bugs --filter B005 --threads 1 --timeout 5000` | Signed min-int division overflow panics in host integer arithmetic. |
| B006 | Eval | `zig build test-eval -- known-bugs --filter B006 --threads 1 --timeout 5000` | Signed min-int negate contradicts the builtin crash contract. |
| B007 | Eval | `zig build test-eval -- known-bugs --filter B007 --threads 1 --timeout 5000` | `.Dec` suffix literals lose exact decimal source precision. |
| B008 | Eval | `zig build test-eval -- known-bugs --filter B008 --threads 1 --timeout 5000` | Out-of-range `.Dec` suffix literals are not rejected. |
| B009 | Eval | `zig build test-eval -- known-bugs --filter B009 --threads 1 --timeout 5000` | Out-of-range `Dec` inference reaches a mono fraction invariant. |
| B010 | Eval | `zig build test-eval -- known-bugs --filter B010 --threads 1 --timeout 5000` | `Dec` multiplication overflow panics in the builtin implementation. |
| B011 | CLI | `zig build test-bughunt-cli -- --filter B011 --threads 1 --timeout 5000` | Calls with more than 64 arguments exceed the ARC argument mask. |
| B012 | Eval | `zig build test-eval -- known-bugs --filter B012 --threads 1 --timeout 5000` | Float conversion builtins have annotation-only bodies in runtime lowering. |
| B013 | Eval | `zig build test-eval -- known-bugs --filter B013 --threads 1 --timeout 5000` | Float-to-int wrapping conversions panic instead of wrapping. |
| B014 | CLI | `zig build test-bughunt-cli -- --filter B014 --threads 1 --timeout 5000` | `roc docs` reports parse errors as internal compiler errors. |
| B015 | CLI | `zig build test-bughunt-cli -- --filter B015 --threads 1 --timeout 5000` | `F32.div_trunc_by` and `F64.div_trunc_by` do not truncate. |
| B017 | CLI | `zig build test-bughunt-cli -- --filter B017 --threads 1 --timeout 5000` | Duplicate top-level definitions reach typed CIR invariants. |
| B018 | Eval | `zig build test-eval -- known-bugs --filter B018 --threads 1 --timeout 5000` | Duplicate tag names in nominal declarations reach canonical type invariants. |
| B019 | Eval | `zig build test-eval -- known-bugs --filter B019 --threads 1 --timeout 5000` | Duplicate record fields in nominal declarations reach canonical type invariants. |
| B021 | Eval | `zig build test-eval -- known-bugs --filter B021 --threads 1 --timeout 5000` | Duplicate type parameters are accepted and shadow earlier parameters. |
| B022 | Eval | `zig build test-eval -- known-bugs --filter B022 --threads 1 --timeout 5000` | Ellipsis expressions reach checked-artifact and mono invariants. |
| B023 | CLI | `zig build test-bughunt-cli -- --filter B023 --threads 1 --timeout 5000` | `roc test` evaluates qualified `Bool.True` incorrectly. |
| B024 | Eval | `zig build test-eval -- known-bugs --filter B024 --threads 1 --timeout 5000` | Nested closures cannot capture enclosing lambda parameters. |
| B026 | Eval | `zig build test-eval -- known-bugs --filter B026 --threads 1 --timeout 5000` | Partial record destructuring in a local function crashes mono specialization. |
| B028 | Eval | `zig build test-eval -- known-bugs --filter B028 --threads 1 --timeout 5000` | Compile-time list-pattern failures panic instead of producing a crash diagnostic. |
| B029 | Eval | `zig build test-eval -- known-bugs --filter B029 --threads 1 --timeout 5000` | Unannotated `crash` constants publish unresolved source types. |
| B030 | Eval | `zig build test-eval -- known-bugs --filter B030 --threads 1 --timeout 5000` | Out-of-range integer literals silently wrap. |
| B031 | Eval | `zig build test-eval -- known-bugs --filter B031 --threads 1 --timeout 5000` | Very large decimal fraction literals become zero. |
| B032 | CLI | `zig build test-bughunt-cli -- --filter B032 --threads 1 --timeout 5000` | Duplicate top-level type declarations reach checked-artifact invariants. |
| B033 | CLI | `zig build test-bughunt-cli -- --filter B033 --threads 1 --timeout 5000` | Top-level destructuring definitions reach checked-artifact invariants. |
| B034 | CLI | `zig build test-bughunt-cli -- --filter B034 --threads 1 --timeout 5000` | Missing platform-required app values panic during app relation validation. |
| B035 | Eval | `zig build test-eval -- known-bugs --filter B035 --threads 1 --timeout 5000` | Out-of-bounds tuple projection leaves an erroneous checked type. |
| B036 | Eval | `zig build test-eval -- known-bugs --filter B036 --threads 1 --timeout 5000` | Or-pattern alternatives can bind different names. |
| B037 | CLI | `zig build test-bughunt-cli -- --filter B037 --threads 1 --timeout 5000` | Too-many-exports diagnostic is not renderable. |
| B038 | Eval | `zig build test-eval -- known-bugs --filter B038 --threads 1 --timeout 5000` | Decimal literals can type-check as integer primitives. |
| B039 | CLI | `zig build test-bughunt-cli -- --filter B039 --threads 1 --timeout 5000` | `List.join_with` on list-valued items leaves type checking stuck. |
| B040 | CLI | `zig build test-bughunt-cli -- --filter B040 --threads 1 --timeout 5000` | `Str.from_utf8` leaves a Roc allocation behind. |
| B041 | CLI | `zig build test-bughunt-cli -- --filter B041 --threads 1 --timeout 5000` | Dotted local imports reach a coordinator missing-env invariant. |
| B042 | Eval | `zig build test-eval -- known-bugs --filter B042 --threads 1 --timeout 5000` | Type errors in match scrutinees can reach mono record lowering. |
| B043 | CLI | `zig build test-bughunt-cli -- --filter B043 --threads 1 --timeout 5000` | `Str.inspect` on wide records overflows the compiler stack. |
| B044 | CLI | `zig build test-bughunt-cli -- --filter B044 --threads 1 --timeout 5000` | Platform relation accepts an unannotated numeric return for a `{}` requirement. |
| B045 | Eval | `zig build test-eval -- known-bugs --filter B045 --threads 1 --timeout 5000` | Custom `from_numeral` literals type-check but cannot be lowered. |
| B046 | Eval | `zig build test-eval -- known-bugs --filter B046 --threads 1 --timeout 5000` | Heterogeneous custom `times` dispatch lowers the RHS as the wrong type. |
| B047 | Eval | `zig build test-eval -- known-bugs --filter B047 --threads 1 --timeout 5000` | Empty tag-union constants are planned as runtime payloads. |
| B049 | Eval | `zig build test-eval -- known-bugs --filter B049 --threads 1 --timeout 5000` | Unannotated numeric literal matches are accepted as exhaustive. |
| B050 | Eval | `zig build test-eval -- known-bugs --filter B050 --threads 1 --timeout 5000` | Chained attached-method calls lose the checked method target. |
| B051 | Eval | `zig build test-eval -- known-bugs --filter B051 --threads 1 --timeout 5000` | Recursive attached-method calls mis-specialize nominal payloads. |
| B052 | Eval | `zig build test-eval -- known-bugs --filter B052 --threads 1 --timeout 5000` | Stored returned closures can mis-specialize captured values. |
| B053 | CLI | `zig build test-bughunt-cli -- --filter B053 --threads 1 --timeout 5000` | Unsuffixed numeric method calls are not defaulted before mono dispatch. |
| B054 | CLI | `zig build test-bughunt-cli -- --filter B054 --threads 1 --timeout 5000` | Tail-recursive list concatenation leaks an intermediate list. |
| B055 | CLI | `zig build test-bughunt-cli -- --filter B055 --threads 1 --timeout 5000` | `--allow-errors` lowers missing numeric methods without a checked target. |
| B056 | Eval | `zig build test-eval -- known-bugs --filter B056 --threads 1 --timeout 5000` | Missing methods inside function bodies can bypass diagnostics. |
| B059 | Eval | `zig build test-eval -- known-bugs --filter B059 --threads 1 --timeout 5000` | Compile-time constant evaluation panics on runtime pattern failures. |
| B060 | Eval | `zig build test-eval -- known-bugs --filter B060 --threads 1 --timeout 5000` | Record-rest declaration patterns lack published decision-plan metadata. |
| B061 | CLI | `zig build test-bughunt-cli -- --filter B061 --threads 1 --timeout 5000` | Boxed builtin nominal constants lack interface capability. |
| B062 | CLI | `zig build test-bughunt-cli -- --filter B062 --threads 1 --timeout 5000` | Zero-sized list operations leak runtime allocations. |
| B063 | Eval | `zig build test-eval -- known-bugs --filter B063 --threads 1 --timeout 5000` | Bad extensible tag-union aliases panic during publication. |
| B065 | Eval | `zig build test-eval -- known-bugs --filter B065 --threads 1 --timeout 5000` | Huge integer `.Dec` suffix literals reach mono range invariants. |
| B066 | CLI | `zig build test-bughunt-cli -- --filter B066 --threads 1 --timeout 5000` | Transitive nominal method owners are not available for method lookup. |
| B068 | Eval | `zig build test-eval -- known-bugs --filter B068 --threads 1 --timeout 5000` | `Str.inspect` cannot inspect values whose type contains an uninhabited tag union. |
| B070 | CLI | `zig build test-bughunt-cli -- --filter B070 --threads 1 --timeout 5000` | Inclusive integer ranges overflow at the maximum value. |
| B071 | CLI | `zig build test-bughunt-cli -- --filter B071 --threads 1 --timeout 5000` | Unannotated imported-nominal list results can break lambda-solved value transforms. |
| B072 | CLI | `zig build test-bughunt-cli -- --filter B072 --threads 1 --timeout 5000` | `roc glue` lowers the synthetic app with a non-runtime checked expression. |
| B073 | CLI | `zig build test-bughunt-cli -- --filter B073 --threads 1 --timeout 5000` | Deep expression chains overflow the compiler stack. |
| B074 | CLI | `zig build test-bughunt-cli -- --filter B074 --threads 1 --timeout 5000` | Effectful top-level definitions are accepted as constants and then lack sealed instances. |
| B075 | CLI | `zig build test-bughunt-cli -- --filter B075 --threads 1 --timeout 5000` | `roc test` crashes when an `expect` executes a hosted stdout effect. |
| B076 | Eval | `zig build test-eval -- known-bugs --filter B076 --threads 1 --timeout 5000` | `F32.from_str` and `F64.from_str` return infinities for out-of-range inputs. |
| B077 | Eval | `zig build test-eval -- known-bugs --filter B077 --threads 1 --timeout 5000` | Signed right shifts by the type width lose the sign bit. |
| B078 | Eval | `zig build test-eval -- known-bugs --filter B078 --threads 1 --timeout 5000` | Guarded-only matches are accepted as exhaustive. |
| B079 | Eval | `zig build test-eval -- known-bugs --filter B079 --threads 1 --timeout 5000` | Nesting `Box.box` around a boxed function reaches an erased-callable LIR invariant. |
| B080 | Eval | `zig build test-eval -- known-bugs --filter B080 --threads 1 --timeout 5000` | Binding an immediately-invoked lambda that uses `return` becomes a runtime crash. |
| B081 | CLI | `zig build test-bughunt-cli -- --filter B081 --threads 1 --timeout 5000` | `List.join_with` recursively dispatches to itself for list-valued items and hangs. |
| B082 | Eval | `zig build test-eval -- known-bugs --filter B082 --threads 1 --timeout 5000` | `break` inside a lambda nested in a loop reaches LIR lowering outside a loop. |
| B083 | CLI | `zig build test-bughunt-cli -- --filter B083 --threads 1 --timeout 5000` | Equality on `Box(T)` compares box identity instead of payload equality. |
| B084 | CLI | `zig build test-bughunt-cli -- --filter B084 --threads 1 --timeout 5000` | Passing an effectful lambda through a higher-order call reaches a callable-set MIR invariant. |
| B085 | Eval | `zig build test-eval -- known-bugs --filter B085 --threads 1 --timeout 5000` | Operators on nominal types ignore attached operator methods. |
| B086 | CLI | `zig build test-bughunt-cli -- --filter B086 --threads 1 --timeout 5000` | Top-level user nominal constants have no published nominal backing during runtime planning. |
| B087 | CLI | `zig build test-bughunt-cli -- --filter B087 --threads 1 --timeout 5000` | Warning-only programs exit nonzero and `roc run` prints warnings twice. |
| B088 | Eval | `zig build test-eval -- known-bugs --filter B088 --threads 1 --timeout 5000` | List rest-only patterns are typed as records during lambda solving. |
| B089 | CLI | `zig build test-bughunt-cli -- --filter B089 --threads 1 --timeout 5000` | `Str.from_utf8_lossy` emits one replacement character per invalid byte. |
| B090 | Eval | `zig build test-eval -- known-bugs --filter B090 --threads 1 --timeout 5000` | Record-extension aliases accept non-record extension arguments. |
| B091 | Eval | `zig build test-eval -- known-bugs --filter B091 --threads 1 --timeout 5000` | Polymorphic top-level constants are accepted but have no sealed runtime instance. |
| B092 | Eval | `zig build test-eval -- known-bugs --filter B092 --threads 1 --timeout 5000` | Ambiguous constrained static dispatch reaches mono specialization. |
| B093 | CLI | `zig build test-bughunt-cli -- --filter B093 --threads 1 --timeout 5000` | Duplicate fields in record-builder expressions reach type-key invariants. |
| B094 | CLI | `zig build test-bughunt-cli -- --filter B094 --threads 1 --timeout 5000` | `Str.repeat` does not short-circuit empty strings. |
| B095 | Eval | `zig build test-eval -- known-bugs --filter B095 --threads 1 --timeout 5000` | Record builders accept a `map2` that does not implement the builder contract. |
| B096 | Eval | `zig build test-eval -- known-bugs --filter B096 --threads 1 --timeout 5000` | Record-extension aliases allow duplicate fields introduced by extension arguments. |
| B097 | Eval | `zig build test-eval -- known-bugs --filter B097 --threads 1 --timeout 5000` | Tag-union extension aliases allow duplicate tags introduced by extension arguments. |
| B098 | Eval | `zig build test-eval -- known-bugs --filter B098 --threads 1 --timeout 5000` | Polymorphic record update functions lose preserved row fields during mono. |
| B099 | Eval | `zig build test-eval -- known-bugs --filter B099 --threads 1 --timeout 5000` | `return` inside closures stored in aggregates lowers to a runtime crash. |
| B100 | Eval | `zig build test-eval -- known-bugs --filter B100 --threads 1 --timeout 5000` | Complex `for` loop patterns reach IR lowering without materialization. |
| B101 | CLI | `zig build test-bughunt-cli -- --filter B101 --threads 1 --timeout 5000` | Imported boxed callables reach a lambda-solved payload-transform invariant. |
| B102 | CLI | `zig build test-bughunt-cli -- --filter B102 --threads 1 --timeout 5000` | Imported boxed polymorphic function constants are not sealed per concrete use. |
| B103 | CLI | `zig build test-bughunt-cli -- --filter B103 --threads 1 --timeout 5000` | Recursive functions erased through `Box` get the wrong callable-set emission key. |
| B104 | CLI | `zig build test-bughunt-cli -- --filter B104 --threads 1 --timeout 5000` | Function-valued tag payloads reach a mono nominal-specialization invariant. |
| B105 | CLI | `zig build test-bughunt-cli -- --filter B105 --threads 1 --timeout 5000` | Imported function-valued tag payloads reference a template missing from the imported closure. |
| B106 | CLI | `zig build test-bughunt-cli -- --filter B106 --threads 1 --timeout 5000` | Top-level record constants with function fields have no sealed concrete instance. |
| B107 | CLI | `zig build test-bughunt-cli -- --filter B107 --threads 1 --timeout 5000` | Top-level tag constants with function payloads lose their relation template closure. |
| B108 | CLI | `zig build test-bughunt-cli -- --filter B108 --threads 1 --timeout 5000` | Open-error custom tags leak when returned through the host exit boundary. |
| B109 | CLI | `zig build test-bughunt-cli -- --filter B109 --threads 1 --timeout 5000` | Open-error payload tags are freed with the wrong alignment at the host exit boundary. |
| B110 | CLI | `zig build test-bughunt-cli -- --filter B110 --threads 1 --timeout 5000` | Imported recursive boxed-tree equality overflows the compiler stack. |
| B111 | CLI | `zig build test-bughunt-cli -- --filter B111 --threads 1 --timeout 5000` | Recursive boxed-tree custom equality overflows the compiler stack. |
| B112 | CLI | `zig build test-bughunt-cli -- --filter B112 --threads 1 --timeout 5000` | Top-level boxed function constants have no sealed callable instance. |
| B113 | CLI | `zig build test-bughunt-cli -- --filter B113 --threads 1 --timeout 5000` | Imported top-level boxed function constants reference a template missing from the imported closure. |
| B114 | CLI | `zig build test-bughunt-cli -- --filter B114 --threads 1 --timeout 5000` | Host calls of top-level boxed function constants hit the missing sealed callable instance. |
| B115 | CLI | `zig build test-bughunt-cli -- --filter B115 --threads 1 --timeout 5000` | Host roundtrips of top-level boxed function constants hit the missing sealed callable instance. |
| B116 | CLI | `zig build test-bughunt-cli -- --filter B116 --threads 1 --timeout 5000` | Host storage of top-level boxed function constants hits the missing sealed callable instance. |
| B117 | CLI | `zig build test-bughunt-cli -- --filter B117 --threads 1 --timeout 5000` | Imported attached methods returning functions are reported as missing methods. |
| B118 | CLI | `zig build test-bughunt-cli -- --filter B118 --threads 1 --timeout 5000` | Static-dispatch methods returning functions reach a lambda-solved payload-transform invariant. |
| B119 | CLI | `zig build test-bughunt-cli -- --filter B119 --threads 1 --timeout 5000` | Imported static-dispatch methods returning functions reach a lambda-solved payload-transform invariant. |
| B120 | CLI | `zig build test-bughunt-cli -- --filter B120 --threads 1 --timeout 5000` | Imported attached methods on qualified values are reported as missing methods. |
| B121 | CLI | `zig build test-bughunt-cli -- --filter B121 --threads 1 --timeout 5000` | Imported nominal `plus` operators ignore attached operator methods. |
| B122 | CLI | `zig build test-bughunt-cli -- --filter B122 --threads 1 --timeout 5000` | Boxed imported nominal values compare box identity instead of payload equality. |
| B123 | CLI | `zig build test-bughunt-cli -- --filter B123 --threads 1 --timeout 5000` | Recursive data carrying function payloads reaches a mono nominal-specialization invariant. |
| B124 | CLI | `zig build test-bughunt-cli -- --filter B124 --threads 1 --timeout 5000` | Imported recursive data carrying function payloads references a template missing from the imported closure. |
| B125 | CLI | `zig build test-bughunt-cli -- --filter B125 --threads 1 --timeout 5000` | Record function fields reach a lambda-solved payload-transform invariant when called directly. |
| B126 | CLI | `zig build test-bughunt-cli -- --filter B126 --threads 1 --timeout 5000` | Returned record function fields reach a mono nominal-specialization invariant when called. |
| B127 | CLI | `zig build test-bughunt-cli -- --filter B127 --threads 1 --timeout 5000` | List function payloads reach a lambda-solved payload-transform invariant when selected and called. |
| B128 | CLI | `zig build test-bughunt-cli -- --filter B128 --threads 1 --timeout 5000` | Top-level list constants containing functions have no sealed callable instance. |
| B129 | CLI | `zig build test-bughunt-cli -- --filter B129 --threads 1 --timeout 5000` | Imported list constants containing functions have no sealed callable instance. |
| B130 | CLI | `zig build test-bughunt-cli -- --filter B130 --threads 1 --timeout 5000` | Tuple function payloads reach a lambda-solved payload-transform invariant when called. |
| B131 | CLI | `zig build test-bughunt-cli -- --filter B131 --threads 1 --timeout 5000` | Top-level tuple constants containing functions have no sealed callable instance. |
| B132 | CLI | `zig build test-bughunt-cli -- --filter B132 --threads 1 --timeout 5000` | Imported tuple constants containing functions have no sealed callable instance. |
| B133 | CLI | `zig build test-bughunt-cli -- --filter B133 --threads 1 --timeout 5000` | `Try` function payloads reach a lambda-solved payload-transform invariant when selected and called. |
| B134 | CLI | `zig build test-bughunt-cli -- --filter B134 --threads 1 --timeout 5000` | Imported `Try` function payloads have no sealed callable instance. |
| B135 | CLI | `zig build test-bughunt-cli -- --filter B135 --threads 1 --timeout 5000` | Record updates preserving function fields reach a lambda-solved payload-transform invariant. |
| B136 | CLI | `zig build test-bughunt-cli -- --filter B136 --threads 1 --timeout 5000` | While-loop function-variable updates reach a mono nominal-specialization invariant. |
| B137 | CLI | `zig build test-bughunt-cli -- --filter B137 --threads 1 --timeout 5000` | For-loop destructuring of function fields reaches a mono nominal-specialization invariant. |
| B138 | CLI | `zig build test-bughunt-cli -- --filter B138 --threads 1 --timeout 5000` | Host calls of imported boxed functions stored in tag payloads lose `Box` provenance. |
| B139 | CLI | `zig build test-bughunt-cli -- --filter B139 --threads 1 --timeout 5000` | Function-variable reassignment emits a duplicate-definition warning and exits nonzero. |
| B140 | CLI | `zig build test-bughunt-cli -- --filter B140 --threads 1 --timeout 5000` | Nested record function fields reach a lambda-solved payload-transform invariant. |
| B141 | CLI | `zig build test-bughunt-cli -- --filter B141 --threads 1 --timeout 5000` | Tag payload records containing callable fields reach a lambda-solved payload-transform invariant. |
| B142 | CLI | `zig build test-bughunt-cli -- --filter B142 --threads 1 --timeout 5000` | Boxing record function fields reaches a mono nominal-specialization invariant. |
| B143 | CLI | `zig build test-bughunt-cli -- --filter B143 --threads 1 --timeout 5000` | Duplicate attached method definitions reach typed CIR duplicate-definition invariants. |
| B144 | CLI | `zig build test-bughunt-cli -- --filter B144 --threads 1 --timeout 5000` | `return` inside higher-order closures lowers to a runtime crash. |
| B145 | CLI | `zig build test-bughunt-cli -- --filter B145 --threads 1 --timeout 5000` | `return` inside tag-stored closures lowers to a runtime crash. |
| B146 | CLI | `zig build test-bughunt-cli -- --filter B146 --threads 1 --timeout 5000` | `return` inside boxed closures lowers to a runtime crash. |
| B147 | CLI | `zig build test-bughunt-cli -- --filter B147 --threads 1 --timeout 5000` | `break` inside record-stored closures lowers to a runtime crash. |
| B148 | CLI | `zig build test-bughunt-cli -- --filter B148 --threads 1 --timeout 5000` | `break` inside higher-order closures reaches LIR lowering outside a loop. |
| B149 | CLI | `zig build test-bughunt-cli -- --filter B149 --threads 1 --timeout 5000` | Record destructuring of callable fields reaches a mono nominal-specialization invariant. |
| B150 | CLI | `zig build test-bughunt-cli -- --filter B150 --threads 1 --timeout 5000` | Renamed record destructuring of callable fields reaches a mono nominal-specialization invariant. |
| B151 | CLI | `zig build test-bughunt-cli -- --filter B151 --threads 1 --timeout 5000` | Tuple destructuring of callable elements reaches a mono nominal-specialization invariant. |
| B152 | CLI | `zig build test-bughunt-cli -- --filter B152 --threads 1 --timeout 5000` | Imported record callable fields have no sealed concrete instance when destructured. |
| B153 | CLI | `zig build test-bughunt-cli -- --filter B153 --threads 1 --timeout 5000` | Imported callable tag payloads reference a template missing from the imported closure. |
| B154 | CLI | `zig build test-bughunt-cli -- --filter B154 --threads 1 --timeout 5000` | Transitive nominal equality loses the original owner environment. |
| B155 | CLI | `zig build test-bughunt-cli -- --filter B155 --threads 1 --timeout 5000` | Transitive nominal operators lose the original owner environment. |
| B156 | CLI | `zig build test-bughunt-cli -- --filter B156 --threads 1 --timeout 5000` | Transitive callable tag payloads reference a template missing from the imported closure. |
| B157 | CLI | `zig build test-bughunt-cli -- --filter B157 --threads 1 --timeout 5000` | Transitive boxed callable constants reference a template missing from the imported closure. |
| B158 | CLI | `zig build test-bughunt-cli -- --filter B158 --threads 1 --timeout 5000` | Transitive static-dispatch methods returning functions lose the original owner environment. |
| B159 | CLI | `zig build test-bughunt-cli -- --filter B159 --threads 1 --timeout 5000` | Top-level boxed polymorphic identity functions are not sealed per concrete use. |
| B160 | CLI | `zig build test-bughunt-cli -- --filter B160 --threads 1 --timeout 5000` | Top-level boxed generic record builders are not sealed per concrete use. |
| B161 | CLI | `zig build test-bughunt-cli -- --filter B161 --threads 1 --timeout 5000` | Boxed top-level functions returning polymorphic functions are not sealed. |
| B162 | CLI | `zig build test-bughunt-cli -- --filter B162 --threads 1 --timeout 5000` | Imported boxed generic record builders are not sealed per concrete use. |
| B163 | CLI | `zig build test-bughunt-cli -- --filter B163 --threads 1 --timeout 5000` | Top-level records storing boxed generic functions have no sealed concrete instance. |
| B164 | CLI | `zig build test-bughunt-cli -- --filter B164 --threads 1 --timeout 5000` | Imported polymorphic recursive boxed-tree equality overflows the compiler stack. |
| B165 | CLI | `zig build test-bughunt-cli -- --filter B165 --threads 1 --timeout 5000` | `Str.inspect` on empty-error list payloads reaches an uninhabited-union invariant. |
| B166 | CLI | `zig build test-bughunt-cli -- --filter B166 --threads 1 --timeout 5000` | `Str.inspect` on empty-error record payloads reaches an uninhabited-union invariant. |
| B167 | CLI | `zig build test-bughunt-cli -- --filter B167 --threads 1 --timeout 5000` | `Str.inspect` on imported empty-error list payloads reaches an uninhabited-union invariant. |
| B168 | CLI | `zig build test-bughunt-cli -- --filter B168 --threads 1 --timeout 5000` | `Str.inspect` on imported empty-error record payloads reaches an uninhabited-union invariant. |
| B169 | CLI | `zig build test-bughunt-cli -- --filter B169 --threads 1 --timeout 5000` | `Str.inspect` on record fields containing uninhabited error unions reaches an invariant. |
| B170 | CLI | `zig build test-bughunt-cli -- --filter B170 --threads 1 --timeout 5000` | `Str.inspect` on lists containing uninhabited error unions reaches an invariant. |
| B171 | CLI | `zig build test-bughunt-cli -- --filter B171 --threads 1 --timeout 5000` | `Str.inspect` on boxed uninhabited error unions reaches an invariant. |
| B172 | CLI | `zig build test-bughunt-cli -- --filter B172 --threads 1 --timeout 5000` | Open-error record payloads are freed with the wrong alignment at the host exit boundary. |
| B173 | CLI | `zig build test-bughunt-cli -- --filter B173 --threads 1 --timeout 5000` | Open-error list payloads are freed with the wrong alignment at the host exit boundary. |
| B174 | CLI | `zig build test-bughunt-cli -- --filter B174 --threads 1 --timeout 5000` | Open-error boxed payloads are freed with the wrong alignment at the host exit boundary. |
| B175 | CLI | `zig build test-bughunt-cli -- --filter B175 --threads 1 --timeout 5000` | Open-error recursive payloads are freed with the wrong alignment at the host exit boundary. |
| B176 | CLI | `zig build test-bughunt-cli -- --filter B176 --threads 1 --timeout 5000` | Open-error multi-branch custom payloads are freed with the wrong alignment at the host exit boundary. |
| B177 | CLI | `zig build test-bughunt-cli -- --filter B177 --threads 1 --timeout 5000` | `roc test` crashes when an `expect` executes a hosted effect returning `Str`. |
| B178 | CLI | `zig build test-bughunt-cli -- --filter B178 --threads 1 --timeout 5000` | `roc test` reports qualified `Bool` tags in patterns as undeclared types. |
| B179 | CLI | `zig build test-bughunt-cli -- --filter B179 --threads 1 --timeout 5000` | `roc test` treats block expects returning qualified `Bool.True` as failed tests. |
| B180 | CLI | `zig build test-bughunt-cli -- --filter B180 --threads 1 --timeout 5000` | Record-extension aliases accept non-record extension arguments. |
| B181 | CLI | `zig build test-bughunt-cli -- --filter B181 --threads 1 --timeout 5000` | Tag-union extension aliases accept non-union extension arguments. |
| B182 | CLI | `zig build test-bughunt-cli -- --filter B182 --threads 1 --timeout 5000` | Unboxed recursive nominal definitions are accepted. |
| B183 | CLI | `zig build test-bughunt-cli -- --filter B183 --threads 1 --timeout 5000` | Or-pattern alternatives can omit names used by other alternatives. |
| B184 | CLI | `zig build test-bughunt-cli -- --filter B184 --threads 1 --timeout 5000` | Record-extension aliases allow duplicate fields introduced by extension arguments. |
| B185 | CLI | `zig build test-bughunt-cli -- --filter B185 --threads 1 --timeout 5000` | Tag-union extension aliases allow duplicate tags introduced by extension arguments. |
| B186 | CLI | `zig build test-bughunt-cli -- --filter B186 --threads 1 --timeout 5000` | Boxed generic functions inside tag payloads lose `Box` provenance. |
| B187 | CLI | `zig build test-bughunt-cli -- --filter B187 --threads 1 --timeout 5000` | Imported boxed callable tag payloads lose `Box` provenance when unboxed. |
| B188 | CLI | `zig build test-bughunt-cli -- --filter B188 --threads 1 --timeout 5000` | Qualified imported record field access is misresolved as an imported method. |
| B189 | CLI | `zig build test-bughunt-cli -- --filter B189 --threads 1 --timeout 5000` | Qualified imported nested record field access is misresolved as an imported method. |
| B190 | CLI | `zig build test-bughunt-cli -- --filter B190 --threads 1 --timeout 5000` | Qualified imported record access to boxed callable fields is misresolved as an imported method. |
| B191 | CLI | `zig build test-bughunt-cli -- --filter B191 --threads 1 --timeout 5000` | Nominal minus operators ignore attached `minus` methods. |
| B192 | CLI | `zig build test-bughunt-cli -- --filter B192 --threads 1 --timeout 5000` | Imported nominal minus operators ignore attached `minus` methods. |
| B193 | CLI | `zig build test-bughunt-cli -- --filter B193 --threads 1 --timeout 5000` | Nominal times operators ignore attached `times` methods. |
| B194 | CLI | `zig build test-bughunt-cli -- --filter B194 --threads 1 --timeout 5000` | Imported nominal times operators ignore attached `times` methods. |
| B195 | CLI | `zig build test-bughunt-cli -- --filter B195 --threads 1 --timeout 5000` | Nominal slash operators ignore attached `div_by` methods. |
| B196 | CLI | `zig build test-bughunt-cli -- --filter B196 --threads 1 --timeout 5000` | Imported nominal slash operators ignore attached `div_by` methods. |
| B197 | CLI | `zig build test-bughunt-cli -- --filter B197 --threads 1 --timeout 5000` | Nominal integer-division operators ignore attached `div_trunc_by` methods. |
| B198 | CLI | `zig build test-bughunt-cli -- --filter B198 --threads 1 --timeout 5000` | Imported nominal integer-division operators ignore attached `div_trunc_by` methods. |
| B199 | CLI | `zig build test-bughunt-cli -- --filter B199 --threads 1 --timeout 5000` | Nominal remainder operators ignore attached `rem_by` methods. |
| B200 | CLI | `zig build test-bughunt-cli -- --filter B200 --threads 1 --timeout 5000` | Imported nominal remainder operators ignore attached `rem_by` methods. |
| B201 | CLI | `zig build test-bughunt-cli -- --filter B201 --threads 1 --timeout 5000` | Nominal less-than operators ignore attached `is_lt` methods. |
| B202 | CLI | `zig build test-bughunt-cli -- --filter B202 --threads 1 --timeout 5000` | Imported nominal less-than operators ignore attached `is_lt` methods. |
| B203 | CLI | `zig build test-bughunt-cli -- --filter B203 --threads 1 --timeout 5000` | Nominal less-than-or-equal operators ignore attached `is_lte` methods. |
| B204 | CLI | `zig build test-bughunt-cli -- --filter B204 --threads 1 --timeout 5000` | Imported nominal less-than-or-equal operators ignore attached `is_lte` methods. |
| B205 | CLI | `zig build test-bughunt-cli -- --filter B205 --threads 1 --timeout 5000` | Nominal greater-than operators ignore attached `is_gt` methods. |
| B206 | CLI | `zig build test-bughunt-cli -- --filter B206 --threads 1 --timeout 5000` | Imported nominal greater-than operators ignore attached `is_gt` methods. |
| B207 | CLI | `zig build test-bughunt-cli -- --filter B207 --threads 1 --timeout 5000` | Nominal greater-than-or-equal operators ignore attached `is_gte` methods. |
| B208 | CLI | `zig build test-bughunt-cli -- --filter B208 --threads 1 --timeout 5000` | Imported nominal greater-than-or-equal operators ignore attached `is_gte` methods. |
| B209 | CLI | `zig build test-bughunt-cli -- --filter B209 --threads 1 --timeout 5000` | Nominal unary negation ignores attached `negate` methods. |
| B210 | CLI | `zig build test-bughunt-cli -- --filter B210 --threads 1 --timeout 5000` | Imported nominal unary negation ignores attached `negate` methods. |
| B211 | CLI | `zig build test-bughunt-cli -- --filter B211 --threads 1 --timeout 5000` | Transitive imported attached methods on qualified values are reported as missing methods. |
