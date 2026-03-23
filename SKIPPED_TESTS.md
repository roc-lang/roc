Here are all the skipped/commented-out tests:

src/eval/test/eval_test.zig (2 tests):
1. "recursive function with record - stack memory restoration (issue #8813)" — LIR interpreter max_call_depth (512) too low for 1000 recursive calls
2. "decode: I32.decode with record field format mismatches and crashes" — monomorphize panics on to_i64 dispatch

src/eval/test/anno_only_interp_test.zig (5 tests):
3. "e_anno_only - function crashes when called directly" — monomorphize panics on annotation-only functions
4. "e_anno_only - non-function crashes when accessed" — same
5. "e_anno_only - function only crashes when called (True branch)" — same
6. "e_anno_only - function only crashes when called (False branch)" — same
7. "e_anno_only - value only crashes when accessed (True branch)" — same
8. "e_anno_only - value only crashes when accessed (False branch)" — same

src/eval/test/comptime_eval_test.zig (3 tests):
9. "issue 8754: pattern matching on recursive tag union variant payload" — SIGSEGV in comptime evaluator
10. "issue 8979: while (True) with body but no exit should crash" — monomorphize panics on while(True) with non-trivial body
11. "comptime eval - closure with single capture" — monomorphize panics on closure capture lowering

Root causes summary:
- 1 test: Call depth limit too low (#1)
- 8 tests: Monomorphize std.debug.panic (uncatchable signal 6) on invalid/edge-case code (#2-5, #10-11)
- 1 test: SIGSEGV in comptime evaluator on recursive types (#9)
- 1 test: Monomorphize panic on annotation-only defs — all 5 anno_only tests share this (#3-8, counted as one root cause)
