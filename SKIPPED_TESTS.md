Here are all the skipped/commented-out tests:

src/eval/test/eval_test.zig (1 test):
1. "recursive function with record - stack memory restoration (issue #8813)" — LIR interpreter max_call_depth (512) too low for 1000 recursive calls

src/eval/test/comptime_eval_test.zig (1 test):
2. "issue 8754: pattern matching on recursive tag union variant payload" — SIGSEGV in comptime evaluator

Root causes summary:
- 1 test: Call depth limit too low (#1)
- 1 test: SIGSEGV in comptime evaluator on recursive types (#2)
