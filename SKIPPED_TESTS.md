Here are all the skipped/commented-out tests:

src/eval/test/eval_test.zig (1 test):
1. "recursive function with record - stack memory restoration (issue #8813)" — LIR interpreter max_call_depth (512) too low for 1000 recursive calls

Root causes summary:
- 1 test: Call depth limit too low (#1)
