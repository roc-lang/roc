# Regression fixture for https://github.com/roc-lang/roc/issues/9519 — the
# split-file arrangement of the same repro as issue_9519_two_lifted_ids.roc.
import Combine

main! = |_args| Ok({})

expect Combine.combine(Combine.Foo.Baz, Combine.Foo.Baz, |a, b| a + b) == Combine.Foo.Baz
expect Combine.combine(Combine.Foo.Bar(1), Combine.Foo.Bar(2), |a, b| a + b) == Combine.Foo.Bar(3)
