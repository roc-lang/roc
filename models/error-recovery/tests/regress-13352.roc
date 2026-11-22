# expect: pass errors
# Fixed by https://github.com/roc-lang/roc/pull/13352: a top-level constant that reads a tuple item next to an
# unbound name must report the unbound name, not panic in Boxy lowering.
x = (1, nope).0

main! = |_args| Ok({})
