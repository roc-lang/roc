# repro for https://github.com/roc-lang/roc/issues/10956
# An open record whose extension variable also carries a where-clause method
# requirement must reach the end of `roc check` without crashing the compiler.
blub : { ..a } -> {} where [a.const : a -> U8]
blub = |_| {}

main! = |_| Ok({})
