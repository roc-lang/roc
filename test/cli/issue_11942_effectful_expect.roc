# Repro for https://github.com/roc-lang/roc/issues/11942: an `expect` whose
# condition performs an effect is reported by the checker and counted as a
# compiler error, never lowered and run.
expect echo!("running test") == {}

main! = |_args| Ok({})
