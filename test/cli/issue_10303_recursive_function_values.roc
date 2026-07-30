# Repro for https://github.com/roc-lang/roc/issues/10303.
# Mutually recursive constants whose back-edge is delayed by a function must check.
Runner :: { run : {} -> {} }

first : Runner
first = { run: |_| (second.run)({}) }

second : Runner
second = { run: |_| (first.run)({}) }

main! = |_args| Ok({})
