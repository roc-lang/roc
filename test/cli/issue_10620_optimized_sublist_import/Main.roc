# Repro for https://github.com/roc-lang/roc/issues/10620: an optimized
# multi-module test must still find [2, 3] inside the longer list.
import Sublist exposing [sublist]

main! = |_args| Ok({})

expect {
	result = [0, 1, 2, 3, 4, 5] |> sublist([2, 3])
	result == Superlist
}
