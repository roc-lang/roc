app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout

# The callback never reads its second argument, whose type holds a type
# variable, so its runtime descriptor is rebuilt from the argument's parts
# before the unused argument is released.
count_items : Iter((item, Str)) -> U64
count_items = |rest| rest.fold(0.U64, |n, _| n + 1)

main! = || {
	Stdout.line!(count_items([(1.I64, "a"), (2.I64, "b"), (3.I64, "c")].iter()).to_str())
	Stdout.line!(count_items([("x", "a")].iter()).to_str())
}
