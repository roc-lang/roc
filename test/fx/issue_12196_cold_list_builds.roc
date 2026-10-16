app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

# repro for https://github.com/roc-lang/roc/issues/12196
# A list literal and `List.with_capacity` built on the cold (out-of-bounds)
# arm of `List.get` in two different functions must compile under
# `--opt=speed` and print the same result as `--opt=dev`.

literal : List(U64), U64 -> List(U64)
literal = |list, i|
	match list.get(i) {
		Ok(x) => [x]
		Err(_) => [i, i]
	}

capacity : List(U64), U64 -> List(U64)
capacity = |list, i|
	match list.get(i) {
		Ok(x) => [x]
		Err(_) => List.with_capacity(i)
	}

main! = || {
	n = Stdin.line!().to_utf8().len()
	a = literal([1, 2, 3], n)
	b = capacity([1, 2, 3], n)
	Stdout.line!("${a.len().to_str()} ${b.len().to_str()}")
}
