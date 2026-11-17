app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost

# Regression test for issue 13028: under `--opt=size`, a twelve-byte record
# copied out of a list element is one `memory.copy`, which encodes smaller
# than the scalar loads and stores of the same bytes.
#
# `Triple` is twelve bytes. `pick` copies one out of a list whose elements
# all depend on the host's input, so the copy reads memory the program
# computes at run time.
Triple : { a : U32, b : U32, c : U32 }

pick : List(Triple), U64 -> Triple
pick = |xs, i| match xs.get(i) {
	Ok(t) => t
	Err(_) => { a: 0, b: 0, c: 0 }
}

main! : () => Str
main! = || {
	n = FallibleHost.json_input!({}).count_utf8_bytes().to_u32_wrap()
	xs = [{ a: n, b: n + 1, c: n + 2 }, { a: n + 3, b: n + 4, c: n + 5 }, { a: n + 6, b: n + 7, c: n + 8 }]
	t = pick(xs, (n % 3).to_u64())
	(t.a + t.b + t.c).to_str()
}
