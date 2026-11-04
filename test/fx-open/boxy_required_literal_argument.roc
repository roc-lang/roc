app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# The platform calls `main!` at the type its requirement names, which
# leaves the numeral in `Budget` to the call: `main!`'s scheme carries a
# literal requirement, and the platform's call, checked before the two were
# paired, records no edge for it. The call derives the literal's argument
# from its instantiation, under both strategies.
main! : List(Str) => Try({}, _)
main! = |args| {
	if List.len(args) > 100 {
		return Err(Budget(1))
	}
	Stdout.line!("ok")
	Ok({})
}
