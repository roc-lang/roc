app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdout
import pf.Stdin

# Compile-time evaluation freezes these closures, captures included, into the
# program's static data, and the program calls them with input it reads at
# runtime. A dev run reserves its hot-reload header before every closure's
# captures, so the frozen ones must reserve it too.
make : Str -> Box(Str -> Str)
make = |prefix| Box.box(|s| Str.concat(prefix, s))

boxed : Box(Str -> Str)
boxed = make("pre-")

# This closure is generic in its argument's item type, and freezes at the
# `List(Str)` it is read at.
counter : Str -> Try((List(item) -> Str), [Never])
counter = |label| Ok(|values| Str.concat(label, Str.inspect(List.len(values))))

counted : Try((List(Str) -> Str), [Never])
counted = counter("count=")

main! = || {
	line = Stdin.line!()
	Stdout.line!(Box.unbox(boxed)(line))
	Stdout.line!(counted.map_ok(|count| count([line, line])).ok_or("unreachable"))
}
