# Reaches a closed module function whose specialization converts a custom
# literal, and one that does not, with a value only known at runtime. A
# rebuild served from the object cache takes the second from its packs and
# converts the literal at compile time again.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import Queries
import pf.Stdout

main! = || {
	count = List.len(Queries.make(3).to_utf8())
	Stdout.line!(Queries.make(count))
	Stdout.line!(U64.to_str(Queries.plain(count)))
}
