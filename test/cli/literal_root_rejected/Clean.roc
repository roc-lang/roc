# Imports Query without specializing its rejecting literal.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import Query
import pf.Stdout

main! = || {
	Stdout.line!(U64.to_str(Query.plain(1)))
}
