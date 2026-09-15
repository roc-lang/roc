# Reaches closed module functions whose results are constants, so a rebuild
# served from the object cache links the constants the cached entries carry.
# The index comes from stdin so the calls happen at runtime.
app [main!] { pf: platform "../../fx/platform/main.roc" }

import Tables
import pf.Stdin
import pf.Stdout

main! = || {
	index = Str.count_utf8_bytes(Stdin.line!())
	total = List.sum(Tables.squares({})) + Tables.weight_of(index) + Tables.weight_of(index + 1)
	Stdout.line!("${Tables.banner({})} ${U64.to_str(total)} ${U64.to_str(List.len(Tables.entries({})))}")
}
