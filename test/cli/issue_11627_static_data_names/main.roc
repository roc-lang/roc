app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Lib

# The two recursions keep both constants behind procedures, so the edited
# app's constant and the cached Lib.wrap's constant are both frozen as static
# data in one program.
local : List(U8)
local = [1, 2]

count_down : U64 -> List(U8)
count_down = |n|
	if n == 0 {
		local
	} else {
		count_down(n - 1)
	}

main! = |args| {
	Stdout.line!(Str.inspect(count_down(args.len())))
	Stdout.line!(Str.inspect(Lib.wrap(args.len())))
	Ok({})
}
