app [main!] {
	pf: platform "../../fx/platform/main.roc",
	thing: "./pkg/main.roc",
}

import thing.Thing
import pf.Stdout

main! = || Stdout.line!(Str.inspect(Thing.new(4)))
