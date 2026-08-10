app [main!] {
	pf: platform "../../fx/platform/main.roc",
	keys: "./pkg/main.roc",
}

import keys.Generic
import keys.Key
import pf.Stdout

main! = ||
	Stdout.line!(Str.inspect(Generic.contains([Key.new("en-US")], Key.new("EN-us"))))
