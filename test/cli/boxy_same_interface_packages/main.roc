## Two packages whose `Shared` modules have one interface and different
## bodies: distinct modules, whose workers and types must stay distinct.
app [main!] {
	left: "./left/main.roc",
	right: "./right/main.roc",
	pf: platform "../../fx/platform/main.roc",
}

import pf.Stdin
import pf.Stdout
import left.Shared as Left
import right.Shared as Right

main! = || {
	n = Stdin.line!().count_utf8_bytes()
	Stdout.line!(Left.tell(Left.make(n)))
	Stdout.line!(Right.tell(Right.make(n)))
}
