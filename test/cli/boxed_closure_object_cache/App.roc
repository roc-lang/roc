# Builds elements whose handlers are the app's own closures and dispatches
# events through Elem. A warm build serves Elem's procedures from the packs
# the cold build wrote, whichever closures the app boxes.
app [main!] { pf: platform "../../fx-open/platform/main.roc" }

import pf.Stdout
import Elem

main! = |args| {
	suffix = if List.len(args) > 1 { "!" } else { "." }
	button =
		Elem.text("button")
			.on(Box.box(|event| Str.concat("clicked by ", event)))
			.on(Box.box(|event| Str.concat(event, suffix)))
	link = Elem.text("link").on(Box.box(|event| Str.concat("followed ", event)))
	Stdout.line!(button.dispatch("mouse"))
	Stdout.line!(link.dispatch("key"))
	Ok({})
}
