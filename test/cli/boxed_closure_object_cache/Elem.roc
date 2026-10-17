# Reduced from roc-signals' Html module: an element carries its event
# handlers as boxed closures, so this module's exports take and return
# values that hold functions only behind a Box.
Elem :: {
	label : Str,
	handlers : List(Box(Str -> Str)),
}.{
	text : Str -> Elem
	text = |label| Elem.{ label, handlers: [] }

	on : Elem, Box(Str -> Str) -> Elem
	on = |elem, handler| Elem.{ label: elem.label, handlers: elem.handlers.append(handler) }

	dispatch : Elem, Str -> Str
	dispatch = |elem, event| {
		var $out = elem.label
		for handler in elem.handlers {
			reply = Box.unbox(handler)(event)
			$out = $out.concat(" ").concat(reply)
		}
		$out
	}
}
