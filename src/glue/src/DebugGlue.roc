## A glue script intended for debugging, rather than generating files.
## Logs the reflected types passed to the glue script and generates nothing.
app [make_glue] { pf: platform "../platform/main.roc" }

import pf.Types exposing [Types]
import pf.File exposing [File]

make_glue : List(Types) -> Try(List(File), Str)
make_glue = |types_list| {
	dbg types_list

	Ok([])
}
