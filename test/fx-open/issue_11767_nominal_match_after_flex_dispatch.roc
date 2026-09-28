app [main!] { pf: platform "./platform/main.roc" }

# Regression for https://github.com/roc-lang/roc/issues/11767
import pf.Stdout

Maybe(a) := [Nothing, Just(a)]

Source :: {}.{
	fetch : Source, (Str -> Try(a, e)) -> Try(List(a), e)
	fetch = |_source, decode| decode("x").map_ok(|item| [item])
}

wrap : a -> Maybe(a)
wrap = |value| Just(value)

# `items` is only known through `fetch`'s dispatch, so `first` dispatches on
# an unannotated receiver whose result the match lifts into `Maybe`.
run = |source| {
	items = source.fetch(|item| Ok(wrap(item)))?
	match items.first() {
		Ok(Just(name)) => Ok(name)
		_ => Ok("none")
	}
}

main! = |_args| {
	Stdout.line!(run(Source.{}) ?? "error")
	Ok({})
}
