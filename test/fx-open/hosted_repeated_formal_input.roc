app [main!] { pf: platform "./platform/fallible_echo_main.roc" }

# FallibleEcho.echo!'s argument row is H's declared formal, which only its
# result's error row widens past: passing a tag the declared row does not list
# is a type mismatch (hosted_repeated_formal.roc is the accepted side). The
# hosted function's type keeps its `H` layer, as the second mismatch shows.

import pf.FallibleEcho
import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args| {
	wide : Try(Str, [Aborted, NotFound, PermissionDenied])
	wide = FallibleEcho.echo!(Aborted)
	match wide {
		Ok(_) => Stdout.line!("ok")
		Err(_) => Stdout.line!("err")
	}
	not_a_str : Str
	not_a_str = FallibleEcho.echo!
	Stdout.line!(not_a_str)
	Ok({})
}
