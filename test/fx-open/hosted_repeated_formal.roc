app [main!] { pf: platform "./platform/fallible_echo_main.roc" }

# FallibleEcho.echo! : H([NotFound, PermissionDenied]) with H(e) : e => Try(Str, e)
# (platform/FallibleEcho.roc). Its hosted `Try` error row is coerced, so a use
# widens it by adding `Aborted`, which sorts first and shifts every declared
# discriminant: a missing or misordered re-tag reads a different tag than the
# host returned. The argument row stays as declared.

import pf.FallibleEcho
import pf.Stdout

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args| {
	wide : Try(Str, [Aborted, NotFound, PermissionDenied])
	wide = FallibleEcho.echo!(NotFound)
	Stdout.line!("echo wide: ${show_wide(wide)}")

	declared : Try(Str, [NotFound, PermissionDenied])
	declared = FallibleEcho.echo!(PermissionDenied)
	Stdout.line!("echo declared: ${show_declared(declared)}")

	Ok({})
}

show_wide : Try(Str, [Aborted, NotFound, PermissionDenied]) -> Str
show_wide = |result|
	match result {
		Ok(value) => "misread as Ok(${value})"
		Err(Aborted) => "misread as Err(Aborted)"
		Err(NotFound) => "NotFound"
		Err(PermissionDenied) => "misread as Err(PermissionDenied)"
	}

show_declared : Try(Str, [NotFound, PermissionDenied]) -> Str
show_declared = |result|
	match result {
		Ok(value) => "misread as Ok(${value})"
		Err(NotFound) => "misread as Err(NotFound)"
		Err(PermissionDenied) => "PermissionDenied"
	}
