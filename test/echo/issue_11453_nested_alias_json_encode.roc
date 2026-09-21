# repro for https://github.com/roc-lang/roc/issues/11453: a method that encodes a record reached through three record aliases
app [main!] {}

Card : { url : Str }

Area : { office : Card }

Model : { area : Area }

Probe := [].{
	enc : Model -> Str
	enc = |payload| Json.to_str(payload)
}

main! = |_args| {
	echo!(Probe.enc({ area: { office: { url: "u" } } }))
	Ok({})
}
