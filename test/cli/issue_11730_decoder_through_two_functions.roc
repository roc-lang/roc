app [main!] { pf: platform "../fx-open/platform/main.roc" }

import pf.Stdout

# repro for https://github.com/roc-lang/roc/issues/11730
# A `Json.parse` decoder passed through two function layers (`fetch` -> `send`
# -> `decode`). `roc check` must check this cleanly; instead it panics with
# "instantiation widened a closed tag union".
decode = |body| {
	decoded : Try({ a : Str }, _)
	decoded = Json.parse(body)

	decoded.map_ok(|r| r.a)
}

send = |decoder, body| decoder(body)

fetch = |body| send(decode, body)

main! = |args| {
	match fetch(Str.join_with(args, "")) {
		Ok(a) => Stdout.line!(a)
		Err(_) => Stdout.line!("err")
	}
	Ok({})
}
