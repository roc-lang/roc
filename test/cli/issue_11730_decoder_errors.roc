app [main!] { pf: platform "../fx-open/platform/main.roc" }

import pf.Stdout

decode = |body| {
	decoded : Try({ a : Str }, _)
	decoded = Json.parse(body)
	decoded.map_ok(|r| r.a)
}

send = |decoder, body| decoder(body)
fetch = |body| send(decode, body)

main! = |_| {
	for body in ["{\"a\":\"ok\"}", "{}", "invalid"] {
		match fetch(body) {
			Ok(value) => Stdout.line!(value)
			Err(MissingRequiredField(field)) => Stdout.line!("missing: ${field}")
			Err(InvalidJson(_)) => Stdout.line!("invalid")
		}
	}
	Ok({})
}
