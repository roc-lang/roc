# Every arm initializes the result descriptor, including zero-sized payloads.
render : [Other, Tag({ name : Str, payload : Try(Str, {}) })] -> Str
render = |value| {
	rich = match value {
		Other => Ok([])
		Tag({ name, payload }) => {
			format = match name {
				"html" => Ok(("text/html", "text"))
				_ => Err({})
			}
			match format {
				Ok((mime, encoding)) => match payload {
					Ok(text) => Ok([{ mime, encoding, value: text }])
					Err({}) => Err("bad payload")
				}
				Err({}) => Ok([])
			}
		}
	}
	reply = match rich {
		Ok(data) => { data, diagnostics: "" }
		Err(diagnostics) => { data: [], diagnostics }
	}
	Str.inspect(reply)
}

render_if = |name| {
	format = if name == "html" Yes(("text/html", "text")) else No
	match format {
		Yes((mime, encoding)) => "${mime}:${encoding}"
		No => "plain"
	}
}

main! = |args| {
	name = args.first() ?? "other"
	other = if name == "other" "html" else "other"
	echo!("${render(Tag({ name, payload: Ok("test") }))}\n${render(Tag({ name: other, payload: Ok("test") }))}\n${render_if(name)}\n${render_if(other)}\n")
	Ok({})
}
