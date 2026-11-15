# A generic function exported from another module derives its record and tuple
# codecs from its own type variables' requirements; each importing call site
# supplies the evidence for its own types.
import Issue13007Codecs

main! = |args| {
	count = args.len()
	text = args.first() ?? "s"
	echo!("${Issue13007Codecs.in_record(count)}\n")
	echo!("${Issue13007Codecs.in_record({ name: text })}\n")
	echo!("${Issue13007Codecs.in_pair(text, count)}\n")
	echo!("${Issue13007Codecs.in_pair(count, [text])}\n")
	parsed : Try(Str, [InvalidJson(Str)])
	parsed = Issue13007Codecs.value_of("{\"value\":\"${text}\"}")
	match parsed {
		Ok(value) => echo!("parsed ${value}\n")
		Err(_) => echo!("parse failed\n")
	}
	Ok({})
}
