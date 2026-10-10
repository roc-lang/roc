# The nested record's string descriptors have more than one exact parent in
# the erased encoder's descriptor graph. Keep the value runtime-dependent.
main! = |args| {
	value = args.first() ?? "hello"
	echo!("${Json.to_str({ data: [{ encoding: "text", mime: "text/html", value }], diagnostics: "", result: value, stdout: "" })}\n")
	Ok({})
}
