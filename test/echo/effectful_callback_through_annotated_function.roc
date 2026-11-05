fetch! : Str, (Str => Try(Str, _)) => Try(Str, _)
fetch! = |uri, client| client(uri)

main! : List(Str) => Try({}, _)
main! = |args| {
	client = |uri| if uri == "" Err(Bad) else Ok(uri)
	echo!("${fetch!("first", client) ?? "fallback"}\n")
	echo!("${fetch!("", client) ?? "fallback"}\n")
	if List.len(args) > 1000 Ok({}) else Err(Fail("ran"))
}
