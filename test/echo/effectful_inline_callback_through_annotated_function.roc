fetch! : Str, (Str => Try(Str, _)) => Try(Str, _)
fetch! = |uri, client| client(uri)

main! : List(Str) => Try({}, _)
main! = |args| {
	echo!("${fetch!("first", |uri| if uri == "" Err(Bad) else Ok(uri)) ?? "fallback"}\n")
	echo!("${fetch!("", |uri| if uri == "" Err(Bad) else Ok(uri)) ?? "fallback"}\n")
	if List.len(args) > 1000 Ok({}) else Err(Fail("ran"))
}
