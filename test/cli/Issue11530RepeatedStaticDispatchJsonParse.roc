app [main!] {}

Page := [P].{
	parse : Page, Str -> Try(U64, [BadJson])
	parse = |_page, text| Json.parse(text).map_err(|_| BadJson)
}

helper = |page, text| (page.parse(text), page.parse(text))

## Repro for https://github.com/roc-lang/roc/issues/11530
main! = |_args| {
	echo!(Str.inspect(helper(Page.P, "0")))
	Ok({})
}
