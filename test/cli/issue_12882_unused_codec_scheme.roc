app [main!] { pf: platform "../fx-open/platform/main.roc" }

main! : List(Str) => Try({}, [Exit(I32)])
main! = |_args| Ok({})

unused = |_| {
	width = "0.9"
	_ = Json.to_str(width)
	width
}
