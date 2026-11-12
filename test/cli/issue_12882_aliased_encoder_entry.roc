app [main!] { pf: platform "../fx-open/platform/main.roc" }

main! = entry!

entry! : List(Str) => Try({}, _)
entry! = |_args| {
	width = "0.9"
	_ = Json.to_str(width)
	Err(WidthNotRefused(width))
}
