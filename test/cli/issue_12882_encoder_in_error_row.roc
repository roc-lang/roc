app [main!] { pf: platform "../fx-open/platform/main.roc" }

# https://github.com/roc-lang/roc/issues/12882
# Encoding a value also carried by main!'s inferred error row must check cleanly.
main! : List(Str) => Try({}, _)
main! = |_args| {
	width = "0.9"
	_ = Json.to_str(width)
	Err(WidthNotRefused(width))
}
