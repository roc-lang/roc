app [main!] { pf: platform "../fx-open/platform/main.roc" }

import pf.Stdout

# https://github.com/roc-lang/roc/issues/12879
# The split's successful payload must remain inhabited when url also escapes in an error.
main! = |args| {
	url = "a:${args.len().to_str()}"
	port = url.split_last(":").map_ok(|parts| parts.after) ?? ""
	Stdout.line!(port)
	if (port.is_empty()) Err(NoPort(url)) else Ok({})
}
