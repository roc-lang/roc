# https://github.com/roc-lang/roc/issues/13235
# Encoding a tag whose only payload is a single-tag zero-sized union reads that
# payload from a union with no storage.
app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

main! = |_args| {
	Stdout.line!(Json.to_str(Ok(C)))
	Stdout.line!(Json.to_str(Ok(Ok(C))))
	Stdout.line!(Json.to_str(Pair(C, D)))
	Ok({})
}
