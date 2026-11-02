# repro for https://github.com/roc-lang/roc/issues/12495
# A dev build served the URL package's cached dev packs must find every key
# `Check.probe` just inserted into its `Dict`, as the cold build does.
app [main!] {
	pf: platform "../../fx-open/platform/main.roc",
	pkg: "https://example.com/roc/issue12495/1.0.0/FakeHashAbcDefGhiJkLmNoPqRsTuVwXyZ12495Dicto.tar.zst",
}

import pf.Stdout
import pkg.Tree
import pkg.Check

main! = |args| {
	prefix = args.get(1) ?? "SPDXRef-"
	tree = Tree.Array(["Subject", "compiler", "native", "platform", "restricted"].map(|name| Str.concat(prefix, name)))
	Stdout.line!("probes=${Check.probe(tree)}")
	Ok({})
}
