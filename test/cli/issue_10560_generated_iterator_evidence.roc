# Repro for https://github.com/roc-lang/roc/issues/10560:
# a generated iterator must preserve named type structure inside callable
# payloads while specializing an open generic procedure interface.
app [main!] { pf: platform "../fx/platform/main.roc" }

Sizing : [Grow]

LayoutConfig : {
	width : Sizing,
}

default_layout : LayoutConfig
default_layout = {
	width: Grow,
}

style = { layout: default_layout }

box = |style_fn, events| {
	[].fold(Iter.single(Open(style_fn, events)), |acc, _child| acc).append(Close)
}

main! = || {}

expect {
	view = box(|_status| style, [])

	match view.collect() {
		[Open(_, []), Close] => Bool.True
		_ => Bool.False
	}
}
