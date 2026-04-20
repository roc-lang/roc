app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

Cli(a) := { value : a, help : Str }.{
	map2 : Cli(a), Cli(b), (a, b -> c) -> Cli(c)
	map2 = |ca, cb, f| {
		value: f(ca.value, cb.value),
		help: Str.concat(ca.help, cb.help),
	}

	option : { long : Str, default : Str } -> Cli(Str)
	option = |config| {
		value: config.default,
		help: "  --${config.long} <value>",
	}

	flag : { long : Str, default : Bool } -> Cli(Bool)
	flag = |config| {
		value: config.default,
		help: "  --${config.long}",
	}
}

main! = || {
	p2 = Cli.option({ long: "count", default: "1" })
	p3 = Cli.flag({ long: "verbose", default: Bool.False })
	_ = Cli.map2(p2, p3, |b, c| { b, c })
	Stdout.line!("done")
}
