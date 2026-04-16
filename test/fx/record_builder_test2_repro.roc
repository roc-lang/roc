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

	run : Cli(a) -> a
	run = |c| c.value
}

main! = || {
	parser = {
		name: Cli.option({ long: "name", default: "world" }),
		count: Cli.option({ long: "count", default: "1" }),
		verbose: Cli.flag({ long: "verbose", default: Bool.False }),
	}.Cli
	config = parser.run()
	Stdout.line!(
		"name=${config.name}, count=${config.count}, verbose=${Str.inspect(config.verbose)}",
	)
}
