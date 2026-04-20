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

	run : Cli(a) -> a
	run = |c| c.value
}

main! = || {
	parser = {
		w: Cli.option({ long: "w", default: "10" }),
		x: Cli.option({ long: "x", default: "20" }),
		y: Cli.option({ long: "y", default: "30" }),
		z: Cli.option({ long: "z", default: "40" }),
	}.Cli
	config = parser.run()
	Stdout.line!("w=${config.w}, x=${config.x}, y=${config.y}, z=${config.z}")
}
