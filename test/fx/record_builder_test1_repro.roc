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
	host_parser = Cli.option({ long: "host", default: "localhost" })
	port_parser = Cli.option({ long: "port", default: "8080" })
	parser = { host: host_parser, port: port_parser }.Cli
	config = parser.run()
	Stdout.line!("host=${config.host}, port=${config.port}")
}
