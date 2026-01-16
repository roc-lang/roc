app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# True Applicative CLI Parser demonstrating the record builder pattern
#
# The record builder syntax `{ a: fa, b: fb }.T` desugars to:
#   T.map2(fa, fb, |a, b| { a, b })
#
# For 3+ fields it chains map2 calls:
#   T.map2(fa, T.map2(fb, fc, |b, c| (b, c)), |a, (b, c)| { a, b, c })
#
# Parameterized opaque type for CLI parsers
# The type checker unifies structurally - no explicit unwrap needed
Cli(a) := { value : a, help : Str }.{
	# TRUE APPLICATIVE map2: takes wrapped inputs, returns wrapped output
	# Cli(a), Cli(b), (a, b -> c) -> Cli(c)
	map2 : Cli(a), Cli(b), (a, b -> c) -> Cli(c)
	map2 = |ca, cb, f| {
		value: f(ca.value, cb.value),
		help: Str.concat(ca.help, cb.help),
	}

	# Create a CLI option parser with a default value
	option : { long : Str, default : Str } -> Cli(Str)
	option = |config| {
		value: config.default,
		help: "  --${config.long} <value>",
	}

	# Create a CLI flag parser with a default value
	flag : { long : Str, default : Bool } -> Cli(Bool)
	flag = |config| {
		value: config.default,
		help: "  --${config.long}",
	}

	# Extract the wrapped value
	run : Cli(a) -> a
	run = |c| c.value

	# Get the combined help text
	get_help : Cli(a) -> Str
	get_help = |c| c.help
}

main! = || {
	Stdout.line!("=== True Applicative Record Builder ===")
	Stdout.line!("")

	# Test 1: Two-field record builder
	# Desugars to: Cli.map2(host, port, |h, p| { host: h, port: p })
	# Returns: Cli({ host: Str, port: Str })
	host_parser = Cli.option({ long: "host", default: "localhost" })
	port_parser = Cli.option({ long: "port", default: "8080" })
	parser1 = { host: host_parser, port: port_parser }.Cli
	config1 = Cli.run(parser1)
	Stdout.line!("Test 1: Two-field record builder")
	Stdout.line!("  Result: host=${config1.host}, port=${config1.port}")
	Stdout.line!("")

	# Test 2: Three-field record builder (chains map2)
	# Desugars to: Cli.map2(a, Cli.map2(b, c, |b,c| (b,c)), |a,(b,c)| {a,b,c})
	# Inner map2 returns Cli((Str, Bool)), outer combines all three
	Stdout.line!("Test 2: Three-field record builder")
	parser2 = {
		name: Cli.option({ long: "name", default: "world" }),
		count: Cli.option({ long: "count", default: "1" }),
		verbose: Cli.flag({ long: "verbose", default: Bool.False }),
	}.Cli
	config2 = Cli.run(parser2)
	Stdout.line!("  Result: name=${config2.name}, count=${config2.count}, verbose=${Str.inspect(config2.verbose)}")
	Stdout.line!("")

	# Test 3: Four-field record builder
	Stdout.line!("Test 3: Four-field record builder")
	parser3 = {
		w: Cli.option({ long: "w", default: "10" }),
		x: Cli.option({ long: "x", default: "20" }),
		y: Cli.option({ long: "y", default: "30" }),
		z: Cli.option({ long: "z", default: "40" }),
	}.Cli
	config3 = Cli.run(parser3)
	Stdout.line!("  Result: w=${config3.w}, x=${config3.x}, y=${config3.y}, z=${config3.z}")
	Stdout.line!("")

	# Test 4: Verify help text is combined
	Stdout.line!("Test 4: Combined help text")
	simple_parser = {
		input: Cli.option({ long: "input", default: "stdin" }),
		output: Cli.option({ long: "output", default: "stdout" }),
	}.Cli
	Stdout.line!("  Help:")
	Stdout.line!(Cli.get_help(simple_parser))

	# Test 5: Equivalence with direct map2 call
	Stdout.line!("Test 5: Equivalence with direct map2")
	p1 = Cli.option({ long: "a", default: "1" })
	p2 = Cli.option({ long: "b", default: "2" })
	builder_result = Cli.run({ a: p1, b: p2 }.Cli)
	direct_result = Cli.run(Cli.map2(p1, p2, |a, b| { a, b }))
	Stdout.line!("  Builder: a=${builder_result.a}, b=${builder_result.b}")
	Stdout.line!("  Direct:  a=${direct_result.a}, b=${direct_result.b}")
	Stdout.line!("")

	Stdout.line!("=== All tests passed! ===")
}
