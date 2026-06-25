app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Rb

main! = || {
	# Direct imported map2 works:
	direct = Rb.map2(Rb.field("localhost"), Rb.field("8080"), |host, port| { host, port }).run()
	Stdout.line!("direct=${direct.host}:${direct.port}")

	# This should lower to the same imported Rb.map2 call.
	config = {
		host: Rb.field("localhost"),
		port: Rb.field("8080"),
	}.Rb.run()

	Stdout.line!("builder=${config.host}:${config.port}")
}
