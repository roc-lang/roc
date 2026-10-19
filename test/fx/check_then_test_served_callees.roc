# repro for https://github.com/roc-lang/roc/issues/12437
app [main!] {
	pf: platform "./platform/main.roc",
	pkg: "./check_then_test_served_callees_pkg/main.roc",
}

import pf.Stdout
import pkg.P

Endpoint : { host : Str, port : U64 }

endpoint : P(Endpoint)
endpoint =
	P.const(|host| |port| { host, port })
		.keep(P.chomp_while(|b| b != ':').map(Str.from_utf8_lossy))
		.skip(P.byte(':'))
		.keep(P.digits)

expect P.parse_str(endpoint, "example.com:443") == Ok({ host: "example.com", port: 443 })

main! = || Stdout.line!(Str.inspect(P.parse_str(endpoint, "example.com:443")))
