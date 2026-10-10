app [main!] { pf: platform "./platform/main.roc" }

# Explicitly reconstruct a hosted error into a wider Roc-owned row on wasm32.
# The host's Ok("ok") must retain its meaning across the declared ABI.

import pf.FallibleHost

main! : () => Str
main! = || {
	match widened!({}) {
		Ok(value) => value
		Err(HostErr(message)) => "misread as Err(HostErr(${message}))"
		Err(Widened(_)) => "misread as Err(Widened)"
	}
}

widened! : {} => Try(Str, [HostErr(Str), Widened(I32)])
widened! = |{}|
	match FallibleHost.str_ok!({}) {
		Ok(value) => Ok(value)
		Err(HostErr(message)) => Err(HostErr(message))
	}
