app [main!] { pf: platform "./platform/main.roc" }

import pf.Decoder
import pf.Headers

main! : Headers => U64
main! = |headers| {
	decoded : { foo : Str, bar : Str }
	decoded = Headers.decode(headers, Decoder.derive({}))

	Str.count_utf8_bytes(decoded.foo) + Str.count_utf8_bytes(decoded.bar)
}
