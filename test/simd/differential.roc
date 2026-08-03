app [main!] {
	pf: platform "../fx/platform/main.roc",
	simd: "main.roc",
}

import pf.Stdin
import pf.Stdout
import simd.SimdDifferential

seed : U128
seed = 21345817372864405881847059188222722561

main! = || {
	input_len = Stdin.line!().to_utf8().len()
	if SimdDifferential.run_corpus(seed.bitwise_xor(input_len.to_u128())) {
		Stdout.line!("PASS simd differential")
	} else {
		crash "SIMD differential returned false"
	}
}

expect SimdDifferential.run_corpus(seed)
