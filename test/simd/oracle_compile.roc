app [main!] {
	pf: platform "../fx/platform/main.roc",
	oracle: "oracle/main.roc",
}

import pf.Stdout
import oracle.SimdOracle

main! = || {
	value = SimdOracle.U8x16.splat(200).plus_wrap(SimdOracle.U8x16.splat(100))
	Stdout.line!(value.to_u128_bits().to_str())
}

expect SimdOracle.U8x16.splat(200).plus_wrap(SimdOracle.U8x16.splat(100)).get_lane(0) == 44
