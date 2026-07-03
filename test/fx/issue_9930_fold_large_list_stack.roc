app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdin
import pf.Stdout

main! = || {
	# Read from stdin so the list contents are unknown at compile time.
	# With a fully constant list, the whole fold is evaluated at build time
	# and the generated code never runs.
	seed = Stdin.line!().to_utf8().len().to_u8_wrap()

	n = 4_000_000
	var $i = 0
	var $buf = List.with_capacity(n)
	while $i < n {
		$buf = $buf.append($i.to_u8_wrap().bitwise_xor(seed))
		$i = $i + 1
	}

	# n is an exact multiple of 256, so each wrapped byte value (0-255, XORed
	# with the seed) occurs an odd number of times (n/256), and XOR of all of
	# 0-255 is 0, so the total is 0 regardless of the seed.
	total = $buf.fold(0.U64, |acc, byte| acc.bitwise_xor(byte.to_u64()))
	Stdout.line!("total: ${total.to_str()}")
}
