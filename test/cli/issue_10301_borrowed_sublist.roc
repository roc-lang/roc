# Repro for https://github.com/roc-lang/roc/issues/10301
app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdin
import pf.Stdout

main! = || {
	bytes = Stdin.line!().to_utf8()
	byte_count = bytes.len()

	chunks = Iter.custom(
		0.U64,
		Unknown,
		|start|
			if start + 16 <= byte_count {
				Ok((bytes.sublist({ start: start, len: 16 }), start + 16))
			} else {
				Err(NoMore)
			},
	)

	var sum = 0.U64
	for chunk in chunks {
		var index = 0.U64
		while index < chunk.len() {
			match chunk.get(index) {
				Ok(byte) => {
					sum = sum + byte.to_u64()
				}
				Err(_) => {}
			}
			index = index + 1
		}
	}
	Stdout.line!(sum.to_str())
}
