app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost

# Regression test for issue 12848: under `--opt=size`, a helper with many
# callers must be emitted once and called, not copied into every caller.
#
# `put_u32` is the helper. It adds the marker constant 987654321
# (0x3ADE68B1), whose encoding appears once in every emitted copy of the
# helper, so the test can count the copies in the compiled module. This
# program has one caller of the helper; its sibling has sixteen.
put_u32 : List(U8), U64 -> List(U8)
put_u32 = |buf, v| {
	w = (v + 987654321) % 4294967296
	buf
		.append((w % 256).to_u8_wrap())
		.append(((w // 256) % 256).to_u8_wrap())
		.append(((w // 65536) % 256).to_u8_wrap())
		.append((w // 16777216).to_u8_wrap())
}

write_1 : List(U8), U64 -> List(U8)
write_1 = |buf, x| put_u32(buf.append(1), x * 1)

main! : () => Str
main! = || {
	x = FallibleHost.json_input!({}).count_utf8_bytes().to_u64()
	var $b = []
	$b = write_1($b, x)
	$b.len().to_str()
}
