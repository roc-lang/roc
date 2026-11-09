app [main!] { pf: platform "./platform/main.roc" }

import pf.FallibleHost

# Regression test for issue 12848: under `--opt=size`, a helper with many
# callers must be emitted once and called, not copied into every caller.
#
# `put_u32` is the helper. It adds the marker constant 987654321
# (0x3ADE68B1), whose encoding appears once in every emitted copy of the
# helper, so the test can count the copies in the compiled module. This
# program has 16 callers of the helper; its sibling has one.
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

write_2 : List(U8), U64 -> List(U8)
write_2 = |buf, x| put_u32(buf.append(2), x * 2)

write_3 : List(U8), U64 -> List(U8)
write_3 = |buf, x| put_u32(buf.append(3), x * 3)

write_4 : List(U8), U64 -> List(U8)
write_4 = |buf, x| put_u32(buf.append(4), x * 4)

write_5 : List(U8), U64 -> List(U8)
write_5 = |buf, x| put_u32(buf.append(5), x * 5)

write_6 : List(U8), U64 -> List(U8)
write_6 = |buf, x| put_u32(buf.append(6), x * 6)

write_7 : List(U8), U64 -> List(U8)
write_7 = |buf, x| put_u32(buf.append(7), x * 7)

write_8 : List(U8), U64 -> List(U8)
write_8 = |buf, x| put_u32(buf.append(8), x * 8)

write_9 : List(U8), U64 -> List(U8)
write_9 = |buf, x| put_u32(buf.append(9), x * 9)

write_10 : List(U8), U64 -> List(U8)
write_10 = |buf, x| put_u32(buf.append(10), x * 10)

write_11 : List(U8), U64 -> List(U8)
write_11 = |buf, x| put_u32(buf.append(11), x * 11)

write_12 : List(U8), U64 -> List(U8)
write_12 = |buf, x| put_u32(buf.append(12), x * 12)

write_13 : List(U8), U64 -> List(U8)
write_13 = |buf, x| put_u32(buf.append(13), x * 13)

write_14 : List(U8), U64 -> List(U8)
write_14 = |buf, x| put_u32(buf.append(14), x * 14)

write_15 : List(U8), U64 -> List(U8)
write_15 = |buf, x| put_u32(buf.append(15), x * 15)

write_16 : List(U8), U64 -> List(U8)
write_16 = |buf, x| put_u32(buf.append(16), x * 16)

main! : () => Str
main! = || {
	x = FallibleHost.json_input!({}).count_utf8_bytes().to_u64()
	var $b = []
	$b = write_1($b, x)
	$b = write_2($b, x)
	$b = write_3($b, x)
	$b = write_4($b, x)
	$b = write_5($b, x)
	$b = write_6($b, x)
	$b = write_7($b, x)
	$b = write_8($b, x)
	$b = write_9($b, x)
	$b = write_10($b, x)
	$b = write_11($b, x)
	$b = write_12($b, x)
	$b = write_13($b, x)
	$b = write_14($b, x)
	$b = write_15($b, x)
	$b = write_16($b, x)
	$b.len().to_str()
}
