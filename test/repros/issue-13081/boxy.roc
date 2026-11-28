app [run!] {
	lib: "./lib/main.roc",
	pf: platform "../../alloc-count/platform/main.roc",
}

import pf.Host
import lib.W
import Fmt

# Issue 13128: Boxy allocates small callback records per element. Count bytes
# around the encode, not calls: copying the growing output costs quadratic
# bytes whereas those callback records cost only linear bytes.
probe! : U64 => U64
probe! = |n| {
	var $items = []
	for i in (0.U64).until(n) {
		$items = $items.append(i)
	}
	before = Host.alloc_bytes!()
	encoded = Fmt.encode({ marker: n, values: $items })
	bytes = Host.alloc_bytes!() - before

	var $expected = [0xa0].concat("marker".to_utf8()).append(n.to_u8_wrap()).concat("values".to_utf8()).append(0x80)
	for i in (0.U64).until(n) {
		$expected = $expected.append(i.to_u8_wrap())
	}
	if encoded != $expected {
		crash "incorrect encoded bytes"
	}
	bytes
}

# Keep the input writer alive across a generic carry. Appending again from
# the old writer must not overwrite the byte in the first result.
kept_alias : U64, Str -> Bool
kept_alias = |seed, text| {
	match W.put_u64(seed, W.new({})) {
		Ok(writer) => {
			state = [text]
			match W.list_carry({ writer, state }, 0, |entries, _| Ok(entries)) {
				Ok(done) => match W.put_u64(17, writer) {
					Ok(other) => W.bytes(done.writer) == [seed.to_u8_wrap(), 0x80] and
						W.bytes(other) == [seed.to_u8_wrap(), 17] and
							W.bytes(writer) == [seed.to_u8_wrap()] and done.state == state
					Err(_) => False
				}
				Err(_) => False
			}
		}
		Err(_) => False
	}
}

run! : Str => Str
run! = |input| {
	n = input.count_utf8_bytes() * 128
	small = probe!(n)
	large = probe!(4 * n)
	linear = large <= 6 * small
	safe = kept_alias(n, input)
	"issue 13128: linear ${Str.inspect(linear)}, alias safe ${Str.inspect(safe)}, requested bytes ${small.to_str()} ${large.to_str()}"
}
