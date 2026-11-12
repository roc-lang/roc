app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# A string literal too long to hold inline is read from the program image, so
# using one allocates nothing. Appending to one copies it, and leaves the
# literal as it was for its next use.
prefix : Str -> Bool
prefix = |text| Str.starts_with(text, "a string literal that is far too long to be held inline")

run! : Str => Str
run! = |input| {
	start = Host.alloc_count!()

	var $matches = 0.U64
	var $i = 0
	while $i < 1000 {
		if prefix(input) {
			$matches = $matches + 1
		}
		$i = $i + 1
	}
	read_allocs = Host.alloc_count!() - start

	first = Str.concat("another literal that is far too long to be held inline", input)
	second = Str.concat("another literal that is far too long to be held inline", input)
	append_allocs = Host.alloc_count!() - start - read_allocs

	expect $matches == 0
	expect read_allocs == 0
	expect append_allocs == 2
	expect first == second
	expect Str.starts_with(second, "another literal that is far too long to be held inline")
	"matches: ${$matches.to_str()}, lengths: ${Str.count_utf8_bytes(first).to_str()} ${Str.count_utf8_bytes(second).to_str()}, allocations: ${read_allocs.to_str()} ${append_allocs.to_str()}"
}
