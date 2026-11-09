app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# repro for https://github.com/roc-lang/roc/issues/12195
#
# `Dict.update` on a uniquely owned dictionary whose key is already present
# rewrites the entry in place, the same way `Dict.insert` overwrites do in
# app_dict_insert_overwrite.roc, so it allocates nothing. A copying update would
# still return the correct dictionary, so the allocation count is asserted
# alongside the result.
run! : Str => Str
run! = |input| {
	# Derived from the runtime input so compile-time evaluation cannot remove
	# the work being measured.
	count = 1000 + Str.count_utf8_bytes(input)
	var $dict = Dict.with_capacity(count)

	# Populate: every key is absent, so this takes the in-place path.
	var $index = 0
	while $index < count {
		$dict = Dict.insert($dict, $index, $index)
		$index = $index + 1
	}

	# Update: every key is present and the capacity is untouched.
	before = Host.alloc_count!()
	var $again = 0
	while $again < count {
		$dict = Dict.update($dict, $again, |r| Ok((r ?? 0) + 1))
		$again = $again + 1
	}
	update_allocs = Host.alloc_count!() - before

	expect Dict.len($dict) == count
	expect Dict.get($dict, count - 1) == Ok(count)
	expect update_allocs == 0

	"entries: ${Dict.len($dict).to_str()}, update allocations: ${update_allocs.to_str()}"
}
