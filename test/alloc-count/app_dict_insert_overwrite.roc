app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Inserting an absent key into a uniquely owned, pre-sized dictionary mutates
# the entry list and Robin Hood bucket table in place and allocates nothing --
# that is what app_dict_insert.roc pins. Overwriting a key that is already
# present likewise performs no growth or bucket shifting and must preserve the
# unique entry backing across the lookup result wrapper.
#
# Narrowed as far as the builtin surface allows:
#
#   * `Dict.get` over the same keys allocates nothing, so the lookup half of
#     `insert` is not responsible.
#   * `List.set` on a uniquely owned list -- including a list of tuples, which
#     is what a dictionary stores -- allocates nothing, so the write itself is
#     fine when the list is genuinely unique.
#
# This allocation assertion pins the ownership schedule in addition to the
# result, because a copying insert would still return the correct dictionary.
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

	# Overwrite: every key is present and the capacity is untouched.
	before = Host.alloc_count!()
	var $again = 0
	while $again < count {
		$dict = Dict.insert($dict, $again, $again * 2)
		$again = $again + 1
	}
	overwrite_allocs = Host.alloc_count!() - before

	expect Dict.len($dict) == count
	expect Dict.get($dict, count - 1) == Ok((count - 1) * 2)
	expect overwrite_allocs == 0

	"entries: ${Dict.len($dict).to_str()}, overwrite allocations: ${overwrite_allocs.to_str()}"
}
