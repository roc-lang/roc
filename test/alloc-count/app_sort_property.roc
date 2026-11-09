app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# https://github.com/roc-lang/roc/issues/12306
# Checks the Roc sort over many lengths, input shapes, and item kinds: every
# result is a sorted permutation of its input, equal items keep their input
# order, a comparator that contradicts itself still yields a permutation, and
# sorting a uniquely owned list allocates nothing beyond its scratch list.

next_random : U64 -> U64
next_random = |state| state.times_wrap(6364136223846793005).plus_wrap(1442695040888963407)

# `len` keys below `modulus`, pseudo-random from `seed`.
random_keys : U64, U64, U64 -> List(U64)
random_keys = |len, seed, modulus| {
	var $state = seed
	var $keys = List.with_capacity(len)
	var $index = 0
	while $index < len {
		$state = next_random($state)
		$keys = List.append($keys, $state.shr_wrap(33) % modulus)
		$index = $index + 1
	}
	$keys
}

# The key sequences the checks sort, by shape name.
keys_for_shape : Str, U64, U64 -> List(U64)
keys_for_shape = |shape, len, seed| {
	zeros = random_keys(len, seed, 1)
	if shape == "random" {
		random_keys(len, seed, 1000000007)
	} else if shape == "few_distinct" {
		random_keys(len, seed, 3)
	} else if shape == "ascending" {
		List.map_with_index(zeros, |_, index| index)
	} else if shape == "descending" {
		List.map_with_index(zeros, |_, index| len - index)
	} else if shape == "sawtooth" {
		List.map_with_index(zeros, |_, index| index % 37)
	} else if shape == "all_equal" {
		List.map(zeros, |_| 42)
	} else if shape == "sorted_runs" {
		# Blocks of 100 ascending keys, each block starting over.
		List.map_with_index(zeros, |_, index| (index % 100) + (index // 100) * 7 % 11)
	} else {
		# Pipe organ: up, then down.
		List.map_with_index(zeros, |_, index| if index * 2 < len { index } else { len - index })
	}
}

shapes : List(Str)
shapes = ["random", "few_distinct", "ascending", "descending", "sawtooth", "all_equal", "sorted_runs", "pipe_organ"]

lengths : List(U64)
lengths = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 15, 16, 17, 31, 32, 33, 63, 64, 95, 96, 97, 127, 131, 132, 133, 255, 256, 300, 511, 512, 1000, 2047, 2048, 2049, 2500, 4096, 6000]

# A sorted permutation of pairs whose second component is the input index:
# keys never decrease, equal keys keep increasing indices, and the index
# multiset is exactly 0..len.
check_stable_sorted : List((U64, U64)), U64 -> Try({}, Str)
check_stable_sorted = |sorted, len| {
	if List.len(sorted) != len {
		return Err("length ${List.len(sorted).to_str()} != ${len.to_str()}")
	}
	var $seen = List.repeat(Bool.False, len)
	var $index = 0
	while $index < len {
		(key, original) = list_pair(sorted, $index)
		if original >= len or list_bool($seen, original) {
			return Err("index ${original.to_str()} missing or repeated")
		}
		$seen = List.set($seen, original, Bool.True) ?? $seen
		if $index > 0 {
			(prev_key, prev_original) = list_pair(sorted, $index - 1)
			if prev_key > key {
				return Err("keys out of order at ${$index.to_str()}")
			}
			if prev_key == key and prev_original > original {
				return Err("equal keys ${key.to_str()} reordered at ${$index.to_str()}")
			}
		}
		$index = $index + 1
	}
	Ok({})
}

list_pair : List((U64, U64)), U64 -> (U64, U64)
list_pair = |list, index| List.get(list, index) ?? (0, 0)

list_bool : List(Bool), U64 -> Bool
list_bool = |list, index| List.get(list, index) ?? Bool.True

pairs_from_keys : List(U64) -> List((U64, U64))
pairs_from_keys = |keys| List.map_with_index(keys, |key, index| (key, index))

compare_pairs : (U64, U64), (U64, U64) -> [Before, Same, After]
compare_pairs = |(left, _), (right, _)| left.order_relative_to(right)

# A record wider than the direct-sort threshold, so it sorts through indices.
Wide : { key : U64, original : U64, pad_a : U64, pad_b : U64, pad_c : U64, pad_d : U64, pad_e : U64, pad_f : U64, pad_g : U64, pad_h : U64, pad_i : U64, pad_j : U64, pad_k : U64 }

wide_from_pair : (U64, U64) -> Wide
wide_from_pair = |(key, original)| { key, original, pad_a: key + 1, pad_b: key + 2, pad_c: key + 3, pad_d: key + 4, pad_e: key + 5, pad_f: key + 6, pad_g: key + 7, pad_h: key + 8, pad_i: key + 9, pad_j: key + 10, pad_k: original }

pair_from_wide : Wide -> (U64, U64)
pair_from_wide = |wide| if wide.pad_a == wide.key + 1 and wide.pad_k == wide.original { (wide.key, wide.original) } else { (wide.key, 1000000000) }

# A heap-allocated string per pair, so refcount mistakes in the sort would
# double free or lose a string.
str_from_pair : (U64, U64) -> Str
str_from_pair = |(key, original)| "sort property item ${pad_number(key)} at input position ${pad_number(original)}"

pad_number : U64 -> Str
pad_number = |n| {
	digits = n.to_str()
	Str.concat(Str.repeat("0", 12 - Str.count_utf8_bytes(digits)), digits)
}

pair_from_str : Str -> (U64, U64)
pair_from_str = |s| {
	after_item = Str.drop_prefix(s, "sort property item ")
	split = Str.split_first(after_item, " at input position ") ?? { before: "", after: "" }
	key = U64.from_str(split.before) ?? 1000000000
	original = U64.from_str(split.after) ?? 1000000000
	(key, original)
}

compare_strs : Str, Str -> [Before, Same, After]
compare_strs = |left, right| {
	(left_key, _) = pair_from_str(left)
	(right_key, _) = pair_from_str(right)
	left_key.order_relative_to(right_key)
}

# Whether `result` holds exactly the items of `input`, whatever their order.
is_permutation : List((U64, U64)), List((U64, U64)) -> Bool
is_permutation = |result, input| {
	if List.len(result) != List.len(input) {
		return Bool.False
	}
	sorted_result = List.sort_with(result, compare_pairs_fully)
	sorted_input = List.sort_with(input, compare_pairs_fully)
	sorted_result == sorted_input
}

compare_pairs_fully : (U64, U64), (U64, U64) -> [Before, Same, After]
compare_pairs_fully = |(left_key, left_index), (right_key, right_index)|
	match left_key.order_relative_to(right_key) {
		Same => left_index.order_relative_to(right_index)
		other => other
	}

check_shape : Str, U64, U64 -> Try({}, Str)
check_shape = |shape, len, seed| {
	keys = keys_for_shape(shape, len, seed)
	pairs = pairs_from_keys(keys)
	site = "${shape} len ${len.to_str()}"

	# Narrow items.
	Try.map_err(check_stable_sorted(List.sort_with(pairs, compare_pairs), len), |err| "${site} pairs: ${err}")?
	# sort_by projects the key.
	Try.map_err(check_stable_sorted(List.sort_by(pairs, |(key, _)| key), len), |err| "${site} sort_by: ${err}")?
	# Reversed sort: compare against the plain sort of negated keys.
	reversed = List.sort_with_reversed(pairs, compare_pairs)
	negated_sorted = List.sort_with(List.map(pairs, |(key, index)| (1000000007 - key, index)), compare_pairs)
	expected_reversed = List.map(negated_sorted, |(key, index)| (1000000007 - key, index))
	if reversed != expected_reversed {
		return Err("${site} reversed sort differs from the plain sort of negated keys")
	}
	# Wide items go through the index sort.
	wide_sorted = List.sort_with(List.map(pairs, wide_from_pair), |left, right| left.key.order_relative_to(right.key))
	Try.map_err(check_stable_sorted(List.map(wide_sorted, pair_from_wide), len), |err| "${site} wide: ${err}")?
	# Refcounted items.
	str_sorted = List.sort_with(List.map(pairs, str_from_pair), compare_strs)
	Try.map_err(check_stable_sorted(List.map(str_sorted, pair_from_str), len), |err| "${site} strs: ${err}")?
	# Comparators that contradict themselves still yield permutations.
	always_after = List.sort_with(pairs, |_, _| After)
	if !is_permutation(always_after, pairs) {
		return Err("${site} always-After comparator lost or duplicated items")
	}
	always_before = List.sort_with(pairs, |_, _| Before)
	if !is_permutation(always_before, pairs) {
		return Err("${site} always-Before comparator lost or duplicated items")
	}
	inconsistent = List.sort_with(pairs, |(left, _), (right, _)| if (left + right) % 3 == 0 { Before } else if (left * 7 + right) % 5 == 0 { Same } else { After })
	if !is_permutation(inconsistent, pairs) {
		return Err("${site} inconsistent comparator lost or duplicated items")
	}
	inconsistent_strs = List.sort_with(List.map(pairs, str_from_pair), |left, right| if (Str.count_utf8_bytes(left) + Str.count_utf8_bytes(right)) % 2 == 0 { After } else { Before })
	if !is_permutation(List.map(inconsistent_strs, pair_from_str), pairs) {
		return Err("${site} inconsistent string comparator lost or duplicated items")
	}
	Ok({})
}

run! : Str => Str
run! = |input| {
	seed = Str.count_utf8_bytes(input) + 17
	var $failures = []
	for len in lengths {
		for shape in shapes {
			match check_shape(shape, len, seed + len) {
				Ok({}) => {}
				Err(message) => {
					$failures = List.append($failures, message)
				}
			}
		}
	}
	checked = List.len(lengths) * List.len(shapes)

	# A uniquely owned list sorts in place: no allocation below 32 items, and
	# exactly the scratch list above. Wide items also allocate their index list.
	small = random_keys(31, seed, 1000)
	medium = random_keys(1000, seed, 1000)
	wide = List.map(pairs_from_keys(random_keys(1000, seed, 1000)), wide_from_pair)
	before = Host.alloc_count!()
	small_sorted = List.sort(small)
	after_small = Host.alloc_count!()
	medium_sorted = List.sort(medium)
	after_medium = Host.alloc_count!()
	wide_sorted = List.sort_with(wide, |left, right| left.key.order_relative_to(right.key))
	after_wide = Host.alloc_count!()
	allocations = "${(after_small - before).to_str()} ${(after_medium - after_small).to_str()} ${(after_wide - after_medium).to_str()}"

	summary = "sort property checks: ${checked.to_str()}, failures: ${List.len($failures).to_str()}, lengths: ${List.len(small_sorted).to_str()} ${List.len(medium_sorted).to_str()} ${List.len(wide_sorted).to_str()}, in-place allocations: ${allocations}"
	if List.is_empty($failures) {
		summary
	} else {
		Str.concat(Str.concat(summary, "\n"), Str.join_with($failures, "\n"))
	}
}
