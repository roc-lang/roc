app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Refcounted keys and values must not make an otherwise unique Dict entry list
# shared during overwrite. Keys exceed Str's inline representation, and all
# inputs are built before the measured region.
run! : Str => Str
run! = |input| {
	count = 32 + Str.count_utf8_bytes(input)
	var $keys = []
	var $values = []
	var $build_index = 0
	while $build_index < count {
		key = "dict-overwrite-refcounted-key-${$build_index.to_str()}-padding"
		value = [$build_index]
		$keys = List.append($keys, key)
		$values = List.append($values, value)
		$build_index = $build_index + 1
	}

	var $dict = Dict.with_capacity(count)
	var $insert_index = 0
	while $insert_index < count {
		key = match List.get($keys, $insert_index) {
			Ok(found) => found
			Err(_) => crash "missing prebuilt key"
		}
		value = match List.get($values, $insert_index) {
			Ok(found) => found
			Err(_) => crash "missing prebuilt value"
		}
		$dict = Dict.insert($dict, key, value)
		$insert_index = $insert_index + 1
	}

	before = Host.alloc_count!()
	var $overwrite_index = 0
	while $overwrite_index < count {
		key = match List.get($keys, $overwrite_index) {
			Ok(found) => found
			Err(_) => crash "missing prebuilt key"
		}
		value = match List.get($values, $overwrite_index) {
			Ok(found) => found
			Err(_) => crash "missing prebuilt value"
		}
		$dict = Dict.insert($dict, key, value)
		$overwrite_index = $overwrite_index + 1
	}
	overwrite_allocs = Host.alloc_count!() - before

	expect Dict.len($dict) == count
	expect overwrite_allocs == 0

	"entries: ${Dict.len($dict).to_str()}, refcounted overwrite allocations: ${overwrite_allocs.to_str()}"
}
