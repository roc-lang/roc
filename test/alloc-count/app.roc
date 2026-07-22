app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Allocation regression coverage for lazy UTF-8 string iteration and byte
# slicing. All measured strings depend on the hosted input so compile-time
# evaluation cannot remove the operations.
run! : Str => Str
run! = |input| {
	# Harness control: converting an SSO string to List(U8) must allocate.
	control_before = Host.alloc_count!()
    bytes = Str.to_utf8(input)
	control_allocs = Host.alloc_count!() - control_before
	expect control_allocs == 1

    before = Host.alloc_count!()

    var $sum = 0
    for byte in bytes {
        $sum = $sum + byte.to_u64()
    }

    loop_allocs = Host.alloc_count!() - before
    expect loop_allocs == 0

	large = Str.concat(input, input)
	multibyte = Str.concat(large, "é")

	check_iter_allocs!(input, $sum)
	check_iter_allocs!(large, $sum * 2)
	check_large_to_utf8_allocs!(large)
	check_drop_allocs!(large)
	check_rejected_drop_allocs!(multibyte)

    "sum: ${$sum.to_str()}, loop allocations: ${loop_allocs.to_str()}"
}

check_iter_allocs! : Str, U64 => {}
check_iter_allocs! = |str, expected| {
	before = Host.alloc_count!()
	sum = Iter.fold(Str.iter_utf8(str), 0, |acc, byte| acc + byte.to_u64())
	allocs = Host.alloc_count!() - before
	expect allocs == 0
	expect sum == expected
	{}
}

check_large_to_utf8_allocs! : Str => {}
check_large_to_utf8_allocs! = |str| {
	before = Host.alloc_count!()
	bytes = Str.to_utf8(str)
	allocs = Host.alloc_count!() - before
	expect allocs == 0
	expect List.len(bytes) == Str.count_utf8_bytes(str)
	{}
}

check_drop_allocs! : Str => {}
check_drop_allocs! = |str| {
	first_before = Host.alloc_count!()
	first = Str.drop_first_bytes(str, 1)
	first_allocs = Host.alloc_count!() - first_before
	expect first_allocs == 0
	expect first == Ok(Str.drop_prefix(str, "s"))

	last_before = Host.alloc_count!()
	last = Str.drop_last_bytes(str, 1)
	last_allocs = Host.alloc_count!() - last_before
	expect last_allocs == 0
	expect last == Ok(Str.drop_suffix(str, "t"))
	{}
}

check_rejected_drop_allocs! : Str => {}
check_rejected_drop_allocs! = |str| {
	first_before = Host.alloc_count!()
	first = Str.drop_first_bytes(str, Str.count_utf8_bytes(str) - 1)
	first_allocs = Host.alloc_count!() - first_before
	expect first_allocs == 0
	expect first == Err(BadUtf8)

	last_before = Host.alloc_count!()
	last = Str.drop_last_bytes(str, 1)
	last_allocs = Host.alloc_count!() - last_before
	expect last_allocs == 0
	expect last == Err(BadUtf8)
	{}
}
