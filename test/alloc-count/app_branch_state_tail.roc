app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for issue 11823. Each helper reassigns a `var` in one branch and then
# passes it to a consuming call in tail position. Nothing reads the `var` after
# the branch, so the output list must stay unique and the appends must not copy.
add : List(U64), U64 -> Try(List(U64), [Full])
add = |list, value| if List.len(list) > 1000000 {
	Err(Full)
} else {
	Ok(List.append(list, value))
}

emit_if : List(U64), Bool -> Try(List(U64), [Full])
emit_if = |bytes, flag| {
	var $out = bytes
	if flag {
		add($out, 10)
	} else {
		$out = add($out, 1)?
		add($out, 10)
	}
}

emit_match : List(U64), U64 -> Try(List(U64), [Full])
emit_match = |bytes, n| {
	var $out = bytes
	match n {
		0 => add($out, 10)
		_ => {
			$out = add($out, n)?
			add($out, 10)
		}
	}
}

emit_ok : List(U64), Bool -> Try(List(U64), [Full])
emit_ok = |bytes, flag| {
	var $out = bytes
	if flag {
		Ok(List.append($out, 10))
	} else {
		$out = add($out, 1)?
		Ok(List.append($out, 10))
	}
}

emit_control : List(U64), Bool -> Try(List(U64), [Full])
emit_control = |bytes, flag| {
	if flag {
		add(bytes, 10)
	} else {
		next = add(bytes, 1)?
		add(next, 10)
	}
}

branch_value : List(U64), U64 -> List(U64)
branch_value = |list, i| {
	var $out = list.append(i)
	if i % 2 == 0 {
		$out.append(1)
	} else {
		$out = $out.append(2)
		$out
	}
}

run_if : U64 -> List(U64)
run_if = |count| {
	var $out = List.with_capacity(count * 2)
	var $i = 0
	while $i < count {
		$out = match emit_if($out, False) {
			Ok(next) => next
			Err(Full) => []
		}
		$i = $i + 1
	}
	$out
}

run_match : U64 -> List(U64)
run_match = |count| {
	var $out = List.with_capacity(count * 2)
	var $i = 0
	while $i < count {
		$out = match emit_match($out, 1) {
			Ok(next) => next
			Err(Full) => []
		}
		$i = $i + 1
	}
	$out
}

run_ok : U64 -> List(U64)
run_ok = |count| {
	var $out = List.with_capacity(count * 2)
	var $i = 0
	while $i < count {
		$out = match emit_ok($out, False) {
			Ok(next) => next
			Err(Full) => []
		}
		$i = $i + 1
	}
	$out
}

run_control : U64 -> List(U64)
run_control = |count| {
	var $out = List.with_capacity(count * 2)
	var $i = 0
	while $i < count {
		$out = match emit_control($out, False) {
			Ok(next) => next
			Err(Full) => []
		}
		$i = $i + 1
	}
	$out
}

run_branch_value : U64 -> List(U64)
run_branch_value = |count| {
	var $out = List.with_capacity(count * 3)
	var $i = 0
	while $i < count {
		$out = branch_value($out, $i)
		$i = $i + 1
	}
	$out
}

run! : Str => Str
run! = |input| {
	count = Str.count_utf8_bytes(input) * 64

	if_before = Host.alloc_count!()
	if_out = run_if(count)
	if_allocs = Host.alloc_count!() - if_before

	match_before = Host.alloc_count!()
	match_out = run_match(count)
	match_allocs = Host.alloc_count!() - match_before

	ok_before = Host.alloc_count!()
	ok_out = run_ok(count)
	ok_allocs = Host.alloc_count!() - ok_before

	control_before = Host.alloc_count!()
	control_out = run_control(count)
	control_allocs = Host.alloc_count!() - control_before

	branch_before = Host.alloc_count!()
	branch_out = run_branch_value(count)
	branch_allocs = Host.alloc_count!() - branch_before

	"lengths: ${List.len(if_out).to_str()} ${List.len(match_out).to_str()} ${List.len(ok_out).to_str()} ${List.len(control_out).to_str()} ${List.len(branch_out).to_str()}, allocations: ${if_allocs.to_str()} ${match_allocs.to_str()} ${ok_allocs.to_str()} ${control_allocs.to_str()} ${branch_allocs.to_str()}"
}
