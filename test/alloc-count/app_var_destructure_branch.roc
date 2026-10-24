app [run!] { pf: platform "./platform/main.roc" }

import pf.Host

# Repro for https://github.com/roc-lang/roc/issues/12419. A function-level `var`
# is reassigned by destructuring a call result inside the `if` branch that
# produces the function's value. Nothing reads the `var` after the `if`, so the
# list must stay unique and the appends must not copy, the same as the control
# that binds a fresh immutable name.
P : { out : List(U64), last : U64 }

push : List(U64), U64 -> P
push = |out, x| { out: out.append(x), last: x }

step_var : List(U64), U64 -> List(U64)
step_var = |out, x| {
	var $out = out
	if x % 2 == 0 {
		{ out: $out, last } = push($out, x)
		$out.append(last)
	} else {
		out.append(x)
	}
}

step_control : List(U64), U64 -> List(U64)
step_control = |out, x| {
	if x % 2 == 0 {
		{ out: o1, last } = push(out, x)
		o1.append(last)
	} else {
		out.append(x)
	}
}

run_var : U64 -> List(U64)
run_var = |count| {
	var $out = List.with_capacity(count * 2)
	var $i = 0
	while $i < count {
		$out = step_var($out, $i)
		$i = $i + 1
	}
	$out
}

run_control : U64 -> List(U64)
run_control = |count| {
	var $out = List.with_capacity(count * 2)
	var $i = 0
	while $i < count {
		$out = step_control($out, $i)
		$i = $i + 1
	}
	$out
}

run! : Str => Str
run! = |input| {
	count = Str.count_utf8_bytes(input) * 64

	var_before = Host.alloc_count!()
	var_out = run_var(count)
	var_allocs = Host.alloc_count!() - var_before

	control_before = Host.alloc_count!()
	control_out = run_control(count)
	control_allocs = Host.alloc_count!() - control_before

	"lengths: ${List.len(var_out).to_str()} ${List.len(control_out).to_str()}, allocations: ${var_allocs.to_str()} ${control_allocs.to_str()}"
}
