app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout
import pf.Host

identity : U64 -> U64
identity = |value| value

in_call_arg : U64 -> U64
in_call_arg = |x| {
	var $count = 0
	y = identity(
		{
			$count = x
			1
		},
	)
	$count + y
}

in_operand : U64 -> U64
in_operand = |x| {
	var $count = 0
	y = 1 + (match x {
		0 => 0
		_ => {
			$count = x
			1
		}
	})
	$count + y
}

in_record : U64 -> U64
in_record = |x| {
	var $count = 0
	r = {
		a: if x > 0 {
			$count = x
			1
		} else {
			0
		},
	}
	$count + r.a
}

bound_first : U64 -> U64
bound_first = |x| {
	var $count = 0
	v = {
		$count = x
		1
	}
	y = identity(v)
	$count + y
}

statement_in_block : U64 -> U64
statement_in_block = |x| {
	var $count = 0
	y = {
		if x > 0 {
			$count = x
		}
		1
	}
	$count + y
}

in_loop : U64 -> U64
in_loop = |limit| {
	var $index = 0
	var $count = 0
	while $index < limit {
		_ = identity(
			match $index % 2 {
				0 => {
					$count = $count + 1
					1
				}
				_ => 0
			},
		)
		$index = $index + 1
	}
	$count
}

list_update : U64 -> U64
list_update = |x| {
	var $items = [x]
	y = {
		$items = $items.append(x)
		x
	}
	$items.append(y).len().to_u64()
}

say! : Str, U64 => U64
say! = |label, value| {
	Stdout.line!(label)
	value
}

ordered! : U64 => U64
ordered! = |x| {
	var $count = 0
	values = (
		say!("left", x),
		{
			$count = say!("right", x + 1)
			$count
		},
	)
	# The earlier operand must run before the nested reassignment, and both
	# the nested result and the later read must retain the new version.
	values.0 + values.1 + $count
}

early! : U64 => U64
early! = |x| {
	values = (
		{
			return x
		},
		say!("unreachable", x),
	)
	values.0 + values.1
}

answer = in_call_arg(7)

branch_answer = in_operand(7)

list_answer = list_update(7)

main! = || {
	x = Host.get_greeting!(Host.new("seven")).count_utf8_bytes().to_u64() - Str.count_utf8_bytes("Hello, seven!").to_u64() + 7
	Stdout.line!("in_call_arg: ${in_call_arg(x).to_str()}")
	Stdout.line!("in_operand: ${in_operand(x).to_str()}")
	Stdout.line!("in_record: ${in_record(x).to_str()}")
	Stdout.line!("bound_first: ${bound_first(x).to_str()}")
	Stdout.line!("constant: ${answer.to_str()}")
	Stdout.line!("branch_constant: ${branch_answer.to_str()}")
	Stdout.line!("list_constant: ${list_answer.to_str()}")
	Stdout.line!("zero_call: ${in_call_arg(x - x).to_str()}")
	Stdout.line!("zero_operand: ${in_operand(x - x).to_str()}")
	Stdout.line!("zero_record: ${in_record(x - x).to_str()}")
	Stdout.line!("statement: ${statement_in_block(x).to_str()}")
	Stdout.line!("loop: ${in_loop(x + 3).to_str()}")
	Stdout.line!("list: ${list_update(x).to_str()}")
	ordered = ordered!(x)
	Stdout.line!("ordered: ${ordered.to_str()}")
	early = early!(x)
	Stdout.line!("early: ${early.to_str()}")
}
