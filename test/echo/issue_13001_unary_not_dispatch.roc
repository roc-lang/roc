# repro for https://github.com/roc-lang/roc/issues/13001
# Unary `!` calls the operand type's `not` method: a nominal type's own `not`,
# and `Bool.not` for a Bool or for an unannotated tag union. The values depend
# on the command line so negation runs at runtime.
Switch := [On, Off].{
	not : Switch -> Switch
	not = |s| match s {
		On => Off
		Off => On
	}

	label : Switch -> Str
	label = |s| match s {
		On => "on"
		Off => "off"
	}
}

go : U64 -> U64
go = |n| {
	var $done = False
	if n > 3 { $done = True }
	if !$done { n } else { 0 }
}

swaps : List(U64) -> U64
swaps = |values| {
	var $swapped = False
	var $iterations = 0
	while True {
		for value in values {
			if value > 0 and $iterations == 0 { $swapped = True }
		}
		$iterations = $iterations + 1
		if !$swapped { break }
		$swapped = False
	}
	$iterations
}

count : List(U64) -> U64
count = |items| {
	var $done = False
	var $n = 0
	var $rest = items
	while !$done {
		match $rest.first() {
			Ok(x) => {
				$n = $n + x
				$rest = $rest.drop_first(1)
			}
			Err(_) => { $done = True }
		}
	}
	$n
}

main! = |args| {
	many = args.len() > 1
	s : Switch
	s = if many { Switch.On } else { Switch.Off }
	echo!((!s).label())
	echo!(Str.inspect((!many, !!many)))
	echo!(Str.inspect((go(args.len()), swaps([0, args.len()]), count([1, 2, args.len()]))))
	Ok({})
}
