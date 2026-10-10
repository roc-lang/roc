# A closure captures the value a `var` holds where the closure is declared,
# however the closure is called: directly, more than once, through another
# name, passed to another function, declared in a branch or inside another
# closure, and when the `var` holds a list that is later appended to.
main! = |args| {
	var $o = 10
	f = |x| x + $o
	g = f
	$o = 20
	echo!("${f(1).to_str()} ${f(2).to_str()} ${g(3).to_str()} ${Str.inspect([1, 2].map(f))}\n")

	var $items = [1, 2]
	count = |_| List.len($items)
	$items = $items.append(3)
	echo!("${count({}).to_str()} ${List.len($items).to_str()}\n")

	var $b = 1
	result = if List.len(args) > 100 {
		0
	} else {
		h = |x| x + $b
		$b = 5
		h(10)
	}
	echo!("${result.to_str()} ${$b.to_str()}\n")

	var $n = 1
	outer = |x| {
		inner = |y| y + $n
		inner(x)
	}
	$n = 7
	echo!("${outer(10).to_str()}\n")

	var $s = "a"
	first = |t| Str.concat(t, $s)
	before = first("x")
	$s = "b"
	second = |t| Str.concat(t, $s)
	echo!("${before} ${first("y")} ${second("z")}\n")
	Ok({})
}
