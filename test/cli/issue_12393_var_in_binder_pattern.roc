# A `match` branch pattern or a `for` pattern cannot bind a `var`: only an
# assignment with `=` changes a var. `roc check` and `roc` report each such
# binder, wherever it appears in the pattern and whether or not a `var` with
# that name is in scope, instead of crashing while lowering the var, and the
# independent work before the rejected code still runs.

whole_branch : [A, B] -> I32
whole_branch = |tag| {
	var $count = 0.I32
	match tag {
		A => {}
		$count => {}
	}
	$count
}

nested : Try(Str, Str) -> Str
nested = |result| {
	var $text = ""
	match result {
		Ok($text) | Err($text) => {}
	}
	$text
}

loop_binder : List(Str) -> Str
loop_binder = |items| {
	var $last = ""
	for $last in items {}
	$last
}

declared_in_loop : List(Str) -> Str
declared_in_loop = |items| {
	var $out = ""
	for (var $item) in items {
		$out = "item"
	}
	$out
}

unspelled_var : [A, B] -> I32
unspelled_var = |tag| {
	var count = 0.I32
	match tag {
		count => {}
	}
	count
}

no_var_in_scope : Str -> Str
no_var_in_scope = |text| match text {
	"${$word}!" => "exclaimed"
	_ => "plain"
}

main! = |_args| {
	echo!("independent work")
	echo!(whole_branch(A).to_str())
	echo!(nested(Ok("a")))
	echo!(loop_binder(["a"]))
	echo!(declared_in_loop(["a"]))
	echo!(unspelled_var(A).to_str())
	echo!(no_var_in_scope("a!"))
	Ok({})
}
