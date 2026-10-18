# A match branch pattern that names an existing `var` is that var's own
# declaration pattern. A branch pattern that disagrees with the scrutinee is a
# type mismatch: checking reports it and retires the match without making the
# var erroneous, so `roc check` and `roc` report the mismatches instead of
# crashing while lowering the var's declaration, and the independent work
# before the rejected code still runs.

after_other_branch : [A, B] -> I32
after_other_branch = |tag| {
	var $count = 0.I32
	match tag {
		A => {}
		$count => {}
	}
	$count
}

only_branch : [A, B] -> I32
only_branch = |tag| {
	var $count = 0.I32
	match tag {
		$count => {}
	}
	$count
}

main! = |_args| {
	echo!("independent work")
	echo!(after_other_branch(A).to_str())
	echo!(only_branch(A).to_str())
	Ok({})
}
