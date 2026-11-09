# `build` appends inside a counted loop and once more after it. The appends
# skip their capacity checks only when the list's spare capacity covers every
# append the rest of the loop can perform, including the one on the pass that
# leaves the loop, so a loop that never iterates still grows the empty list.
build = |n| {
	var $xs = []
	var $i = 0.U64
	while $i < n {
		$xs = $xs.append(0)
		$i = $i + 1
	}
	$xs.append(1)
}

expect {
	|n| {
		var $xs = []
		var $i = 0.U64
		while $i < n {
			$xs = $xs.append(0)
			$i = $i + 1
		}
		$xs.append(1)
	}
}(0) == [1]

main! = |_args| {
	echo!(Str.inspect(build(0)))
	echo!(Str.inspect(build(1)))
	echo!(Str.inspect(build(3)))
	Ok({})
}
