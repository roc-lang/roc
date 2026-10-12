# Each literal below is rejected only after the names bound from it were
# checked: by the `Try` or tuple its pattern matches once that relation
# determines its type, through any chain of local aliases and destructures,
# or, for the loop's iterable, by the default its definition gives it. Each
# name bound from such a literal binds nothing, so the method calls on those
# names add no report of their own: each literal's rejection is the only
# report for its function, and running the program crashes only once one of
# them is reached.
from_str = |_x| {
	y = "abc"?
	Ok(y.to_str())
}

from_numeral = |_x| {
	y = 5?
	Ok(y.to_str())
}

from_match = |_x|
	match "abc" {
		Ok(y) => y.to_str()
		Err(_) => ""
	}

from_destructure = |_x| {
	(a, _b) = "abc"
	a.to_str()
}

from_local = |_x| {
	s = "abc"
	y = s?
	Ok(y.to_str())
}

from_alias_chain = |_x| {
	s = "abc"
	t = s
	y = t?
	Ok(y.to_str())
}

from_destructure_chain = |_x| {
	p = "abc"
	(a, _b) = p
	c = a
	c.to_str()
}

from_loop = |_x| {
	var $acc = ""
	for y in 5 {
		$acc = y.to_str()
	}
	$acc
}

main! = |args| {
	echo!("before")
	if List.len(args) > 100 {
		_ = from_numeral(1)
		echo!(from_match(1))
		echo!(from_destructure(1))
		_ = from_local(1)
		_ = from_alias_chain(1)
		echo!(from_destructure_chain(1))
		echo!(from_loop(1))
	}
	_ = from_str(1)
	echo!("after")
	Ok({})
}
