# https://github.com/roc-lang/roc/issues/13255
# A parenthesized lambda bound by a block-local declaration is the same local
# function as the bare lambda, so a later local function or closure that calls
# it reaches the values it captures.

called_from_local_fn = |n| {
	cf = (|y| y + n)
	h = |y| cf(y)
	h(n)
}

expect called_from_local_fn(1) == 2

called_from_block_result = |n| {
	h = {
		cf = (|y| y + n)
		|y| cf(y)
	}
	h(n)
}

expect called_from_block_result(0) == 0
expect called_from_block_result(3) == 6

nested_parens = |n| {
	cf = ((|y| y + n))
	h = |y| cf(y)
	h(n)
}

expect nested_parens(2) == 4

bare_lambda = |n| {
	cf = |y| y + n
	h = |y| cf(y)
	h(n)
}

expect bare_lambda(1) == 2
