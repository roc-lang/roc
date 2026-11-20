# https://github.com/roc-lang/roc/issues/13255
# A parenthesized lambda bound by a block-local declaration is the same local
# function as the bare lambda, so it can call itself.

parenthesized = |n| {
	cf = (|y| if y > 10 y else cf(y + n))
	cf(n)
}

expect parenthesized(1) == 11

bare_lambda = |n| {
	cf = |y| if y > 10 y else cf(y + n)
	cf(n)
}

expect bare_lambda(1) == 11
