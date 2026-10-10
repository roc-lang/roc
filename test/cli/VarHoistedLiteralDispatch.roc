VarHoistedLiteralDispatch :: [].{}

# https://github.com/roc-lang/roc/issues/11777
two : {} -> I64
two = |{}| {
	var $n = 1 + 1
	$n
}

six : {} -> Dec
six = |{}| {
	var $n = 0
	$n = 2 * 3
	$n
}

expect two({}) == 2
expect six({}) == 6
