# SpecConstr shapes the speed pipeline rewrites: a call-pattern worker on a known constructor,
# a while loop whose tuple result is read only in part, and a case-of-case.
N := [Z, S(N)]

len : N, I64 -> I64
len = |n, acc| match n {
	Z => acc
	S(m) => len(m, acc + 1)
}

sum_to : I64 -> I64
sum_to = |n| {
	var $i = 0
	var $s = 0
	var $p = 1
	while $i < n {
		$s = $s + $i
		$p = $p + $p
		$i = $i + 1
	}
	$s
}

pick : N -> I64
pick = |x| {
	y = match x {
		Z => Bool.True
		S(Z) => Bool.True
		S(S(_)) => Bool.False
	}
	if y 10 else 20
}

expect len(S(S(S(Z))), 0) == 3
expect sum_to(10) == 45
expect sum_to(0) == 0
expect pick(S(Z)) + pick(S(S(Z))) == 30
