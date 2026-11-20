# The operand before the anonymous-recursion error evaluates, for its effect,
# before the expect stops at that error; the operand after it never runs.
f = |n, m| if n < 4 { S(m) } else { m }

pick = |a, _b, _c| a

expect pick(dbg 41, f(0, Z), dbg 42) == 41
