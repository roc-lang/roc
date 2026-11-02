app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# Each `match check(...)` inlines `check`, so two `Err` paths select the same
# outer arm, which SpecConstr shares through a join. The match bound to `is_big`
# leaves that join in value position, so a literal written on one path reaches
# the `if` that follows, which must keep both of its arms for the other paths.
check : U64 -> Try(U64, [Bad])
check = |n| if n > 100 Err(Bad) else if n % 7 == 0 Err(Bad) else Ok(n)

first_found : U64 -> [Found(U64), Missing]
first_found = |n|
	match check(n) {
		Ok(v) => Found(v + 1000)
		Err(_) => match check(n + 1) {
			Ok(v) => Found(v + 2000)
			Err(_) => match check(n + 2) {
				Ok(v) => Found(v + 3000)
				Err(_) => Missing
			}
		}
	}

describe : U64 -> Str
describe = |n| {
	found = match first_found(n) {
		Found(v) => v.to_str()
		Missing => "missing".concat(n.to_str())
	}
	is_big = match check(n) {
		Ok(v) => v > 50
		Err(_) => n.to_str().count_utf8_bytes() > 3
	}
	if is_big "big ${found}" else "small ${found}"
}

main! = || {
	for n in [0, 1, 7, 60, 98, 101] {
		Stdout.line!(describe(n))
	}
}
