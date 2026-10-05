app [main!] { pf: platform "./platform/main.roc" }

import pf.Stdout

# A `while` loop over `level` from 0 through 12 calls `pick`, whose
# `level >= 10` test must take its true branch on the last three iterations,
# and the loop must stop at its bound.

main! = |_| {
	var $level = 0.U64
	while $level <= 12 {
		parser = pick($level) ?? 99
		Stdout.line!("${$level.to_str()}:${parser.to_str()}")
		$level = $level + 1
	}
	Ok({})
}

pick : U64 -> Try(U64, [Bug])
pick = |level| {
	limit = if level == 0 { 5000 } else if level * 4 >= 55 { 0 } else { 55 }
	if limit > 1000 {
		Ok(0)
	} else if level >= 10 {
		Ok(10)
	} else {
		Ok(2)
	}
}
