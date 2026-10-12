# The short-circuit branch constructs a Bool at a sealed loop-condition type.
# Its producer-owned tag row need not contain the other Bool constructor.
app [main!] { pf: platform "../fx/platform/main.roc" }

import pf.Stdin
import pf.Stdout

choose : Bool -> Bool
choose = |value| {
	var $pending = value
	while $pending and value {
		$pending = Bool.False
	}
	$pending
}

main! = || Stdout.line!(if choose(Str.is_empty(Stdin.line!())) "true" else "false")
