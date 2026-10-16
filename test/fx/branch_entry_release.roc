app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

# A value that one successor of a branch never reads is released where that
# successor begins, not when the enclosing function returns.
# The host counts how many boxes from `Host.boxed_add!` have been dropped.

dropped! : () => Str
dropped! = || if Host.boxed_drop_report!().starts_with("drops primitive=1") { "released" } else { "STILL ALIVE" }

unused_arm! : I64 => {}
unused_arm! = |n| {
	Host.reset_boxed_drop_report!()
	b = Host.boxed_add!(1)
	if n > 5 {
		f = Box.unbox(b)
		Stdout.line!("used ${f(1).to_str()}")
	} else {
		Stdout.line!("if arm: ${dropped!()}")
	}
}

list_loop! : () => {}
list_loop! = || {
	Host.reset_boxed_drop_report!()
	boxes = [Host.boxed_add!(1)]
	for _b in boxes {}
	Stdout.line!("list loop: ${dropped!()}")
}

while_body! : I64 => {}
while_body! = |limit| {
	Host.reset_boxed_drop_report!()
	b = Host.boxed_add!(1)
	var $i = 0
	while $i < limit {
		if $i == 0 {
			f = Box.unbox(b)
			$i = $i + f(0)
		} else {
			$i = $i + 1
		}
	}
	Stdout.line!("while body: ${dropped!()}")
}

main! = || {
	unused_arm!(1)
	list_loop!()
	while_body!(3)
}
