app [main!] {
	u: "./inspect_override_pkg/main.roc",
}

# A conditional `to_inspect` declared in one module, inspected through
# generic helpers declared in another, at types this module supplies. Each
# use decides the override for its own instance: `Needs(U64)` uses it,
# `Needs(NoStr)` renders its default form.
import u.Needs
import u.Show

NoStr := { x : U8 }

local_wrap = |x| Show.wrap(x)

main! = |_args| {
	n : NoStr
	n = NoStr.{ x: 2 }
	echo!("${Show.show(Needs.{ v: 1.U64 })} ${Show.show(Needs.{ v: n })}\n")
	echo!("${Show.wrap(2.U64)} ${Show.wrap(n)} ${Show.wrap_ann(3.I8)} ${Show.wrap_ann(n)}\n")
	echo!("${Show.nested(4.U16)} ${Show.nested(n)} ${local_wrap(5.U64)} ${local_wrap(Needs.{ v: 6.U64 })}\n")
	Ok({})
}
