# repro for https://github.com/roc-lang/roc/issues/12739
# Methods bound to other modules' functions dispatch to the procedure their
# alias chain reaches: a function with a where-clause, an alias of it in its
# own module, a type declared in a third module, and a type declared in a
# function body.
import Issue12739Lib
import Issue12739Mid

Rows := { n : U8 }.{
	label = |r| "rows ${r.n.to_str()}"
	to_inspect = Issue12739Lib.describe
	shown = Issue12739Lib.render
}

local_inspect = |n| {
	Local := { n : U8 }.{
		to_inspect = Issue12739Lib.plain
	}
	Str.inspect(Local.({ n: n }))
}

main! = |_args| {
	x = Rows.({ n: 1 })
	t = Issue12739Mid.make(7)
	echo!("${Str.inspect(x)} ${x.shown()} ${Str.inspect([x, x])} ${Str.inspect(t)} ${Str.inspect(t == t)} ${t.to_inspect()} ${local_inspect(3)}\n")
	Ok({})
}
