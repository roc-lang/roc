import Needs

Show := [].{
	show = |x| Str.inspect(x)

	wrap = |x| Str.inspect(Needs.{ v: x })

	wrap_ann : a -> Str
	wrap_ann = |x| Str.inspect(Needs.{ v: x })

	nested = |x| show({ items: [Needs.{ v: x }] })
}
