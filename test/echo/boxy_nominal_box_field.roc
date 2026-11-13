# A nominal over `Box` stored in a tag payload inside a list item, in a running
# program (`test/cli/BoxyNominalBoxField.roc` covers `roc test`). The loop's
# iterator holds the list in a generic closure, whose release walks the items
# through their descriptor.
Boxed :: Box({ prop : U64, other : U64 }).{
	make : U64 -> Boxed
	make = |value| Boxed.(Box.box({ prop: value, other: 4096 }))

	prop : Boxed -> U64
	prop = |Boxed.(boxed)| Box.unbox(boxed).prop
}

Run : { actual : [From, Sliced(Boxed)], name : Str }

sliced : List(Run) -> List(Run)
sliced = |runs| {
	var $out = List.with_capacity(runs.len())
	for run in runs {
		$out = $out.append({ ..run, actual: Sliced(Boxed.make(run.name.count_utf8_bytes())) })
	}
	$out
}

props : List(Run) -> List(U64)
props = |runs|
	runs.map(
		|run| match run.actual {
			Sliced(boxed) => Boxed.prop(boxed)
			From => 0
		},
	)

main! = |args| {
	first = "a".repeat(args.len())
	runs = sliced([{ actual: From, name: first }, { actual: From, name: "${first}b" }])
	echo!(Str.inspect(props(runs)))
	Ok({})
}
