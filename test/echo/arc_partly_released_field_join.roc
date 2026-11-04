# Issue 12751: a record field released on one branch and read after the
# join. The record still owns its other fields at the jump, through a view the
# join body reads only after an outer join.

Field : { name : Str, value : Try(Str, I64) }

spell : Str, Bool -> Str
spell = |name, edge| if edge and name == "from" "in" else Str.concat(name, "!")

renamed : Iter(Field), Bool -> List(Field)
renamed = |fields, edge| {
	var $out = []
	for f in fields {
		$out = $out.append({ name: spell(f.name, edge), value: f.value })
	}
	$out
}

main! = |args| {
	list = [{ name: "from", value: Ok("a") }, { name: "x", value: Err(1) }]
	fields = if args.len() > 0 list.iter() else list.iter().map(|f| { ..f, value: Err(2) })
	echo!(Str.inspect(renamed(fields, args.len() > 1)))
	Ok({})
}
