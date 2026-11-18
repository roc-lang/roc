# https://github.com/roc-lang/roc/issues/12954
# Checking must not execute an annotated recursive effectful function.
main! = |_args| {
	forever!()
	Ok({})
}

forever! : () => {}
forever! = || {
	echo!("again\n")
	forever!()
}

with_unit! : {} => {}
with_unit! = |{}| {
	echo!("unit\n")
	with_unit!({})
}

ping! : () => {}
ping! = || {
	echo!("ping\n")
	pong!()
}

pong! : () => {}
pong! = || ping!()

unannotated! = || {
	echo!("inferred\n")
	_ = unannotated!()
	{}
}
