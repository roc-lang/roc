Right := [].{
	apply : Str -> Str
	apply = |suffix| (saved.run)(suffix)
}

make = |captured, step| { run: |suffix| step(captured, suffix) }
base = "stored"
saved = make(base, |a, b| Str.concat(a, b))

expect Right.apply("!") == "stored!"
