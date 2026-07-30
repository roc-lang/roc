app [main!] { pf: platform "platform/main.roc" }

main! = || {
    c1 = 10.U64
    c2 = "hello"

	f1 = |x| x + c1
	f2 = |x| Str.concat(c2, Str.inspect(x))

	fns = (f1, f2)
	_ = fns
}
