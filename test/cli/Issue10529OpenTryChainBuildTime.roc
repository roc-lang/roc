app [main!] { pf: platform "../fx-open/platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/10529

take0 = |b| Ok({ val: b.get(0).map_err(|_| End)?, rest: b.drop_first(1) })
take1 = |b| Ok({ val: take0(b)?.val, rest: take0(b)?.rest })
take2 = |b| Ok({ val: take1(b)?.val, rest: take1(b)?.rest })
take3 = |b| Ok({ val: take2(b)?.val, rest: take2(b)?.rest })
take4 = |b| Ok({ val: take3(b)?.val, rest: take3(b)?.rest })
take5 = |b| Ok({ val: take4(b)?.val, rest: take4(b)?.rest })
take6 = |b| Ok({ val: take5(b)?.val, rest: take5(b)?.rest })
take7 = |b| Ok({ val: take6(b)?.val, rest: take6(b)?.rest })
take8 = |b| Ok({ val: take7(b)?.val, rest: take7(b)?.rest })
take9 = |b| Ok({ val: take8(b)?.val, rest: take8(b)?.rest })

main! : List(Str) => Try({}, [Exit(I32), ..])
main! = |_args| {
	_ = take9([1, 2, 3])
	Ok({})
}
