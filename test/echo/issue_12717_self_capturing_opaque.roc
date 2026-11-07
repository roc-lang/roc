# An opaque record whose closures capture the record itself (`step`) and its
# fields (`peek`). The commit boxes `step`'s capture of `Rec` while lowering
# boxes `peek`'s explicitly, so both captures have the same layout.
Rec :: { step : (U64 -> U64) -> U64, items : List(U64), peek : U64 -> U64 }.{
	next = |rec| {
		Rec.(r) = rec
		Rec.({ step: |f| Rec.run(rec, f), items: [], peek: |_| r.items.len() })
	}

	run = |Rec.(r), f| r.items.append((r.step)(f)).len()
}

start = Rec.({ step: |_| 0, items: [], peek: |_| 0 })

main! = |_args| {
	echo!(start.next().run(|n| n).to_str())
	Ok({})
}

expect start.next().run(|n| n) == 1
