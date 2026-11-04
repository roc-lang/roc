## A nominal over `Box` stored in a tag payload, a list item, a record field and
## a tuple. Its box is the nominal's own representation, so a descriptor of the
## container describes that one box, with no recursion box around it, and a
## generic body releases exactly the allocation the box made.
BoxyNominalBoxField := [].{}

Span :: { length : U64, start : U64 }.{
	make : U64, U64 -> Span
	make = |start, length| Span.{ start, length }

	start : Span -> U64
	start = |r| r.start

	length : Span -> U64
	length = |r| r.length
}

Boxed :: Box({ prop : U64, bytes : Span }).{
	make : U64, Span -> Boxed
	make = |prop, bytes| Boxed.(Box.box({ prop, bytes }))

	value : Boxed -> { prop : U64, bytes : Span }
	value = |Boxed.(boxed)| Box.unbox(boxed)
}

## Two nominals, with a refcounted payload the box's release must walk.
Named :: Boxed2.{
	make : Str -> Named
	make = |text| Named.(Boxed2.make(text))

	text : Named -> Str
	text = |Named.(inner)| Boxed2.text(inner)
}

Boxed2 :: Box({ text : Str }).{
	make : Str -> Boxed2
	make = |text| Boxed2.(Box.box({ text: text }))

	text : Boxed2 -> Str
	text = |Boxed2.(boxed)| Box.unbox(boxed).text
}

Evidence : [From, Override(U64), Sliced(Boxed)]

Run : { actual : Evidence, name : Str }

upd : List(Run) -> List(Run)
upd = |runs| {
	var $out = List.with_capacity(runs.len())
	for run in runs {
		$out = $out.append({ ..run, actual: Sliced(Boxed.make(1, Span.make(0, 1))) })
	}
	$out
}

starts : List(Run) -> List((U64, U64))
starts = |runs|
	runs.map(
		|run| match run.actual {
			Sliced(boxed) => {
				slice = Boxed.value(boxed)
				(slice.bytes.start(), slice.bytes.length())
			}
			_ => (99, 99)
		},
	)

## A generic body keeps the item alive past the list's own release.
last_of : List(a), a -> a
last_of = |items, fallback| {
	copy = items.map(|item| item)
	match copy.last() {
		Ok(item) => item
		Err(_) => fallback
	}
}

expect {
	runs = upd([{ actual: From, name: "a" }, { actual: Override(1), name: "b" }])
	starts(runs) == [(0, 1), (0, 1)]
}

expect starts([{ actual: From, name: "a" }]) == [(99, 99)]

expect {
	long = "a string long enough to be stored outside the box"
	items = [Named.make(long), Named.make("short")]
	Named.text(last_of(items, Named.make("fallback"))) == "short"
		and items.map(Named.text) == [long, "short"]
}

expect {
	pair = (Named.make("kept in a tuple"), [Boxed.make(7, Span.make(1, 2))])
	Named.text(pair.0) == "kept in a tuple" and pair.1.map(|b| Boxed.value(b).prop) == [7]
}
