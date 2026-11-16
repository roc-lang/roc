app [main!] { pf: platform "./platform/main.roc" }

import pf.Host
import pf.Stdout

# Record literals whose field kinds nothing pins: no annotation names the
# record, no field is read where it is built, and the hosted `Box(a)` slot it
# is passed to accepts any type. Each one holds an iterator, which the host
# stores and hands back to a function that names the record's type and reads
# the iterator through it. The producers differ (a list, adapter chains short
# and long, a custom unfold), and one reader steps all of them.

digits! : Iter(U8) => U64
digits! = |items| {
	var $digits = 0
	for item in items {
		$digits = $digits * 10 + item.to_u64()
	}
	$digits
}

take_items! : () => U64
take_items! = || {
	taken : Box({ items : Iter(U8) })
	taken = Host.take_seed!()
	digits!(Box.unbox(taken).items)
}

take_tagged! : () => (U64, Str)
take_tagged! = || {
	taken : Box({ items : Iter(U8), tag : Str })
	taken = Host.take_seed!()
	state = Box.unbox(taken)
	(digits!(state.items), state.tag)
}

take_nested! : () => (U64, U8)
take_nested! = || {
	taken : Box({ outer : { items : Iter(U8) }, count : U8 })
	taken = Host.take_seed!()
	state = Box.unbox(taken)
	(digits!(state.outer.items), state.count)
}

take_wrapped! : () => U64
take_wrapped! = || {
	taken : Box([Wrap({ items : Iter(U8) })])
	taken = Host.take_seed!()
	match Box.unbox(taken) {
		Wrap(state) => digits!(state.items)
	}
}

take_list! : () => U64
take_list! = || {
	taken : Box(List({ items : Iter(U8) }))
	taken = Host.take_seed!()
	var $digits = 0
	for state in Box.unbox(taken) {
		$digits = $digits * 100 + digits!(state.items)
	}
	$digits
}

take_stream! : () => U64
take_stream! = || {
	taken : Box({ items : Stream(U8) })
	taken = Host.take_seed!()
	var $digits = 0
	for! item in Box.unbox(taken).items {
		$digits = $digits * 10 + item.to_u64()
	}
	$digits
}

take_nested_iterators! : () => U64
take_nested_iterators! = || {
	taken : Box({ items : Iter(Iter(U8)) })
	taken = Host.take_seed!()
	var $digits = 0
	for inner in Box.unbox(taken).items {
		$digits = $digits * 100 + digits!(inner)
	}
	$digits
}

keep : a -> a
keep = |value| value

main! = || {
	Host.store_seed!(Box.box({ items: [1.U8, 2, 3].iter() }))
	Stdout.line!("list literal: ${take_items!().to_str()}")

	Host.store_seed!(Box.box({ items: [1.U8, 2, 3].iter().map(|item| item + 1).keep_if(|item| item > 2) }))
	Stdout.line!("adapter chain: ${take_items!().to_str()}")

	state = { items: [4.U8, 5].iter() }
	Host.store_seed!(Box.box(state))
	Stdout.line!("bound record: ${take_items!().to_str()}")

	list = [6.U8, 7]
	Host.store_seed!(Box.box({ items: list.iter() }))
	Stdout.line!("bound list: ${take_items!().to_str()}")

	Host.store_seed!(Box.box(keep({ items: [8.U8, 9].iter() })))
	Stdout.line!("through a function: ${take_items!().to_str()}")

	Host.store_seed!(Box.box({ items: [1.U8, 2].iter(), tag: "tagged" }))
	tagged = take_tagged!()
	Stdout.line!("two fields: ${tagged.0.to_str()} ${tagged.1}")

	Host.store_seed!(Box.box({ outer: { items: [3.U8, 4].iter() }, count: 2.U8 }))
	nested = take_nested!()
	Stdout.line!("nested: ${nested.0.to_str()} ${nested.1.to_str()}")

	Host.store_seed!(Box.box(Wrap({ items: [5.U8, 6].iter() })))
	Stdout.line!("tag payload: ${take_wrapped!().to_str()}")

	Host.store_seed!(Box.box([{ items: [1.U8, 2].iter() }, { items: [3.U8].iter().map(|item| item + 1) }]))
	Stdout.line!("list of records: ${take_list!().to_str()}")

	Host.store_seed!(Box.box({ items: [7.U8, 8].iter().stream() }))
	Stdout.line!("stream: ${take_stream!().to_str()}")

	Host.store_seed!(Box.box({ items: Iter.custom(0.U8, Err(Unknown), |count| if count < 3 { Ok((count + 1, count + 1)) } else { Err(NoMore) }) }))
	Stdout.line!("custom: ${take_items!().to_str()}")

	Host.store_seed!(Box.box({ items: [0.U8, 1].iter().map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1).map(|item| item + 1) }))
	Stdout.line!("long chain: ${take_items!().to_str()}")

	Host.store_seed!(Box.box({ items: [[1.U8, 2].iter(), [3.U8].iter()].iter() }))
	Stdout.line!("nested iterators: ${take_nested_iterators!().to_str()}")
}
