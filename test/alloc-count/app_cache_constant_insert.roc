app [run!] { pf: platform "./platform/main.roc" }

# Repro for https://github.com/roc-lang/roc/issues/12241: inserting into a
# top-level constant, and appending to a list unboxed from one, allocates the
# same number of times whether or not the app's modules were served from the
# object cache.

import pf.Host
import PieceTable

Document : { table : PieceTable, lines : List(U64) }

document : Document
document = { table: PieceTable.from_bytes([1, 2, 3]), lines: [0] }

Holder : { items : Box(List({ a : U64, b : U64 })), step : U64 }

holder : Holder
holder = { items: Box.box([{ a: 1, b: 2 }]), step: 5 }

grow : Holder, U64 -> U64
grow = |h, count| {
	var $items = Box.unbox(h.items)
	var $index = 0
	while $index < count {
		$items = $items.append({ a: $index, b: h.step })
		$index = $index + 1
	}
	$items.len()
}

insert : Document, U64 -> Document
insert = |doc, count| {
	var $doc = doc
	var $index = 0
	while $index < count {
		$doc = { ..$doc, table: PieceTable.insert($doc.table, 0, [7]) }
		$index = $index + 1
	}
	$doc
}

run! : Str => Str
run! = |input| {
	count = Str.count_utf8_bytes(input).to_u64() / 16
	before = Host.alloc_count!()
	inserted = insert(document, count)
	allocs = Host.alloc_count!() - before
	before_grow = Host.alloc_count!()
	grown = grow(holder, count)
	grow_allocs = Host.alloc_count!() - before_grow

	"pieces: ${inserted.table.len().to_str()}, insert allocations: ${allocs.to_str()}, items: ${grown.to_str()}, grow allocations: ${grow_allocs.to_str()}"
}
