OpenRecordsAtUses :: [].{}

# A record nobody wrote a type for stays open after its function finishes.
# Each use of the function closes it, or makes it a nominal type whose record
# has exactly those fields and is stored the same way, wherever the record sits
# in the function's type.

Item := { key : U64, content : Str }.{
	show : Item -> Str
	show = |item| Str.concat(item.key.to_str(), item.content)
}

Other := { key : U64, content : Str }.{
	show : Other -> Str
	show = |other| Str.concat("other ", other.content)
}

Acc := { sum : U64 }.{
	double : Acc -> U64
	double = |acc| acc.sum * 2
}

Cfg := { name : Str, level : U8 ?? 3 }.{
	describe : Cfg -> Str
	describe = |cfg| Str.concat(cfg.name, cfg.level.to_str())
}

Wrap(a) := { inner : a }.{
	map : Wrap(a), (a -> b) -> Wrap(b)
	map = |w, f| { inner: f(w.inner) }
}

Holder(a) := { held : a }

make = |key| { key, content: "c" }

wrap_list = |key| [{ key, content: "l" }]

boxed = |key| Box.box({ key, content: "b" })

tried = |key| if key > 0 Ok({ key, content: "t" }) else Err(NoKey)

paired = |key| ({ key, content: "p" }, key)

apply = |f| f({ key: 5, content: "a" })

reader = |key| |content| { key, content }

makers = |key| [|content| { key, content }]

hold = |key| Holder.{ held: { key, content: "h" } }

twice = |key| [make(key), make(key + 1)]

# The receiver is unknown inside these, so the records sit in the types of
# their static-dispatch constraints.
build = |xs| xs.map(|x| { key: x, content: "m" })

total = |xs| xs.fold({ sum: 0 }, |acc, x| { sum: acc.sum + x })

with_defaults = |name| { name, level: 7 }

expect {
	item : Item
	item = make(1)
	item.show() == "1c"
}

expect {
	plain = make(2)
	plain.content == "c"
}

expect {
	item : Item
	item = make(3)
	other : Other
	other = make(4)
	plain = make(5)
	(item.show(), other.show(), plain.key) == ("3c", "other c", 5)
}

expect {
	items : List(Item)
	items = wrap_list(6)
	items.map(Item.show) == ["6l"]
}

expect {
	item : Box(Item)
	item = boxed(7)
	Box.unbox(item).show() == "7b"
}

expect {
	item : Try(Item, [NoKey])
	item = tried(8)
	item.map_ok(Item.show) == Ok("8t")
}

expect {
	pair : (Item, U64)
	pair = paired(9)
	pair.0.show() == "9p"
}

expect apply(Item.show) == "5a"

expect apply(|r| r.content) == "a"

expect {
	read : Str -> Item
	read = reader(10)
	read("r").show() == "10r"
}

expect {
	fs : List(Str -> Item)
	fs = makers(11)
	fs.map(|f| f("s").show()) == ["11s"]
}

expect {
	held : Holder(Item)
	held = hold(12)
	match held {
		{ held: item } => item.show() == "12h"
	}
}

expect {
	items : List(Item)
	items = twice(13)
	items.map(Item.show) == ["13c", "14c"]
}

expect {
	items : List(Item)
	items = build([1, 2])
	items.map(Item.show) == ["1m", "2m"]
}

expect {
	wrapped : Wrap(Item)
	wrapped = build(Wrap.{ inner: 9 })
	wrapped.inner.show() == "9m"
}

expect {
	acc : Acc
	acc = total([1, 2, 3])
	acc.double() == 12
}

expect total([4, 5]).sum == 9

expect {
	cfg : Cfg
	cfg = with_defaults("x")
	cfg.describe() == "x7"
}
