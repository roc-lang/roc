TopLevelOpenRowValues :: [].{}

# A top-level value whose type keeps an open tag row is monomorphic: every
# use shares that row's extension, which checking leaves at its default.
pick : Bool -> Try(U8, [Foo, ..e])
pick = |b| if b Ok(1) else Err(Foo)

wrap : Bool -> [Val(Try(U8, [Foo, ..e])), Other]
wrap = |b| Val(pick(b))

x : Try(U8, _)
x = pick(True)

y : Try(U8, _)
y = pick(False)

w = wrap(False)

pairs = { a: pick(False), b: 2 }

expect x == Ok(1)

expect x != Err(Bar)

expect y == Err(Foo)

expect y != Err(Bar)

expect Str.inspect(y) == "Err(Foo)"

expect w == Val(Err(Foo))

expect w != Other

expect pairs == { a: Err(Foo), b: 2 }

expect Str.inspect(w) == "Val(Err(Foo))"

expect {
	d = Dict.from_list([(pick(False), 1)])
	Dict.get(d, Err(Foo)) == Ok(1)
}
