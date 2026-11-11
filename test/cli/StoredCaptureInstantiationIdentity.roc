StoredCaptureInstantiationIdentity :: [].{}

# Reusing restored capture witnesses must not merge distinct stored closure
# environments or lose type arguments hidden from the returned function.
Stored(a) := { run : {} -> a }.{
	make : a -> Stored(a)
	make = |value| { run: |{}| value }

	read : Stored(a) -> a
	read = |stored| (stored.run)({})

	map : Stored(a), (a -> b) -> Stored(b)
	map = |source, transform| { run: |{}| transform(source.read()) }
}

first : Stored(U64)
first = Stored.map(Stored.make(4.U64), |value| value + 1)

second : Stored(U64)
second = Stored.map(Stored.make(40.U64), |value| value + 1)

text : Stored(Str)
text = Stored.map(Stored.make("captured"), |value| Str.concat(value, "!"))

expect first.read() == 5
expect second.read() == 41
expect text.read() == "captured!"
expect first.read() + second.read() == 46
