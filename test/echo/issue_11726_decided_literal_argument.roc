# Regression for https://github.com/roc-lang/roc/issues/11726
#
# The literal in `make` and in `pair` converts at a type over the function's
# own variable, so it is a literal requirement of that function. Each call
# below instantiates the variable at one that no scheme quantifies and
# nothing constrains. Such a variable is decided, so the call's instantiation
# is closed: the literal is converted once, at compile time, at the type the
# variable seals to, under either strategy.
Sql(a) := { text : Str }.{
	from_quote : Str -> Try(Sql(a), [BadQuotedBytes(Str)])
	from_quote = |raw|
		if raw == "bad" {
			Err(BadQuotedBytes("rejected"))
		} else {
			Ok(Sql.{ text: raw.repeat(2) })
		}

	shout : Sql(a) -> Str
	shout = |sql| "${sql.text}!"
}

make : Str -> Sql(a)
make = |_| "good"

# The requirement's type also names `b`, which `item`'s own scheme
# quantifies: `item` forwards the requirement, and `item`'s caller, which
# fixes `b`, converts the literal.
Both(a, b) := { text : Str }.{
	from_quote : Str -> Try(Both(a, b), [BadQuotedBytes(Str)])
	from_quote = |raw| Ok(Both.{ text: raw.repeat(3) })

	label : b, Both(a, b) -> Str
	label = |_, pair| pair.text
}

both : Str -> Both(a, b)
both = |_| "both"

item : b -> Str
item = |witness| Both.label(witness, both("x"))

main! = |args| {
	_made = make(args.len().to_str())
	echo!("${make("x").shout()}\n")
	echo!("${item(1.U8)}\n")
	echo!("ran\n")
	Ok({})
}
