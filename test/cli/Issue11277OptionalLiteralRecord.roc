# A custom numeral target with a closed optional slot must lower in both
# specialization strategies, including when conversion omits the slot.
Px := { n : U32, m ?: U32 }.{
	from_numeral : Numeral -> Try(Px, [InvalidNumeral(Str)])
	from_numeral = |numeral| {
		Inner : U32
		match Inner.from_numeral(numeral) {
			Ok(n) => Ok(Px.{ n })
			Err(err) => Err(err)
		}
	}
}

expect 380.Px.n == 380
expect 380.Px.?m == Err(MissingField)

# Supplying the optional field must preserve its payload as well as presence.
Direct := { n : U32, m ?: U32 }.{
	from_numeral : Numeral -> Try(Direct, [InvalidNumeral(Str)])
	from_numeral = |numeral| {
		match U32.from_numeral(numeral) {
			Ok(n) => Ok({ n, m: n + 1 })
			Err(err) => Err(err)
		}
	}
}

expect 380.Direct.n == 380
expect 380.Direct.?m == Ok(381)

# An annotation and a suffix must select the same custom conversion.
expect {
	px : Px
	px = 380
	px.n == 380 and px.?m == Err(MissingField)
}
expect {
	direct : Direct
	direct = 380
	direct.n == 380 and direct.?m == Ok(381)
}

# Quote conversion uses the same literal-target path, with owned payloads.
Quoted := { text : Str, extra ?: Str }.{
	from_quote : Str -> Try(Quoted, [BadQuotedBytes(Str)])
	from_quote = |text| Ok({ text: text })
}

QuotedPresent := { text : Str, extra ?: Str }.{
	from_quote : Str -> Try(QuotedPresent, [BadQuotedBytes(Str)])
	from_quote = |text| Ok({ text, extra: text })
}

expect "a quote longer than an inline string".Quoted.text == "a quote longer than an inline string"
expect "a quote longer than an inline string".Quoted.?extra == Err(MissingField)
expect "a quote longer than an inline string".QuotedPresent.text == "a quote longer than an inline string"
expect "a quote longer than an inline string".QuotedPresent.?extra == Ok("a quote longer than an inline string")
