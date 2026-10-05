# Conversion always rejects once this generic body is specialized, but
# evaluating a checked caller embeds that failure only on the failing branch.
RejectedBranches :: [].{
	Quoted(a) := { text : Str }.{
		from_quote : Str -> Try(Quoted(a), [BadQuotedBytes(Str)])
		from_quote = |_| Err(BadQuotedBytes("rejected branch literal"))
	}

	get : U64 -> Quoted(a)
	get = |n| if n == 0 Quoted.{ text: "valid" } else "rejected"
}
