Host := [].{
	## Named like the argument type a generator declares for `send!`.
	SendArgs := { text : Str }

	## Named like the result type a generator declares for `fetch!`.
	FetchRetRecord := { status : U16 }

	## Named like the name a generator prefers for the result of `load!`, and
	## like a helper named after that.
	LoadResult := { loaded : Str }

	LoadResultTag := { index : U8 }

	send! : Str, U64 => {}

	load! : Str => Try(Str, U64)

	fetch! : Str => { body : Str, code : U16 }
}
