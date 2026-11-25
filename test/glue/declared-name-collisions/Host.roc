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

	## Two functions whose names join to one host name in the generators that
	## drop the underscore.
	send_it! : Str, U64 => {}

	sendIt! : Str, U64 => {}

	## The platform declares a linker symbol named like this function's
	## argument type.
	ping! : U64, U64 => U64
}
