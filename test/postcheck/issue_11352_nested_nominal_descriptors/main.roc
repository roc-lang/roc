# Run with: roc test main.roc --no-cache --specialize=no
import Wrap exposing [Wrap]

# repro for https://github.com/roc-lang/roc/issues/11352
retry : Try(Try(U64, [Unset]), [NoMatch]) -> Try(Try(U64, [Unset]), [NoMatch])
retry = |val| {
	match val {
		Err(NoMatch) => retry(Ok(Err(Unset)))
		Ok(prev) => Ok(prev)
	}
}

expect retry(Err(NoMatch)) == Ok(Err(Unset))

inner_ok : Try(U64, Try(U64, [Missing])) -> U64
inner_ok = |val| match val {
	Ok(n) => n
	Err(Ok(n)) => n + 1
	Err(Err(Missing)) => 0
}

expect inner_ok(Err(Ok(41))) == 42

concat_nested : Try(Try(Str, [Unset]), Str) -> Str
concat_nested = |val| match val {
	Err(e) => concat_nested(Ok(Ok(Str.concat(e, " followed by a suffix that is heap allocated"))))
	Ok(Ok(s)) => s
	Ok(Err(Unset)) => "unset"
}

expect concat_nested(Err("a long heap allocated string")) == "a long heap allocated string followed by a suffix that is heap allocated"

unwrap : Try(Try(a, e), e2), a -> a
unwrap = |t, fallback| match t {
	Ok(Ok(x)) => x
	_ => fallback
}

expect {
	t : Try(Try(Str, U8), U16)
	t = Ok(Ok("an inner string long enough to be heap allocated"))
	unwrap(t, "fallback") == "an inner string long enough to be heap allocated"
}

expect {
	t : Try(Try(Str, U8), U16)
	t = Ok(Err(3))
	unwrap(t, "fallback") == "fallback"
}

expect {
	t : Try(Try(U64, U64), U64)
	t = Ok(Err(3))
	Str.inspect(t) == "Ok(Err(3))"
}

same : Try(Try(U64, U64), U64), Try(Try(U64, U64), U64) -> Bool
same = |a, b| a == b

expect same(Ok(Err(3)), Ok(Err(3)))
expect !same(Ok(Err(3)), Ok(Ok(3)))

count : List(Try(Try(U64, U64), U64)) -> U64
count = |items| items.len()

expect count([Ok(Ok(1)), Err(2)]) == 2

wrapped : Wrap(Wrap(U64, Str), U64) -> U64
wrapped = |w| match w {
	Yes(Yes(n)) => n
	Yes(No(_)) => 1
	No(n) => n + 2
}

expect wrapped(Wrap.yes(Wrap.yes(7))) == 7
expect wrapped(Wrap.no(5)) == 7
