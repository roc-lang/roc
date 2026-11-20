SealedNominalResultRows := {}

import SealedNominalShape

# A function whose result is a named type over a tag row is a static worker
# under `--specialize=no`, for `Try` and for a user type of the same shape:
# each caller converts the result to the row it uses it at.

Outcome(good, bad) := [Good(good), Bad(bad)]

Wrapped(e) := [Wrapped(Try(Str, e)), Bare]

parse_try : Str -> Try(Str, [Empty, TooLong(Str)])
parse_try = |text|
    if text == "" Err(Empty) else if text.count_utf8_bytes() > 3 Err(TooLong(text)) else Ok(text)

parse_outcome : Str -> Outcome(Str, [Empty, TooLong(Str)])
parse_outcome = |text|
    if text == "" Outcome.Bad(Empty) else if text.count_utf8_bytes() > 3 Outcome.Bad(TooLong(text)) else Outcome.Good(text)

pick_try : Str -> Try([Short(Str), Blank], Str)
pick_try = |text| if text == "" Ok(Blank) else if text == "!" Err("bang") else Ok(Short(text))

pick_outcome : Str -> Outcome([Short(Str), Blank], Str)
pick_outcome = |text| if text == "" Outcome.Good(Blank) else if text == "!" Outcome.Bad("bang") else Outcome.Good(Short(text))

wrap : Str -> Wrapped([Empty, TooLong(Str)])
wrap = |text| if text == "bare" Wrapped.Bare else Wrapped.Wrapped(parse_try(text))

# A type declared in another module seals the same way.
parse_shape : Str -> SealedNominalShape(Str, [Empty, TooLong(Str)])
parse_shape = |text|
    if text == "" SealedNominalShape.Bad(Empty) else if text.count_utf8_bytes() > 3 SealedNominalShape.Bad(TooLong(text)) else SealedNominalShape.Good(text)

# Direct calls at a wider closed row.

show_try : Try(Str, [Empty, TooLong(Str), Other(U64)]) -> Str
show_try = |r| match r {
    Ok(s) => "ok ${s}"
    Err(Empty) => "empty"
    Err(TooLong(s)) => "too long ${s}"
    Err(Other(n)) => "other ${n.to_str()}"
}

show_outcome : Outcome(Str, [Empty, TooLong(Str), Other(U64)]) -> Str
show_outcome = |r| match r {
    Good(s) => "ok ${s}"
    Bad(Empty) => "empty"
    Bad(TooLong(s)) => "too long ${s}"
    Bad(Other(n)) => "other ${n.to_str()}"
}

via_try : Str, Bool -> Str
via_try = |text, other| show_try(if other Err(Other(7)) else parse_try(text))

via_outcome : Str, Bool -> Str
via_outcome = |text, other| show_outcome(if other Outcome.Bad(Other(7)) else parse_outcome(text))

expect via_try("", Bool.False) == "empty"
expect via_try("abcdef", Bool.False) == "too long abcdef"
expect via_try("ab", Bool.False) == "ok ab"
expect via_try("ab", Bool.True) == "other 7"
expect via_outcome("", Bool.False) == "empty"
expect via_outcome("abcdef", Bool.False) == "too long abcdef"
expect via_outcome("ab", Bool.False) == "ok ab"
expect via_outcome("ab", Bool.True) == "other 7"

show_shape : SealedNominalShape(Str, [Empty, TooLong(Str), Other(U64)]) -> Str
show_shape = |r| match r {
    Good(s) => "ok ${s}"
    Bad(Empty) => "empty"
    Bad(TooLong(s)) => "too long ${s}"
    Bad(Other(n)) => "other ${n.to_str()}"
}

via_shape : Str, Bool -> Str
via_shape = |text, other| show_shape(if other SealedNominalShape.Bad(Other(7)) else parse_shape(text))

expect via_shape("", Bool.False) == "empty"
expect via_shape("abcdef", Bool.False) == "too long abcdef"
expect via_shape("ab", Bool.False) == "ok ab"
expect via_shape("ab", Bool.True) == "other 7"

# The ok row widens the same way.

show_pick_try : Try([Short(Str), Blank, Long(Str)], Str) -> Str
show_pick_try = |r| match r {
    Ok(Short(s)) => "short ${s}"
    Ok(Blank) => "blank"
    Ok(Long(s)) => "long ${s}"
    Err(e) => "err ${e}"
}

show_pick_outcome : Outcome([Short(Str), Blank, Long(Str)], Str) -> Str
show_pick_outcome = |r| match r {
    Good(Short(s)) => "short ${s}"
    Good(Blank) => "blank"
    Good(Long(s)) => "long ${s}"
    Bad(e) => "err ${e}"
}

expect show_pick_try(if Bool.False Ok(Long("x")) else pick_try("hi")) == "short hi"
expect show_pick_try(if Bool.False Ok(Long("x")) else pick_try("")) == "blank"
expect show_pick_try(if Bool.False Ok(Long("x")) else pick_try("!")) == "err bang"
expect show_pick_outcome(if Bool.False Outcome.Good(Long("x")) else pick_outcome("hi")) == "short hi"
expect show_pick_outcome(if Bool.False Outcome.Good(Long("x")) else pick_outcome("")) == "blank"
expect show_pick_outcome(if Bool.False Outcome.Good(Long("x")) else pick_outcome("!")) == "err bang"

# A row a declaration hands on to `Try`.

show_wrapped : Wrapped([Empty, TooLong(Str), Other(U64)]) -> Str
show_wrapped = |w| match w {
    Wrapped(inner) => "wrapped ${show_try(inner)}"
    Bare => "bare"
}

expect show_wrapped(if Bool.False Wrapped.Wrapped(Err(Other(1))) else wrap("")) == "wrapped empty"
expect show_wrapped(if Bool.False Wrapped.Wrapped(Err(Other(1))) else wrap("abcdef")) == "wrapped too long abcdef"
expect show_wrapped(if Bool.False Wrapped.Wrapped(Err(Other(1))) else wrap("bare")) == "bare"
expect show_wrapped(if Bool.True Wrapped.Wrapped(Err(Other(1))) else wrap("bare")) == "wrapped other 1"

# A caller whose own row is a type variable re-tags the result by name.

or_else : Outcome(Str, [Empty, TooLong(Str), ..rest]), [Empty, TooLong(Str), ..rest] -> Outcome(Str, [Empty, TooLong(Str), ..rest])
or_else = |first, fallback| match first {
    Good(s) => Outcome.Good(s)
    Bad(Empty) => Outcome.Bad(fallback)
    Bad(other) => Outcome.Bad(other)
}

generic_outcome : Str, [Empty, TooLong(Str), ..rest] -> Outcome(Str, [Empty, TooLong(Str), ..rest])
generic_outcome = |text, fallback| or_else(parse_outcome(text), fallback)

generic_try : Str, [Empty, TooLong(Str), ..rest] -> Try(Str, [Empty, TooLong(Str), ..rest])
generic_try = |text, fallback| match parse_try(text) {
    Ok(s) => Ok(s)
    Err(Empty) => Err(fallback)
    Err(other) => Err(other)
}

expect show_outcome(generic_outcome("", Other(3))) == "other 3"
expect show_outcome(generic_outcome("abcdef", Other(3))) == "too long abcdef"
expect show_outcome(generic_outcome("ab", Other(3))) == "ok ab"
expect show_try(generic_try("", Other(3))) == "other 3"
expect show_try(generic_try("abcdef", Other(3))) == "too long abcdef"

# The function as a value, used at a wider result.

apply_outcome : (Str -> Outcome(Str, [Empty, TooLong(Str), Other(U64)])), Str -> Str
apply_outcome = |f, text| show_outcome(f(text))

apply_try : (Str -> Try(Str, [Empty, TooLong(Str), Other(U64)])), Str -> Str
apply_try = |f, text| show_try(f(text))

expect apply_outcome(parse_outcome, "") == "empty"
expect apply_outcome(parse_outcome, "abcdef") == "too long abcdef"
expect apply_outcome(|_| Outcome.Bad(Other(9)), "") == "other 9"
expect apply_try(parse_try, "abcdef") == "too long abcdef"
expect apply_try(|_| Err(Other(9)), "") == "other 9"

# A list of results keeps every item at the list's row.

all_outcomes : List(Outcome(Str, [Empty, TooLong(Str), Other(U64)]))
all_outcomes = [parse_outcome(""), Outcome.Bad(Other(2)), parse_outcome("abcdef"), parse_outcome("ok")]

expect all_outcomes.map(show_outcome) == ["empty", "other 2", "too long abcdef", "ok ok"]

# A `where` method whose result is the named type, served by each kind of
# implementation.

Reader := [Reader(Str)].{
    read : Reader -> Outcome(Str, [Empty, TooLong(Str)])
    read = |Reader(text)| parse_outcome(text)
}

read_open : a -> Str where [a.read : a -> Outcome(Str, [Empty, TooLong(Str), ..])]
read_open = |x| match x.read() {
    Good(s) => "ok ${s}"
    Bad(Empty) => "empty"
    Bad(TooLong(s)) => "too long ${s}"
    Bad(_) => "something else"
}

expect read_open(Reader.Reader("")) == "empty"
expect read_open(Reader.Reader("abcdef")) == "too long abcdef"
expect read_open(Reader.Reader("ab")) == "ok ab"
