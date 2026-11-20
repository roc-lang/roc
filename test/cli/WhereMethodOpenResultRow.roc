WhereMethodOpenResultRow := {}

# A `..` on a `where` method's result row accepts an implementing type whose
# method returns more tags than the row lists, written bare or named, on the
# method's own result row or inside a type argument. The body then sees a row
# with unknown other tags, so its `match` has a catch-all. Without `..` an
# implementation returns only the listed tags, and the body may match its
# copy of the row exhaustively or use it at a wider one.

Narrow := [N].{
    parse : Narrow -> [BadNum, Fine(Str)]
    parse = |_| Fine("narrow")

    check : Narrow -> Try(Str, [BadNum])
    check = |_| Err(BadNum)
}

Wide := [W(U64)].{
    parse : Wide -> [BadNum, Fine(Str), TooBig(Str)]
    parse = |W(n)| if n > 9 TooBig("${n.to_str()} is too big") else if n == 0 BadNum else Fine("wide ${n.to_str()}")

    check : Wide -> Try(Str, [BadNum, TooBig(Str)])
    check = |W(n)| if n > 9 Err(TooBig("${n.to_str()} is too big")) else if n == 0 Err(BadNum) else Ok("wide ${n.to_str()}")
}

exact : a -> Str where [a.parse : a -> [BadNum, Fine(Str)]]
exact = |x| match x.parse() {
    Fine(s) => "fine ${s}"
    BadNum => "bad number"
}

widened : a, Bool -> [BadNum, Fine(Str), Skipped] where [a.parse : a -> [BadNum, Fine(Str)]]
widened = |x, skip| if skip Skipped else x.parse()

bare : a -> Str where [a.parse : a -> [BadNum, Fine(Str), ..]]
bare = |x| match x.parse() {
    Fine(s) => "fine ${s}"
    BadNum => "bad number"
    _ => "something else"
}

named : a -> Str where [a.parse : a -> [BadNum, Fine(Str), ..others]]
named = |x| match x.parse() {
    Fine(s) => "fine ${s}"
    BadNum => "bad number"
    _ => "something else"
}

# The named form can hand the other tags back to the caller.
handed_back : a -> [BadNum, Fine(Str), ..others] where [a.parse : a -> [BadNum, Fine(Str), ..others]]
handed_back = |x| x.parse()

in_argument : a -> Str where [a.check : a -> Try(Str, [BadNum, ..])]
in_argument = |x| match x.check() {
    Ok(s) => "ok ${s}"
    Err(BadNum) => "bad number"
    Err(_) => "something else"
}

show : [BadNum, Fine(Str), TooBig(Str)] -> Str
show = |parsed| match parsed {
    Fine(s) => "fine ${s}"
    BadNum => "bad number"
    TooBig(message) => message
}

expect exact(Narrow.N) == "fine narrow"
expect widened(Narrow.N, Bool.False) == Fine("narrow")
expect widened(Narrow.N, Bool.True) == Skipped
expect bare(Narrow.N) == "fine narrow"
expect bare(Wide.W(3)) == "fine wide 3"
expect bare(Wide.W(0)) == "bad number"
expect bare(Wide.W(12)) == "something else"
expect named(Narrow.N) == "fine narrow"
expect named(Wide.W(3)) == "fine wide 3"
expect named(Wide.W(0)) == "bad number"
expect named(Wide.W(12)) == "something else"
expect show(handed_back(Wide.W(12))) == "12 is too big"
expect show(handed_back(Wide.W(3))) == "fine wide 3"
expect show(handed_back(Narrow.N)) == "fine narrow"
expect in_argument(Narrow.N) == "bad number"
expect in_argument(Wide.W(3)) == "ok wide 3"
expect in_argument(Wide.W(12)) == "something else"
