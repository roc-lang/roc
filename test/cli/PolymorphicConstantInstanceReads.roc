# Every read of a polymorphic constant names the instance it reads: a
# dispatch to a method bound to a polymorphic callable binding that reads
# another polymorphic constant, a name a destructuring binds read at two
# types, and an instance whose variable no use fixes, compared with `==`.
PolymorphicConstantInstanceReads := [W].{
    wrap = mk(empty)
}

empty : List(_a)
empty = []

mk : List(a) -> (PolymorphicConstantInstanceReads, a -> List(a))
mk = |items| |_w, x| List.append(items, x)

w : PolymorphicConstantInstanceReads
w = W

id = |x| x

pair = (id, id)

destructured = {
    (first, second) = pair
    (first(1.U8), first("x"), second(2.U16))
}

busy : Try(Str, _)
busy = Ok("busy")

failure = |_cause| busy

expect w.wrap(1.U8) == [1]
expect w.wrap("s") == ["s"]
expect destructured == (1, "x", 2)
expect failure(Oops) == Ok("busy")
