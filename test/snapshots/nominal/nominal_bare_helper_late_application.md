# META
~~~ini
description=A generalized bare constructor is related to distinct nominal applications and a structural row only at its uses
type=snippet
~~~
# SOURCE
~~~roc
Choice(a) := [None, Some(a), Message(Str)]
Other(a) := [Some(a), OtherEnd]

make_some = |value| Some(value)

first : Choice(U64)
first = make_some(42)

second : Other(Str)
second = make_some("text")

third : [Some(Bool)]
third = make_some(True)
~~~
# EXPECTED
NIL
