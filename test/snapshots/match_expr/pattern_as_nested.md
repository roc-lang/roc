# META
~~~ini
description=Nested as patterns with tuples and records
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, address: { city } as addr } as fullPerson => (fullPerson, addr, city)
    { name } as simplePerson => (simplePerson, name, "unknown")
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent CloseCurly KwAs LowerIdent CloseCurly KwAs LowerIdent OpFatArrow OpenRound LowerIdent Comma LowerIdent Comma LowerIdent CloseRound OpenCurly LowerIdent CloseCurly KwAs LowerIdent OpFatArrow OpenRound LowerIdent Comma LowerIdent Comma String CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(match <24 branches>)
~~~
# FORMATTED
~~~roc
when person is {
	{ name, (address: {
		city
	}) as addr }
	<malformed>
	fullPerson
	<malformed>((fullPerson, addr, city))
	{
		name
	}
	<malformed>
	simplePerson
	<malformed>((simplePerson, name, "unknown"))
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:14

**Parse Error**
at 2:41 to 2:41

**Parse Error**
at 2:55 to 2:55

**Parse Error**
at 3:14 to 3:14

**Parse Error**
at 3:30 to 3:30

**Parse Error**
at 1:1 to 4:2

**Parse Error**
at 4:2 to 4:2

**Unsupported Node**
at 1:14 to 4:1

**Unsupported Node**
at 4:2 to 4:2

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
