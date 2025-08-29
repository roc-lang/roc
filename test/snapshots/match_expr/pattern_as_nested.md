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
(match <2 branches>)
~~~
# FORMATTED
~~~roc
match person
	{
		{ name, address : {
			city
		} }
		as 
		addr
	} as fullPerson => (fullPerson, addr, city)
	{
		name
	} as simplePerson => (simplePerson, name, "unknown")
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:5 to 2:31

**Parse Error**
at 2:31 to 2:34

**Unsupported Node**
at 2:55 to 2:57

**Unsupported Node**
at 3:14 to 3:16

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
