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
(match
  (scrutinee     (lc "person")
)
  (branch1     (binop_thick_arrow
      (binop_as
        (record_literal
          (lc "name")
          (binop_colon
            (lc "address")
            (binop_as
              (record_literal
                (lc "city")
              )
              (lc "addr")
            )
          )
        )
        (lc "fullPerson")
      )
      (block
        (tuple_literal
          (lc "fullPerson")
          (lc "addr")
          (lc "city")
        )
        (binop_thick_arrow
          (binop_as
            (block
              (lc "name")
            )
            (lc "simplePerson")
          )
          (tuple_literal
            (lc "simplePerson")
            (lc "name")
            (str_literal_big "unknown")
          )
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match person
	{name, address: {city} as addr} as fullPerson => 
		(fullPerson, addr, city)
		{
			name
		} as simplePerson => (simplePerson, name, "unknown")
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_as_nested.md:1:7:1:13
UNUSED VARIABLE - pattern_as_nested.md:2:7:2:11
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**pattern_as_nested.md:1:7:1:13:**
```roc
match person {
```
      ^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_as_nested.md:2:5:3:63:**
```roc
    { name, address: { city } as addr } as fullPerson => (fullPerson, addr, city)
    { name } as simplePerson => (simplePerson, name, "unknown")
```


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 28
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
~~~
# TYPES
~~~roc
~~~
