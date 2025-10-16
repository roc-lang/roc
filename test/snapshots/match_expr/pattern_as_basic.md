# META
~~~ini
description=Basic as pattern to bind both pattern and whole value
type=expr
~~~
# SOURCE
~~~roc
match (1, 2) {
    (x, y) as point => point
}
~~~
# EXPECTED
UNUSED VARIABLE - pattern_as_basic.md:2:6:2:7
UNUSED VARIABLE - pattern_as_basic.md:2:9:2:10
# PROBLEMS
**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**pattern_as_basic.md:2:6:2:7:**
```roc
    (x, y) as point => point
```
     ^


**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**pattern_as_basic.md:2:9:2:10:**
```roc
    (x, y) as point => point
```
        ^


# TOKENS
~~~zig
KwMatch,OpenRound,Int,Comma,Int,CloseRound,OpenCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,KwAs,LowerIdent,OpFatArrow,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-tuple
		(e-int (raw "1"))
		(e-int (raw "2")))
	(branches
		(branch
			(p-as (name "point")
				(p-tuple
					(p-ident (raw "x"))
					(p-ident (raw "y"))))
			(e-ident (raw "point")))))
~~~
# FORMATTED
~~~roc
match (1, 2) {
	(x, y) as point => point
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-tuple
				(elems
					(e-num (value "1"))
					(e-num (value "2")))))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-as (as "point")
							(p-tuple
								(patterns
									(p-assign (ident "x"))
									(p-assign (ident "y")))))))
				(value
					(e-lookup-local
						(p-as (as "point")
							(p-tuple
								(patterns
									(p-assign (ident "x"))
									(p-assign (ident "y")))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
