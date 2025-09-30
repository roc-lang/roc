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
KwMatch(1:1-1:6),OpenRound(1:7-1:8),Int(1:8-1:9),Comma(1:9-1:10),Int(1:11-1:12),CloseRound(1:12-1:13),OpenCurly(1:14-1:15),
OpenRound(2:5-2:6),LowerIdent(2:6-2:7),Comma(2:7-2:8),LowerIdent(2:9-2:10),CloseRound(2:10-2:11),KwAs(2:12-2:14),LowerIdent(2:15-2:20),OpFatArrow(2:21-2:23),LowerIdent(2:24-2:29),
CloseCurly(3:1-3:2),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(e-match
	(e-tuple @1.7-1.13
		(e-int @1.8-1.9 (raw "1"))
		(e-int @1.11-1.12 (raw "2")))
	(branches
		(branch @2.5-2.29
			(p-as @2.5-2.14 (name "point")
				(p-tuple @2.5-2.11
					(p-ident @2.6-2.7 (raw "x"))
					(p-ident @2.9-2.10 (raw "y"))))
			(e-ident @2.24-2.29 (raw "point")))))
~~~
# FORMATTED
~~~roc
match (1, 2) {
	(x, y) as point => point
}
~~~
# CANONICALIZE
~~~clojure
(e-match @1.1-3.2
	(match @1.1-3.2
		(cond
			(e-tuple @1.7-1.13
				(elems
					(e-num @1.8-1.9 (value "1"))
					(e-num @1.11-1.12 (value "2")))))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-as @2.5-2.14 (as "point")
							(p-tuple @2.5-2.11
								(patterns
									(p-assign @2.6-2.7 (ident "x"))
									(p-assign @2.9-2.10 (ident "y")))))))
				(value
					(e-lookup-local @2.24-2.29
						(p-as @2.5-2.14 (as "point")
							(p-tuple @2.5-2.11
								(patterns
									(p-assign @2.6-2.7 (ident "x"))
									(p-assign @2.9-2.10 (ident "y")))))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "Error"))
~~~
