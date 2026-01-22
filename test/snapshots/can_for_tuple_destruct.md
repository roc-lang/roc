# META
~~~ini
description=Test for loop with tuple destructuring
type=expr
~~~
# SOURCE
~~~roc
{
    result = ""
    for (x, y) in [(1, "a"), (2, "b")] {
        result
    }
    result
}
~~~
# EXPECTED
UNUSED VARIABLE - can_for_tuple_destruct.md:3:10:3:11
UNUSED VARIABLE - can_for_tuple_destruct.md:3:13:3:14
TYPE MISMATCH - can_for_tuple_destruct.md:3:40:5:6
# PROBLEMS
**UNUSED VARIABLE**
Variable `x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_x` to suppress this warning.
The unused variable is declared here:
**can_for_tuple_destruct.md:3:10:3:11:**
```roc
    for (x, y) in [(1, "a"), (2, "b")] {
```
         ^


**UNUSED VARIABLE**
Variable `y` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_y` to suppress this warning.
The unused variable is declared here:
**can_for_tuple_destruct.md:3:13:3:14:**
```roc
    for (x, y) in [(1, "a"), (2, "b")] {
```
            ^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**can_for_tuple_destruct.md:3:40:5:6:**
```roc
    for (x, y) in [(1, "a"), (2, "b")] {
        result
    }
```

It has the type:

    Str

But I expected it to be:

    {}

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
KwFor,OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,KwIn,OpenSquare,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,Comma,OpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,CloseSquare,OpenCurly,
LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "result"))
			(e-string
				(e-string-part (raw ""))))
		(s-for
			(p-tuple
				(p-ident (raw "x"))
				(p-ident (raw "y")))
			(e-list
				(e-tuple
					(e-int (raw "1"))
					(e-string
						(e-string-part (raw "a"))))
				(e-tuple
					(e-int (raw "2"))
					(e-string
						(e-string-part (raw "b")))))
			(e-block
				(statements
					(e-ident (raw "result")))))
		(e-ident (raw "result"))))
~~~
# FORMATTED
~~~roc
{
	result = ""
	for (x, y) in [(1, "a"), (2, "b")] {
		result
	}
	result
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "result"))
		(e-string
			(e-literal (string ""))))
	(s-for
		(p-tuple
			(patterns
				(p-assign (ident "x"))
				(p-assign (ident "y"))))
		(e-list
			(elems
				(e-tuple
					(elems
						(e-num (value "1"))
						(e-string
							(e-literal (string "a")))))
				(e-tuple
					(elems
						(e-num (value "2"))
						(e-string
							(e-literal (string "b")))))))
		(e-block
			(e-lookup-local
				(p-assign (ident "result")))))
	(e-lookup-local
		(p-assign (ident "result"))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
