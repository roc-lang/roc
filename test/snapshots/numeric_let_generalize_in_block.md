# META
~~~ini
description=Numeric without let-generalization gives type error (only lambdas get let-generalization)
type=expr
~~~
# SOURCE
~~~roc
{
    n = 42
    a = I64.to_str(n)
    b = Dec.to_str(n)
    Str.concat(a, b)
}
~~~
# EXPECTED
TYPE MISMATCH - numeric_let_generalize_in_block.md:4:20:4:21
# PROBLEMS
**TYPE MISMATCH**
The first argument being passed to this function has the wrong type:
**numeric_let_generalize_in_block.md:4:20:4:21:**
```roc
    b = Dec.to_str(n)
```
                   ^

This argument has the type:

    I64

But the function needs the first argument to be:

    Dec

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "n"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "a"))
			(e-apply
				(e-ident (raw "I64.to_str"))
				(e-ident (raw "n"))))
		(s-decl
			(p-ident (raw "b"))
			(e-apply
				(e-ident (raw "Dec.to_str"))
				(e-ident (raw "n"))))
		(e-apply
			(e-ident (raw "Str.concat"))
			(e-ident (raw "a"))
			(e-ident (raw "b")))))
~~~
# FORMATTED
~~~roc
{
	n = 42
	a = I64.to_str(n)
	b = Dec.to_str(n)
	Str.concat(a, b)
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "n"))
		(e-num (value "42")))
	(s-let
		(p-assign (ident "a"))
		(e-call
			(e-lookup-external
				(builtin))
			(e-lookup-local
				(p-assign (ident "n")))))
	(s-let
		(p-assign (ident "b"))
		(e-call
			(e-lookup-external
				(builtin))
			(e-lookup-local
				(p-assign (ident "n")))))
	(e-call
		(e-lookup-external
			(builtin))
		(e-lookup-local
			(p-assign (ident "a")))
		(e-lookup-local
			(p-assign (ident "b")))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
