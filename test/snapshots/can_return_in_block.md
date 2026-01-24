# META
~~~ini
description=Test return statement in block
type=expr
~~~
# SOURCE
~~~roc
{
    x = 5
    if x > 10 {
        return 0
    } else {
        x
    }
}
~~~
# EXPECTED
RETURN OUTSIDE FUNCTION - can_return_in_block.md:4:9:4:17
# PROBLEMS
**RETURN OUTSIDE FUNCTION**
The `return` keyword can only be used inside function bodies.

**can_return_in_block.md:4:9:4:17:**
```roc
        return 0
```
        ^^^^^^^^


# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
KwIf,LowerIdent,OpGreaterThan,Int,OpenCurly,
KwReturn,Int,
CloseCurly,KwElse,OpenCurly,
LowerIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(e-if-then-else
			(e-binop (op ">")
				(e-ident (raw "x"))
				(e-int (raw "10")))
			(e-block
				(statements
					(s-return
						(e-int (raw "0")))))
			(e-block
				(statements
					(e-ident (raw "x")))))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	if x > 10 {
		return 0
	} else {
		x
	}
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(e-if
		(if-branches
			(if-branch
				(e-binop (op "gt")
					(e-lookup-local
						(p-assign (ident "x")))
					(e-num (value "10")))
				(e-block
					(e-runtime-error (tag "return_outside_fn")))))
		(if-else
			(e-block
				(e-lookup-local
					(p-assign (ident "x")))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
