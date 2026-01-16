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
NIL
# PROBLEMS
NIL
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
					(e-return
						(e-num (value "0"))))))
		(if-else
			(e-block
				(e-lookup-local
					(p-assign (ident "x")))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_gt : a, a -> Bool]"))
~~~
