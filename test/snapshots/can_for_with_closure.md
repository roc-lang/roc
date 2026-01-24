# META
~~~ini
description=Test for loop body that creates closures capturing loop variable
type=expr
~~~
# SOURCE
~~~roc
{
    items = [1, 2, 3]
    outer = 10
    for item in items {
        |_| item + outer
    }
}
~~~
# EXPECTED
TYPE MISMATCH - can_for_with_closure.md:4:23:6:6
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**can_for_with_closure.md:4:23:6:6:**
```roc
    for item in items {
        |_| item + outer
    }
```

It has the type:

    _arg -> a
      where [
        a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
        a.plus : a, b -> a,
        b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),
      ]

But I expected it to be:

    {}

# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
OpBar,Underscore,OpBar,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "items"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))
		(s-decl
			(p-ident (raw "outer"))
			(e-int (raw "10")))
		(s-for
			(p-ident (raw "item"))
			(e-ident (raw "items"))
			(e-block
				(statements
					(e-lambda
						(args
							(p-underscore))
						(e-binop (op "+")
							(e-ident (raw "item"))
							(e-ident (raw "outer")))))))))
~~~
# FORMATTED
~~~roc
{
	items = [1, 2, 3]
	outer = 10
	for item in items {
		|_| item + outer
	}
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "items"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3")))))
	(s-let
		(p-assign (ident "outer"))
		(e-num (value "10")))
	(s-for
		(p-assign (ident "item"))
		(e-lookup-local
			(p-assign (ident "items")))
		(e-block
			(e-closure
				(captures
					(capture (ident "item"))
					(capture (ident "outer")))
				(e-lambda
					(args
						(p-underscore))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "item")))
						(e-lookup-local
							(p-assign (ident "outer"))))))))
	(e-empty_record))
~~~
# TYPES
~~~clojure
(expr (type "{}"))
~~~
