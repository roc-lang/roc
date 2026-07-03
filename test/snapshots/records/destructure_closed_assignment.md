# META
~~~ini
description=Record destructure assignment without `..` is closed: an extra field in the value is a type mismatch
type=snippet
~~~
# SOURCE
~~~roc
compute : U64
compute = {
    { x, y } = { x: 1, y: 2, z: 3 }
    x + y
}
~~~
# EXPECTED
TYPE MISMATCH - destructure_closed_assignment.md:3:16:3:36
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This expression is used in an unexpected way. ─────────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  { x, y } = { x: 1, y: 2, z: 3 }                                           │
 │             ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                           │
 └───────────────────────────────────── destructure_closed_assignment.md:3:16 ┘

    It has the type:

        { x: a, y: b, z: c }
          where [
            a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),
            b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),
            c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]),
          ]

    But you are trying to use it as:

        { x: _field, y: a }
          where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
    Hint: This pattern doesn't bind the `z` field. Match it explicitly with `z:
    _`, or add `..` to match all the remaining fields.

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "compute")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "compute"))
			(e-block
				(statements
					(s-decl
						(p-record
							(field (name "x") (rest false))
							(field (name "y") (rest false)))
						(e-record
							(field (field "x")
								(e-int (raw "1")))
							(field (field "y")
								(e-int (raw "2")))
							(field (field "z")
								(e-int (raw "3")))))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-ident (raw "y"))))))))
~~~
# FORMATTED
~~~roc
compute : U64
compute = {
	{ x, y } = { x: 1, y: 2, z: 3 }
	x + y
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "compute"))
		(e-block
			(s-let
				(p-record-destructure
					(destructs
						(record-destruct (label "x") (ident "x")
							(required
								(p-assign (ident "x"))))
						(record-destruct (label "y") (ident "y")
							(required
								(p-assign (ident "y"))))))
				(e-record
					(fields
						(field (name "x")
							(e-num (value "1")))
						(field (name "y")
							(e-num (value "2")))
						(field (name "z")
							(e-num (value "3"))))))
			(e-dispatch-call (method "plus") (constraint-fn-var 142)
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args
					(e-lookup-local
						(p-assign (ident "y"))))))
		(annotation
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
