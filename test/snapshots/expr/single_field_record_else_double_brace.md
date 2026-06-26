# META
~~~ini
description=An else branch of `{ { x } }` is a single-field record (issue #9723)
type=expr
~~~
# SOURCE
~~~roc
{
	x = 5
	if True {
		{ x }
	} else {
		{ x }
	}
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - single_field_record_else_double_brace.md:3:5:3:9
# PROBLEMS

┌─────────────────────────┐
│ UNCONDITIONAL CONDITION ├─ This if condition is known at compile time, so ──┐
└┬────────────────────────┘  this conditional will always make the same       │
 │                           choice.                                          │
 │                                                                            │
 │  if True {                                                                 │
 │     ‾‾‾‾                                                                   │
 └────────────────────────────── single_field_record_else_double_brace.md:3:5 ┘


# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
KwIf,UpperIdent,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,
CloseCurly,KwElse,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,
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
			(e-tag (raw "True"))
			(e-block
				(statements
					(e-record
						(field (field "x")))))
			(e-block
				(statements
					(e-record
						(field (field "x"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
				(e-tag (name "True"))
				(e-block
					(e-record
						(fields
							(field (name "x")
								(e-lookup-local
									(p-assign (ident "x")))))))))
		(if-else
			(e-block
				(e-record
					(fields
						(field (name "x")
							(e-lookup-local
								(p-assign (ident "x"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "{ x: Dec }"))
~~~
