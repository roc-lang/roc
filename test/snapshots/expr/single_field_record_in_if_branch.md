# META
~~~ini
description=Punned single-field record in an if branch matches an explicit record branch (issue #9723)
type=expr
~~~
# SOURCE
~~~roc
{
	items = [1, 2, 3]
	if True {
		{ items }
	} else {
		{ items: [4, 5, 6] }
	}
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - single_field_record_in_if_branch.md:3:5:3:9
# PROBLEMS

┌─────────────────────────┐
│ UNCONDITIONAL CONDITION ├─ This if condition is known at compile time, so ──┐
└┬────────────────────────┘  this conditional will always make the same       │
 │                           choice.                                          │
 │                                                                            │
 │  if True {                                                                 │
 │     ‾‾‾‾                                                                   │
 └─────────────────────────────────── single_field_record_in_if_branch.md:3:5 ┘


# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
KwIf,UpperIdent,OpenCurly,
OpenCurly,LowerIdent,CloseCurly,
CloseCurly,KwElse,OpenCurly,
OpenCurly,LowerIdent,OpColon,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,CloseCurly,
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
		(e-if-then-else
			(e-tag (raw "True"))
			(e-block
				(statements
					(e-record
						(field (field "items")))))
			(e-block
				(statements
					(e-record
						(field (field "items")
							(e-list
								(e-int (raw "4"))
								(e-int (raw "5"))
								(e-int (raw "6"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
	(e-if
		(if-branches
			(if-branch
				(e-tag (name "True"))
				(e-block
					(e-record
						(fields
							(field (name "items")
								(e-lookup-local
									(p-assign (ident "items")))))))))
		(if-else
			(e-block
				(e-record
					(fields
						(field (name "items")
							(e-list
								(elems
									(e-num (value "4"))
									(e-num (value "5"))
									(e-num (value "6")))))))))))
~~~
# TYPES
~~~clojure
(expr (type "{ items: List(Dec) }"))
~~~
