# META
~~~ini
description=Formatter idempotence test for issue 8851 comment 2 - chained empty parens with tuple dispatch
type=snippet
~~~
# SOURCE
~~~roc
a=()->b()()()
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - formatter_idempotence_issue_8851_comment2.md:1:3:1:5
NAME NOT IN SCOPE - formatter_idempotence_issue_8851_comment2.md:1:7:1:8
# PROBLEMS

┌─────────────────────────┐
│ EMPTY TUPLE NOT ALLOWED ├─ I am part way through parsing this tuple, but ───┐
└┬────────────────────────┘  it is empty.                                     │
 │                                                                            │
 │  a=()->b()()()                                                             │
 │    ‾‾                                                                      │
 └────────────────────────── formatter_idempotence_issue_8851_comment2.md:1:3 ┘

    If you want to represent nothing, try using an empty record: `{}`.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `b` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  a=()->b()()()                                                             │
 │        ‾                                                                   │
 └────────────────────────── formatter_idempotence_issue_8851_comment2.md:1:7 ┘

    Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,CloseRound,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceOpenRound,CloseRound,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-arrow-call
				(e-tuple)
				(e-apply
					(e-apply
						(e-apply
							(e-ident (raw "b")))))))))
~~~
# FORMATTED
~~~roc
a = ()->b()()()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-call
			(e-call
				(e-call
					(e-runtime-error (tag "ident_not_in_scope"))))
			(e-runtime-error (tag "empty_tuple")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
