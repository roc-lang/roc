# META
~~~ini
description=Formatter idempotence test for issue 8851 comment 3 - dispatch with space before field access
type=snippet
~~~
# SOURCE
~~~roc
a=0->b .c()
~~~
# EXPECTED
NAME NOT IN SCOPE - formatter_idempotence_issue_8851_comment3.md:1:6:1:7
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `b` in this scope. ───────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  a=0->b .c()                                                               │
 │       ‾                                                                    │
 └────────────────────────── formatter_idempotence_issue_8851_comment3.md:1:6 ┘

    Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpArrow,LowerIdent,DotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-method-call (method ".c")
				(receiver
					(e-arrow-call
						(e-int (raw "0"))
						(e-ident (raw "b"))))
				(args)))))
~~~
# FORMATTED
~~~roc
a = 0->b()
	.c()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-method-call (method "c")
			(receiver
				(e-call
					(e-runtime-error (tag "ident_not_in_scope"))
					(e-num (value "0"))))
			(args))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
