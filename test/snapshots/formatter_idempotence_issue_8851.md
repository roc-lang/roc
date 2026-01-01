# META
~~~ini
description=Formatter idempotence test for issue 8851 - chained empty parens with static dispatch
type=snippet
~~~
# SOURCE
~~~roc
a = 0->b().c()
~~~
# EXPECTED
UNDEFINED VARIABLE - formatter_idempotence_issue_8851.md:1:8:1:9
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

**formatter_idempotence_issue_8851.md:1:8:1:9:**
```roc
a = 0->b().c()
```
       ^


# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpArrow,LowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-field-access
				(e-local-dispatch
					(e-int (raw "0"))
					(e-apply
						(e-ident (raw "b"))))
				(e-apply
					(e-ident (raw "c")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-dot-access (field "c")
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
