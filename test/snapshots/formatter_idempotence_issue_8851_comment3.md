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
UNDEFINED VARIABLE - formatter_idempotence_issue_8851_comment3.md:1:6:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

**formatter_idempotence_issue_8851_comment3.md:1:6:1:7:**
```roc
a=0->b .c()
```
     ^


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
			(e-field-access
				(e-local-dispatch
					(e-int (raw "0"))
					(e-ident (raw "b")))
				(e-apply
					(e-ident (raw ".c")))))))
~~~
# FORMATTED
~~~roc
a = 0->b().c()
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
