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
UNDEFINED VARIABLE - formatter_idempotence_issue_8851_comment2.md:1:7:1:8
# PROBLEMS
**EMPTY TUPLE NOT ALLOWED**
I am part way through parsing this tuple, but it is empty:
**formatter_idempotence_issue_8851_comment2.md:1:3:1:5:**
```roc
a=()->b()()()
```
  ^^

If you want to represent nothing, try using an empty record: `{}`.

**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

**formatter_idempotence_issue_8851_comment2.md:1:7:1:8:**
```roc
a=()->b()()()
```
      ^


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
			(e-local-dispatch
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
