# META
~~~ini
description=If expression with conditional
type=expr
~~~
# SOURCE
~~~roc
if x > 5 "big" else "small"
~~~
# EXPECTED
UNDEFINED VARIABLE - if_expression.md:1:4:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**if_expression.md:1:4:1:5:**
```roc
if x > 5 "big" else "small"
```
   ^


# TOKENS
~~~zig
KwIf,LowerIdent,OpGreaterThan,Int,StringStart,StringPart,StringEnd,KwElse,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-binop (op ">")
		(e-ident (raw "x"))
		(e-int (raw "5")))
	(e-string
		(e-string-part (raw "big")))
	(e-string
		(e-string-part (raw "small"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if
	(if-branches
		(if-branch
			(e-binop (op "gt")
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-num (value "5")))
			(e-string
				(e-literal (string "big")))))
	(if-else
		(e-string
			(e-literal (string "small")))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
