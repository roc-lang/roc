# META
~~~ini
description=if_then_else (5)
type=expr
~~~
# SOURCE
~~~roc
if bool { # Comment after then open
	A # Comment after expr
} else B
~~~
# EXPECTED
UNDEFINED VARIABLE - if_then_else_simple_comments_formatting.md:1:4:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

**if_then_else_simple_comments_formatting.md:1:4:1:8:**
```roc
if bool { # Comment after then open
```
   ^^^^


# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:8),OpenCurly(1:9-1:10),
UpperIdent(2:2-2:3),
CloseCurly(3:1-3:2),KwElse(3:3-3:7),UpperIdent(3:8-3:9),EndOfFile(3:9-3:9),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-3.9
	(e-ident @1.4-1.8 (raw "bool"))
	(e-block @1.9-3.2
		(statements
			(e-tag @2.2-2.3 (raw "A"))))
	(e-tag @3.8-3.9 (raw "B")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-3.9
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block @1.9-3.2
				(e-tag @2.2-2.3 (name "A")))))
	(if-else
		(e-tag @3.8-3.9 (name "B"))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.9 (type "[A, B]*"))
~~~
