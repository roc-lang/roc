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
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:8),OpenCurly(1:9-1:10),Newline(1:12-1:36),
UpperIdent(2:2-2:3),Newline(2:5-2:24),
CloseCurly(3:1-3:2),KwElse(3:3-3:7),UpperIdent(3:8-3:9),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-if-then-else @1.1-1.1
	(e-ident @1.4-1.8 (raw "bool"))
	(e-block @1.9-3.2
		(statements
			(e-tag @2.2-2.3 (raw "A"))))
	(e-tag @3.8-3.9 (raw "B")))
~~~
# FORMATTED
~~~roc
if bool { # Comment after then open
	A # Comment after expr
} else B
~~~
# CANONICALIZE
~~~clojure
(e-if @1.1-1.1
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
(expr @1.1-1.1 (type "[A, B]*"))
~~~
