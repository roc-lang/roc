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
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:8),OpenCurly(1:9-1:10),Newline(1:12-1:36),
UpperIdent(2:2-2:3),Newline(2:5-2:24),
CloseCurly(3:1-3:2),KwElse(3:3-3:7),UpperIdent(3:8-3:9),EndOfFile(3:9-3:9),
~~~
# PARSE
~~~clojure
(e-if-then-else @1-1-3-9
	(e-ident @1-4-1-8 (qaul "") (raw "bool"))
	(e-block @1-9-3-2
		(statements
			(e-tag @2-2-2-3 (raw "A"))))
	(e-tag @3-8-3-9 (raw "B")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1-1-3-9 (branch-var 81) (id 82)
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block @1-9-3-2
				(e-tag @2-2-2-3 (ext-var 0) (name "A") (args "TODO")))))
	(if-else
		(e-tag @3-8-3-9 (ext-var 0) (name "B") (args "TODO"))))
~~~
# TYPES
~~~clojure
(expr (id 82) (type "[A, B]*"))
~~~
