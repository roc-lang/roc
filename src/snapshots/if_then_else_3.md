# META
~~~ini
description=if_then_else (3)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else 2
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `bool` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
KwIf(1:1-1:3),LowerIdent(1:4-1:8),OpenCurly(1:9-1:10),Newline(1:1-1:1),
Int(2:2-2:3),Newline(1:1-1:1),
CloseCurly(3:1-3:2),KwElse(3:3-3:7),Int(3:8-3:9),EndOfFile(3:9-3:9),
~~~
# PARSE
~~~clojure
(e-if-then-else @1-1-3-9
	(e-ident @1-4-1-8 (qaul "") (raw "bool"))
	(e-block @1-9-3-2
		(statements
			(e-int @2-2-2-3 (raw "1"))))
	(e-int @3-8-3-9 (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if @1-1-3-9 (cond-var 0) (branch-var 0) (id 84)
	(if-branches
		(if-branch
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-block @1-9-3-2
				(e-int @2-2-2-3 (int-var 75) (precision-var 74) (literal "1") (value "TODO") (bound "u8")))))
	(if-else
		(e-int @3-8-3-9 (int-var 79) (precision-var 78) (literal "2") (value "TODO") (bound "u8"))))
~~~
# TYPES
~~~clojure
(expr (id 84) (type "*"))
~~~