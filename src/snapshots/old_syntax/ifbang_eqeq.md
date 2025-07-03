# META
~~~ini
description=ifbang_eqeq fail
type=expr
~~~
# SOURCE
~~~roc
if!==9
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `if!` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpEquals(1:4-1:6),Int(1:6-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.7 (op "==")
	(e-ident @1.1-1.4 (qaul "") (raw "if!"))
	(e-int @1.6-1.7 (raw "9")))
~~~
# FORMATTED
~~~roc
if! == 9
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.7 (op "eq")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-int @1.6-1.7 (value "9")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.7 (type "a"))
~~~
