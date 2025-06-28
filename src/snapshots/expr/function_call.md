# META
~~~ini
description=Function call expression
type=expr
~~~
# SOURCE
~~~roc
add(5, 3)
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `add` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceOpenRound(1:4-1:5),Int(1:5-1:6),Comma(1:6-1:7),Int(1:8-1:9),CloseRound(1:9-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-apply @1-1-1-10
	(e-ident @1-1-1-4 (qaul "") (raw "add"))
	(e-int @1-5-1-6 (raw "5"))
	(e-int @1-8-1-9 (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1-1-1-10 (id 77)
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-int @1-5-1-6 (value "5"))
	(e-int @1-8-1-9 (value "3")))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "*"))
~~~
