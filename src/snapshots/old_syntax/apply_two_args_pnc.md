# META
~~~ini
description=apply_two_args_pnc
type=expr
~~~
# SOURCE
~~~roc
whee(12,  34)
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `whee` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),Int(1:6-1:8),Comma(1:8-1:9),Int(1:11-1:13),CloseRound(1:13-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.14
	(e-ident @1.1-1.5 (raw "whee"))
	(e-int @1.6-1.8 (raw "12"))
	(e-int @1.11-1.13 (raw "34")))
~~~
# FORMATTED
~~~roc
whee(12, 34)
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.14
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-int @1.6-1.8 (value "12"))
	(e-int @1.11-1.13 (value "34")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.14 (type "*"))
~~~
