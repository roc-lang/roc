# META
~~~ini
description=Dot access expression
type=expr
~~~
# SOURCE
~~~roc
list.map(fn)
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `list` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `fn` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),NoSpaceDotLowerIdent(1:5-1:9),NoSpaceOpenRound(1:9-1:10),LowerIdent(1:10-1:12),CloseRound(1:12-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(e-field-access @1-1-1-13
	(e-ident @1-1-1-5 (qaul "") (raw "list"))
	(e-apply @1-5-1-13
		(e-ident @1-5-1-9 (qaul "") (raw ".map"))
		(e-ident @1-10-1-12 (qaul "") (raw "fn"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access @1-1-1-13 (field "map") (id 76)
	(receiver
		(e-runtime-error (tag "ident_not_in_scope")))
	(args
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr (id 76) (type "*"))
~~~