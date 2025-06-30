# META
~~~ini
description=tuple_destructure_bang
type=expr
~~~
# SOURCE
~~~roc
(launchTheNukes!, code) = config

launchTheNukes! code
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `launchTheNukes!` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `code` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:2-1:17),Comma(1:17-1:18),LowerIdent(1:19-1:23),CloseRound(1:23-1:24),OpAssign(1:25-1:26),LowerIdent(1:27-1:33),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:16),LowerIdent(3:17-3:21),EndOfFile(3:21-3:21),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.24
	(e-ident @1.2-1.17 (qaul "") (raw "launchTheNukes!"))
	(e-ident @1.19-1.23 (qaul "") (raw "code")))
~~~
# FORMATTED
~~~roc
(launchTheNukes!, code)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-1.24 (id 77)
	(elems
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "(Error, Error)"))
~~~
