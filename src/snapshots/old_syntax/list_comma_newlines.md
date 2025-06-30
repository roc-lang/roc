# META
~~~ini
description=list_comma_newlines
type=expr
~~~
# SOURCE
~~~roc
[s
,
]
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `s` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenSquare(1:1-1:2),LowerIdent(1:2-1:3),Newline(1:1-1:1),
Comma(2:1-2:2),Newline(1:1-1:1),
CloseSquare(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-list @1.1-3.2
	(e-ident @1.2-1.3 (qaul "") (raw "s")))
~~~
# FORMATTED
~~~roc
[
	s,
]
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-3.2 (elem-var 74) (id 75)
	(elems
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr (id 75) (type "List(Error)"))
~~~
