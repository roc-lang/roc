# META
~~~ini
description=list_lots_of_spaces
type=expr
~~~
# SOURCE
~~~roc
[J
#
,

#
u]
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `u` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenSquare(1:1-1:2),UpperIdent(1:2-1:3),Newline(1:1-1:1),
Newline(2:2-2:2),
Comma(3:1-3:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(5:2-5:2),
LowerIdent(6:1-6:2),CloseSquare(6:2-6:3),EndOfFile(6:3-6:3),
~~~
# PARSE
~~~clojure
(e-list @1.1-6.3
	(e-tag @1.2-1.3 (raw "J"))
	(e-ident @6.1-6.2 (qaul "") (raw "u")))
~~~
# FORMATTED
~~~roc
[
	J,


	u,
]
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-6.3 (elem-var 74) (id 77)
	(elems
		(e-tag @1.2-1.3 (ext-var 73) (name "J") (args "TODO"))
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr (id 77) (type "List(Error)"))
~~~
