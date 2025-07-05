# META
~~~ini
description=annotated_tuple_destructure
type=expr
~~~
# SOURCE
~~~roc
( x, y ) : Foo
( x, y ) = ( "foo", 3.14 )

x
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**UNDEFINED VARIABLE**
Nothing is named `y` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
OpenRound(1:1-1:2),LowerIdent(1:3-1:4),Comma(1:4-1:5),LowerIdent(1:6-1:7),CloseRound(1:8-1:9),OpColon(1:10-1:11),UpperIdent(1:12-1:15),Newline(1:1-1:1),
OpenRound(2:1-2:2),LowerIdent(2:3-2:4),Comma(2:4-2:5),LowerIdent(2:6-2:7),CloseRound(2:8-2:9),OpAssign(2:10-2:11),OpenRound(2:12-2:13),StringStart(2:14-2:15),StringPart(2:15-2:18),StringEnd(2:18-2:19),Comma(2:19-2:20),Float(2:21-2:25),CloseRound(2:26-2:27),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.9
	(e-ident @1.3-1.4 (raw "x"))
	(e-ident @1.6-1.7 (raw "y")))
~~~
# FORMATTED
~~~roc
(x, y)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-1.9
	(elems
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-runtime-error (tag "ident_not_in_scope"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.9 (type "(Error, Error)"))
~~~
