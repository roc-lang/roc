# META
~~~ini
description=error_inline_alias_qualified fail
type=expr
~~~
# SOURCE
~~~roc
f : List elem -> [Nil, Cons elem a] as Module.LinkedList a
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),UpperIdent(1:5-1:9),LowerIdent(1:10-1:14),OpArrow(1:15-1:17),OpenSquare(1:18-1:19),UpperIdent(1:19-1:22),Comma(1:22-1:23),UpperIdent(1:24-1:28),LowerIdent(1:29-1:33),LowerIdent(1:34-1:35),CloseSquare(1:35-1:36),KwAs(1:37-1:39),UpperIdent(1:40-1:46),NoSpaceDotUpperIdent(1:46-1:57),LowerIdent(1:58-1:59),EndOfFile(1:59-1:59),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "f"))
~~~
# FORMATTED
~~~roc
f
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
