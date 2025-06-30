# META
~~~ini
description=tuple_access_after_ident
type=expr
~~~
# SOURCE
~~~roc
abc = (1, 2, 3)
abc.0
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `abc` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAssign(1:5-1:6),OpenRound(1:7-1:8),Int(1:8-1:9),Comma(1:9-1:10),Int(1:11-1:12),Comma(1:12-1:13),Int(1:14-1:15),CloseRound(1:15-1:16),Newline(1:1-1:1),
LowerIdent(2:1-2:4),NoSpaceDotInt(2:4-2:6),EndOfFile(2:6-2:6),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.4 (qaul "") (raw "abc"))
~~~
# FORMATTED
~~~roc
abc
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
