# META
~~~ini
description=where_clause_on_newline
type=expr
~~~
# SOURCE
~~~roc
f : a -> U64
    where a implements Hash

f
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),LowerIdent(1:5-1:6),OpArrow(1:7-1:9),UpperIdent(1:10-1:13),Newline(1:1-1:1),
KwWhere(2:5-2:10),LowerIdent(2:11-2:12),KwImplements(2:13-2:23),UpperIdent(2:24-2:28),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
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
