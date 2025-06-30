# META
~~~ini
description=ann_parens_where_implements_func
type=expr
~~~
# SOURCE
~~~roc
x:(a
where
e
implements K->Z)
s
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),Newline(1:1-1:1),
KwWhere(2:1-2:6),Newline(1:1-1:1),
LowerIdent(3:1-3:2),Newline(1:1-1:1),
KwImplements(4:1-4:11),UpperIdent(4:12-4:13),OpArrow(4:13-4:15),UpperIdent(4:15-4:16),CloseRound(4:16-4:17),Newline(1:1-1:1),
LowerIdent(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "x"))
~~~
# FORMATTED
~~~roc
x
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
