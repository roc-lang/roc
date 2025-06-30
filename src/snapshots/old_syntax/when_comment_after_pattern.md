# META
~~~ini
description=when_comment_after_pattern
type=expr
~~~
# SOURCE
~~~roc
when nns is
O#
 ->r
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:9),LowerIdent(1:10-1:12),Newline(1:1-1:1),
UpperIdent(2:1-2:2),Newline(2:3-2:3),
OpArrow(3:2-3:4),LowerIdent(3:4-3:5),EndOfFile(3:5-3:5),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (qaul "") (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
