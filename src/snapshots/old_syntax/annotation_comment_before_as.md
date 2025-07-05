# META
~~~ini
description=annotation_comment_before_as
type=expr
~~~
# SOURCE
~~~roc
e:A#
  as H
n
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `e` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),Newline(1:5-1:5),
KwAs(2:3-2:5),UpperIdent(2:6-2:7),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "e"))
~~~
# FORMATTED
~~~roc
e
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
