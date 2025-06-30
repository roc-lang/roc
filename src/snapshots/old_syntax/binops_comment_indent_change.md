# META
~~~ini
description=binops_comment_indent_change
type=expr
~~~
# SOURCE
~~~roc
r^
-f
#
 -P
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `r` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpCaret(1:2-1:3),Newline(1:1-1:1),
OpUnaryMinus(2:1-2:2),LowerIdent(2:2-2:3),Newline(1:1-1:1),
Newline(3:2-3:2),
OpUnaryMinus(4:2-4:3),UpperIdent(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "r"))
~~~
# FORMATTED
~~~roc
r
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
