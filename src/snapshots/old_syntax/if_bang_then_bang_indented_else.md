# META
~~~ini
description=if_bang_then_bang_indented_else
type=expr
~~~
# SOURCE
~~~roc
 if!a!then
t
  else
 l
5
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `if!a!then` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:2-1:11),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
KwElse(3:3-3:7),Newline(1:1-1:1),
LowerIdent(4:2-4:3),Newline(1:1-1:1),
Int(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-ident @1.2-1.11 (qaul "") (raw "if!a!then"))
~~~
# FORMATTED
~~~roc
if!a!then
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.2-1.11 (type "Error"))
~~~
