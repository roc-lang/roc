# META
~~~ini
description=def_multistring_apply
type=expr
~~~
# SOURCE
~~~roc
e=""""""a
p
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `e` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:2-1:3),MultilineStringStart(1:3-1:6),StringPart(1:6-1:6),MultilineStringEnd(1:6-1:9),LowerIdent(1:9-1:10),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
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
