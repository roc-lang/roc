# META
~~~ini
description=unicode_overflow_str
type=expr
~~~
# SOURCE
~~~roc
m"\u(FFFFFF)"s
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `m` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),StringStart(1:2-1:3),StringPart(1:3-1:13),StringEnd(1:13-1:14),LowerIdent(1:14-1:15),EndOfFile(1:15-1:15),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "m"))
~~~
# FORMATTED
~~~roc
m
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
