# META
~~~ini
description=opaque_reference_pattern_with_arguments
type=expr
~~~
# SOURCE
~~~roc
when n is
  @Add n m -> n + m
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
OpaqueName(2:3-2:7),LowerIdent(2:8-2:9),LowerIdent(2:10-2:11),OpArrow(2:12-2:14),LowerIdent(2:15-2:16),OpPlus(2:17-2:18),LowerIdent(2:19-2:20),EndOfFile(2:20-2:20),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
