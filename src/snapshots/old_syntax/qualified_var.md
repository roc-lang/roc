# META
~~~ini
description=qualified_var
type=expr
~~~
# SOURCE
~~~roc
One.Two.whee
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `whee` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceDotUpperIdent(1:4-1:8),NoSpaceDotLowerIdent(1:8-1:13),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.13 (raw "One.Two.whee"))
~~~
# FORMATTED
~~~roc
One.whee
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.13 (type "Error"))
~~~
