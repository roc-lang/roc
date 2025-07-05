# META
~~~ini
description=list_pattern_weird_rest_pattern fail
type=expr
~~~
# SOURCE
~~~roc
when [] is
    [...] -> ""
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpenSquare(1:6-1:7),CloseSquare(1:7-1:8),LowerIdent(1:9-1:11),Newline(1:1-1:1),
OpenSquare(2:5-2:6),TripleDot(2:6-2:9),CloseSquare(2:9-2:10),OpArrow(2:11-2:13),StringStart(2:14-2:15),StringPart(2:15-2:15),StringEnd(2:15-2:16),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
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
