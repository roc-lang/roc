# META
~~~ini
description=when_over_indented_int fail
type=expr
~~~
# SOURCE
~~~roc
when 4 is
    5 -> Num.neg
     2 -> 2
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
LowerIdent(1:1-1:5),Int(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
Int(2:5-2:6),OpArrow(2:7-2:9),UpperIdent(2:10-2:13),NoSpaceDotLowerIdent(2:13-2:17),Newline(1:1-1:1),
Int(3:6-3:7),OpArrow(3:8-3:10),Int(3:11-3:12),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
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
