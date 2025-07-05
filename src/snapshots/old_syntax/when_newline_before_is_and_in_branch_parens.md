# META
~~~ini
description=when_newline_before_is_and_in_branch_parens
type=expr
~~~
# SOURCE
~~~roc
when 0
is B->(
t)
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:6-1:7),Newline(1:1-1:1),
LowerIdent(2:1-2:3),UpperIdent(2:4-2:5),OpArrow(2:5-2:7),NoSpaceOpenRound(2:7-2:8),Newline(1:1-1:1),
LowerIdent(3:1-3:2),CloseRound(3:2-3:3),EndOfFile(3:3-3:3),
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
