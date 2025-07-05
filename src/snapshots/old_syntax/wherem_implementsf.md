# META
~~~ini
description=wherem_implementsf fail
type=expr
~~~
# SOURCE
~~~roc
s:(s
wherem
implementsF)A
_
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `s` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),Newline(1:1-1:1),
LowerIdent(2:1-2:7),Newline(1:1-1:1),
LowerIdent(3:1-3:12),CloseRound(3:12-3:13),UpperIdent(3:13-3:14),Newline(1:1-1:1),
Underscore(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "s"))
~~~
# FORMATTED
~~~roc
s
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
