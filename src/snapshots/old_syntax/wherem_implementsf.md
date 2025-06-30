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
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `s` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),Newline(1:1-1:1),
LowerIdent(2:1-2:7),Newline(1:1-1:1),
LowerIdent(3:1-3:12),CloseRound(3:12-3:13),UpperIdent(3:13-3:14),Newline(1:1-1:1),
Underscore(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "s"))
~~~
# FORMATTED
~~~roc
s
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
