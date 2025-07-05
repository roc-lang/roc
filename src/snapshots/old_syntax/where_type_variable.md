# META
~~~ini
description=where_type_variable fail
type=expr
~~~
# SOURCE
~~~roc
role : Role where
role = Admin

role
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `role` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpColon(1:6-1:7),UpperIdent(1:8-1:12),KwWhere(1:13-1:18),Newline(1:1-1:1),
LowerIdent(2:1-2:5),OpAssign(2:6-2:7),UpperIdent(2:8-2:13),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:5),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "role"))
~~~
# FORMATTED
~~~roc
role
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
