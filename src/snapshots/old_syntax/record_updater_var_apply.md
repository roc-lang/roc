# META
~~~ini
description=record_updater_var_apply
type=expr
~~~
# SOURCE
~~~roc
foo&bar  5
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAmpersand(1:4-1:5),LowerIdent(1:5-1:8),Int(1:10-1:11),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.4 (raw "foo"))
~~~
# FORMATTED
~~~roc
foo
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Error"))
~~~
