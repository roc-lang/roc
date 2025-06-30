# META
~~~ini
description=record_updater_var_apply
type=expr
~~~
# SOURCE
~~~roc
foo&bar  5
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAmpersand(1:4-1:5),LowerIdent(1:5-1:8),Int(1:10-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.4 (qaul "") (raw "foo"))
~~~
# FORMATTED
~~~roc
foo
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
