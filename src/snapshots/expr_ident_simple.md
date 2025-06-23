# META
~~~ini
description=Simple identifier lookup canonicalization
type=expr
~~~
# SOURCE
~~~roc
foo
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(ident (1:1-1:4) "" "foo")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_runtime_error (1:1-1:4) "ident_not_in_scope")
~~~
# TYPES
~~~clojure
(expr 13 (type "Error"))
~~~