# META
~~~ini
description=record_type_tab fail
type=expr
~~~
# SOURCE
~~~roc
f : { foo 	 }
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),OpenCurly(1:5-1:6),LowerIdent(1:7-1:10),CloseCurly(1:13-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "f"))
~~~
# FORMATTED
~~~roc
f
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
