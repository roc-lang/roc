# META
~~~ini
description=outdented_record
type=expr
~~~
# SOURCE
~~~roc
x = foo {
  bar: blah
}
x
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),LowerIdent(1:5-1:8),OpenCurly(1:9-1:10),Newline(1:1-1:1),
LowerIdent(2:3-2:6),OpColon(2:6-2:7),LowerIdent(2:8-2:12),Newline(1:1-1:1),
CloseCurly(3:1-3:2),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "x"))
~~~
# FORMATTED
~~~roc
x
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
