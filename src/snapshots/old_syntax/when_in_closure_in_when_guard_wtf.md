# META
~~~ini
description=when_in_closure_in_when_guard_wtf
type=expr
~~~
# SOURCE
~~~roc
when f
is
s if\t->when 0
is z->f
 z->m
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),Newline(1:1-1:1),
LowerIdent(2:1-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),KwIf(3:3-3:5),OpBackslash(3:5-3:6),LowerIdent(3:6-3:7),OpArrow(3:7-3:9),LowerIdent(3:9-3:13),Int(3:14-3:15),Newline(1:1-1:1),
LowerIdent(4:1-4:3),LowerIdent(4:4-4:5),OpArrow(4:5-4:7),LowerIdent(4:7-4:8),Newline(1:1-1:1),
LowerIdent(5:2-5:3),OpArrow(5:3-5:5),LowerIdent(5:5-5:6),EndOfFile(5:6-5:6),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (qaul "") (raw "when"))
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
