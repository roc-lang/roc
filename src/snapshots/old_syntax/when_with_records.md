# META
~~~ini
description=when_with_records
type=expr
~~~
# SOURCE
~~~roc
when x is
 { y } -> 2
 { z, w } -> 4
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
OpenCurly(2:2-2:3),LowerIdent(2:4-2:5),CloseCurly(2:6-2:7),OpArrow(2:8-2:10),Int(2:11-2:12),Newline(1:1-1:1),
OpenCurly(3:2-3:3),LowerIdent(3:4-3:5),Comma(3:5-3:6),LowerIdent(3:7-3:8),CloseCurly(3:9-3:10),OpArrow(3:11-3:13),Int(3:14-3:15),EndOfFile(3:15-3:15),
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
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
