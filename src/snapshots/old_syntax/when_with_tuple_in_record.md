# META
~~~ini
description=when_with_tuple_in_record
type=expr
~~~
# SOURCE
~~~roc
when {foo: (1, 2)} is
 {foo: (1, x)} -> x
 {foo: (_, b)} -> 3 + b
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpenCurly(1:6-1:7),LowerIdent(1:7-1:10),OpColon(1:10-1:11),OpenRound(1:12-1:13),Int(1:13-1:14),Comma(1:14-1:15),Int(1:16-1:17),CloseRound(1:17-1:18),CloseCurly(1:18-1:19),LowerIdent(1:20-1:22),Newline(1:1-1:1),
OpenCurly(2:2-2:3),LowerIdent(2:3-2:6),OpColon(2:6-2:7),OpenRound(2:8-2:9),Int(2:9-2:10),Comma(2:10-2:11),LowerIdent(2:12-2:13),CloseRound(2:13-2:14),CloseCurly(2:14-2:15),OpArrow(2:16-2:18),LowerIdent(2:19-2:20),Newline(1:1-1:1),
OpenCurly(3:2-3:3),LowerIdent(3:3-3:6),OpColon(3:6-3:7),OpenRound(3:8-3:9),Underscore(3:9-3:10),Comma(3:10-3:11),LowerIdent(3:12-3:13),CloseRound(3:13-3:14),CloseCurly(3:14-3:15),OpArrow(3:16-3:18),Int(3:19-3:20),OpPlus(3:21-3:22),LowerIdent(3:23-3:24),EndOfFile(3:24-3:24),
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
