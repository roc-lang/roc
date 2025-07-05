# META
~~~ini
description=list_closing_indent_not_enough
type=expr
~~~
# SOURCE
~~~roc
myList = [
    0,
    [
        a,
        b,
],
    1,
]
42
~~~
# EXPECTED
UNDEFINED VARIABLE - list_closing_indent_not_enough.md:1:1:1:7
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:7),OpAssign(1:8-1:9),OpenSquare(1:10-1:11),Newline(1:1-1:1),
Int(2:5-2:6),Comma(2:6-2:7),Newline(1:1-1:1),
OpenSquare(3:5-3:6),Newline(1:1-1:1),
LowerIdent(4:9-4:10),Comma(4:10-4:11),Newline(1:1-1:1),
LowerIdent(5:9-5:10),Comma(5:10-5:11),Newline(1:1-1:1),
CloseSquare(6:1-6:2),Comma(6:2-6:3),Newline(1:1-1:1),
Int(7:5-7:6),Comma(7:6-7:7),Newline(1:1-1:1),
CloseSquare(8:1-8:2),Newline(1:1-1:1),
Int(9:1-9:3),EndOfFile(9:3-9:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.7 (raw "myList"))
~~~
# FORMATTED
~~~roc
myList
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.7 (type "Error"))
~~~
