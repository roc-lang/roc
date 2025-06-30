# META
~~~ini
description=list_closing_same_indent_with_trailing_comma
type=expr
~~~
# SOURCE
~~~roc
myList = [
    0,
    1,
]
42
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `myList` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:7),OpAssign(1:8-1:9),OpenSquare(1:10-1:11),Newline(1:1-1:1),
Int(2:5-2:6),Comma(2:6-2:7),Newline(1:1-1:1),
Int(3:5-3:6),Comma(3:6-3:7),Newline(1:1-1:1),
CloseSquare(4:1-4:2),Newline(1:1-1:1),
Int(5:1-5:3),EndOfFile(5:3-5:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.7 (qaul "") (raw "myList"))
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
