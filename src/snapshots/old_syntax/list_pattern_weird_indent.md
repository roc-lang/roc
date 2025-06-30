# META
~~~ini
description=list_pattern_weird_indent
type=expr
~~~
# SOURCE
~~~roc
when [] is
    [1, 2,
3] -> ""
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpenSquare(1:6-1:7),CloseSquare(1:7-1:8),LowerIdent(1:9-1:11),Newline(1:1-1:1),
OpenSquare(2:5-2:6),Int(2:6-2:7),Comma(2:7-2:8),Int(2:9-2:10),Comma(2:10-2:11),Newline(1:1-1:1),
Int(3:1-3:2),CloseSquare(3:2-3:3),OpArrow(3:4-3:6),StringStart(3:7-3:8),StringPart(3:8-3:8),StringEnd(3:8-3:9),EndOfFile(3:9-3:9),
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
