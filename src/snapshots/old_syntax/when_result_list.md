# META
~~~ini
description=when_result_list
type=expr
~~~
# SOURCE
~~~roc
when Ok [] is
    Ok [] -> {}
    _ -> {}
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),UpperIdent(1:6-1:8),OpenSquare(1:9-1:10),CloseSquare(1:10-1:11),LowerIdent(1:12-1:14),Newline(1:1-1:1),
UpperIdent(2:5-2:7),OpenSquare(2:8-2:9),CloseSquare(2:9-2:10),OpArrow(2:11-2:13),OpenCurly(2:14-2:15),CloseCurly(2:15-2:16),Newline(1:1-1:1),
Underscore(3:5-3:6),OpArrow(3:7-3:9),OpenCurly(3:10-3:11),CloseCurly(3:11-3:12),EndOfFile(3:12-3:12),
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
