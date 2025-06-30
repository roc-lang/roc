# META
~~~ini
description=list_pattern_not_terminated fail
type=expr
~~~
# SOURCE
~~~roc
when [] is
    [1, 2, -> ""
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpenSquare(1:6-1:7),CloseSquare(1:7-1:8),LowerIdent(1:9-1:11),Newline(1:1-1:1),
OpenSquare(2:5-2:6),Int(2:6-2:7),Comma(2:7-2:8),Int(2:9-2:10),Comma(2:10-2:11),OpArrow(2:12-2:14),StringStart(2:15-2:16),StringPart(2:16-2:16),StringEnd(2:16-2:17),EndOfFile(2:17-2:17),
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
