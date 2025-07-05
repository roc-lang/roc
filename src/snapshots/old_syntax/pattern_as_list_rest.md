# META
~~~ini
description=pattern_as_list_rest
type=expr
~~~
# SOURCE
~~~roc
when myList is
    [first, .. as rest] -> 0
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:12),LowerIdent(1:13-1:15),Newline(1:1-1:1),
OpenSquare(2:5-2:6),LowerIdent(2:6-2:11),Comma(2:11-2:12),DoubleDot(2:13-2:15),KwAs(2:16-2:18),LowerIdent(2:19-2:23),CloseSquare(2:23-2:24),OpArrow(2:25-2:27),Int(2:28-2:29),EndOfFile(2:29-2:29),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "when"))
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
