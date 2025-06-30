# META
~~~ini
description=pattern_as
type=expr
~~~
# SOURCE
~~~roc
when 0 is
    _ as n -> n
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
Underscore(2:5-2:6),KwAs(2:7-2:9),LowerIdent(2:10-2:11),OpArrow(2:12-2:14),LowerIdent(2:15-2:16),EndOfFile(2:16-2:16),
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
