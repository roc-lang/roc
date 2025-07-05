# META
~~~ini
description=malformed_pattern_module_name malformed
type=expr
~~~
# SOURCE
~~~roc
when x is
    Foo.val -> 1
    _ -> 4
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
UpperIdent(2:5-2:8),NoSpaceDotLowerIdent(2:8-2:12),OpArrow(2:13-2:15),Int(2:16-2:17),Newline(1:1-1:1),
Underscore(3:5-3:6),OpArrow(3:7-3:9),Int(3:10-3:11),EndOfFile(3:11-3:11),
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
