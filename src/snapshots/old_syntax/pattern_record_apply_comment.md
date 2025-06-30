# META
~~~ini
description=pattern_record_apply_comment
type=expr
~~~
# SOURCE
~~~roc
s{t#
}:s
p#
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `s` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:3-1:4),Newline(1:5-1:5),
CloseCurly(2:1-2:2),OpColon(2:2-2:3),LowerIdent(2:3-2:4),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "s"))
~~~
# FORMATTED
~~~roc
s
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
