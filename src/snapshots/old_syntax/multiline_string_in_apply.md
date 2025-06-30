# META
~~~ini
description=multiline_string_in_apply
type=expr
~~~
# SOURCE
~~~roc
e""""\""""
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `e` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),MultilineStringStart(1:2-1:5),StringPart(1:5-1:8),MultilineStringEnd(1:8-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "e"))
~~~
# FORMATTED
~~~roc
e
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
