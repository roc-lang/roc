# META
~~~ini
description=try_plain_prefix
type=expr
~~~
# SOURCE
~~~roc
try  Str.toU64  "123"
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `try` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),UpperIdent(1:6-1:9),NoSpaceDotLowerIdent(1:9-1:15),StringStart(1:17-1:18),StringPart(1:18-1:21),StringEnd(1:21-1:22),EndOfFile(1:22-1:22),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.4 (qaul "") (raw "try"))
~~~
# FORMATTED
~~~roc
try
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
