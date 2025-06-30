# META
~~~ini
description=try_pipe_suffix
type=expr
~~~
# SOURCE
~~~roc
Str.toU64 "123"|> try
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `toU64` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceDotLowerIdent(1:4-1:10),StringStart(1:11-1:12),StringPart(1:12-1:15),StringEnd(1:15-1:16),OpPizza(1:16-1:18),LowerIdent(1:19-1:22),EndOfFile(1:22-1:22),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.10 (qaul "Str") (raw ".toU64"))
~~~
# FORMATTED
~~~roc
Str.toU64
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.10 (type "Error"))
~~~
