# META
~~~ini
description=try_plain_prefix
type=expr
~~~
# SOURCE
~~~roc
try  Str.toU64  "123"
~~~
# EXPECTED
UNDEFINED VARIABLE - try_plain_prefix.md:1:1:1:4
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `try` in this scope.
Is there an `import` or `exposing` missing up-top?

**try_plain_prefix.md:1:1:1:4:**
```roc
try  Str.toU64  "123"
```
^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:4),UpperIdent(1:6-1:9),NoSpaceDotLowerIdent(1:9-1:15),StringStart(1:17-1:18),StringPart(1:18-1:21),StringEnd(1:21-1:22),EndOfFile(1:22-1:22),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.4 (raw "try"))
~~~
# FORMATTED
~~~roc
try
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Error"))
~~~
