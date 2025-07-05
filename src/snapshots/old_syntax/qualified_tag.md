# META
~~~ini
description=qualified_tag malformed
type=expr
~~~
# SOURCE
~~~roc
One.Two.Whee
~~~
# EXPECTED
UNDEFINED VARIABLE - qualified_tag.md:1:4:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `Two` in this scope.
Is there an `import` or `exposing` missing up-top?

**qualified_tag.md:1:4:1:8:**
```roc
One.Two.Whee
```
   ^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceDotUpperIdent(1:4-1:8),NoSpaceDotUpperIdent(1:8-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.13 (raw "One.Two.Whee"))
~~~
# FORMATTED
~~~roc
Whee
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.4-1.8 (type "Error"))
~~~
