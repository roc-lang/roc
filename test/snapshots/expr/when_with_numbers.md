# META
~~~ini
description=When is old syntax use match instead (should error)
type=expr
~~~
# SOURCE
~~~roc
when x is
 1 -> 2
 3 -> 4
~~~
# EXPECTED
UNDEFINED VARIABLE - when_with_numbers.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

**when_with_numbers.md:1:1:1:5:**
```roc
when x is
```
^^^^


# TOKENS
~~~zig
LowerIdent,LowerIdent,LowerIdent,
Int,OpArrow,Int,
Int,OpArrow,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-ident (raw "when"))
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
(expr (type "Error"))
~~~
