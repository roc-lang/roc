# META
~~~ini
description=multiline_string_in_apply
type=expr
~~~
# SOURCE
~~~roc
e""""\""""
~~~
# EXPECTED
UNDEFINED VARIABLE - multiline_string_in_apply.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `e` in this scope.
Is there an `import` or `exposing` missing up-top?

**multiline_string_in_apply.md:1:1:1:2:**
```roc
e""""\""""
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),MultilineStringStart(1:2-1:5),StringPart(1:5-1:8),MultilineStringEnd(1:8-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "e"))
~~~
# FORMATTED
~~~roc
e
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
