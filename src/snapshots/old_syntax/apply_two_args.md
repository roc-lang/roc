# META
~~~ini
description=apply_two_args
type=expr
~~~
# SOURCE
~~~roc
whee  12  34
~~~
# EXPECTED
UNDEFINED VARIABLE - apply_two_args.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `whee` in this scope.
Is there an `import` or `exposing` missing up-top?

**apply_two_args.md:1:1:1:5:**
```roc
whee  12  34
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:7-1:9),Int(1:11-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "whee"))
~~~
# FORMATTED
~~~roc
whee
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
