# META
~~~ini
description=when_with_function_application
type=expr
~~~
# SOURCE
~~~roc
when x is
    1 -> Num.neg
     2
    _ -> 4
~~~
# EXPECTED
UNDEFINED VARIABLE - when_with_function_application.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

**when_with_function_application.md:1:1:1:5:**
```roc
when x is
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
Int(2:5-2:6),OpArrow(2:7-2:9),UpperIdent(2:10-2:13),NoSpaceDotLowerIdent(2:13-2:17),Newline(1:1-1:1),
Int(3:6-3:7),Newline(1:1-1:1),
Underscore(4:5-4:6),OpArrow(4:7-4:9),Int(4:10-4:11),EndOfFile(4:11-4:11),
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
