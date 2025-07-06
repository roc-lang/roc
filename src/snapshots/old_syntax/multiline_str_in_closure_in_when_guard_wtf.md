# META
~~~ini
description=multiline_str_in_closure_in_when_guard_wtf
type=expr
~~~
# SOURCE
~~~roc
when f
is
s if\t->""""""->e
~~~
# EXPECTED
UNDEFINED VARIABLE - multiline_str_in_closure_in_when_guard_wtf.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

**multiline_str_in_closure_in_when_guard_wtf.md:1:1:1:5:**
```roc
when f
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),Newline(1:1-1:1),
LowerIdent(2:1-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),KwIf(3:3-3:5),OpBackslash(3:5-3:6),LowerIdent(3:6-3:7),OpArrow(3:7-3:9),MultilineStringStart(3:9-3:12),StringPart(3:12-3:12),MultilineStringEnd(3:12-3:15),OpArrow(3:15-3:17),LowerIdent(3:17-3:18),EndOfFile(3:18-3:18),
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
