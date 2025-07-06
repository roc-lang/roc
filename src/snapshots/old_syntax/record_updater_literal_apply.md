# META
~~~ini
description=record_updater_literal_apply
type=expr
~~~
# SOURCE
~~~roc
data =
    { x: 5, y: 0 }
        |> &y  3

data
~~~
# EXPECTED
UNDEFINED VARIABLE - record_updater_literal_apply.md:1:1:1:5
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `data` in this scope.
Is there an `import` or `exposing` missing up-top?

**record_updater_literal_apply.md:1:1:1:5:**
```roc
data =
```
^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpAssign(1:6-1:7),Newline(1:1-1:1),
OpenCurly(2:5-2:6),LowerIdent(2:7-2:8),OpColon(2:8-2:9),Int(2:10-2:11),Comma(2:11-2:12),LowerIdent(2:13-2:14),OpColon(2:14-2:15),Int(2:16-2:17),CloseCurly(2:18-2:19),Newline(1:1-1:1),
OpPizza(3:9-3:11),OpAmpersand(3:12-3:13),LowerIdent(3:13-3:14),Int(3:16-3:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:5),EndOfFile(5:5-5:5),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "data"))
~~~
# FORMATTED
~~~roc
data
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
