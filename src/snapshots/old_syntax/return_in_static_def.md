# META
~~~ini
description=return_in_static_def
type=expr
~~~
# SOURCE
~~~roc
staticValueDef =
    someVal =
        if 10 > 5 then
             x = 5
             return    x
        else
             6

    someVal + 2


staticValueDef
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `staticValueDef` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:15),OpAssign(1:16-1:17),Newline(1:1-1:1),
LowerIdent(2:5-2:12),OpAssign(2:13-2:14),Newline(1:1-1:1),
KwIf(3:9-3:11),Int(3:12-3:14),OpGreaterThan(3:15-3:16),Int(3:17-3:18),LowerIdent(3:19-3:23),Newline(1:1-1:1),
LowerIdent(4:14-4:15),OpAssign(4:16-4:17),Int(4:18-4:19),Newline(1:1-1:1),
KwReturn(5:14-5:20),LowerIdent(5:24-5:25),Newline(1:1-1:1),
KwElse(6:9-6:13),Newline(1:1-1:1),
Int(7:14-7:15),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(9:5-9:12),OpPlus(9:13-9:14),Int(9:15-9:16),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(12:1-12:15),EndOfFile(12:15-12:15),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.15 (qaul "") (raw "staticValueDef"))
~~~
# FORMATTED
~~~roc
staticValueDef
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
