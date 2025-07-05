# META
~~~ini
description=function_with_tuple_ext_type
type=expr
~~~
# SOURCE
~~~roc
f : (Str)a -> (Str)a
f = \x -> x

f ("Str", 42)
~~~
# EXPECTED
UNDEFINED VARIABLE - function_with_tuple_ext_type.md:1:1:1:2
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `f` in this scope.
Is there an `import` or `exposing` missing up-top?

**function_with_tuple_ext_type.md:1:1:1:2:**
```roc
f : (Str)a -> (Str)a
```
^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),OpenRound(1:5-1:6),UpperIdent(1:6-1:9),CloseRound(1:9-1:10),LowerIdent(1:10-1:11),OpArrow(1:12-1:14),OpenRound(1:15-1:16),UpperIdent(1:16-1:19),CloseRound(1:19-1:20),LowerIdent(1:20-1:21),Newline(1:1-1:1),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),OpBackslash(2:5-2:6),LowerIdent(2:6-2:7),OpArrow(2:8-2:10),LowerIdent(2:11-2:12),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpenRound(4:3-4:4),StringStart(4:4-4:5),StringPart(4:5-4:8),StringEnd(4:8-4:9),Comma(4:9-4:10),Int(4:11-4:13),CloseRound(4:13-4:14),EndOfFile(4:14-4:14),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "f"))
~~~
# FORMATTED
~~~roc
f
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
