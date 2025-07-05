# META
~~~ini
description=when_in_function
type=expr
~~~
# SOURCE
~~~roc
func = \x -> when n is
              0 -> 0
42
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `func` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpAssign(1:6-1:7),OpBackslash(1:8-1:9),LowerIdent(1:9-1:10),OpArrow(1:11-1:13),LowerIdent(1:14-1:18),LowerIdent(1:19-1:20),LowerIdent(1:21-1:23),Newline(1:1-1:1),
Int(2:15-2:16),OpArrow(2:17-2:19),Int(2:20-2:21),Newline(1:1-1:1),
Int(3:1-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "func"))
~~~
# FORMATTED
~~~roc
func
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
