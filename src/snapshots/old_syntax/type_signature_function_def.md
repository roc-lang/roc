# META
~~~ini
description=type_signature_function_def
type=expr
~~~
# SOURCE
~~~roc
foo : Int, Float -> Bool
foo = \x, _ -> 42

42
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpColon(1:5-1:6),UpperIdent(1:7-1:10),Comma(1:10-1:11),UpperIdent(1:12-1:17),OpArrow(1:18-1:20),UpperIdent(1:21-1:25),Newline(1:1-1:1),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),OpBackslash(2:7-2:8),LowerIdent(2:8-2:9),Comma(2:9-2:10),Underscore(2:11-2:12),OpArrow(2:13-2:15),Int(2:16-2:18),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:1-4:3),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.4 (raw "foo"))
~~~
# FORMATTED
~~~roc
foo
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Error"))
~~~
