# META
~~~ini
description=record_type_with_function
type=expr
~~~
# SOURCE
~~~roc
x : { init : {} -> Model, update : Model, Str -> Model, view : Model -> Str }

42
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),OpenCurly(1:5-1:6),LowerIdent(1:7-1:11),OpColon(1:12-1:13),OpenCurly(1:14-1:15),CloseCurly(1:15-1:16),OpArrow(1:17-1:19),UpperIdent(1:20-1:25),Comma(1:25-1:26),LowerIdent(1:27-1:33),OpColon(1:34-1:35),UpperIdent(1:36-1:41),Comma(1:41-1:42),UpperIdent(1:43-1:46),OpArrow(1:47-1:49),UpperIdent(1:50-1:55),Comma(1:55-1:56),LowerIdent(1:57-1:61),OpColon(1:62-1:63),UpperIdent(1:64-1:69),OpArrow(1:70-1:72),UpperIdent(1:73-1:76),CloseCurly(1:77-1:78),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(3:1-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "x"))
~~~
# FORMATTED
~~~roc
x
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
