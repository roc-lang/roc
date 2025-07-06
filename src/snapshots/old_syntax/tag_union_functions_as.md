# META
~~~ini
description=tag_union_functions_as
type=expr
~~~
# SOURCE
~~~roc
main_for_host : [StdoutWrite Str ({} -> Op), StderrWrite Str ({} -> Op), Done] as Op
main_for_host = main
42
~~~
# EXPECTED
UNDEFINED VARIABLE - tag_union_functions_as.md:1:1:1:14
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `main_for_host` in this scope.
Is there an `import` or `exposing` missing up-top?

**tag_union_functions_as.md:1:1:1:14:**
```roc
main_for_host : [StdoutWrite Str ({} -> Op), StderrWrite Str ({} -> Op), Done] as Op
```
^^^^^^^^^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:14),OpColon(1:15-1:16),OpenSquare(1:17-1:18),UpperIdent(1:18-1:29),UpperIdent(1:30-1:33),OpenRound(1:34-1:35),OpenCurly(1:35-1:36),CloseCurly(1:36-1:37),OpArrow(1:38-1:40),UpperIdent(1:41-1:43),CloseRound(1:43-1:44),Comma(1:44-1:45),UpperIdent(1:46-1:57),UpperIdent(1:58-1:61),OpenRound(1:62-1:63),OpenCurly(1:63-1:64),CloseCurly(1:64-1:65),OpArrow(1:66-1:68),UpperIdent(1:69-1:71),CloseRound(1:71-1:72),Comma(1:72-1:73),UpperIdent(1:74-1:78),CloseSquare(1:78-1:79),KwAs(1:80-1:82),UpperIdent(1:83-1:85),Newline(1:1-1:1),
LowerIdent(2:1-2:14),OpAssign(2:15-2:16),LowerIdent(2:17-2:21),Newline(1:1-1:1),
Int(3:1-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.14 (raw "main_for_host"))
~~~
# FORMATTED
~~~roc
main_for_host
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.14 (type "Error"))
~~~
