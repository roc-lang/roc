# META
~~~ini
description=apply_closure_pizza
type=expr
~~~
# SOURCE
~~~roc
foo
|> Dict.keepIf \(k, _v) -> List.contains keysToDelete k |> Bool.not
~~~
# EXPECTED
UNDEFINED VARIABLE - apply_closure_pizza.md:1:1:1:4
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:4),Newline(1:1-1:1),
OpPizza(2:1-2:3),UpperIdent(2:4-2:8),NoSpaceDotLowerIdent(2:8-2:15),OpBackslash(2:16-2:17),NoSpaceOpenRound(2:17-2:18),LowerIdent(2:18-2:19),Comma(2:19-2:20),NamedUnderscore(2:21-2:23),CloseRound(2:23-2:24),OpArrow(2:25-2:27),UpperIdent(2:28-2:32),NoSpaceDotLowerIdent(2:32-2:41),LowerIdent(2:42-2:54),LowerIdent(2:55-2:56),OpPizza(2:57-2:59),UpperIdent(2:60-2:64),NoSpaceDotLowerIdent(2:64-2:68),EndOfFile(2:68-2:68),
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
