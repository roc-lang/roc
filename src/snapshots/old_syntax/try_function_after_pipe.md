# META
~~~ini
description=try_function_after_pipe
type=expr
~~~
# SOURCE
~~~roc
"123"    
       |> try Str.toU64
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:5),StringEnd(1:5-1:6),Newline(1:1-1:1),
OpPizza(2:8-2:10),LowerIdent(2:11-2:14),UpperIdent(2:15-2:18),NoSpaceDotLowerIdent(2:18-2:24),EndOfFile(2:24-2:24),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.6
	(e-string-part @1.2-1.5 (raw "123")))
~~~
# FORMATTED
~~~roc
"123"
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.6
	(e-literal @1.2-1.5 (string "123")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Str"))
~~~
