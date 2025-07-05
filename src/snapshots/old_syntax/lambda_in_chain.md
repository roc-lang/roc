# META
~~~ini
description=lambda_in_chain
type=expr
~~~
# SOURCE
~~~roc
"a string"
|> Str.toUtf8
|> List.map \byte -> byte + 1
|> List.reverse
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
StringStart(1:1-1:2),StringPart(1:2-1:10),StringEnd(1:10-1:11),Newline(1:1-1:1),
OpPizza(2:1-2:3),UpperIdent(2:4-2:7),NoSpaceDotLowerIdent(2:7-2:14),Newline(1:1-1:1),
OpPizza(3:1-3:3),UpperIdent(3:4-3:8),NoSpaceDotLowerIdent(3:8-3:12),OpBackslash(3:13-3:14),LowerIdent(3:14-3:18),OpArrow(3:19-3:21),LowerIdent(3:22-3:26),OpPlus(3:27-3:28),Int(3:29-3:30),Newline(1:1-1:1),
OpPizza(4:1-4:3),UpperIdent(4:4-4:8),NoSpaceDotLowerIdent(4:8-4:16),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-string @1.1-1.11
	(e-string-part @1.2-1.10 (raw "a string")))
~~~
# FORMATTED
~~~roc
"a string"
~~~
# CANONICALIZE
~~~clojure
(e-string @1.1-1.11
	(e-literal @1.2-1.10 (string "a string")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.11 (type "Str"))
~~~
