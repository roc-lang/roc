# META
~~~ini
description=parse_alias
type=expr
~~~
# SOURCE
~~~roc
Blah a b : Foo.Bar.Baz x y

42
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:9),OpColon(1:10-1:11),UpperIdent(1:12-1:15),NoSpaceDotUpperIdent(1:15-1:19),NoSpaceDotUpperIdent(1:19-1:23),LowerIdent(1:24-1:25),LowerIdent(1:26-1:27),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(3:1-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.5 (raw "Blah"))
~~~
# FORMATTED
~~~roc
Blah
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.5 (ext-var 73) (name "Blah") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[Blah]*"))
~~~
