# META
~~~ini
description=record_builder_suffix_syntax
type=expr
~~~
# SOURCE
~~~roc
{
    x: 5,
    y: 0,
}.Foo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,Int,Comma,
CloseCurly,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record-builder
	(mapper (e-tag (raw "Foo")))
	(field (field "x")
		(e-int (raw "5")))
	(field (field "y")
		(e-int (raw "0"))))
~~~
# FORMATTED
~~~roc
{
	x: 5,
	y: 0,
}.Foo
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "record_builder_map2_not_found"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
