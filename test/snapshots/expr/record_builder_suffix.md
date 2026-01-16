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
DOES NOT EXIST - record_builder_suffix.md:1:1:4:6
# PROBLEMS
**DOES NOT EXIST**
`Foo.map2` does not exist.

`Foo` is in scope, but it has no associated `map2`.

It's referenced here:
**record_builder_suffix.md:1:1:4:6:**
```roc
{
    x: 5,
    y: 0,
}.Foo
```


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
(e-runtime-error (tag "nested_value_not_found"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
