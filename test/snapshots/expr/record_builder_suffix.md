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
RECORD BUILDER NOT SUPPORTED - record_builder_suffix.md:1:1:4:6
# PROBLEMS
**RECORD BUILDER NOT SUPPORTED**
The type `Foo` is used in a record builder expression, but does not implement `map2`:
**record_builder_suffix.md:1:1:4:6:**
```roc
{
    x: 5,
    y: 0,
}.Foo
```

Hint: To use `Foo` as a record builder, add a `map2` method to its type module.

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
