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
NOT IMPLEMENTED - record_builder_suffix.md:1:1:4:6
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record_builder expression

**record_builder_suffix.md:1:1:4:6:**
```roc
{
    x: 5,
    y: 0,
}.Foo
```

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!


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
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
