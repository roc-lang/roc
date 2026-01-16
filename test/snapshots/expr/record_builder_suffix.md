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
MISSING METHOD - record_builder_suffix.md:1:1:4:6
# PROBLEMS
**MISSING METHOD**
This **map2** method is being called on a value whose type doesn't have that method:
**record_builder_suffix.md:1:1:4:6:**
```roc
{
    x: 5,
    y: 0,
}.Foo
```

The value's type, which does not have a method named **map2**, is:

    [Foo, .._others]

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
(e-dot-access (field "map2")
	(receiver
		(e-tag (name "Foo")))
	(args
		(e-num (value "5"))
		(e-num (value "0"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-record
				(fields
					(field (name "x")
						(e-lookup-local
							(p-assign (ident "x"))))
					(field (name "y")
						(e-lookup-local
							(p-assign (ident "y")))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
