# META
~~~ini
description=Value alias referencing associated item
type=snippet
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    bar = 42
}

# Value alias to the associated item
myBar : U64
myBar = Foo.bar

result : U64
result = myBar
~~~
# EXPECTED
TYPE MISMATCH - nominal_associated_value_alias.md:7:9:7:16
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_value_alias.md:7:9:7:16:**
```roc
myBar = Foo.bar
```
        ^^^^^^^

It has the type:
    _Num(_size)_

But the type annotation says it should have the type:
    _U64_

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Whatever"))))
			(associated
				(s-decl
					(p-ident (raw "bar"))
					(e-int (raw "42")))))
		(s-type-anno (name "myBar")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "myBar"))
			(e-ident (raw "Foo.bar")))
		(s-type-anno (name "result")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "result"))
			(e-ident (raw "myBar")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	bar = 42
}

# Value alias to the associated item
myBar : U64
myBar = Foo.bar

result : U64
result = myBar
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "nominal_associated_value_alias.Foo.bar"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "myBar"))
		(e-lookup-local
			(p-assign (ident "nominal_associated_value_alias.Foo.bar")))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "result"))
		(e-lookup-local
			(p-assign (ident "myBar")))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)"))
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Num(_size)"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
