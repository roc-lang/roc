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
NIL
# PROBLEMS
**DOES NOT EXIST**
`Foo.bar` does not exist.

`Foo` is in scope, but it has no associated `bar`.

It's referenced here:
**nominal_associated_value_alias.md:7:9:7:16:**
```roc
myBar = Foo.bar
```
        ^^^^^^^


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
		(e-runtime-error (tag "nested_value_not_found"))
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
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
