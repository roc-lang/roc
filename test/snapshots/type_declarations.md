# META
~~~ini
description=Various type declarations
type=snippet
~~~
# SOURCE
~~~roc
Map(a, b) : List(a), (a -> b) -> List(b)

Foo : (Bar, Baz)

Some(a) : { foo : Ok(a), bar : Something }

Maybe(a) : [Some(a), None]

SomeFunc(a) : Maybe(a), a -> Maybe(a)

MyType : U64

MyType2 : Module.Thingy
~~~
# EXPECTED
UNDECLARED TYPE - type_declarations.md:3:8:3:11
UNDECLARED TYPE - type_declarations.md:3:13:3:16
UNDECLARED TYPE - type_declarations.md:5:19:5:21
UNDECLARED TYPE - type_declarations.md:5:32:5:41
UNDECLARED TYPE - type_declarations.md:13:17:13:24
# PROBLEMS
**UNDECLARED TYPE**
The type _Bar_ is not declared in this scope.

This type is referenced here:
**type_declarations.md:3:8:3:11:**
```roc
Foo : (Bar, Baz)
```
       ^^^


**UNDECLARED TYPE**
The type _Baz_ is not declared in this scope.

This type is referenced here:
**type_declarations.md:3:13:3:16:**
```roc
Foo : (Bar, Baz)
```
            ^^^


**UNDECLARED TYPE**
The type _Ok_ is not declared in this scope.

This type is referenced here:
**type_declarations.md:5:19:5:21:**
```roc
Some(a) : { foo : Ok(a), bar : Something }
```
                  ^^


**UNDECLARED TYPE**
The type _Something_ is not declared in this scope.

This type is referenced here:
**type_declarations.md:5:32:5:41:**
```roc
Some(a) : { foo : Ok(a), bar : Something }
```
                               ^^^^^^^^^


**UNDECLARED TYPE**
The type _Module.Thingy_ is not declared in this scope.

This type is referenced here:
**type_declarations.md:13:17:13:24:**
```roc
MyType2 : Module.Thingy
```
                ^^^^^^^


# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Map")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "b")))))
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tuple
				(ty (name "Bar"))
				(ty (name "Baz"))))
		(s-type-decl
			(header (name "Some")
				(args
					(ty-var (raw "a"))))
			(ty-record
				(anno-record-field (name "foo")
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "a"))))
				(anno-record-field (name "bar")
					(ty (name "Something")))))
		(s-type-decl
			(header (name "Maybe")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Some"))
						(ty-var (raw "a")))
					(ty (name "None")))))
		(s-type-decl
			(header (name "SomeFunc")
				(args
					(ty-var (raw "a"))))
			(ty-fn
				(ty-apply
					(ty (name "Maybe"))
					(ty-var (raw "a")))
				(ty-var (raw "a"))
				(ty-apply
					(ty (name "Maybe"))
					(ty-var (raw "a")))))
		(s-type-decl
			(header (name "MyType")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "MyType2")
				(args))
			(ty (name "Module.Thingy")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Map")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-fn (effectful false)
			(ty-apply (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-parens
				(ty-fn (effectful false)
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-rigid-var-lookup (ty-rigid-var (name "b")))))
			(ty-apply (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-alias-decl
		(ty-header (name "Foo"))
		(ty-tuple
			(ty-malformed)
			(ty-malformed)))
	(s-alias-decl
		(ty-header (name "Some")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-record
			(field (field "foo")
				(ty-malformed))
			(field (field "bar")
				(ty-malformed))))
	(s-alias-decl
		(ty-header (name "Maybe")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Some")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-tag-name (name "None"))))
	(s-alias-decl
		(ty-header (name "SomeFunc")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-fn (effectful false)
			(ty-apply (name "Maybe") (local)
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-apply (name "Maybe") (local)
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
	(s-alias-decl
		(ty-header (name "MyType"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "MyType2"))
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Map(a, b)")
			(ty-header (name "Map")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "Foo")
			(ty-header (name "Foo")))
		(alias (type "Some(a)")
			(ty-header (name "Some")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Maybe(a)")
			(ty-header (name "Maybe")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "SomeFunc(a)")
			(ty-header (name "SomeFunc")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "MyType")
			(ty-header (name "MyType")))
		(alias (type "MyType2")
			(ty-header (name "MyType2"))))
	(expressions))
~~~
