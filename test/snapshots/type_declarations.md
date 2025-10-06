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
MODULE NOT IMPORTED - type_declarations.md:13:11:13:24
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


**MODULE NOT IMPORTED**
There is no module with the name `Module` imported into this Roc file.

You're attempting to use this module here:
**type_declarations.md:13:11:13:24:**
```roc
MyType2 : Module.Thingy
```
          ^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceOpenRound(1:4-1:5),LowerIdent(1:5-1:6),Comma(1:6-1:7),LowerIdent(1:8-1:9),CloseRound(1:9-1:10),OpColon(1:11-1:12),UpperIdent(1:13-1:17),NoSpaceOpenRound(1:17-1:18),LowerIdent(1:18-1:19),CloseRound(1:19-1:20),Comma(1:20-1:21),OpenRound(1:22-1:23),LowerIdent(1:23-1:24),OpArrow(1:25-1:27),LowerIdent(1:28-1:29),CloseRound(1:29-1:30),OpArrow(1:31-1:33),UpperIdent(1:34-1:38),NoSpaceOpenRound(1:38-1:39),LowerIdent(1:39-1:40),CloseRound(1:40-1:41),
UpperIdent(3:1-3:4),OpColon(3:5-3:6),OpenRound(3:7-3:8),UpperIdent(3:8-3:11),Comma(3:11-3:12),UpperIdent(3:13-3:16),CloseRound(3:16-3:17),
UpperIdent(5:1-5:5),NoSpaceOpenRound(5:5-5:6),LowerIdent(5:6-5:7),CloseRound(5:7-5:8),OpColon(5:9-5:10),OpenCurly(5:11-5:12),LowerIdent(5:13-5:16),OpColon(5:17-5:18),UpperIdent(5:19-5:21),NoSpaceOpenRound(5:21-5:22),LowerIdent(5:22-5:23),CloseRound(5:23-5:24),Comma(5:24-5:25),LowerIdent(5:26-5:29),OpColon(5:30-5:31),UpperIdent(5:32-5:41),CloseCurly(5:42-5:43),
UpperIdent(7:1-7:6),NoSpaceOpenRound(7:6-7:7),LowerIdent(7:7-7:8),CloseRound(7:8-7:9),OpColon(7:10-7:11),OpenSquare(7:12-7:13),UpperIdent(7:13-7:17),NoSpaceOpenRound(7:17-7:18),LowerIdent(7:18-7:19),CloseRound(7:19-7:20),Comma(7:20-7:21),UpperIdent(7:22-7:26),CloseSquare(7:26-7:27),
UpperIdent(9:1-9:9),NoSpaceOpenRound(9:9-9:10),LowerIdent(9:10-9:11),CloseRound(9:11-9:12),OpColon(9:13-9:14),UpperIdent(9:15-9:20),NoSpaceOpenRound(9:20-9:21),LowerIdent(9:21-9:22),CloseRound(9:22-9:23),Comma(9:23-9:24),LowerIdent(9:25-9:26),OpArrow(9:27-9:29),UpperIdent(9:30-9:35),NoSpaceOpenRound(9:35-9:36),LowerIdent(9:36-9:37),CloseRound(9:37-9:38),
UpperIdent(11:1-11:7),OpColon(11:8-11:9),UpperIdent(11:10-11:13),
UpperIdent(13:1-13:8),OpColon(13:9-13:10),UpperIdent(13:11-13:17),NoSpaceDotUpperIdent(13:17-13:24),
EndOfFile(14:1-14:1),
~~~
# PARSE
~~~clojure
(file @1.1-13.24
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-1.41
			(header @1.1-1.10 (name "Map")
				(args
					(ty-var @1.5-1.6 (raw "a"))
					(ty-var @1.8-1.9 (raw "b"))))
			(ty-fn @1.13-1.41
				(ty-apply @1.13-1.20
					(ty @1.13-1.17 (name "List"))
					(ty-var @1.18-1.19 (raw "a")))
				(ty-fn @1.23-1.29
					(ty-var @1.23-1.24 (raw "a"))
					(ty-var @1.28-1.29 (raw "b")))
				(ty-apply @1.34-1.41
					(ty @1.34-1.38 (name "List"))
					(ty-var @1.39-1.40 (raw "b")))))
		(s-type-decl @3.1-3.17
			(header @3.1-3.4 (name "Foo")
				(args))
			(ty-tuple @3.7-3.17
				(ty @3.8-3.11 (name "Bar"))
				(ty @3.13-3.16 (name "Baz"))))
		(s-type-decl @5.1-5.43
			(header @5.1-5.8 (name "Some")
				(args
					(ty-var @5.6-5.7 (raw "a"))))
			(ty-record @5.11-5.43
				(anno-record-field @5.13-5.24 (name "foo")
					(ty-apply @5.19-5.24
						(ty @5.19-5.21 (name "Ok"))
						(ty-var @5.22-5.23 (raw "a"))))
				(anno-record-field @5.26-5.41 (name "bar")
					(ty @5.32-5.41 (name "Something")))))
		(s-type-decl @7.1-7.27
			(header @7.1-7.9 (name "Maybe")
				(args
					(ty-var @7.7-7.8 (raw "a"))))
			(ty-tag-union @7.12-7.27
				(tags
					(ty-apply @7.13-7.20
						(ty @7.13-7.17 (name "Some"))
						(ty-var @7.18-7.19 (raw "a")))
					(ty @7.22-7.26 (name "None")))))
		(s-type-decl @9.1-9.38
			(header @9.1-9.12 (name "SomeFunc")
				(args
					(ty-var @9.10-9.11 (raw "a"))))
			(ty-fn @9.15-9.38
				(ty-apply @9.15-9.23
					(ty @9.15-9.20 (name "Maybe"))
					(ty-var @9.21-9.22 (raw "a")))
				(ty-var @9.25-9.26 (raw "a"))
				(ty-apply @9.30-9.38
					(ty @9.30-9.35 (name "Maybe"))
					(ty-var @9.36-9.37 (raw "a")))))
		(s-type-decl @11.1-11.13
			(header @11.1-11.7 (name "MyType")
				(args))
			(ty @11.10-11.13 (name "U64")))
		(s-type-decl @13.1-13.24
			(header @13.1-13.8 (name "MyType2")
				(args))
			(ty @13.11-13.24 (name "Module.Thingy")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-1.41
		(ty-header @1.1-1.10 (name "Map")
			(ty-args
				(ty-rigid-var @1.5-1.6 (name "a"))
				(ty-rigid-var @1.8-1.9 (name "b"))))
		(ty-fn @1.13-1.41 (effectful false)
			(ty-apply @1.13-1.20 (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var @1.5-1.6 (name "a"))))
			(ty-parens @1.22-1.30
				(ty-fn @1.23-1.29 (effectful false)
					(ty-rigid-var-lookup (ty-rigid-var @1.5-1.6 (name "a")))
					(ty-rigid-var-lookup (ty-rigid-var @1.8-1.9 (name "b")))))
			(ty-apply @1.34-1.41 (name "List") (builtin)
				(ty-rigid-var-lookup (ty-rigid-var @1.8-1.9 (name "b"))))))
	(s-alias-decl @3.1-3.17
		(ty-header @3.1-3.4 (name "Foo"))
		(ty-tuple @3.7-3.17
			(ty-malformed @3.8-3.11)
			(ty-malformed @3.13-3.16)))
	(s-alias-decl @5.1-5.43
		(ty-header @5.1-5.8 (name "Some")
			(ty-args
				(ty-rigid-var @5.6-5.7 (name "a"))))
		(ty-record @5.11-5.43
			(field (field "foo")
				(ty-malformed @5.19-5.21))
			(field (field "bar")
				(ty-malformed @5.32-5.41))))
	(s-alias-decl @7.1-7.27
		(ty-header @7.1-7.9 (name "Maybe")
			(ty-args
				(ty-rigid-var @7.7-7.8 (name "a"))))
		(ty-tag-union @7.12-7.27
			(ty-tag-name @7.13-7.20 (name "Some")
				(ty-rigid-var-lookup (ty-rigid-var @7.7-7.8 (name "a"))))
			(ty-tag-name @7.22-7.26 (name "None"))))
	(s-alias-decl @9.1-9.38
		(ty-header @9.1-9.12 (name "SomeFunc")
			(ty-args
				(ty-rigid-var @9.10-9.11 (name "a"))))
		(ty-fn @9.15-9.38 (effectful false)
			(ty-apply @9.15-9.23 (name "Maybe") (local)
				(ty-rigid-var-lookup (ty-rigid-var @9.10-9.11 (name "a"))))
			(ty-rigid-var-lookup (ty-rigid-var @9.10-9.11 (name "a")))
			(ty-apply @9.30-9.38 (name "Maybe") (local)
				(ty-rigid-var-lookup (ty-rigid-var @9.10-9.11 (name "a"))))))
	(s-alias-decl @11.1-11.13
		(ty-header @11.1-11.7 (name "MyType"))
		(ty-lookup @11.10-11.13 (name "U64") (builtin)))
	(s-alias-decl @13.1-13.24
		(ty-header @13.1-13.8 (name "MyType2"))
		(ty-malformed @13.11-13.24)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-1.41 (type "Map(a, b)")
			(ty-header @1.1-1.10 (name "Map")
				(ty-args
					(ty-rigid-var @1.5-1.6 (name "a"))
					(ty-rigid-var @1.8-1.9 (name "b")))))
		(alias @3.1-3.17 (type "Foo")
			(ty-header @3.1-3.4 (name "Foo")))
		(alias @5.1-5.43 (type "Some(a)")
			(ty-header @5.1-5.8 (name "Some")
				(ty-args
					(ty-rigid-var @5.6-5.7 (name "a")))))
		(alias @7.1-7.27 (type "Maybe(a)")
			(ty-header @7.1-7.9 (name "Maybe")
				(ty-args
					(ty-rigid-var @7.7-7.8 (name "a")))))
		(alias @9.1-9.38 (type "SomeFunc(a)")
			(ty-header @9.1-9.12 (name "SomeFunc")
				(ty-args
					(ty-rigid-var @9.10-9.11 (name "a")))))
		(alias @11.1-11.13 (type "MyType")
			(ty-header @11.1-11.7 (name "MyType")))
		(alias @13.1-13.24 (type "MyType2")
			(ty-header @13.1-13.8 (name "MyType2"))))
	(expressions))
~~~
