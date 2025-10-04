# META
~~~ini
description=Type parameters should not contain underscores (_) as they represent 'I don't care' types, which doesn't make sense when declaring a type.
type=file
~~~
# SOURCE
~~~roc
module []

# Type with underscore in parameter position
MyType(_) : Str

# Type with underscore and regular parameter
MyType2(_, b) : b

# Type with parameters where underscore comes second
MyType3(a, _) : a

# More complex type with underscore parameter
ComplexType(_, b) : { field: b }

# Type with multiple underscores
MultiType(_, _, c) : c
~~~
# EXPECTED
MODULE HEADER DEPRECATED - underscore_in_type_parameters.md:1:1:1:10
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:4:8:4:9
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:7:9:7:10
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:10:12:10:13
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:13:13:13:14
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:16:11:16:12
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:16:14:16:15
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**underscore_in_type_parameters.md:1:1:1:10:**
```roc
module []
```
^^^^^^^^^


**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:4:8:4:9:**
```roc
MyType(_) : Str
```
       ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:7:9:7:10:**
```roc
MyType2(_, b) : b
```
        ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:10:12:10:13:**
```roc
MyType3(a, _) : a
```
           ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:13:13:13:14:**
```roc
ComplexType(_, b) : { field: b }
```
            ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:16:11:16:12:**
```roc
MultiType(_, _, c) : c
```
          ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:16:14:16:15:**
```roc
MultiType(_, _, c) : c
```
             ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(4:1-4:7),NoSpaceOpenRound(4:7-4:8),Underscore(4:8-4:9),CloseRound(4:9-4:10),OpColon(4:11-4:12),UpperIdent(4:13-4:16),
UpperIdent(7:1-7:8),NoSpaceOpenRound(7:8-7:9),Underscore(7:9-7:10),Comma(7:10-7:11),LowerIdent(7:12-7:13),CloseRound(7:13-7:14),OpColon(7:15-7:16),LowerIdent(7:17-7:18),
UpperIdent(10:1-10:8),NoSpaceOpenRound(10:8-10:9),LowerIdent(10:9-10:10),Comma(10:10-10:11),Underscore(10:12-10:13),CloseRound(10:13-10:14),OpColon(10:15-10:16),LowerIdent(10:17-10:18),
UpperIdent(13:1-13:12),NoSpaceOpenRound(13:12-13:13),Underscore(13:13-13:14),Comma(13:14-13:15),LowerIdent(13:16-13:17),CloseRound(13:17-13:18),OpColon(13:19-13:20),OpenCurly(13:21-13:22),LowerIdent(13:23-13:28),OpColon(13:28-13:29),LowerIdent(13:30-13:31),CloseCurly(13:32-13:33),
UpperIdent(16:1-16:10),NoSpaceOpenRound(16:10-16:11),Underscore(16:11-16:12),Comma(16:12-16:13),Underscore(16:14-16:15),Comma(16:15-16:16),LowerIdent(16:17-16:18),CloseRound(16:18-16:19),OpColon(16:20-16:21),LowerIdent(16:22-16:23),
EndOfFile(17:1-17:1),
~~~
# PARSE
~~~clojure
(file @1.1-16.23
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @4.1-4.16
			(header @4.1-4.10 (name "MyType")
				(args
					(_)))
			(ty @4.13-4.16 (name "Str")))
		(s-type-decl @7.1-7.18
			(header @7.1-7.14 (name "MyType2")
				(args
					(_)
					(ty-var @7.12-7.13 (raw "b"))))
			(ty-var @7.17-7.18 (raw "b")))
		(s-type-decl @10.1-10.18
			(header @10.1-10.14 (name "MyType3")
				(args
					(ty-var @10.9-10.10 (raw "a"))
					(_)))
			(ty-var @10.17-10.18 (raw "a")))
		(s-type-decl @13.1-13.33
			(header @13.1-13.18 (name "ComplexType")
				(args
					(_)
					(ty-var @13.16-13.17 (raw "b"))))
			(ty-record @13.21-13.33
				(anno-record-field @13.23-13.31 (name "field")
					(ty-var @13.30-13.31 (raw "b")))))
		(s-type-decl @16.1-16.23
			(header @16.1-16.19 (name "MultiType")
				(args
					(_)
					(_)
					(ty-var @16.17-16.18 (raw "c"))))
			(ty-var @16.22-16.23 (raw "c")))))
~~~
# FORMATTED
~~~roc
module []

# Type with underscore in parameter position
MyType(_) : Str

# Type with underscore and regular parameter
MyType2(_, b) : b

# Type with parameters where underscore comes second
MyType3(a, _) : a

# More complex type with underscore parameter
ComplexType(_, b) : { field : b }

# Type with multiple underscores
MultiType(_, _, c) : c
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @4.1-4.16
		(ty-header @4.1-4.10 (name "MyType")
			(ty-args
				(ty-underscore @4.8-4.9)))
		(ty-lookup @4.13-4.16 (name "Str") (builtin)))
	(s-alias-decl @7.1-7.18
		(ty-header @7.1-7.14 (name "MyType2")
			(ty-args
				(ty-underscore @7.9-7.10)
				(ty-rigid-var @7.12-7.13 (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var @7.12-7.13 (name "b"))))
	(s-alias-decl @10.1-10.18
		(ty-header @10.1-10.14 (name "MyType3")
			(ty-args
				(ty-rigid-var @10.9-10.10 (name "a"))
				(ty-underscore @10.12-10.13)))
		(ty-rigid-var-lookup (ty-rigid-var @10.9-10.10 (name "a"))))
	(s-alias-decl @13.1-13.33
		(ty-header @13.1-13.18 (name "ComplexType")
			(ty-args
				(ty-underscore @13.13-13.14)
				(ty-rigid-var @13.16-13.17 (name "b"))))
		(ty-record @13.21-13.33
			(field (field "field")
				(ty-rigid-var-lookup (ty-rigid-var @13.16-13.17 (name "b"))))))
	(s-alias-decl @16.1-16.23
		(ty-header @16.1-16.19 (name "MultiType")
			(ty-args
				(ty-underscore @16.11-16.12)
				(ty-underscore @16.14-16.15)
				(ty-rigid-var @16.17-16.18 (name "c"))))
		(ty-rigid-var-lookup (ty-rigid-var @16.17-16.18 (name "c")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @4.1-4.16 (type "MyType(Error)")
			(ty-header @4.1-4.10 (name "MyType")
				(ty-args
					(ty-underscore @4.8-4.9))))
		(alias @7.1-7.18 (type "MyType2(Error, b)")
			(ty-header @7.1-7.14 (name "MyType2")
				(ty-args
					(ty-underscore @7.9-7.10)
					(ty-rigid-var @7.12-7.13 (name "b")))))
		(alias @10.1-10.18 (type "MyType3(a, Error)")
			(ty-header @10.1-10.14 (name "MyType3")
				(ty-args
					(ty-rigid-var @10.9-10.10 (name "a"))
					(ty-underscore @10.12-10.13))))
		(alias @13.1-13.33 (type "ComplexType(Error, b)")
			(ty-header @13.1-13.18 (name "ComplexType")
				(ty-args
					(ty-underscore @13.13-13.14)
					(ty-rigid-var @13.16-13.17 (name "b")))))
		(alias @16.1-16.23 (type "MultiType(Error, Error, c)")
			(ty-header @16.1-16.19 (name "MultiType")
				(ty-args
					(ty-underscore @16.11-16.12)
					(ty-underscore @16.14-16.15)
					(ty-rigid-var @16.17-16.18 (name "c"))))))
	(expressions))
~~~
