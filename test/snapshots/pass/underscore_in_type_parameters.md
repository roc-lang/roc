# META
~~~ini
description=Type parameters should not contain underscores (_) as they represent 'I don't care' types, which doesn't make sense when declaring a type.
type=snippet
~~~
# SOURCE
~~~roc
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
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:2:8:2:9
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:5:9:5:10
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:8:12:8:13
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:11:13:11:14
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:14:11:14:12
UNDERSCORE IN TYPE ALIAS - underscore_in_type_parameters.md:14:14:14:15
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:2:8:2:9:**
```roc
MyType(_) : Str
```
       ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:5:9:5:10:**
```roc
MyType2(_, b) : b
```
        ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:8:12:8:13:**
```roc
MyType3(a, _) : a
```
           ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:11:13:11:14:**
```roc
ComplexType(_, b) : { field: b }
```
            ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:14:11:14:12:**
```roc
MultiType(_, _, c) : c
```
          ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_parameters.md:14:14:14:15:**
```roc
MultiType(_, _, c) : c
```
             ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
UpperIdent(2:1-2:7),NoSpaceOpenRound(2:7-2:8),Underscore(2:8-2:9),CloseRound(2:9-2:10),OpColon(2:11-2:12),UpperIdent(2:13-2:16),
UpperIdent(5:1-5:8),NoSpaceOpenRound(5:8-5:9),Underscore(5:9-5:10),Comma(5:10-5:11),LowerIdent(5:12-5:13),CloseRound(5:13-5:14),OpColon(5:15-5:16),LowerIdent(5:17-5:18),
UpperIdent(8:1-8:8),NoSpaceOpenRound(8:8-8:9),LowerIdent(8:9-8:10),Comma(8:10-8:11),Underscore(8:12-8:13),CloseRound(8:13-8:14),OpColon(8:15-8:16),LowerIdent(8:17-8:18),
UpperIdent(11:1-11:12),NoSpaceOpenRound(11:12-11:13),Underscore(11:13-11:14),Comma(11:14-11:15),LowerIdent(11:16-11:17),CloseRound(11:17-11:18),OpColon(11:19-11:20),OpenCurly(11:21-11:22),LowerIdent(11:23-11:28),OpColon(11:28-11:29),LowerIdent(11:30-11:31),CloseCurly(11:32-11:33),
UpperIdent(14:1-14:10),NoSpaceOpenRound(14:10-14:11),Underscore(14:11-14:12),Comma(14:12-14:13),Underscore(14:14-14:15),Comma(14:15-14:16),LowerIdent(14:17-14:18),CloseRound(14:18-14:19),OpColon(14:20-14:21),LowerIdent(14:22-14:23),
EndOfFile(15:1-15:1),
~~~
# PARSE
~~~clojure
(file @2.1-14.23
	(type-module @2.1-2.7)
	(statements
		(s-type-decl @2.1-2.16
			(header @2.1-2.10 (name "MyType")
				(args
					(_)))
			(ty @2.13-2.16 (name "Str")))
		(s-type-decl @5.1-5.18
			(header @5.1-5.14 (name "MyType2")
				(args
					(_)
					(ty-var @5.12-5.13 (raw "b"))))
			(ty-var @5.17-5.18 (raw "b")))
		(s-type-decl @8.1-8.18
			(header @8.1-8.14 (name "MyType3")
				(args
					(ty-var @8.9-8.10 (raw "a"))
					(_)))
			(ty-var @8.17-8.18 (raw "a")))
		(s-type-decl @11.1-11.33
			(header @11.1-11.18 (name "ComplexType")
				(args
					(_)
					(ty-var @11.16-11.17 (raw "b"))))
			(ty-record @11.21-11.33
				(anno-record-field @11.23-11.31 (name "field")
					(ty-var @11.30-11.31 (raw "b")))))
		(s-type-decl @14.1-14.23
			(header @14.1-14.19 (name "MultiType")
				(args
					(_)
					(_)
					(ty-var @14.17-14.18 (raw "c"))))
			(ty-var @14.22-14.23 (raw "c")))))
~~~
# FORMATTED
~~~roc
# Type with underscore in parameter position
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
	(s-alias-decl @2.1-2.16
		(ty-header @2.1-2.10 (name "MyType")
			(ty-args
				(ty-underscore @2.8-2.9)))
		(ty-lookup @2.13-2.16 (name "Str") (builtin)))
	(s-alias-decl @5.1-5.18
		(ty-header @5.1-5.14 (name "MyType2")
			(ty-args
				(ty-underscore @5.9-5.10)
				(ty-rigid-var @5.12-5.13 (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var @5.12-5.13 (name "b"))))
	(s-alias-decl @8.1-8.18
		(ty-header @8.1-8.14 (name "MyType3")
			(ty-args
				(ty-rigid-var @8.9-8.10 (name "a"))
				(ty-underscore @8.12-8.13)))
		(ty-rigid-var-lookup (ty-rigid-var @8.9-8.10 (name "a"))))
	(s-alias-decl @11.1-11.33
		(ty-header @11.1-11.18 (name "ComplexType")
			(ty-args
				(ty-underscore @11.13-11.14)
				(ty-rigid-var @11.16-11.17 (name "b"))))
		(ty-record @11.21-11.33
			(field (field "field")
				(ty-rigid-var-lookup (ty-rigid-var @11.16-11.17 (name "b"))))))
	(s-alias-decl @14.1-14.23
		(ty-header @14.1-14.19 (name "MultiType")
			(ty-args
				(ty-underscore @14.11-14.12)
				(ty-underscore @14.14-14.15)
				(ty-rigid-var @14.17-14.18 (name "c"))))
		(ty-rigid-var-lookup (ty-rigid-var @14.17-14.18 (name "c")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @2.1-2.16 (type "MyType(Error)")
			(ty-header @2.1-2.10 (name "MyType")
				(ty-args
					(ty-underscore @2.8-2.9))))
		(alias @5.1-5.18 (type "MyType2(Error, b)")
			(ty-header @5.1-5.14 (name "MyType2")
				(ty-args
					(ty-underscore @5.9-5.10)
					(ty-rigid-var @5.12-5.13 (name "b")))))
		(alias @8.1-8.18 (type "MyType3(a, Error)")
			(ty-header @8.1-8.14 (name "MyType3")
				(ty-args
					(ty-rigid-var @8.9-8.10 (name "a"))
					(ty-underscore @8.12-8.13))))
		(alias @11.1-11.33 (type "ComplexType(Error, b)")
			(ty-header @11.1-11.18 (name "ComplexType")
				(ty-args
					(ty-underscore @11.13-11.14)
					(ty-rigid-var @11.16-11.17 (name "b")))))
		(alias @14.1-14.23 (type "MultiType(Error, Error, c)")
			(ty-header @14.1-14.19 (name "MultiType")
				(ty-args
					(ty-underscore @14.11-14.12)
					(ty-underscore @14.14-14.15)
					(ty-rigid-var @14.17-14.18 (name "c"))))))
	(expressions))
~~~
