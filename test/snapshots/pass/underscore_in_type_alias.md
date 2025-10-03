# META
~~~ini
description=Type aliases should not contain underscores (_) as they represent 'I don't care' types, which doesn't make sense when declaring a type.
type=file
~~~
# SOURCE
~~~roc
module []

MyType : _

OtherType := _

ComplexType := List(_)

RecordType := { field: _, other: U32 }

FunctionType := _ -> _

TupleType := (_, U32, _)

TagType := [Some(_), None]
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:7:21:7:21
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:7:16:7:20
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:13:15:13:15
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:15:18:15:18
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
module []
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
module []
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:7:21:7:21:**
```roc
ComplexType := List(_)
```
                    ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:7:16:7:20:**
```roc
ComplexType := List(_)
```
               ^^^^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
module []
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
module []
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
module []
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:13:15:13:15:**
```roc
TupleType := (_, U32, _)
```
              ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
module []
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:15:18:15:18:**
```roc
TagType := [Some(_), None]
```
                 ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:7),OpColon(3:8-3:9),Underscore(3:10-3:11),
UpperIdent(5:1-5:10),OpColonEqual(5:11-5:13),Underscore(5:14-5:15),
UpperIdent(7:1-7:12),OpColonEqual(7:13-7:15),UpperIdent(7:16-7:20),NoSpaceOpenRound(7:20-7:21),Underscore(7:21-7:22),CloseRound(7:22-7:23),
UpperIdent(9:1-9:11),OpColonEqual(9:12-9:14),OpenCurly(9:15-9:16),LowerIdent(9:17-9:22),OpColon(9:22-9:23),Underscore(9:24-9:25),Comma(9:25-9:26),LowerIdent(9:27-9:32),OpColon(9:32-9:33),UpperIdent(9:34-9:37),CloseCurly(9:38-9:39),
UpperIdent(11:1-11:13),OpColonEqual(11:14-11:16),Underscore(11:17-11:18),OpArrow(11:19-11:21),Underscore(11:22-11:23),
UpperIdent(13:1-13:10),OpColonEqual(13:11-13:13),OpenRound(13:14-13:15),Underscore(13:15-13:16),Comma(13:16-13:17),UpperIdent(13:18-13:21),Comma(13:21-13:22),Underscore(13:23-13:24),CloseRound(13:24-13:25),
UpperIdent(15:1-15:8),OpColonEqual(15:9-15:11),OpenSquare(15:12-15:13),UpperIdent(15:13-15:17),NoSpaceOpenRound(15:17-15:18),Underscore(15:18-15:19),CloseRound(15:19-15:20),Comma(15:20-15:21),UpperIdent(15:22-15:26),CloseSquare(15:26-15:27),
EndOfFile(16:1-16:1),
~~~
# PARSE
~~~clojure
(file @1.1-15.27
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.11
			(header @3.1-3.7 (name "MyType")
				(args))
			(_))
		(s-type-decl @5.1-5.15
			(header @5.1-5.10 (name "OtherType")
				(args))
			(_))
		(s-type-decl @7.1-7.23
			(header @7.1-7.12 (name "ComplexType")
				(args))
			(ty-apply @7.16-7.23
				(ty @7.16-7.20 (name "List"))
				(_)))
		(s-type-decl @9.1-9.39
			(header @9.1-9.11 (name "RecordType")
				(args))
			(ty-record @9.15-9.39
				(anno-record-field @9.17-9.25 (name "field")
					(_))
				(anno-record-field @9.27-9.37 (name "other")
					(ty @9.34-9.37 (name "U32")))))
		(s-type-decl @11.1-11.23
			(header @11.1-11.13 (name "FunctionType")
				(args))
			(ty-fn @11.17-11.23
				(_)
				(_)))
		(s-type-decl @13.1-13.25
			(header @13.1-13.10 (name "TupleType")
				(args))
			(ty-tuple @13.14-13.25
				(_)
				(ty @13.18-13.21 (name "U32"))
				(_)))
		(s-type-decl @15.1-15.27
			(header @15.1-15.8 (name "TagType")
				(args))
			(ty-tag-union @15.12-15.27
				(tags
					(ty-apply @15.13-15.20
						(ty @15.13-15.17 (name "Some"))
						(_))
					(ty @15.22-15.26 (name "None")))))))
~~~
# FORMATTED
~~~roc
module []

MyType : _

OtherType := _

ComplexType := List(_)

RecordType := { field : _, other : U32 }

FunctionType := _ -> _

TupleType := (_, U32, _)

TagType := [Some(_), None]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-3.11
		(ty-header @3.1-3.7 (name "MyType"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @5.1-5.15
		(ty-header @5.1-5.10 (name "OtherType"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @7.1-7.23
		(ty-header @7.1-7.12 (name "ComplexType"))
		(ty-apply @7.16-7.23 (name "List") (builtin)
			(ty-underscore @7.21-7.21)))
	(s-nominal-decl @9.1-9.39
		(ty-header @9.1-9.11 (name "RecordType"))
		(ty-record @9.15-9.39
			(field (field "field")
				(ty-underscore @1.1-1.1))
			(field (field "other")
				(ty-lookup @9.34-9.37 (name "U32") (builtin)))))
	(s-nominal-decl @11.1-11.23
		(ty-header @11.1-11.13 (name "FunctionType"))
		(ty-fn @11.17-11.23 (effectful false)
			(ty-underscore @1.1-1.1)
			(ty-underscore @1.1-1.1)))
	(s-nominal-decl @13.1-13.25
		(ty-header @13.1-13.10 (name "TupleType"))
		(ty-tuple @13.14-13.25
			(ty-underscore @13.15-13.15)
			(ty-lookup @13.18-13.21 (name "U32") (builtin))
			(ty-underscore @1.1-1.1)))
	(s-nominal-decl @15.1-15.27
		(ty-header @15.1-15.8 (name "TagType"))
		(ty-tag-union @15.12-15.27
			(ty-tag-name @15.13-15.20 (name "Some")
				(ty-underscore @15.18-15.18))
			(ty-tag-name @15.22-15.26 (name "None")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @3.1-3.11 (type "MyType")
			(ty-header @3.1-3.7 (name "MyType")))
		(nominal @5.1-5.15 (type "OtherType")
			(ty-header @5.1-5.10 (name "OtherType")))
		(nominal @7.1-7.23 (type "ComplexType")
			(ty-header @7.1-7.12 (name "ComplexType")))
		(nominal @9.1-9.39 (type "RecordType")
			(ty-header @9.1-9.11 (name "RecordType")))
		(nominal @11.1-11.23 (type "FunctionType")
			(ty-header @11.1-11.13 (name "FunctionType")))
		(nominal @13.1-13.25 (type "TupleType")
			(ty-header @13.1-13.10 (name "TupleType")))
		(nominal @15.1-15.27 (type "TagType")
			(ty-header @15.1-15.8 (name "TagType"))))
	(expressions))
~~~
