# META
~~~ini
description=Type aliases should not contain underscores (_) as they represent 'I don't care' types, which doesn't make sense when declaring a type.
type=snippet
~~~
# SOURCE
~~~roc
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
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:5:21:5:21
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:5:16:5:20
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:11:15:11:15
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - underscore_in_type_alias.md:13:18:13:18
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
MyType : _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
MyType : _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:5:21:5:21:**
```roc
ComplexType := List(_)
```
                    ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:5:16:5:20:**
```roc
ComplexType := List(_)
```
               ^^^^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
MyType : _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
MyType : _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
MyType : _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:11:15:11:15:**
```roc
TupleType := (_, U32, _)
```
              ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:1:1:1:1:**
```roc
MyType : _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_in_type_alias.md:13:18:13:18:**
```roc
TagType := [Some(_), None]
```
                 ^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
UpperIdent(1:1-1:7),OpColon(1:8-1:9),Underscore(1:10-1:11),
UpperIdent(3:1-3:10),OpColonEqual(3:11-3:13),Underscore(3:14-3:15),
UpperIdent(5:1-5:12),OpColonEqual(5:13-5:15),UpperIdent(5:16-5:20),NoSpaceOpenRound(5:20-5:21),Underscore(5:21-5:22),CloseRound(5:22-5:23),
UpperIdent(7:1-7:11),OpColonEqual(7:12-7:14),OpenCurly(7:15-7:16),LowerIdent(7:17-7:22),OpColon(7:22-7:23),Underscore(7:24-7:25),Comma(7:25-7:26),LowerIdent(7:27-7:32),OpColon(7:32-7:33),UpperIdent(7:34-7:37),CloseCurly(7:38-7:39),
UpperIdent(9:1-9:13),OpColonEqual(9:14-9:16),Underscore(9:17-9:18),OpArrow(9:19-9:21),Underscore(9:22-9:23),
UpperIdent(11:1-11:10),OpColonEqual(11:11-11:13),OpenRound(11:14-11:15),Underscore(11:15-11:16),Comma(11:16-11:17),UpperIdent(11:18-11:21),Comma(11:21-11:22),Underscore(11:23-11:24),CloseRound(11:24-11:25),
UpperIdent(13:1-13:8),OpColonEqual(13:9-13:11),OpenSquare(13:12-13:13),UpperIdent(13:13-13:17),NoSpaceOpenRound(13:17-13:18),Underscore(13:18-13:19),CloseRound(13:19-13:20),Comma(13:20-13:21),UpperIdent(13:22-13:26),CloseSquare(13:26-13:27),
EndOfFile(14:1-14:1),
~~~
# PARSE
~~~clojure
(file @1.1-13.27
	(type-module @1.1-1.7)
	(statements
		(s-type-decl @1.1-1.11
			(header @1.1-1.7 (name "MyType")
				(args))
			(_))
		(s-type-decl @3.1-3.15
			(header @3.1-3.10 (name "OtherType")
				(args))
			(_))
		(s-type-decl @5.1-5.23
			(header @5.1-5.12 (name "ComplexType")
				(args))
			(ty-apply @5.16-5.23
				(ty @5.16-5.20 (name "List"))
				(_)))
		(s-type-decl @7.1-7.39
			(header @7.1-7.11 (name "RecordType")
				(args))
			(ty-record @7.15-7.39
				(anno-record-field @7.17-7.25 (name "field")
					(_))
				(anno-record-field @7.27-7.37 (name "other")
					(ty @7.34-7.37 (name "U32")))))
		(s-type-decl @9.1-9.23
			(header @9.1-9.13 (name "FunctionType")
				(args))
			(ty-fn @9.17-9.23
				(_)
				(_)))
		(s-type-decl @11.1-11.25
			(header @11.1-11.10 (name "TupleType")
				(args))
			(ty-tuple @11.14-11.25
				(_)
				(ty @11.18-11.21 (name "U32"))
				(_)))
		(s-type-decl @13.1-13.27
			(header @13.1-13.8 (name "TagType")
				(args))
			(ty-tag-union @13.12-13.27
				(tags
					(ty-apply @13.13-13.20
						(ty @13.13-13.17 (name "Some"))
						(_))
					(ty @13.22-13.26 (name "None")))))))
~~~
# FORMATTED
~~~roc
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
	(s-alias-decl @1.1-1.11
		(ty-header @1.1-1.7 (name "MyType"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @3.1-3.15
		(ty-header @3.1-3.10 (name "OtherType"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @5.1-5.23
		(ty-header @5.1-5.12 (name "ComplexType"))
		(ty-apply @5.16-5.23 (name "List") (builtin)
			(ty-underscore @5.21-5.21)))
	(s-nominal-decl @7.1-7.39
		(ty-header @7.1-7.11 (name "RecordType"))
		(ty-record @7.15-7.39
			(field (field "field")
				(ty-underscore @1.1-1.1))
			(field (field "other")
				(ty-lookup @7.34-7.37 (name "U32") (builtin)))))
	(s-nominal-decl @9.1-9.23
		(ty-header @9.1-9.13 (name "FunctionType"))
		(ty-fn @9.17-9.23 (effectful false)
			(ty-underscore @1.1-1.1)
			(ty-underscore @1.1-1.1)))
	(s-nominal-decl @11.1-11.25
		(ty-header @11.1-11.10 (name "TupleType"))
		(ty-tuple @11.14-11.25
			(ty-underscore @11.15-11.15)
			(ty-lookup @11.18-11.21 (name "U32") (builtin))
			(ty-underscore @1.1-1.1)))
	(s-nominal-decl @13.1-13.27
		(ty-header @13.1-13.8 (name "TagType"))
		(ty-tag-union @13.12-13.27
			(ty-tag-name @13.13-13.20 (name "Some")
				(ty-underscore @13.18-13.18))
			(ty-tag-name @13.22-13.26 (name "None")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-1.11 (type "MyType")
			(ty-header @1.1-1.7 (name "MyType")))
		(nominal @3.1-3.15 (type "OtherType")
			(ty-header @3.1-3.10 (name "OtherType")))
		(nominal @5.1-5.23 (type "ComplexType")
			(ty-header @5.1-5.12 (name "ComplexType")))
		(nominal @7.1-7.39 (type "RecordType")
			(ty-header @7.1-7.11 (name "RecordType")))
		(nominal @9.1-9.23 (type "FunctionType")
			(ty-header @9.1-9.13 (name "FunctionType")))
		(nominal @11.1-11.25 (type "TupleType")
			(ty-header @11.1-11.10 (name "TupleType")))
		(nominal @13.1-13.27 (type "TagType")
			(ty-header @13.1-13.8 (name "TagType"))))
	(expressions))
~~~
