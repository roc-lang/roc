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
UpperIdent,OpColon,Underscore,
UpperIdent,OpColonEqual,Underscore,
UpperIdent,OpColonEqual,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,Underscore,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
UpperIdent,OpColonEqual,Underscore,OpArrow,Underscore,
UpperIdent,OpColonEqual,OpenRound,Underscore,Comma,UpperIdent,Comma,Underscore,CloseRound,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,Comma,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyType")
				(args))
			(_))
		(s-type-decl
			(header (name "OtherType")
				(args))
			(_))
		(s-type-decl
			(header (name "ComplexType")
				(args))
			(ty-apply
				(ty (name "List"))
				(_)))
		(s-type-decl
			(header (name "RecordType")
				(args))
			(ty-record
				(anno-record-field (name "field")
					(_))
				(anno-record-field (name "other")
					(ty (name "U32")))))
		(s-type-decl
			(header (name "FunctionType")
				(args))
			(ty-fn
				(_)
				(_)))
		(s-type-decl
			(header (name "TupleType")
				(args))
			(ty-tuple
				(_)
				(ty (name "U32"))
				(_)))
		(s-type-decl
			(header (name "TagType")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Some"))
						(_))
					(ty (name "None")))))))
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
	(s-alias-decl
		(ty-header (name "MyType"))
		(ty-underscore))
	(s-nominal-decl
		(ty-header (name "OtherType"))
		(ty-underscore))
	(s-nominal-decl
		(ty-header (name "ComplexType"))
		(ty-apply (name "List") (builtin)
			(ty-underscore)))
	(s-nominal-decl
		(ty-header (name "RecordType"))
		(ty-record
			(field (field "field")
				(ty-underscore))
			(field (field "other")
				(ty-lookup (name "U32") (builtin)))))
	(s-nominal-decl
		(ty-header (name "FunctionType"))
		(ty-fn (effectful false)
			(ty-underscore)
			(ty-underscore)))
	(s-nominal-decl
		(ty-header (name "TupleType"))
		(ty-tuple
			(ty-underscore)
			(ty-lookup (name "U32") (builtin))
			(ty-underscore)))
	(s-nominal-decl
		(ty-header (name "TagType"))
		(ty-tag-union
			(ty-tag-name (name "Some")
				(ty-underscore))
			(ty-tag-name (name "None")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "MyType")
			(ty-header (name "MyType")))
		(nominal (type "OtherType")
			(ty-header (name "OtherType")))
		(nominal (type "ComplexType")
			(ty-header (name "ComplexType")))
		(nominal (type "RecordType")
			(ty-header (name "RecordType")))
		(nominal (type "FunctionType")
			(ty-header (name "FunctionType")))
		(nominal (type "TupleType")
			(ty-header (name "TupleType")))
		(nominal (type "TagType")
			(ty-header (name "TagType"))))
	(expressions))
~~~
