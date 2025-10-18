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
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpColon,UpperIdent,
UpperIdent,NoSpaceOpenRound,Underscore,Comma,LowerIdent,CloseRound,OpColon,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,Underscore,CloseRound,OpColon,LowerIdent,
UpperIdent,NoSpaceOpenRound,Underscore,Comma,LowerIdent,CloseRound,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,
UpperIdent,NoSpaceOpenRound,Underscore,Comma,Underscore,Comma,LowerIdent,CloseRound,OpColon,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyType")
				(args
					(_)))
			(ty (name "Str")))
		(s-type-decl
			(header (name "MyType2")
				(args
					(_)
					(ty-var (raw "b"))))
			(ty-var (raw "b")))
		(s-type-decl
			(header (name "MyType3")
				(args
					(ty-var (raw "a"))
					(_)))
			(ty-var (raw "a")))
		(s-type-decl
			(header (name "ComplexType")
				(args
					(_)
					(ty-var (raw "b"))))
			(ty-record
				(anno-record-field (name "field")
					(ty-var (raw "b")))))
		(s-type-decl
			(header (name "MultiType")
				(args
					(_)
					(_)
					(ty-var (raw "c"))))
			(ty-var (raw "c")))))
~~~
# FORMATTED
~~~roc
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
	(s-alias-decl
		(ty-header (name "MyType")
			(ty-args
				(ty-underscore)))
		(ty-lookup (name "Str") (builtin)))
	(s-alias-decl
		(ty-header (name "MyType2")
			(ty-args
				(ty-underscore)
				(ty-rigid-var (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
	(s-alias-decl
		(ty-header (name "MyType3")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-underscore)))
		(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
	(s-alias-decl
		(ty-header (name "ComplexType")
			(ty-args
				(ty-underscore)
				(ty-rigid-var (name "b"))))
		(ty-record
			(field (field "field")
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-alias-decl
		(ty-header (name "MultiType")
			(ty-args
				(ty-underscore)
				(ty-underscore)
				(ty-rigid-var (name "c"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "c")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "MyType(Error)")
			(ty-header (name "MyType")
				(ty-args
					(ty-underscore))))
		(alias (type "MyType2(Error, b)")
			(ty-header (name "MyType2")
				(ty-args
					(ty-underscore)
					(ty-rigid-var (name "b")))))
		(alias (type "MyType3(a, Error)")
			(ty-header (name "MyType3")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-underscore))))
		(alias (type "ComplexType(Error, b)")
			(ty-header (name "ComplexType")
				(ty-args
					(ty-underscore)
					(ty-rigid-var (name "b")))))
		(alias (type "MultiType(Error, Error, c)")
			(ty-header (name "MultiType")
				(ty-args
					(ty-underscore)
					(ty-underscore)
					(ty-rigid-var (name "c"))))))
	(expressions))
~~~
