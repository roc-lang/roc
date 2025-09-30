# META
~~~ini
description=Type declaration scope integration - redeclaration and undeclared type errors
type=file
~~~
# SOURCE
~~~roc
# First declare a type
Foo : U64

# Try to redeclare the same type (should error)
Foo : Str

# Declare another type that uses an undeclared type
Bar : SomeUndeclaredType

# Declare a type that properly uses a declared type
Baz : Foo
~~~
# EXPECTED
TYPE REDECLARED - type_scope_integration.md:5:1:5:10
UNDECLARED TYPE - type_scope_integration.md:8:7:8:25
TYPE MODULE MISSING MATCHING TYPE - type_scope_integration.md:2:1:11:10
# PROBLEMS
**TYPE REDECLARED**
The type _Foo_ is being redeclared.

The redeclaration is here:
**type_scope_integration.md:5:1:5:10:**
```roc
Foo : Str
```
^^^^^^^^^

But _Foo_ was already declared here:
**type_scope_integration.md:2:1:2:10:**
```roc
Foo : U64
```
^^^^^^^^^


**UNDECLARED TYPE**
The type _SomeUndeclaredType_ is not declared in this scope.

This type is referenced here:
**type_scope_integration.md:8:7:8:25:**
```roc
Bar : SomeUndeclaredType
```
      ^^^^^^^^^^^^^^^^^^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `type_scope_integration.roc`, but no top-level type declaration named `type_scope_integration` was found.

Add either:
`type_scope_integration := ...` (nominal type)
or:
`type_scope_integration : ...` (type alias)
**type_scope_integration.md:2:1:11:10:**
```roc
Foo : U64

# Try to redeclare the same type (should error)
Foo : Str

# Declare another type that uses an undeclared type
Bar : SomeUndeclaredType

# Declare a type that properly uses a declared type
Baz : Foo
```


# TOKENS
~~~zig
UpperIdent(2:1-2:4),OpColon(2:5-2:6),UpperIdent(2:7-2:10),
UpperIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:10),
UpperIdent(8:1-8:4),OpColon(8:5-8:6),UpperIdent(8:7-8:25),
UpperIdent(11:1-11:4),OpColon(11:5-11:6),UpperIdent(11:7-11:10),
EndOfFile(12:1-12:1),
~~~
# PARSE
~~~clojure
(file @2.1-11.10
	(type-module @2.1-2.4)
	(statements
		(s-type-decl @2.1-2.10
			(header @2.1-2.4 (name "Foo")
				(args))
			(ty @2.7-2.10 (name "U64")))
		(s-type-decl @5.1-5.10
			(header @5.1-5.4 (name "Foo")
				(args))
			(ty @5.7-5.10 (name "Str")))
		(s-type-decl @8.1-8.25
			(header @8.1-8.4 (name "Bar")
				(args))
			(ty @8.7-8.25 (name "SomeUndeclaredType")))
		(s-type-decl @11.1-11.10
			(header @11.1-11.4 (name "Baz")
				(args))
			(ty @11.7-11.10 (name "Foo")))))
~~~
# FORMATTED
~~~roc
# First declare a type
# First declare a type
Foo : U64

# Try to redeclare the same type (should error)
Foo : Str

# Declare another type that uses an undeclared type
Bar : SomeUndeclaredType

# Declare a type that properly uses a declared type
Baz : Foo
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @2.1-2.10
		(ty-header @2.1-2.4 (name "Foo"))
		(ty @2.7-2.10 (name "U64")))
	(s-alias-decl @5.1-5.10
		(ty-header @5.1-5.4 (name "Foo"))
		(ty @5.7-5.10 (name "Str")))
	(s-alias-decl @8.1-8.25
		(ty-header @8.1-8.4 (name "Bar"))
		(ty @8.7-8.25 (name "SomeUndeclaredType")))
	(s-alias-decl @11.1-11.10
		(ty-header @11.1-11.4 (name "Baz"))
		(ty @11.7-11.10 (name "Foo"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @2.1-2.10 (type "Foo")
			(ty-header @2.1-2.4 (name "Foo")))
		(alias @5.1-5.10 (type "Foo")
			(ty-header @5.1-5.4 (name "Foo")))
		(alias @8.1-8.25 (type "Error")
			(ty-header @8.1-8.4 (name "Bar")))
		(alias @11.1-11.10 (type "Baz")
			(ty-header @11.1-11.4 (name "Baz"))))
	(expressions))
~~~
