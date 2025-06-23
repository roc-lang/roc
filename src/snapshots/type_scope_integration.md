# META
~~~ini
description=Type declaration scope integration - redeclaration and undeclared type errors
type=file
~~~
# SOURCE
~~~roc
module [Foo, Bar]

# First declare a type
Foo : U64

# Try to redeclare the same type (should error)
Foo : Str

# Declare another type that uses an undeclared type
Bar : SomeUndeclaredType

# Declare a type that properly uses a declared type
Baz : Foo
~~~
# PROBLEMS
**TYPE REDECLARED**
The type ``Foo`` is being redeclared.

The redeclaration is here:
**type_scope_integration.md:7:1:10:4:**
```roc
Foo : Str

# Declare another type that uses an undeclared type
Bar : SomeUndeclaredType
```

But ``Foo`` was already declared here:
**type_scope_integration.md:4:1:7:4:**
```roc
Foo : U64

# Try to redeclare the same type (should error)
Foo : Str
```


**UNDECLARED TYPE**
The type ``SomeUndeclaredType`` is not declared in this scope.

This type is referenced here:
**type_scope_integration.md:10:7:10:25:**
```roc
Bar : SomeUndeclaredType
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:12),Comma(1:12-1:13),UpperIdent(1:14-1:17),CloseSquare(1:17-1:18),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:23),
UpperIdent(4:1-4:4),OpColon(4:5-4:6),UpperIdent(4:7-4:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:48),
UpperIdent(7:1-7:4),OpColon(7:5-7:6),UpperIdent(7:7-7:10),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(9:2-9:52),
UpperIdent(10:1-10:4),OpColon(10:5-10:6),UpperIdent(10:7-10:25),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:2-12:52),
UpperIdent(13:1-13:4),OpColon(13:5-13:6),UpperIdent(13:7-13:10),EndOfFile(13:10-13:10),
~~~
# PARSE
~~~clojure
(file (1:1-13:10)
	(module (1:1-1:18)
		(exposes (1:8-1:18)
			(exposed_item (upper_ident "Foo"))
			(exposed_item (upper_ident "Bar"))))
	(statements
		(type_decl (4:1-7:4)
			(header (4:1-4:4) "Foo" (args))
			(ty "U64"))
		(type_decl (7:1-10:4)
			(header (7:1-7:4) "Foo" (args))
			(ty "Str"))
		(type_decl (10:1-13:4)
			(header (10:1-10:4) "Bar" (args))
			(ty "SomeUndeclaredType"))
		(type_decl (13:1-13:10)
			(header (13:1-13:4) "Baz" (args))
			(ty "Foo"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(s_type_decl (4:1-7:4)
		(type_header (4:1-4:4) "Foo")
		(ty (4:7-4:10) "U64"))
	(s_type_decl (7:1-10:4)
		(type_header (7:1-7:4) "Foo")
		(ty (7:7-7:10) "Str"))
	(s_type_decl (10:1-13:4)
		(type_header (10:1-10:4) "Bar")
		(ty (10:7-10:25) "SomeUndeclaredType"))
	(s_type_decl (13:1-13:10)
		(type_header (13:1-13:4) "Baz")
		(ty (13:7-13:10) "Foo")))
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~