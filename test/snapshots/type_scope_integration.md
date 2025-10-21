# META
~~~ini
description=Type declaration scope integration - redeclaration and undeclared type errors
type=snippet
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
COMPILER DIAGNOSTIC - /Users/rtfeldman/code/roc2/test/snapshots/type_scope_integration.md:0:0:0:0
UNDECLARED TYPE - type_scope_integration.md:8:7:8:25
# PROBLEMS
**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'type_alias_redeclared' is not yet handled in report generation.
**/Users/rtfeldman/code/roc2/test/snapshots/type_scope_integration.md:0:0:0:0**

**UNDECLARED TYPE**
The type _SomeUndeclaredType_ is not declared in this scope.

This type is referenced here:
**type_scope_integration.md:8:7:8:25:**
```roc
Bar : SomeUndeclaredType
```
      ^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty (name "Str")))
		(s-type-decl
			(header (name "Bar")
				(args))
			(ty (name "SomeUndeclaredType")))
		(s-type-decl
			(header (name "Baz")
				(args))
			(ty (name "Foo")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Foo"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "Foo"))
		(ty-lookup (name "Str") (builtin)))
	(s-alias-decl
		(ty-header (name "Bar"))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "Baz"))
		(ty-lookup (name "Foo") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Foo")
			(ty-header (name "Foo")))
		(alias (type "Foo")
			(ty-header (name "Foo")))
		(alias (type "Bar")
			(ty-header (name "Bar")))
		(alias (type "Baz")
			(ty-header (name "Baz"))))
	(expressions))
~~~
